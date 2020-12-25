#lang racket/base
(require racket/class
         racket/place
         racket/match
         racket/contract
         (for-syntax racket/base)
         drracket/private/syncheck/traversals
         drracket/private/syncheck/syncheck-intf
         drracket/private/syncheck/xref
         "../../private/eval-helpers-and-pref-init.rkt"
         "intf.rkt"
         "local-member-names.rkt")

(provide go monitor)

(define (create-rename-answerer-thread orig-cust local-chan table)
  ;; the hope is that changing the custodian like this
  ;; shouldn't leak these threads, but it does seem to
  ;; so for now we don't use it
  (parameterize (#;[current-custodian orig-cust])
    (thread
     (λ () 
       (with-handlers ([exn:fail? (λ (x) (eprintf "online-comp.rkt: thread failed ~a\n"
                                                  (exn-message x)))])
         (let loop ()
           (define id/name (place-channel-get local-chan))
           (define id (list-ref id/name 0))
           (define name (list-ref id/name 1))
           (define res ((hash-ref table id) name))
           (place-channel-put local-chan res)
           (loop))))))
  (void))

;; the online-check-syntax logger is a public api
;; so use this struct so the listener can trust
;; the messages that are coming from this file
(struct instructions-to-monitor-thread (val))

(define-logger online-check-syntax)
(define (go expanded path the-source orig-cust)
  ;; this is called from expanding-place.rkt
  ;; in a context where current-library-collection-paths
  ;; is set properly (via eval-helpers-and-pref-init.rkt)
  ;; so send that over to the thread that is going
  ;; to process the syntax objects so it is set there too
  (log-message online-check-syntax-logger 'info  ""
               (instructions-to-monitor-thread
                (let ([clcp (current-library-collection-paths)])
                  (λ ()
                    (current-library-collection-paths clcp)))))
  (define c (make-channel))
  (unless (exn? expanded)
    (log-message online-check-syntax-logger 'info  "" (list expanded)))
  (log-message online-check-syntax-logger 'info  "" (instructions-to-monitor-thread c))
  ;; wait for everything to actually get sent back to the main place
  (channel-get c))

(define (build-trace stxes the-source orig-cust path)
  (parameterize ([current-max-to-send-at-once 50])
    (define obj (new build-place-chan-trace%
                     [src the-source]
                     [orig-cust orig-cust]))
    (define-values (expanded-expression expansion-completed)
      (make-traversal (current-namespace)
                      (get-init-dir path)))
    (parameterize ([current-annotations obj])
      (for ([stx (in-list stxes)])
        (expanded-expression stx))
      (expansion-completed))
    (send obj get-trace)))

(define build-place-chan-trace%
  (class build-trace%
    (inherit add-to-trace)
    (init-field  orig-cust)
    (define-values (remote-chan local-chan) (place-channel))
    (define table (make-hash))
    (create-rename-answerer-thread orig-cust local-chan table)
    (define/override (syncheck:add-arrow/name-dup/pxpy _start-text
                                                       start-pos-left start-pos-right
                                                       start-px start-py
                                                       _end-text
                                                       end-pos-left end-pos-right
                                                       end-px end-py
                                                       actual? level require-arrow? name-dup?)
      (define id (hash-count table))
      (hash-set! table id name-dup?)
      (add-to-trace (vector 'syncheck:add-arrow/name-dup/pxpy
                            start-pos-left start-pos-right start-px start-py
                            end-pos-left end-pos-right end-px end-py
                            actual? level require-arrow? remote-chan id)))
    
    (define/override (syncheck:add-id-set to-be-renamed/poss dup-name?)
      (define id (hash-count table))
      (hash-set! table id dup-name?)
      (add-to-trace (vector 'syncheck:add-id-set (map cdr to-be-renamed/poss) remote-chan id)))
    (super-new)))
    

(define (monitor send-back path the-source orig-cust)
  (define lr (make-log-receiver (current-logger)
                                'info
                                'online-check-syntax))
  (thread
   (λ ()
     (let loop ()
       (define val (sync lr))
       (match val
         [(vector level message obj name)
          (cond
            [(and (list? obj) (andmap syntax? obj))
             (with-handlers ([exn:fail?
                              (λ (exn)
                                (define sp (open-output-string))
                                (parameterize ([current-error-port sp])
                                  ((error-display-handler) (exn-message exn) exn))
                                (send-back (get-output-string sp)))])
               (define trace (build-trace obj the-source orig-cust path))
               (send-back trace))]
            [(instructions-to-monitor-thread? obj)
             (define val (instructions-to-monitor-thread-val obj))
             (cond
               [(procedure? val)
                ;; initialize this thread with state need to make the expansion
                ;; work properly
                (val)]
               [(channel? val)
                ;; signal back to the main place that we've gotten everything
                ;; and sent it back over
                (channel-put val (void))])])]
         [_ (void)])
       (loop)))))
