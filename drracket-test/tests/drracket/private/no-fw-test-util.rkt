#lang racket/base
(require racket/gui/base
         racket/class
         racket/contract)

(provide use-hash-for-prefs
         fire-up-separate-drracket-and-run-tests
         not-on-eventspace-handler-thread
         on-eventspace-handler-thread
         poll-until
         wait-for-events-in-frame-eventspace
         (contract-out
          [queue-callback/res (-> (-> any) any)]))

;; fire-up-separate-drracket-and-run-tests : (-> any) -> any
;; creates a separate custodian, eventspace, namespace, etc to
;; start up a new DrRacket. This has the advantage over fire-up-drracket-and-run-tests
;; that a single test suite can start up DrRacket multiple times, but it has the 
;; disadvantage that there is little sharing between the test suite implementation code and
;; DrRacket, so writing the testing code is more painful
;;
;; the only things shared are mred/mred (and its dependencies).
(define (fire-up-separate-drracket-and-run-tests run-test)
  (define c (make-custodian))
  (define s (make-semaphore 0))
  (define orig-ns (current-namespace))
  (parameterize ([current-custodian c])
    (parameterize ([exit-handler (λ (v) 
                                   (semaphore-post s)
                                   (custodian-shutdown-all c))]
                   [current-namespace (make-empty-namespace)]
                   [current-command-line-arguments #()])
      (parameterize ([current-eventspace (make-eventspace)])
        (namespace-attach-module orig-ns 'mred/mred)
        
        (let ([s (make-semaphore)])
          (queue-callback
           (λ ()
             (use-hash-for-prefs
              (dynamic-require 'framework/preferences 'preferences:low-level-get-preference)
              (dynamic-require 'framework/preferences 'preferences:low-level-put-preferences)
              (dynamic-require 'framework/preferences 'preferences:restore-defaults)
              (dynamic-require 'framework/preferences 'preferences:set)
              (dynamic-require 'framework/preferences 'preferences:default-set?)
              '())
             (semaphore-post s)))
          (semaphore-wait s))

        ;; do this now so that dynamically requiring framework
        ;; exports during the call to 'run-test' is safe
        (namespace-require 'framework)
        
        (queue-callback
         (λ ()
           (dynamic-require 'drracket #f)
           (thread (λ ()
                     (run-test)
                     (exit)))
           (yield (make-semaphore 0)))))))
  (semaphore-wait s))

(define (use-hash-for-prefs preferences:low-level-get-preference 
                            preferences:low-level-put-preferences
                            preferences:restore-defaults
                            preferences:set
                            preferences:default-set?
                            prefs)
  ;; change the preferences system so that it doesn't write to 
  ;; a file; partly to avoid problems of concurrency in drdr
  ;; but also to make the test suite easier for everyone to run.
  (let ([prefs-table (make-hash)])
    (preferences:low-level-put-preferences
     (λ (names vals)
       (for ([name (in-list names)]
             [val (in-list vals)])
         (hash-set! prefs-table name val))))
    (preferences:low-level-get-preference 
     (λ (name [fail (lambda () #f)])
       (hash-ref prefs-table name fail)))
    
    ;; set all preferences to their defaults (some pref values may have
    ;; been read by this point, but hopefully that won't affect the
    ;; startup of drracket)
    (preferences:restore-defaults)
    
    ;; initialize some preferences to simulate these
    ;; being saved already in the user's prefs file 
    ;; call preferences:set too since the prefs file
    ;; may have been "read" already at this point
    (for ([pref (in-list prefs)])
      (define pref-key (list-ref pref 0))
      (define pref-val (list-ref pref 1))
      (define m (regexp-match #rx"^plt:framework-pref:(.*)$" (symbol->string pref-key)))
      (cond
        [m
         (hash-set! prefs-table pref-key pref-val)
         (define fw-pref-key (string->symbol (list-ref m 1)))
         (when (preferences:default-set? fw-pref-key)
           (preferences:set fw-pref-key pref-val))]
        [else
         ;; this currently doesn't happen, and it is easy to forget
         ;; that prefix, so print a message here to remind 
         (printf "WARNING: setting a preference that isn't set via the framework: ~s\n" 
                 pref-key)]))))

(define (queue-callback/res thunk)
  (not-on-eventspace-handler-thread
   'queue-callback/res
   #:more (λ () (format "\n  thunk: ~e" thunk)))
  (let ([c (make-channel)])
    (queue-callback (λ () (channel-put c (with-handlers ((exn:fail? values))
                                           (call-with-values thunk list))))
                    #f)
    (define res (channel-get c))
    (when (exn? res) (raise res))
    (apply values res)))

;; poll-until : (-> alpha) number (-> alpha) -> alpha
;; waits until pred return a true value and returns that.
;; if that doesn't happen by `secs', calls fail and returns that.
(define (poll-until pred
                    [secs 10]
                    [fail (lambda ()
                            (error 'poll-until 
                                   "timeout after ~e secs, ~e never returned a true value"
                                   secs pred))])
  (let ([step 1/20])
    (let loop ([counter secs])
      (if (<= counter 0)
          (fail)
          (let ([result (pred)])
            (or result
                (begin
                  (sleep step)
                  (loop (- counter step)))))))))

(define (wait-for-events-in-frame-eventspace fr)
  (define sema (make-semaphore 0))
  (parameterize ([current-eventspace (send fr get-eventspace)])
    (queue-callback
     (λ () (semaphore-post sema))
     #f))
  (semaphore-wait sema))


(define (not-on-eventspace-handler-thread fn #:more [more #f])
  (when (eq? (current-thread) (eventspace-handler-thread (current-eventspace)))
    (error fn
           (string-append
            "expected to be run on some thread other than the eventspace handler thread"
            (if more (more) "")))))

(define (on-eventspace-handler-thread fn)
  (unless (eq? (current-thread) (eventspace-handler-thread (current-eventspace)))
    (error fn "expected to be run on the eventspace handler thread")))
