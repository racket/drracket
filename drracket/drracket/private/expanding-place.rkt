#lang racket/base
(require racket/place
         racket/port
         racket/list
         racket/unit
         "eval-helpers-and-pref-init.rkt"
         compiler/cm
         framework/preferences
         syntax/readerr
         wxme
         errortrace/stacktrace)
(module+ test (require rackunit))

(provide start)

(struct exn-info (str full-str src-vecs exn-stack missing-mods) #:prefab)

(struct job (cust working-thd stop-watching-abnormal-termination))

;; key : any (used by equal? for comparision, but back in the main place)
;; monitor-pc : (or/c #f place-channel)
;;     -- #f means a "end of expansion" notification,
;;     -- place-channel means to notify at the beginning
(struct handler (key monitor-pc proc) #:transparent)
(define handlers '())

(define module-language-parallel-lock-client
  'uninitialized-module-language-parallel-lock-client)

(define old-registry-chan (make-channel))

(define expanding-place-logger (make-logger 
                                'drracket-background-compilation
                                (current-logger)))
(define-syntax-rule
  (ep-log-info expr)
  (when (log-level? expanding-place-logger 'info)
    (log-message expanding-place-logger
                 'info
                 expr
                 (current-continuation-marks))))

(define (start p)
  ;; get the module-language-compile-lock in the initial message
  (set! module-language-parallel-lock-client
        (compile-lock->parallel-lock-client
         (place-channel-get p)
         (current-custodian)
         current-parallel-lock-shutdown-evt))
  
  ;; get the handlers in a second message
  (set! handlers 
        (filter
         values
         (for/list ([lst (place-channel-get p)])
           (define file (list-ref lst 0))
           (define id (list-ref lst 1))
           (define monitor-pc (list-ref lst 2))
           (handler (list file id) monitor-pc (dynamic-require file id)))))
  
  (let loop ([current-job #f]
             ;; the old-registry argument holds on to the namespace-module-registry
             ;; from a previous run in order to keep entries in the bytecode cache
             [old-registry #f])
    (sync
     (handle-evt 
      old-registry-chan
      (λ (reg) (loop current-job reg)))
     (handle-evt
      p
      (λ (message)
        (cond
          [(eq? message 'abort)
           (when current-job (abort-job current-job))
           (loop #f old-registry)]
          [(vector? message)
           (when current-job (abort-job current-job))
           (define program-as-string (vector-ref message 0))
           (define port-name (vector-ref message 1))
           (define response-pc (vector-ref message 2))
           (define settings (vector-ref message 3))
           (define pc-status-expanding-place (vector-ref message 4))
           (define currently-open-files (vector-ref message 5))
           (loop (new-job program-as-string port-name response-pc settings pc-status-expanding-place)
                 old-registry)]))))))

(define (abort-job job)
  (when (log-level? expanding-place-logger 'info)
    (define stack (continuation-mark-set->context
                   (continuation-marks 
                    (job-working-thd job))))
    (ep-log-info (format "expanding-place.rkt: kill; worker-thd stack (size ~a) dead? ~a:" 
                         (length stack)
                         (thread-dead? (job-working-thd job))))
    (for ([x (in-list stack)])
      (ep-log-info (format "  ~s" x))))
  ((job-stop-watching-abnormal-termination job))
  (custodian-shutdown-all (job-cust job)))

(struct exn:access exn:fail ())

(define sys-namespace (current-namespace))

(define (new-job program-as-string the-source response-pc settings pc-status-expanding-place)
  (define custodian-limit
    (and (custodian-memory-accounting-available?)
         (preferences:get 'drracket:child-only-memory-limit)))
  (define cust-parent (make-custodian))
  (define cust (parameterize ([current-custodian cust-parent])
                 (make-custodian)))
  (when custodian-limit
    (custodian-limit-memory cust-parent custodian-limit cust-parent))
  (define memory-killed-cust-box (make-custodian-box cust-parent #t))
  (define exn-chan (make-channel))
  (define extra-exns-chan (make-channel))
  (define result-chan (make-channel))
  (define normal-termination (make-channel))
  (define abnormal-termination (make-channel))
  (define path (and (path? the-source) the-source))
  (define orig-cust (current-custodian))
  (define (stop-watching-abnormal-termination) 
    (channel-put normal-termination #t))
  
  (define working-thd
    (parameterize ([current-custodian cust])
      (thread
       (λ ()
         (define no-annotations?
           ;; if there is an annotation set up, then this compiled
           ;; code won't be the right compiled code, so don't save it
           (equal? (prefab-module-settings-annotations settings) 'none))
         (ep-log-info "expanding-place.rkt: 00 starting monitors")
         (for ([handler (in-list handlers)])
           (define pc (handler-monitor-pc handler))
           (when pc 
             (define (failed x)
               (eprintf "starting monitor ~s failed:\n" (handler-key handler))
               ((error-display-handler) (exn-message x) x))
             (with-handlers ([exn:fail? failed])
               (define (send-back val)
                 (place-channel-put 
                  response-pc
                  (vector 'monitor-message
                          (handler-key handler)
                          val)))
               ((handler-proc handler) send-back path the-source orig-cust))))
         
         (ep-log-info "expanding-place.rkt: 01 starting thread")
         (define sema (make-semaphore 0))
         (ep-log-info "expanding-place.rkt: 02 setting basic parameters")
         (set-basic-parameters/no-gui)
         (namespace-attach-module sys-namespace 'wxme)
         
         (define loaded-paths '())
         (define original-path (make-parameter #f))
         (current-load/use-compiled
          (let ([ol (current-load/use-compiled)])
            (λ (path mod-name)
              (parameterize ([original-path path])
                (ol path mod-name)))))
         (current-load
          (let ([cl (current-load)])
            (λ (path mod-name)
              (set! loaded-paths
                    (cons (or (current-module-declare-source)
                              (original-path)
                              path)
                          loaded-paths))
              (cl path mod-name))))
         (ep-log-info "expanding-place.rkt: 03 setting module language parameters")
         (when (equal? (prefab-module-settings-annotations settings) 'debug)
           (current-compile
            (make-debug-compile-handler/errortrace-annotate
             (current-compile)
             errortrace-annotate)))
         (set-module-language-parameters settings
                                         module-language-parallel-lock-client
                                         null
                                         #:use-use-current-security-guard? #t)
         (ep-log-info "expanding-place.rkt: 04 setting directories")
         (let ([init-dir (get-init-dir path)])
           (current-directory init-dir)
           (current-directory-for-user init-dir))
         (current-load-relative-directory #f)
         (define sp
           (cond
             [(bytes? program-as-string)
              (wxme-port->port (open-input-bytes program-as-string))]
             [else
              (open-input-string program-as-string)]))
         (port-count-lines! sp)
         (ep-log-info "expanding-place.rkt: 05 installing security guard")
         (install-security-guard) ;; must come after the call to set-module-language-parameters
         (ep-log-info "expanding-place.rkt: 06 setting uncaught-exception-handler")
         (error-display-handler
          (let ([e-d-h (error-display-handler)])
            (λ (msg exn)
              (channel-put extra-exns-chan exn)
              (e-d-h msg exn))))
         (uncaught-exception-handler
          (λ (exn)
            (parameterize ([current-custodian orig-cust])
              (thread
               (λ ()
                 (stop-watching-abnormal-termination)
                 (semaphore-post sema)
                 (channel-put exn-chan (list exn loaded-paths)))))
            (semaphore-wait sema)
            ((error-escape-handler))))
         (ep-log-info "expanding-place.rkt: 07 starting read-syntax")
         (define stx
           (parameterize ([read-accept-reader #t])
             (read-syntax the-source sp)))
         (ep-log-info "expanding-place.rkt: 08 read")
         (when (eof-object? stx) 
           (define-values (line col pos) (port-next-location sp))
           (raise-read-eof-error "no program to process"
                                 the-source
                                 1 0 1 pos))
         (define-values (name lang transformed-stx)
           (transform-module path
                             (namespace-syntax-introduce stx)
                             raise-hopeless-syntax-error))
         (define log-io? (log-level? expanding-place-logger 'warning))
         (define-values (in out)
           (if (or log-io? no-annotations?)
               (make-pipe)
               (values #f (open-output-nowhere))))
         (define the-io (make-channel))
         (cond
           [log-io?
            (thread (λ () (catch-and-log in the-io)))]
           [no-annotations?
            (thread (λ () (catch-and-check-non-empty in the-io)))])
         (ep-log-info "expanding-place.rkt: 09 starting expansion")
         (define expanded 
           (parameterize ([current-output-port out]
                          [current-error-port out])
             (expand transformed-stx)))
         (ep-log-info "expanding-place.rkt: 10 finished expansion")
         (define no-io-happened?
           (cond
             [(or log-io? no-annotations?)
              (close-output-port out)
              (channel-get the-io)]
             [else #f]))
         (channel-put old-registry-chan 
                      (namespace-module-registry (current-namespace)))
         (place-channel-put pc-status-expanding-place 'finished-expansion)
         (ep-log-info "expanding-place.rkt: 11 getting handler results")
         (define handler-results
           (filter
            values
            (for/list ([handler (in-list handlers)]
                       #:unless (handler-monitor-pc handler))
              (ep-log-info (format "expanding-place.rkt:    handler ~s" (handler-key handler)))
              (let/ec k
                (define proc-res
                  (with-handlers ([exn:fail?
                                   (λ (exn)
                                     (ep-log-info
                                      (let ()
                                        (define sp (open-output-string))
                                        (fprintf sp
                                                 "error running handler ~a:\n"
                                                 (handler-key handler))
                                        (display (exn-message exn) sp)
                                        (for ([x (in-list (continuation-mark-set->context
                                                           (exn-continuation-marks exn)))])
                                          (fprintf sp "  ~s\n" x))
                                        (get-output-string sp)))
                                     (k #f))])
                    ((handler-proc handler) expanded
                                            path
                                            the-source
                                            orig-cust)))
                (list (handler-key handler) proc-res)))))
         (ep-log-info "expanding-place.rkt: 12 handlers finished")
         (define compiled-bytes
           (cond
             [(and no-annotations?
                   ;; we don't try to reuse the compiled bytes
                   ;; if there was any IO because we cannot tell
                   ;; which part is to be replayed and which
                   ;; isn't; just re-run the expansion on the
                   ;; user's side so they see the IO directly
                   no-io-happened?)
              (define compiled (compile expanded))
              (define bp (open-output-bytes))
              (parameterize ([current-write-relative-directory (current-directory)])
                (write compiled bp))
              (get-output-bytes bp)]
             [else #f]))
         (ep-log-info "expanding-place.rkt: 13 compile finished")
         
         (parameterize ([current-custodian orig-cust])
           (thread
            (λ ()
              (stop-watching-abnormal-termination)
              (semaphore-post sema)
              (channel-put result-chan
                           (vector handler-results
                                   loaded-paths
                                   (and compiled-bytes
                                        (vector name lang compiled-bytes)))))))
         (semaphore-wait sema)
         (ep-log-info "expanding-place.rkt: 14 finished")))))
  
  (thread
   (λ ()
     (let loop ([watch-dead? #t])
       (sync 
        (handle-evt 
         normal-termination
         (λ (x) (loop #f)))
        (if watch-dead?
            (handle-evt 
             (thread-dead-evt working-thd)
             (λ (x) 
               (ep-log-info "expanding-place.rkt: abnormal termination")
               (channel-put abnormal-termination #t)
               (loop #f)))
            never-evt)))))
  
  (thread
   (λ ()
     (let loop ([extra-exns '()])
       (sync
        (handle-evt
         abnormal-termination
         (λ (val) 
           (place-channel-put pc-status-expanding-place
                              'abnormal-termination)
           (place-channel-put 
            response-pc
            (vector 'abnormal-termination 
                    ;; note: this message is not used directly, a string 
                    ;; constant is used back in the drracket place; but
                    ;; this is checked to see if it was an out of memory error
                    (if (custodian-box-value memory-killed-cust-box)
                        "Expansion thread terminated unexpectedly"
                        "Expansion thread terminated unexpectedly (out of memory)")
                    '()
                    
                    ;; give up on dep paths in this case:
                    '()))))
        (handle-evt
         result-chan
         (λ (val+loaded-paths+compiled-bytes)
           (place-channel-put response-pc (vector 'handler-results
                                                  (vector-ref val+loaded-paths+compiled-bytes 0)
                                                  (vector-ref val+loaded-paths+compiled-bytes 1)
                                                  (vector-ref val+loaded-paths+compiled-bytes 2)))))
        (handle-evt extra-exns-chan (λ (exn) (loop (cons exn extra-exns))))
        (handle-evt
         exn-chan
         (λ (exn+loaded-paths)
           (place-channel-put pc-status-expanding-place 'exn-raised)
           (define main-exn (list-ref exn+loaded-paths 0))
           
           ;; inform the handlers that an exn has been raised
           (when (exn? main-exn)
             (for ([handler (in-list handlers)]
                   #:unless (handler-monitor-pc handler))
               ((handler-proc handler) main-exn path the-source orig-cust)))
           
           (define exn-type (exn->type main-exn the-source))
           
           (define (format-srcloc srcloc)
             (define pos
               (cond
                 [(and (srcloc-line srcloc)
                       (srcloc-column srcloc))
                  (format ":~a:~a" (srcloc-line srcloc) (srcloc-column srcloc))]
                 [(srcloc-line srcloc)
                  (format ":~a" (srcloc-line srcloc))]
                 [(srcloc-position srcloc)
                  (format "::~a" (srcloc-position srcloc))]
                 [else ""]))
             (format "~a~a" (srcloc-source srcloc) pos))
             
           (define (srcloc->srcinfo a-srcloc)
             (cond
               [(and (srcloc? a-srcloc)
                     (equal? the-source (srcloc-source a-srcloc))
                     (srcloc-position a-srcloc)
                     (srcloc-span a-srcloc))
                (vector (srcloc-position a-srcloc)
                        (srcloc-span a-srcloc))]
               [else #f]))
           
           (define exn-infos
             (for/list ([an-exn (in-list (cons main-exn (reverse extra-exns)))])
               (exn-info 
                (trim-message
                 (if (exn? an-exn) 
                     (regexp-replace* #rx"[ \t]*\n[ \t]*" (exn-message an-exn) " ")
                     (format "uncaught exn: ~s" an-exn)))
                (if (exn? an-exn)
                    (exn-message an-exn)
                    (format "uncaught exn: ~s" an-exn))
                (cond
                  [(exn:srclocs? an-exn)
                   (sort
                    (for/list ([srcloc ((exn:srclocs-accessor an-exn) an-exn)]
                               #:when (srcloc->srcinfo srcloc))
                      (srcloc->srcinfo srcloc))
                    <
                    #:key (λ (x)
                            (cond
                              [(vector? x) (vector-ref x 0)]
                              [else +inf.0])))]
                  [(exn? an-exn)
                   (define marks (continuation-mark-set->context
                                  (exn-continuation-marks an-exn)))
                   (define info
                     (for/or ([mark-pr (in-list marks)])
                       (srcloc->srcinfo (cdr mark-pr))))
                   (cond
                     [info (list info)]
                     [else '()])]
                  [else '()])
                (cond
                  [(exn? an-exn)
                   (define ctxt 
                     (continuation-mark-set->context
                      (exn-continuation-marks an-exn)))
                   (for/list ([ctxt-elem (if (< (length ctxt) 100)
                                             ctxt
                                             (take ctxt 100))])
                     (define name (car ctxt-elem))
                     (define loc (cdr ctxt-elem))
                     (cond
                       [(not name) (format-srcloc loc)]
                       [(not loc) (format "~a" name)]
                       [else (format "~a:~a" (format-srcloc loc) name)]))]
                  [else '()])
                (and (exn:missing-module? an-exn)
                     ((exn:missing-module-accessor an-exn) an-exn)))))
           (place-channel-put 
            response-pc
            (vector 
             exn-type
             exn-infos
             (list-ref exn+loaded-paths 1)))))))))
  
  (job cust working-thd stop-watching-abnormal-termination))

(define (exn->type main-exn the-source)
  (cond
    [(exn:access? main-exn)
     'access-violation]
    [(and (exn:fail:read? main-exn)
          (andmap (λ (srcloc) (equal? (srcloc-source srcloc) the-source))
                  (exn:fail:read-srclocs main-exn)))
     'reader-in-defs-error]
    [(and (exn? main-exn)
          (regexp-match? #rx"[^\n]*: unbound identifier"
                         (exn-message main-exn)))
     'exn:variable]
    [else 'exn]))

(module+ test
  (define (call-exn->type expr-to-eval #:use-the-src? [use-the-src? #t])
    (define the-src 'the-src)
    (with-handlers ([(λ (x) #t) (λ (x) (exn->type x the-src))])
      (parameterize ([current-namespace (make-base-namespace)])
        (eval (read-syntax (if use-the-src? the-src 'not-the-src)
                           (open-input-string expr-to-eval))))))

  (check-equal? (call-exn->type "(module m racket/base free-variable)")
                'exn:variable)
  (check-equal? (call-exn->type "(car 1)")
                'exn)
  (check-equal? (call-exn->type "(raise 1)")
                'exn)
  (check-equal? (call-exn->type "(car")
                'reader-in-defs-error)
  (check-equal? (call-exn->type "(car" #:use-the-src? #f)
                'exn))

(define (catch-and-log port the-io)
  (let loop ([no-io-happened? #t])
    (sync
     (handle-evt (read-line-evt port 'linefeed)
                 (λ (l)
                   (cond
                     [(eof-object? l)
                      (channel-put the-io no-io-happened?)]
                     [else
                      (log-warning (format "online comp io: ~a" l))
                      (loop #f)]))))))

(define (catch-and-check-non-empty port the-io)
  (let loop ([was-io?/bytes #f])
    (cond
      [was-io?/bytes
       (sync
        (handle-evt
         (read-bytes!-evt was-io?/bytes port)
         (λ (num/eof)
           (cond
             [(eof-object? num/eof)
              (channel-put the-io #f)]
             [else
              (loop was-io?/bytes)]))))]
      [else
       (sync
        (handle-evt
         (read-bytes-evt 1 port)
         (λ (bytes/eof)
           (cond
             [(eof-object? bytes/eof)
              (channel-put the-io #t)]
             [else
              (loop (make-bytes 1000))]))))])))

(define (raise-hopeless-syntax-error . args)
  (apply raise-syntax-error '|Module Language| args))

(define (install-security-guard)
  (current-security-guard
   (make-security-guard
    (current-security-guard)
    (λ (prim path whats)
      (when (or (member 'write whats)
                (member 'execute whats)
                (member 'delete whats))
        (raise (exn:access (format "~a: forbidden ~a access to ~a" prim whats path)
                           (current-continuation-marks)))))
    (λ (prim target port what)
      (raise (exn:access (format "~a: forbidden ~a access to ~a:~a" prim what target port)
                         (current-continuation-marks))))
    (λ (prim path1 path2)
      (raise (exn:access (format "~a: forbidden to link ~a to ~a" prim path1 path2)
                         (current-continuation-marks)))))))

;; trim-message : string -> string[200 chars max]
(define (trim-message str)
  (cond
    [(<= (string-length str) 200)
     str]
    [else
     (define prefix-len 99)
     (define suffix-len 98)
     (define middle "...")
     
     ;; (+ prefix-len suffix-len (string-length middle)) must be 200 (or less)
     (string-append (substring str 0 prefix-len)
                    middle
                    (substring str (- (string-length str) suffix-len) (string-length str)))]))

(define with-mark (make-with-mark (λ (x) #f)))

;; these never get used as the invocation is used only for debugging annotations
(define (test-coverage-point body expr phase) body)
(define profile-key #f)
(define (initialize-profile-point key name expr) (void))
(define profiling-enabled (make-parameter #f))
(define (register-profile-start key) (void))
(define (register-profile-done key start) (void))
(define key-module-name 'drracket/private/drracket-errortrace-key)
(define-values/invoke-unit/infer stacktrace/errortrace-annotate/key-module-name@)
