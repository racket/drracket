(unit/sig stepper:model^
  (import [i : stepper:model-input^]
          mred^
          [z : zodiac:system^]
          [d : drscheme:export^]
          [p : mzlib:print-convert^]
          [e : zodiac:interface^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^]
          stepper:shared^)
  
  (define image? i:image?)
  
  (define (send-to-other-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))

  (define drscheme-eventspace (current-eventspace))

  (define (send-to-drscheme-eventspace thunk)
    (send-to-other-eventspace drscheme-eventspace thunk))
  
  (define par-constructor-style-printing #f)
  (define (constructor-style-printing?)
    par-constructor-style-printing)
  
  (define par-abbreviate-cons-as-list #f)
  (define (abbreviate-cons-as-list?)
    par-abbreviate-cons-as-list)
  
  (define par-cons #f)
  (define (user-cons? val)
    (eq? val par-cons))
  
  (define par-vector #f)
  (define (user-vector? val)
    (eq? val par-vector))
  
  (define user-pre-defined-vars #f)
  
  (define (check-pre-defined-var identifier)
    (memq identifier user-pre-defined-vars))
  
  (define user-namespace #f)

  (define (check-global-defined identifier)
    (with-handlers
        ([exn:variable? (lambda args #f)])
      (global-lookup identifier)
      #t))
  
  (define (global-lookup identifier)
    (parameterize ([current-namespace user-namespace])
      (global-defined-value identifier)))
    
  (define finished-exprs null)

  (define current-expr #f)
  (define packaged-envs a:initial-env-package)
         
  (define user-eventspace (make-eventspace))
         
  ;; user eventspace management
  
  ; here's how this stuff works.  To prevent the processing of any old events
  ; on the user's eventspace queue, suspend-user-computation basically sits
  ; on the thread.  The only way to get anything done on this thread is to 
  ; release the stepper-semaphore, either with a command of 'step, which 
  ; allows the user-eventspace thread to return to whatever it was doing
  ; when it was suspended, or with a thunk command, in which case the 
  ; user eventspace thread goes and executes that thunk before resuming
  ; waiting.  The stepper-command-waiting semaphore is used to prevent 
  ; stacked requests from demolishing each other. It might be better to 
  ; use a queue for this.
  
  (define stepper-semaphore (make-semaphore))
  (define stepper-command-waiting-semaphore (make-semaphore))
  (semaphore-post stepper-command-waiting-semaphore)
  (define stepper-return-val-semaphore (make-semaphore))
  (define stepper-awaken-arg #f)
  (define eval-depth 0)
  
  (define (suspend-user-computation)
    (semaphore-wait stepper-semaphore)
    (let ([local-awaken-arg stepper-awaken-arg])
      (semaphore-post stepper-command-waiting-semaphore)
      (cond 
        [(eq? local-awaken-arg 'step)
         (void)]
        [(procedure? local-awaken-arg)
         (set! eval-depth (+ eval-depth 1))
         (local-awaken-arg)
         (set! eval-depth (- eval-depth 1))
         (suspend-user-computation)]
        [else (e:internal-error "unknown value in stepper-awaken-arg.")])))

  (define (continue-user-computation)
    (semaphore-wait stepper-command-waiting-semaphore)
    (set! stepper-awaken-arg 'step)
    (semaphore-post stepper-semaphore))
  
  (define (send-to-user-eventspace thunk)
    (semaphore-wait stepper-command-waiting-semaphore)
    (set! stepper-awaken-arg thunk)
    (semaphore-post stepper-semaphore))
  
  ;; start user thread going
  (send-to-other-eventspace
   user-eventspace
   suspend-user-computation)

  (define user-primitive-eval #f)
  (define user-vocabulary #f)
         
  (define reader 
    (z:read i:text-stream
            (z:make-location 1 1 0 "stepper-text")))
  
  (send-to-user-eventspace
   (lambda ()
     (set! user-primitive-eval (current-eval))
     (d:basis:initialize-parameters (make-custodian) i:settings)
     (set! user-namespace (current-namespace))
     (set! user-pre-defined-vars (map car (make-global-value-list)))
     (set! user-vocabulary (d:basis:current-vocabulary))
     (set! par-constructor-style-printing (p:constructor-style-printing))
     (set! par-abbreviate-cons-as-list (p:abbreviate-cons-as-list))
     (set! par-cons (global-defined-value 'cons))
     (set! par-vector (global-defined-value 'vector))
     (semaphore-post stepper-return-val-semaphore)))
  (semaphore-wait stepper-return-val-semaphore)
  
  (define print-convert
    (let ([print-convert-result 'not-a-real-value])    
      (lambda (val)
        (send-to-user-eventspace
         (lambda ()
           (set! print-convert-result
                 (parameterize ([p:current-print-convert-hook
                                 (lambda (v basic-convert sub-convert)
                                   (if (image? v)
                                       v
                                       (basic-convert v)))])
                   (p:print-convert val)))
           (semaphore-post stepper-return-val-semaphore)))
        (semaphore-wait stepper-return-val-semaphore)
        print-convert-result)))
  
  (define (read-next-expr)
    (send-to-user-eventspace
     (lambda ()
       (let/ec k
         (let ([exception-handler (make-exception-handler k)])
           (d:interface:set-zodiac-phase 'reader)
           (let* ([new-expr (with-handlers
                                ((exn:read? exception-handler))
                              (reader))])
             (if (z:eof? new-expr)
                 (begin
                   (send-to-drscheme-eventspace
                    (lambda ()
                      (i:receive-result (make-finished-result finished-exprs))))
                   'finished)
                 (let* ([new-parsed (if (z:eof? new-expr)
                                        #f
                                        (begin
                                          (d:interface:set-zodiac-phase 'expander)
                                          (with-handlers
                                              ((exn:syntax? exception-handler))
                                            (z:scheme-expand new-expr 'previous user-vocabulary))))])
                   (let*-values ([(annotated-list envs) (a:annotate (list new-expr) (list new-parsed) packaged-envs break #f)]
                                 [(annotated) (car annotated-list)])
                     (printf "annotated:~n~a~n" annotated)
                     (set! packaged-envs envs)
                     (set! current-expr new-parsed)
                     (check-for-repeated-names new-parsed exception-handler)
                     (let ([expression-result
                            (parameterize ([current-exception-handler exception-handler])
                              (user-primitive-eval annotated))])
                       (send-to-drscheme-eventspace
                        (lambda ()
                          (add-finished-expr expression-result)
                          (read-next-expr)))))))))))))

  (define (check-for-repeated-names expr exn-handler)
    (with-handlers
        ([exn:user? exn-handler]
         [exn:syntax? exn-handler])
      (when (z:define-values-form? expr)
        (for-each (lambda (name) 
                    (when (check-global-defined name)
                      (e:static-error expr
                             "name is already bound: ~s" name)))
                  (map z:varref-var (z:define-values-form-vars expr))))))
         
  (define (add-finished-expr expression-result)
    (let ([reconstructed (r:reconstruct-completed current-expr expression-result)])
      (set! finished-exprs (append finished-exprs (list reconstructed)))))
  
  (define held-expr no-sexp)
  (define held-redex no-sexp)
  
  (define (break mark-list break-kind returned-value-list)
    (let ([reconstruct-helper
           (lambda (finish-thunk)
             (send-to-drscheme-eventspace
              (lambda ()
                (let* ([reconstruct-pair
                        (r:reconstruct-current current-expr mark-list break-kind returned-value-list)]
                       [reconstructed (car reconstruct-pair)]
                       [redex (cadr reconstruct-pair)])
                  (finish-thunk reconstructed redex)))))])
      (case break-kind
        [(normal-break)
         (printf "entering normal-break~n")
         (when (not (r:skip-redex-step? mark-list))
           (printf "not skipping step~n")
           (reconstruct-helper 
            (lambda (reconstructed redex)
              (set! held-expr reconstructed)
              (set! held-redex redex)
              (continue-user-computation)))
           (suspend-user-computation))
         (printf "finished normal-break~n")]
        [(result-break)
         (when (not (or (r:skip-redex-step? mark-list)
                        (and (null? returned-value-list)
                             (eq? held-expr no-sexp))))
           (reconstruct-helper 
            (lambda (reconstructed reduct)
;              ; this invariant (contexts should be the same)
;              ; fails in the presence of unannotated code.  For instance,
;              ; currently (map my-proc (cons 3 empty)) goes to
;              ; (... <body-of-my-proc> ...), where the context of the first one is
;              ; empty and the context of the second one is (... ...).
;              ; so, I'll just disable this invariant test.
;              (when (not (equal? reconstructed held-expr))
;                (e:internal-error 'reconstruct-helper
;                                  "pre- and post- redex/uct wrappers do not agree:~nbefore: ~a~nafter~a"
;                                  held-expr reconstructed))
              (let ([result (make-before-after-result finished-exprs
                                                      held-expr
                                                      held-redex
                                                      reconstructed
                                                      reduct)])
                (set! held-expr no-sexp)
                (set! held-redex no-sexp)
                (i:receive-result result))))
           (suspend-user-computation))]
        [(double-break)
         ; a double-break occurs at the beginning of a let's body.
         (send-to-drscheme-eventspace
          (lambda ()
            (let* ([reconstruct-quintuple
                    (r:reconstruct-current current-expr mark-list break-kind returned-value-list)])
              (set! finished-exprs (append finished-exprs (car reconstruct-quintuple)))
              (when (not (eq? held-expr no-sexp))
                (e:internal-error 'break-reconstruction
                                  "held-expr not empty when a double-break occurred"))
              (i:receive-result (apply make-before-after-result 
                                       finished-exprs
                                       (cdr reconstruct-quintuple))))))
         (suspend-user-computation)]
        [else (e:internal-error 'break "unknown label on break")])))
  
  (define (handle-exception exn)
    (if (not (eq? held-expr no-sexp))
        (begin
          (printf "held-expr: ~a~n" held-expr)
          (i:receive-result (make-before-error-result finished-exprs held-expr held-redex (exn-message exn))))
        (begin
          (printf "no held sexp~n")
          (i:receive-result (make-error-result finished-exprs (exn-message exn))))))
           
  (define (make-exception-handler k)
    (lambda (exn)
      (send-to-drscheme-eventspace
       (lambda ()
         (handle-exception exn)))
      (k)))
  
  ; start the ball rolling with a "fake" user computation
  (send-to-user-eventspace
   (lambda ()
     (suspend-user-computation)
     (send-to-drscheme-eventspace
      read-next-expr)))

  ; result of invoking stepper-instance : (->)
  continue-user-computation)