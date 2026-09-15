#lang racket/base
(require "run-module-language-program.rkt"
         "eval-helpers-and-pref-init.rkt"
         (submod "stack-checkpoint.rkt" item->srcloc)
         "drracket-errortrace-key.rkt"
         racket/contract
         racket/serialize
         racket/match
         racket/gui/base
         racket/class
         racket/pretty
         racket/port
         racket/unit
         framework/preferences
         (prefix-in file: file/convertible)
         (prefix-in number-snip: framework/private/number-snip-size)
         errortrace/stacktrace)

#|

This file runs in a separate process created by DrRacket to run
Racket programs (when the `run-in-separate-process` option in the
language dialog is set).

It uses stdin and stdout to communicate with DrRacket, leaving stderr
for bugs in this code to hopefully have some useful debugging information.

|#



(define original-output-port (current-output-port))
(define original-error-port (current-error-port))

(define sending-sema (make-semaphore 1))
(define (send-msg msg)
  (define sm (serialize msg))
  (semaphore-wait sending-sema)
  (writeln sm original-output-port)
  (flush-output original-output-port)
  (semaphore-post sending-sema))

(file-stream-buffer-mode original-error-port 'none) ;; stderr isn't supposed to be used; it'll show error messages from bugs, tho

(define oprintf
  (λ args
    (apply fprintf original-error-port args)
    (flush-output original-error-port)))

(let ([o-e-h (exit-handler)])
  (exit-handler
   (λ (x)
     (close-output-port current-output-pipe-out)
     (close-output-port current-error-pipe-out)
     (close-output-port current-value-pipe-out)
     (custodian-shutdown-all user-custodian)
     (o-e-h x))))

(define debug-error-display-handler
  (let ([original-error-display-hander (error-display-handler)])
    (λ (str exn)
      (define srclocs1
        (if (exn? exn)
            (filter values (map cdr (continuation-mark-set->context (exn-continuation-marks exn))))
            '()))
      ;; supposed to be the stack from the continuation marks
      (define srclocs2
        (if (exn? exn)
            (map errortrace-stack-item->srcloc (continuation-mark-set->list (exn-continuation-marks exn) drracket-errortrace-key))
            '()))
      (define details (exn->error-display-handler-exn-details exn))
      (send-msg `("error-display-handler" ,(exn-message exn) ,srclocs1 ,srclocs2 ,details)))))

(define-values (current-output-pipe-in current-output-pipe-out) (make-pipe-with-specials))
(define-values (current-error-pipe-in current-error-pipe-out) (make-pipe-with-specials))
(define-values (current-value-pipe-in current-value-pipe-out) (make-pipe-with-specials))

(define (forward-output-back from-port name)
  (define chan (make-channel))
  (define bts (make-bytes 256))
  (define (send-something res)
    (cond
      [(procedure? res)
       (define spec (res #f #f #f #f)) ;; pass #f in for the source location as I believe it is ignored anyway?
       (send-msg `(,name ,spec))]
      [else
       (send-msg
        `(,name ,(if (= res (bytes-length bts))
                     bts
                     (subbytes bts 0 res))))]))
  (void
   (thread
    (λ ()
      (let loop ()
        (sync
         (handle-evt
          (read-bytes-avail!-evt bts from-port)
          (λ (res)
            (cond
              [(eof-object? res)
               ;;; stop forwarding data if the pipe is closed
               (void)]
              [else
               (send-something res)
               (loop)])))
         (handle-evt
          chan
          (λ (resp-chan)
            (let get-all-bytes-loop ()
              (define b (read-bytes-avail!* bts from-port))
              (cond
                [(equal? b 0)
                 (channel-put resp-chan (void))
                 (loop)]
                [else
                 (send-something b)
                 (get-all-bytes-loop)])))))))))
  (λ ()
    (define c (make-channel))
    (channel-put chan c)
    (channel-get c)))

(define wait-for-stdout-io (forward-output-back current-output-pipe-in "stdout"))
(define wait-for-stderr-io (forward-output-back current-error-pipe-in "stderr"))
(define wait-for-value-io (forward-output-back current-value-pipe-in "value"))

(define default-pretty-print-current-style-table (pretty-print-current-style-table))

;; ths function is an edited version of the language.rkt's printing support; compared to that:
;; - it doesn't support syntax objects (because they are not serializable)
;; - it doesn't support arbitrary snips (as it isn't clear how to marshall them)
;; - it doesn't support `to-snip-value?` / `value->snip` because nothing seems to use that API
;; - it supports only "print" mode (because the #lang languages probably should support only that mode in general)
;; - when in "print" mode, the fraction-style is apparently always 'mixed-fraction-e, so this code supports only that
(define (make-setup-printing-parameters/extras show-sharing insert-newlines)
  (define gave-up? #f)
  (define-syntax-rule
    (dyn name)
    (define name (if gave-up?
                     (string->symbol (format "~a-gave-up" 'name))
                     (dynamic-require 'pict 'name))))
  (define pict:convertible?
    (with-handlers ((exn:fail? (λ (exn)
                                 (set! gave-up? #t)
                                 (log-error (exn-message exn))
                                 (λ (val) #f))))
      (dynamic-require 'pict/convert 'pict-convertible?)))
  (define pict-convert (if gave-up?
                           'pict-convert-gave-up
                           (dynamic-require 'pict/convert 'pict-convert)))
  (dyn pict-width)
  (dyn pict-height)
  (dyn pict-ascent)
  (dyn pict-descent)
  (dyn draw-pict)
  (dyn convert-bounds-padding)
  (define (mk-pict-snip-args convertible)
    (define-values (l-pad t-pad r-pad b-pad) (apply values (convert-bounds-padding)))
    (define pict (pict-convert convertible))
    (define w (pict-width pict))
    (define aw (+ (abs w) l-pad r-pad))
    (define h (pict-height pict))
    (define ah (+ (abs h) t-pad b-pad))
    (define a (+ (pict-ascent pict) t-pad))
    (define d (+ (pict-descent pict) b-pad))
    (define rdc (new record-dc%))
    (send rdc set-smoothing 'aligned)
    (send rdc set-clipping-rect 0 0 aw ah)
    (draw-pict pict rdc
               (+ (if (negative? w) aw 0) l-pad)
               (+ (if (negative? h) ah 0) t-pad))
    (define recorded-datum (send rdc get-recorded-datum))
    (list "pict-snip" aw ah d a recorded-datum))

  (define convert-table-thread-cell (make-thread-cell #f))
  (define (get-convert-table)
    (unless (thread-cell-ref convert-table-thread-cell)
      (thread-cell-set! convert-table-thread-cell (make-weak-hasheq)))
    (thread-cell-ref convert-table-thread-cell))

  (define exact-prefix 'never)
  (define fraction-view (preferences:get 'framework:fraction-snip-style))
  (define number-size (number-snip:make-pretty-print-size #:exact-prefix exact-prefix
                                                          #:inexact-prefix (if (pretty-print-show-inexactness) 'always 'never)
                                                          #:fraction-view fraction-view))

  (define original-pretty-print-print-hook (pretty-print-print-hook))
  (define (drracket-pretty-print-print-hook value display? port)
    (define convert-table (get-convert-table))
    (cond
      [(not (port-writes-special? port)) (original-pretty-print-print-hook value display? port)]
      [(pict:convertible? value)
       (write-special (mk-pict-snip-args value) port)]
      [(and (number? value)
            (number-size value display? port))
       (write-special (list "number" value (if (pretty-print-show-inexactness) 'always 'never) fraction-view) port)]
      [(hash-ref convert-table value #f)
       =>
       (λ (backing-scale+bytes)
         (hash-remove! convert-table value)
         (write-special (cons "bitmap" backing-scale+bytes) port))]
      [else (original-pretty-print-print-hook value display? port)]))

  (define original-pretty-print-size-hook (pretty-print-size-hook))
  (define (drracket-pretty-print-size-hook value display? port)
    (define convert-table (get-convert-table))
    (cond
      [(not (port-writes-special? port)) (original-pretty-print-size-hook value display? port)]
      [(pict:convertible? value) 1]
      [(and (number? value) (number-size value display? port))]
      [(syntax? value) 1]
      [(hash-ref convert-table value #f)
       ;; this handler can be called multiple times per value
       ;; avoid building the png bytes more than once
       1]
      [(and (file:convertible? value)
            (file:convert value 'png@2x-bytes #f))
       =>
       (λ (converted)
         (hash-set! convert-table value (list 2 converted))
         1)]
      [(and (file:convertible? value)
            (file:convert value 'png-bytes #f))
       =>
       (λ (converted)
         (hash-set! convert-table value (list 1 converted))
         1)]
      [else (original-pretty-print-size-hook value display? port)]))

  (values
   (λ (thunk width)
     (parameterize ([pretty-print-pre-print-hook (λ (val port) (void))]
                    [pretty-print-post-print-hook (λ (val port) (void))]
                    [pretty-print-exact-as-decimal #f]
                    [pretty-print-depth #f]
                    [pretty-print-.-symbol-without-bars #f]
                    [pretty-print-show-inexactness #f]
                    [pretty-print-abbreviate-read-macros #t]
                    [pretty-print-current-style-table default-pretty-print-current-style-table]
                    [pretty-print-remap-stylable (λ (x) #f)]
                    [pretty-print-print-line
                     (lambda (line port offset width)
                       (when (and (number? width)
                                  (not (eq? 0 line)))
                         (newline port))
                       0)]
                    [pretty-print-columns width]
                    [pretty-print-size-hook drracket-pretty-print-size-hook]
                    [pretty-print-print-hook drracket-pretty-print-print-hook]
                    [print-graph show-sharing])
       (thunk)))
   drracket-pretty-print-size-hook
   drracket-pretty-print-print-hook))

(define user-break-parameterization
  (parameterize-break
   #t
   (current-break-parameterization)))

;; when running code inside DrRacket directly, this parameter is looked at
;; by the current-eval handler but here we don't set that handler, so we
;; just make a dummy parameter that's ignored
(define outermost (make-parameter #f))

;; these two hopeless functions are stub versions of the functions with the same names
;; in module-language.rkt. here we never have direct access to the interactions window,
;; so we just report the error and then kill the process
(define hopeless-escape (make-parameter "hopeless-escape uninitialized"))

(define (raise-hopeless-exception exn [suffix #f])
  (define escape (hopeless-escape))
  (unless escape (if exn (raise exn) (error "\nInteractions disabled")))
  (when exn ((error-display-handler) (exn-message exn) exn))
  (flush-output (current-output-port))
  (flush-output (current-error-port))
  (escape #t suffix))

(define (raise-hopeless-syntax-error . error-args)
  (with-handlers ([exn:fail? raise-hopeless-exception])
    (apply raise-syntax-error '|Module Language|
           error-args)))

(define user-custodian (make-custodian))
(define user-eventspace (parameterize ([current-custodian user-custodian])
                          (make-eventspace)))
(define drracket-determined-width (make-parameter 'infinity))

(define (drracket-current-print val)
  (unless (void? val)
    (define port
      (if (equal? (current-output-port) current-output-pipe-out)
          current-value-pipe-out
          (current-output-port)))
    (parameterize ([pretty-print-columns (drracket-determined-width)])
      (print val port))
    (newline port)))

(parameterize ([current-eventspace user-eventspace])
  (queue-callback
   (λ ()
     (error-display-handler debug-error-display-handler)
     (current-print drracket-current-print)
     (current-output-port current-output-pipe-out)
     (current-error-port current-error-pipe-out))))

(void
 (thread
  (λ ()
    (sync (eventspace-handler-thread user-eventspace))
    (exit 0))))

(define errortrace-annotate
  (let ()
    (define key-module-name 'drracket/private/drracket-errortrace-key)

    #;
    (define (special-source-handling-for-drr src)
      (define rep (drracket:rep:current-rep))
      (cond
        [rep
         (define defs (send rep get-definitions-text))
         (cond
           [(send rep port-name-matches? src)
            (send rep get-port-name)]
           [(send defs port-name-matches? src)
            (send defs get-port-name)]
           [else #f])]
        [(is-a? src editor<%>) src] ;; can we skip this? ....probably?
        [else #f]))
    ;; it isn't clear what the complex version is actually accomplishing!
    (define (special-source-handling-for-drr src) #f)
    (define with-mark (make-with-mark special-source-handling-for-drr))

    (define test-coverage-enabled (make-parameter #f))
    (define current-test-coverage-info (make-thread-cell #f))
    (define (test-coverage-point body expr phase) body)

    (define profile-key (gensym))
    (define profiling-enabled (make-parameter #f))
    (define (initialize-profile-point key name expr) (void))
    (define current-profile-info (make-thread-cell #f))
    (define (register-profile-start key) #f)
    (define (register-profile-done key start) (void))

    (define-values/invoke-unit/infer stacktrace/errortrace-annotate/key-module-name@)

    errortrace-annotate))

(define (send-finished-evaluation-message hopeless-exn-raised? suffix)
  (flush-output current-output-pipe-out)
  (flush-output current-error-pipe-out)
  (flush-output current-value-pipe-out)
  (wait-for-stdout-io)
  (wait-for-stderr-io)
  (wait-for-value-io)
  (send-msg `("finished-evaluation" ,hopeless-exn-raised? ,suffix)))

(let loop ()
  (define datum-in (read (current-input-port)))
  (cond
    [(eof-object? datum-in) (exit 0)]
    [else
     (match (deserialize datum-in)
       [(list "complete-program" pretty-print-width submodules-to-run annotations prefab-module-settings currently-open-files show-sharing insert-newlines defs-port-name path the-bytes)
        (parameterize ([current-eventspace user-eventspace])
          (queue-callback
           (λ ()
             (drracket-determined-width pretty-print-width)

             ;; these are the steps that the language.rkt does `on-execute`;
             ;; but parts commented out here because not all are supported
             (case annotations
               [(debug)
                ;; errortrace-annotate probably comes from this:
                #;(define-values/invoke-unit/infer stacktrace/errortrace-annotate/key-module-name@)
                (current-compile (make-debug-compile-handler/errortrace-annotate (current-compile) errortrace-annotate))
                #;
                (error-display-handler
                 (drracket:debug:make-debug-error-display-handler
                  (error-display-handler)))]

               #;
               [(debug/profile)
                (drracket:debug:profiling-enabled #t)
                (error-display-handler
                 (drracket:debug:make-debug-error-display-handler
                  (error-display-handler)))
                (current-eval (drracket:debug:make-debug-eval-handler (current-eval)))]

               #;
               [(test-coverage)
                (drracket:debug:test-coverage-enabled #t)
                (error-display-handler
                 (drracket:debug:make-debug-error-display-handler
                  (error-display-handler)))
                (current-eval (drracket:debug:make-debug-eval-handler (current-eval)))])

             ;; printing
             (let ()
               (define-values (my-setup-printing-parameters
                               drracket-pretty-print-size-hook
                               drracket-pretty-print-print-hook)
                 (make-setup-printing-parameters/extras show-sharing insert-newlines))

               (pretty-print-print-hook drracket-pretty-print-print-hook)
               (pretty-print-size-hook drracket-pretty-print-size-hook)
               (define first-time? (make-parameter #t))
               (global-port-print-handler
                (λ (value port [depth 0])
                  (define cols
                    (cond
                      [(not insert-newlines) 'infinity]
                      [(exact-integer? (print-value-columns)) (print-value-columns)]
                      [else (drracket-determined-width)]))

                  (my-setup-printing-parameters
                   (λ ()
                     (define (do-print) (pretty-print value port depth))
                     (cond
                       [(first-time?)
                        (define orig-pretty-print-print-line (pretty-print-print-line))
                        (define pppl
                          (if insert-newlines
                              ;; when drracket:module-language:drracket-determined-width
                              ;; is set, we need to compensate for the newline
                              ;; difference, so we do this to avoid that last newline
                              (if (equal? (drracket-determined-width) 'infinity)
                                  orig-pretty-print-print-line
                                  (λ (new-line-number port len cols)
                                    (when new-line-number
                                      (orig-pretty-print-print-line new-line-number port len cols))))
                              orig-pretty-print-print-line))
                        (parameterize ([pretty-print-columns cols]
                                       [pretty-print-print-line pppl]
                                       [first-time? #f])
                          (do-print))]
                       [else (do-print)]))
                   'infinity))))

             ;; this is module-language.rkt's on-execute
             ;; need to get `currently-open-files` from the drracket process
             (set-module-language-parameters
              prefab-module-settings
              #f ;; module-language-parallel-lock-client -- we don't support this
              currently-open-files)

             (define (get-reader)
               (λ (src port)
                 (define v
                   (parameterize ([read-accept-reader #t])
                     (read-syntax src port)))
                 (if (eof-object? v)
                     v
                     (namespace-syntax-introduce v))))
             (define repl-init-thunk (make-thread-cell #f))
             (define-values (hopeless-exn-raised? suffix)
               (let/ec escape
                 (parameterize ([hopeless-escape escape])
                   (define get-sexp/syntax/eof
                     (front-end/complete-program get-reader
                                                 path
                                                 (λ () #f) ;; get-pre-compiled
                                                 submodules-to-run
                                                 'drracket:init:system-eventspace ;; ignored when the-irl is #f
                                                 raise-hopeless-exception raise-hopeless-syntax-error
                                                 repl-init-thunk

                                                 void ;; call-set-irl-mcli-vec
                                                 ;; we don't need to set-irl-mcli-vec! because we'll get the
                                                 ;; drracket:submit-predicate via read-language, I believe

                                                 (let ([p (open-input-bytes the-bytes defs-port-name)])
                                                   (port-count-lines! p)
                                                   p)
                                                 #f ;; the-irl
                                                 ))

                   (run-some-user-code user-break-parameterization
                                       outermost
                                       pretty-print-width
                                       get-sexp/syntax/eof)

                   ;; this prompt is the same as in rep.rkt in evaluate-from-port
                   (call-with-continuation-prompt
                    (λ ()
                      (call-with-break-parameterization
                       user-break-parameterization
                       (λ ()
                         ;; this is the module language's front-end/finished-complete-program
                         (cond [(thread-cell-ref repl-init-thunk)
                                => (λ (t) (thread-cell-set! repl-init-thunk #f) (t))]))))
                    (default-continuation-prompt-tag)
                    (λ args (void)))
                   (values #f #f))))
             (send-finished-evaluation-message hopeless-exn-raised? suffix))))
        (loop)]
       [(list "interaction" pretty-print-width ints-port-name port-line port-col port-pos the-bytes)
        (parameterize ([current-eventspace user-eventspace])
          (queue-callback
           (λ ()
             (drracket-determined-width pretty-print-width)
             (define port (open-input-bytes the-bytes ints-port-name))
             (port-count-lines! port)
             (set-port-next-location! port port-line port-col port-pos)
             (define get-sexp/syntax/eof (front-end/interaction port))
             (run-some-user-code user-break-parameterization
                                 outermost
                                 pretty-print-width
                                 get-sexp/syntax/eof)
             (send-finished-evaluation-message #f #f))))
        (loop)])]))
