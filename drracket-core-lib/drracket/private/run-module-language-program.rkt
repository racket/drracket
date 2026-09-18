#lang racket/base
(require (submod "stack-checkpoint.rkt" with-stack-checkpoint)
         "eval-helpers-and-pref-init.rkt"
         (submod "insulated-read-language.rkt" mcli)
         racket/gui/base
         racket/pretty)

#|

This file contains helper routines used by the module-language to read and run
a program. It also gets used when drracket runs a program in a separate process.

|#

(provide run-some-user-code
         front-end/complete-program
         front-end/interaction)

;; this is expected to be called on the user's thread to run the code in
;; the definitions window or in the interactions window; it is called from
;; evaluate-from-port and abstracted here to be shared in user-in-separate-process.rkt
(define (run-some-user-code user-break-parameterization outermost pretty-print-width
                            get-sexp/syntax/eof)
  ; Evaluate the user's expression. We're careful to turn on
  ;   breaks as we go in and turn them off as we go out.
  ;   (Actually, we adjust breaks however the user wanted it.)

  ;; this binding of last-results is to catch the results
  ;; that come from throwing to the prompt instead of
  ;; a normal exit
  (define last-results
    (call-with-values
     (λ ()
       (call-with-continuation-prompt
        (λ ()
          (call-with-break-parameterization
           user-break-parameterization
           (λ ()
             (let loop ()
               (define sexp/syntax/eof (with-stack-checkpoint (get-sexp/syntax/eof)))
               (cond
                 [(eof-object? sexp/syntax/eof) (abort-current-continuation
                                                 (default-continuation-prompt-tag)
                                                 (λ () (values)))]
                 [else
                  (define results
                    (call-with-values
                     (λ ()
                       (parameterize ([outermost #t])
                         (with-stack-checkpoint
                             (eval-syntax sexp/syntax/eof))))
                     list))
                  (parameterize ([pretty-print-columns pretty-print-width])
                    (for ([x (in-list results)])
                      ((current-print) x)))
                  (loop)])))))
        (default-continuation-prompt-tag)
        (letrec ([me
                  (λ args
                    (cond
                      [(and (pair? args)
                            (null? (cdr args))
                            (procedure? (car args))
                            (procedure-arity-includes? (car args) 0))
                       (call-with-continuation-prompt (car args)
                                                      (default-continuation-prompt-tag)
                                                      me)]
                      [else
                       (call-with-continuation-prompt
                        (λ ()
                          (call-with-continuation-prompt
                           (λ ()
                             (apply
                              abort-current-continuation
                              (default-continuation-prompt-tag)
                              args)))))]))])
          me)))
     list))
  (parameterize ([pretty-print-columns pretty-print-width])
    (for ([x (in-list last-results)])
      ((current-print) x))))

(define (front-end/complete-program get-reader path get-pre-compiled submodules-to-run
                                    drracket:init:system-eventspace
                                    raise-hopeless-exception raise-hopeless-syntax-error repl-init-thunk call-set-irl-mcli-vec
                                    port [the-irl #f])
  (define (super-thunk)
    (define reader (get-reader))
    (reader (object-name port) port))
  (define resolved-modpath (and path (module-path-index-resolve
                                      (module-path-index-join
                                       path
                                       #f))))

  (define-values (name lang module-expr)
    (cond
      [(get-pre-compiled)
       =>
       (λ (transform-module-results)
         (define compiled-expression
           (parameterize ([read-accept-compiled #t])
             (read (open-input-bytes (vector-ref transform-module-results 2)))))
         (values
          (vector-ref transform-module-results 0)
          (vector-ref transform-module-results 1)
          (with-syntax ([x compiled-expression]) #'x)))]
      [else
       (define expr
         ;; just reading the definitions might be a syntax error,
         ;; possibly due to bad language (eg, no foo/lang/reader)
         (with-handlers ([exn:fail? (λ (e) (raise-hopeless-exception e))])
           (super-thunk)))
       (when (eof-object? expr)
         (raise-hopeless-syntax-error (string-append
                                       "There must be a valid module in the\n"
                                       "definitions window.  Try starting your program with\n"
                                       "\n"
                                       "  #lang racket\n"
                                       "or\n"
                                       "  #lang htdp/bsl\n"
                                       "\n"
                                       "and clicking ‘Run’.")))
       (let ([more (super-thunk)])
         (unless (eof-object? more)
           (raise-hopeless-syntax-error
            "there can only be one expression in the definitions window"
            more)))
       (transform-module path expr raise-hopeless-syntax-error)]))

  (define modspec (or path `',name))
  (define (check-interactive-language)
    (unless (memq '#%top-interaction (namespace-mapped-symbols))
      (raise-hopeless-exception
       #f ; no error message, just a suffix
       (format "~s does not support a REPL (no #%top-interaction)"
               lang))))
  ;; We're about to send the module expression to drracket now, the rest
  ;; of the setup is done in `front-end/finished-complete-program' below,
  ;; so use `repl-init-thunk' to store an appropriate continuation for
  ;; this setup.  Once we send the expression, we'll be called again only
  ;; if it was evaluated (or expanded) with no errors, so begin with a
  ;; continuation that deals with an error, and if we're called again,
  ;; change it to a continuation that initializes the repl for the
  ;; module.  So the code is split among several thunks that follow.
  (define (*pre)
    (thread-cell-set! repl-init-thunk *error)
    (current-module-declare-name resolved-modpath)
    (current-module-declare-source path))
  (define (*post)
    (current-module-declare-name #f)
    (current-module-declare-source #f)
    (when path ((current-module-name-resolver) resolved-modpath #f))
    (thread-cell-set! repl-init-thunk *init))
  (define (*error)
    (current-module-declare-name #f)
    (current-module-declare-source #f)
    ;; syntax error => try to require the language to get a working repl
    (with-handlers ([void (λ (e)
                            (raise-hopeless-syntax-error
                             "invalid language specification"
                             lang))])
      (namespace-require lang))
    (check-interactive-language))
  (define (*init)
    (parameterize ([current-namespace (current-namespace)])
      ;; the prompt makes it continue after an error
      (call-with-continuation-prompt
       (λ () (with-stack-checkpoint 
                 (begin
                   (*do-module-specified-configuration)
                   (namespace-require modspec)
                   (for ([submod (in-list submodules-to-run)])
                     (define submod-spec `(submod ,modspec ,@submod))
                     (when (module-declared? submod-spec)
                       (dynamic-require submod-spec #f))))))))
    (current-namespace (module->namespace modspec))
    (check-interactive-language))
  (define (*do-module-specified-configuration)
    (define info (module->language-info modspec #t))
    (unless (mcli? info) (set! info #f))
    (call-set-irl-mcli-vec info)
    (when info
      (let ([get-info
             ((dynamic-require (vector-ref info 0)
                               (vector-ref info 1))
              (vector-ref info 2))])
        (let ([configs (get-info 'configure-runtime '())])
          (for ([config (in-list configs)])
            ((dynamic-require (vector-ref config 0)
                              (vector-ref config 1))
             (vector-ref config 2))))))
    (define cr-submod `(submod ,modspec configure-runtime))
    (when (module-declared? cr-submod)
      (dynamic-require cr-submod #f)))
  ;; here's where they're all combined with the module expression
  (expr-getter *pre module-expr *post))

(define (front-end/interaction port)
  (λ ()
    (let ([v (parameterize ([read-accept-reader #t]
                            [read-accept-lang #f])
               (with-stack-checkpoint
                   ((current-read-interaction) 
                    (object-name port)
                    port)))])
      (if (eof-object? v)
          v
          (let ([w (cons '#%top-interaction v)])
            (if (syntax? v)
                (namespace-syntax-introduce
                 (datum->syntax #f w v))
                v))))))

;; utility for the front-end method: return a function that will return
;; each of the given syntax values on each call, executing thunks when
;; included; when done with the list, send eof.
(define (expr-getter . exprs/thunks)
  (define (loop)
    (if (null? exprs/thunks)
        eof
        (let ([x (car exprs/thunks)])
          (set! exprs/thunks (cdr exprs/thunks))
          (if (procedure? x) (begin (x) (loop)) x))))
  loop)