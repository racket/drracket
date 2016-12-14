#lang racket/base

#|


Will not work with the definitions text surrogate interposition that
#lang allows. Need to deprecate that one!

--> scribble (in at-exp-lib) uses it to add keybindings; should
    add support for that directly

--> in documentation for 

|#

(require framework
	 racket/gui/base
         racket/class
         racket/contract
         racket/sandbox
         racket/runtime-path
         racket/match
         syntax-color/racket-lexer
         (for-syntax racket/base))

(provide
 (contract-out
  #:∀ S
  [pick-new-language
   (-> (is-a?/c text%)
       (listof (object/c [metadata->settings (->m string? S)]))
       (or/c #f object?)
       S
       (values (or/c #f object?)
               (or/c S #f)))]
  [looks-like-module?
   (-> (is-a?/c text%) boolean?)]

  [call-read-language (-> irl? symbol? any/c any)]

  ;; returns the part of the port that contributed to the actual language name
  ;; so, the first number is how many characters were comments and the
  ;; second is how of the following part of the port got read
  ;; (unlike positions in a port, these count from 0)
  [get-read-language-port-start+end
   (-> irl?
       (values exact-nonnegative-integer?
               (or/c #f exact-nonnegative-integer?)))]

  [get-insulated-module-lexer (-> irl? any/c)]
  [get-insulated-indentation-function (-> irl? (procedure-arity-includes/c 2))]
  [get-definitions-text-surrogate
   (-> irl? (or/c object? #f))]
  
  ;; supplies a callback that's invoked when the internal sandbox dies
  [make-irl (-> (-> exn:fail? any) irl?)]


  ;; call to let the abstraction know that it needs a new `read-language`
  ;; result. If the boolean is #t, then it jettisons all cached loaded
  ;; modules too (otherwise, it re-uses them).
  [reset-irl! (-> irl? port? boolean? void?)])

 skip-past-comments)

(struct irl ([evaluator #:mutable] [use-evaluator? #:mutable] fail-callback))

(define (get-read-language-port-start+end an-irl)
  (call-sandbox-proc an-irl
                     (λ () (values 0 #f))
                     'get-read-language-port-start+end/sandbox))

(define (3arg-racket-lexer in offset mode)
  (define-values (a b c d e) (racket-lexer in))
  (values a b c d e 0 #f))

(define (get-insulated-module-lexer an-irl)
  (define module-lexer
    (call-sandbox-proc an-irl
                       (λ () 3arg-racket-lexer)
                       'get-insulated-module-lexer/sandbox))
  (λ (in offset mode)
    (call-in-sandbox-context/abort
     an-irl
     (λ () (3arg-racket-lexer in offset mode))
     (λ () (module-lexer in offset mode)))))

(define (get-insulated-indentation-function an-irl)
  (define indent (call-read-language an-irl 'drracket:indentation #f))
  (cond
    [indent
     (λ (txt pos)
       ;; this isn't really safe since the txt can be
       ;; modified by the insulated language callback
       (call-in-sandbox-context/abort
        an-irl
        (λ () #f)
        (λ () (indent txt pos))))]
    [else (λ (x y) #f)]))

(define (get-definitions-text-surrogate an-irl)
  (call-sandbox-proc an-irl
                     (λ () #f)
                     'get-definitions-text-surrogate/sandbox))

(define (call-in-sandbox-context/abort an-irl fallback-thunk thunk)
  (match-define (irl evaluator use-evaluator? failure) an-irl)
  (cond
    [use-evaluator?
     (with-handlers ([exn:fail?
                      (λ (exn)
                        (failure exn)
                        (set-irl-use-evaluator?! an-irl #f)
                        (fallback-thunk))])
       (call-in-sandbox-context
        evaluator
        thunk))]
    [else
     (fallback-thunk)]))

(define (call-read-language an-irl key default)
  (define val
    (call-sandbox-proc an-irl
                       (λ ()
                         ((read-language (open-input-string "#lang racket")) key default))
                       'call-read-language/sandbox key default))
  (case key
    [(color-lexer)
     (cond
       [(procedure-arity-includes? val 3)
        (λ (port offset mode)
          (call-in-sandbox-context/abort
           an-irl
           (λ ()
             (let-values ([(a b c d e) (racket-lexer (open-input-string "\""))])
               (values a b c d e #f 0)))
           (λ ()
             (val port offset mode))))]
       [else
        (λ (port)
          (call-in-sandbox-context/abort
           an-irl
           (λ () (racket-lexer (open-input-string "\"")))
           (λ ()
             (val port))))])]
    [(drracket:submit-predicate)
     (λ (port only-whitespace?)
       (call-in-sandbox-context/abort
        an-irl
        (λ () #t)
        (λ ()
          (val port only-whitespace?))))]
    [(drracket:show-big-defs/ints-labels)
     ;; contract: any/c
     val]
    [(drracket:default-filters)
     ;; contract: (or/c #f (listof (list/c string? string?)))
     val]
    [(drracket:default-extension)
     ;; contract: (or/c #f (and/c string? (not/c #rx"[.]")))
     val]
    [(drracket:indentation)
     ;; contract:
     #;
     (or/c #f
           (-> (is-a?/c racket:text<%>) exact-nonnegative-integer?
               (or/c #f exact-nonnegative-integer?)))
     val]

    [(drracket:toolbar-buttons drscheme:toolbar-buttons)
     ;; contract:
     #;
     (or/c #f (listof (or/c (list/c string?
                                    (is-a?/c bitmap%)
                                    (-> (is-a?/c drracket:unit:frame<%>) any))
                            (list/c string?
                                    (is-a?/c bitmap%)
                                    (-> (is-a?/c drracket:unit:frame<%>) any)
                                    (or/c real? #f)))))
     val]

    [(drracket:opt-out-toolbar-buttons drscheme:opt-out-toolbar-buttons)
     ;; contract: (or/c #f (listof symbol?))
     val]
    [else
     (eprintf "unknown key: ~s\n" key)
     val]))

(define (reset-irl! an-irl port flush-cache?)
  (when flush-cache?
    (set-irl-evaluator! (make-irl-evaluator)))
  (set-irl-use-evaluator?! an-irl #t)
  (call-sandbox-proc an-irl
                     void
                     'reset-irl!/sandbox port))

(define (call-sandbox-proc an-irl fallback proc . args)
  (call-in-sandbox-context/abort
   an-irl
   fallback
   (λ ()
     (apply (dynamic-require `(submod ,insulated-read-language.rkt inside-sandbox) proc)
            args))))

(define (make-irl callback)
  (irl (make-irl-evaluator callback)
       #f
       callback))

(define (make-irl-evaluator callback)
  (define (disallow-modification proc path modes)
    (define allowed?
      (for/and ([mode (in-list modes)])
        (not (memq mode '(write delete)))))
    (unless allowed?
      (error proc "drracket IDE modification code not allowed to access ~a with modes ~a"
             path modes)))
  (define (disallow-network proc host port c/s)
    (error proc
           (string-append
            "drracket IDE modification code not allowed to access the network; "
            "tried to connect to ~a:~a as a ~a"
            host port c/s)))
  (define e
    (parameterize ([sandbox-output (current-output-port) #;'bytes]
                   [sandbox-error-output (current-error-port) #;'bytes]
                   [sandbox-make-code-inspector
                    (let ([code-inspector (current-code-inspector)])
                      (λ () code-inspector))]
                   [sandbox-security-guard
                    (make-security-guard
                     (current-security-guard)
                     disallow-modification
                     disallow-network)])
      (make-evaluator '(begin))))

  (define dead-evt (make-custodian-box (get-user-custodian e) #f))
  (thread
   (λ ()
     (sync dead-evt)
     (let loop ([n 10])
       (cond
         [(= n 0)
          (error 'insulated-read-language.rkt
                 "custodian of the evaluator shut down, but evaluator-alive? returns #t")]
         [(evaluator-alive? e)
          (sleep)
          (loop)]
         [else
          (define reason
            (with-handlers ([exn:fail:sandbox-terminated? values])
              (e 'this-should-be-ignored-because-the-evaluator-has-terminated)))
          (queue-callback
           (λ ()
             (callback reason)))]))))
  
  (define trusted-namespace (current-namespace))
  (call-in-sandbox-context
   e
   (λ ()
     (namespace-attach-module trusted-namespace 'drracket/tool-lib)))
  e)

(define (pick-new-language text all-languages module-language module-language-settings)
  (with-handlers ([exn:fail:read? (λ (x) (values #f #f))])
    (define found-language? #f)
    (define settings #f)
    (for ([lang (in-list all-languages)])
      (define lang-spec (send lang get-reader-module))
      (when lang-spec
        (let* ([lines (send lang get-metadata-lines)]
               [str (send text get-text
                          0
                          (send text paragraph-end-position (- lines 1)))]
               [sp (open-input-string str)])
          (when (regexp-match #rx"#reader" sp)
            (define spec-in-file (read sp))
            (when (equal? lang-spec spec-in-file)
              (set! found-language? lang)
              (set! settings (send lang metadata->settings str))
              (send text while-unlocked
                    (λ () 
                      (send text delete 0 (send text paragraph-start-position lines)))))))))
      
    ;; check to see if it looks like the module language.
    (unless found-language?
      (when module-language
        (when (looks-like-module? text)
          (set! found-language? module-language)
          (set! settings module-language-settings))))
    (values found-language?
            settings)))

(define (looks-like-module? text)
  (or (looks-like-new-module-style? text)
      (looks-like-old-module-style? text)
      (with-handlers ([exn:fail? (λ (x) #f)])
        (read-language (open-input-text-editor text 0 'end (λ (x) x) text #f) 
                       (λ () #f))
        #t)))

(define (looks-like-old-module-style? text)
  (with-handlers ([exn:fail:read? (λ (x) #f)])
    (define tp (open-input-text-editor text 0 'end (lambda (s) s) text #t))
    (define r1 (parameterize ([read-accept-reader #f]) (read tp)))
    (define r2 (parameterize ([read-accept-reader #f]) (read tp)))
    (and (eof-object? r2)
         (pair? r1)
         (eq? (car r1) 'module))))

(define (looks-like-new-module-style? text)
  (define tp (open-input-text-editor text 0 'end (lambda (s) s) text #t))
  (define l1 (with-handlers ([exn:fail? (lambda (exn) eof)])
               ;; If tp contains a snip, read-line fails.
               (read-line tp)))
  (and (string? l1)
       (regexp-match? #rx"#lang .*$" l1)))


(module skip-past-comments racket/base
  (provide skip-past-comments)
  (require (for-syntax racket/base))
  (define (skip-past-comments port)
    (define (get-it str)
      (for ([c1 (in-string str)])
        (define c2 (read-char-or-special port))
        (unless (equal? c1 c2)
          (error 'get-it
                 "expected ~s, got ~s, orig string ~s"
                 c1 c2 str))))
    (let loop ()
      (define p (peek-char-or-special port))
      (cond-strs
       port
       [";"
        (let loop ()
          (define c (read-char-or-special port))
          (case c
            [(#\linefeed #\return #\u133 #\u8232 #\u8233)
             (void)]
            [else
             (unless (eof-object? c)
               (loop))]))
        (loop)]
       ["#|"
        (let loop ([depth 0])
          (define p1 (peek-char-or-special port))
          (cond
            [(and (equal? p1 #\|)
                  (equal? (peek-char-or-special port 1) #\#))
             (get-it "|#")
             (cond
               [(= depth 0) (void)]
               [else (loop (- depth 1))])]
            [(and (equal? p1 #\#)
                  (equal? (peek-char-or-special port 1) #\|))
             (get-it "#|")
             (loop (+ depth 1))]
            [else
             (read-char-or-special port)
             (loop depth)]))
        (loop)]
       ["#;"
        (with-handlers ((exn:fail:read? void))
          (read port)
          (loop))]
       ["#! "
        (read-line-slash-terminates port)
        (loop)]
       ["#!/"
        (read-line-slash-terminates port)
        (loop)]
       [else
        (define p (peek-char-or-special port))
        (cond
          [(eof-object? p) (void)]
          [(and (char? p) (char-whitespace? p))
           (read-char-or-special port)
           (loop)]
          [else (void)])])))


  (define-syntax (cond-strs stx)
    (syntax-case stx (else)
      [(_ port [chars rhs ...] ... [else last ...])
       (begin
         (for ([chars (in-list (syntax->list #'(chars ...)))])
           (unless (string? (syntax-e chars))
             (raise-syntax-error 'chars "expected a string" stx chars))
           (for ([char (in-string (syntax-e chars))])
             (unless (< (char->integer char) 128)
               (raise-syntax-error 'chars "expected only one-byte chars" stx chars))))
         #'(cond
             [(check-chars port chars)
              rhs ...]
             ...
             [else last ...]))]))

  (define (check-chars port chars)
    (define matches?
      (for/and ([i (in-naturals)]
                [c (in-string chars)])
        (equal? (peek-char-or-special port i) c)))
    (when matches?
      (for ([c (in-string chars)])
        (read-char-or-special port)))
    matches?)

  (define (read-line-slash-terminates port)
    (let loop ([previous-slash? #f])
      (define c (read-char-or-special port))
      (case c
        [(#\\) (loop #t)]
        [(#\linefeed #\return)
         (cond
           [previous-slash?
            (define p (peek-char-or-special port))
            (when (and (equal? c #\return)
                       (equal? p #\linefeed))
              (read-char-or-special port))
            (loop #f)]
           [else
            (void)])]
        [else
         (unless (eof-object? c)
           (loop #f))]))))
(require (submod "." skip-past-comments))

(define-runtime-path insulated-read-language.rkt
  '(lib "insulated-read-language.rkt" "drracket" "private"))

(module inside-sandbox racket/base
  (require racket/match
           racket/class
           racket/contract/option
           (submod ".." skip-past-comments)
           profile)
  (provide reset-irl!/sandbox
           call-read-language/sandbox
           get-read-language-port-start+end/sandbox
           get-insulated-module-lexer/sandbox
           get-definitions-text-surrogate/sandbox)

  (define language-get-info #f)
  (define before-port-position 0)
  (define after-port-position #f)

  (define module-lexer #f)
  
  (define (get-insulated-module-lexer/sandbox)
    (unless module-lexer
      (set! module-lexer (waive-option (dynamic-require 'syntax-color/module-lexer 'module-lexer))))
    module-lexer)

  (define (get-definitions-text-surrogate/sandbox)
    (define surrogate
      (and language-get-info
           (language-get-info 'definitions-text-surrogate #f)))
    (and surrogate
         (new (dynamic-require surrogate 'surrogate%))))
  
  (define (reset-irl!/sandbox port)
    (skip-past-comments port)
    
    ;; initialize in case the `read-language` somehow goes
    ;; very wrong and we don't get them reset. these
    ;; values will always trigger a call to get us back
    ;; here to try again
    (set! before-port-position 0)
    (set! after-port-position #f)
    
    (define-values (_1 _2 before-pos) (port-next-location port))
    (set! language-get-info
          (with-handlers ([exn:fail? (λ (exn)
                                       (log-error
                                        (apply
                                         string-append
                                         (exn-message exn)
                                         "\n"
                                         (for/list ([l (in-list
                                                        (continuation-mark-set->context
                                                         (exn-continuation-marks
                                                          exn)))])
                                           (format "  ~s\n" l))))
                                       #f)])
            (read-language port)))
    (define-values (_3 _4 after-pos) (port-next-location port))
    (set! before-port-position (- before-pos 1))
    (set! after-port-position (- after-pos 1)))
  
  (define (call-read-language/sandbox key default)
    (if language-get-info
        (language-get-info key default)
        default))
  
  (define (get-read-language-port-start+end/sandbox)
    (values before-port-position after-port-position)))

(module+ test
  (require rackunit racket/port)
  (define (clear-em str)
    (define sp (if (port? str) str (open-input-string str)))
    (skip-past-comments sp)
    (for/list ([i (in-port read-char-or-special sp)])
      i))
  (check-equal? (clear-em ";") '())
  (check-equal? (clear-em ";\n1") '(#\1))
  (check-equal? (clear-em ";  \n1") '(#\1))
  (check-equal? (clear-em ";  \r\n1") '(#\1))
  (check-equal? (clear-em ";  \u8233\n1") '(#\1))
  (check-equal? (clear-em "         1") '(#\1))
  (check-equal? (clear-em "#| |#1") '(#\1))
  (check-equal? (clear-em "#| #| #| #| #| |# |# |# |# |#1") '(#\1))
  (check-equal? (clear-em "#| #| #| #| #| |# |# #| |# |# |# |#1") '(#\1))
  (check-equal? (clear-em "#||#1") '(#\1))
  (check-equal? (clear-em "#|#|#|#|#||#|#|#|#|#1") '(#\1))
  (check-equal? (clear-em "#|#|#|#|#||#|##||#|#|#|#1") '(#\1))
  (check-equal? (clear-em " #!    \n         1") '(#\1))
  (check-equal? (clear-em " #!/    \n         1") '(#\1))
  (check-equal? (clear-em " #!/    \\\n2\n         1") '(#\1))
  (check-equal? (clear-em " #!/    \\\r2\n         1") '(#\1))
  (check-equal? (clear-em " #!/    \\\r\n2\n         1") '(#\1))
  (check-equal? (clear-em " #!/    \n\r\n         1") '(#\1))
  (check-equal? (clear-em "#;()1") '(#\1))
  (check-equal? (clear-em "#;  (1 2 3 [] {} ;xx\n 4)  1") '(#\1))
  (check-equal? (clear-em "#||##|#lang rong|#1") '(#\1))

  (let ()
    (define-values (in out) (make-pipe-with-specials))
    (thread
     (λ ()
       (display ";" out)
       (write-special '(x) out)
       (display "\n1" out)
       (close-output-port out)))
    (check-equal? (clear-em in) '(#\1)))
  
  (let ()
    (define-values (in out) (make-pipe-with-specials))
    (thread
     (λ ()
       (write-special '(x) out)
       (display "\n1" out)
       (close-output-port out)))
    (check-equal? (clear-em in) '((x) #\newline #\1))))

