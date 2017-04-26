#lang racket/base

#|


Will not work with the definitions text surrogate interposition that
#lang allows. Need to deprecate that one

--> scribble (in at-exp-lib) uses it to add keybindings; should
    add support for that directly

--> it may take a long time for the read-language call to return (or one
    of the others, I suppose?) use a separate thread?

|#

(require framework
         racket/gui/base
         racket/class
         racket/contract
         racket/runtime-path
         racket/match
         racket/port
         syntax-color/racket-lexer
         (for-syntax racket/base))

(define recognized-read-language-symbol/c
  (or/c 'drracket:default-filters
        'drracket:default-extension
        'drracket:indentation
        'drracket:keystrokes
        'drracket:show-big-defs/ints-labels
        'drracket:submit-predicate
        'drracket:toolbar-buttons
        'drscheme:toolbar-buttons
        'drracket:opt-out-toolbar-buttons
        'drscheme:opt-out-toolbar-buttons
        'drracket:opt-in-toolbar-buttons
        'color-lexer
        'definitions-text-surrogate))

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

  [call-read-language (-> irl? recognized-read-language-symbol/c any/c any)]

  ;; returns the part of the port that contributed to the actual language name
  ;; so, the first number is how many characters were comments and the
  ;; second is how of the following part of the port got read
  ;; (unlike positions in a port, these count from 0)
  [get-read-language-port-start+end
   (-> irl?
       (values (or/c #f exact-nonnegative-integer?)
               (or/c #f exact-nonnegative-integer?)))]
  [get-read-language-last-position (-> irl? (or/c #f exact-nonnegative-integer?))]

  [get-read-language-name (-> irl? (or/c #f string?))]

  [get-insulated-module-lexer (-> irl? (procedure-arity-includes/c 3))]
  [get-definitions-text-surrogate (-> irl? (or/c object? #f))]

  [set-irl-mcli-vec! (-> irl? (or/c mcli? #f) void?)]
  [get-insulated-submit-predicate (-> irl? (or/c #f (procedure-arity-includes/c 2)))]
  
  ;; supplies a callback that's invoked when an error happens that disables the `irl`.
  ;; the path is used for the current-load-relative-directory
  [make-irl (-> path-string? (-> exn:fail? any) irl?)]


  ;; call to let the abstraction know that it needs a new `read-language`
  ;; result. If the boolean is #t, then it jettisons all cached loaded
  ;; modules too (otherwise, it re-uses them).
  [reset-irl! (-> irl? port? path-string? boolean? void?)])

 skip-past-comments
 mcli?)

(struct irl ([namespace #:mutable]
             [prefs-layer #:mutable]
             [use-evaluator? #:mutable]
             [directory #:mutable]
             fail-callback))

(define (get-read-language-port-start+end an-irl)
  (call-irl-proc an-irl
                 (λ () (values #f #f))
                 'get-read-language-port-start+end/inside))

(define (get-read-language-name an-irl)
  (call-irl-proc an-irl
                 (λ () #f)
                 'get-read-language-name/inside))

(define (get-read-language-last-position an-irl)
  (call-irl-proc an-irl
                 (λ () #f)
                 'get-read-language-last-position/inside))

(define (3arg-racket-lexer in offset mode)
  (define-values (a b c d e) (racket-lexer in))
  (values a b c d e 0 #f))

(define (get-insulated-module-lexer an-irl)
  (define module-lexer
    (call-irl-proc an-irl
                   (λ () 3arg-racket-lexer)
                   'get-insulated-module-lexer/inside))
  (λ (in offset mode)
    (call-in-irl-context/abort
     an-irl
     (λ () (3arg-racket-lexer in offset mode))
     (λ () (module-lexer in offset mode)))))

(define (get-definitions-text-surrogate an-irl)
  (call-irl-proc an-irl
                 (λ () #f)
                 'get-definitions-text-surrogate/inside))

(define mcli? (vector/c module-path? symbol? any/c #:flat? #t))
(define (get-insulated-submit-predicate an-irl)
  (define submit-predicate
    (call-irl-proc an-irl
                   (λ () #f)
                   'get-submit-predicate/inside))
  (and submit-predicate
       (λ (port only-whitespace-after-insertion-point?)
         (call-in-irl-context/abort
          an-irl
          (λ () only-whitespace-after-insertion-point?)
          (λ () (submit-predicate port only-whitespace-after-insertion-point?))))))

(define (set-irl-mcli-vec! an-irl mcli/f)
  (call-irl-proc an-irl
                 void
                 'set-irl-mcli-vec!/inside
                 mcli/f))

(define (call-in-irl-context/abort an-irl fallback-thunk thunk)
  (match-define (irl namespace pref-layer use-evaluator? directory failure) an-irl)
  (cond
    [use-evaluator?
     (parameterize ([current-directory directory]
                    [current-load-relative-directory directory]
                    [current-namespace namespace]
                    [preferences:current-layer pref-layer])
       (let/ec k
         (call-with-exception-handler
          (λ (exn)
            (cond
              [(exn:fail? exn)
               (failure exn)
               (set-irl-use-evaluator?! an-irl #f)
               (k (fallback-thunk))]
              [else exn]))
          thunk)))]
    [else (fallback-thunk)]))

(define (call-read-language an-irl key default)
  (define val 
    (call-irl-proc an-irl
                   (λ () default)
                   'call-read-language/inside key default))
  (case key
    [(color-lexer)
     (cond
       [(procedure-arity-includes? val 3)
        (λ (port offset mode)
          (call-in-irl-context/abort
           an-irl
           (λ ()
             (let-values ([(a b c d e) (racket-lexer (open-input-string "\""))])
               (values a b c d e #f 0)))
           (λ ()
             (val port offset mode))))]
       [else
        (λ (port)
          (call-in-irl-context/abort
           an-irl
           (λ () (racket-lexer (open-input-string "\"")))
           (λ ()
             (val port))))])]
    [(drracket:submit-predicate)
     (and val
          (λ (port only-whitespace?)
            (call-in-irl-context/abort
             an-irl
             (λ () only-whitespace?)
             (λ () (val port only-whitespace?)))))]
    [(drracket:indentation)
     (and val
          (λ (txt pos)
            (call-in-irl-context/abort
             an-irl
             (λ () #f)
             (λ () (val txt pos)))))]
    [(drracket:keystrokes)
     (for/list ([pr (in-list val)])
       (define key (list-ref pr 0))
       (define proc (list-ref pr 1))
       (list key (λ (txt evt)
                   (call-in-irl-context/abort
                    an-irl
                    void
                    (λ () (proc txt evt))))))]
    [else
     val]))
  
(define (reset-irl! an-irl port path flush-cache?)
  (when flush-cache?
    (set-irl-namespace! an-irl (make-irl-namespace))
    (set-irl-prefs-layer! an-irl (preferences:new-layer original-preferences-layer)))
  (set-irl-use-evaluator?! an-irl #t)
  (set-irl-directory! an-irl path)
  (call-irl-proc an-irl
                 void
                 'reset-irl!/inside port))

(define (call-irl-proc an-irl fallback proc . args)
  (call-in-irl-context/abort
   an-irl
   fallback
   (λ () (apply (dynamic-require in-irl-namespace.rkt proc) args))))

(define original-preferences-layer (preferences:current-layer))
(define (make-irl directory callback)
  (irl (make-irl-namespace)
       (preferences:new-layer original-preferences-layer)
       #f
       directory
       callback))

(define (make-irl-namespace)
  (define ns (make-base-empty-namespace))
  (define trusted-namespace (current-namespace))
  (parameterize ([current-namespace ns])
    (namespace-attach-module trusted-namespace 'drracket/tool-lib))
  ns)

(define (pick-new-language text all-languages module-language
                           module-language-settings)
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
                      (send text delete 0
                            (send text paragraph-start-position lines)))))))))
      
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
      (looks-like-old-module-style? text)))

(define (looks-like-old-module-style? text)
  (with-handlers ([exn:fail:read? (λ (x) #f)])
    (define tp (open-input-text-editor text 0 'end (lambda (s) s) text #t))
    (define r1 (parameterize ([read-accept-reader #f]) (read tp)))
    (define r2 (parameterize ([read-accept-reader #f]) (read tp)))
    (and (eof-object? r2)
         (pair? r1)
         (eq? (car r1) 'module))))

(define (looks-like-new-module-style? text)
  (looks-like-new-module-style?/port
    (open-input-text-editor text 0 'end (lambda (s) s) text #t)))

(define (looks-like-new-module-style?/port special-tp)
  (define (special-filter f bytes)
    ;; @ is not accepted anywhere in either of the regexps below
    (bytes-set! bytes 0 (char->integer #\@))
    1)
  (define tp (special-filter-input-port special-tp special-filter))
  (skip-past-comments tp)
  (or (regexp-match? #rx"^#lang " (peeking-input-port tp))
      (regexp-match? #rx"^#![a-zA-Z0-9+-_]" tp)))

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
            [(eof-object? p1) (void)]
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
        (let/ec k
          (with-handlers ([exn:fail:read? (λ (x) (k (void)))])
            (read port))
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

(define-runtime-path in-irl-namespace.rkt '(lib "in-irl-namespace.rkt" "drracket" "private"))

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
  (check-equal? (clear-em "#|") '()) ;; make sure this terminates

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
    (check-equal? (clear-em in) '((x) #\newline #\1)))

  
  (check-equal? (let ([t (new text%)])
                  (send t insert "#lang racket/base")
                  (looks-like-new-module-style? t))
                #t)
  (check-equal? (let ([t (new text%)])
                  (send t insert "#ang racket/base")
                  (looks-like-new-module-style? t))
                #f)
  (check-equal? (let ([t (new text%)])
                  (send t insert ";; abc\n#lang racket/base")
                  (looks-like-new-module-style? t))
                #t)
  (check-equal? (let ([t (new text%)])
                  (send t insert ";; abc\n #lang racket/base")
                  (looks-like-new-module-style? t))
                #t)
  (check-equal? (let ([t (new text%)])
                  (send t insert ";; abc\n #!r6rs")
                  (looks-like-new-module-style? t))
                #t)

  (let ()
    (define-values (in out) (make-pipe-with-specials))
    (thread
     (λ ()
       (write-special '(x) out)
       (display "\n1" out)
       (close-output-port out)))
    (check-false (looks-like-new-module-style?/port in)))

  (check-false (looks-like-new-module-style?/port
                (open-input-string "(module m racket/base")))
  (check-true (looks-like-new-module-style?/port
               (open-input-string "#lang racket/base")))
  (check-true (looks-like-new-module-style?/port (open-input-string "#!r")))
  (check-false (looks-like-new-module-style?/port (open-input-string "#langg")))
  (check-false (looks-like-new-module-style?/port (open-input-string "#la"))))
