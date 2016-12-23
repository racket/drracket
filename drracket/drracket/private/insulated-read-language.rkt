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
        'drracket:show-big-defs/ints-labels
        'drracket:submit-predicate
        'drracket:toolbar-buttons
        'drscheme:toolbar-buttons
        'drracket:opt-out-toolbar-buttons
        'drscheme:opt-out-toolbar-buttons
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

(struct irl ([namespace #:mutable] [use-evaluator? #:mutable] [directory #:mutable] fail-callback))

(define (get-read-language-port-start+end an-irl)
  (call-irl-proc an-irl
                 (λ () (values #f #f))
                 'get-read-language-port-start+end/inside))

(define (get-read-language-name an-irl)
  (call-irl-proc an-irl
                 (λ () #f)
                 'get-read-language-name/inside))

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
  (match-define (irl namespace use-evaluator? directory failure) an-irl)
  (cond
    [use-evaluator?
     (parameterize ([current-directory directory]
                    [current-load-relative-directory directory])
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
    [else
     val]))
  
(define (reset-irl! an-irl port path flush-cache?)
  (when flush-cache?
    (set-irl-namespace! an-irl (make-irl-namespace)))
  (set-irl-use-evaluator?! an-irl #t)
  (set-irl-directory! an-irl path)
  (call-irl-proc an-irl
                 void
                 'reset-irl!/inside port))

(define (call-irl-proc an-irl fallback proc . args)
  (call-in-irl-context/abort
   an-irl
   fallback
   (λ ()
     (apply (dynamic-require `(submod ,insulated-read-language.rkt in-irl-namespace)
                             proc)
            args))))

(define (make-irl directory callback)
  (irl (make-irl-namespace)
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
  (define tp (open-input-text-editor text 0 'end (lambda (s) s) text #t))
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

(module in-irl-namespace racket/base
  (require racket/match
           racket/class
           racket/contract/option
           racket/contract
           racket/draw
           racket/port
           (submod ".." skip-past-comments)
           syntax/modread)
  (provide reset-irl!/inside
           call-read-language/inside
           get-read-language-port-start+end/inside
           get-read-language-name/inside
           get-insulated-module-lexer/inside
           get-definitions-text-surrogate/inside
           get-submit-predicate/inside
           set-irl-mcli-vec!/inside
           ;; for test suite
           compute-lang-info)

  (define language-get-info #f)
  (define before-port-position 0)
  (define after-port-position #f)
  (define mcli-vec #f)

  (define (set-irl-mcli-vec!/inside _mcli-vec) (set! mcli-vec _mcli-vec))

  (define (get-submit-predicate/inside)
    (or (and mcli-vec
             (add-contract
              'drracket:submit-predicate
              (key->contract 'drracket:submit-predicate)
              ((dynamic-require (vector-ref mcli-vec 0)
                                (vector-ref mcli-vec 1))
               (vector-ref mcli-vec 2))))
        (call-read-language/inside 'drracket:submit-predicate
                                   #f)))
  
  (define module-lexer #f)
  (define lang-name "<<unknown>>")
  
  (define (get-insulated-module-lexer/inside)
    (unless module-lexer
      (set! module-lexer (waive-option (dynamic-require 'syntax-color/module-lexer 'module-lexer))))
    module-lexer)

  (define (get-definitions-text-surrogate/inside)
    (define surrogate-module
      (and language-get-info
           (add-contract 'definitions-text-surrogate
                         (key->contract 'definitions-text-surrogate)
                         (language-get-info 'definitions-text-surrogate #f))))
    (and surrogate-module
         (new (add-contract 'definitions-text-surrogate
                            (implementation?/c
                             ;; the framework should be shared in the namespace
                             ;; with this module by the time we get here
                             (dynamic-require 'framework 'racket:text-mode<%>))
                            (dynamic-require surrogate-module 'surrogate%)))))

  (define-logger drracket-language)

  (define (compute-lang-info port)
    (skip-past-comments port)
    (define peeking-port (peeking-input-port port))
    (port-count-lines! peeking-port)
    
    (define-values (_1 _2 before-pos) (port-next-location port))
    (define language-get-info
      (with-module-reading-parameterization
       (λ ()
         (parameterize ([current-load-relative-directory (current-directory)])
           (with-handlers ([exn:fail? (λ (exn)
                                        (log-drracket-language-debug
                                         (apply
                                          string-append
                                          (exn-message exn)
                                          (for/list ([l (in-list
                                                         (continuation-mark-set->context
                                                          (exn-continuation-marks exn)))])
                                            (format "\n  ~a" l))))
                                        #f)])
             (read-language peeking-port))))))
    (cond
      [language-get-info
       (define-values (_3 _4 peeking-pos) (port-next-location peeking-port))
       (define lang-name (make-string (- peeking-pos 1)))
       (read-string! lang-name port)
       (define-values (_5 _6 after-pos) (port-next-location port))
       (values language-get-info
               lang-name
               (- before-pos 1)
               (- after-pos 1))]
      [else (values #f #f #f #f)]))
    
  (define (reset-irl!/inside port)
    (set!-values (language-get-info
                  lang-name
                  before-port-position
                  after-port-position)
                 (compute-lang-info port)))
  
  (define (call-read-language/inside key default)
    (cond
      [language-get-info
       (add-contract key
                     (key->contract key)
                     (language-get-info key default))]
      [else
       default]))

  (define (add-contract key ctc val)
    (contract ctc
              (case key
                [(drracket:default-filters
                  drracket:default-extension
                  drracket:toolbar-buttons
                  drscheme:toolbar-buttons)
                 (copy-the-strings val)]
                [else val])
              lang-name
              "drracket"
              (format "~a's ~a on ~a" lang-name (object-name language-get-info) key)
              #f))

  (define (copy-the-strings val)
    (cond
      [(pair? val) (cons (copy-the-strings (car val))
                         (copy-the-strings (cdr val)))]
      [(string? val) (if (immutable? val)
                         val
                         (string->immutable-string val))]
      [else val]))

  ;; this isn't right yet, but we should make it right!!
  (define read-only-text/c
    (object/c))
  
  ;; NB: all string?s are expected to be inside only lists
  ;; or are expected to be immutable (via the contract);
  ;; (the ones inside lists that are not required to be immutable
  ;; is for backwards compatibility; they are copied)
  (define (key->contract key)
    (case key
      [(definitions-text-surrogate) (or/c #f module-path?)]
      [(color-lexer)
       ;; the contract here is taken care of inside module-lexer
       any/c]
      [(drracket:submit-predicate) (or/c (-> input-port? boolean? boolean?) #f)]
      [(drracket:show-big-defs/ints-labels) any/c]
      [(drracket:default-filters) (or/c #f (listof (list/c string? string?)))]
      [(drracket:default-extension) (or/c #f (and/c string? (not/c #rx"[.]")))]
      [(drracket:indentation)
       (or/c #f
             (-> read-only-text/c
                 exact-nonnegative-integer?
                 (or/c #f exact-nonnegative-integer?)))]

      [(drracket:toolbar-buttons drscheme:toolbar-buttons)
       (or/c #f (listof (or/c (list/c string?
                                      (is-a?/c bitmap%)
                                      ;; this is problematic; the object is the drracket frame;
                                      ;; how do we guard against installing callbacks here?
                                      (-> object? any))
                              (list/c string?
                                      (is-a?/c bitmap%)
                                      ;; this is problematic; the object is the drracket frame;
                                      ;; how do we guard against installing callbacks here?
                                      (-> object? any)
                                      (or/c real? #f)))))]

      [(drracket:opt-out-toolbar-buttons drscheme:opt-out-toolbar-buttons)
       (or/c #f (listof symbol?))]))
  
  (define (get-read-language-port-start+end/inside)
    (values before-port-position after-port-position))

  (define (get-read-language-name/inside) lang-name))
  
(module+ test
  (require rackunit racket/port (submod ".." in-irl-namespace))
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
    (check-equal? (clear-em in) '((x) #\newline #\1)))


  (define (compute-lang-info/wrap str)
    (define sp (open-input-string str))
    (port-count-lines! sp)
    (define-values (language-get-info lang-name before-port-position after-port-position)
      (compute-lang-info sp))
    (list lang-name before-port-position after-port-position))
  
  (check-equal? (compute-lang-info/wrap "#lang racket")
                (list "#lang racket" 0 12))
  (check-equal? (compute-lang-info/wrap "#lang racket/base")
                (list "#lang racket/base" 0 17))
  (check-equal? (compute-lang-info/wrap ";; abc\n#lang racket")
                (list "#lang racket" 7 19))
  (check-equal? (compute-lang-info/wrap ";; abc\n#lang racket\n;; fdajk")
                (list "#lang racket" 7 19))

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
  )

