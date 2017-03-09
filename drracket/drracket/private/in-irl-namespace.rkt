#lang racket/base

(require racket/match
         racket/class
         racket/contract/option
         racket/contract
         racket/draw
         racket/port
         (submod "insulated-read-language.rkt" skip-past-comments)
         syntax/modread)
(provide reset-irl!/inside
         call-read-language/inside
         get-read-language-port-start+end/inside
         get-read-language-last-position/inside
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
(define read-language-last-position #f)

(define (set-irl-mcli-vec!/inside _mcli-vec) (set! mcli-vec _mcli-vec))

(define (get-submit-predicate/inside)
  (or (and mcli-vec
           (let ([get-info
                  ((dynamic-require (vector-ref mcli-vec 0)
                                    (vector-ref mcli-vec 1))
                   (vector-ref mcli-vec 2))])
             (add-contract
              'drracket:submit-predicate
              (key->contract 'drracket:submit-predicate)
              (get-info 'drracket:submit-predicate #f))))
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
             (- after-pos 1)
             (- after-pos 1))]
    [else
     (define-values (_5 _6 peeking-pos) (port-next-location peeking-port))
     (values #f #f #f #f (+ before-pos peeking-pos -2))]))
    
(define (reset-irl!/inside port)
  (set!-values (language-get-info
                lang-name
                before-port-position
                after-port-position
                read-language-last-position)
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
    [(drracket:keystrokes)
     ;; string? is too permissive; need racket/gui to publish
     ;; the actual contract (used on `map-function`)
     (listof (list/c string? (-> any/c any/c any/c)))]
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

    [(drracket:opt-out-toolbar-buttons drscheme:opt-out-toolbar-buttons drracket:opt-in-toolbar-buttons)
     (or/c #f (listof symbol?))]
    [else
     (error 'key->contract "unknown key")]))

(define (get-read-language-last-position/inside) read-language-last-position)

(define (get-read-language-port-start+end/inside)
  (values before-port-position after-port-position))

(define (get-read-language-name/inside) lang-name)

(module+ test
  (require rackunit racket/gui/base)

    (define (compute-lang-info/wrap str)
    (define sp (open-input-string str))
    (port-count-lines! sp)
    (define-values (language-get-info lang-name
                                      before-port-position after-port-position
                                      read-language-last-position)
      (compute-lang-info sp))
    (list lang-name
          before-port-position
          after-port-position
          read-language-last-position))
  
  (check-equal? (compute-lang-info/wrap "#lang racket")
                (list "#lang racket" 0 12 12))
  (check-equal? (compute-lang-info/wrap "#lang racket/base")
                (list "#lang racket/base" 0 17 17))
  (check-equal? (compute-lang-info/wrap ";; abc\n#lang racket")
                (list "#lang racket" 7 19 19))
  (check-equal? (compute-lang-info/wrap ";; abc\n#lang racket\n;; fdajk")
                (list "#lang racket" 7 19 19))
  (check-equal? (compute-lang-info/wrap ";; abc\n(stuff)")
                (list #f #f #f 8))
  (check-equal? (compute-lang-info/wrap ";; abcdefgjd\n\n\n(stuff)")
                (list #f #f #f 16))
  (check-equal? (compute-lang-info/wrap "(stuff)")
                (list #f #f #f 1))
  (check-equal? (compute-lang-info/wrap "123 456")
                (list #f #f #f 1)))
