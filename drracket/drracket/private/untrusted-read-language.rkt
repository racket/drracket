#lang racket/base

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
       (listof object?)
       (or/c #f object?)
       S
       (values (or/c #f object?)
               (or/c S #f)))]
  [looks-like-module?
   (-> (is-a?/c text%) boolean?)]

  [call-read-language (-> untrusted-language? symbol? any/c any)]

  ;; returns the same result that calling `read-language` and then
  ;; calling port-next-location would have returned
  [get-read-language-port-position (-> untrusted-language? exact-positive-integer?)]

  ;; supplies a callback that's invoked when the internal sandbox dies
  [make-untrusted-language (-> (-> void?) untrusted-language?)]


  ;; call to let the abstraction know that it needs a new `read-language`
  ;; result. If the boolean is #t, then it jettisons all cached loaded
  ;; modules too (otherwise, it re-uses them).
  [reset-untrusted-language! (-> untrusted-language? port? boolean? void?)]))

(struct untrusted-language ([evaluator #:mutable] fail-callback))

(define-runtime-path untrusted-read-language.rkt '(lib "untrusted-read-language.rkt" "drracket" "private"))
(module inside-sandbox racket/base
  (require racket/match)
  (provide reset-untrusted-language!/sandbox
           call-read-language/sandbox
           get-read-language-port-position/sandbox)

  (define language-get-info #f)
  (define port-position 0)
  
  (define (reset-untrusted-language!/sandbox port)
    (set! language-get-info (read-language port))
    (define-values (line col pos) (port-next-location port))
    (set! port-position pos))
  (define (call-read-language/sandbox key default) (language-get-info key default))
  (define (get-read-language-port-position/sandbox) port-position))

(define (get-read-language-port-position an-untrusted-language)
  (call-sandbox-proc an-untrusted-language
                     (λ () 0)
                     'get-read-language-port-position/sandbox))

(define-syntax-rule
  (call-in-sandbox-context/abort evaluator fallback thunk)
  (call-in-sandbox-context/abort/proc evaluator (λ () fallback) thunk))
(define (call-in-sandbox-context/abort/proc evaluator fallback-thunk thunk)
  (with-handlers ([exn:fail?
                   (λ (exn)
                     ((untrusted-language-fail-callback evaluator) exn)
                     (fallback-thunk))])
    (call-in-sandbox-context
     evaluator
     thunk)))

(define (call-read-language an-untrusted-language key default)
  (define val
    (call-sandbox-proc an-untrusted-language
                       (λ ()
                         ((read-language (open-input-string "#lang racket")) key default))
                       'call-read-language/sandbox key default))
  (case key
    [(color-lexer)
     (match-define (untrusted-language evaluator _fail) an-untrusted-language)
     (cond
       [(procedure-arity-includes? val 3)
        (λ (port offset mode)
          (call-in-sandbox-context/abort
           evaluator
           (let-values ([(a b c d e) (racket-lexer (open-input-string "\""))])
             (values a b c d e #f 0))
           (λ ()
             (val port offset mode))))]
       [else
        (λ (port)
          (call-in-sandbox-context/abort
           evaluator
           (racket-lexer (open-input-string "\""))
           (λ ()
             (val port))))])]
    [(drracket:submit-predicate)
     (match-define (untrusted-language evaluator _eval) an-untrusted-language)
     (λ (port only-whitespace?)
       (call-in-sandbox-context/abort
        evaluator
        #t
        (λ ()
          (val port only-whitespace?))))]
    [else
     (eprintf "unknown key: ~s\n" key)
     val]))

(define (reset-untrusted-language! an-untrusted-language port flush-cache?)
  (when flush-cache?
    (set-untrusted-language-evaluator! (make-untrusted-language-evaluator)))
  (call-sandbox-proc an-untrusted-language
                     void
                     'reset-untrusted-language!/sandbox port))

(define (call-sandbox-proc an-untrusted-language fallback proc . args)
  (match-define (untrusted-language evaluator _eval) an-untrusted-language)
  (call-in-sandbox-context/abort
   (fallback)
   evaluator
   (λ ()
     (apply (dynamic-require `(submod ,untrusted-read-language.rkt inside-sandbox) proc)
            args))))

(define (make-untrusted-language callback)
  (untrusted-language (make-untrusted-language-evaluator callback)))

(define (make-untrusted-language-evaluator callback)
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
  (define trusted-custodian (current-custodian))
  (define trusted-eventspace (current-eventspace))
  (call-in-sandbox-context
   e
   (λ ()
     (let ([dead-evt (thread-dead-evt (current-thread))])
       (parameterize ([current-custodian trusted-custodian]
                      [current-eventspace trusted-eventspace])
         (thread
          (λ ()
            (sync dead-evt)
            (let loop ()
              (cond
                [(evaluator-alive? e)
                 (sleep)
                 (loop)]
                [else
                 (define reason
                   (with-handlers ([exn:fail:sandbox-terminated?
                                    exn:fail:sandbox-terminated-reason])
                     (e 'this-should-be-ignored-because-the-evaluator-has-terminated)))
                 (queue-callback
                  (λ ()
                    (callback reason)))]))))))))
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


(module+ main
  (collect-garbage) (collect-garbage) (collect-garbage)
  (require profile)
  (time (make-untrusted-language void))
  (time (make-untrusted-language void))
  (time (make-untrusted-language void))
  (time (make-untrusted-language void))
  (define ue (time (make-untrusted-language void)))
  (printf "done creating\n")
  (time (reset-untrusted-language! ue (open-input-string "#lang racket/base") #f))
  (time (reset-untrusted-language! ue (open-input-string "#lang racket/base") #f))
  (time (reset-untrusted-language! ue (open-input-string "#lang racket/base") #f))
  (time (reset-untrusted-language! ue (open-input-string "#lang racket/base") #f))
  )
