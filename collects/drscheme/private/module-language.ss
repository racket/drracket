
(module module-language mzscheme
  (provide module-language@)
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           "drsig.ss")
  
  (define module-language@
    (unit/sig drscheme:module-language^
      (import [drscheme:language-configuration : drscheme:language-configuration/internal^]
              [drscheme:language : drscheme:language^]
              [drscheme:unit : drscheme:unit^])
      
      ;; add-module-language : -> void
      ;; adds the special module-only language to drscheme
      (define (add-module-language)
        (drscheme:language-configuration:add-language
         (instantiate module-language% ())))
      
      ;; module-mixin : (implements drscheme:language:language<%>)
      ;;             -> (implements drscheme:language:language<%>)
      (define (module-mixin %)
        (class %
          (define/override (use-namespace-require/copy?) #t)
          (rename [super-on-execute on-execute]
                  [super-front-end front-end])
          (field [iteration-number 0])

          (define/override (on-execute settings run-in-user-thread)
            (set! iteration-number 0)
            (super-on-execute settings run-in-user-thread))

          (define/override (get-style-delta) module-language-style-delta)
          
          (define/override (front-end input settings)
            (if (and (drscheme:language:text/pos? input)
                     (is-a? (drscheme:language:text/pos-text input)
                            drscheme:unit:definitions-text%))
                (let ([super-thunk (super-front-end input settings)]
                      [module-name #f])
                  (lambda ()
                    (set! iteration-number (+ iteration-number 1))
                    (let ([super-result (super-thunk)])
                      (cond
                        [(= iteration-number 1)
                         (let-values ([(name new-module)
                                       (transform-module-to-export-everything
                                        (expand super-result))])
                           (set! module-name name)
                           new-module)]
                        [(= 2 iteration-number)
                         (if (eof-object? super-result)
                             (with-syntax ([name module-name])
                               (syntax (require name)))
                             (raise-syntax-error
                              'module-language
                              "there can only be one expression in the definitions window"
                              super-result))]
                        [else eof]))))
                (super-front-end input settings)))
          (super-instantiate ()
            (module '(lib "full-mred.ss" "lang"))
            (language-position (list "module")))))

      ;; module-language-style-delta : (instanceof style-delta%)
      (define module-language-style-delta (make-object style-delta% 'change-family 'modern))

      ;; transform-module-to-export-everything : syntax -> syntax
      (define (transform-module-to-export-everything stx)
        (syntax-case stx (module #%plain-module-begin)
          [(module name lang (#%plain-module-begin bodies ...))
           (with-syntax ([(unprovided-vars ...) (extract-unprovided-vars 
                                                 (syntax->list
                                                  (syntax (bodies ...))))])
             (values
              (syntax name)
              (syntax (module name lang (#%plain-module-begin 
                                         (provide unprovided-vars ...) 
                                         bodies ...)))))]
          [else
           (raise-syntax-error 'module-language
                               "only module expressions are allowed"
                               stx)]))
      
      ;; extract-unprovided-vars : (listof syntax) -> (listof syntax)
      (define (extract-unprovided-vars bodies)
        (let ([provided-vars (extract-provided-vars bodies)]
              [defined-vars (extract-defined-vars bodies)]
              [ht (make-hash-table)])
          
          (for-each
           (lambda (provided-var)
             (hash-table-put! 
              ht
              (syntax-object->datum provided-var)
              (cons
               provided-var
               (hash-table-get ht (syntax-object->datum provided-var)
                               (lambda () null)))))
           provided-vars)
          
          (let loop ([defined-vars defined-vars])
            (cond
              [(null? defined-vars) null]
              [else 
               (let* ([defined-var (car defined-vars)]
                      [ht-entry (hash-table-get ht 
                                                (syntax-object->datum defined-var)
                                                (lambda () null))])
                 (if (memf (lambda (x) (module-identifier=? x defined-var))
                           ht-entry)
                     (loop (cdr defined-vars))
                     (cons defined-var (loop (cdr defined-vars)))))]))))
      
      ;; extract-provided-vars : (listof syntax) -> (listof syntax[identifier])
      (define (extract-provided-vars bodies)
        (let loop ([bodies bodies]
                   [sofar null])
          (cond
            [(null? bodies) (apply append sofar)]
            [else
             (syntax-case (car bodies) (provide)
               [(provide vars ...)
                (loop (cdr bodies)
                      (cons (syntax->list (syntax (vars ...)))
                            sofar))]
               [_ (loop (cdr bodies)
                        sofar)])])))

      ;; extract-defined-vars : (listof syntax) -> (listof syntax[identifier])      
      (define (extract-defined-vars bodies)
        (let loop ([bodies bodies]
                   [sofar null])
          (cond
            [(null? bodies) (apply append sofar)]
            [else
             (syntax-case (car bodies) (define-values)
               [(define-values (vars ...) xxx)
                (loop (cdr bodies)
                      (cons (syntax->list (syntax (vars ...)))
                            sofar))]
               [_ (loop (cdr bodies)
                        sofar)])])))
      
      
      ;; module-language : (implements drscheme:language:language<%>)
      (define module-language%
        (module-mixin
         (drscheme:language:module-based-language->language-mixin
          (drscheme:language:simple-module-based-language->module-based-language-mixin
           drscheme:language:simple-module-based-language%)))))))
