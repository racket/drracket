(module tool-contract-language mzscheme
  (provide (rename -#%module-begin #%module-begin)
           (all-from-except mzscheme #%module-begin))
  
  (require (lib "contract.ss"))
  
  (define-syntax (-#%module-begin stx)
    (syntax-case stx ()
      [(_ (name type type-names strs ...) ...)
       (and (andmap identifier? (syntax->list (syntax (name ...))))
            (andmap (lambda (x) (not (string? (syntax-object->datum x))))
                    (syntax->list (syntax (type-names ...))))
            (andmap (lambda (x) (string? (syntax-object->datum x)))
                    (apply append (map syntax->list (syntax->list (syntax ((strs ...) ...)))))))
       (with-syntax ([wrap-tool-inputs (datum->syntax-object stx 'wrap-tool-inputs #'here)])
         (syntax/loc stx
          (#%module-begin
           (provide wrap-tool-inputs)
           (define-syntax wrap-tool-inputs
             (lambda (in-stx)
               (syntax-case in-stx ()
                 [(_ body tool-name)
                  (with-syntax ([(in-type (... ...))
                                 (map (lambda (in-type-obj) 
                                        (datum->syntax-object 
                                         in-stx
                                         (syntax-object->datum in-type-obj)
                                         in-type-obj))
                                      (syntax->list (syntax (type ...))))]
                                [(in-name (... ...))
                                 (map (lambda (in-name-obj) 
                                        (datum->syntax-object 
                                         in-stx 
                                         (syntax-object->datum in-name-obj)
                                         in-name-obj))
                                      (syntax->list (syntax (name ...))))])
                    (syntax/loc in-stx
                     (let ([in-name (contract (let ([in-name in-type]) in-name)
                                              in-name
                                              'drscheme
                                              tool-name
                                              (quote-syntax in-name))] (... ...))
                       body)))]))))))]
      [(_ (name type type-names strs ...) ...)
       (begin
         (for-each
          (lambda (str-stx)
            (when (string? (syntax-object->datum str-stx))
              (raise-syntax-error 'tool-contract-language.ss "expected type name specification"
                                  stx
                                  str-stx)))
          (syntax->list (syntax (type-names ...))))
         (for-each
          (lambda (name)
            (unless (identifier? name)
              (raise-syntax-error 'tool-contract-language.ss "expected identifier" stx name)))
          (syntax->list (syntax (name ...))))
         (for-each
          (lambda (str)
            (unless (string? (syntax-object->datum str))
              (raise-syntax-error 'tool-contract-language.ss "expected docs string" stx str)))
          (apply append (map syntax->list (syntax->list (syntax ((strs ...) ...)))))))])))
