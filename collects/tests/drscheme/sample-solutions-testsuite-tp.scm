(module sample-solutions-testsuite-tp mzscheme
  (provide require-library)
  (require (lib "include.ss"))
  
  (define-syntax require-library
    (let ([cache null])
      (lambda (stx)
        (syntax-case stx ()
          [(_ fn-stx lib-stx)
           (let ([fn (syntax-object->datum (syntax fn-stx))]
                 [lib (syntax-object->datum (syntax lib-stx))])
             (unless (equal? lib "solutions")
               (raise-syntax-error
                #f
                "expected `solutions' collection as second argument to require-library"
                stx))
             (unless (string? fn)
               (raise-syntax-error
                #f
                "expected string constant as first argument to require-library"
                stx))
             (if (member fn cache)
                 (syntax (void))
                 (with-syntax ([full-fn (build-path (collection-path "solutions") fn)]
                               [orig stx])
                   (set! cache (cons fn cache))
                   (syntax
                    (include-at/relative-to orig orig full-fn)))))]))))
  
  (provide sqr)
  (define (sqr x) (* x x)))
