
(define-macro begin-construction-time 
  (lambda body 
    `(#%begin-elaboration-time ,@body)))

(begin-elaboration-time 
 (require-library "invoke.ss"))

(begin-elaboration-time
 (define-values/invoke-unit (require-unit reference-file)
   (require-library "referf.ss")))

(define-macro require-library-unit/sig (require-unit #t #t #f #t 'require-library-unit/sig))
(define-macro require-library-unit (require-unit #t #t #f #f 'require-library-unit))
(define-macro require-relative-library-unit/sig (require-unit #t #t #t #t 'require-relative-library-unit/sig))
(define-macro require-relative-library-unit (require-unit #t #t #t #f 'require-relative-library-unit))
(define-macro require-unit/sig (require-unit #t #f #f #t 'require-unit/sig))
(define-macro require-unit (require-unit #t #f #f #f 'require-unit))

(define-macro reference-file (reference-file #t #f #f))

(require-library "spidey.ss")
