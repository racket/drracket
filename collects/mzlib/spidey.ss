
(define-macro define-constructor
  (lambda args '(#%void)))

(define-macro define-type
  (lambda args '(#%void)))

(define-macro :
  (lambda (v . args) v))

(define-macro mrspidey:control
  (lambda args '(#%void)))

(define-macro polymorphic
  (lambda (arg) arg))
