
(define-macro entry-point 
  (lambda (f)
    `(lambda () (as-entry ,f))))

(define-macro entry-point-1
  (lambda (f)
    (let ([x (gensym)])
      `(lambda (,x) (as-entry (lambda () (,f ,x)))))))

(define-macro entry-point-2
  (lambda (f)
    (let ([x (gensym)]
	  [y (gensym)])
      `(lambda (,x ,y) (as-entry (lambda () (,f ,x ,y)))))))

(define-macro entry-point-3
  (lambda (f)
    (let ([x (gensym)]
	  [y (gensym)]
	  [z (gensym)])
      `(lambda (,x ,y ,z) (as-entry (lambda () (,f ,x ,y ,z)))))))

(define-macro entry-point-0-1
  (lambda (f)
    (let ([x (gensym)])
      `(case-lambda
	[() (as-entry ,f)]
	[(,x) (as-entry (lambda () (,f ,x)))]))))

(define-macro entry-point-1-2
  (lambda (f)
    (let ([x (gensym)]
	  [y (gensym)])
      `(case-lambda
	[(,x) (as-entry (lambda () (,f ,x)))]
	[(,x ,y) (as-entry (lambda () (,f ,x ,y)))]))))

(define-macro entry-point-1-2-3
  (lambda (f)
    (let ([x (gensym)]
	  [y (gensym)]
	  [z (gensym)])
      `(case-lambda
	[(,x) (as-entry (lambda () (,f ,x)))]
	[(,x ,y) (as-entry (lambda () (,f ,x ,y)))]
	[(,x ,y ,z) (as-entry (lambda () (,f ,x ,y ,z)))]))))
