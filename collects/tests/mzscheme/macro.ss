
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))


(SECTION 'MACRO)

(define-macro mx
  (lambda (x)
    (list x 1 8)))
(test 9 'macro (mx +))
(test -7 'macro (mx -))
(test 18 'macro (let ([mx (lambda (x) (x 1 8 9))]) (mx +)))
(when (defined? 'let-macro)
      (teval '(test 13 'let-macro (let-macro mx (lambda (x) (list x 6 7)) (mx +))))
      (teval '(test -7 'let-macro (let-macro mx2 (lambda (x y) (list 'mx y)) (mx2 + -))))
      (teval '(test '(10) 'let-macro ((lambda () (let-macro x (lambda x (cons 'list x)) (x 10))))))
      (teval '(test '(10) 'let-macro (let () (define-macro x (lambda x (cons 'list x))) (x 10))))
      ; (test '(10) eval '((lambda () (define-macro x (lambda x (cons 'list x))) (x 10))))
      )

(define a-global-var 1)
(define-macro a-macro (lambda () a-global-var))
(test 1 'macro (a-macro))

(when (defined? 'let-macro)
      (teval '(define (defmacro-test)
		(define-macro define-alias (lambda (x y) `(define ,x ,y)))
		(test 45 'define
		      (let ((x 5))
			(define-alias foo (lambda (y) (bar x y)))
			(define-alias bar (lambda (a b) (+ (* a b) a)))
			(foo (+ x 3)))))))

(report-errs)
