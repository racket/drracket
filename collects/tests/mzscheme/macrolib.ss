
(if (not (defined? 'SECTION))
    (load "testing.ss"))

(SECTION 'macrolib)

(require-library "macro.ss")

(let ([u (letrec ([x x]) x)])
  (let ([l1
	 (let+ ([rec a a]
		[recs [b c] [c b]]
		[rec d 1]
		[val e 1]
		[val x 1]
		[val y 2]
		[vals (x y) (y x)]
		[rec (values f) (values 1)]
		[vals [(values g h) (values 2 3)]]
		[val i 3]
		[_ (set! i 4)
		   (set! i 5)])
	       'x
	       (list a b c d e x y f g h i))]
	[l2 (list u u u 1 1 2 1 1 2 3 5)])
    (test l1 'let-plus l2)))

(require-library "shared.ss")

(test "((car . cdr) #(one two three four five six) #&box (list1 list2 list3 list4) #<weak-box> 3 3)"
      'shared
      (let ([s (open-output-string)])
	(display
	 (shared ((a (cons 'car 'cdr)) 
		  (b (vector 'one 'two 'three 'four 'five 'six))
		  (c (box 'box))
		  (d (list 'list1 'list2 'list3 'list4))
		  (e (make-weak-box 'weak-box))
		  (f (+ 1 2))
		  (g 3))
		 (list a b c d e f g))
	 s)
	(get-output-string s)))

(define x 7)
(test 6 'local (local ((define x 6)) x))
(test 7 'local x)
(test 6 vector-ref (struct->vector (local ((define x 6) (define-struct a (b))) (make-a x))) 1)
(test #t 'local (local [(define o (lambda (x) (if (zero? x) #f (e (sub1 x)))))
			(define e (lambda (x) (if (zero? x) #t (o (sub1 x)))))]
		       (e 10)))
(test 'second 'local (local ((define x 10) (define u 'second)) (cons x 1) u))
(test-values '(4 6) (lambda () (local ((define y 6) (define x 4)) (values x y))))
(test 10 'local (let ([x 10]) (local ((define y (lambda () x))) (define x 5) (y))))
(test 5 'local (let ([x 10]) (local ((define y (lambda () x))) (define x 5) x)))
(test 8 'local (local [(define lambda 8)] lambda))
(test 9 'local (local [(define lambda 9) (define lambda2 lambda)] lambda2))
(test 1 'local (local ((define-values (a b c) (values 1 2 3))) a))
(test 1 (lambda () (local ((define-values (a b c) (values 1 2 3))) a)))
(syntax-test '(local))
(syntax-test '(local . 1))
(syntax-test '(local ()))
(syntax-test '(local () . 1))
(syntax-test '(local 1 1))
(syntax-test '(local (1) 1))
(syntax-test '(local (x) 1))
(syntax-test '(local ((+ 1 2)) 1))
(syntax-test '(local ((define x)) 1))
(syntax-test '(local ((define x 4) (+ 1 2)) 1))
(syntax-test '(local ((define x 4) (+ 1 2) (define y 10)) 1))

(report-errs)
