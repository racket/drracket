
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

(test 'hi 'local (local () 'hi))
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
(test 8 'local (let ([lambda 9]) (local [(define lambda 8)] lambda)))
(test 9 'local (let ([lambda 10]) (local [(define lambda 9) (define lambda2 lambda)] lambda2)))
(test 19 'local (local [(define lambda 19) (define lambda2 lambda)] lambda2))
(test 1 'local (local ((define-values (a b c) (values 1 2 3))) a))
(test 1 (lambda () (local ((define-values (a b c) (values 1 2 3))) a)))
(test 8 'local (local [(define lambda 8)] lambda))
(test 12 'local (local [(define (f y) (add1 y))] (f 11)))
(test 120 'local (local [(define (f y) 'ignore-me (add1 y))] (f 119)))
(test 17 'local (local [(define-values (apple b) (values 12 17))] b))
(test 4 'local (local [(define-struct cons (car cdr))] (cons-car (make-cons 4 5))))
(test 40 'local (local [(define-struct (cons struct:exn) (car cdr))] (cons-car (make-cons "" (void) 40 50))))
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
(syntax-test '(local ((define (x 8) 4)) 1))
(syntax-test '(local ((define (x . 8) 4)) 1))
(syntax-test '(local ((define x 8 4)) 1))
(syntax-test '(local ((define 1 8 4)) 1))
(syntax-test '(let ([define 10]) (local ((define x 4)) 10)))
(syntax-test '(let ([define-values 10]) (local ((define-values (x) 4)) 10)))
(syntax-test '(let ([define-struct 10]) (local ((define-struct x ())) 10)))

(for-each syntax-test 
	  (list '(evcase)
		'(evcase 1 (a))
		'(evcase 1 (a b) a)
		'(evcase 1 (a . b) a)
		'(evcase 1 [else 5] [1 10])))
(define => 17)
(test (void) 'void-evcase (with-handlers ([(lambda (x) #t) (lambda (x) 17)]) (evcase 1)))
(define save-comp (compile-allow-cond-fallthrough))
(compile-allow-cond-fallthrough #f)
(test #t andmap (lambda (x) (= x 17))
      (list
       (evcase 3 [3 17])
       (evcase 3 [(+ 1 2) 17] [3 1])
       (evcase 3 [3 4 5 17])
       (evcase 3 [4 1] [3 4 5 17])
       (evcase 3 [4 1 2 3 4] [3 4 5 17])
       (evcase 3 [4 4] [2 10] [else 17])
       (let ([else 10]) (evcase 3 [4 4] [2 10] [else 15] [3 17]))
       (let ([else 3]) (evcase 3 [else 17] [2 14]))
       (with-handlers ([(lambda (x) #t) (lambda (x) 17)]) (evcase 1))
       (evcase 3 [3 =>])
       (evcase 3 [3 => 17])
       (let ([=> 12]) (evcase 3 [3 => 17]))
       (let ([=> 17]) (evcase 3 [3 =>]))))
(compile-allow-cond-fallthrough save-comp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library "invoke.ss")

(define make-z
  (lambda (x-val)
    (unit
     (import z)
     (export (x z) y)
     
     (define x x-val)
     (define y (lambda () (- z x))))))
    
(define z1 (make-z 8))
(define z2 (make-z 7))

(define m3
  (compound-unit
   (import)
   (link [Z1 (z1 (Z2 z))][Z2 (z2 (Z1 z))])
   (export [Z1 (y y1) (z x1)][Z2 (y y2) (z x2)])))

(define-values/invoke-unit (y1 x1 y2 x2) m3)
(test '(-1 1 8 7) 'invoke-open-unit (list (y1) (y2) x1 x2))

; Linking environments

(when (defined? 'x)
  (undefine 'x))

(define (make--eval)
  (let ([n (make-namespace)])
    (lambda (e)
      (let ([orig (current-namespace)])
	(dynamic-wind
	 (lambda () (current-namespace n))
	 (lambda () 
	   (require-library "invoke.ss")
	   (eval e))
	 (lambda () (current-namespace orig)))))))

(define u
  (unit
   (import)
   (export x)
   (define x 5)))
(define e (make--eval))
(e (list 'define-values/invoke-unit '(x) u #f))
(test #f defined? 'x)
(test #t e '(defined? 'x))

(define u2
  (let ([u u])
    (unit
     (import)
     (export)
     (global-define-values/invoke-unit (x) u #f))))
(define e (make--eval))
(e (list 'define-values/invoke-unit '() u2 #f))
(test #f defined? 'x)
(test #t e '(defined? 'x))


; Export var from embedded unit:

(define-signature e ((unit w : (embedded-v))))
(define-values/invoke-unit/sig (embedded-v)
 (compound-unit/sig
  (import)
  (link [E : e ((compound-unit/sig
		 (import)
		 (link [w : (embedded-v) ((unit/sig (embedded-v)
						    (import)
						    (define embedded-v 0)))])
		 (export (unit w))))])
  (export (var ((E w) embedded-v)))))
(test 0 'embedded-v embedded-v)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)

