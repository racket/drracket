
; Test MzScheme's name inference

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'NAMES)

(arity-test inferred-name 1 1)
(test #f inferred-name 0)
(test #f inferred-name 'hello)
(test #f inferred-name "hi")

; Test ok when no name for proc
(test #f inferred-name (lambda () 0))
(test #f inferred-name (case-lambda))
(test #f inferred-name (case-lambda [(x) 9]))
(test #f inferred-name (case-lambda [(x) 9][(y z) 12]))

; Test constructs that don't provide a name
(test #f inferred-name (let ([x (cons (lambda () 10) 0)]) (car x)))
(test #f inferred-name (let ([x (let ([y (lambda (x) x)]) (y (lambda () 10)))]) x))

; Test ok when name for proc
(define f (lambda () 0))
(define f2 (lambda (a) 0))
(define f3 (case-lambda))
(define f4 (case-lambda [(x) 9]))
(define f5 (case-lambda [(x) 9][(y z) 10]))

(test 'f inferred-name f)
(test 'f2 inferred-name f2)
(test 'f3 inferred-name f3)
(test 'f4 inferred-name f4)
(test 'f5 inferred-name f5)

; Test constructs that do provide a name
(test 'a inferred-name (let ([a (lambda () 0)]) a))
(test 'a inferred-name (let ([a (lambda () 0)]) (let ([b a]) b)))
(test 'b inferred-name (let* ([b (lambda () 0)]) b))
(test 'c inferred-name (letrec ([c (lambda () 0)]) c))
(test 'loop inferred-name (let loop () loop))

(test 'd inferred-name (let ([d (begin (lambda () x))]) d))
(test 'e inferred-name (let ([e (begin0 (lambda () x))]) e))

(test 'd2 inferred-name (let ([d2 (begin 7 (lambda () x))]) d2))
(test 'e2 inferred-name (let ([e2 (begin0 (lambda () x) 7)]) e2))

(test 'd3 inferred-name (let ([d3 (begin (cons 1 2) (lambda () x))]) d3))
(test 'e3 inferred-name (let ([e3 (begin0 (lambda () x) (cons 1 2))]) e3))

(test 'f inferred-name (let ([f (begin0 (begin (cons 1 2) (lambda () x)) (cons 1 2))]) f))

(test 'g1 inferred-name (let ([g1 (if (cons 1 2) (lambda () x) #f)]) g1))
(test 'g2 inferred-name (let ([g2 (if (negative? (car (cons 1 2))) #t (lambda () x))]) g2))

(test 'w inferred-name (let ([w (let ([x 5]) (lambda () x))]) w))
(test 'z inferred-name (let ([z (let ([x 5]) (cons 1 2) (lambda () x))]) z))

(set! f (lambda () 10))
(test 'f inferred-name f)

; Test class stuff ok when no name
(test #f inferred-name (class object% () (sequence (super-init))))
(test #f inferred-name (interface ()))

; Test class stuff ok when name
(test 'c1 inferred-name (let ([c1 (class object% () (sequence (super-init)))]) c1))
(test 'i1 inferred-name (let ([i1 (interface ())]) i1))
(test 'm inferred-name
      (ivar
       (make-object
	(class object% ()
	       (public
		[m (lambda () 10)])
	       (sequence (super-init))))
       m))
 ; Use external name:
(test 'mex inferred-name
      (ivar
       (make-object
	(class object% ()
	       (public
		[(m mex) (lambda () 10)])
	       (sequence (super-init))))
       mex))

; Test unit stuff ok when no name
(test #f inferred-name (unit (import) (export)))
(test #f inferred-name (compound-unit (import) (link) (export)))

; Test class stuff ok when name
(test 'u1 inferred-name (let ([u1 (unit (import) (export))]) u1))
(test 'u2 inferred-name (let ([u2 (compound-unit (import) (link) (export))]) u2))

(test 'x inferred-name (invoke-unit
		        (unit (import) (export) (define x (lambda () 0)) x)))
(test 'x2 inferred-name (invoke-unit
		        (unit (import) (export x2) (define x2 (lambda () 0)) x2)))
 ; Use external name:
(test 'x3 inferred-name (invoke-unit
			 (unit (import) (export (x x3)) (define x (lambda () 0)) x)))

(report-errs)
