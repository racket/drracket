
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'optimization)

(define (comp=? c1 c2)
  (let ([s1 (open-output-string)]
	[s2 (open-output-string)])
    (write c1 s1)
    (write c2 s2)
    (string=? (get-output-string s1) (get-output-string s2))))

(define test-comp
  (case-lambda
   [(expr1 expr2) (test-comp expr1 expr2 #t)]
   [(expr1 expr2 same?)
    (test same? `(compile ,same? ,expr2) (comp=? (compile expr1) (compile expr2)))]))

(test-comp 5 '(if #t 5 (cons 1 2)))
(test-comp 5 '(if #f (cons 1 2) 5))

(test-comp 5 '(begin0 5 'hi "apple" 1.5))
(test-comp 5 '(begin0 5 (begin0 'hi "apple" 1.5)))
(test-comp 5 '(begin0 5 (begin0 'hi "apple") 1.5))
(test-comp 5 '(begin0 5 (begin 'hi "apple" 1.5)))
(test-comp 5 '(begin0 5 (begin 'hi "apple") 1.5))
(test-comp 5 '(begin0 (begin0 5 'hi "apple" 1.5)))
(test-comp 5 '(begin0 (begin0 5 'hi "apple") 1.5))

(test-comp 5 '(begin 'hi "apple" 1.5 5))
(test-comp 5 '(begin (begin 'hi "apple" 1.5) 5))
(test-comp 5 '(begin (begin 'hi "apple") 1.5 5))
(test-comp 5 '(begin (begin0 'hi "apple" 1.5) 5))
(test-comp 5 '(begin (begin0 'hi "apple") 1.5 5))
(test-comp 5 '(begin (begin 'hi "apple" 1.5 5)))
(test-comp 5 '(begin 'hi (begin "apple" 1.5 5)))

(test-comp '(let ([x 8][y 9]) (lambda () x))
	   '(let ([x 8][y 9]) (lambda () (if #f y x))))
(test-comp '(let ([x 8][y 9]) (lambda () (+ x y)))
	   '(let ([x 8][y 9]) (lambda () (if #f y (+ x y)))))

(test-comp '(let ([x 5]) (set! x 2)) '(let ([x 5]) (set! x x) (set! x 2)))

(test-comp '(let* () (f 5))
	   '(f 5))
(test-comp '(letrec* () (f 5))
	   '(f 5))
(test-comp '(with-handlers () (f 5))
	   '(f 5))
(test-comp '(parameterize () (f 5))
	   '(f 5))
(test-comp '(fluid-let () (f 5))
	   '(f 5))

(report-errs)
