
(define-struct x (a b c))

make-x
x?
x-b
set-x-c!

rumplestilskin

(1 2)
(letrec () 'constant)
(letrec-values ([(x y) 'constant]) 'constant)
(let () 'constant)
(let-values ([(x y) 'constant]) 'constant)
(let ([x 1]) x)
(lambda (x) 'constant)
(case-lambda [(x . y) x] [(x) x] [y x])
`(,x) ;; this one won't show the unbound x :(
(if 1 2 3)
(set! y 1)
(define x 2)
(set! x 1)
(begin 123 x y)
(begin0 123 x y)
