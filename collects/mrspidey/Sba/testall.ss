; testall.ss
; Tries to test all of Teiresias's type inference algorithm
; and uses type assertions to detect when it fails

; ----------------------------------------------------------------------
; Constants

(: 1   (exact num))
(: 'a  (exact sym))
(: #\a (exact char))
(: #t  (exact true))
(: #f  (exact false))
(: nil (exact nil))
(: '()  (exact nil))
(: "Cormac" (exact str))
(: '(a b c) (exact (listof sym)))
(: '(a b 1) (exact (listof (union sym num))))
(: #(1 2 3) (exact (vec num)))
(: #(1 2 "abc") (exact (vec (union num str))))
;;;(: #&4      (exact (box num)))

; ----------------------------------------------------------------------
; misc kinds of exprs

; top-level def
(define x 'a-symbol)
(: x (exact sym))


; lexical binding -various sorts
(let ([y 4][z 'a]) 
  (: y (exact num))
  (: z (exact sym)))
(letrec ([y 4][z 'a]) 
  (: y (exact num))
  (: z (exact sym)))
(let* ([y 4][z 'a]) 
  (: y (exact num))
  (: z (exact sym)))

; if - one and two armed
(: (if (read) 1 'a) (exact (union num sym)))
(: (if (read) 1)    (exact (union num void)))

; set!
(let ([z 4]) 
  (set! z 'a) 
  (: z (exact (union num sym))))

; begin
(: (begin 'a 1) (exact num))

; now various lambdas and applications
((lambda (x y) (: x (exact num)) (: y (exact sym))) 
 4 'a)
((lambda x (: x (exact (cons num (cons num nil))))) 1 2)
(apply (lambda (x) (: x (exact num))) (list 1))
(apply (lambda x (: x (exact (cons num (cons num nil))))) (cons 1 (cons 2 nil)))
(: (lambda (x) x) (_ -> _))
(: ((lambda (x) 'a) 3) (exact sym))

;; Multiple calls, merged returns
(let ([I (lambda (x) (: x (exact (union num sym))))])
  (: (I 3) (exact (union num sym)))
  (: (I 'a) (exact (union num sym))))

; ----------------------------------------------------------------------
; if-splitting

; (if (pred arg) ...)
(let ([x (if (read) 1 'a)])
  (: x (exact (union num sym)))
  (if (number? x)
      (: x (exact num))
      (: x (exact sym))))

; assigned arg
(let ([x (if (read) 1 'a)])
  (set! x 2)
  (if (number? x)
      (: x (exact (union num sym)))
      (: x (exact (union num sym)))))

; (if x ...)
(let ([x (if (read) #t #f)])
  (if x 
      (: x (exact true))
      (: x (exact false))))
(let ([x (if (read) 5 #f)])
  (if x 
      (: x (exact num))
      (: x (exact false))))

; (if (not x) ...)
(let ([x (if (read) #t #f)])
  (if (not x)
      (: x (exact false))
      (: x (exact true))))
(let ([x (if (read) 5 #f)])
  (if (not x)
      (: x (exact false))
      (: x (exact num))))

; (if (not (pred arg)) ...)
(let ([x (if (read) 1 'a)])
  (if (not (number? x))
      (: x (exact sym))
      (: x (exact num))))

; ----------------------------------------------------------------------
; Flow sensitive

(let ([x (if 1 (cons 1 2) 'a)])
  (: x (exact (union (cons num num) sym)))
  (car x)
  (: x (exact (cons num num)))
  (cdr x))

; not on assigned vars
(let ([x (if (read) (cons 1 2) 'a)])
  (set! x 'b)
  (: x (exact (union (cons num num) sym)))
  (car x)
  (: x (exact (union (cons num num) sym)))
  (cdr x))

; ----------------------------------------------------------------------
; Boxes

(let ([x (: (box 1) (exact (box num)))])
  (: (unbox x) (exact num)))
(let ([x (: (box 1) (exact (box (union num sym))))])
  (set-box! x 'a)
  (: (unbox x) (exact (union num sym))))
(let ([x (if (read) (box 'a) #f)])
  (if (box? x) (: x (exact (box sym)))))

; ----------------------------------------------------------------------
; User primitives and types

(define-constructor Box #t)

(define-primitive Box? (_ -> bool) (predicate Box))
(define-primitive Box (a -> (Box a)))
(define-primitive unBox ((Box a) -> a))
(define-primitive set-Box! ((Box (! a)) a -> void))

(let ([x (: (Box 1) (exact (Box num)))])
  (: (unBox x) (exact num)))
(let ([x (: (Box 1) (exact (Box (union num sym))))])
  (set-Box! x 'a)
  (: (unBox x) (exact (union num sym))))
(let ([x (if (read) (Box 'a) #f)])
  (if (Box? x) (: x (exact (Box sym)))))

(define-type env (listof (list sym num)))
(define-primitive mk-env (-> env))
(: (mk-env) (exact (listof (list sym num))))

; ----------------------------------------------------------------------
; define-structure

(define-structure (Triple a b c))

(let ([x (: (make-Triple 1 'a #\c) (exact (structure:Triple num sym char)))])
  (: (Triple-a x) (exact num))
  (: (Triple-1 x) (exact num)))
(let ([x (: (make-Triple 1 'a #\c) (exact (structure:Triple (union num sym) sym char)))])
  (set-Triple-a! x 'a)
  (: (Triple-a x) (exact (union num sym))))
(let ([x (if (read) (make-Triple 1 'a #\c) #f)])
  (if (Triple? x) (: x (exact (structure:Triple num sym char)))))

; ----------------------------------------------------------------------
; loops

(define Map
  (lambda (f l)
    (if (null? l)
        '()
        (cons (f (car l)) (Map f (cdr l))))))

(: (Map add1 '(1 2 3)) (exact (listof num)))

; ----------------------------------------------------------------------
; Could test all the primitives ...

