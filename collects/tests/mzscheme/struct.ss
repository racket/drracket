
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'STRUCT)

(test 7 call-with-values
      (lambda () (struct a (b c)))
      (lambda args (length args)))
(let-values ([(type make pred sel1 set1 sel2 set2) (struct a (b c))])
   (test #t struct-type? type)
   (test #t procedure? make)
   (test 2 arity make)
   (test 1 arity sel1)
   (test 2 arity set1)
   (test #t struct-setter-procedure? set2)
   (test #f struct-setter-procedure? sel2))

(define-struct a (b c))
(define-struct aa ())
(define ai (make-a 1 2))
(define aai (make-aa))
(test #t struct-type? struct:a)
(test #f struct-type? 5)
(test #t procedure? a?)
(test #t a? ai)
(test #f a? 1)
(test #f aa? ai)
(test 1 a-b ai)
(test 2 a-c ai)
(define ai2 (make-a 1 2))
(set-a-b! ai2 3)
(set-a-c! ai2 4)
(test 1 a-b ai)
(test 2 a-c ai)
(test 3 a-b ai2)
(test 4 a-c ai2)
(define-struct a (b c))
(test #f a? ai)
(arity-test make-a 2 2)
(error-test `(make-aa 1) exn:application:arity?)
(arity-test a? 1 1)
(arity-test a-b 1 1)
(arity-test a-c 1 1)
(arity-test set-a-b! 2 2)
(arity-test set-a-c! 2 2)
(error-test `(a-b 5))
(error-test `(a-b ,ai))
(error-test `(set-a-b! ai 5))
(error-test `(set-a-c! ai 5))
(error-test `(begin (define-struct (a 9) (b c)) (void)) exn:struct?)

(arity-test struct-type? 1 1)

(define (gen-struct-syntax-test formname suffix)
  (syntax-test `(,formname 1 (x) ,@suffix))
  (syntax-test `(,formname a (1) ,@suffix))
  (syntax-test `(,formname a (x 1) ,@suffix))
  (syntax-test `(,formname a (x . y) ,@suffix))
  (syntax-test `(,formname (a) (x) ,@suffix))
  (syntax-test `(,formname (a . y) (x) ,@suffix))
  (syntax-test `(,formname (a 2 3) (x) ,@suffix)))
(define (struct-syntax-test formname)
  (syntax-test `(,formname))
  (syntax-test `(,formname . a))
  (syntax-test `(,formname a . x))
  (syntax-test `(,formname a x))
  (gen-struct-syntax-test formname '()))

(struct-syntax-test 'struct)
(struct-syntax-test 'define-struct)
(gen-struct-syntax-test 'let-struct '(5))

(define-struct base0 ())
(define-struct base1 (a))
(define-struct base2 (l r))
(define-struct base3 (x y z))

(define-struct (one00 struct:base0) ())
(define-struct (one01 struct:base1) ())
(define-struct (one02 struct:base2) ())
(define-struct (one03 struct:base3) ())

(define-struct (one10 struct:base0) (a))
(define-struct (one11 struct:base1) (a))
(define-struct (one12 struct:base2) (a))
(define-struct (one13 struct:base3) (a))

(define-struct (one20 struct:base0) (l r))
(define-struct (one21 struct:base1) (l r))
(define-struct (one22 struct:base2) (l r))
(define-struct (one23 struct:base3) (l r))

(define-struct (one30 struct:base0) (x y z))
(define-struct (one31 struct:base1) (x y z))
(define-struct (one32 struct:base2) (x y z))
(define-struct (one33 struct:base3) (x y z))

(define-struct (two100 struct:one00) (a))
(define-struct (two101 struct:one01) (a))
(define-struct (two102 struct:one02) (a))
(define-struct (two103 struct:one03) (a))
(define-struct (two110 struct:one10) (a))
(define-struct (two111 struct:one11) (a))
(define-struct (two112 struct:one12) (a))
(define-struct (two113 struct:one13) (a))
(define-struct (two120 struct:one20) (a))
(define-struct (two121 struct:one21) (a))
(define-struct (two122 struct:one22) (a))
(define-struct (two123 struct:one23) (a))
(define-struct (two130 struct:one30) (a))
(define-struct (two131 struct:one31) (a))
(define-struct (two132 struct:one32) (a))
(define-struct (two133 struct:one33) (a))

(define x00 (make-one00))

(define x01 (make-one01 1))

(define x10 (make-one10 1))
(define x11 (make-one11 1 2))
(define x12 (make-one12 1 2 3))
(define x13 (make-one13 1 2 3 4))

(define x31 (make-one31 1 2 3 4))

(define x33 (make-one33 1 2 3 4 5 6))

(define x132 (make-two132 1 2 3 4 5 6))

(define (ones v)
  (cond
   [(one00? v) 'one00]
   [(one01? v) 'one01]
   [(one02? v) 'one02]
   [(one03? v) 'one03]
   
   [(one10? v) 'one10]
   [(one11? v) 'one11]
   [(one12? v) 'one12]
   [(one13? v) 'one13]
   
   [(one20? v) 'one20]
   [(one21? v) 'one21]
   [(one22? v) 'one22]
   [(one23? v) 'one23]
   
   [(one30? v) 'one30]
   [(one31? v) 'one31]
   [(one32? v) 'one32]
   [(one33? v) 'one33]))

(define (multi v)
  (cond
   [(two130? v) 'two130]
   [(two131? v) 'two131]
   [(two132? v) 'two132]
   [(two133? v) 'two133]
   
   [(one10? v) 'one10]
   [(one11? v) 'one11]
   [(one12? v) 'one12]
   [(one13? v) 'one13]
   
   [(one20? v) 'one20]
   [(one21? v) 'one21]
   [(one22? v) 'one22]
   [(one23? v) 'one23]
   
   [(base0? v) 'base0]
   [(base1? v) 'base1]
   [(base2? v) 'base2]
   [(base3? v) 'base3]))

(define (dummy v)
  'ok)

(define (go f v n)
  (time
   (let loop ([n n])
     (unless (zero? n)
	     (f v)
	     (loop (sub1 n))))))

(define check
  (lambda (l)
    (cond
     [(null? l) #f]
     [else
      (test (caddr l) (car l) (cadr l))
      (check (cdddr l))])))

(define ones-test
  (list x00 'one00
	x10 'one10
	x11 'one11
	x12 'one12
	x13 'one13
	x33 'one33))
	
(define multi-test
  (list x00 'base0
	x10 'one10
	x11 'one11
	x12 'one12
	x13 'one13
	x33 'base3
	x132 'two132))	     

(letrec ([bundle
	  (lambda (l f)
	    (if (null? l)
		null
		(list* f (car l) (cadr l)
		       (bundle (cddr l) f))))])
  (check (append
	  (bundle ones-test ones)
	  (bundle multi-test multi)
	  (list base1-a x11 1
		one11-a x11 2
		one10-a x10 1
		
		base1-a x31 1
		one31-z x31 4
		
		base2-l x132 1
		two132-a x132 6
		one32-y x132 4))))


(error-test '(struct x (y z)) exn:application:arity?)
(error-test '(let ([x (struct x (y z))]) 10) exn:application:arity?)

(report-errs)
