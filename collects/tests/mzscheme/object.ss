
; Test MzScheme's object system

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'OBJECT)

(define (test-class* cl* renames)
  (syntax-test  `(,cl*))
  (syntax-test  `(,cl* ,@renames . x))
  (syntax-test  `(,cl* ,@renames 0))
  (syntax-test  `(,cl* ,@renames () . x))
  (syntax-test  `(,cl* ,@renames () 0))
  (syntax-test  `(,cl* ,@renames () x))
  (syntax-test  `(,cl* ,@renames () ()))
  (syntax-test  `(,cl* ,@renames () () (0) x))
  (syntax-test  `(,cl* ,@renames () () 0))
  (syntax-test  `(,cl* ,@renames () () . x))
  (syntax-test  `(,cl* ,@renames () () () . x))
  (syntax-test  `(,cl* ,@renames () () () x))
  (syntax-test  `(,cl* ,@renames () () () public))
  (syntax-test  `(,cl* ,@renames () () () (x)))
  (syntax-test  `(,cl* ,@renames () () (x) ()))

  (begin
    (define (try-dotted cl)
      (syntax-test  `(,cl* ,@renames () () () (,cl . x))))
    
    (map try-dotted '(public private inherit rename
			     inherit-from rename-from
			     sequence)))
  
  (begin
    (define (try-defn-kind cl)
      (syntax-test  `(,cl* ,@renames () () () (,cl 8)))
      (syntax-test  `(,cl* ,@renames () () () (,cl [8 9])))
      (syntax-test  `(,cl* ,@renames () () () (,cl [(x) 9])))
      (syntax-test  `(,cl* ,@renames () () () (,cl [(x y x) 9])))
      (syntax-test  `(,cl* ,@renames () () () (,cl [x . 1])))
      (syntax-test  `(,cl* ,@renames () () () (,cl [x 1 . 3])))
      (syntax-test  `(,cl* ,@renames () () () (,cl [x 1 3]))))
    
    (try-defn-kind 'public)
    (try-defn-kind 'private))

  (begin
    (define (try-defn-rename-kind cl)
      (syntax-test  `(,cl* ,@renames () () () (,cl [((x) y) 9])))
      (syntax-test  `(,cl* ,@renames () () () (,cl [(x (y)) 9])))
      (syntax-test  `(,cl* ,@renames () () () (,cl [(x . y) 9])))
      (syntax-test  `(,cl* ,@renames () () () (,cl [(x 1) 9])))
      (syntax-test  `(,cl* ,@renames () () () (,cl [(1 x) 9]))))
    (try-defn-rename-kind 'public))

  (begin
    (define (try-ref-kind cl)
      (syntax-test  `(,cl* ,@renames () () () (,cl 8)))
      (syntax-test  `(,cl* ,@renames () () () (,cl x 8)))
      (syntax-test  `(,cl* ,@renames () () () (,cl (x . y))))
      (syntax-test  `(,cl* ,@renames () () () (,cl (x y z)))))
    
    (map try-ref-kind '(inherit rename share)))
  (error-test `(,cl* ,@renames () () () (inherit x)) exn:object:inherit?)
  (error-test `(,cl* ,@renames () () () (inherit (x y))) exn:object:inherit?)
  (syntax-test  `(,cl* ,@renames () () () (inherit (x y z))))
  (syntax-test  `(,cl* ,@renames () () () (inherit (x 5))))
  (syntax-test  `(,cl* ,@renames () () () (inherit (x))))
  (syntax-test  `(,cl* ,@renames () () () (rename x)))
  (syntax-test  `(,cl* ,@renames () () () (rename (x))))
  (syntax-test  `(,cl* ,@renames () () () (rename ((x) y))))
  (syntax-test  `(,cl* ,@renames () () () (rename ((x y) y))))
  (syntax-test  `(,cl* ,@renames () () () (rename ((1) y))))

  (syntax-test  `(,cl* ,@renames () () () (sequence 1 . 2)))
  
  (syntax-test  `(,cl* ,@renames () () () (public [x 7] [x 9])))
  (syntax-test  `(,cl* ,@renames () () (x) (public [x 7])))
  (syntax-test  `(,cl* ,@renames () () (x) (public [(x w) 7])))
  (syntax-test  `(,cl* ,@renames () () () (public [(x y) 7] [(z y) 9])))
  (syntax-test  `(,cl* ,@renames () () () (public [(x y) 7] [(x z) 9])))

  (syntax-test  `(,cl* ,@renames () a ()))
  (syntax-test  `(,cl* ,@renames () (1 . a) ())))

(test-class* 'class* ())
(test-class* 'class*/names '((this super)))

(syntax-test  `(class*/names 8 () () () ()))
(syntax-test  `(class*/names () () () ()))
(syntax-test  `(class*/names (8) () () ()))
(syntax-test  `(class*/names (this . 8) () () ()))
(syntax-test  `(class*/names (this 8) () () ()))
(syntax-test  `(class*/names (this super-init . 8) () () ()))
(syntax-test  `(class*/names (this super-init 8) () () ()))

(test #t class? (class* () () ()))
(test #t class? (class* () () ()))
(test #t class? (class* () () x))
(test #t class? (class* () () () (public)))
(test #t class? (class* () () () (public sequence)))
(test #t class? (class* () () (x) (public [(y x) 9])))
(test #t class? (class*/names (this super-init) () () () (public)))

(syntax-test  `(interface))
(syntax-test  `(interface . x))
(syntax-test  `(interface 8))
(syntax-test  `(interface () 8))
(syntax-test  `(interface () x . y))
(syntax-test  `(interface () x 8))
(syntax-test  `(interface () x x))
(error-test `(interface (8) x) exn:object:interface-type?)

(test #t interface? (interface ()))
(test #t interface? (interface () x))
(test #f interface? (class* () () ()))

(define i0.1 (interface () x y))
(define i0.2 (interface () y c d))
(define i1 (interface (i0.1 i0.2) e))
(define ix (interface () x y))

(test #t interface-extension? i1 i0.1)
(test #t interface-extension? i1 i0.2)
(test #f interface-extension? i0.1 i1)
(test #f interface-extension? i0.2 i1)
(test #f interface-extension? i0.2 i0.1)
(test #f interface-extension? i0.1 i0.2)

(error-test '(let [(bad (class* () (i0.1) ()))] bad) exn:object:implement?)
(test #t class? (class* () (i0.1) () (public x y)))
(error-test '(let ([cl (class* () (i0.1 i0.2) () (public x y c))]) cl) exn:object:implement?)
(error-test '(class* () (i1) () (public x y c)) exn:object:implement?)
(test #t class? (class* () (i0.1 i0.1) () (public x y c d)))
(error-test '(class* () (i1) () (public x y c d)) exn:object:implement?)
(test #t class? (class* () (i1) () (public x y c d e)))

(define c1 
  (let ((v 10))
    (class* '() (i1) (in [in-2 'banana] . in-rest)
	   (public (x 1) (y 2))
	   (private (a in) (b3 3))
	   (public (b1 2) (b2 2) (e 0))
	   (public (c 3) (d 7)
		   (f-1-a (lambda () a))
		   (f-1-b1 (lambda () b1))
		   (f-1-b2 (lambda () b2))
		   (f-1-c (lambda () c))
		   (f-1-v (lambda () v))
		   (f-1-x (lambda () x))
		   (f-1-top-a (lambda () (ivar this a)))
		   (f-1-other-e (lambda (o) (ivar o e)))
		   (f-1-set-b2 (lambda (v) (set! b2 v) b2))
		   (f-1-in-2 (lambda () in-2))
		   (f-1-in-rest (lambda () in-rest)))
	   (sequence
	     (set! e in)))))
  
(test #t implementation? c1 i0.1)
(test #t implementation? c1 i0.2)
(test #t implementation? c1 i1)
(test #f implementation? c1 ix)

(define o1 (make-object c1 0 'apple "first" "last"))

(define c2 
  (let ((v 20))
    (class c1 ()
	   (inherit b2 (sup-set-b2 f-1-set-b2))
	   (rename (also-e e)
		   (also-b2 b2))
	   (public (a 4) (b1 5) (c 6)
		   (f-2-a (lambda () a))
		   (f-2-b1 (lambda () b1))
		   (f-2-b2 (lambda () b2))
		   (f-2-also-b2 (lambda () also-b2))
		   (f-2-c (lambda () c))
		   ((i-f-2-v f-2-v) (lambda () v))
		   (f-2-v-copy (lambda () (i-f-2-v)))
		   (f-2-set-b2 (lambda (v) (sup-set-b2 v))))
	   (private (y 3))
	   (sequence
	     (super-init 1)))))

(test #t implementation? c2 i0.1)
(test #t implementation? c2 i0.2)
(test #t implementation? c2 i1)
(test #f implementation? c2 ix)

(define o2 (make-object c2))

(define c2.1
  (class*/names (this c2-init) c2 () ()
	  (sequence
	    (c2-init))))

(define o2.1 (make-object c2.1))

(define c3
  (class* () () ()
	  (public (x 6) (z 7) (b2 8)
		  (f-3-b2 (lambda () b2)))))

(define o3 (make-object c3))

(define c6
  (class null (x-x)
    (public
     [(i-a x-a) (lambda () 'x-a)]
     [(x-a i-a) (lambda () 'i-a)]
     [(i-x x-x) (lambda () 'x-x)]
     [x-a-copy (lambda () (i-a))]
     [i-a-copy (lambda () (x-a))])))

(define o6 (make-object c6 'bad))

(define c7
  (class*/names (self super-init) () () ()
    (public
     [get-self (lambda () self)])))

(define o7 (make-object c7))

(define display-test 
  (lambda (p v)
    (printf "Should be ~s: ~s ~a~n"
	    p v (if (equal? p v)
		    ""
		    "ERROR"))))

(define ivar? exn:object:ivar?)

(test #t is-a? o1 c1)
(test #t is-a? o1 i1)
(test #t is-a? o2 c1)
(test #t is-a? o2 i1)
(test #f is-a? o1 c2)
(test #t is-a? o2 c2)
(test #t is-a? o2.1 c1)
(test #f is-a? o1 c3)
(test #f is-a? o2 c3)
(test #f is-a? o1 ix)
(test #f is-a? o2 ix)
(test #f is-a? o3 i1)
(test #f is-a? i1 i1)
(test #t subclass? c2 c1)
(test #t subclass? c2.1 c1)
(test #f subclass? c1 c2)
(test #f subclass? c1 c3)
(test #f subclass? i1 c3)
(test #t ivar-in-class? 'f-1-a c1)
(test #t ivar-in-class? 'f-1-a c2)
(test #f ivar-in-class? 'f-2-a c1)
(test #t ivar-in-class? 'f-2-a c2)

(error-test '(is-a? o1 o1))
(error-test '(subclass? o1 o1))
(error-test '(subclass? o1 i1))
(error-test '(implementation? o1 o1))
(error-test '(implementation? o1 c1))
(error-test '(ivar-in-class? 0 c1))
(error-test '(ivar-in-class? 'a i1))
(error-test '(ivar-in-class? 'a o1))

(arity-test object? 1 1)
(arity-test class? 1 1)
(arity-test interface? 1 1)
(arity-test is-a? 2 2)
(arity-test subclass? 2 2)
(arity-test interface-extension? 2 2)
(arity-test ivar-in-class? 2 2)

(arity-test uq-ivar 2 2)
(arity-test uq-make-generic 2 2)

(error-test '(ivar o1 a) ivar?)
(test 4 uq-ivar o2 'a)

(test 0 'send (send o1 f-1-a))
(test 1 'send (send o2 f-1-a))
(test 4 'send (send o2 f-2-a))

(test 'apple 'send (send o1 f-1-in-2))
(test 'banana 'send (send o2 f-1-in-2))
(test '("first" "last") 'send (send o1 f-1-in-rest))
(test '() 'send (send o2 f-1-in-rest))

(error-test '(send o1 f-1-top-a) ivar?)
(test 4 'send (send o2 f-1-top-a))

(test 5 uq-ivar o2 'b1)

(test 2 'send (send o1 f-1-b1))
(test 2 'send (send o1 f-1-b2))
(test 5 'send (send o2 f-1-b1))
(test 2 'send (send o2 f-1-b2))
(test 5 'send (send o2 f-2-b1))
(test 2 'send (send o2 f-2-b2))
(test 2 'send (send o2 f-2-also-b2))

(test 3 uq-ivar o1 'c)
(test 6 uq-ivar o2 'c)

(test 3 'send (send o1 f-1-c))
(test 6 'send (send o2 f-1-c))
(test 6 'send (send o2 f-2-c))

(test 7 uq-ivar o1 'd)
(test 7 uq-ivar o2 'd)

(test 10 'send (send o1 f-1-v))
(test 10 'send (send o2 f-1-v))
(test 20 'send (send o2 f-2-v))
(test 20 'send (send o2 f-2-v-copy))

(error-test '(ivar o2 i-f-2-v) ivar?)

(test 0 'send (send o1 f-1-other-e o1))
(test 1 'send (send o1 f-1-other-e o2))

(test 2 uq-ivar o2 'y)

(test 3 'send (send o2 f-2-set-b2 3))
(test 3 'send (send o2 f-2-also-b2))

(test 'i-a 'send (send o6 i-a))
(test 'x-a 'send (send o6 x-a))
(test 'i-a 'send (send o6 i-a-copy))
(test 'x-a 'send (send o6 x-a-copy))
(test 'x-x 'send (send o6 x-x))

(test #t eq? o7 (send o7 get-self))

(define g1 (make-generic c1 x))
(test 1 g1 o1)
(test 1 g1 o2)

(define g2 (make-generic c2 x))
(test 1 g2 o2)

(define g0 (make-generic i0.1 x))
(test 1 g0 o1)
(test 1 g0 o2)
(test 'hi g0 (make-object (class* () (i0.1) () (public [x 'hi][y 'bye]))))

(error-test '(g2 o1) exn:object:generic?)
(error-test '(g0 o3) exn:object:generic?)

(error-test '(class* 7 () ()) exn:object:class-type?)
(error-test '(let ([c (class* 7 () ())]) c) exn:object:class-type?)
(error-test '(class* () (i1 7) ()) exn:object:interface-type?)
(error-test '(let ([c (class* () (i1 7) ())]) c) exn:object:interface-type?)
(error-test '(interface (8) x) exn:object:interface-type?)
(error-test '(let ([i (interface (8) x)]) i) exn:object:interface-type?)
(error-test '(interface (i1 8) x) exn:object:interface-type?)
(error-test '(make-generic c2 not-there) exn:object:class-ivar?)

(error-test '(make-object (class* c1 () ())) exn:object:init:never?)
(error-test '(make-object (let ([c (class* c1 () ())]) c)) exn:object:init:never?)

(error-test '(make-object 
	      (class* c2 () () (sequence (super-init) (super-init)))) 
	    exn:object:init:multiple?)
(error-test '(make-object 
	      (let ([c (class* c2 () () (sequence (super-init) (super-init)))]) c))
	    exn:object:init:multiple?)

(error-test '(make-object (class null (x))) exn:application:arity?)
(error-test '(make-object (let ([c (class null (x))]) c)) exn:application:arity?)
	      

(define c100
  (let loop ([n 99][c c1])
    (if (zero? n)
	c
	(loop (sub1 n) (class c args 
			      (public (z n))
			      (sequence
				(apply super-init args)))))))

(define o100 (make-object c100 100))
(test 100 'send (send o100 f-1-a))
(test 1 'ivar (ivar o100 z))

(test 5 'init (let ([g-x 8]) (make-object (class* () () ([x (set! g-x 5)]))) g-x))
(test 8 'init (let ([g-x 8]) (make-object (class* () () ([x (set! g-x 5)])) 0) g-x))

(test (letrec ([x x]) x) 'init (send (make-object 
				      (class* () () ([x y] [y x]) (public (f (lambda () x)))))
				     f))

(define inh-test-expr
 (lambda (super derive-pre? rename? override? override-pre?)
   (let* ([order
	   (lambda (pre? a b)
	     (if pre?
		 (list a b)
		 (list b a)))]
	  [base-class
	   `(class ,(if super
			super
			'(class null (n) (public [name (lambda () n)])))
		   ()
		   ,(if (not rename?)
			'(inherit name)
			'(rename [super-name name]))
		   ,@(order
		      derive-pre?
		      `(public [w ,(if rename? 'super-name 'name)])
		      '(sequence (super-init 'tester))))])
     `(ivar
       (make-object
	,(if override?
	     `(class ,base-class ()
		     ,@(order
			override-pre?
			'(sequence (super-init))
			'(public [name (lambda () 'o-tester)])))
	     base-class))
       w))))

(define (do-override-tests super)
  (define (eval-test v e)
    (teval `(test ,v (quote, e)
		  (let ([v ,e]) 
		    (if (procedure? v)
			(v)
			v)))))

  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #f  #f #f))
  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #f  #t #t))
  (eval-test '(letrec ([x x]) x) (inh-test-expr super #f #f  #t #t))

  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #t  #f #f))
  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #t  #t #f))
  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #t  #t #t))
  
  (eval-test ''tester (inh-test-expr super #f #f  #f #f))
  (eval-test ''o-tester (inh-test-expr super #t #f  #t #f))
  (eval-test ''o-tester (inh-test-expr super #f #f  #t #f))

  (eval-test ''tester (inh-test-expr super #f #t  #f #f))  
  (eval-test ''tester (inh-test-expr super #f #t  #t #t))
  (eval-test ''tester (inh-test-expr super #f #t  #t #f)))

(do-override-tests #f)

(when (defined? 'primclass%)
      (error-test '(make-object primclass%) exn:application:arity?)
      (error-test '(make-object primsubclass%) exn:application:arity?)

      (define o (make-object primclass% 'tester))
      (arity-test (ivar o name) 0 0)
      (test 'tester (ivar o name))
      (test "primclass%" (ivar o class-name))

      (define o2 (make-object primsubclass% 'tester))
      (arity-test (ivar o2 name) 0 0)
      (arity-test (ivar o2 detail) 0 0)
      (test 'tester (ivar o2 name))
      (test #f (ivar o2 detail))
      (test "primsubclass%" (ivar o2 class-name))

      (do-override-tests 'primclass%)
      (do-override-tests 'primsubclass%)

      (define name-g (make-generic primclass% name))
      (define class-name-g (make-generic primclass% class-name))

      (define sub-name-g (make-generic primsubclass% name))
      (define sub-class-name-g (make-generic primsubclass% class-name))
      (define sub-detail-g (make-generic primsubclass% detail))

      (test 'tester (name-g o))
      (test "primclass%" (class-name-g o))

      (test 'tester (name-g o2))
      (test "primsubclass%" (class-name-g o2))
      (test 'tester (sub-name-g o2))
      (test "primsubclass%" (sub-class-name-g o2))
      (test #f (sub-detail-g o2))

      (define c%
	(class primsubclass% ()
	   (inherit name detail class-name)
	   (sequence (super-init 'example))
	   (public
	    [n name]
	    [d detail]
	    [c class-name])))

      (define o3 (make-object c%))
      (test 'example (ivar o3 n))
      (test #f (ivar o3 d))
      (test "primsubclass%" (ivar o3 c))
      (test 'example (ivar o3 name))
      (test #f (ivar o3 detail))
      (test "primsubclass%" (ivar o3 class-name))

      (test 'example (name-g o3))
      (test "primsubclass%" (class-name-g o3))
      (test 'example (sub-name-g o3))
      (test "primsubclass%" (sub-class-name-g o3))
      (test #f (sub-detail-g o3)))

(report-errs)

