
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))


(SECTION 'class/d)

(require-library "classd.ss")

(syntax-test '(class/d object% ((public x)) (define (x) 1)))
(syntax-test '(class/d object% () ((public x))))
;; Should this be an error?
; (syntax-test '(class/d object% () ((public x x)) (define x 10)))

(test
 1
 'test-1
 (send (make-object (class/d object% () ((public y)) (define (y) 1) (super-init))) y))

(test
 1
 'test-2
 (send (make-object (class/d object% () ((public y)) (define (y) 1) (define (z) 1) (super-init))) y))

(test
 3
 'test-3
 (let ([x 1])
   (make-object 
    (class/d object% () ()
	     (set! x 2)
	     (set! x 3)
	     (super-init)))
   x))

(test
 2
 'test-4
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((override x))
                             (super-init)
                             (define (x) 2)))
       x))


(test
 2
 'test-5
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((inherit x)
                              (public y))
                             (super-init)
                             (define (y) (+ (x) (x)))))
       y))

(test
 2
 'test-6
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((rename [super-x x])
                              (public y))
                             (super-init)
                             (define (y) (+ (super-x) (super-x)))))
       y))

(test
 2
 'test-7
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((rename [super-x x])
                              (override x))
                             (super-init)
                             (define (x) (+ (super-x) (super-x)))))
       x))

(test
 2
 'test-8
 (send (make-object (class/d object% (xxx)
		      ((public x))
		      (define (x) xxx)
		      (super-init))
	 2)
       x))

(test
 1
 'test-9
 (send (make-object (class/d*/names (local-this local-super-init)
				    object%
				    ((interface ()))
				    ()
				    ((public x))
				    (define (x) 1)
				    (local-super-init)))
       x))

(test
 1
 'test-10
 (send (make-object (class/d* object%
			      ((interface ()))
			      ()
			      ((public x))
			      (define (x) 1)
			      (super-init)))
       x))

(test
 77
 'test-10
 (ivar (make-object (class/d object% ()
			     ((public x))
			     (define y 77)
			     (define x y)
			     (super-init)))
       x))

(test
 (cons 78 16)
 'test-10
 (ivar (make-object (class/d (class object% () (public [x 16]) (sequence (super-init))) ()
			     ((override x)
			      (rename [super-x x]))
			     (super-init)
			     (define y 78)
			     (define x (cons y super-x))))
       x))

(test
 (cons 79 29)
 'test-10
 (ivar (make-object (class (class/d object% ()
				    ((public x z))
				    (define y 79)
				    (define x 19)
				    (define z (cons y x))
				    (super-init)) ()
		      (override
			[x 29])
		      (sequence
			(super-init))))
       z))
