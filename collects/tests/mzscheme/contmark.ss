

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'continuation-marks)

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list (current-continuation-marks) key))

(test null extract-current-continuation-marks 'key)

(test '(10) 'wcm (with-continuation-mark 'key 10 
		   (extract-current-continuation-marks 'key)))
(test '(11) 'wcm (with-continuation-mark 'key 10 
		   (with-continuation-mark 'key 11
		     (extract-current-continuation-marks 'key))))
(test '(9) 'wcm (with-continuation-mark 'key 10 
	     (with-continuation-mark 'key2 9
	       (with-continuation-mark 'key 11
		 (extract-current-continuation-marks 'key2)))))
(test '() 'wcm (with-continuation-mark 'key 10 
	    (with-continuation-mark 'key2 9
	      (with-continuation-mark 'key 11
		(extract-current-continuation-marks 'key3)))))

(test '() 'wcm (let ([x (with-continuation-mark 'key 10 (list 100))])
		 (extract-current-continuation-marks 'key)))

(test '(11) 'wcm (with-continuation-mark 'key 11
		   (let ([x (with-continuation-mark 'key 10 (extract-current-continuation-marks 'key))])
		     (extract-current-continuation-marks 'key))))

(test '((11) (10 11) (11)) 'wcm (with-continuation-mark 'key 11
				  (list (extract-current-continuation-marks 'key)
					(with-continuation-mark 'key 10 (extract-current-continuation-marks 'key))
					(extract-current-continuation-marks 'key))))

(test '(11) 'wcm-invoke/tail (with-continuation-mark 'x 10
			       (invoke-unit
				(unit 
				  (import)
				  (export)
				  
				  (with-continuation-mark 'x 11
				    (continuation-mark-set->list
				     (current-continuation-marks)
				     'x))))))

(test '(11 10) 'wcm-invoke/nontail (with-continuation-mark 'x 10
				     (invoke-unit
				      (unit 
					(import)
					(export)
					
					(define l (with-continuation-mark 'x 11
						    (continuation-mark-set->list
						     (current-continuation-marks)
						     'x)))
					l))))

(test '(11 10) 'wcm-begin0 (with-continuation-mark 'x 10
			     (begin0
			      (with-continuation-mark 'x 11
				(extract-current-continuation-marks 'x))
			      (+ 2 3))))
(test '(11 10) 'wcm-begin0/const (with-continuation-mark 'x 10
				   (begin0
				    (with-continuation-mark 'x 11
				      (extract-current-continuation-marks 'x))
				    'constant)))

(define (get-marks)
  (extract-current-continuation-marks 'key))

(define (tail-apply f)
  (with-continuation-mark 'key 'tail
    (f)))

(define (non-tail-apply f)
  (with-continuation-mark 'key 'non-tail
    (car (cons (f) null))))

(test '(tail) tail-apply get-marks)
(test '(non-tail) non-tail-apply get-marks)
(test '(tail non-tail) non-tail-apply (lambda () (tail-apply get-marks)))
(test '(non-tail) tail-apply (lambda () (non-tail-apply get-marks)))

(define (mark-x f)
  (lambda ()
    (with-continuation-mark 'key 'x (f))))

(test '(x) tail-apply (mark-x get-marks))
(test '(x non-tail) non-tail-apply (mark-x get-marks))

(test '(x) tail-apply (lambda () (tail-apply (mark-x get-marks))))
(test '(x non-tail non-tail) non-tail-apply (lambda () (non-tail-apply (mark-x get-marks))))
(test '(x non-tail) tail-apply (lambda () (non-tail-apply (mark-x get-marks))))
(test '(x non-tail) non-tail-apply (lambda () (tail-apply (mark-x get-marks))))

;; Make sure restoring continuations restores the marks:
(let ([l null])
 (let ([did-once? #f]
       [did-twice? #f]
       [try-again #f]
       [get-marks #f])
   
   (with-continuation-mark
    'key (let/cc k (set! try-again k) 1)
    (begin
      (unless did-once?
	(set! get-marks (let/cc k k)))
      (set! l (cons (extract-current-continuation-marks 'key) l))))

   (if did-once?
       (unless did-twice?
	 (set! did-twice? #t)
	 (get-marks #f))
       (begin
	 (set! did-once? #t)
	 (try-again 2))))

 (test '((1) (2) (1)) 'call/cc-restore-marks l))

(define (p-equal? a b)
  (let loop ([a a][b b])
    (cond
     [(eq? a b) #t]
     [(equal? (car a) (car b))
      (loop (cdr a) (cdr b))]
     [else
      (printf "a: ~s~n" a)
      (printf "b: ~s~n" b)
      #f])))

;; Create a deep stack with a deep mark stack
(test #t
      'deep-stacks
      (p-equal?
       (let loop ([n 1000][l null])
	 (if (zero? n)
	     l
	     (loop (sub1 n) (cons n l))))
       (let loop ([n 1000])
	 (if (zero? n)
	     (extract-current-continuation-marks 'x)
	     (let ([x (with-continuation-mark 'x n (loop (sub1 n)))])
	       x)))))

;; Create a deep mark stack 10 times
(let loop ([n 10])
  (unless (zero? n)
    (let* ([max 1000]
	   [r (add1 (random max))])
      (test (list 0 r)
	    `(loop ,n)
	    (with-continuation-mark 'base 0
	      (let loop ([n max])
		(if (zero? n)
		    (append
		     (extract-current-continuation-marks 'base)
		     (extract-current-continuation-marks r))
		    (with-continuation-mark n n
		      (loop (sub1 n))))))))
    (loop (sub1 n))))

;; Make sure marks are separate in separate threads
(let ([s1 (make-semaphore 0)]
      [s2 (make-semaphore 0)]
      [result null])
  (thread (lambda ()
	    (with-continuation-mark 'key 'b.1
	      (begin
		(semaphore-wait s1)
		(with-continuation-mark 'key 'b.2
		  (begin
		    (semaphore-post s2)
		    (semaphore-wait s1)
		    (with-continuation-mark 'key 'b.4
		      (begin
			(set! result (extract-current-continuation-marks 'key))
			(semaphore-post s2)))
		    'ok))
		'ok))))
  (thread-wait
   (thread (lambda ()
	     (with-continuation-mark 'key 'a.1
	       (begin
		 (semaphore-post s1)
		 (with-continuation-mark 'key 'a.2
		   (begin
		     (semaphore-wait s2)
		     (with-continuation-mark 'key 'a.3
		       (begin
			 (semaphore-post s1)
			 (with-continuation-mark 'key 'a.4
			   (begin
			     (semaphore-wait s2)
			     (set! result (append (extract-current-continuation-marks 'key) result))))
			 'ok))
		     'ok))
		 'ok)))))
  (test '(a.4 a.3 a.2 a.1 b.4 b.2 b.1) 'thread-marks result))

(arity-test current-continuation-marks 0 0)
(arity-test continuation-mark-set->list 2 2)
(arity-test continuation-mark-set? 1 1)

(error-test '(continuation-mark-set->list 5 1))

(test #f continuation-mark-set? 5)
(test #t continuation-mark-set? (current-continuation-marks))

(report-errs)
