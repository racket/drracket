
(unit/sig mzlib:thread^
  (import)

  #|
  t accepts a function, f, and creates a thread. It returns the thread and a
  function, g. When g is applied it passes it's argument to f, and evaluates
  the call of f in the time of the thread that was created. Calls to g do not
  block.
  |#
  
  (define consumer-thread
    (case-lambda
     [(f) (consumer-thread f void)]
     [(f init)
      (unless (procedure? f) (raise-type-error 'consumer-thread "procedure" f))
      (let ([sema (make-semaphore 0)]
	    [protect (make-semaphore 1)]
	    [front-state null]
	    [back-state null])
	(values 
	 (thread
	  (letrec ([loop
		    (lambda ()
		      (semaphore-wait sema)
		      (let ([local-state
			     (begin
			       (semaphore-wait protect)
			       (if (null? back-state)
				   (let ([new-front (reverse front-state)])
				     (set! back-state (cdr new-front))
				     (set! front-state null)
				     (semaphore-post protect)
				     (car new-front))
				   (begin0
				    (car back-state)
				    (set! back-state (cdr back-state))
				    (semaphore-post protect))))])
			(apply f local-state))
		      (loop))])
	    (lambda ()
	      (init)
	      (loop))))
	 (lambda new-state
	   (let ([num (length new-state)])
	     (unless (procedure-arity-includes? f num) 
		     (raise 
		      (make-exn:application:arity
		       (format "<procedure-from-consumer-thread>: consumer procedure arity is ~e; provided ~s argument~a"
			       (arity f) num (if (= 1 num) "" "s"))
		       (current-continuation-marks)
		       num
		       (arity f)))))
	   (semaphore-wait protect)
	   (set! front-state (cons new-state front-state))
	   (semaphore-post protect)
	   (semaphore-post sema))))]))


  (define (merge-input a b)
    (or (input-port? a)
	(raise-type-error 'merge-input "input-port" a))
    (or (input-port? b)
	(raise-type-error 'merge-input "input-port" b))
    (let-values ([(rd wt) (make-pipe)])
		(let* ([copy1-sema (make-semaphore 500)]
		       [copy2-sema (make-semaphore 500)]
		       [ready1-sema (make-semaphore)]
		       [ready2-sema (make-semaphore)]
		       [check-first? #t]
		       [close-sema (make-semaphore)]
		       [mk-copy (lambda (from to copy-sema ready-sema)
				  (lambda ()
				    (let loop ()
				      (semaphore-wait copy-sema)
				      (let ([c (read-char from)])
					(unless (eof-object? c)
						(semaphore-post ready-sema)
						(write-char c to)
						(loop))))
				    (semaphore-post close-sema)))])
		  (thread (mk-copy a wt copy1-sema ready1-sema)) 
		  (thread (mk-copy b wt copy2-sema ready2-sema))
		  (thread (lambda () 
			    (semaphore-wait close-sema)		 
			    (semaphore-wait close-sema)
			    (close-output-port wt)))
		  (make-input-port
		   (lambda () (let ([c (read-char rd)])
				(unless (eof-object? c)
					(if (and check-first? (semaphore-try-wait? ready1-sema))
					    (semaphore-post copy1-sema)
					    (if (not (semaphore-try-wait? ready2-sema))
						; check-first? must be #f
						(if (semaphore-try-wait? ready1-sema)
						    (semaphore-post copy1-sema)
						    (error 'join "internal error: char from nowhere!"))
						(semaphore-post copy2-sema)))
					(set! check-first? (not check-first?)))
				c))
		   (lambda () (char-ready? rd))
		   (lambda () (close-input-port rd))))))

  (define with-semaphore
    (lambda (s f)
      (semaphore-wait s)
      (begin0 (f)
	      (semaphore-post s))))

  (define semaphore-wait-multiple 
   (case-lambda
    [(semaphores) (semaphore-wait-multiple semaphores #f #f)]
    [(semaphores timeout) (semaphore-wait-multiple semaphores timeout #f)]
    [(semaphores timeout allow-break?)
     (let ([break-enabled? (or allow-break? (break-enabled))])
       (parameterize ([break-enabled #f])
	 (for-each
	  (lambda (s)
	    (or (semaphore? s) 
		(raise-type-error 'semaphore-wait-multiple "list of semaphores" semaphores)))
	  semaphores)
	 (or (not timeout) (real? timeout) (>= timeout 0)
	     (raise-type-error 'semaphore-wait-multiple "positive real number" timeout))
	 (let* ([result-l null]
		[ok? #f]
		[set-l (make-semaphore 1)]
		[one-done (make-semaphore)]
		[threads (let loop ([l semaphores])
			   (if (null? l)
			       null
			       (cons (let ([s (car l)])
				       (thread (lambda ()
						 (let/ec 
						     k
						   (current-exception-handler k)
						   (semaphore-wait/enable-break s)
						   (with-semaphore 
						    set-l
						    (lambda () (set! result-l 
								     (cons s result-l))))
						   (semaphore-post one-done)))))
				     (loop (cdr l)))))]
		[timer-thread (if timeout
				  (thread (lambda () (sleep timeout) (semaphore-post one-done)))
				  #f)])
	   (dynamic-wind
	    void
	    (lambda ()
	      ; wait until someone is done
	      ((if break-enabled? semaphore-wait/enable-break semaphore-wait) one-done)
	      (set! ok? #t))
	    (lambda ()
	      ; tell everyone to stop
	      (for-each (lambda (th) (break-thread th)) threads)
	      (when timer-thread (break-thread timer-thread))
	      ; wait until everyone's done
	      (for-each thread-wait threads)
	      ; If more that too manay suceeded, repost to the extras
	      (let ([extras (if ok? 
				(if (null? result-l) 
				    null 
				    (cdr result-l))
				result-l)])
		(for-each (lambda (s) (semaphore-post s)) extras))))
	   (if (null? result-l)
	       #f 
	       (car result-l)))))]))
 
  (define dynamic-enable-break
    (polymorphic
     (lambda (thunk)
       (parameterize ([break-enabled #t])
	 (thunk)))))
  
  (define dynamic-disable-break
    (polymorphic
     (lambda (thunk)
       (parameterize ([break-enabled #f])
	 (thunk)))))
  
  (define make-single-threader
    (polymorphic
     (lambda ()
       (let ([sema (make-semaphore 1)])
	 (lambda (thunk)
	   (dynamic-wind
	    (lambda () (semaphore-wait sema))
	    thunk
	    (lambda () (semaphore-post sema))))))))
   
   )

