
(define make-invokable-unit
  (lambda (application)
    (let* ([U
	    (compound-unit/sig (import)
	      (link [core : mzlib:core^ (mzlib:core@)]
		    [trigger : mzlib:trigger^ (mzlib:trigger@)]
		    [mred : mred^ (mred@ core trigger application)]
		    [application : mred:application^ (application mred core)])
	      (export (open mred)
		      (open application)))])
      (compound-unit/sig (import)
	 (link [mred : ((open mred^) (open mred:application^)) (U)])
	 (export (unit mred))))))

(define (go flags)
  (define die? #f)
  (define my-app
    (unit/sig 
     mred:application^
     (import mred^ mzlib:core^)
     
     (define app-name "Tester")
     (define console (if (memq 'console flags)
			 (make-object console-frame%)
			 #f))
     (define eval-string pretty-print@:pretty-print)
     (when (memq 'thread flags)
	   (let ([s (make-semaphore 1)]
		 [s2 (make-semaphore 0)]
		 [done (make-semaphore 0)])
	     ; Use of semaphore-callback insures that thread is a child
	     ; of the eventspace
	     (semaphore-callback s
				 (lambda ()
				   (semaphore-post done)
				   (thread (lambda ()
					     (let loop ()
					       (sleep 1)
					       (loop))))
				   (when (begin0
					  die?
					  (set! die? (not die?)))
				       (kill-thread (current-thread))))) ; kills handler thread
	     ; Add another callback that we know will not get triggered
	     (semaphore-callback s2 void)
	     (wx:yield done)))
     (when (memq 'eventspace flags)
	   (let ([e (wx:make-eventspace)])
	     (parameterize ([wx:current-eventspace e])
		(send (make-object wx:frame% null "Testing" -1 -1 100 100)
		      show #t))))
     (unless (memq 'force flags)
	     (run-exit-callbacks))))
  
  (let loop ()
    (collect-garbage)
    (collect-garbage)
    (dump-memory-stats)
    (let ([e (if (memq 'force flags)
		 (wx:make-eventspace)
		 (wx:current-eventspace))])
      (parameterize ([wx:current-eventspace e])
	  (invoke-unit/sig
	   (make-invokable-unit my-app)))
      (when (memq 'force flags)
	    (wx:kill-eventspace e)))
    (loop)))



   