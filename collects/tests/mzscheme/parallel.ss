
;; Runs 3 threads perfoming the test suite simultaneously. Each
;;  thread creates a directory sub<n> to run in, so that filesystem
;;  tests don't collide.

; Runs n versions of test in parallel threads and namespaces, 
; waiting until all are done
(define (parallel n test)
  (let ([done (make-semaphore)]
	[go (make-semaphore)])
    (let loop ([n n])
      (unless (zero? n)
	      (let ([ns (make-namespace)]
		    [p (make-parameterization)])
		(thread
		 (lambda ()
		   (with-parameterization 
		    p
		    (lambda ()
		      (parameterize ([current-namespace ns]
				     [parameterization-branch-handler
				      (lambda () (make-parameterization p))])
				    (let ([dirname (format "sub~s" n)])
				      (unless (directory-exists? dirname)
					      (make-directory dirname))
				      (current-directory dirname)
				      (dynamic-wind
				       void
				       (lambda ()
					 (load test))
				       (lambda ()
					 (semaphore-post done)
					 (semaphore-wait go)
					 (printf "~nThread ~s:" n)
					 (eval '(report-errs))
					 (current-directory (build-path 'up))
					 (delete-directory dirname)
					 (semaphore-post done)))))))))
		(loop (sub1 n)))))
    (let loop ([n n])
      (unless (zero? n)
	      (semaphore-wait done)
	      (loop (sub1 n))))
    (let loop ([n n])
      (unless (zero? n)
	      (semaphore-post go)
	      (semaphore-wait done)
	      (loop (sub1 n))))))

(parallel 3 (path->complete-path "all.ss" (current-load-relative-directory)))
