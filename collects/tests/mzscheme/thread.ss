

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'threads)

(define t (thread (lambda () 8)))
(test #t thread? t)

(arity-test thread 1 1)
(error-test '(thread 5) type?)
(error-test '(thread (lambda (x) 8)) type?)
(arity-test thread? 1 1)

; Should be able to make an arbitrarily deep chain of custodians
; if only the first & last are accssible:
(test #t custodian?
      (let loop ([n 1000][c (current-custodian)])
	(if (zero? n)
	    c
	    (loop (sub1 n) (make-custodian c)))))

(define SLEEP-TIME 0.1)
(define result 0)
(define th1 0)
(define set-ready
  (let ([s (make-semaphore 1)]
	[r #f])
    (lambda (v)
      (semaphore-wait s)
      (begin0
       r
       (set! r v)
       (semaphore-post s)))))
(define cm (make-custodian))
(define th2 (parameterize ([current-custodian cm])
              (thread 
	       (lambda ()
		 (let ([cm2 (make-custodian cm)])
		   (parameterize ([current-custodian cm2])
		      (set! th1 (thread 
				 (lambda ()
				   (let loop ()
				     (let ([r (set-ready #f)])
				       (sleep SLEEP-TIME)
				       (set! result (add1 result))
				       (when r (semaphore-post r)))
				     (loop)))))))))))
(define start result)
(let ([r (make-semaphore)])
  (set-ready r)
  (semaphore-wait r))
(test #f eq? start result)
(kill-thread th2)
(set! start result)
(let ([r (make-semaphore)])
  (set-ready r)
  (semaphore-wait r))
(test #f eq? start result)
(test #t thread-running? th1)
(custodian-shutdown-all cm)
(thread-wait th1)
(set! start result)
(test #f thread-running? th1)
(sleep SLEEP-TIME)
(test #t eq? start result)

(error-test `(parameterize ([current-custodian cm]) (kill-thread (current-thread)))
	    exn:misc:thread:kill?)

(test #t custodian? cm)
(test #f custodian? 1)
(arity-test custodian? 1 1)

(arity-test make-custodian 0 1)
(error-test '(make-custodian 0))

(test (void) kill-thread t)
(arity-test kill-thread 1 1)
(error-test '(kill-thread 5) type?)

(test #t thread-running? (current-thread))
(arity-test thread-running? 1 1)
(error-test '(thread-running? 5) type?)

(arity-test sleep 0 1)
(error-test '(sleep 'a) type?)
(error-test '(sleep 1+3i) type?)

(define s (make-semaphore 1))

(test #t semaphore? s)

(arity-test make-semaphore 0 1)
(error-test '(make-semaphore "a") type?)
(error-test '(make-semaphore -1) type?)
(error-test '(make-semaphore 1.0) type?)
(error-test '(make-semaphore (expt 2 64)) exn:misc:semaphore?)
(arity-test semaphore? 1 1)

(define test-block
  (lambda (block? thunk)
    (let* ([hit? #f]
	   [t (parameterize
	       ([current-custodian (make-custodian)])
	       (thread (lambda () (thunk) (set! hit? #t))))])
      (sleep 0.1)
      (begin0 (test block? 'nondeterministic-block-test (not hit?))
	      (kill-thread t)))))

(test #t semaphore-try-wait? s) 
(test #f semaphore-try-wait? s) 
(semaphore-post s) 
(test #t semaphore-try-wait? s) 
(test #f semaphore-try-wait? s) 
(semaphore-post s) 
(test-block #f (lambda () (semaphore-wait s)))
(test-block #t (lambda () (semaphore-wait s)))
(semaphore-post s) 
(test-block #f (lambda () (semaphore-wait/enable-break s)))
(test-block #t (lambda () (semaphore-wait/enable-break s)))

(arity-test semaphore-try-wait? 1 1)
(arity-test semaphore-wait 1 1)
(arity-test semaphore-post 1 1)

(define s (make-semaphore))
(define result 0)
(define t-loop
  (lambda (n m)
    (lambda ()
      (if (zero? n)
	  (begin
	    (set! result m)
	    (semaphore-post s))
	  (thread (t-loop (sub1 n) (add1 m)))))))
(thread (t-loop 25 1))
(semaphore-wait s)
(test 26 'thread-loop result)

(report-errs)
