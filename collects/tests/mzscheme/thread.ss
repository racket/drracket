

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'threads)

(define SLEEP-TIME 0.1)

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

(let ([kept-going? #f])
  (let ([c (make-custodian)])
    (parameterize ([current-custodian c])
     (thread-wait
      (thread
       (lambda ()
	 (custodian-shutdown-all c)
	 (set! kept-going? #t))))))
  (test #f 'kept-going-after-shutdown? kept-going?))

(error-test `(parameterize ([current-custodian cm]) (kill-thread (current-thread)))
	    exn:misc?)

(test #t custodian? cm)
(test #f custodian? 1)
(arity-test custodian? 1 1)

(arity-test custodian-shutdown-all 1 1)

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
(error-test '(make-semaphore (expt 2 64)) exn:application:mismatch?)
(arity-test semaphore? 1 1)

(define test-block
  (lambda (block? thunk)
    (let* ([hit? #f]
	   [t (parameterize ([current-custodian (make-custodian)])
		(thread (lambda () (thunk) (set! hit? #t))))])
      (sleep SLEEP-TIME)
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

; Make sure you can break a semaphore-wait:
(test 'ok
      'break-semaphore-wait
      (let* ([s1 (make-semaphore 0)]
	     [s2 (make-semaphore 0)]
	     [t (thread (lambda ()
			  (semaphore-post s1)
			  (with-handlers ([exn:misc:user-break? (lambda (x) (semaphore-post s2))])
			    (semaphore-wait (make-semaphore 0)))))])
	(semaphore-wait s1)
	(sleep SLEEP-TIME)
	(break-thread t)
	(semaphore-wait s2)
	'ok))

; Make sure two waiters can be released
(test 'ok
      'double-semaphore-wait
      (let* ([s1 (make-semaphore 0)]
	     [s2 (make-semaphore 0)]
	     [go (lambda ()
		   (semaphore-post s2)
		   (semaphore-wait s1)
		   (semaphore-post s2))])
	(thread go) (thread go)
	(semaphore-wait s2) (semaphore-wait s2)
	(semaphore-post s1) (semaphore-post s1)
	(semaphore-wait s2) (semaphore-wait s2)
	'ok))

; Tests inspired by a question from David Tillman
(define (read-line/expire1 port expiration)
  (with-handlers ([exn:misc:user-break? (lambda (exn) #f)])
    (let ([timer (thread (let ([id (current-thread)])
			   (lambda () 
			     (sleep expiration)
			     (break-thread id))))])
      (dynamic-wind
       void
       (lambda () (read-line port))
       (lambda () (kill-thread timer))))))
(define (read-line/expire2 port expiration)
  (let ([done (make-semaphore 0)]
	[result #f])
    (let ([t1 (thread (lambda () 
			(set! result (read-line port))
			(semaphore-post done)))]
	  [t2 (thread (lambda () 
			(sleep expiration)
			(semaphore-post done)))])
      (semaphore-wait done)
      (kill-thread t1)
      (kill-thread t2)
      result)))

(define (go read-line/expire)
  (define p (let ([c 0]
		  [nl-sleep? #f]
		  [nl? #f])
	      (make-input-port (lambda () 
				 (when nl-sleep?
				       (sleep 0.4)
				       (set! nl-sleep? #f))
				 (if nl?
				     (begin
				       (set! nl? #f)
				       #\newline)
				     (begin
				       (set! nl? #t)
				       (set! nl-sleep? #t)
				       (set! c (add1 c))
				       (integer->char c))))
			       (lambda ()
				 (when nl-sleep?
				       (sleep 0.4)
				       (set! nl-sleep? #f))
				 #t)
			       void)))
  (test #f read-line/expire p 0.2) ; should get char but not newline
  (test "" read-line/expire p 0.6)) ; picks up newline

(go read-line/expire1)
(go read-line/expire2)

;; Make sure queueing works, and check kill/wait interaction:
(let* ([s (make-semaphore)]
       [l null]
       [wait (lambda (who)
	       (thread
		(lambda ()
		  (semaphore-wait s)
		  (set! l (cons who l)))))]
       [pause (lambda () (sleep 0.01))])
  (wait 0) (pause)
  (wait 1) (pause)
  (wait 2)
  (pause)
  (test null 'queue l)
  (semaphore-post s) (pause)
  (test '(0) 'queue l)
  (semaphore-post s) (pause)
  (test '(1 0) 'queue l)
  (semaphore-post s) (pause)
  (test '(2 1 0) 'queue l)
  
  (set! l null)
  (wait 0) (pause)
  (let ([t (wait 1)])
    (pause)
    (wait 2)
    (pause)
    (test null 'queue l)
    (kill-thread t)
    (semaphore-post s) (pause)
    (test '(0) 'queue l)
    (semaphore-post s) (pause)
    (test '(2 0) 'queue l)
    (semaphore-post s) (pause)
    (test '(2 0) 'queue l)
    (wait 3) (pause)
    (test '(3 2 0) 'queue l)))

;; Nested threads
(test 5 call-in-nested-thread (lambda () 5))

(error-test '(call-in-nested-thread (lambda () (kill-thread (current-thread)))) exn:thread?)
(error-test '(call-in-nested-thread (lambda () ((error-escape-handler)))) exn:thread?)
(error-test '(call-in-nested-thread (lambda () (raise 5))) (lambda (x) (= x 5)))

(define c1 (make-custodian))
(define c2 (make-custodian))
(define c3 (make-custodian))
(define output-stream null)
(define (output v)
  (set! output-stream 
	(append output-stream (list v))))
(define (test-stream v)
  (test v 'output-stream output-stream))

(define (chain c)
  (set! output-stream null)
  
  (output 'os)
  (with-handlers ([void (lambda (x) x)])
    (call-in-nested-thread
     (lambda ()
       (output 'ms)
       (begin0
	(dynamic-wind
	 (lambda () (output 'mpre))
	 (lambda ()
	   (let ([t1 (current-thread)])
	     (call-in-nested-thread
	      (lambda ()
		(output 'is)
		(with-handlers ([void (lambda (x) 
					(if (exn:misc:user-break? x)
					    (output 'ibreak)
					    (output 'iother))
					(raise x))])
		  (if (procedure? c)
		      (c t1)
		      (custodian-shutdown-all c)))
		(output 'ie)
		'inner-result)
	      c2)))
	 (lambda () (output 'mpost)))
	(output 'me)))
     c1)))

(test 'inner-result chain c3)
(test-stream '(os ms mpre is ie mpost me))

(test #t exn:thread? (chain c1))
(test-stream '(os ms mpre is ibreak))

(parameterize ([break-enabled #f])
  (test #t exn:thread? (chain c1))
  (test-stream '(os ms mpre is ie)))

(test #t exn:thread? (chain c2))
(test-stream '(os ms mpre is mpost))

(test #t exn:thread? (chain (lambda (t1) (kill-thread (current-thread)))))
(test-stream '(os ms mpre is mpost))

(test #t exn:application? (chain 'wrong))
(test-stream '(os ms mpre is iother mpost))

(test #t exn:misc:user-break? (chain (let ([t (current-thread)]) (lambda (t1) (break-thread t)))))
(test-stream '(os ms mpre is ibreak mpost))

(test #t exn:thread? (chain (lambda (t1) (kill-thread t1))))
(test-stream '(os ms mpre is ibreak))

(parameterize ([break-enabled #f])
  (test #t exn:thread? (let ([t (current-thread)])
			 (chain (lambda (t1)
				  (custodian-shutdown-all c1)
				  (test #t thread-running? (current-thread))
				  (test #t thread-running? t)
				  (test #f thread-running? t1)))))
  (test-stream '(os ms mpre is ie)))

(error-test '(let/cc k (call-in-nested-thread (lambda () (k)))) exn:application:continuation?)
(error-test '(let/ec k (call-in-nested-thread (lambda () (k)))) exn:application:continuation?)
(error-test '((call-in-nested-thread (lambda () (let/cc k k)))) exn:application:continuation?)
(error-test '((call-in-nested-thread (lambda () (let/ec k k)))) exn:application:continuation?)

(error-test '(call-in-nested-thread 5))
(error-test '(call-in-nested-thread (lambda (x) 10)))
(error-test '(call-in-nested-thread (lambda () 10) 5))

(arity-test call-in-nested-thread 1 2)

(report-errs)
