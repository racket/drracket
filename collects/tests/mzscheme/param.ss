
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'parameters)

(let ([p (open-output-file "tmp5" 'replace)])
  (display (compile '(cons 1 2)) p)
  (close-output-port p))

(define-struct tester (x))
(define a-tester (make-tester 5))

(define (check-write-string display v s)
  (let ([p (open-output-string)])
    (display v p)
    (let ([s2 (get-output-string p)])
      (or (string=? s s2)
	  (error 'check-string "strings didn't match: ~s vs. ~s"
		 s s2)))))

(define exn:check-string? exn:user?)

(define called-break? #f)

(define erroring-set? #f)

(define erroring-port
  (make-output-port (let ([orig (current-output-port)])
		      (lambda (s) 
			(if erroring-set?
			    (begin
			      (set! erroring-set? #f)
			      (error 'output))
			    (display s orig))))
		    void))

(define erroring-eval
  (let ([orig (current-eval)])
    (lambda (x)
      (if erroring-set?
	  (begin
	    (set! erroring-set? #f)
	    (error 'eval))
	  (orig x)))))

(define blocking-thread
  (lambda (thunk)
    (let ([x #f])
      (thread-wait (thread (lambda () (set! x (thunk)))))
      x)))

(define main-cust (current-custodian))

(define zero-arg-proc (lambda () #t))
(define one-arg-proc (lambda (x) #t))
(define two-arg-proc (lambda (x y) #t))
(define three-arg-proc (lambda (x y z) #t))

(define test-param1 (make-parameter 'one))
(define test-param2 (make-parameter 
		     'two
		     ; generates type error:
		     (lambda (x) (if (symbol? x)
				     x
				     (add1 'x)))))

(test 'one test-param1)
(test 'two test-param2) 

(arity-test make-parameter 1 2)
(error-test '(make-parameter 0 zero-arg-proc))
(error-test '(make-parameter 0 two-arg-proc))

(define-struct bad-test (value exn?))

(define params (list
		(list read-case-sensitive 
		      (list #f #t) 
		      '(if (eq? (read (open-input-string "HELLO")) (quote hello))
			   (void) 
			   (error (quote hello)))
		      exn:user?
		      #f)
		(list read-square-bracket-as-paren
		      (list #t #f)
		      '(when (symbol? (read (open-input-string "[4]")))
			     (error 'read))
		      exn:user?
		      #f)
		(list read-curly-brace-as-paren
		      (list #t #f)
		      '(when (symbol? (read (open-input-string "{4}")))
			     (error 'read))
		      exn:user?
		      #f)
		(list read-accept-box
		      (list #t #f)
		      '(read (open-input-string "#&5"))
		      exn:read?
		      #f)
		(list read-accept-graph
		      (list #t #f)
		      '(read (open-input-string "#0=(1 . #0#)"))
		      exn:read?
		      #f)
		(list read-accept-compiled
		      (list #t #f)
		      '(let ([p (open-input-file "tmp5")])
			 (dynamic-wind
			  void
			  (lambda () (read p))
			  (lambda () (close-input-port p))))
		      exn:read?
		      #f)
		(list read-accept-bar-quote
		      (list #t #f)
		      '(let ([p (open-input-string "|hello #$ there| x")])
			 (read p)
			 (read p))
		      exn:read?
		      #f)
		(list print-graph
		      (list #t #f)
		      '(check-write-string display (quote (#0=(1 2) . #0#)) "(#0=(1 2) . #0#)")
		      exn:check-string?
		      #f)
		(list print-struct
		      (list #t #f)
		      '(check-write-string display a-tester "#(struct:tester 5)")
		      exn:check-string?
		      #f)
		(list print-box
		      (list #t #f)
		      '(check-write-string display (box 5) "#&5")
		      exn:check-string?
		      #f)
		(list print-vector-length
		      (list #t #f)
		      '(check-write-string write (vector 1 2 2) "#3(1 2)")
		      exn:check-string?
		      #f)

		(list current-input-port
		      (list (make-input-port (lambda () #\x) (lambda () #t) void)
			    (make-input-port (lambda () 5) (lambda () #t) void))
		      '(read-char)
		      exn:i/o:port:user?
		      '("bad string"))
		(list current-output-port
		      (list (current-output-port)
			    erroring-port)
		      '(begin 
			 (set! erroring-set? #t) 
			 (display 5) 
			 (set! erroring-set? #f))
		      exn:user?
		      '("bad string"))

#|
		; Doesn't work since error-test sets the port!
		(list current-error-port
		      (list (current-error-port)
			    erroring-port)
		      '(begin 
			 (set! erroring-set? #t) 
			 ((error-display-handler) "hello")
			 (set! erroring-set? #f))
		      exn:user?
		      "bad setting")
|#
		
		(list compile-allow-cond-fallthrough
		      (list #t #f)
		      '(cond)
		      exn:else?
		      #f)
		
		(list compile-allow-set!-undefined
		      (list #t #f)
		      '(eval `(set! ,(gensym) 9))
		      exn:variable?
		      #f)

		(list current-namespace
		      (list (make-namespace)
			    (make-namespace 'hash-percent-syntax))
		      '(begin 0)
		      exn:variable?
		      '("bad setting"))

		(list error-print-width
		      (list 10 50)
		      '(when (< 10 (error-print-width)) (error 'print-width))
		      exn:user?
		      '("bad setting"))
		(list error-value->string-handler
		      (list (error-value->string-handler) (lambda (x w) (error 'converter)))
		      '(format "~e" 10)
		      exn:user?
		      (list "bad setting" zero-arg-proc one-arg-proc three-arg-proc))

		(list break-enabled
		      (list #t #f)
		      '(let ([cont? #f])
			 (thread-wait
			  (thread
			   (lambda ()
			     (break-thread (current-thread))
			     (sleep)
			     (set! cont? #t))))
			 (when cont?
			   (error 'break-enabled)))
		      exn:user?
		      #f)

		(list current-print
		      (list (current-print)
			    (lambda (x) (display "frog")))
		      `(let ([i (open-input-string "5")]
			     [o (open-output-string)])
			 (parameterize ([current-input-port i]
					[current-output-port o])
			     (read-eval-print-loop))
			 (let ([s (get-output-string o)])
			   (unless (char=? #\5 (string-ref s 2))
				   (error 'print))))
		      exn:user?
		      (list "bad setting" zero-arg-proc two-arg-proc))

		(list current-prompt-read
		      (list (current-prompt-read)
			    (let ([x #f]) 
			      (lambda () 
				(set! x (not x))
				(if x
				    '(quote hi)
				    eof))))
		      `(let ([i (open-input-string "5")]
			     [o (open-output-string)])
			 (parameterize ([current-input-port i]
					[current-output-port o])
			     (read-eval-print-loop))
			 (let ([s (get-output-string o)])
			   (unless (and (char=? #\> (string-ref s 0))
					(not (char=? #\h (string-ref s 0))))
				   (error 'prompt))))
		      exn:user?
		      (list "bad setting" one-arg-proc two-arg-proc))

		(list current-load
		      (list (current-load) (lambda (f) (error "This won't do it")))
		      '(load "tmp5")
		      exn:user?
		      (list "bad setting" zero-arg-proc two-arg-proc))
		(list current-eval	
		      (list (current-eval) erroring-eval)
		      '(begin 
			 (set! erroring-set? #t) 
			 (eval 5)
			 (set! erroring-set? #f))
		      exn:user?
		      (list "bad setting" zero-arg-proc two-arg-proc))

		(list current-load-relative-directory
		      (list (current-load-relative-directory) 
			    (build-path (current-load-relative-directory) 'up))
		      '(load-relative "loadable.ss")
		      exn:i/o:filesystem?
		      (append (list 0)
			      (map
			       (lambda (t)
				 (make-bad-test t exn:i/o:filesystem?))
			       (list
				"definitely a bad path"
				(string #\a #\nul #\b)
				"relative"
				(build-path 'up))))
		      equal?)

		(list global-port-print-handler
		      (list write display)
		      '(let ([s (open-output-string)])
			 (print "hi" s)
			 (unless (char=? #\" (string-ref (get-output-string s) 0))
				 (error 'global-port-print-handler)))
		      exn:user?
		      (list "bad setting" zero-arg-proc one-arg-proc three-arg-proc))

		(list current-custodian
		      (list main-cust (make-custodian))
		      '(let ([th (parameterize ([current-custodian main-cust])
				    (thread (lambda () (sleep 1))))])
			 (kill-thread th))
		      exn:misc?
		      (list "bad setting"))

		(list exit-handler
		      (list void (lambda (x) (error 'exit-handler)))
		      '(exit)
		      exn:user?
		      (list "bad setting" zero-arg-proc two-arg-proc))

		(list test-param1
		      (list 'one 'bad-one)
		      '(when (eq? (test-param1) 'bad-one)
			     (error 'bad-one))
		      exn:user?
		      #f)
		(list test-param2
		      (list 'two 'bad-two)
		      '(when (eq? (test-param2) 'bad-two)
			     (error 'bad-two))
		      exn:user?
		      '("bad string"))))

(for-each
 (lambda (d)
   (let ([param (car d)]
	 [alt1 (caadr d)]
	 [alt2 (cadadr d)]
	 [expr (caddr d)]
	 [exn? (cadddr d)])
     (parameterize ([param alt1])
	  (test (void) void (teval expr)))
     (parameterize ([param alt2])
	  (error-test expr exn?))))
 params)

(define test-param3 (make-parameter 'hi))
(test 'hi test-param3)
(test 'hi 'thread-param
      (let ([v #f])
	(thread-wait (thread
		      (lambda ()
			(set! v (test-param3)))))
	v))
(test (void) test-param3 'bye)
(test 'bye test-param3)
(test 'bye 'thread-param
      (let* ([v #f]
	     [r (make-semaphore)]
	     [s (make-semaphore)]
	     [t (thread
		 (lambda ()
		   (semaphore-post r)
		   (semaphore-wait s)
		   (set! v (test-param3))))])
	(semaphore-wait r)
	(test-param3 'bye-again)
	(semaphore-post s)
	(thread-wait t)
	v))
(test 'bye-again test-param3)

(test #f parameter? add1)

(for-each
 (lambda (d)
   (let* ([param (car d)]
	  [alt1 (caadr d)]
	  [bads (cadddr (cdr d))])
     (test #t parameter? param)
     (arity-test param 0 1)
     (when bads
	   (for-each
	    (lambda (bad)
	      (let-values ([(bad exn?)
			    (if (bad-test? bad)
				(values (bad-test-value bad)
					(bad-test-exn? bad))
				(values bad
					exn:application:type?))])
		(error-test `(,param ,bad) exn?)))
	    bads))))
 params)

(test #t parameter-procedure=? read-accept-compiled read-accept-compiled)
(test #f parameter-procedure=? read-accept-compiled read-case-sensitive)
(error-test '(parameter-procedure=? read-accept-compiled 5))
(error-test '(parameter-procedure=? 5 read-accept-compiled))
(arity-test parameter-procedure=? 2 2)
(arity-test parameter? 1 1)

; Test current-library-collection-paths?
; Test require-library-use-compiled?

(report-errs)
