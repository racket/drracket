
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'parameterizations)

(let ([p (open-output-file "tmp5" 'replace)])
  (display (compile '(cons 1 2)) p)
  (close-output-port p))

(define-struct tester (x))
(define a-tester (make-tester 5))

(define (check-write-string v s)
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
(define main-executor (current-will-executor))

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
		      exn:read:unsupported?
		      #f)
		(list read-accept-graph
		      (list #t #f)
		      '(read (open-input-string "#0=(1 . #0#)"))
		      exn:read:unsupported?
		      #f)
		(list read-accept-compiled
		      (list #t #f)
		      '(let ([p (open-input-file "tmp5")])
			 (dynamic-wind
			  void
			  (lambda () (read p))
			  (lambda () (close-input-port p))))
		      exn:read:unsupported?
		      #f)
		(list read-accept-type-symbol
		      (list #t #f)
		      '(read (open-input-string "#<hello>"))
		      exn:read:unsupported?
		      #f)
		(list read-accept-bar-quote
		      (list #t #f)
		      '(let ([p (open-input-string "|hello #$ there| x")])
			 (read p)
			 (read p))
		      exn:read:unsupported?
		      #f)
		(list print-graph
		      (list #t #f)
		      '(check-write-string (quote (#0=(1 2) . #0#)) "(#0=(1 2) . #0#)")
		      exn:check-string?
		      #f)
		(list print-struct
		      (list #t #f)
		      '(check-write-string a-tester "#(struct:tester 5)")
		      exn:check-string?
		      #f)
		(list print-box
		      (list #t #f)
		      '(check-write-string (box 5) "#&5")
		      exn:check-string?
		      #f)

		(list current-input-port
		      (list (make-input-port (lambda () #\x) (lambda () #t) void)
			    (make-input-port (lambda () 5) (lambda () #t) void))
		      '(read-char)
		      exn:i/o:user-port?
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

		(list debug-info-handler
		      (list (debug-info-handler)
			    (lambda () 'boo!))
		      `(with-handlers ([(lambda (x) (not (eq? (exn-debug-info x) 'boo!))) void])
			   (/ 0))
		      exn:application:math:zero?
		      (list "bad setting" one-arg-proc two-arg-proc))

		(list user-break-poll-handler
		      (list (user-break-poll-handler)
			    (lambda () (set! called-break? #t) #f))
		      `(begin
			 (set! called-break? #f)
			 ((user-break-poll-handler))
			 (if called-break?
			     (error 'break))
			 (set! called-break? #f))
		      exn:user?
		      (list "bad setting" one-arg-proc two-arg-proc))
		(list break-enabled
		      (list #t #f)
		      '(let ([cont? #f])
			 (thread-wait
			  (parameterize ([parameterization-branch-handler
					  current-parameterization])
			     (thread
			      (lambda ()
				(break-thread (current-thread))
				(sleep)
				(set! cont? #t)))))
			 (when cont?
			       (error 'break-enabled)))
		      exn:user?
		      #f)
		; exception-break-enabled: still needs test!

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
		      exn:i/o:filesystem:file?
		      (append (list 0)
			      (map
			       (lambda (t)
				 (make-bad-test t exn:i/o:filesystem:path?))
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
		      exn:misc:thread:kill?
		      (list "bad setting"))

		(list current-will-executor
		      (list main-executor (make-will-executor))
		      '(unless (eq? main-executor (current-will-executor))
			       (error 'will-exec))
		      exn:user?
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

(define p1 (make-parameterization))
(define p2 (make-parameterization p1))
(define p3 (make-parameterization p2))
(define p3.again (make-parameterization-with-sharing p3 p3 null void))

(test #t parameterization? p1)
(test #f parameterization? 'hi)
(arity-test parameterization? 1 1)

(test 'one (in-parameterization p3 test-param1))
(test 'two (in-parameterization p3 test-param2))

(define test-param3 (make-parameter 'hi))
(test 'hi (in-parameterization p3 test-param3))
((in-parameterization p3 test-param3) 'goodbye)
(test 'goodbye (in-parameterization p3.again test-param3))

(arity-test make-parameterization 0 1)
(error-test '(make-parameterization #f))

(arity-test in-parameterization 2 2)
(error-test '(in-parameterization #f current-output-port))
(error-test '(in-parameterization p1 (lambda (x) 8)))
(error-test '(in-parameterization p1 add1))

; Randomly set some
(for-each
 (lambda (d)
   (let* ([param (car d)]
	  [alt1 (caadr d)])
     (when (zero? (random 2))
	   (display "setting ") (display param) (newline)
	   (test (void) (in-parameterization p1 param) alt1))))
 params)

(test #f parameter? add1)

(for-each
 (lambda (d)
   (let* ([param (car d)]
	  [alt1 (caadr d)]
	  [bads (cadddr (cdr d))]
	  [pp1 (in-parameterization p1 param)])
     (test #t parameter? param)
     (arity-test param 0 1)
     (arity-test pp1 0 1)
     (when bads
	   (for-each
	    (lambda (bad)
	      (let-values ([(bad exn?)
			    (if (bad-test? bad)
				(values (bad-test-value bad)
					(bad-test-exn? bad))
				(values bad
					exn:application:type?))])
			  (error-test `(,param ,bad) exn?)
			  (error-test `(,pp1 ,bad) exn?)))
	    bads))))
 params)

((in-parameterization p1 error-print-width) 577)
(define main-pw (error-print-width))
(test #f = 577 main-pw)

(test #t = main-pw (blocking-thread (lambda () (error-print-width))))
(parameterize ([parameterization-branch-handler
		(lambda ()
		  (make-parameterization p1))])
    (test #t equal? '(577 578 578)
	    (blocking-thread 
	     (lambda () 
	       (list
		(begin0 
		 (error-print-width)
		 (error-print-width 578))
		(error-print-width)
		(blocking-thread ; this thread made with p1's branch handler, which is the default one
		 (lambda () (error-print-width)))))))
    (test #t = main-pw (error-print-width)))

(test #t = main-pw (error-print-width))
(test #t = main-pw (blocking-thread (lambda () (error-print-width))))

(test 577 'ize (parameterize ([error-print-width 577])
		  (error-print-width)))
(test main-pw error-print-width)

(test 577 with-new-parameterization
      (lambda ()
	(error-print-width 577)
	(error-print-width)))
(test main-pw error-print-width)

(define (make-sharing share-from)
  (make-parameterization-with-sharing 
   p1 #f 
   (list read-case-sensitive test-param2 test-param1)
   (lambda (x)
     (if (or (parameter-procedure=? x read-case-sensitive)
	     (parameter-procedure=? x test-param2))
	 share-from
	 #f))))

(define (check-sharing p-share other inh)
  (define (check-one-param param v1 v2 shared?)
    (with-parameterization 
     p-share
     (lambda ()
       (test v1 param)
       (parameterize ([param v2])
          (test v2 param)
	  (test (if shared? v2 v1) (in-parameterization other param))
	  (test v1 (in-parameterization inh param))
	  (if shared?
	      (begin
		((in-parameterization other param) v1)
		(test v1 param)
		(param v2))
	      (with-parameterization
	       other
	       (lambda ()
		 (parameterize ([param v1])
		    (test v2 (in-parameterization p-share param)))))))
       (test v1 param)
       (test v1 (in-parameterization other param))
       (with-parameterization 
	other
	(lambda ()
	  (parameterize ([param v2])
	     (test v2 param)
	     (test v1 (in-parameterization inh param))
	     (test (if shared? v2 v1) (in-parameterization p-share param)))))
       (with-parameterization 
	inh
	(lambda ()
	  (let ([o1 ((in-parameterization other param))]
		[o2 ((in-parameterization p-share param))])
	    (parameterize ([param v2])
	     (test v2 param)
	     (test o1 (in-parameterization other param))
	     (test o2 (in-parameterization p-share param))))))
       (test v1 param))))

  (check-one-param read-accept-compiled #f #t #f)
  (check-one-param read-case-sensitive #f #t #t)
  (check-one-param test-param1 'one 'uno #f)
  (check-one-param test-param2 'two 'dos #t))

((in-parameterization p1 read-accept-compiled) #f)
((in-parameterization p1 read-case-sensitive) #f)

((in-parameterization p1 test-param2) 'two)
((in-parameterization p1 test-param1) 'one)

(define ps1.a (make-sharing p3))
(define ps1.b (make-sharing p3))
(define ps2 (make-sharing ps1.a))

(check-sharing ps1.a p3 p1)
(check-sharing ps1.b p3 p1)
(check-sharing ps2 p3 p1)
(check-sharing ps2 ps1.a p1)

(test #t parameterization? (make-parameterization-with-sharing (current-parameterization) #f null void))
(test #t parameterization? (make-parameterization-with-sharing (current-parameterization) (current-parameterization) null void))

(arity-test make-parameterization-with-sharing 4 4)
(error-test '(make-parameterization-with-sharing #f #f null void))
(error-test '(make-parameterization-with-sharing (current-parameterization) 2 null void))
(error-test '(make-parameterization-with-sharing (current-parameterization) #f (list 5) void))
(error-test '(make-parameterization-with-sharing (current-parameterization) #f (list read-case-sensitive read-case-sensitive) void))
(error-test '(make-parameterization-with-sharing (current-parameterization) #f (list read-case-sensitive) (lambda () 0)))
(error-test '(make-parameterization-with-sharing (current-parameterization) #f (list read-case-sensitive) (lambda (x) 0))
	    exn:misc:parameterization?)

(arity-test with-new-parameterization 1 1)
(arity-test with-parameterization 2 2)

(arity-test parameterization-branch-handler 0 1)
(error-test '(parameterization-branch-handler 0))
(error-test '(parameterization-branch-handler (lambda (x) x)))
(error-test '(parameterize ([parameterization-branch-handler void])
			   (thread void))
	    exn:misc:parameterization?)

(test #t parameter-procedure=? read-accept-compiled read-accept-compiled)
(test #f parameter-procedure=? read-accept-compiled read-case-sensitive)
(error-test '(parameter-procedure=? read-accept-compiled 5))
(error-test '(parameter-procedure=? 5 read-accept-compiled))
(arity-test parameter-procedure=? 2 2)

; Test current-library-collection-paths?
; Test require-library-use-compiled?

; Use this with SGC to check GC behavior:
(define save-it #f)
(define (pgc-check)
  (let ([rp (current-parameterization)])
    (let loop ([n 100][p rp])
      (if (zero? n)
	  (set! save-it p)
	  (begin
	    (make-parameter n)
	    (make-parameterization)
	    (make-parameterization-with-sharing rp rp null void)
	    (loop (sub1 n) (make-parameterization-with-sharing p p null void)))))))

(report-errs)
