(define-struct test (program
		     r4rs-load-answer prepend-filename? r4rs-execute-answer r4rs-execute-location
		     mred-execute-answer mred-load-answer mred-read-test? breaking-test?))

(define test-data
  (list

   ;; basic tests
   (make-test "("
	      "1.1-1.2: syntax error: missing close paren"
	      #t
	      "syntax error: missing close paren"
	      (vector 0 1)
	      "read: expected a ')'; started at position 1 in "
	      "read: expected a ')'; started at position 1 in "
	      #t
	      #f)
   (make-test "."
	      "1.1-1.2: syntax error: can't use `.' outside list"
	      #t
	      "syntax error: can't use `.' outside list"
	      (vector 0 1)
	      "read: illegal use of \".\" at position 1 in "
	      "read: illegal use of \".\" at position 1 in "
	      #t
	      #f)
   (make-test "(lambda ())"
	      "1.1-1.12: lambda: malformed expression"
	      #t
	      "lambda: malformed expression"
	      (vector 0 11)
	      "lambda: bad syntax in: (lambda ())"
	      "lambda: bad syntax in: (lambda ())"
	      #f
	      #f)
   (make-test "x"
	      "1.1-1.2: reference to undefined identifier: x"
	      #t
	      "reference to undefined identifier: x"
	      (vector 0 1)
	      "reference to undefined identifier: x"
	      "reference to undefined identifier: x"
	      #f
	      #f)
   (make-test "(raise 1)"
	      "uncaught exception: 1"
	      #f
	      "uncaught exception: 1"
	      #f
	      "uncaught exception: 1"
	      "uncaught exception: 1"
	      #f
	      #f)
   (make-test "(raise #f)"
	      "uncaught exception: #f"
	      #f
	      "uncaught exception: #f"
	      #f
	      "uncaught exception: #f"
	      "uncaught exception: #f"
	      #f
	      #f)
   (make-test "(values 1 2)"
	      (format "1~n2")
	      #f
	      (format "1~n2")
	      #f
	      (format "1~n2")
	      (format "1~n2")
	      #f
	      #f)
   (make-test "(list 1 2)"
	      "(1 2)"
	      #f
	      "(1 2)"
	      #f
	      "(1 2)"
	      "(1 2)"
	      #f
	      #f)
   
   ;; eval tests
   (make-test "    (eval '(values 1 2))"
	      (format "1~n2")
	      #f
	      (format "1~n2")
	      #f
	      (format "1~n2")
	      (format "1~n2")
	      #f
	      #f)
   (make-test "    (eval '(list 1 2))"
	      "(1 2)"
	      #f
	      "(1 2)"
	      #f
	      "(1 2)"
	      "(1 2)"
	      #f
	      #f)
   (make-test "    (eval '(lambda ()))"
	      "1.5-1.24: lambda: malformed expression"
	      #t
	      "lambda: malformed expression"
	      (vector 4 23)
	      "lambda: bad syntax in: (lambda ())"
	      "lambda: bad syntax in: (lambda ())"
	      #f
	      #f)
   (make-test "    (eval 'x)"
	      "1.5-1.14: reference to undefined identifier: x"
	      #t
	      "reference to undefined identifier: x"
	      (vector 4 13)
	      "reference to undefined identifier: x"
	      "reference to undefined identifier: x"
	      #f
	      #f)
   
   ;; printer setup test
   (make-test "(car (void))"
	      "1.1-1.13: car: expects argument of type <pair>; given #<void>"
	      #t
	      "car: expects argument of type <pair>; given #<void>"
	      (vector 0 12)
	      "car: expects argument of type <pair>; given #<void>"
	      "car: expects argument of type <pair>; given #<void>"
	      #f
	      #f)
   
   
   ;; error in the middle
   (make-test "1 2 ( 3 4"
	      "1.5-1.6: syntax error: missing close paren"
	      #t
	      (format "1~n2~nsyntax error: missing close paren")
	      (vector 4 5)
	      (format "1~n2~nread: expected a ')'; started at position 5 in ")
	      (format "read: expected a ')'; started at position 5 in ")
	      #t
	      #f)
   (make-test "1 2 . 3 4"
	      "1.5-1.6: syntax error: can't use `.' outside list"
	      #t
	      (format "1~n2~nsyntax error: can't use `.' outside list")
	      (vector 4 5)
	      (format "1~n2~nread: illegal use of \".\" at position 5 in ")
	      (format "read: illegal use of \".\" at position 5 in ")
	      #t
	      #f)
   (make-test "1 2 x 3 4"
	      "1.5-1.6: reference to undefined identifier: x"
	      #t
	      (format "1~n2~nreference to undefined identifier: x")
	      (vector 4 5)
	      (format "1~n2~nreference to undefined identifier: x")
	      (format "reference to undefined identifier: x")
	      #f
	      #f)
   (make-test "1 2 (raise 1) 3 4"
	      "uncaught exception: 1"
	      #f
	      (format "1~n2~nuncaught exception: 1")
	      'unlocated-error
	      (format "1~n2~nuncaught exception: 1")
	      (format "uncaught exception: 1")
	      #f
	      #f)
   (make-test "1 2 (raise #f) 3 4"
	      "uncaught exception: #f"
	      #f
	      (format "1~n2~nuncaught exception: #f")
	      'unlocated-error
	      (format "1~n2~nuncaught exception: #f")
	      "uncaught exception: #f"
	      #f
	      #f)
   
   ;; new namespace test
   (make-test (format "(current-namespace (make-namespace))~nif")
	      "compile: illegal use of a syntactic form name in: if"
	      #f
	      "compile: illegal use of a syntactic form name in: if"
	      'unlocated-error

	      "compile: illegal use of a syntactic form name in: if"
	      "compile: illegal use of a syntactic form name in: if"
	      #f
	      #f)

   ;; error escape handler test
   (make-test (format "(let ([old (error-escape-handler)])~n(+ (let/ec k~n(dynamic-wind~n(lambda () (error-escape-handler (lambda () (k 5))))~n(lambda () (car))~n(lambda () (error-escape-handler old))))~n10))")
	      (format "5.12-5.17: car: expects 1 argument, given 0~n15")
	      #t
	      (format "car: expects 1 argument, given 0~n15")
	      'definitions

	      (format "car: expects 1 argument, given 0~n15")
	      (format "car: expects 1 argument, given 0~n15")
	      #f
	      #f)
   
   
   ;; macro tests
   (make-test "(define-macro m (lambda (x) (+ x 1))) (m 2)"
	      "3"
	      #f
	      "3"
	      #f
	      "3"
	      "3"
	      #f
	      #f)
   (make-test "(define-macro m (lambda (x) `(+ ,x 1))) (m (+ 1 2))"
	      "4"
	      #f
	      "4"
	      #f
	      "4"
	      "4"
	      #f
	      #f)
   (make-test "(define-macro m (car))"
	      "1.17-1.22: car: expects 1 argument, given 0"
	      #t
	      "car: expects 1 argument, given 0"
	      (vector 16 21)
	      "car: expects 1 argument, given 0"
	      "car: expects 1 argument, given 0"
	      #f
	      #f)
   (make-test
    (format "(define-macro m (lambda () (car)))~n(m)")
    "1.28-1.33: car: expects 1 argument, given 0"
    #t
    "car: expects 1 argument, given 0"
    (vector 27 32)
    "car: expects 1 argument, given 0"
    "car: expects 1 argument, given 0"
    #f
    #f)
   (make-test
    (format "(define-macro m (lambda (x) `(+ ,x 1)))~n(m #t)")
    "2.1-2.7: +: expects type <number> as 1st argument, given: #t; other arguments were: 1"
    #t
    "+: expects type <number> as 1st argument, given: #t; other arguments were: 1"
    (vector 40 46)
    "+: expects type <number> as 1st argument, given: #t; other arguments were: 1"
    "+: expects type <number> as 1st argument, given: #t; other arguments were: 1"
    #f
    #f)
   (make-test
    "(define-macro m 1)"
    "1.1-1.19: define-macro: expander is not a procedure"
    #t
    "define-macro: expander is not a procedure"
    (vector 0 18)
    "define-macro: not a procedure"
    "define-macro: not a procedure"
    #f
    #f)
   (make-test
    "(define-macro m (values (let ([x (lambda (x) x)]) x) (let ([y (lambda (x) x)]) y)))"
    "context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
    #f
    "context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
    #f
    "context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
    "context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
    #f
    #f)
   
   (make-test
    (format "(define-macro m (lambda (x) (values x x)))~n(m 1)")
    "context expected 1 value, received 2 values: 1 1"
    #f
    "context expected 1 value, received 2 values: 1 1"
    #f
    "context expected 1 value, received 2 values: 1 1"
    "context expected 1 value, received 2 values: 1 1"
    #f
    #f)

   (make-test
    (format "(define s (make-semaphore 0))~n(queue-callback~n(lambda ()~n(dynamic-wind~nvoid~n(lambda () (car))~n(lambda () (semaphore-post s)))))~n(yield s)")
    "6.12-6.17: car: expects 1 argument, given 0"
    #t
    "car: expects 1 argument, given 0"
    (vector 99 104)
    "car: expects 1 argument, given 0"
    "car: expects 1 argument, given 0"
    #f
    #f)
   
   ;; breaking tests
   (make-test "(semaphore-wait (make-semaphore 0))"
	      "1.1-1.36: user break"
	      #t
	      "user break"
	      (vector 0 35)

	      "user break"
	      "user break"
	      #f
	      #t)

   (make-test "(let l()(l))"
	      "1.9-1.12: user break"
	      #t
	      "user break"
	      (vector 8 11)

	      "user break"
	      "user break"
	      #f
	      #t)

   ;; continuation tests
   (make-test (format "(define k (call/cc (lambda (x) x)))~n(k 17)~nk")
	      "17" #f "17" #f
	      "17" "17" #f #f)
   (make-test (format "(define v (vector (call/cc (lambda (x) x))))~n((vector-ref v 0) 2)~nv")
	      "#1(2)" #f "#1(2)" #f
	      "#1(2)" "#1(2)" #f #f)
   (make-test (format "(define v (vector (eval '(call/cc (lambda (x) x)))))~n((vector-ref v 0) 2)~nv")
	      "#1(2)" #f "#1(2)" #f
	      "#1(2)" "#1(2)" #f #f)

   ))

(define drscheme-frame (wait-for-drscheme-frame))

(define interactions-text (ivar drscheme-frame interactions-text))
(define interactions-canvas (ivar drscheme-frame interactions-canvas))
(define definitions-text (ivar drscheme-frame definitions-text))
(define definitions-canvas (ivar drscheme-frame definitions-canvas))
(define execute-button (ivar drscheme-frame execute-button))
(define insert-string
  (lambda (string)
    (let loop ([n 0])
      (unless (= n (string-length string))
	(let ([c (string-ref string n)])
	  (if (char=? c #\newline)
	      (fw:test:keystroke #\return)
	      (fw:test:keystroke c)))
	(loop (+ n 1))))))

(define wait-for-execute (lambda () (wait-for-button execute-button)))
(define get-int-pos (lambda () (get-text-pos interactions-text)))


(define tmp-load-filename
  (normalize-path (build-path (current-load-relative-directory) "repl-test-tmp.ss")))

;; given a filename "foo", we perform two operations on the contents 
;; of the file "foo.ss".  First, we insert its contents into the REPL
;; directly, and second, we use the load command.  We compare the
;; the results of these operations against expected results.
	  
(define run-test
  (lambda (execute-text-start escape mred?)
    (lambda (in-vector)
      (let* ([program (test-program in-vector)]
	     [pre-answer-load (test-r4rs-load-answer in-vector)]
	     [prepend-filename? (test-prepend-filename? in-vector)]
	     [answer-load (if prepend-filename?
			      (string-append "." tmp-load-filename ": " pre-answer-load)
			      pre-answer-load)]
	     [answer-execute (test-r4rs-execute-answer in-vector)]
	     [execute-location (test-r4rs-execute-location in-vector)]
	     [mred-execute-answer (test-mred-execute-answer in-vector)]
	     [mred-load-answer (test-mred-load-answer in-vector)]
	     [mred-read-test? (test-mred-read-test? in-vector)]
	     [breaking-test? (test-breaking-test? in-vector)])
	
	(clear-definitions drscheme-frame)
	; load contents of test-file into the REPL, recording
	; the start and end positions of the text
	
	(insert-string program)
	(do-execute drscheme-frame (not breaking-test?))
	(when breaking-test?
	  (fw:test:button-push (ivar drscheme-frame stop-execute-button))
	  (wait-for-execute))

	(let* ([execute-text-end (- (get-int-pos) 1)] ;; subtract one to skip last newline
	       [received-execute
		(send interactions-text get-text 
		      execute-text-start execute-text-end)])
	  
	  ; check focus and selection for execute test
	  (unless mred?
	    (cond
	     [(eq? execute-location 'definitions)
	      (unless (send definitions-canvas has-focus?)
		(printf "FAILED execute test for ~s~n  expected definitions to have the focus~n"
			program))]
	      [(eq? execute-location 'unlocated-error)
	       (unless (send interactions-canvas has-focus?)
		 (printf "FAILED execute test for ~s~n  expected interactions to have the focus~n"
			 program))]
	      [(and execute-location (send definitions-canvas has-focus?))
               (let ([error-range (send interactions-text get-error-range)])
                 (unless (and error-range
                              (= (car error-range) (vector-ref execute-location 0))
                              (= (cdr error-range) (vector-ref execute-location 1)))
                   (printf "FAILED execute test for ~s~n  error-range is ~s~n  expected ~a ~a~n"
                           program
                           error-range
                           (vector-ref execute-location 0)
                           (vector-ref execute-location 1))))]
	      [execute-location
	       (printf "FAILED execute test for ~s~n  expected definitions canvas to have the focus~n"
		       program)]
	      [(not (send interactions-canvas has-focus?))
	       (printf "FAILED execute test for ~s~n  expected interactions to have the focus~n"
		       program)]
	      [else (void)]))
	  
	  ; check text for execute test
	  (let ([expected
		 (if mred?
		     (if mred-read-test?
			 (string-append mred-execute-answer "USERPORT")
			 mred-execute-answer)
		     answer-execute)])
	    (unless (string=? received-execute expected)
	      (printf "FAILED execute test for ~s~n  expected: ~s~n       got: ~s~n"
		      program expected received-execute)))
	  
	  (fw:test:new-window interactions-canvas)
	  
	  ; construct the load file
	  
	  (call-with-output-file tmp-load-filename
	    (lambda (port) (display program port))
	    'truncate)
	  
	  ; stuff the load command into the REPL 
	  
	  (for-each fw:test:keystroke
		    (string->list (format "(load ~s)" tmp-load-filename)))
	  
	  ; record current text position, then stuff a CR into the REPL
	  
	  (let ([load-text-start (+ 1 (send interactions-text last-position))])
	    
	    (fw:test:keystroke #\return)
	    
	    (when breaking-test?
	      (fw:test:button-push (ivar drscheme-frame stop-execute-button)))
	    (wait-for-execute)
	    
	    (let* ([load-text-end (- (get-int-pos) 1)] ;; subtract one to eliminate newline
		   [received-load 
		    (send interactions-text get-text 
			  load-text-start load-text-end)])
	      
	      ; check load text 
	      (let ([expected
		     (if mred?
			 (if mred-read-test?
			     (string-append mred-load-answer
					    tmp-load-filename)
			     mred-load-answer)
			 answer-load)])
		(unless (string=? received-load expected)
		  (printf "FAILED load test for ~s~n  expected: ~s~n       got: ~s~n"
			  program expected received-load)))
	      
	      ; check for edit-sequence
	      (when (repl-in-edit-sequence?)
		(printf "FAILED: repl in edit-sequence")
		(escape)))))))))

(define (run-test-in-language-level raw?)
  (let ([level (if raw? "Graphical without Debugging (MrEd)" "Graphical (MrEd)")]
	[drs (wait-for-drscheme-frame)])
    (printf "running ~a tests~n" level)
    (set-language-level! level)
    (fw:test:new-window definitions-canvas)
    (clear-definitions drscheme-frame)
    (do-execute drscheme-frame)
    (let/ec escape (for-each (run-test (get-int-pos) escape raw?) test-data))))

(define (kill-tests)
  (let ([drs (wait-for-drscheme-frame)])
    (clear-definitions drs)
    (do-execute drs)

    (fw:test:menu-select "Scheme" "Kill")

    (let ([win (wait-for-new-frame drs)])
      (fw:test:button-push "Ok")
      (let ([drs2 (wait-for-new-frame win)])
	(unless (eq? drs2 drs)
	  (error 'kill-tests "expected original drscheme frame to come back to the front"))))
	
    (type-in-definitions drs "(kill-thread (current-thread))")
    (do-execute drs #f)

    (let ([win (wait-for-new-frame drs)])
      (fw:test:button-push "Ok")
      (let ([drs2 (wait-for-new-frame win)])
	(unless (eq? drs2 drs)
	  (error 'kill-tests "expected original drscheme frame to come back to the front"))))))

(run-test-in-language-level #t)
(run-test-in-language-level #f)
(kill-tests)
