;;; repl-test.ss

(require-library "function.ss")
(require-library "file.ss")

(load "drscheme-test-util.ss")

(letrec* ([test-data
	   (list
	    (vector "("
		    "0.0-0.0: missing close paren"
		    #t
		    "missing close paren"
		    (vector 0 1))
	    (vector "."
		    "0.0-0.0: can't use `.' outside list"
		    #t
		    "can't use `.' outside list"
		    (vector 0 1))
	    (vector "(begin)"
		    "0.0-0.6: Malformed begin"
		    #t
		    "Malformed begin"
		    (vector 0 7))
	    (vector "x"
		    "0.0-0.0: reference to undefined identifier: x"
		    #t
		    "reference to undefined identifier: x"
		    (vector 0 1))
	    (vector "(raise 1)"
		    "uncaught exception: 1"
		    #f
		    "uncaught exception: 1"
		    #f)
	    (vector "(raise #f)"
		    "uncaught exception: #f"
		    #f
		    "uncaught exception: #f"
		    #f)
	    (vector "(values 1 2)"
		    (format "1~n2")
		    #f
		    (format "1~n2")
		    #f)
	    (vector "(list 1 2)"
		    "(1 2)"
		    #f
		    "(1 2)"
		    #f)
	    (vector "    (eval '(values 1 2))"
		    (format "1~n2")
		    #f
		    (format "1~n2")
		    #f)
	    (vector "    (eval '(list 1 2))"
		    "(1 2)"
		    #f
		    "(1 2)"
		    #f)
	    (vector "    (eval '(begin))"
		    "0.4-0.18: Malformed begin"
		    #t
		    "Malformed begin"
		    (vector 4 19))
	    (vector "    (eval 'x)"
		    "0.4-0.12: reference to undefined identifier: x"
		    #t
		    "reference to undefined identifier: x"
		    (vector 4 13))
	    (vector "1 2 ( 3 4"
		    "0.4-0.4: missing close paren"
		    #t
		    (format "1~n2~nmissing close paren")
		    (vector 4 5))
	    (vector "1 2 . 3 4"
		    "0.4-0.4: can't use `.' outside list"
		    #t
		    (format "1~n2~ncan't use `.' outside list")
		    (vector 4 5))
	    (vector "1 2 x 3 4"
		    "0.4-0.4: reference to undefined identifier: x"
		    #t
		    (format "1~n2~nreference to undefined identifier: x")
		    (vector 4 5))
	    (vector "1 2 (raise 1) 3 4"
		    "uncaught exception: 1"
		    #f
		    (format "1~n2~nuncaught exception: 1")
		    'unlocated-error)
	    (vector "1 2 (raise #f) 3 4"
		    "uncaught exception: #f"
		    #f
		    (format "1~n2~nuncaught exception: #f")
		    'unlocated-error)
	    (vector (format "(let ([old (error-escape-handler)])~n(+ (let/ec k~n(dynamic-wind~n(lambda () (error-escape-handler (lambda () (k 5))))~n(lambda () (car))~n(lambda () (error-escape-handler old))))~n10))")
		    (format "4.12-4.16: car: expects 1 argument, given 0~n15")
		    #t
		    (format "car: expects 1 argument, given 0~n15")
		    (vector 138 143))
	    (vector "(define-macro m (lambda (x) (+ x 1))) (m 2)"
		    "3"
		    #f
		    "3"
		    #f)
	    (vector "(define-macro m (lambda (x) `(+ ,x 1))) (m (+ 1 2))"
		    "4"
		    #f
		    "4"
		    #f)
	    (vector "(define-macro m (car))"
		    "0.16-0.20: car: expects 1 argument, given 0"
		    #t
		    "car: expects 1 argument, given 0"
		    (vector 16 21))
	    (vector
	     (format "(define-macro m (lambda () (car)))~n(m)")
	     "1.1-1.3: car: expects 1 argument, given 0"
	     #t
	     "car: expects 1 argument, given 0"
	     (vector 35 38))
	    (vector
	     (format "(define-macro m (lambda (x) `(+ ,x 1)))~n(m #t)")
	     "1.1-1.6: +: expects type <number> as 1st argument, given: #t; other arguments were: 1"
	     #t
	     "+: expects type <number> as 1st argument, given: #t; other arguments were: 1"
	     (vector 40 46))
	    (vector
	     "(define-macro m 1)"
	     "0.0-1.17: Expander is not a procedure"
	     #t
	     "Expander is not a procedure"
	     (vector 0 18))
	    (vector
	     "(define-macro m (values (let ([x (lambda (x) x)]) x) (let ([y (lambda (x) x)]) y)))"
	     "0.16-0.81: context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
	     #t
	     "context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
	     (vector 16 82))
	    (vector
	     (format "(define-macro m (lambda (x) (values x x)))~n(m 1)")
	     "1.1-1.5: context expected 1 value, received 2 values: 1 1"
	     #t
	     "context expected 1 value, received 2 values: 1 1"
	     (vector 43 48)))]

	  [drscheme-frame (wait-for-drscheme-frame)]
	  [user-directory
	   (normalize-path
	    ((in-parameterization (ivar (ivar drscheme-frame interactions-edit) user-param)
				  current-directory)))]

	  [interactions-edit (ivar drscheme-frame interactions-edit)]
	  [interactions-canvas (ivar drscheme-frame interactions-canvas)]
	  [definitions-edit (ivar drscheme-frame definitions-edit)]
	  [definitions-canvas (ivar drscheme-frame definitions-canvas)]
	  [execute-button (ivar drscheme-frame execute-button)]
	  [insert-string
	   (lambda (string)
	     (let loop ([n 0])
	       (unless (= n (string-length string))
		 (let ([c (string-ref string n)])
		   (if (char=? c #\newline)
		       (mred:test:keystroke #\return)
		       (mred:test:keystroke c)))
		 (loop (+ n 1)))))]
	  [wait-for-execute (lambda () (wait-for-button execute-button))]
	  [get-int-pos (lambda () (get-start-of-last-line interactions-edit))]


	  [do-execute 
	   (lambda ()
	     (push-button-and-wait execute-button))]

	  [tmp-load-filename
	   (normalize-path (build-path (current-load-relative-directory) "repl-test-tmp.ss"))]

	  ;; given a filename "foo", we perform two operations on the contents 
	  ;; of the file "foo.ss".  First, we insert its contents into the REPL
	  ;; directly, and second, we use the load command.  We compare the
	  ;; the results of these operations against expected results in 
	  ;; an .execute and a .load file
	  
	  [run-test
	   (lambda (execute-text-start escape)
	     (lambda (in-vector)
	       (let* ([program (vector-ref in-vector 0)]
		      [pre-answer-load (vector-ref in-vector 1)]
		      [prepend-filename? (vector-ref in-vector 2)]
		      [answer-load (if prepend-filename?
				       (string-append tmp-load-filename ": " pre-answer-load)
				       pre-answer-load)]
		      [answer-execute (vector-ref in-vector 3)]
		      [execute-location (vector-ref in-vector 4)])
		 
		 (mred:test:new-window definitions-canvas)
		 (mred:test:menu-select "Edit" "Select All")
		 (mred:test:menu-select "Edit" (if (eq? wx:platform 'macintosh)
						   "Clear"
						   "Delete"))
		 
		 ; load contents of test-file into the REPL, recording
		 ; the start and end positions of the text
		 
		 (insert-string program)
		 (do-execute)
		 (let* ([execute-text-end (- (get-int-pos) 1)] ;; subtract one to skip last newline
			[received-execute
			 (send interactions-edit get-text 
			       execute-text-start execute-text-end)])

		   ;; check result of execute test 

		   ; check focus and selection
		   (cond
		    [(eq? execute-location 'unlocated-error) 
		     (unless (send interactions-canvas is-focus-on?)
		       (printf "FAILED execute test for ~s~n  expected interactions to have the focus~n"
			       program))]
		    [(and execute-location (send definitions-canvas is-focus-on?))
		     (unless (and (= (send definitions-edit get-start-position) (vector-ref execute-location 0))
				  (= (send definitions-edit get-end-position) (vector-ref execute-location 1)))
		       (printf "FAILED execute test for ~s~n  start/end position are ~a ~a~n  expected ~a ~a~n"
			       program
			       (send definitions-edit get-start-position)
			       (send definitions-edit get-end-position)
			       (vector-ref execute-location 0)
			       (vector-ref execute-location 1)))]
		    [execute-location
		     (printf "FAILED execute test for ~s~n  expected definitions canvas to have the focus~n"
			     program)]
		    [(not (send interactions-canvas is-focus-on?))
		     (printf "FAILED execute test for ~s~n  expected interactions to have the focus~n"
			     program)]
		    [else (void)])
		   ; check text 
		     (unless (string=? received-execute answer-execute)
		       (printf "FAILED execute test for ~s~n  expected: ~s~n       got: ~s~n"
			       program answer-execute received-execute))

		   (mred:test:new-window interactions-canvas)
		   
		   ; construct the load file

		   (call-with-output-file tmp-load-filename
		     (lambda (port) (display program port))
		     'truncate)

		   ; stuff the load command into the REPL 
		   
		   (for-each mred:test:keystroke
			     (string->list (format "(load ~s)"
						   (find-relative-path
						    user-directory
						    tmp-load-filename))))
		   
		   ; record current text position, then stuff a CR into the REPL
		   
		   (let ([load-text-start (+ 1 (send interactions-edit last-position))])
		     
		     (mred:test:keystroke #\return)
		     
		     (wait-for-execute)
		     
		     (let* ([load-text-end (- (get-int-pos) 1)] ;; subtract one to eliminate newline
			    [received-load 
			     (send interactions-edit get-text 
				   load-text-start load-text-end)])
		       
		       (unless (string=? received-load answer-load)
			 (printf "FAILED load test for ~s~n  expected: ~s~n       got: ~s~n"
				 program answer-load received-load))
		       
		       (when (repl-in-edit-sequence?)
			 (printf "FAILED: repl in edit-sequence")
			 (escape))))))))]

	  [run-test-in-language-level
	   (lambda (level)
	     (set-language-level! "MrEd" drscheme-frame)
	     (mred:test:new-window definitions-canvas)
	     (mred:test:menu-select "Edit" "Select All")
	     (mred:test:menu-select "Edit" (if (eq? wx:platform 'macintosh)
					       "Clear"
					       "Delete"))
	     (do-execute)
	     (let/ec escape (for-each (run-test (get-int-pos) escape) test-data)))])

  (run-test-in-language-level "MrEd")
  (run-test-in-language-level "Quasi-R4RS"))
