>(module repl-test mzscheme
  (require "drscheme-test-util.ss"
           (lib "class.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix fw: (lib "framework.ss" "framework")))
  
  (provide run-test)
  
  (define-struct test (program               ;; : (union string 'abc-text-box)
                       execute-answer        ;; : string
                       load-answer           ;; : string

                       has-backtrace?        ;; : boolean
                       
                       source-location ;; : (union 'definitions
                                       ;;          'interactions
                                       ;;          (cons (vector number number number)
                                       ;;                (vector number number number)))
		       ;; if cons, the car and cdr are the start and end positions resp, and
		       ;;    the numbers are the line, column and offset of the source location, resp.
		       ;;    these numbers start at zero.
		       ;; if 'interactions, no source location and
		       ;;    the focus must be in the interactions window
		       ;; if 'definitions, no source location and
		       ;;     the focus must be in the definitions window
                       
                       source-location-in-message  ;; : (union #f 'read 'expand)
		       ;; 'read indicates that the error message is a read error, so
		       ;; the source location is the port info, and 'expand indicates
		       ;; that the error messsage is an expansion time error, so the
		       ;; the source location is the repl.
		       ;; #f indicates no source location error message

                       breaking-test?))            ;; : boolean
  
  (define test-data
    (list
     
     ;; basic tests
     (make-test "("
                "read: expected a ')'; started in "
                "read: expected a ')'; started in "
                #f
                (cons (vector 0 0 0) (vector 0 1 1))
		'read
                #f)
     (make-test "."
                "read: illegal use of \".\" in "
                "read: illegal use of \".\" in "
                #f
		(cons (vector 0 0 0) (vector 0 1 1))
		'read
                #f)
     (make-test "(lambda ())"
                "lambda: bad syntax in: (lambda ())"
                "lambda: bad syntax in: (lambda ())"
                #f
                (cons (vector 0 0 0) (vector 0 11 11))
		'expand
                #f)
     (make-test "xx"
                "reference to undefined identifier: xx"
                "reference to undefined identifier: xx"
                #f
                (cons (vector 0 0 0) (vector 0 2 2))
		'expand
                #f)
     (make-test "(raise 1)"
                "uncaught exception: 1"
                "uncaught exception: 1"
                #f
                'interactions
                #f
                #f)
     (make-test "(raise #f)"
                "uncaught exception: #f"
                "uncaught exception: #f"
                #f
		'interactions
                #f
                #f)
     (make-test "(values 1 2)"
                "1\n2"
                "1\n2"
                #f
		'interactions
                #f
                #f)
     (make-test "(list 1 2)"
                "(1 2)"
                "(1 2)"
                #f
		'interactions
                #f
                #f)
     
   ;; leading comment test
     (make-test "#!\n1"
                "1"
                "1"
                #f
		'interactions
                #f
                #f)
     
   ;; eval tests
     (make-test "    (eval '(values 1 2))"
                "1\n2"
                "1\n2"
		#f
		'interactions
                #f
                #f)
     (make-test "    (eval '(list 1 2))"
                "(1 2)"
                "(1 2)"
                #f
		'interactions
                #f
                #f)
     (make-test "    (eval '(lambda ()))"
                "lambda: bad syntax in: (lambda ())"
                "lambda: bad syntax in: (lambda ())"
                2
		(cons (vector 0 4 4) (vector 0 23 23))
                #f
                #f)
     (make-test "    (eval 'x)"
                "reference to undefined identifier: x"
                "reference to undefined identifier: x"
                2
		(cons (vector 0 4 4) (vector 0 13 13))
                #f
                #f)
     
     (make-test "(eval (box 1))"
		"#&1"
		"#&1"
                #f
		'interactions
		#f
		#f)
     
     (make-test "(eval '(box 1))"
		"#&1"
		"#&1"
                #f
		'interactions
		#f
		#f)
     
   ;; printer setup test
     (make-test "(car (void))"
                "car: expects argument of type <pair>; given #<void>"
                "car: expects argument of type <pair>; given #<void>"
                2
		(cons (vector 0 0 0) (vector 0 12 12))
                #f
                #f)
     
     
   ;; error in the middle
     (make-test "1 2 ( 3 4"
                "1\n2\nread: expected a ')'; started at position 5 in "
                "read: expected a ')'; started at position 5 in "
		2
		(cons (vector 0 5 4) (vector 0 5 5))
                #t
                #f)
     (make-test "1 2 . 3 4"
                "1\n2\nread: illegal use of \".\" at position 5 in "
                "read: illegal use of \".\" at position 5 in "
		2
		(cons (vector 0 4 4) (vector 0 5 5))
                #t
                #f)
     (make-test "1 2 x 3 4"
                "1\n2\nreference to undefined identifier: x"
                "reference to undefined identifier: x"
		2
		(cons (vector 0 4 4) (vector 0 5 5))
                #f
                #f)
     (make-test "1 2 (raise 1) 3 4"
                "1\n2\nuncaught exception: 1"
                "uncaught exception: 1"
		#f
		'interactions
                #f
                #f)
     (make-test "1 2 (raise #f) 3 4"
                "1\n2\nuncaught exception: #f"
                "uncaught exception: #f"
		#f
		'interactions
                #f
                #f)
     
   ;; new namespace test
     (make-test "(current-namespace (make-namespace))\nif"
                "compile: illegal use of a syntactic form name in: if"
                "compile: illegal use of a syntactic form name in: if"
                
                2
		(cons (vector 0 37 37) (vector 0 39 39))
                #f
                #f)

     (make-test "(current-namespace (make-namespace 'empty))\nif"
                "compile: illegal use of a syntactic form name in: if"
                "compile: illegal use of a syntactic form name in: if"
                2
		(cons (vector 0 37 37) (vector 0 39 39))
                #f
                #f)
     
   ;; error escape handler test
     (make-test
      "(let ([old (error-escape-handler)])\n(+ (let/ec k\n(dynamic-wind\n(lambda () (error-escape-handler (lambda () (k 5))))\n(lambda () (car))\n(lambda () (error-escape-handler old))))\n10))"
      "car: expects 1 argument, given 0\n15"
      "car: expects 1 argument, given 0\n15"
      2
      'definitions
      #f
      #f)
     
   ;; non-strings snip test
     (make-test 'abc-text-box
                "non-string-snip"
                "non-string-snip"
                #f
		'interactions
                #f
                #f)
     
     (make-test
      "(define s (make-semaphore 0))\n(queue-callback\n(lambda ()\n(dynamic-wind\nvoid\n(lambda () (car))\n(lambda () (semaphore-post s)))))\n(yield s)"
      "car: expects 1 argument, given 0"
      "car: expects 1 argument, given 0"
      2
      (cons (vector 0 99 99) (vector 0 104 104))
      #f
      #f)
     
   ;; breaking tests
     (make-test "(semaphore-wait (make-semaphore 0))"
                "user break"
                "user break"
                2
		(cons (vector 0 0 0) (vector 0 35 35))
                #f
                #t)
     
     (make-test "(let l()(l))"
                "user break"
                "user break"
                2
		(cons (vector 0 8 8) (vector 0 11 11))
                #f
                #t)
     
     ;; continuation tests
     (make-test "(define k (call/cc (lambda (x) x)))\n(k 17)\nk"
		"17" "17"
                #f
		'interactions
		#f
		#f)
     (make-test "(define v (vector (call/cc (lambda (x) x))))\n((vector-ref v 0) 2)\nv"
		"#1(2)" "#1(2)"
                #f
		'interactions
		#f #f)
     (make-test "(define v (vector (eval '(call/cc (lambda (x) x)))))\n((vector-ref v 0) 2)\nv"
		"#1(2)" "#1(2)"
                #f
		'interactions
		#f #f)
     
     ;; thread tests
     (make-test "(begin (thread (lambda () x)) (sleep 1/10))"
                "reference to undefined identifier: x"
                "reference to undefined identifier: x"
                #t
		(cons (vector 0 26 26) (vector 0 27 27))
                #f
                #f)))
  
  (define drscheme-frame (wait-for-drscheme-frame))
  
  (define interactions-text (send drscheme-frame get-interactions-text))
  (define interactions-canvas (send drscheme-frame get-interactions-canvas))
  (define definitions-text (send drscheme-frame get-definitions-text))
  (define definitions-canvas (send drscheme-frame get-definitions-canvas))
  (define execute-button (send drscheme-frame get-execute-button))
  (define (insert-string string)
    (let loop ([n 0])
      (unless (= n (string-length string))
	(let ([c (string-ref string n)])
	  (if (char=? c #\newline)
	      (fw:test:keystroke #\return)
	      (fw:test:keystroke c)))
	(loop (+ n 1)))))
  
  (define wait-for-execute (lambda () (wait-for-button execute-button)))
  (define get-int-pos (lambda () (get-text-pos interactions-text)))
  
  (define tmp-load-filename
    (normalize-path (build-path (collection-path "tests" "drscheme") "repl-test-tmp.ss")))
  
;; given a filename "foo", we perform two operations on the contents 
;; of the file "foo.ss".  First, we insert its contents into the REPL
;; directly, and second, we use the load command.  We compare the
;; the results of these operations against expected results.
  
  (define run-single-test
    (lambda (execute-text-start escape raw?)
      (lambda (in-vector)
        (let* ([program (test-program in-vector)]
               [execute-answer (test-execute-answer in-vector)]
	       [source-location (test-source-location in-vector)]
	       [source-location-in-message (test-source-location-in-message in-vector)]
	       [error-start (and source-location-in-message
				 (number->string (+ 1 (vector-ref (car source-location) 2))))]
	       [formatted-execute-answer
		(let* ([pre
			(if source-location-in-message
			    (string-append execute-answer
					   (case source-location-in-message
					     [(read) "USERPORT"]
					     [(expand) "#<struct:object:definitions-text%>"])
					   ":"
					   error-start)
			    execute-answer)]
		       [w/backtrace
			(if (and (test-has-backtrace? in-vector)
				 (not raw?))
			    (string-append "{image} " pre)
			    pre)]
		       [w/docs-icon
			(if (eq? source-location-in-message 'expand)
			    (string-append "{image} " w/backtrace)
			    w/backtrace)])
		  w/docs-icon)]
               [load-answer (test-load-answer in-vector)]
               [formatted-load-answer
		(let* ([w/backtrace/file-icon
			(if raw?
			    (string-append "{image} " load-answer)
			    (string-append "{image} {image} " load-answer))]
		       [w/docs-icon
			(if (eq? source-location-in-message 'expand)
			    (string-append "{image} " w/backtrace/file-icon)
			    w/backtrace/file-icon)])
		  (if source-location-in-message
		      (string-append w/docs-icon tmp-load-filename ":" error-start)
		      w/docs-icon))]
               [breaking-test? (test-breaking-test? in-vector)])
          
          (clear-definitions drscheme-frame)
          ; load contents of test-file into the REPL, recording
          ; the start and end positions of the text
          
          (cond
            [(string? program)
             (insert-string program)]
            [(eq? program 'abc-text-box)
             (fw:test:menu-select "Edit" "Insert Text Box")
             (fw:test:keystroke #\a)
             (fw:test:keystroke #\b)
             (fw:test:keystroke #\c)])
	  
          (do-execute drscheme-frame #f)
          (when breaking-test?
            (fw:test:button-push (send drscheme-frame get-break-button)))
	  (wait-for-execute)
          
          (let* ([execute-text-end (- (get-int-pos) 1)] ;; subtract one to skip last newline
                 [received-execute
                  (fetch-output drscheme-frame execute-text-start execute-text-end)])
            
            ; check focus and selection for execute test
            (unless raw?
              (cond
                [(eq? source-location 'definitions)
                 (unless (send definitions-canvas has-focus?)
                   (printf "FAILED execute test for ~s\n  expected definitions to have the focus\n"
                           program))]
                [(eq? source-location 'interactions)
                 (unless (send interactions-canvas has-focus?)
                   (printf "FAILED execute test for ~s\n  expected interactions to have the focus\n"
                           program))]
                [(send definitions-canvas has-focus?)
		 (let ([start (car source-location)]
		       [finish (cdr source-location)])
		   (let ([error-range (send interactions-text get-error-range)])
		     (unless (and error-range
				  (= (car error-range) (vector-ref start 0))
				  (= (cdr error-range) (vector-ref finish 0)))
		       (printf "FAILED execute test for ~s\n  error-range is ~s\n  expected ~a ~a\n"
			       program
			       error-range
			       (vector-ref start 0)
			       (vector-ref finish 0)))))]))
            
            ; check text for execute test
	    (unless (string=? received-execute formatted-execute-answer)
	      (printf "FAILED execute test for ~s (~a)\n  expected: ~s\n       got: ~s\n"
		      program
		      raw?
		      formatted-execute-answer received-execute))
            
            (fw:test:new-window interactions-canvas)
            
            ; save the file so that load is in sync
            (fw:test:menu-select "File" "Save Definitions")
            
            ; make sure that a prompt is available at end of the REPL
            (unless (and (char=? #\>
                                 (send interactions-text get-character
                                       (- (send interactions-text last-position) 2)))
                         (char=? #\space
                                 (send interactions-text get-character
                                       (- (send interactions-text last-position) 1))))
              (fw:test:keystroke #\return))
            
            ; stuff the load command into the REPL 
            (for-each fw:test:keystroke
                      (string->list (format "(load ~s)" tmp-load-filename)))
            
            ; record current text position, then stuff a CR into the REPL
            (let ([load-text-start (+ 1 (send interactions-text last-position))])
              
              (fw:test:keystroke #\return)
              
              (when breaking-test?
                (fw:test:button-push (send drscheme-frame get-break-button)))
              (wait-for-execute)
              
              (let* ([load-text-end (- (get-int-pos) 1)] ;; subtract one to eliminate newline
                     [received-load 
                      (fetch-output drscheme-frame load-text-start load-text-end)])
                
                ; check load text 
		(unless (string=? received-load formatted-load-answer)
		  (printf "FAILED load test for ~s\n  expected: ~s\n       got: ~s\n"
			  program formatted-load-answer received-load))
                
                ; check for edit-sequence
                (when (repl-in-edit-sequence?)
                  (printf "FAILED: repl in edit-sequence")
                  (escape)))))))))
  
  (define (run-test-in-language-level raw?)
    (let ([level (if raw?
                     (list "Full" "Graphical without debugging (MrEd)")
                     (list "Full" "Graphical (MrEd)"))]
          [drs (wait-for-drscheme-frame)])
      (printf "running ~a tests\n" level)
      (set-language-level! level)
      (fw:test:new-window definitions-canvas)
      (clear-definitions drscheme-frame)
      (do-execute drscheme-frame)
      (let/ec escape (for-each (run-single-test (get-int-pos) escape raw?) test-data))))
  
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
  
  (define (run-test)
    (when (file-exists? tmp-load-filename)
      (delete-file tmp-load-filename))
    (save-drscheme-window-as tmp-load-filename)
    
    ;(set-language-level! (list "Full" "Graphical (MrEd)")) (kill-tests)
    
    (run-test-in-language-level #t)
    (run-test-in-language-level #f)))
