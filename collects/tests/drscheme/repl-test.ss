
(module repl-test mzscheme
  (require "drscheme-test-util.ss"
           (lib "class.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix fw: (lib "framework.ss" "framework")))
  
  (provide run-test)
  
  (define-struct loc (line col offset))
  ;; loc = (make-loc number number number)
  ;; numbers in loc structs start at zero.
  
  (define-struct test (program               ;; : (union string 'abc-text-box 'fraction-sum)
                       execute-answer        ;; : string
                       load-answer           ;; : (union #f string)
                       
                       has-backtrace?        ;; : boolean
                       ;; indicates if the backtrace icon should appear for this test
                       ;; only applies to the debug tests
                       
                       source-location ;; : (union 'definitions
                       ;;          'interactions
                       ;;          (cons loc loc))
		       ;; if cons, the car and cdr are the start and end positions resp.
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
                       ;; if this field is not #f, the execute-answer and load-answer fields
                       ;; are expected to be `format'able strings with one ~a in them.
                       
                       docs-icon?                  ;; : boolean 
                       ;; true if this should have a docs icon in front of the response.
                       
                       breaking-test?))            ;; : boolean
  
  (define test-data
    (list
     
     ;; basic tests
     (make-test "("
                "~aread: expected a ')'"
                "~aread: expected a ')'"
                #f
                (cons (make-loc 0 0 0) (make-loc 0 1 1))
		'read
                #f
                #f)
     (make-test "."
                "~aread: illegal use of \".\""
                "~aread: illegal use of \".\""
                #f
		(cons (make-loc 0 0 0) (make-loc 0 1 1))
		'read
                #f
                #f)
     (make-test "(lambda ())"
                "~alambda: bad syntax in: (lambda ())"
                "~alambda: bad syntax in: (lambda ())"
                #f
                (cons (make-loc 0 0 0) (make-loc 0 11 11))
		'expand
                #t
                #f)
     (make-test "xx"
                "reference to undefined identifier: xx"
                "reference to undefined identifier: xx"
                #t
                (cons (make-loc 0 0 0) (make-loc 0 2 2))
		#f
                #f
                #f)
     (make-test "(raise 1)"
                "uncaught exception: 1"
                "uncaught exception: 1"
                #f
                'interactions
                #f
                #f
                #f)
     (make-test "(raise #f)"
                "uncaught exception: #f"
                "uncaught exception: #f"
                #f
		'interactions
                #f
                #f
                #f)
     (make-test "(values 1 2)"
                "1\n2"
                "1\n2"
                #f
		'interactions
                #f
                #f
                #f)
     (make-test "(list 1 2)"
                "(1 2)"
                "(1 2)"
                #f
		'interactions
                #f
                #f
                #f)
     
     ;; top-level semantics test
     (make-test "(define (f) (+ 1 1)) (define + -) (f)"
                "0"
                "0"
                #f
		'interactions
                #f
                #f
                #f)
     
     ;; leading comment test
     (make-test "#!\n1"
                "1"
                "1"
                #f
		'interactions
                #f
                #f
                #f)
     
     ;; eval tests
     (make-test "    (eval '(values 1 2))"
                "1\n2"
                "1\n2"
		#f
		'interactions
                #f
                #f
                #f)
     (make-test "    (eval '(list 1 2))"
                "(1 2)"
                "(1 2)"
                #f
		'interactions
                #f
                #f
                #f)
     (make-test "    (eval '(lambda ()))"
                "lambda: bad syntax in: (lambda ())"
                "lambda: bad syntax in: (lambda ())"
                2
		(cons (make-loc 0 4 4) (make-loc 0 23 23))
                #f
                #t
                #f)
     (make-test "    (eval 'x)"
                "reference to undefined identifier: x"
                "reference to undefined identifier: x"
                2
		(cons (make-loc 0 4 4) (make-loc 0 13 13))
                #f
                #f
                #f)
     
     (make-test "(eval (box 1))"
		"#&1"
		"#&1"
                #f
		'interactions
		#f
                #f
		#f)
     
     (make-test "(eval '(box 1))"
		"#&1"
		"#&1"
                #f
		'interactions
		#f
                #f
		#f)
     
     ; printer setup test
     (make-test "(car (void))"
                "car: expects argument of type <pair>; given #<void>"
                "car: expects argument of type <pair>; given #<void>"
                2
		(cons (make-loc 0 0 0) (make-loc 0 12 12))
                #f
                #f
                #f)
     
     ;; error in the middle
     (make-test "1 2 ( 3 4"
                "1\n2\n~aread: expected a ')'"
                "~aread: expected a ')'"
		#f
		(cons (make-loc 0 4 4) (make-loc 0 5 5))
                'read
                #f
                #f)
     (make-test "1 2 . 3 4"
                "1\n2\n~aread: illegal use of \".\""
                "~aread: illegal use of \".\""
		#f
		(cons (make-loc 0 4 4) (make-loc 0 5 5))
                'read
                #f
                #f)
     (make-test "1 2 (lambda ()) 3 4"
                "1\n2\n~alambda: bad syntax in: (lambda ())"
                "~alambda: bad syntax in: (lambda ())"
		#f
		(cons (make-loc 0 4 4) (make-loc 0 15 15))
                'expand
                #t
                #f)
     (make-test "1 2 x 3 4"
                "1\n2\nreference to undefined identifier: x"
                "reference to undefined identifier: x"
		#t
		(cons (make-loc 0 4 4) (make-loc 0 5 5))
                #f
                #f
                #f)
     (make-test "1 2 (raise 1) 3 4"
                "1\n2\nuncaught exception: 1"
                "uncaught exception: 1"
		#f
		'interactions
                #f
                #f
                #f)
     (make-test "1 2 (raise #f) 3 4"
                "1\n2\nuncaught exception: #f"
                "uncaught exception: #f"
		#f
		'interactions
                #f
                #f
                #f)
     
     ;; new namespace test
     (make-test "(current-namespace (make-namespace))\nif"
                "~aif: bad syntax in: if"
                "~aif: bad syntax in: if"
                #f
		(cons (make-loc 1 0 37) (make-loc 1 2 39))
                'expand
                #t
                #f)
     
     (make-test "(current-namespace (make-namespace 'empty))\nif"
                "~acompile: bad syntax; reference to top-level identifiers is not allowed, because no #%top syntax transformer is bound in: if"
                #f
                #f
		(cons (make-loc 1 0 44) (make-loc 1 0 46))
                'expand
                #t
                #f)
     
     ;; macro tests
     (make-test "(define-syntax (c stx) (syntax-case stx () [(_ p q r) (syntax (+ p q r))]))"
		""
		""
                #f
		'interactions
		#f
                #f
		#f)
     
     
     
     ;; error escape handler test
     (make-test
      "(let ([old (error-escape-handler)])\n(+ (let/ec k\n(dynamic-wind\n(lambda () (error-escape-handler (lambda () (k 5))))\n(lambda () (car))\n(lambda () (error-escape-handler old))))\n10))"
      "car: expects 1 argument, given 0\n15"
      "car: expects 1 argument, given 0\n15"
      #t
      'definitions
      #f
      #f
      #f)
     
     ; non-strings snip test
     (make-test 'abc-text-box
                "{embedded \"abc\"}"
                "{embedded \"abc\"}"
                #f
		'interactions
                #f
                #f
                #f)
     
     ; fraction snip test
     (make-test 'fraction-sum
                "{number 5/6 \"5/6\"}"
                "{number 5/6 \"5/6\"}"
                #f
                'interactions
                #f
                #f
                #f)
     
     
     
     (make-test
      "(define s (make-semaphore 0))\n(queue-callback\n(lambda ()\n(dynamic-wind\nvoid\n(lambda () (car))\n(lambda () (semaphore-post s)))))\n(yield s)"
      "car: expects 1 argument, given 0"
      "car: expects 1 argument, given 0"
      2
      (cons (make-loc 0 99 99) (make-loc 0 104 104))
      #f
      #f
      #f)
     
     ;; breaking tests
     (make-test "(semaphore-wait (make-semaphore 0))"
                "user break"
                "user break"
                2
		(cons (make-loc 0 0 0) (make-loc 0 35 35))
                #f
                #f
                #t)
     
     (make-test "(let l()(l))"
                "user break"
                "user break"
                2
		(cons (make-loc 0 8 8) (make-loc 0 11 11))
                #f
                #f
                #t)
     
     ;; continuation tests
     (make-test "(define k (call/cc (lambda (x) x)))\n(k 17)\nk"
		"17" "17"
                #f
		'interactions
		#f
                #f
		#f)
     (make-test "(define v (vector (call/cc (lambda (x) x))))\n((vector-ref v 0) 2)\nv"
		"#1(2)" "#1(2)"
                #f
		'interactions
		#f
                #f
                #f)
     (make-test "(define v (vector (eval '(call/cc (lambda (x) x)))))\n((vector-ref v 0) 2)\nv"
		"#1(2)" "#1(2)"
                #f
		'interactions
		#f
                #f
                #f)
     
     ;; thread tests
     (make-test "(begin (thread (lambda () x)) (sleep 1/10))"
                "reference to undefined identifier: x"
                "reference to undefined identifier: x"
                #t
		(cons (make-loc 0 26 26) (make-loc 0 27 27))
                #f
                #f
                #f)))
  
  (define docs-image-string "{image #f}")
  (define backtrace-image-string "{image #f}")
  (define file-image-string "{image #f}")
  
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
  
  (define tmp-load-short-filename "repl-test-tmp.ss")
  (define tmp-load-filename
    (normal-case-path
     (normalize-path 
      (build-path (collection-path "tests" "drscheme")
                  tmp-load-short-filename))))
  
  (define short-tmp-load-filename
    (let-values ([(base name dir?) (split-path tmp-load-filename)])
      name))
  
  
  ;; setup-fraction-sum-interactions : -> void 
  ;; clears the definitions window, and executes `1/2' to
  ;; get a fraction snip in the interactions window.
  ;; Then, copies that and uses it to construct the sum
  ;; of the 1/2 image and 1/3.
  (define (setup-fraction-sum-interactions)
    (clear-definitions drscheme-frame)
    (type-in-definitions drscheme-frame "1/2")
    (do-execute drscheme-frame)
    (let* ([start (send interactions-text paragraph-start-position 2)]
           
           ;; since the fraction is supposed to be one char wide, we just
           ;; select one char, so that, if the regular number prints out,
           ;; this test will fail.
           [end (+ start 1)])
      (send interactions-text set-position start end)
      (fw:test:menu-select "Edit" "Copy"))
    (clear-definitions drscheme-frame)
    (type-in-definitions drscheme-frame "(+ ")
    (fw:test:menu-select "Edit" "Paste")
    (type-in-definitions drscheme-frame " 1/3)"))
  
  
  ; given a filename "foo", we perform two operations on the contents 
  ; of the file "foo.ss".  First, we insert its contents into the REPL
  ; directly, and second, we use the load command.  We compare the
  ; the results of these operations against expected results.
  (define run-single-test
    (lambda (execute-text-start escape raw?)
      (lambda (in-vector)
        (let* ([program (test-program in-vector)]
               [execute-answer (test-execute-answer in-vector)]
	       [source-location (test-source-location in-vector)]
	       [source-location-in-message (test-source-location-in-message in-vector)]
               [start-line (and source-location-in-message
                                (number->string (+ 1 (loc-line (car source-location)))))]
               [start-col (and source-location-in-message
                               (number->string (+ 1 (loc-col (car source-location)))))]
	       [formatted-execute-answer
		(let* ([w/backtrace
			(if (and (test-has-backtrace? in-vector)
				 (not raw?))
			    (string-append backtrace-image-string " ")
			    "")]
		       [w/docs-icon
			(if (and #f (test-docs-icon? in-vector))
			    (string-append docs-image-string " " w/backtrace)
			    w/backtrace)]
		       [final
                        ;; if there is a source-location for the message, put the
                        ;; icons just before it. Otherwise, but the icons at
                        ;; the beginning of the entire string.
			(if source-location-in-message
			    (format execute-answer w/docs-icon)
			    (string-append w/docs-icon execute-answer))])
		  final)]
               [load-answer (test-load-answer in-vector)]
               [formatted-load-answer
		(and load-answer
                     (let* ([w/backtrace
                             (if raw?
                                 load-answer
                                 (if (or (eq? source-location 'definitions)
                                         (pair? source-location))
                                     (string-append backtrace-image-string " " load-answer)
                                     load-answer))]
                            [w/file-icon
                             (if raw?
                                 (if source-location-in-message
                                     (string-append file-image-string " " w/backtrace)
                                     w/backtrace)
                                 (if (or (eq? source-location 'definitions)
                                         (pair? source-location))
                                     (string-append file-image-string " " w/backtrace)
                                     w/backtrace))]
                            [w/docs-icon
                             (if (and #f (test-docs-icon? in-vector))
                                 (string-append docs-image-string " " w/file-icon)
                                 w/file-icon)])
                       (if source-location-in-message
                           (format w/docs-icon 
                                   (format "~a:~a:~a: "
                                           short-tmp-load-filename
                                           start-line
                                           start-col))
                           w/docs-icon)))]
               [breaking-test? (test-breaking-test? in-vector)])
          
          (clear-definitions drscheme-frame)
          ; load contents of test-file into the REPL, recording
          ; the start and end positions of the text
          
          (cond
            [(string? program)
             (insert-string program)]
            [(eq? program 'fraction-sum)
             (setup-fraction-sum-interactions)]
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
				  (= (cadr error-range) (loc-offset start))
				  (= (caddr error-range) (loc-offset finish)))
		       (printf "FAILED execute test for ~s\n  error-range is ~s\n  expected ~a ~a\n"
			       program
			       error-range
			       (loc-offset start)
			       (loc-offset finish)))))]))
            
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
                      (string->list (format "(load ~s)" tmp-load-short-filename)))
            
            ; record current text position, then stuff a CR into the REPL
            (let ([load-text-start (+ 1 (send interactions-text last-position))])
              
              (fw:test:keystroke #\return)
              
              (when breaking-test?
                (fw:test:button-push (send drscheme-frame get-break-button)))
              (wait-for-execute)
              
              (when load-answer
                (let* ([load-text-end (- (get-int-pos) 1)] ;; subtract one to eliminate newline
                       [received-load 
                        (fetch-output drscheme-frame load-text-start load-text-end)])
                  
                  ; check load text 
                  (unless (string=? received-load formatted-load-answer)
                    (printf "FAILED load test for ~s\n  expected: ~s\n       got: ~s\n"
                            program formatted-load-answer received-load)))))
            
            ; check for edit-sequence
            (when (repl-in-edit-sequence?)
              (printf "FAILED: repl in edit-sequence")
              (escape)))))))
  
  (define (run-test-in-language-level raw?)
    (let ([level (list "PLT" "Graphical (MrEd)")]
          [drs (wait-for-drscheme-frame)])
      (printf "running ~s tests\n" level)

      (if raw?
          (begin
            (set-language-level! level #f)
            (fw:test:set-check-box! "Debugging" #f)
            (let ([f (get-top-level-focus-window)])
              (fw:test:button-push "OK")
              (wait-for-new-frame f)))
          (set-language-level! level))

      (fw:test:new-window definitions-canvas)
      (clear-definitions drscheme-frame)
      (do-execute drscheme-frame)
      (let/ec escape 
        (for-each (run-single-test (get-int-pos) escape raw?) test-data))))
  
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
    
    ;(set-language-level! (list "PLT" "Graphical (MrEd)")) (kill-tests)
    
    (run-test-in-language-level #f)
    (run-test-in-language-level #t)))
