;;; repl-test.ss

(require-library "function.ss")
(require-library "file.ss")

(load "drscheme-test-util.ss")

(printf "Starting REPL tests~n")

(letrec* ([drscheme-frame (wait-for-drscheme-frame)]
	  [user-directory ((in-parameterization (ivar (ivar drscheme-frame interactions-edit) user-param) current-directory))]
	  [test-file-nums ; list of test file numbers in repl-test subdirectory
	  '(1 2 3 4 5 6 7 8 11 12 13 14 15 16 17 18 19 24)]
	  [dir (build-path (current-load-relative-directory) "repl-tests")]
	  [test-files (map 
		       (lambda (n)
			 (build-path dir  (string-append (number->string n) ".")))
		       test-file-nums)]
	  [interactions-edit (ivar drscheme-frame interactions-edit)]
	  [interactions-canvas (ivar drscheme-frame interactions-canvas)]
	  [definitions-edit (ivar drscheme-frame definitions-edit)]
	  [definitions-canvas (ivar drscheme-frame definitions-canvas)]
	  [execute-button (ivar drscheme-frame execute-button)]
	  [get-answer
	   (lambda (filename)
	     (call-with-input-file filename
	       (lambda (port)
		 (apply string
			(let loop ()
			  (let ([c (read-char port)])
			    (if (eof-object? c)
				null
				(cons c (loop)))))))))]
	  [insert-file
	   (lambda (filename)
	     (call-with-input-file filename
	       (lambda (port)
		 (let loop ()
		   (let ([c (read-char port)])
		     (unless (eof-object? c)
			     (if (char=? c #\newline)
				 (mred:test:keystroke #\return)
				 (mred:test:keystroke c))
			     (loop)))))))]
	  [wait-for-execute 
	   (lambda () (wait-for-button execute-button))]
	  [get-int-pos (lambda () (get-text-pos interactions-edit))]
	  [do-execute 
	   (lambda ()
	     (push-button-and-wait execute-button))]
	  
	  ;; given a filename "foo", we perform two operations on the contents 
	  ;; of the file "foo.ss".  First, we insert its contents into the REPL
	  ;; directly, and second, we use the load command.  We compare the
	  ;; the results of these operations against expected results in 
	  ;; an .execute and a .load file
	  
	  [run-test
	   (lambda (escape)
	     (lambda (file)
	       (let ([answer-load (get-answer (string-append file "load"))]
		     [answer-execute (get-answer (string-append file "execute"))]
		     [test-file (string-append file "ss")])
		 
		 (mred:test:new-window definitions-canvas)
		 (mred:test:menu-select "Edit" "Select All")
		 (mred:test:menu-select "Edit" (if (eq? wx:platform 'macintosh)
						   "Clear"
						   "Delete"))
		 (do-execute)
		 (mred:test:new-window definitions-canvas)
		 
		 ; load contents of test-file into the REPL, recording
		 ; the start and end positions of the text
		 
		 (let ([execute-text-start (get-int-pos)])
		   (insert-file test-file)
		   (do-execute)
		   (let ([execute-text-end (get-int-pos)])
		     (mred:test:new-window interactions-canvas)
		     
		     ; stuff the load command into the REPL 
		     
		     (for-each mred:test:keystroke
			       (string->list (format "(load ~s)" (find-relative-path user-directory test-file))))
		     
		     ; record current text position, then stuff a CR into the REPL
		     
		     (let ([load-text-start (+ 1 (send interactions-edit last-position))])
		       
		       (mred:test:keystroke #\return)
		       
		       (wait-for-execute)
		       
		       (let* ([load-text-end (get-int-pos)]
			      [received-execute
			       (send interactions-edit get-text 
				     execute-text-start execute-text-end)]
			      [received-load 
			       (send interactions-edit get-text 
				     load-text-start load-text-end)])
			 
			 (unless (string=? received-execute answer-execute)
			   (printf "FAILED execute test for ~a~nexpected: ~s~n     got: ~s~n"
				   file received-execute answer-execute))
			 
			 (unless (string=? received-load answer-load)
			   (printf "FAILED load test for ~a~n" file))
			 
			 (when (repl-in-edit-sequence?)
			   (printf "FAILED: repl in edit-sequence")
			   (escape)))))))))])
	 
   	 (set-language-level! "Quasi-R4RS" drscheme-frame)
	 	 
	 (let/ec escape (for-each (run-test escape) test-files))
	 
	 (printf "Finished REPL tests~n"))
