;;; repl-test.ss

(require-library "function.ss")

(load "drscheme-test-util.ss")

(define get-slash
  (lambda ()
    (case wx:platform
      [(unix) "/"]
      [(windows) "\\"]
      [else ":"])))

(letrec* ([_ (wait-for-drscheme-frame)]
	  [test-file-nums ; list of test file numbers in repl-test subdirectory
	  '(1 2 3 4 5 6 7 8 11 12 13 14 15 16 17 18 19 24)]
	  [dir (build-path (current-load-relative-directory) "repl-tests")]
	  [test-files (map 
		       (lambda (n)
			 (string-append dir (get-slash) (number->string n) ".")) 
		       test-file-nums)]
	  [drscheme-frame (mred:test:get-active-frame)]
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
	  (rec loop
	       (lambda ()
		 (unless (send execute-button is-enabled?)
		   (sleep 1/2)
		   (loop))))]
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

		  (for-each (lambda (c) (mred:test:keystroke c))
			    (string->list (format "(load ~s)" test-file)))

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
			(printf "FAILED execute test for ~a~n" file))

		      (unless (string=? received-load answer-load)
			(printf "FAILED load test for ~a~n" file))))))))])

  (printf "Starting REPL tests~n")

  (for-each run-test test-files)

  (printf "Finished REPL tests~n"))

