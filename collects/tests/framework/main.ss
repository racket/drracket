(require-library "launchers.ss" "launcher")
(require-library "cores.ss")
(require-library "cmdlines.ss")
(require-library "macro.ss")
(require-library "function.ss")

(unless (file-exists? (build-path (current-load-relative-directory) "receive-sexps-port.ss"))
  (call-with-output-file (build-path (current-load-relative-directory) "receive-sexps-port.ss")
    (lambda (port)
      (write 6012 port))))

(define-signature TestSuite^
  ((struct eof-result ())
   load-framework-automatically
   shutdown-listener shutdown-mred mred-running?
   send-sexp-to-mred queue-sexp-to-mred
   test
   wait-for-frame

   ;; sexp -> void
   ;; grabs the frontmost window, executes the sexp and waits for a new frontmost window
   wait-for-new-frame

   wait-for))

(define-signature internal-TestSuite^
  ((open TestSuite^)
   test-name
   failed-tests))

(define-signature Engine^
  (only-these-tests
   section-name
   section-jump))

(require-library "guis.ss" "tests" "utils")

(define TestSuite
  (unit/sig internal-TestSuite^
    (import (program)
	    Engine^
	    launcher-maker^
	    mzlib:pretty-print^
	    mzlib:function^)

    (define test-name "<<setup>>")
    (define failed-tests null)

    (define-struct eof-result ())

    (define load-framework-automatically? #t)

    (define listener
      (let loop ()
	(let ([port (load-relative "receive-sexps-port.ss")])
	  (with-handlers ([(lambda (x) #t)
			   (lambda (x)
			     (let ([next (+ port 1)])
			       (call-with-output-file (build-path (current-load-relative-directory)
								  "receive-sexps-port.ss")
				 (lambda (p)
				   (write next p))
				 'truncate)
			       (printf "  tcp-listen failed for port ~a, attempting ~a~n"
				       port next)
			       (loop)))])
	    (tcp-listen port)))))

    (define in-port #f)
    (define out-port #f)

    (define restart-mred
      (lambda ()
	(shutdown-mred)
	(let-values ([(base _1 _2) (split-path program)])
	  ((case (system-type)
	     [(macos) system*]
	     [else (lambda (x) (thread (lambda () (system* x))))])
	   (mred-program-launcher-path "Framework Test Engine")))
	(let-values ([(in out) (tcp-accept listener)])
	  (set! in-port in)
	  (set! out-port out))
	(when load-framework-automatically?
	  (queue-sexp-to-mred
	   `(begin
	      (require-library "framework.ss" "framework")
	      (require-library "gui.ss" "tests" "utils")
	      (test:run-interval 0))))))

    (define load-framework-automatically
      (case-lambda
       [(new-load-framework-automatically?)
	(unless (eq? (not (not new-load-framework-automatically?))
		     load-framework-automatically?)
	  (set! load-framework-automatically? (not (not new-load-framework-automatically?)))
	  (shutdown-mred))]
       [() load-framework-automatically?]))

    (define shutdown-listener
      (lambda ()
	(shutdown-mred)
	(tcp-close listener)))

    (define shutdown-mred
      (lambda ()
	(when (and in-port
		   out-port)
	  (close-output-port out-port)
	  (close-input-port in-port)
	  (set! in-port #f)
	  (set! in-port #f))))

    (define mred-running?
      (lambda ()
	(if (char-ready? in-port)
	    (not (eof-object? (peek-char in-port)))
	    #t)))

    (define queue-sexp-to-mred
      (lambda (sexp)
	(send-sexp-to-mred
	 `(let ([thunk (lambda () ,sexp)]
		[sema (make-semaphore 0)])
	    (queue-callback (lambda ()
			      (thunk)
			      (semaphore-post sema)))
	    (semaphore-wait sema)))))

    (define send-sexp-to-mred
      (lambda (sexp)
	(let ([show-text 
	       (lambda (sexp)
		 
		 (parameterize ([pretty-print-print-line
				 (let ([prompt "  "]
				       [old-liner (pretty-print-print-line)])
				   (lambda (ln port ol cols)
				     (let ([ov (old-liner ln port ol cols)])
				       (if ln 
					   (begin (display prompt port)
						  (+ (string-length prompt) ov))
					   ov))))])
		   (pretty-print sexp)
		   (newline)))])
	  (unless (and in-port
		       out-port
		       (or (not (char-ready? in-port))
			   (not (eof-object? (peek-char in-port)))))
	    (restart-mred))
	  (printf "  ~a // ~a: sending to mred:~n" section-name test-name)
	  (show-text sexp)
	  (write sexp out-port)
	  (newline out-port)
	  (let ([answer
		 (with-handlers ([(lambda (x) #t)
				  (lambda (x) (list 'cant-read
						    (string-append
						     (exn-message x)
						     "; rest of string: "
						     (apply
						      string
						      (let loop ()
							(if (char-ready? in-port)
							    (cons (read-char in-port)
								  (loop))
							    null))))))])
		   (read in-port))])
	    (unless (or (eof-object? answer)
			(and (list? answer)
			     (= 2 (length answer))))
	      (error 'send-sexp-to-mred "unpected result from mred: ~s~n" answer))

	    (if (eof-object? answer)
		(raise (make-eof-result))
		(case (car answer)
		  [(error)
		   (error 'send-sexp-to-mred "mred raised \"~a\"" (second answer))]
		  [(cant-read) (error 'mred/cant-parse (second answer))]
		  [(normal) 
		   (printf "  ~a // ~a: received from mred:~n" section-name test-name)
		   (show-text (second answer))
		   (eval (second answer))]))))))


    (define test
      (case-lambda
       [(in-test-name passed? sexp/proc) (test in-test-name passed? sexp/proc 'section)]
       [(in-test-name passed? sexp/proc jump)
	(fluid-let ([test-name in-test-name])
	  (when (or (not only-these-tests)
		    (memq test-name only-these-tests))
	    (let ([failed
		   (with-handlers ([(lambda (x) #t)
				    (lambda (x)
				      (if (exn? x)
					  (exn-message x)
					  x))])
		     (let ([result
			    (if (procedure? sexp/proc)
				(sexp/proc)
				(begin0 (send-sexp-to-mred sexp/proc)
					(send-sexp-to-mred ''check-for-errors)))])

		       (not (passed? result))))])
	      (when failed
		(printf "FAILED ~a: ~a~n" failed test-name)
		(set! failed-tests (cons (cons section-name test-name) failed-tests))
		(case jump
		  [(section) (section-jump)]
		  [(continue) (void)]
		  [else (jump)])))))]))

  (define (wait-for/wrapper wrapper sexp)
    (let ([timeout 10]
	  [pause-time 1/2])
      (send-sexp-to-mred
       (wrapper
	`(let ([test (lambda () ,sexp)])
	   (let loop ([n ,(/ timeout pause-time)])
	     (if (zero? n)
		 (error 'wait-for
			,(format "after ~a seconds, ~s didn't come true" timeout sexp))
		 (unless (test)
		   (sleep ,pause-time)
		   (loop (- n 1))))))))))

  (define (wait-for sexp) (wait-for/wrapper (lambda (x) x) sexp))

  (define (wait-for-new-frame sexp)
    (wait-for/wrapper
     (lambda (w)
       `(let ([frame (get-top-level-focus-window)])
	  ,sexp
	  ,w))
     `(not (eq? frame (get-top-level-focus-window)))))

  (define (wait-for-frame name)
    (wait-for `(let ([win (get-top-level-focus-window)])
		 (and win
		      (string=? (send win get-label) ,name)))))))

(define Engine
  (unit/sig Engine^
    (import (argv)
	    internal-TestSuite^
	    mzlib:command-line^
	    mzlib:function^
	    mzlib:file^
	    mzlib:string^
	    mzlib:pretty-print^)

    (define section-jump void)
    (define section-name "<<setup>>")
    (define only-these-tests #f)

    (define preferences-file (build-path (find-system-path 'pref-dir)
					 (case (system-type)
					   [(macos) "MrEd Preferences"]
					   [(windows) "mred.pre"]
					   [(unix) ".mred.prefs"])))
    (define old-preferences-file (let-values ([(base name _2) (split-path preferences-file)])
				   (build-path base (string-append name ".save"))))
    

    (with-handlers ([(lambda (x) #f)
		     (lambda (x) (display (exn-message x)) (newline))])
      (let* ([all-files (map symbol->string (load-relative "README"))]
	     [all? #f]
	     [files-to-process null]
	     [command-line-flags
	      `((once-each
		 [("-a" "--all")
		  ,(lambda (flag)
		     (set! all? #t))
		  ("Run all of the tests")])
		(multi
		 [("-o" "--only")
		  ,(lambda (flag _only-these-tests)
		     (set! only-these-tests (cons (string->symbol _only-these-tests)
						  (or only-these-tests null))))
		  ("Only run test named <test-name>" "test-name")]))])
	
	(let* ([saved-command-line-file (build-path (collection-path "tests" "framework") "saved-command-line.ss")]
	       [parsed-argv (if (equal? argv (vector))
				(if (file-exists? saved-command-line-file)
				    (begin
				      (let ([result (call-with-input-file saved-command-line-file read)])
					(printf "reusing command-line arguments: ~s~n" result)
					result))
				    (vector))
				argv)])
	  (parse-command-line "framework-test" parsed-argv command-line-flags
			      (lambda (collected . files)
				(set! files-to-process (if (or all? (null? files)) all-files files)))
			      `("Names of the tests; defaults to all tests"))
	  (call-with-output-file saved-command-line-file
	    (lambda (port)
	      (write parsed-argv port))
	    'truncate))

	
	(when (file-exists? preferences-file)
	  (printf "  saving preferences file ~s to ~s~n" preferences-file old-preferences-file)
	  (if (file-exists? old-preferences-file)
              (printf "  backup preferences file exists, using that one~n")
              (begin (copy-file preferences-file old-preferences-file)
                     (printf "  saved preferences file~n"))))
    
	(for-each (lambda (x)
		    (when (member x all-files)
		      (shutdown-mred)
		      (let/ec k
			(fluid-let ([section-name x]
				    [section-jump k])
			  (with-handlers ([(lambda (x) #t)
					   (lambda (exn)
					     (printf "~a~n" (if (exn? exn) (exn-message exn) exn)))])
			    (printf "beginning ~a test suite~n" x)

			    (invoke-unit/sig
			     (eval
			      `(unit/sig ()
				   (import TestSuite^
					   mzlib:function^
					   mzlib:file^
					   mzlib:string^
					   mzlib:pretty-print^)
				   (include ,x)))
			     TestSuite^
			     mzlib:function^
			     mzlib:file^
			     mzlib:string^
			     mzlib:pretty-print^)
			    (printf "PASSED ~a test suite~n" x))))))
		  files-to-process)))

    (printf "  restoring preferences file ~s to ~s~n" old-preferences-file preferences-file)
    (when (file-exists? preferences-file)
      (unless (file-exists? old-preferences-file)
	(error 'framework-test "lost preferences file backup!"))
      (delete-file preferences-file)
      (copy-file old-preferences-file preferences-file)
      (delete-file old-preferences-file))
    (printf "  restored preferences file~n")

    (shutdown-listener)

    (unless (null? failed-tests)
      (printf "FAILED tests:~n")
      (for-each (lambda (failed-test)
		  (printf "  ~a // ~a~n" (car failed-test) (cdr failed-test)))
		failed-tests))))

(invoke-unit/sig
 (compound-unit/sig
   (import (P : (program))
	   (A : (argv)))
   (link
    [L : launcher-maker^ ((require-library "launcherr.ss" "launcher"))]
    [C : mzlib:core^ ((require-library "corer.ss"))]
    [M : mzlib:command-line^ ((require-library "cmdliner.ss"))]
    [T : internal-TestSuite^ (TestSuite P E L (C pretty-print) (C function))]
    [E : Engine^ (Engine A T M (C function) (C file) (C string) (C pretty-print))])
   (export))
 (program)
 (argv))
