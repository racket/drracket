#!/bin/sh

string=? ; exec /home/scheme/Executables/mzscheme -x -qgr $0 $@

(define arg (if (= 0 (vector-length argv))
		"-x"
		(vector-ref argv 0)))

(define plt-collects
  (cond
   [(string=? arg "-x") ":"]
   [(string=? arg "-robby") "/home/robby/plt:"]
   [else (error 'plt-home "don't recognize ~a~n" arg)]))

(define (print-plt-collects)
  (printf "PLTCOLLECTS is now ~s~n" (getenv "PLTCOLLECTS")))

;; this takes -x, -robby etc into account
(define (extend-plt-collects path)
  (let ([new-collects
	 (if (char=? (string-ref plt-collects 0) #\:)
	     (string-append path ":")
	     (string-append path ":" plt-collects))])
    (putenv "PLTCOLLECTS" new-collects)
    (print-plt-collects)))
(define (reset-plt-collects)
  (putenv "PLTCOLLECTS" plt-collects)
  (print-plt-collects))

(define MRED (list "/home/scheme/plt/bin/mred"))

(printf "WARNING: this script will move your ~~/.mredrc to ~~/mredrc
and write into your .mredrc. It should restore things, but if tests
fail your .mredrc may by in ~~/mredrc.

If you pass an argument, that is passed on to
/home/scheme/Executables/mred. It's used for -robby or something like that

If you see lines prefixed by `>' they are from mred's stderr and
mean that test has failed (even tho the script does not stop or say FAIL)

If you see the splash screen (except during the splash screen test),
the test failed. The console window popping up occasionally is okay,
tho.

You must run this script from the directory where it is located.

")

(printf "mred is: ~s~n" MRED)
(reset-plt-collects)

(when (file-exists? "~/.mredrc")
  (printf "-- moving ~~/.mredrc to ~~/mredrc~n")
  (system "mv -f ~/.mredrc ~/mredrc"))

(define (set-mredrc s)
  (let ([p (open-output-file "~/.mredrc" 'replace)])
    (display s p)
    (close-output-port p))
  (printf "set .mredrc to:~n~a~n" s))
(define (clear-mredrc)
  (when (file-exists? "~/.mredrc")
    (delete-file "~/.mredrc"))
  (printf "cleared .mredrc~n"))

(define (move-in-mredrc)
  (printf "-- moving ~~/mredrc to ~~/.mredrc~n")
  (if (file-exists? "~/mredrc")
      (system "mv -f ~/mredrc ~/.mredrc")))

(current-exception-handler
 (let ([old (current-exception-handler)])
   (lambda (exn)
     (when (file-exists? "~/mredrc")
       (move-in-mredrc))
     (old exn))))

(user-break-poll-handler
 (let ([old (user-break-poll-handler)])
   (lambda ()
     (if (old)
	 (begin (move-in-mredrc)
		(printf "caught break ~n")
		#t)
	 #f))))

(define test-mred
  (lambda (expected . args)
    (let*-values ([(command) (append MRED args)]
		  [(stdout stdin pid stderr info)
		   (apply values
			  (apply process*
				 command))])
      (printf "running: ~s~n" command)
      (thread
       (lambda ()
	 (let loop ()
	   (let ([line (read-line stderr)])
	     (unless (eof-object? line)
	       (display "> ")
	       (display line)
	       (newline)
	       (loop))))))
      (let ([received (read-line stdout)])
	(if (equal? expected received)
	    (printf "test passed~n")
	    (printf "FAILED TEST: got: ~s wanted: ~s~n" received expected))
	(printf "killing ~a~n" pid)
	(system (format "kill ~a" pid))))))

(define (test:no-flags)
  (printf "~n- testing no flags~n")
  (set-mredrc "(fprintf mred:constants:original-output-port \"whee~n\")")
  (test-mred "whee")
  (clear-mredrc))

(define (test:-f)
  (printf"~n- testing -f flag~n")
  (test-mred "whee" "-f" "whee.ss"))

(define (test:-e)
  (printf "~n- testing -e flag~n")
  (test-mred "whee" "-e" "(printf \"whee~n\")"))

(define (test:-nu-e)
  (printf "~n- testing -nu with -e flag~n")
  (test-mred "whee" 
	     "-e"
	     "(when (defined? 'mred:frame%)
		(printf \"wh\"))"
	     "-nu"
	     "-e"
	     "(when (and (defined? 'mred:frame%)
			 (defined? 'mred:console))
		(printf \"ee~n\"))"))

(define (test:-nu-f)
  (printf "~n- testing -nu with -f flag~n")
  (test-mred "whee" 
	     "-f"
	     "wh-has-mred.ss"
	     "-nu"
	     "-f"
	     "ee-has-mred.ss"))

(define (test:.mredrc)
  (printf "~n- testing that .mredrc is loaded~n")
  (set-mredrc "(fprintf mred:constants:original-output-port \"whee~n\")")
  (test-mred "whee")
  (clear-mredrc))

(define (test:.mredrc-nu-f)
  (printf "~n- testing that .mredrc is run during -nu flag~n")
  (set-mredrc "(fprintf mred:constants:original-output-port \"ee~n\")")
  (test-mred "whee" "-f" "wh.ss" "-nu")
  (clear-mredrc))

(define (test:mred:startup-before)
  (printf "~n- testing mred:startup after -nu~n")
  (set-mredrc "(printf \"ee~n\")")
  (test-mred "whee" "-nu" "-e"
	     "(define mred:startup (lambda x (printf \"wh\")))")
  (clear-mredrc))

(define (test:-a)
  (printf "~n- testing -a flag~n")
  (extend-plt-collects (current-load-relative-directory))
  (test-mred
   "whee" "-a" "app-a" "tapp.ss" "tsig.ss")
  (reset-plt-collects))

(define (test:-A)
  (printf "~n- testing -A flag~n")
  (extend-plt-collects (current-load-relative-directory))
  (test-mred
   "whee" "-A" "app-A")
  (reset-plt-collects))

(define (test:-p)
  (printf "~n- testing -p flag~n")
  (test-mred
   "whee" 
   "-nu"
   "-p" "/home/scheme/plt/collects/icons/anchor.gif" "splash screen test" "150" "4"
   "-e" "(printf \"whee~n\")"))

(define (test:no-icons)
  (let ([icons-before/after (collection-path "icons")])
    (let-values ([(path _1 _2) (split-path icons-before/after)])
      (let ([icons-during (build-path path "renamed-icons")])
	(printf "~n- testing icon-less startup~n")
	(printf "~n  This test expects only one icons collection is available")
	(printf "~n-- moving ~a to ~a ~n" icons-before/after icons-during)
	(rename-file icons-before/after icons-during)
	(test-mred "whee" "-e" "(printf \"whee~n\")")
	(printf "~n-- moving ~a to ~a ~n" icons-during icons-before/after)
	(rename-file icons-during icons-before/after)))))

(test:no-icons)
(test:no-flags)
(test:-e)
(test:-f)
(test:-nu-e)
(test:-nu-f)
(test:.mredrc)
(test:.mredrc-nu-f)
(test:mred:startup-before)
(test:-a)
(test:-A)
(test:-p)

(move-in-mredrc)