
(values
 (if (string=? (system-library-subpath) "rs6k-aix")
     (letrec ([pseudo-process* 
	       (lambda (c . args)
		 (if (null? args)
		     (let ([r (process* "/usr/bin/csh" "-t")])
		       (display c (cadr r))
		       (newline (cadr r))
		       r)
		     (apply pseudo-process* (string-append c " " (car args)) (cdr args))))])
	     pseudo-process*)
     process*)
 
 (lambda (start-process quiet? error)
   (let* ([l (start-process quiet?)]
	  [in (car l)]
	  [out (cadr l)]
	  [in-error (cadddr l)]
	  [control (cadddr (cdr l))]

	  [collect-output (box "")]
	  
	  [make-collector
	   (lambda (in dest box)
	     (thread (lambda () 
		       (let loop ()
			 (let ([t (read-line in)])
			   (unless (eof-object? t)
				   (unless quiet? (fprintf (dest) "~a~n" t))
				   (set-box! box (string-append (unbox box) 
								(string #\newline #\space) t))
				   (loop)))))))]
	  [in-thread (make-collector in current-output-port collect-output)]
	  [in-error-thread (make-collector in-error current-error-port collect-output)])

     (control 'wait)

     (thread-wait in-thread)
     (thread-wait in-error-thread)

     (close-input-port in)
     (close-input-port in-error)
     (close-output-port out)

     (unless (eq? (control 'status) 'done-ok)
	     (error (if quiet?
			(unbox collect-output)
			"command failed"))))))
