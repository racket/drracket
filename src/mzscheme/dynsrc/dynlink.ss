
(define argv-l (vector->list argv))

(define (flag f)
  (let loop ([l argv-l][p null])
    (if (null? l)
	#f
	(if (equal? f (car l))
	    (begin
	      (set! argv-l (append p l))
	      #t)
	    (loop (cdr l) (append p (list (car l))))))))

(define verbose? (flag "-q"))


(unit
 (define include-dir (collection-path "mzscheme" "include"))
 
 (define link-command
   (case (system-library-subpath)
     [("sparc-solaris") "ld -G"]
     [("sparc-sunos4") "ld -Bdynamic"]
     [("i386-freebsd") "ld -Bshareable"]
     [("rs6k-aix") (let ([version (read (car (process* "/usr/bin/uname" "-v")))])
		     (format "cc -bM:SRE -bI:~a/mzscheme.exp -bE:~a/ext.exp ~a" 
			     include-dir
			     include-dir
			     (if (version = 3)
				 "-e _nostart"
				 "-bnoentry")))]
     [("parisc-hpux") "ld -b"]
     [else "ld -shared"]))
 
 (define std-library (build-path (collection-path "mzscheme" "lib") (system-library-subpath) "mzdyn.o"))

 (define (link quiet? input-files output-file)
   (let* ([l (process (format "~a -o ~a ~a ~a"  link-command output-file
			      (let loop ([l input-files][s ""])
				(if (null? l)
				    s
				    (loop (cdr l) (format "~a ~a" (car l) s))))
			      std-library))]
	  [in (car l)]
	  [out (cadr l)]
	  [in-error (caddr l)]
	  [control (cadddr l)]

	  [collect-output (box "")]
	  [collect-error (box "")]
	  
	  [make-collector
	   (lambda (in dest box)
	     (thread (lambda () (let loop ()
				  (let ([t (read-line in)])
				    (unless (eof-object? t)
					    (unless quiet? (fprintf (dest) "~a~n" t))
					    (set-box! box (string-append (unbox box) t))
					    (loop)))))))]
	  [in-thread (make-collector in current-output-port collect-output)]
	  [in-error-thread (make-collector in-error current-error-port collect-error)])

     (thread-wait in-thread)
     (thread-wait in-error-thread)
     
     (unless (eq? ((cadddr l) 'status) 'done-error)
	     (error 'link
		    (if (string=? "" (unbox collect-error))
			(unbox collect-output)
			(unbox collect-error)))))))
	     
