(let* ([drscheme:tool-directories drscheme:tool-directories]
       [tool-filenames
	(let loop ([dirs drscheme:tool-directories])
	  (cond
	   [(null? dirs) null]
	   [else
	    (let* ([dir (car dirs)]
		   [full-dir (collection-path "drscheme" "tools" dir)])
	      (if (and (directory-exists? full-dir)
		       (not (string=? "CVS" dir)))
		  (cons dir (loop (cdr dirs)))
		  (loop (cdr dirs))))]))]

       [rem
	(lambda (dir)
	  (set! tool-filenames
		(let loop ([fns tool-filenames])
		  (cond
		   [(null? fns) null]
		   [else (if (string=? dir (car fns))
			     (cdr fns)
			     (cons (car fns) (loop (cdr fns))))]))))])

  ;; load them first here, so the progress bar is right.
  ;; they will be cached for the require-library/proc
  ;; in the body of the unit
  (for-each
   (lambda (dir)
     (if (file-exists? (build-path (collection-path "drscheme" "tools" dir) "unit.ss"))
         (with-handlers ([(lambda (x) #t)
                          (lambda (exn)
                            (rem dir)
                            (message-box
                             (format "Tool ~s failed to load" dir)
                             (if (exn? exn)
                                 (exn-message exn)
                                 (format "~s" exn))))])
           (require-library/proc "unit.ss" "drscheme" "tools" dir))
         (rem dir)))
   tool-filenames)

 (unit/sig ()
   (import [mred : mred^]
           [mzlib : mzlib:core^]
           [framework : framework^]
           [print-convert : mzlib:print-convert^]
           [zodiac : zodiac:system^]
           [export : drscheme:export^])

   (for-each
    (lambda (dir)
      (with-handlers ([(lambda (x) #t)
		       (lambda (exn)
			 (mred:message-box
			  (format "Tool ~s failed when executed" dir)
			  (if (exn? exn)
			      (exn-message exn)
			      (format "~s" exn))))])
	(invoke-unit/sig
	 (require-library/proc "unit.ss" "drscheme" "tools" dir)
	 (mred : mred^)
	 (mzlib : mzlib:core^)
	 (framework : framework^)
	 (print-convert : mzlib:print-convert^)
	 (export : drscheme:export^)
	 (zodiac : zodiac:system^))))
    tool-filenames)))
