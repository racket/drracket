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
		  (loop (cdr dirs))))]))])

  ;; load them first here, so the progress bar is right
  ;; they will be cached for the require-library/proc
  ;; in the body of the unit
  (for-each
   (lambda (dir)
     (require-library/proc "unit.ss" "drscheme" "tools" dir))
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
      (invoke-unit/sig
       (require-library/proc "unit.ss" "drscheme" "tools" dir)
       (mred : mred^)
       (mzlib : mzlib:core^)
       (framework : framework^)
       (print-convert : mzlib:print-convert^)
       (export : drscheme:export^)
       (zodiac : zodiac:system^)))
    tool-filenames)))
