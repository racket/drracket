(begin-construction-time
 `(compound-unit/sig
      (import [mred : mred-interfaces^]
	      [mzlib : mzlib:core^]
	      [framework : framework^]
	      [print-convert : mzlib:print-convert^]
	      [zodiac : zodiac:system^]
	      [export : drscheme:export^])
    (link 
     ,@(let loop ([dirs drscheme:tool-directories])
	 (cond
	   [(null? dirs) null]
	   [else
	    (let* ([dir (car dirs)]
		   [full-dir (collection-path "drscheme" "tools" dir)])
	      (if (and (directory-exists? full-dir)
		       (not (string=? "CVS" dir)))
		  `((,(string->symbol dir)
		     : () ((require-library "unit.ss" "drscheme" "tools" ,dir)
			   mred mzlib framework print-convert export zodiac))
		    .
		    ,(loop (cdr dirs)))
		  (loop (cdr dirs))))])))
    (export)))
