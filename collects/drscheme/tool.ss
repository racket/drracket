(begin-elaboration-time
 `(compound-unit/sig
      (import [mred : mred^]
	      [mzlib : mzlib:core^]
	      [print-convert : mzlib:print-convert^]
	      [zodiac : zodiac:system^]
	      [plt:parameters : plt:parameters^]
	      [export : drscheme:export^])
    
    (link 
     ,@(let loop ([dirs drscheme:tool-directories])
	 (cond
	   [(null? dirs) null]
	   [else
	    (let* ([dir (car dirs)]
		   [full-dir (build-path plt:home-directory "drscheme" "tools"
					 dir)])
	      (if (and (directory-exists? full-dir)
		       (not (string=? "RCS" dir)))
		  (let* ([unit-path (build-path full-dir "unit.ss")]
			 [link-sym (string->symbol dir)]
			 [link-clause
			  `(,link-sym : () ((reference-unit/sig ,unit-path)
					    mred mzlib print-convert export zodiac
					    plt:parameters))])
		    `(,link-clause ,@(loop (cdr dirs))))
		  (loop (cdr dirs))))])))
    (export)))
