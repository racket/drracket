(unit/sig drscheme:edit^
  (import [mred : mred^]
	  [aries : plt:aries^]
	  [zodiac : drscheme:zodiac^])
  
  (define edit%
    (class mred:scheme-mode-edit% args
      (public
	[get-zodiac-sexp
	 (lambda ()
	   (let* ([loc (zodiac:make-location 0 0 0 this)]
		  [port (mred:read-snips/chars-from-buffer this)]
		  [reader (zodiac:read port loc)]
		  [bodies (let read-loop ()
			    (let ([expr (reader)])
			      (if (zodiac:eof? expr)
				  null
				  (cons expr (read-loop)))))]
		  [built (if (null? bodies)
			     '(void)
			     `(begin ,@bodies))]
		  [structured (zodiac:structurize-syntax
			       built
			       (zodiac:make-zodiac 'drscheme loc loc))])
	     structured))])
      (sequence
	(apply super-init args)))))