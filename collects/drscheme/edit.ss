
(unit/sig drscheme:text^
  (import [mzlib:date : mzlib:date^]
	  [fw : framework^]
	  [zodiac : zodiac:system^])
  
  (define text<%>
    (interface (fw:scheme:text<%>)
      printing-on
      printing-off
      is-printing?))

  (define text%
    (class* fw:scheme:text% (text<%>) args
      (private
	[printing? #f])
      (public
	[is-printing?
	 (lambda ()
	   printing?)]
	[printing-on
	 (lambda ()
	   (set! printing? #t))]
	[printing-off
	 (lambda ()
	   (set! printing? #f))])
;      (rename [super-on-paint on-paint])
;      (inherit get-filename)
;      (override
;	[on-paint
;	 (lambda (before? dc left top right bottom dx dy draw-caret)
;	   (super-on-paint before? dc left top right bottom dx dy draw-caret)
;	   (let ([str (string-append
;			(mzlib:date:date->string (seconds->date (current-seconds)))
;			" "
;			(if (string? (get-filename))
;			    (get-filename)
;			    "Untitled"))])
;	      (send dc draw-text str dx dy)))])
      (public
	[get-zodiac-sexp
	 (lambda ()
	   (let* ([loc (zodiac:make-location 0 0 0 this)]
		  [port (fw:gui-utils:read-snips/chars-from-text this)]
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