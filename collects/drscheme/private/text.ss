
(module text mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
           "drsig.ss"
           (lib "framework.ss" "framework"))
  
  (provide text@)
  
  (define text@
    (unit/sig drscheme:text^
      (import)
      (define text<%>
        (interface (scheme:text<%>)
          printing-on
          printing-off
          is-printing?))
      
      (define text%
        (class100* scheme:text% (text<%>) args
          (private-field
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
          (sequence
            (apply super-init args)))))))
