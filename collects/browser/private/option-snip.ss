(module option-snip mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "string.ss"))

  (provide option-snip%)

  (define inset 2)
  (define arrow-sep 5)
  (define arrow-height 5)

  (define arrow (list (make-object point% 0 0)
		      (make-object point% arrow-height arrow-height)
		      (make-object point% (* 2 arrow-height) 0)))

  (define arrow-cursor (make-object cursor% 'arrow))

  (define option-snip%
    (class snip%
      (inherit get-admin set-snipclass set-count get-style get-flags set-flags)
      (init-field [options null])
      (define w #f)
      (define h #f)
      (define current-option #f)

      (define/public (add-option o)
	(set! options (append options (list o)))
	(set! w #f)
	(set! h #f)
	(let ([a (get-admin)])
	  (when a
	    (send a resized this #t))))

      (override*
	[get-extent  ; called by an editor to get the snip's size
	 (lambda (dc x y wbox hbox descentbox spacebox lspacebox rspacebox)
	   (unless w
	     (let ([font (send (get-style) get-font)])
	       (let ([w+hs
		      (map (lambda (o)
			     (let-values ([(tw th td ta) (send dc get-text-extent o font)])
			       (cons tw th)))
			   options)])
		 (if (null? w+hs)
		     (begin
		       (set! w 10)
		       (set! h 10))
		     (begin
		       (set! w (+ (* 2 inset) arrow-sep 2 (* 2 arrow-height) (apply max (map car w+hs))))
		       (set! h (+ (* 2 inset) 1 (apply max arrow-height (map cdr w+hs)))))))))
	   (when hbox
	     (set-box! hbox h))
	   (when wbox
	     (set-box! wbox w))
	   (when descentbox
	     (set-box! descentbox 0))
	   (when spacebox
	     (set-box! spacebox 0))
	   (when rspacebox
	     (set-box! rspacebox 0))
	   (when lspacebox
	     (set-box! lspacebox 0)))]
	[draw  ; called by an editor to draw the snip
	 (lambda (dc x y . other)
	   (unless w
	     (get-extent dc x y #f #f #f #f #f #f))
	   (send dc draw-rectangle x y (sub1 w) (sub1 h))
	   (send dc draw-line (+ x 1) (+ y h -1) (+ x w -1) (+ y h -1))
	   (send dc draw-line (+ x w -1) (+ y 1) (+ x w -1) (+ y h -1))
	   (let ([pen (send dc get-pen)]
		 [brush (send dc get-brush)])
	     (send dc set-brush (send the-brush-list find-or-create-brush (send pen get-color) 'solid))
	     (send dc draw-polygon arrow 
		   (+ x (- w 2 inset (* 2 arrow-height)))
		   (+ y (/ (- h arrow-height) 2)))
	     (send dc set-brush brush))
	   (unless (null? options)
	     (send dc draw-text (or current-option (car options)) (+ x inset) (+ y inset))))]
	[copy
	 (lambda ()
	   (make-object option-snip% options))]
	[size-cache-invalid
	 (lambda () (set! w #f) (set! h #f))]
	[on-event (lambda (dc x y editorx editory event)
		    (when (send event button-down?)
		      (let ([popup (make-object popup-menu%)])
			(for-each (lambda (o)
				    (make-object menu-item% o popup
						 (lambda (i e)
						   (set! current-option o)
						   (let ([a (get-admin)])
						     (when a
						       (send a needs-update this 0 0 w h))))))
				  options)
			(let ([a (get-admin)])
			  (when a
			    (send a popup-menu popup this 0 0))))))]
	[adjust-cursor (lambda (dc x y editorx editory event)
			 arrow-cursor)])
      (super-instantiate ())
      (set-flags (cons 'handles-events (get-flags)))
      (set-count 1))))