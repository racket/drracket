(unit/sig ()
  (import mred^
	  framework^
	  typeset:utils-input^)

  (define (snipize obj)
    (if (is-a? obj snip%)
	obj
	(make-object string-snip% (format "~a" obj))))

  (define (snipize/copy obj)
    (if (is-a? obj snip%)
	(send obj copy)
	(make-object string-snip% (format "~a" obj))))

  (define (set-box/f! b v) (when (box? b) (set-box! b v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                     ;;;
;;;                   POSTSCRIPT                        ;;;
;;;                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define ps-figure-editor-admin%
    (class/d editor-admin% (filename editor)
      ((override get-dc
                 get-max-view
                 get-view
                 grab-caret
                 needs-update
                 refresh-delayed?
                 resized
                 scroll-to
                 update-cursor))

      (define delayed? #t)

      (define dc
        (let ([ps-setup (make-object ps-setup%)])
          (send ps-setup copy-from (current-ps-setup))
          (send ps-setup set-file filename)
          (send ps-setup set-mode 'file)
          (parameterize ([current-ps-setup ps-setup])
            (make-object post-script-dc% #f))))
      
      (define (get-dc xb yb)
        (set-box/f! xb 0)
        (set-box/f! yb 0)
        dc)

      (define (calc-view xb yb wb hb full?)
        (set-box/f! xb 0)
        (set-box/f! yb 0)
        (let-values ([(w h) (send dc get-size)])
          (set-box/f! wb w)
          (set-box/f! hb h)))
      (define (get-max-view xb yb wb hb full?)
	(calc-view xb yb wb hb full?))

      (define (get-view xb yb wb hb full?)
	(calc-view xb yb wb hb full?))

      (define (grab-caret domain)
	(void))
      (define (needs-update localx localy x y)
	(void))
      (define (refresh-delayed?)
	delayed?)
      (define (resized refresh?) 
        (when refresh?
          (let-values ([(w h) (send dc get-size)])
            (send editor refresh 0 0 w h 'no-caret))))
      
      (define (scroll-to localx localy w h refresh? bias)
        (when refresh?
          (let-values ([(w h) (send dc get-size)])
            (send editor refresh 0 0 w h 'no-caret))))
      (define (update-cursor) (void))
      
      (super-init)

      (send dc start-doc (format "Creating ~a" filename))
      (send dc start-page)

      (set! delayed? #t)
      (send editor set-admin #f)
      (send editor size-cache-invalid)
      (send editor set-admin this)

      (set! delayed? #f)
      (let-values ([(w h) (send dc get-size)])
        (send editor refresh 0 0 w h 'no-caret))
      (send dc end-page)
      (send dc end-doc)))

  (define (postscript snip filename)
    (unless (is-a? snip editor-snip%)
      (error 'postscript
             "expected first argument to be an editor-snip%, got: ~e, other args: ~e"
             snip filename))
    (unless (string? filename)
      (error 'postscript
             "expected second argument to be a string, got: ~e, other args: ~e"
             filename
             snip))
    (let* ([editor (send snip get-editor)]
           [editor-admin (send editor get-admin)])
      (make-object ps-figure-editor-admin% filename editor)
      (send editor set-admin editor-admin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                     ;;;
;;;                    ALIGNMENT                        ;;;
;;;                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (para-align alignment)
    (lambda (snip)
      (if (is-a? snip editor-snip%)
	  (let* ([new (send snip copy)]
		 [new-e (send new get-editor)])
	    (when (is-a? new-e text%)
	      (let loop ([pn (+ (send new-e last-paragraph) 1)])
		(unless (zero? pn)
		  (send new-e set-paragraph-alignment (- pn 1) alignment)
		  (loop (- pn 1)))))
	    new)
	  snip)))

  (define lr-align-center (para-align 'center))
  (define lr-align-left (para-align 'left))
  (define lr-align-right (para-align 'right))

  (define (tb-align alignment snip)
    (if (is-a? snip editor-snip%)
	(let* ([new (send snip copy)]
	       [new-e (send new get-editor)])
	  (when (is-a? new-e text%)
	    (let ([sd (make-object style-delta%)])
	      (send sd set-alignment-on alignment)
	      (send new-e change-style sd 0 (send new-e last-position))))
	  new)
	snip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                     ;;;        
;;;                     BRACKETS                        ;;;
;;;                                                     ;;;        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define bracket-snip%
    (class editor-snip% (between-snip left-margin top-margin right-margin bottom-margin)
      (inherit get-editor)
      (override
       [write
	(lambda (p)
	  (send (get-editor) write-to-file p))])
      (public
	[height #f]
	[width #f])
      (rename [super-get-extent get-extent]
	      [super-draw draw])
      (override
       [get-extent
	(lambda (dc x y w h descent space lspace rspace)
	  (for-each (lambda (x) (when (and (box? x) (> 0 (unbox x))) (set-box! x 0)))
		    (list w h descent space lspace rspace))
	  (super-get-extent dc x y w h descent space lspace rspace)

	  ;(when (box? descent) (set-box! descent (+ (unbox descent) bottom-margin)))
	  ;(when (box? space) (set-box! space (+ (unbox space) top-margin)))
	  ;(when (box? lspace) (set-box! lspace (+ (unbox lspace) left-margin)))
	  ;(when (box? rspace) (set-box! rspace (+ (unbox rspace) right-margin)))

	  (when (box? h)
	    (set! height (unbox h)))
	  (when (box? w)
	    (set! width (unbox w))))])
      
      (inherit get-style)
      (inherit set-tight-text-fit)
      (sequence 
	(let ([text (make-object text:basic%)])
	  (super-init text #f
		      left-margin top-margin right-margin bottom-margin
		      0 0 0 0)
	  (set-tight-text-fit #t)
	  (send text insert (send between-snip copy))))))

  (define double-bracket-snip%
    (class* bracket-snip% () (between-snip)
      (inherit get-style)
      (override
       [copy
	(lambda ()
	  (let ([snip (make-object double-bracket-snip% between-snip)])
	    (send snip set-style (get-style))
	    snip))])
      
      (inherit height width)
      (rename [super-draw draw])
      (override
       [draw
	(lambda (dc x y left top right bottom dx dy draw-caret)
	  (let ([vertical-line
		 (lambda (x)
		   (send dc draw-line x y x (+ y height -1)))]
		[horizontal-lines
		 (lambda (x)
		   (send dc draw-line x y (+ x 5) y)
		   (send dc draw-line x (+ y height -1) (+ x 5) (+ y height -1)))]
		[old-pen (send dc get-pen)])

	    (when (is-a? dc post-script-dc%)
	      (send dc set-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid)))

	    (horizontal-lines x)
	    (horizontal-lines (+ x width -6))
	    (vertical-line x)
	    (vertical-line (+ x width -1))
	    (vertical-line (+ x 3))
	    (vertical-line (+ x width -4))

	    (send dc set-pen old-pen))
	  (super-draw dc x y left top right bottom dx dy draw-caret))])
      (inherit set-snipclass)
      (sequence
	(super-init between-snip 6 1 6 1)
	(set-snipclass double-bracket-snipclass))))

  (define single-bracket-snip%
    (class* bracket-snip% () (between-snip)
      (inherit get-style)
      (override
       [copy
	(lambda ()
	  (let ([snip (make-object single-bracket-snip% between-snip)])
	    (send snip set-style (get-style))
	    snip))])
      
      (inherit height width)
      (rename [super-draw draw])
      (override
       [draw
	(lambda (dc x y left top right bottom dx dy draw-caret)
	  (let ([vertical-line
		 (lambda (x)
		   (send dc draw-line x y x (+ y height -1)))]
		[horizontal-lines
		 (lambda (x)
		   (send dc draw-line x y (+ x 3) y)
		   (send dc draw-line x (+ y height -1) (+ x 3) (+ y height -1)))]
		[old-pen (send dc get-pen)])

	    (when (is-a? dc post-script-dc%)
	      (send dc set-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid)))

	    (horizontal-lines (+ x 1))
	    (horizontal-lines (+ x width -5))
	    (vertical-line (+ x 1))
	    (vertical-line (+ x width -2))
	    (send dc set-pen old-pen))
	  (super-draw dc x y left top right bottom dx dy draw-caret))])
      (inherit set-snipclass)
      (sequence
	(super-init between-snip 4 1 4 1)
	(set-snipclass single-bracket-snipclass))))

  (define bracket-snipclass%
    (class snip-class% (%)
      (override
       [read
	(lambda (p)
	  (let* ([bs (make-object % (make-object snip%))]
		 [t (send bs get-editor)])
	    (send t read-from-file p)))])
      (sequence (super-init))))

  (define single-bracket-snipclass (make-object bracket-snipclass% single-bracket-snip%))
  (send single-bracket-snipclass set-version 1)
  (send single-bracket-snipclass set-classname "robby:single-bracket")
  (send (get-the-snip-class-list) add single-bracket-snipclass)

  (define double-bracket-snipclass (make-object bracket-snipclass% double-bracket-snip%))
  (send double-bracket-snipclass set-version 1)
  (send double-bracket-snipclass set-classname "robby:double-bracket")
  (send (get-the-snip-class-list) add double-bracket-snipclass)

  ;; bracket : snip -> snip
  ;; adds double square brackets around the snip
  (define (double-bracket snip)
    (make-object double-bracket-snip% (snipize snip)))

  (define (single-bracket snip)
    (make-object single-bracket-snip% (snipize snip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                     ;;;
;;;                         GREEK                       ;;;
;;;                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; greek : (union char string number) -> snip
  ;; renders the alphabetic characters in the argument into greek letters
  (define greek
    (letrec ([snipclass
	      (make-object (class snip-class% ()
			     (override
			       [read
				(lambda (stream-in)
				  (make-object greek-snip%
				    (send stream-in get-string)
				    (send stream-in get-number)))])
			     (sequence (super-init))))]
	     [greek-snip%
	      (class snip% (str size)
		(inherit get-style)
		(private
		  [font
		   (send the-font-list find-or-create-font
			 size 'symbol 'normal 'normal #f)])
		(override
		  [write
		   (lambda (stream-out)
		     (send stream-out << str)
		     (send stream-out << size))]
		  [get-extent
		   (lambda (dc x y wb hb descentb spaceb lspace rspace)
		     (let-values ([(width height descent ascent)
				   (send dc get-text-extent str font)])
		       (set-box/f! wb (max 0 width))
		       (set-box/f! hb (max 0 height))
		       (set-box/f! descentb (max 0 descent))
		       (set-box/f! spaceb (max 0 ascent))
		       (set-box/f! lspace 0)
		       (set-box/f! rspace 0)))]
		  [draw
		   (lambda (dc x y left top right bottom dx dy draw-caret)
		     (let ([old-font (send dc get-font)])
		       (send dc set-font font)
		       (send dc draw-text str x y)
		       (send dc set-font old-font)))]
		  [copy
		   (lambda ()
		     (let ([snip (make-object greek-snip% str size)])
		       (send snip set-style (get-style))
		       snip))])
		(inherit set-snipclass)
		(sequence
		  (super-init)
		  (set-snipclass snipclass)))])

      (send snipclass set-version 1)
      (send snipclass set-classname "robby:greek")
      (send (get-the-snip-class-list) add snipclass)
      (lambda (in)
	(let ([str (cond
		    [(string? in) in]
		    [(char? in) (string in)]
		    [(number? in) (string (integer->char in))])])
	  (make-object greek-snip% str (typeset-size))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                     ;;;
;;;                       DRAWINGS                      ;;;
;;;                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; drawing : ((dc -> exact-int exact-int exact-int) (dc exact-int
  ;; exact-int -> void) -> snip) get-extent determines the amount of
  ;; space the new snip needs. The six results are the width, height,
  ;; descent, ascent, lspace and rspace. (The descent and space do not
  ;; actually add space to the snip, they only helps to determine
  ;; where to lineup adjacent snips.)  draw actually draws the snip.
  (define (drawing name eextent ddraw)
    (unless (string? name)
      (error
       'draw
       "expected string as first argument, got: ~e; other args: ~e ~e"
       name eextent ddraw))
    (unless (and (procedure? eextent) (procedure? ddraw))
      (error
       'draw
       "expected procedures as second and third arguments, got: ~e ~e; first args: ~e"
       eextent ddraw name))
    (letrec ([drawing%
	      (class snip% ()
		(inherit get-style)
		(override
		 [write
		  (lambda (stream-out)
		    (send stream-out put name))]
		 [copy
		  (lambda ()
		    (let ([ans (make-object drawing%)])
		      (send ans set-style (get-style))
		      ans))]
		 [draw
		  (lambda (dc x y left top right bottom dx dy draw-caret)
		    (ddraw dc x y))]
		 [get-extent
		  (lambda (dc x y width-b height-b descent-b space-b lspace-b rspace-b)
		    (let ([old-font (send dc get-font)])
		      (send dc set-font (send (get-style) get-font))
		      (let-values ([(width height descent space lspace rspace) (eextent dc)])
			(set-box/f! width-b width)
			(set-box/f! height-b height)
			(set-box/f! descent-b descent)
			(set-box/f! space-b space)
			(set-box/f! lspace-b lspace)
			(set-box/f! rspace-b rspace))
		      (send dc set-font old-font)))])
		(inherit set-snipclass)
		(sequence
		  (super-init)
		  (set-snipclass drawing-snipclass)))])
      (send drawing-snipclass add-drawing name drawing%)
      (make-object drawing%)))

  (define drawing-snipclass
    (make-object (class/d snip-class% ()
		   ((override read)
		    (public add-drawing))

		   (define drawing-table null)

		   (define (add-drawing name class%)
		     (let ([binding (assoc name drawing-table)])
		       (if binding
			   (set-car! (cdr binding) class%)
			   (set! drawing-table (cons (list name class%) drawing-table)))))

		   (define (read stream-in)
		     (let* ([name (send stream-in get-string)]
			    [class (assoc name drawing-table)])
		       (if class
			   (make-object (cadr class))
			   (let* ([bad-bitmap (make-object bitmap% 10 10 #t)]
				  [bdc (make-object bitmap-dc% bad-bitmap)])
			     (send bdc clear)
			     (send bdc draw-rectangle 0 0 10 10)
			     (send bdc draw-line 0 0 10 10)
			     (send bdc draw-line 10 0 0 10)
			     (send bdc set-bitmap #f)
			     (make-object image-snip% bad-bitmap)))))
		   (super-init))))
  (send drawing-snipclass set-version 1)
  (send drawing-snipclass set-classname "robby:drawing")
  (send (get-the-snip-class-list) add drawing-snipclass)

  (define ellipses
    (let* ([margin 2]
	   [get-w/h/d/s/l/r
	    (lambda (dc)
	      (let ([old-font (send dc get-font)])
		(send dc set-font (send the-font-list find-or-create-font (typeset-size)
					'roman 'normal 'normal #f))
		(let-values ([(width height descent space) (send dc get-text-extent "a")])
		  (begin0 (values (+ margin (* 3 width) margin) height descent space margin margin)
			  (send dc set-font old-font)))))])
      (drawing "robby:ellipses"
	       get-w/h/d/s/l/r
	       (lambda (dc x y)
		 (let*-values ([(w h d s _1 _2) (get-w/h/d/s/l/r dc)]
			       [(yp) (+ y s (floor (+ 1/2 (/ (- h s d) 2))))]
			       [(l) (+ x margin)]
			       [(r) (+ x w (- margin))]
			       [(ellipse-size) 2/3]
			       [(draw-dot)
				(lambda (x y)
				  (if (is-a? dc post-script-dc%)
				      (send dc draw-ellipse
					    (- x (/ ellipse-size 2)) (- y (/ ellipse-size 2))
					    ellipse-size ellipse-size)
				      (send dc draw-point x y)))]
			       [(old-pen) (send dc get-pen)]
			       [(old-brush) (send dc get-brush)])
					;(send dc draw-rectangle x y w h)
					;(send dc draw-rectangle x (+ y s) w (- h d s))

		   (send dc set-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
		   (send dc set-brush (send the-brush-list find-or-create-brush "BLACK" 'solid))

		   (draw-dot l yp)
		   (draw-dot (/ (+ l r) 2) yp)
		   (draw-dot r yp)

		   (send dc set-brush old-brush)
		   (send dc set-pen old-pen))))))

  (define-values (arrow b-arrow g-arrow bg-arrow checked-arrow blank-arrow)
    (let* ([arrow/letter-space 1]
	   [arrow-height 6]
	   [get-w/h/d/s/l/r
	    (lambda (descender?)
	      (lambda (dc)
		(let*-values ([(width height descent space) (send dc get-text-extent "bg")]
			      [(cap-size) (- height space descent)]
			      [(text-height) (- height (if descender? 0 descent))]
			      [(arrow-space) (- (+ text-height arrow/letter-space)
						(- (/ cap-size 2) (/ arrow-height 2)))]
			      [(total-arrow-height) (+ cap-size arrow-space)])
		  (values (* width 2)
			  total-arrow-height
			  0
			  arrow-space
			  0
			  0))))]
	   [draw-arrow
	    (lambda (dc x y descender?)
	      (let*-values ([(w h d s _1 _2) ((get-w/h/d/s/l/r descender?) dc)]
			    [(bgw bgh bgd bgs) (send dc get-text-extent "bg")]
			    [(text-height) (- bgh (if descender? 0 bgd))]
			    [(cap-size) (- h d s)])

					;(send dc draw-rectangle x y w h)
					;(send dc draw-rectangle x (+ y s) w (- h d s))

		(let* ([x1 (+ x w)]
		       [y1 (+ y (- h (/ cap-size 2)))]
		       [x2 (- x1 4)]
		       [y2 (- y1 3)]
		       [x3 x2]
		       [y3 (+ y1 3)]
		       [old-pen (send dc get-pen)])

		  (when (is-a? dc post-script-dc%)
		    (send dc set-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid)))

		  (send dc draw-line x2 y1 x y1)
		  
		  (send dc draw-line x1 y1 x2 y2)
		  (send dc draw-line x2 y2 x3 y3)
		  (send dc draw-line x3 y3 x1 y1)

		  (send dc set-pen old-pen))))]

	   [draw-text
	    (lambda (dc x y text descender? set-font?)
	      (let-values ([(w h d s _1 _2) ((get-w/h/d/s/l/r descender?) dc)]
			   [(bw bh bd bs) (send dc get-text-extent text)]
                           [(old-font) (send dc get-font)])
                (when set-font?
                  (send dc set-font (send the-font-list find-or-create-font (typeset-size)
                                          'roman 'normal 'normal #f)))
		(send dc draw-text text (floor (+ x (- (/ w 2) (/ bw 2)))) y)
                (send dc set-font old-font)))]
	   
	   [arrow
	    (drawing "robby:arrow"
		     (get-w/h/d/s/l/r #t)
		     (lambda (dc x y) (draw-arrow dc x y #t)))]
	   [b-arrow
	    (drawing "robby:b-arrow"
		     (get-w/h/d/s/l/r #f)
		     (lambda (dc x y)
		       (draw-text dc x y "b" #f #t)
		       (draw-arrow dc x  y #f)))]
	   [g-arrow
	    (drawing "robby:g-arrow"
		     (get-w/h/d/s/l/r #t)
		     (lambda (dc x y)
		       (draw-text dc x y "g" #t #t)
		       (draw-arrow dc x y #t)))]
	   [bg-arrow
	    (drawing "robby:bg-arrow"
		     (get-w/h/d/s/l/r #t)
		     (lambda (dc x y)
		       (draw-text dc x y "bg" #t #t)
		       (draw-arrow dc x y #t)))]
	   [checked-arrow
	    (drawing "robby:checked-arrow"
		     (get-w/h/d/s/l/r #f)
		     (lambda (dc x y)
		       (let ([old-font (send dc get-font)])
			 (send dc set-font (send the-font-list
						 find-or-create-font
						 (typeset-size)
						 'symbol
						 (send old-font get-style)
						 (send old-font get-weight)
						 (send old-font get-underlined)))
			 (draw-text dc x y (string (integer->char 214)) #f #f)
			 (send dc set-font old-font)
			 (draw-arrow dc x y #f))))]
	   [blank-arrow
	    (drawing "robby:blank-arrow"
		     (get-w/h/d/s/l/r #f)
		     (lambda (dc x y)
		       (void)))])
      (values arrow b-arrow g-arrow bg-arrow checked-arrow blank-arrow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                     ;;;
;;;                     SUB/SUPERSCRIPT                 ;;;
;;;                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-struct size (width height descent space left right))
  (define-struct pos (x y))

  (define position-admin%
    (class/d snip-admin% (position-snip calc-positions snips)
      ((public get-sizes get-poss)
       (override get-dc get-editor
		 get-view get-view-size
		 needs-update
		 recounted release-snip
		 resized
		 scroll-to
		 set-caret-owner
		 update-cursor))

      (define sizes (map (lambda (snip) (make-size 0 0 0 0 0 0)) snips))
      (define poss (map (lambda (snip) (make-pos 0 0)) snips))

      (define (get-sizes)
	(update-sizes/poss)
	sizes)

      (define (get-poss)
	(update-sizes/poss)
	poss)

      (define (update-sizes/poss)
	(with-editor
	 (lambda (editor)
	   (let ([dc (send editor get-dc)])
	     (when dc
	       (set! sizes 
		     (map
		      (lambda (snip)
			(let ([bwb (box 0)]
			      [bhb (box 0)]
			      [bdb (box 0)]
			      [bsb (box 0)]
			      [blb (box 0)]
			      [brb (box 0)]
			      [xb (box 0)]
			      [yb (box 0)])
					;(send editor get-snip-location position-snip xb yb)
			  (send snip get-extent dc (unbox xb) (unbox yb) bwb bhb bdb bsb blb brb)
			  (make-size (unbox bwb)
				     (unbox bhb)
				     (unbox bdb)
				     (unbox bsb)
				     (unbox blb)
				     (unbox brb))))
		      snips))
	       (set! poss (calc-positions sizes)))))))
      
      (define (with-editor f)
	(let ([admin (send position-snip get-admin)])
	  (if admin
	      (let ([editor (send admin get-editor)])
		(if editor
		    (f editor)
		    #f))
	      #f)))
      (define (with-editor-admin f)
	(with-editor
	 (lambda (editor)
	   (let ([admin (send editor get-admin)])
	     (if admin
		 (f admin)
		 #f)))))
      
      (define (get-dc)
	(with-editor (lambda (editor) (send editor get-dc))))
      (define (get-editor) (with-editor (lambda (x) x)))
      (define (get-view xb yb wb hb wanted-snip)
	(for-each (lambda (b) (set-box/f! b 10)) (list xb yb wb hb))
	(with-editor
	 (lambda (editor)
	   (if wanted-snip
	       (begin
		 (update-sizes/poss)
		 (let loop ([snips snips]
			    [sizes sizes]
			    [poss poss])
		   (cond
		    [(null? snips) (void)]
		    [else
		     (let ([snip (car snips)]
			   [size (car sizes)]
			   [pos (car poss)])
		       (if (eq? wanted-snip snip)
			   (begin
			     (set-box/f! xb (pos-x pos))
			     (set-box/f! yb (pos-y pos))
			     (set-box/f! wb (size-width size))
			     (set-box/f! hb (size-height size)))
			   (loop (cdr snips)
				 (cdr sizes)
				 (cdr poss))))])))
	       (send editor get-view xb yb wb hb wanted-snip))))
	(void))

      (define (get-view-size wb hb)
	(set-box/f! wb 10)
	(set-box/f! hb 10)
	(with-editor
	 (lambda (editor)
	   (send editor get-view #f #f wb hb position-snip))))

      (define (needs-update wanted-snip localx localy w h)
	(with-editor-admin
	 (lambda (admin)
	   (update-sizes/poss)
	   (let-values ([(thisx thisy)
			 (let loop ([snips snips]
				    [poss poss])
			   (cond
			    [(null? snips) (values 0 0)]
			    [else (let ([snip (car snips)]
					[pos (car poss)])
				    (if (eq? wanted-snip snip)
					(values (pos-x pos)
						(pos-y pos))
					(loop (cdr snips)
					      (cdr poss))))]))])
	     (send admin needs-update position-snip thisx thisy w h)))))

      (define (refresh-snip wanted-snip)
	(with-editor
	 (lambda (editor)
	   (let ([dc (send editor get-dc)])
	     (when dc
	       (update-sizes/poss)
	       (let loop ([snips snips]
			  [sizes sizes])
		 (cond
		  [(null? snips) (void)]
		  [else
		   (let ([snip (car snips)]
			 [size (car sizes)])
		     (if (eq? snip wanted-snip)
			 (needs-update snip 0 0 (size-width size) (size-height size))
			 (loop (cdr snips))))])))))))

      (define (recounted snip update-now?)
	(when update-now?
	  (refresh-snip snip)))

      (define (release-snip snip) #f)

      (define (resized snip refresh?)
	(update-sizes/poss)
	(when refresh?
	  (refresh-snip snip)))

      (define (scroll-to wanted-snip localx localy w h refresh? bias)
	(with-editor-admin
	 (lambda (admin)
	   (let-values ([(thisx thisy)
			 (let loop ([snips snips]
				    [poss poss])
			   (cond
			    [(null? snips) (values 0 0)]
			    [else (let ([snip (car snips)]
					[pos (car poss)])
				    (if (eq? wanted-snip snip)
					(values (pos-x pos)
						(pos-y pos))
					(loop (cdr snips)
					      (cdr poss))))]))])
	     (send admin scroll-to thisx thisy w h refresh? bias)))))

      (define (set-caret-owner snip domain)
	(void))

      (define (update-cursor)
	(with-editor-admin
	 (lambda (admin)
	   (send admin update-cursor))))

      (super-init)
      (for-each (lambda (snip) (send snip set-admin this)) snips)))

  (define position-snip%
    (class/d snip% (position-snipclass calc-positions calc-size _snips) 
      ((inherit set-snipclass get-style)
       (override get-extent draw copy write))

      (define snips (map (lambda (snip) (send snip copy)) _snips))

      (define (write p)
	(send p << (length snips))
	(for-each (lambda (snip)
		    (send p << (send (send snip get-snipclass) get-classname))
		    (send snip write p))
		  snips))

      (define (copy)
	(let ([snip (make-object position-snip%
		      position-snipclass
		      calc-positions
		      calc-size
		      snips)])
	  (send snip set-style (get-style))
	  snip))

      (define (get-extent dc x y wb hb db sb lb rb)
	(let ([sizes (send admin get-sizes)])
	  (let ([size (calc-size sizes)])
	    (set-box/f! wb (size-width size))
	    (set-box/f! hb (size-height size))
	    (set-box/f! db (size-descent size))
	    (set-box/f! sb (size-space size))
	    (set-box/f! lb (size-left size))
	    (set-box/f! rb (size-right size)))))
      
      (define (draw dc x y left top right bottom dx dy draw-caret)
	(let ([positions (calc-positions (send admin get-sizes))])
	  (for-each
	   (lambda (snip pos)
	     (send snip draw dc
		   (+ x (pos-x pos))
		   (+ y (pos-y pos))
		   left top right bottom dx dy draw-caret))
	   snips
	   positions)))

      (super-init)

      (define admin (make-object position-admin% this calc-positions snips))
      (set-snipclass position-snipclass)))
  
  (define position-snipclass%
    (class/d snip-class% (calc-positions calc-size)
      ((override read))

      (define (read f)
	(define (get-next)
	  (let* ([classname (send f get-string)]
		 [snipclass (send (get-the-snip-class-list) find classname)])
	    (send snipclass read f)))

	(make-object position-snip% 
	  this
	  calc-positions
	  calc-size
	  (let loop ([n (send f get-exact)])
	    (cond
	     [(<= n 0) null]
	     [else (cons (get-next) (loop (- n 1)))]))))

      (super-init)))

  (define (position calc-positions calc-size name)
    (define position-snipclass (make-object position-snipclass% calc-positions calc-size))
    (send position-snipclass set-classname name)
    (send position-snipclass set-version 1)
    (send (get-the-snip-class-list) add position-snipclass)

    (lambda (snips)
      (make-object position-snip% position-snipclass calc-positions calc-size snips)))

  (define sup
    (let ([make-sup
	   (position
	    (lambda (sizes)
	      (let ([base (car sizes)]
		    [pow (cadr sizes)])
		(list (make-pos
		       0
		       (- (max (/ (size-height pow) 2) (size-space base))
			  (size-space base)))
		      (make-pos
		       (size-width base)
		       (max 0 (- (size-space base) (/ (size-height pow) 2)))))))
	    (lambda (sizes)
	      (let ([base (car sizes)]
		    [pow (cadr sizes)])
		(make-size
		 (+ (size-width base) (size-width pow))
		 (+ (- (size-height base) (size-space base)) (max (size-space base) (floor (/ (size-height pow) 2))))
		 (size-descent base)
		 (max (size-space base) (floor (/ (size-height pow) 2)))
		 (size-left base)
		 (size-right pow))))
	    "robby:sup")])
      (lambda (base pow)
	(make-sup
	 (list (snipize/copy base)
	       (snipize/copy pow))))))

  (define sub
    (let ([make-sub
	   (position
	    (lambda (sizes)
	      (let ([base (car sizes)]
		    [sub (cadr sizes)])
		(list (make-pos 0 0)
		      (make-pos
		       (size-width base)
		       (- (size-height base)
			  (size-descent base)
			  (floor (/ (size-height sub) 2)))))))
	    (lambda (sizes)
	      (let ([base (car sizes)]
		    [sub (cadr sizes)])
		(make-size
		 (+ (size-width base) (size-width sub))
		 (+ (- (size-height base) (size-descent base)) (max (size-descent base) (floor (/ (size-height sub) 2))))
		 (max (size-descent base) (floor (/ (size-height sub) 2)))
		 (size-space base)
		 (size-left base)
		 (size-right sub))))
	    "robby:sub")])
      (lambda (base sub)
	(make-sub
	 (list (snipize/copy base)
	       (snipize/copy sub))))))

  (unit/sig typeset:utils^
    (import)
    (rename (-single-bracket single-bracket)
	    (-double-bracket double-bracket)
	    (-tb-align tb-align)
	    (-greek greek)
	    (-drawing drawing)
	    (-ellipses ellipses)

	    (-position position)
	    (-sup sup) (-sub sub)

            (-postscript postscript)
            
	    (-arrow arrow) (-b-arrow b-arrow)
	    (-g-arrow g-arrow) (-bg-arrow bg-arrow)
	    (-checked-arrow checked-arrow)
	    (-blank-arrow blank-arrow)
	    (-typeset-size typeset-size))

    (define -single-bracket single-bracket) 
    (define -double-bracket double-bracket)
    (define -tb-align tb-align)
    (define -greek greek)
    (define -drawing drawing)
    (define -ellipses ellipses)

    (define -position position)
    (define -sup sup)
    (define -sub sub)

    (define -postscript postscript)
    
    (define -arrow arrow)
    (define -b-arrow b-arrow)
    (define -g-arrow g-arrow)
    (define -bg-arrow bg-arrow)
    (define -checked-arrow checked-arrow)
    (define -blank-arrow blank-arrow)

    (define -typeset-size typeset-size)))