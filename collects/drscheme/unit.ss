
(define drscheme:unit@
  (unit/sig drscheme:unit^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [drscheme:basis : drscheme:basis^]
	    [drscheme:setup : drscheme:setup^]
	    [drscheme:compound-unit : drscheme:compound-unit^]
	    [drscheme:tool : drscheme:tool^]
	    [drscheme:frame : drscheme:frame^])
    
    (mred:debug:printf 'invoke "drscheme:unit@")

    (define snip%
      (class wx:snip% (n fn)
	(inherit get-admin set-snipclass)
	(rename [super-get-flags get-flags])
	(private
	 [invalidate-to
	  (lambda (c)
	    (let-values ([(left1 top1 right1 bottom1) (get-pos)]
			 [(left2 top2 right2 bottom2) (send c get-pos)])
	      (let ([media (send (get-admin) get-media)]
		    [top (min top1 top2)]
		    [left (min left1 left2)]
		    [bottom (max bottom1 bottom2)]
		    [right (max right1 right2)])
		(send media invalidate-bitmap-cache
		      left top (- right left) (- bottom top)))))])
	(public
	 [frame% drscheme:frame:unit-frame%]
	 [this% snip%]
	 [width 70]
	 [height 30]
	 [parents null]
	 [children null])
	
	(private
	 ;; frame is either a filename or a frame object.
	 [frame fn])
	(public
	 ;; this is called with a filename when the frame is closed
	 [on-close-frame
	  (lambda (s)
	    (set! frame s))]
	 [open
	  (lambda ()
	    (if (is-a? frame wx:frame%)
		(send frame show #t)
		(set! frame (make-object frame% frame #f this))))]
	 
	 [get-pos
	  (lambda ()
	    (let ([media (send (get-admin) get-media)])
	      (let ([lbox (box 0)]
		    [rbox (box 0)]
		    [tbox (box 0)]
		    [bbox (box 0)])
		(send media get-snip-location this lbox tbox #f)
		(send media get-snip-location this rbox bbox #t)
		(values (unbox lbox) (unbox tbox)
			(unbox rbox) (unbox bbox)))))]
	 [add-child
	  (lambda (c)
	    (if (eq? c this)
		(wx:message-box "A unit cannot be a parent or child of itself.")
		(unless (member c children)
		  (set! children (cons c children))
		  (invalidate-to c))))]
	 [add-parent
	  (lambda (c)
	    (unless (or (member c parents)
			(eq? c this))
	      (set! parents (cons c parents))
	      (invalidate-to c)
	      (when (object? frame)
		(send frame on-change-imports))))]
	 [name n]
	 [copy (lambda () (make-object this% name frame))]
	 [write (lambda (s) 
		  (let* ([filename
			  (if (object? wx:frame%)
			      (send (ivar frame edit) get-filename)
			      frame)]
			 [string (format "~s" (list name filename))])
		    (send s put string)))]
	 [resize
	  (lambda (w h)
	    (set! width w)
	    (set! height h)
	    (send (get-admin) resized this #t)
	    #t)]
	 [draw
	  (lambda (dc x y left top right bottom dx dy draw-caret)
	    '(printf "x: ~a y:~a width: ~a height: ~a~n" x y width height)
	    (let ([xbox (box 0)]
		  [ybox (box 0)]
		  [wbox (box 0)]
		  [hbox (box 0)])
	      (send dc get-clipping-region xbox ybox wbox hbox)
	      (let* ([old-left (unbox xbox)]
		     [old-top (unbox ybox)]
		     [old-width (unbox wbox)]
		     [old-height (unbox hbox)])
		'(printf "old-left: ~a old-top: ~a old-width: ~a old-height: ~a~n"
			old-left old-top old-width old-height)
		(if (< old-width 0)
		    '(send dc set-clipping-region x y width height)
		    (let* ([old-right (+ left old-width)]
			   [old-bottom (+ top old-height)]
			   [new-left (max old-left x)]
			   [new-top (max old-top y)]
			   [new-width (- (min old-right (+ x width)) new-left)]
			   [new-height (- (min old-bottom (+ y height)) new-top)])
		      '(send dc set-clipping-region 
			    new-left new-top new-width new-height)
		      '(printf "new-left: ~a new-top: ~a new-width: ~a new-height: ~a~n"
			      new-left new-top new-width new-height)))
		(send dc draw-rectangle x y width height)
		(let ([h (box 0)]
		      [w (box 0)])
		  (send dc get-text-extent name w h)
		  (send dc draw-text name 
			(+ x (/ (- width (unbox w)) 2))
			(+ y (/ (- height (unbox h)) 2))))
		(if (< old-width 0)
		    (send dc destroy-clipping-region)
		    (send dc set-clipping-region 
			  (unbox xbox) (unbox ybox) (unbox wbox) (unbox hbox))))))]
	 [get-extent
	  (opt-lambda (dc x y
			  [width-box null] [height-box null]
			  [descent-box null] [space-box null]
			  [lspace-box null] [rspace-box null])
	    (let ([size
		   (lambda (v)
		     (lambda (x)
		       (unless (null? x)
			 (set-box! x v))))])
	      ((size width) width-box)
	      ((size height) height-box)
	      (for-each (size 3) 
			(list descent-box space-box lspace-box rspace-box))))]
	 [snipclassq unit-snipclass])
	(sequence
	  (super-init)
	  (set-snipclass snipclassq))))

    (define snip-class%
      (let ([s% snip%])
	(class wx:snip-class% args
	  (inherit set-classname set-version)
	  (public
	    [snip% s%] 
	    [classname "drscheme:unit:snip%"]
	    [version 3]
	    [write-header
	     (lambda (p)
	       (send p put "h"))]
	    [read-header
	     (lambda (p)
	       (send p get-string (box 0)))]
	    [read
	     (lambda (p)
	       (let ([l (mzlib:string@:read-string (send p get-string null))])
		 (make-object snip% (car l) (cadr l))))])
	  (sequence
	    (apply super-init args)
	    (set-classname classname)
	    (set-version version)
	    (send (wx:get-the-snip-class-list) add this)))))

    (define unit-snipclass (make-object snip-class%))))