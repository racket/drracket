(test 
 'single-panel
 (lambda (x) (eq? x 'passed))
 `(let* ([semaphore (make-semaphore 0)]
	 [semaphore-frame%
	  (class frame% args
	    (override
	     [on-close (lambda () (semaphore-post semaphore))])
	    (sequence
	      (apply super-init args)))]
	 [f (make-object semaphore-frame% "Single Panel Test")]
	 [blue-brush (send the-brush-list find-or-create-brush "BLUE" 'solid)]
	 [green-brush (send the-brush-list find-or-create-brush "FOREST GREEN" 'solid)]
	 [black-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid)]
	 [grid-canvas%
	  (class canvas% (lines parent label stretchable-width? stretchable-height?)
	    (inherit get-dc get-client-size)
	    (override
	     [on-paint
	      (lambda ()
		(let-values ([(width height) (get-client-size)])
		  (let ([dc (get-dc)]
			[single-width (/ width lines)]
			[single-height (/ height lines)])
		    (send dc set-pen black-pen)
		    (let loop ([i lines])
		      (cond
			[(zero? i) (void)]
			[else
			 (let loop ([j lines])
			   (cond
			     [(zero? j) (void)]
			     [else 
			      (send dc set-brush
				    (if (= 0 (modulo (+ i j) 2))
					blue-brush green-brush))
			      (send dc draw-rectangle
				    (* single-width (- i 1))
				    (* single-height (- j 1))
				    single-width
				    single-height)
			      (loop (- j 1))]))
			 (loop (- i 1))])))))])
	    (inherit set-label min-width min-height stretchable-height stretchable-width)
	    (sequence
	      (super-init parent)
	      (stretchable-width stretchable-width?)
	      (stretchable-height stretchable-height?)
	      (min-width 50)
	      (min-height 50)
	      (set-label label)))]
	  
	 [border-panel (make-object horizontal-panel% f '(border))]
	 [single-panel (make-object panel:single% border-panel)]
	 [children (list (make-object grid-canvas% 3 single-panel "Small" #f #f)
			 (make-object grid-canvas% 3 single-panel "Wide" #f #t)
			 (make-object grid-canvas% 3 single-panel "Tall" #t #f)
			 (make-object grid-canvas% 3 single-panel "Wide and Tall" #t #t))]
	 [active-child (car children)]
	 [radios (make-object horizontal-panel% f)]
	 [make-radio
	  (lambda (label choices callback)
	    (let* ([panel (make-object vertical-panel% radios '(border))]
		   [message (make-object message% label panel)]
		   [radio (make-object radio-box% #f choices panel (lambda (radio _) (callback radio)))]
		   [button (make-object button%
			     "Cycle" panel
			     (lambda (_1 _2)
			       (let ([before (send radio get-selection)]
				     [tot (send radio get-number)])
				 (let loop ([n tot])
				   (unless (zero? n)
				     (send radio set-selection (- tot n))
				     (callback radio)
				     (sleep/yield 1)
				     (loop (- n 1))))
				 (send radio set-selection before)
				 (callback radio))))])
	      radio))]
	 [radio
	  (make-radio
	   "Active Child"
	   (map (lambda (x) (send x get-label)) children)
	   (lambda (radio)
	     (let loop ([n (length children)]
			[cs children])
	       (cond
		 [(null? cs) (void)]
		 [else (let ([c (car cs)])
			 (if (string=? (send radio get-item-label (send radio get-selection))
				       (send c get-label))
			     (begin (set! active-child c)
				    (send single-panel active-child active-child))
			     (loop (- n 1)
				   (cdr cs))))]))))]
	 [vertical-alignment 'center]
	 [horizontal-alignment 'center]
	 [update-alignment (lambda () 
			     (send single-panel set-alignment horizontal-alignment vertical-alignment))]
	 [horiz
	  (make-radio 
	   "Horizontal Alignment"
	   (list "left" "center" "right")
	   (lambda (radio)
	     (set! horizontal-alignment (string->symbol (send radio get-item-label (send radio get-selection))))
	     (update-alignment)))]
	 [vert
	  (make-radio
	   "Vertical Alignment"
	   (list "top" "center" "bottom")
	   (lambda (radio)
	     (set! vertical-alignment (string->symbol (send radio get-item-label (send radio get-selection))))
	     (update-alignment)))]
	 [buttons (make-object horizontal-panel% f)]
	 [result 'failed]
	 [failed (make-object button% "Failed" buttons (lambda (_1 _2) (semaphore-post semaphore)))]
	 [passed (make-object button% "Passed" buttons (lambda (_1 _2) 
							 (set! result 'passed)
							 (semaphore-post semaphore)))])
    (send border-panel min-width 100)
    (send border-panel min-height 100)
    (send vert set-selection 1)
    (send horiz set-selection 1)
    (send buttons stretchable-height #f)
    (send buttons set-alignment 'right 'center)
    (send radios stretchable-height #f)
    (send f show #t)
    (yield semaphore)
    (send f show #f)
    result))
