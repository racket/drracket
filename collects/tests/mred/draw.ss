
(define sys-path 
  (lambda (f)
    (build-path (collection-path "icons") f)))

(let* ([f (make-object mred:frame% ()
		       "Graphics Test"
		       -1 -1 300 350)]
       [vp (make-object mred:vertical-panel% f)]
       [hp0 (make-object mred:horizontal-panel% vp)]
       [hp (make-object mred:horizontal-panel% vp)]
       [hp2 (make-object mred:horizontal-panel% vp)]
       [bb (make-object wx:bitmap% (sys-path "bb.gif")
			wx:const-bitmap-type-gif)]
       [return (make-object wx:bitmap% (sys-path "return.xbm")
			    wx:const-bitmap-type-xbm)]
       [tmp-mdc (make-object wx:memory-dc%)]
       [use-bitmap? #f]
       [depth-one? #f])
  (send hp0 stretchable-in-y #f)
  (send hp stretchable-in-y #f)
  (send hp2 stretchable-in-y #f)
  (let ([canvas
	 (make-object
	  (make-class mred:canvas%
	    (inherit get-dc)
	    (public
	     [no-bitmaps? #f]
	     [set-bitmaps (lambda (on?) (set! no-bitmaps? (not on?)) (on-paint))]
	     [no-stipples? #f]
	     [set-stipples (lambda (on?) (set! no-stipples? (not on?)) (on-paint))]
	     [scale 1]
	     [set-scale (lambda (s) (set! scale s) (on-paint))]
	     [offset 0]
	     [set-offset (lambda (o) (set! offset o) (on-paint))]
	     [on-paint
	      (case-lambda
	       [() (on-paint #f)]
	       [(ps?)
		(let* ([can-dc (get-dc)]
		       [pen0s (make-object wx:pen% "BLACK" 0 wx:const-solid)]
		 [pen1s (make-object wx:pen% "BLACK" 1 wx:const-solid)]
		 [pen2s (make-object wx:pen% "BLACK" 2 wx:const-solid)]
		 [pen0t (make-object wx:pen% "BLACK" 0 wx:const-transparent)]
		 [pen1t (make-object wx:pen% "BLACK" 1 wx:const-transparent)]
		 [pen2t (make-object wx:pen% "BLACK" 2 wx:const-transparent)]
		 [brushs (make-object wx:brush% "BLACK" wx:const-solid)]
		 [brusht (make-object wx:brush% "BLACK" wx:const-transparent)]
		 [penr (make-object wx:pen% "RED" 1 wx:const-solid)]
		 [brushb (make-object wx:brush% "BLUE" wx:const-solid)]
		 [mem-dc (if use-bitmap?
			     (make-object wx:memory-dc%)
			     #f)]
		 [bm (if use-bitmap?
			 (make-object wx:bitmap% (* scale 300) (* scale 300)
				      (if depth-one? 1 -1))
			 #f)]
		 [draw-series
		  (lambda (dc pens pent size x y flevel last?)
		    (let* ([ofont (send dc get-font)])
		      (if (positive? flevel)
			  (send dc set-font
				(make-object wx:font%
					     10 wx:const-decorative
					     wx:const-normal 
					     (if (> flevel 1)
						 wx:const-bold
						 wx:const-normal)
					     #t)))

		      (send dc set-pen pens)
		      (send dc set-brush brusht)
		      
		      ; Test should overlay this line:
		      (send dc draw-line 
			    (+ x 3) (+ y 12)
			    (+ x 40) (+ y 12))

		      (send dc draw-text (string-append size " Pen")
			    (+ x 5) (+ y 8))
		      (send dc set-font ofont)
		      
		      (send dc draw-line
			    (+ x 5) (+ y 27) (+ x 10) (+ 27 y))
		      (send dc draw-rectangle
			    (+ x 5) (+ y 30) 5 5)
		      (send dc draw-line
			    (+ x 12) (+ y 30) (+ x 12) (+ y 35))
		      
		      (send dc draw-line
			    (+ x 5) (+ y 40) (+ x 10) (+ 40 y))
		      (send dc draw-rectangle
			    (+ x 5) (+ y 41) 5 5)
		      (send dc draw-line
			    (+ x 10) (+ y 41) (+ x 10) (+ 46 y))
		      
		      (send dc draw-line
			    (+ x 15) (+ y 25) (+ x 20) (+ 25 y))
		      (send dc draw-line
			    (+ x 20) (+ y 30) (+ x 20) (+ 25 y))
		      
		      (send dc draw-line
			    (+ x 30) (+ y 25) (+ x 25) (+ 25 y))
		      (send dc draw-line
			    (+ x 25) (+ y 30) (+ x 25) (+ 25 y))
		      
		      (send dc draw-line
			    (+ x 35) (+ y 30) (+ x 40) (+ 30 y))
		      (send dc draw-line
			    (+ x 40) (+ y 25) (+ x 40) (+ 30 y))
		      
		      (send dc draw-line
			    (+ x 50) (+ y 30) (+ x 45) (+ 30 y))
		      (send dc draw-line
			    (+ x 45) (+ y 25) (+ x 45) (+ 30 y))

		      ; Check line thickness with "X"
		      (send dc draw-line
			    (+ x 20) (+ y 45) (+ x 40) (+ 39 y))
		      (send dc draw-line
			    (+ x 20) (+ y 39) (+ x 40) (+ 45 y))
		      
		      (send dc draw-rectangle
			    (+ x 5) (+ y 50) 10 10)
		      (send dc draw-rounded-rectangle
			    (+ x 5) (+ y 65) 10 10 3)
		      (send dc draw-ellipse
			    (+ x 5) (+ y 80) 10 10)
		      
		      (send dc set-brush brushs)
		      (send dc draw-rectangle
			    (+ x 17) (+ y 50) 10 10)
		      (send dc draw-rounded-rectangle
			    (+ x 17) (+ y 65) 10 10 3)
		      (send dc draw-ellipse
			    (+ x 17) (+ y 80) 10 10)
		      
		      (send dc set-pen pent)
		      (send dc draw-rectangle
			    (+ x 29) (+ y 50) 10 10)
		      (send dc draw-rounded-rectangle
			    (+ x 29) (+ y 65) 10 10 3)
		      (send dc draw-ellipse
			    (+ x 29) (+ y 80) 10 10)
		      

		      (send dc set-pen pens)
		      (send dc draw-rectangle
			    (+ x 17) (+ y 95) 10 10)
		      (send dc set-logical-function wx:const-clear)
		      (send dc draw-rectangle
			    (+ x 18) (+ y 96) 8 8)
		      (send dc set-logical-function wx:const-copy)
		      
		      (send dc draw-rectangle
			    (+ x 29) (+ y 95) 10 10)
		      (send dc set-logical-function wx:const-clear)
		      (send dc set-pen pent)
		      (send dc draw-rectangle
			    (+ x 30) (+ y 96) 8 8)

		      (send dc set-pen pens)
		      (send dc draw-rectangle
			    (+ x 5) (+ y 95) 10 10)
		      (send dc set-logical-function wx:const-xor)
		      (send dc draw-rectangle
			    (+ x 5) (+ y 95) 10 10)
		      (send dc set-logical-function wx:const-copy)
		      
		      (send dc draw-line
			    (+ x 5) (+ y 110) (+ x 8) (+ y 110))
		      (send dc draw-line
			    (+ x 8) (+ y 110) (+ x 11) (+ y 113))
		      (send dc draw-line
			    (+ x 11) (+ y 113) (+ x 11) (+ y 116))
		      (send dc draw-line
			    (+ x 11) (+ y 116) (+ x 8) (+ y 119))
		      (send dc draw-line
			    (+ x 8) (+ y 119) (+ x 5) (+ y 119))
		      (send dc draw-line
			    (+ x 5) (+ y 119) (+ x 2) (+ y 116))
		      (send dc draw-line
			    (+ x 2) (+ y 116) (+ x 2) (+ y 113))
		      (send dc draw-line
			    (+ x 2) (+ y 113) (+ x 5) (+ y 110))
		      
		      (send dc draw-lines
			    (list
			     (make-object wx:point% 5 95)
			     (make-object wx:point% 8 95)
			     (make-object wx:point% 11 98)
			     (make-object wx:point% 11 101)
			     (make-object wx:point% 8 104)
			     (make-object wx:point% 5 104)
			     (make-object wx:point% 2 101)
			     (make-object wx:point% 2 98)
			     (make-object wx:point% 5 95))
			    (+ x 12) (+ y 15))

		      (send dc draw-line
			    (+ x 5) (+ y 125) (+ x 10) (+ y 125))
		      (send dc draw-line
			    (+ x 11) (+ y 125) (+ x 16) (+ y 125))

		      (send dc set-brush brusht)
		      (send dc draw-arc 
			    (+ x 20) (+ y 135)
			    (+ x 5) (+ y 150)
			    (+ x 20) (+ y 150))
		      (send dc draw-arc 
			    (+ x 35) (+ y 150)
			    (+ x 20) (+ y 135)
			    (+ x 20) (+ y 150))
		      (send dc set-brush brushs)
		      (send dc draw-arc 
			    (+ x 60) (+ y 135)
			    (+ x 36) (+ y 150)
			    (+ x 60) (+ y 150))		
		      (send dc set-brush brusht)      

		      (unless (or no-bitmaps? (not last?))
			      (let ([x 5] [y 165])
				(send dc draw-icon
				      (mred:get-icon) x y)
				(set! x (+ x (send (mred:get-icon) get-width)))
				(let ([do-one
				       (lambda (bm mode)
					 (if (send bm ok?)
					     (begin
					       (send tmp-mdc select-object bm)
					       (let ([h (send bm get-height)]
						     [w (send bm get-width)])
						 (send dc blit x y 
						       w h
						       tmp-mdc 0 0
						       mode)
						 (set! x (+ x w 10)))
					       (send tmp-mdc select-object null))
					     (printf "bad bitmap~n")))])
				  (do-one bb wx:const-copy)
				  (do-one return wx:const-copy)
				  (send dc set-pen penr)
				  (do-one return wx:const-copy)
				  (do-one return wx:const-colour)
				  (do-one bb wx:const-colour)
				  (let ([bg (send dc get-background)])
				    (send dc set-background brushs)
				    (do-one return wx:const-colour)
				    (send dc set-background bg))
				  (send dc set-pen pens))))
			      
		      (unless (or no-stipples? (not last?))
			      (send dc set-brush brushb)
			      (send dc draw-rectangle 80 200 100 40)
			      (when (send return ok?)
				    (let ([b (make-object wx:brush% "GREEN" wx:const-stipple)])
				      (send b set-stipple return)
				      (send dc set-brush b)
				      (send dc draw-rectangle 85 205 30 30)
				      (send dc set-brush brushs)
				      (send b set-style wx:const-opaque-stipple)
				      (send dc set-brush b)
				      (send dc draw-rectangle 120 205 30 30)
				      (send dc set-brush brushs)
				      (send b set-stipple bb)
				      (send dc set-brush b)
				      (send dc draw-rectangle 155 205 20 30)
				      (send dc set-brush brushs)
				      (send b set-stipple null))))

		      (if (not (or ps? (eq? dc can-dc)))
			  (send can-dc blit 0 0 
				(* scale 300) (* scale 300)
				mem-dc 0 0 wx:const-copy)))

		    'done)])

		  (send (get-dc) set-user-scale 1 1)
		  (send (get-dc) set-device-origin 0 0)

		  (let ([dc (if ps?
				(let ([dc (make-object wx:post-script-dc% null #t)])
				  (and (send dc ok?) dc))
				(if (and use-bitmap? (send bm ok?))
				    (begin
				      (send mem-dc select-object bm)
				      mem-dc)
				    (get-dc)))])
		    (when dc
			  (when ps?
				(send dc start-doc "Draw Test")
				(send dc start-page))

			  (send dc set-user-scale scale scale)
			  (send dc set-device-origin offset offset)

			  (send dc clear)
			  ; check default pen/brush:
			  (send dc draw-rectangle 0 0 5 5)
			  (send dc draw-line 0 0 20 6)

			  (draw-series dc pen0s pen0t "0 x 0" 5 0 0 #f)
			  
			  (draw-series dc pen1s pen1t "1 x 1" 70 0 1 #f)
			  
			  (draw-series dc pen2s pen2t "2 x 2" 135 0 2 #t)

			  (when ps?
				(send dc end-page)
				(send dc end-doc))))
		  
		  'done)])]))
	  vp 0 50 300 300)])
    (make-object mred:radio-box% hp0
		 (lambda (self event)
		   (set! use-bitmap? (< 0 (send event get-command-int)))
		   (set! depth-one? (< 1 (send event get-command-int)))
		   (send canvas on-paint))
		 null
		 -1 -1 -1 -1
		 '("Canvas" "Pixmap" "Bitmap")
		 0 wx:const-horizontal)
    (make-object mred:button% hp
		 (lambda (self event)
		   (send canvas on-paint #t))
		 "PostScript")
    (make-object mred:check-box% hp
		 (lambda (self event)
		   (send canvas set-scale (if (send event checked?) 2 1)))
		 "*2")
    (make-object mred:check-box% hp
		 (lambda (self event)
		   (send canvas set-offset (if (send event checked?) 10 0)))
		 "+10")
    (send (make-object mred:check-box% hp2
		       (lambda (self event)
			 (send canvas set-bitmaps (send event checked?)))
		       "Icons")
	  set-value #t)
    (send (make-object mred:check-box% hp2
		       (lambda (self event)
			 (send canvas set-stipples (send event checked?)))
		       "Stipples")
	  set-value #t))

  (send f show #t))
