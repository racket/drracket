(module arrow mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))
  
  (provide draw-arrow)
  
  
  (define pi (* 2 (asin 1)))
  (define arrow-head-angle (/ pi 8))
  (define cos-angle (cos arrow-head-angle))
  (define sin-angle (sin arrow-head-angle))
  (define arrow-head-size 10)
  (define arrow-root-radius 3.5)
  (define (draw-arrow dc start-x start-y end-x end-y dx dy)
    (send dc draw-line
          (+ start-x dx) (+ start-y dy)
          (+ end-x dx) (+ end-y dy))
    (send dc draw-ellipse 
          (- (+ start-x dx) arrow-root-radius)
          (- (+ start-y dy) arrow-root-radius)
          (* 2 arrow-root-radius)
          (* 2 arrow-root-radius))
    (unless (and (= start-x end-x)
                 (= start-y end-y))
      (let* ([delta   0]
             [ofs-x   (- start-x end-x)]
             [ofs-y   (- start-y end-y)]
             [len     (sqrt (+ (* ofs-x ofs-x) (* ofs-y ofs-y)))]
             [ofs-x   (/ ofs-x len)]
             [ofs-y   (/ ofs-y len)]
             [head-x  (* ofs-x arrow-head-size)]
             [head-y  (* ofs-y arrow-head-size)]
             [end-x   (+ end-x (* ofs-x delta))]
             [end-y   (+ end-y (* ofs-y delta))]
             [pt1     (make-object point% end-x end-y)]
             [pt2     (make-object point%
                        (+ end-x (* cos-angle head-x) 
                           (* sin-angle head-y))
                        (+ end-y (- (* sin-angle head-x))
                           (* cos-angle head-y)))]
             [pt3     (make-object point%
                        (+ end-x (* cos-angle head-x)
                           (- (* sin-angle head-y)))
                        (+ end-y (* sin-angle head-x)
                           (* cos-angle head-y)))]
             [pts (list pt1 pt2 pt3)])
        (send dc draw-polygon pts dx dy)))))
