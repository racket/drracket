
(module arrow mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))
  
  (provide draw-arrow)
  
  (define pi (* 2 (asin 1)))
  
  (define arrow-head-angle (/ pi 8))
  (define cos-arrow-head-angle (cos arrow-head-angle))
  (define sin-arrow-head-angle (sin arrow-head-angle))
  
  (define arrow-head-size 8)
  (define arrow-head-size-cos-arrow-head-angle (* arrow-head-size cos-arrow-head-angle))
  (define arrow-head-size-sin-arrow-head-angle (* arrow-head-size sin-arrow-head-angle))
  
  (define arrow-root-radius 2.5)
  (define arrow-root-diameter (* 2 arrow-root-radius))
  
  ; If alpha is the angle between the x axis and the Start->End vector:
  ;
  ; p2-x = end-x + arrow-head-size * cos(alpha + pi - arrow-head-angle)
  ;      = end-x - arrow-head-size * cos(alpha - arrow-head-angle)
  ;      = end-x - arrow-head-size * (cos(alpha) * cos(arrow-head-angle) + sin(alpha) * sin(arrow-head-angle))
  ;      = end-x - arrow-head-size-cos-arrow-head-angle * cos-alpha - arrow-head-size-sin-arrow-head-angle * sin-alpha
  ;      = end-x - arrow-head-size-cos-arrow-head-angle-cos-alpha - arrow-head-size-sin-arrow-head-angle-sin-alpha
  ;
  ; p2-y = end-y + arrow-head-size * sin(alpha + pi - arrow-head-angle)
  ;      = end-y - arrow-head-size * sin(alpha - arrow-head-angle)
  ;      = end-y - arrow-head-size * (sin(alpha) * cos(arrow-head-angle) - cos(alpha) * sin(arrow-head-angle))
  ;      = end-y - arrow-head-size-cos-arrow-head-angle * sin-alpha + arrow-head-size-sin-arrow-head-angle * cos-alpha
  ;      = end-y - arrow-head-size-cos-arrow-head-angle-sin-alpha + arrow-head-size-sin-arrow-head-angle-cos-alpha
  ;
  ; p3-x = end-x + arrow-head-size * cos(alpha + pi + arrow-head-angle)
  ;      = end-x - arrow-head-size * cos(alpha + arrow-head-angle)
  ;      = end-x - arrow-head-size * (cos(alpha) * cos(arrow-head-angle) - sin(alpha) * sin(arrow-head-angle))
  ;      = end-x - arrow-head-size-cos-arrow-head-angle * cos-alpha + arrow-head-size-sin-arrow-head-angle * sin-alpha
  ;      = end-x - arrow-head-size-cos-arrow-head-angle-cos-alpha + arrow-head-size-sin-arrow-head-angle-sin-alpha
  ;
  ; p3-y = end-y + arrow-head-size * sin(alpha + pi + arrow-head-angle)
  ;      = end-y - arrow-head-size * sin(alpha + arrow-head-angle)
  ;      = end-y - arrow-head-size * (sin(alpha) * cos(arrow-head-angle) + cos(alpha) * sin(arrow-head-angle))
  ;      = end-y - arrow-head-size-cos-arrow-head-angle * sin-alpha - arrow-head-size-sin-arrow-head-angle * cos-alpha
  ;      = end-y - arrow-head-size-cos-arrow-head-angle-sin-alpha - arrow-head-size-sin-arrow-head-angle-cos-alpha
  
  ; dc<%> real real real real real real -> void
  ; draw one arrow
  ; The reason of the "-0.5" in the definition of start-x and end-x in the let
  ; right below is because, well, after numerous experiments done under carefully
  ; controlled conditions by a team of independent experts, it was thought to
  ; be The Right Thing for the arrows to be drawn correctly, maybe.
  (define (draw-arrow dc start-x start-y end-x end-y dx dy)
    (let ([start-x (+ start-x dx -0.5)]
          [start-y (+ start-y dy)]
          [end-x (+ end-x dx -0.5)]
          [end-y (+ end-y dy)])
      (send dc draw-line start-x start-y end-x end-y)
      (send dc draw-ellipse (- start-x arrow-root-radius) (- start-y arrow-root-radius)
            arrow-root-diameter arrow-root-diameter)
      (unless (and (= start-x end-x) (= start-y end-y))
        (let* ([offset-x (- end-x start-x)]
               [offset-y (- end-y start-y)]
               [arrow-length (sqrt (+ (* offset-x offset-x) (* offset-y offset-y)))]
               [cos-alpha (/ offset-x arrow-length)]
               [sin-alpha (/ offset-y arrow-length)]
               [arrow-head-size-cos-arrow-head-angle-cos-alpha (* arrow-head-size-cos-arrow-head-angle cos-alpha)]
               [arrow-head-size-cos-arrow-head-angle-sin-alpha (* arrow-head-size-cos-arrow-head-angle sin-alpha)]
               [arrow-head-size-sin-arrow-head-angle-cos-alpha (* arrow-head-size-sin-arrow-head-angle cos-alpha)]
               [arrow-head-size-sin-arrow-head-angle-sin-alpha (* arrow-head-size-sin-arrow-head-angle sin-alpha)]
               ; pt1 is the tip of the arrow, pt2 is the first point going clockwise from pt1
               [pt1 (make-object point% end-x end-y)]
               [pt2 (make-object point%
                      (- end-x arrow-head-size-cos-arrow-head-angle-cos-alpha arrow-head-size-sin-arrow-head-angle-sin-alpha)
                      (+ end-y (- arrow-head-size-cos-arrow-head-angle-sin-alpha) arrow-head-size-sin-arrow-head-angle-cos-alpha))]
               [pt3 (make-object point%
                      (+ end-x (- arrow-head-size-cos-arrow-head-angle-cos-alpha) arrow-head-size-sin-arrow-head-angle-sin-alpha)
                      (- end-y arrow-head-size-cos-arrow-head-angle-sin-alpha arrow-head-size-sin-arrow-head-angle-cos-alpha))])
          (send dc draw-polygon (list pt1 pt2 pt3)))))))
