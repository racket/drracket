; arrow.ss
; defines arrow:media-edit%, an extention of graphics:media-edit% with arrows
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------

(define arrow:media-edit%
  (let* ([pi (* 2 (asin 1))]
         [arrow-head-angle (/ pi 8)]
         [cos-angle (cos arrow-head-angle)]
         [sin-angle (sin arrow-head-angle)]
         [arrow-head-size 10]
         [arrow-root-radius 3.5]
         [cursor-arrow (make-object wx:cursor% wx:const-cursor-arrow)])

    (class-asi graphics:media-edit%
      (inherit delete-graphic draw-graphics add-graphic set-cursor)
      (public
        [delete-arrow (lambda (arrow) (delete-graphic arrow))]
        [draw-arrows  (lambda () (draw-graphics))]
        [add-arrow
         (lambda (start-pos start-dx start-dy 
                   end-pos end-dx end-dy
                   delta brush pen
                   clickback-head clickback-root)
           (pretty-debug-gui (list 'add-arrow 
                               start-pos start-dx start-dy
                               end-pos end-dx end-dy
                               delta clickback-head clickback-root))
           (add-graphic 
            (list start-pos end-pos)
            (match-lambda
             [((start-x . start-y) (end-x . end-y))
               (pretty-debug-gui
                 `(locs ,start-x ,start-y ,end-x ,end-y
                    ,start-dx ,start-dy ,end-dx ,end-dy))
              (let* 
                  ([start-x (+ start-x start-dx)]
                   [start-y (+ start-y start-dy)]
                   [end-x   (+ end-x end-dx)]
                   [end-y   (+ end-y end-dy)]
                   [ofs-x   (- start-x end-x)]
                   [ofs-y   (- start-y end-y)]
                   [len     (sqrt (+ (* ofs-x ofs-x) (* ofs-y ofs-y)))]
                   [ofs-x   (/ ofs-x len)]
                   [ofs-y   (/ ofs-y len)]
                   [head-x  (* ofs-x arrow-head-size)]
                   [head-y  (* ofs-y arrow-head-size)]
                   [end-x   (+ end-x (* ofs-x delta))]
                   [end-y   (+ end-y (* ofs-y delta))]
                   [pt1     (make-object wx:point% end-x end-y)]
                   [pt2     (make-object 
                             wx:point%
                             (+ end-x (* cos-angle head-x) 
                                (* sin-angle head-y))
                             (+ end-y (- (* sin-angle head-x))
                                (* cos-angle head-y)))]
                   [pt3     (make-object 
                             wx:point%
                             (+ end-x (* cos-angle head-x)
                                (- (* sin-angle head-y)))
                             (+ end-y (* sin-angle head-x)
                                (* cos-angle head-y)))]
                   [pts (list pt1 pt2 pt3)]
                   [draw-fn
                    (lambda (dc dx dy)
                      '(pretty-debug-gui
                        (list 'draw-line (+ start-x dx) (+ start-y dy)
                              (+ end-x dx) (+ end-y dy)))
                      (let ([old-brush (send dc get-brush)]
                            [old-pen   (send dc get-pen)]
                            [old-logfn (send dc get-logical-function)])
                        (send dc set-brush brush)
                        (send dc set-pen pen)
                        ;; (send dc set-logical-function wx:const-or)
                        (send dc draw-line
                              (+ start-x dx) (+ start-y dy)
                              (+ end-x dx) (+ end-y dy))
                        (send dc draw-polygon pts dx dy)
                        (send dc draw-ellipse 
                              (- (+ start-x dx) arrow-root-radius)
                              (- (+ start-y dy) arrow-root-radius)
                              (* 2 arrow-root-radius)
                              (* 2 arrow-root-radius))
                        (send dc set-brush old-brush)
                        (send dc set-pen old-pen)                    
                        (send dc set-logical-function old-logfn)))]
                   [on-head?
                    (lambda (x y)
                      (let*
                          ([xs (map (lambda (pt) (send pt get-x)) pts)]
                           [ys (map (lambda (pt) (send pt get-y)) pts)]
                           [min-x (apply min xs)]
                           [min-y (apply min ys)]
                           [max-x (apply max xs)]
                           [max-y (apply max ys)])
                        (and (>= x min-x)
                             (<= x max-x)
                             (>= y min-y)
                             (<= y max-y))))]
                   [on-root?
                    (lambda (x y)
                      (and (>= x (- start-x arrow-root-radius))
                           (<= x (+ start-x arrow-root-radius))
                           (>= y (- start-y arrow-root-radius))
                           (<= y (+ start-y arrow-root-radius))))]
                   [event-fn
                    (lambda (event x y)
                      (cond
                       [(on-head? x y)
                        (set-cursor cursor-arrow)
                        (clickback-head event)]
                       [(on-root? x y)
                        (set-cursor cursor-arrow)
                        (clickback-root event)]
                       [else 
                         ;; Back to default cursor
                         ;; (set-cursor '())
                        #f]))])

                ;; Return draw-thunk and event function
                (cons draw-fn event-fn))])))]))))

;; ----------------------------------------
