; progress.ss - not used
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

(define progress-box%
  (class mred:frame% (title arg-bar-names . top-left-loc)

    (inherit show set-size)

    (public
     num-bars
     bar-names
     bar-fractions
     the-canvas 
     [top-margin   12]
     [bot-margin   24]
     [right-margin 20]
     [left-margin  10]
     [middle-margin 20]
     [bar-length  150]
     [line-spacing 8]
     [barname-width  58]
     [barname-height 12]
     [bar-margin     2]
     [solid-brush (make-object wx:brush% "DARKGREEN" wx:const-solid)]
     [trans-brush (make-object wx:brush% "DARKGREEN" wx:const-transparent)]
     [border-pen  (make-object wx:pen% "BLACK" 1 wx:const-solid)]
     [in-pen      (make-object wx:pen% "DARKGREEN" 1 wx:const-solid)]

     [mark-progress
      (lambda (bar-num fraction)
        (vector-set! bar-fractions bar-num fraction)
        (send the-canvas on-paint))]
     )

    (sequence
      
      (super-init '() title -1 -1 200 200)

      (set! num-bars (length arg-bar-names))
      (set! bar-names (list->vector arg-bar-names))
      (set! bar-fractions (make-vector num-bars 0))

      (set! the-canvas
          (make-object 
           (class wx:canvas% args
             (inherit get-dc)
             (public
               [on-paint
                (lambda ()
                  (let ([the-dc (send the-canvas get-dc)])
                    ;; Draw the text
                    (for i 0 num-bars
                         '(pretty-print
                           `(send the-dc draw-text
                                  ,(vector-ref bar-names i)
                                  ,left-margin
                                  ,(+ top-margin 
                                      (* (+ barname-height line-spacing) i))))
                         (send the-dc draw-text
                               (vector-ref bar-names i)
                               left-margin
                               (+ top-margin
                                  (* (+ barname-height line-spacing) i))))

                    ; Draw the bar borders
                    (send the-dc set-pen border-pen)
                    (send the-dc set-brush trans-brush)
                    (for i 0 num-bars
                         '(pretty-print `(send the-dc draw-rectangle
                                              ,(+ left-margin barname-width middle-margin)
                                              ,(+ top-margin 
                                                  (* (+ barname-height line-spacing) i))
                                              ,(+ (* bar-length 1) (* 2 bar-margin))
                                              ,barname-height))
                         (send the-dc draw-rectangle
                               (+ left-margin barname-width middle-margin)
                               (+ top-margin 
                                  (* (+ barname-height line-spacing) i))
                               (+ (* bar-length 1) (* 2 bar-margin))
                               barname-height))

                    ;; Draw the bars
                    (send the-dc set-pen in-pen)
                    (send the-dc set-brush solid-brush)
                    (for i 0 num-bars
                         (unless (zero? (vector-ref bar-fractions i))
                           '(pretty-print `(send the-dc draw-rectangle
                                 ,(+ left-margin barname-width middle-margin
                                    bar-margin)
                                 ,(+ top-margin 
                                    (* (+ barname-height line-spacing) i)
                                    bar-margin)
                                 ,(vector-ref bar-fractions i)
                                 ,(* bar-length (vector-ref bar-fractions i))
                                 ,(- barname-height (* 2 bar-margin))))
                           (send the-dc draw-rectangle
                                 (+ left-margin barname-width middle-margin
                                    bar-margin)
                                 (+ top-margin 
                                    (* (+ barname-height line-spacing) i)
                                    bar-margin)
                                 (* bar-length (vector-ref bar-fractions i))
                                 (- barname-height (* 2 bar-margin)))))
                    )
                  (wx:flush-display))])

             (sequence
               (apply super-init args)
               (let ([the-dc (get-dc)])
                 (send the-dc set-font
                       (make-object wx:font% 12 wx:const-modern 
                                    wx:const-normal wx:const-normal #f))
                 )))

             this))

      '(let* ([sizes 
               (map 
                (lambda (name)
                  (let ([wb (box 0)]
                        [hb (box 0)])
                    (printf ".") (flush-output)
                    (send (send the-canvas get-dc) get-text-extent name wb hb)
                    (printf ".") (flush-output)
                    (cons (unbox wb) (unbox hb))))
                arg-bar-names)])

         (pretty-print `(sizes ,sizes))
         (set! barname-width  (apply max (map car sizes)))
         (set! barname-height (apply max (map cdr sizes))))
        
      ;; Set the frame size + position
      (let ([w (+ left-margin barname-width middle-margin 
                  bar-length (* 2 bar-margin) right-margin)]
            [h (+ top-margin
                  (* (+ barname-height line-spacing) num-bars)
                  bot-margin)])
        (match top-left-loc
          [(x y) (set-size (- x w) y w h)]
          [() (void)])
      (set-size w h))
      ;; We're ready
      (show #t)
      (wx:flush-display)
      (wx:yield)
      )))
         
'(begin
  (define p (make-object progress-box% "title" '("bar-name-1" "bar-name-2")))
  (send p mark-progress 0 0.3)
  (send p mark-progress 1 0.6)
  )

