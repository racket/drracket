; graphics.ss
; Defines graphics:media-edit%, a subclass of spidey:dynamic+margin-edit%
; with facilities for graphics on top of text
;
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

(define-structure (graphic pos* locs->thunks draw-fn click-fn))

(define graphics:media-edit%
  (class spidey:dynamic+margin-edit% args
    (inherit set-cursor)
    (rename
      [super-after-insert after-insert]
      [super-after-delete after-delete]
      [super-on-paint on-paint]
      [super-on-event on-event]
      [super-resized resized]
      [super-size-cache-invalid size-cache-invalid])

    (public
      [graphics-list ()]
      [pos->locs
       (lambda (pos)
         (let* ([xb (box 0)]
                [yb (box 0)]
                [real-pos (send this real-start-position pos)] )
           (send this position-location real-pos xb yb #t)
           (pretty-debug-gui 
             `(pos->locs ,pos ,real-pos ,(unbox xb) ,(unbox yb)))
           (cons (unbox xb) (unbox yb))))]
      [calc-graphic-thunks!
       (lambda (graphic)
         (match-let*
             ([locs (map pos->locs (graphic-pos* graphic))]
              [locs->thunks (graphic-locs->thunks graphic)]
              [(draw-fn . click-fn) (locs->thunks locs)])
           (set-graphic-draw-fn! graphic draw-fn)
           (set-graphic-click-fn! graphic click-fn)))]
      [recalc-graphics
       (lambda ()
         '(pretty-debug-gui `(recalc-graphics ,@graphics-list))
         (for-each calc-graphic-thunks! graphics-list))]
      [add-graphic
       (lambda (pos* locs->thunks)
         (pretty-debug-gui (list 'add-graphic pos* locs->thunks))
         (let ([graphic (make-graphic pos* locs->thunks 0 0)])
           (calc-graphic-thunks! graphic)
           (set! graphics-list (cons graphic graphics-list))
           graphic))]
      [delete-graphic
       (lambda (graphic)
         '(pretty-debug-gui `(delete-graphic ,graphic))
         (set! graphics-list (remv graphic graphics-list)))]
      [draw-graphics
       (lambda ()
         (let ([admin (send this get-admin)])
           (pretty-debug-gui `(draw-graphics))
           (send this invalidate-bitmap-cache)

           ;; Kludge to get redrawing right
           ;;(send super before-insert 0 1)
           ;;(send super after-insert 0 1)

           '(unless (null? admin)
              (send admin needs-update 0 0 100000 100000))
           '(pretty-debug-gui `(draw-graphics-done))))]

      ;; overwritten methods
      [resized
       (lambda (snip redraw-now)
         '(pretty-debug-gui `(resized ,snip ,redraw-now))
         (super-resized snip redraw-now)
         (recalc-graphics)
         (when redraw-now (draw-graphics)))]

      [after-delete
       (lambda (start len)
         '(pretty-debug-gui `(after-delete ,start ,len))
         (super-after-delete start len)
         (recalc-graphics)
         (draw-graphics))]

      [after-insert
       (lambda (start len)
         '(pretty-debug-gui `(after-insert ,start ,len))
         (super-after-insert start len)
         (recalc-graphics)
         (draw-graphics))]
      [size-cache-invalid
        (lambda ()
          (super-size-cache-invalid)
          (recalc-graphics))]          

      [on-paint
       (lambda (before dc left top right bottom dx dy draw-caret)
         (super-on-paint
          before dc left top right bottom 
          dx dy draw-caret)
         (unless before 
             '(pretty-debug-gui `(on-paint))
           (for-each
            (match-lambda
             [($ graphic pos* locs->thunks draw-fn click-fn)
              '(pretty-debug-gui (list 'on-paint-graphic pos*))
              (draw-fn dc dx dy)])
            graphics-list)))]

      [on-event
       (lambda (event)
         (set-cursor '())
         (let* ([admin (send this get-admin)]
                [root-x (box 0)]
                [root-y (box 0)])
           (send admin get-dc root-x root-y)
           (let ([actual-x (+ (send event get-x) (unbox root-x))]
                 [actual-y (+ (send event get-y) (unbox root-y))])

             ;; Now try to find a clickback to handle it
             (let loop ([graphics graphics-list])
               (match graphics
                 [() (super-on-event event)]
                 [(($ graphic _ _ _ click-fn) . rest-graphics) 
                  (or (click-fn event actual-x actual-y)
                      ;; Otherwise try next graphic
                      (loop rest-graphics))])))))]
      ) 
    (sequence
      ;;(pretty-debug-gui `(init graphic:media-edit% ,@init))
      (apply super-init args)
      )))

;; ----------------------------------------
