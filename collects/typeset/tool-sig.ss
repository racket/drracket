(define-signature typeset:utils-input^
  (typeset-size))

(define-signature typeset:utils^
  (single-bracket
   double-bracket
   tb-align
   greek
   drawing
   ellipses

   ;(struct size (width height descent space left right))
   ;(struct pos (x y))
   position
   sup sub
   postscript

   typeset-size
   
   arrow b-arrow g-arrow bg-arrow checked-arrow blank-arrow)) ;; these should move out

(require-library "invoke.ss")