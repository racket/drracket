
;; documentation moved to doc.txt

(define-signature texpict^
  ((struct pict (draw width height ascent descent children))
   (struct child (pict dx dy))

   read-in-sizes  ; string -> void

   using-pict2e-package

   draw-bezier-lines

   output-measure-commands

   tex-series-prefix
   serialize-tex-picts
   
   current-tex-sizer

   black-and-white

   find-lt  ; (left & top)  ; pict pict-path -> dx dy
   find-lc  ; (left & vertical center)
   find-lb  ; (left & bottom)
   find-ltl ; (left and top baseline)
   find-lbl ; (left and bottom baseline)
   find-ct  ; (horizontal center & top)
   find-cc
   find-cb
   find-ctl
   find-cbl
   find-rt
   find-rc
   find-rb
   find-rtl
   find-rbl

   launder  ; pict -> pict

   blank        ; -> pict
                ; w h -> pict
                ; w h d -> pict

   tex               ; string -> pict
   text-line         ; string -> pict
   text-line/phantom ; string string -> pict
   tex-paragraph     ; w string ['top|'bottom] -> pict

   left-brace     ; h -> pict
   right-brace    ; h -> pict
   left-delimit   ; str h -> pict
   right-delimit  ; str h -> pict
   middle-delimit ; str h -> pict
   top-brace      ; w -> pict
   bottom-brace   ; w -> pict

   clip-descent   ; pict -> pict
   inset          ; pict i -> pict
                  ; pict hi vi -> pict
                  ; pict l t r b -> pict

   hline        ; w h -> pict
   dash-hline   ; w h seg-length -> pict ; default seg-length is 5
   vline        ; w h -> pict
   dash-vline   ; w h seg-length -> pict ; default seg-length is 5

   frame        ; pict -> pict
   dash-frame   ; pict seg-length -> pict ; default seg-length is 5
   oval         ; pict -> pict
   oval/radius  ; pict r -> pict ; r is radius of corners

   big-circle   ; diameter -> pict

   thick       ; pict -> pict
   thin        ; pict -> pict

   ghost        ; pict -> pict

   record       ; pict pict ... -> pict

   vl-append    ; d pict ... -> pict ; d units between each picture
   vc-append
   vr-append
   ht-append
   hc-append
   hb-append
   htl-append       ; align bottoms of ascents
   hbl-append       ; align tops of descents (normal text alignment)

   lt-superimpose ; pict ... -> pict
   lb-superimpose
   lc-superimpose
   ltl-superimpose
   lbl-superimpose
   rt-superimpose
   rb-superimpose
   rc-superimpose
   rtl-superimpose
   rbl-superimpose
   ct-superimpose
   cb-superimpose
   cc-superimpose
   ctl-superimpose
   cbl-superimpose

   table ; ncols pict-list col-aligns row-aligns col-seps row-seps -> pict

   colorize ; pict color-string -> pict

   picture      ; w h command-list -> pict

   cons-picture ; pict command-list -> pict

   prog-picture ; (dx dy -> void) -> pict

   pict->string

   pict->commands
   
   use-old-connect))
