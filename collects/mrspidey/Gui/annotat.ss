; annotat.ss
; Defines flow-arrow:media-edit%, type-annotation% and flow-arrow%
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

;; Constants concerning positioning
;; Global so easy to modify
       
(define arrow-start-dx   2)
(define arrow-start-dy   12)
(define arrow-end-dx     2)
(define arrow-end-dy     12)
(define arrow-delta     7)

(define jump-start-dx   -15)
(define jump-start-dy   25)
(define jump-end-dx     -25)
(define jump-end-dy     25)

(define offset-menu-exp-y  20)
(define offset-menu-snip-y 25)

(define offsets-load-menu-src-known  (cons 0 18))
(define offsets-load-menu-dest-known (cons 0 18))
(define offsets-load-menu-src-jump   (cons -25 35))
(define offsets-load-menu-dest-jump  (cons -25 35))

(define SELECT-SHOW-VALUE-SET    0)
(define SELECT-CLOSE-VALUE-SET   1)
(define SELECT-PARENTS      2)
(define SELECT-ANCESTORS    3)
(define SELECT-PATH-SOURCE  4)
(define SELECT-CHILDREN     5)
(define SELECT-DESCENDANTS  6)
(define SELECT-COPY-VALUE-SET    7)
(define SELECT-CLOSE-MENU   8)
(define SELECT-RECOMPUTE-VALUE-SET   11)

(define LOAD-NONLOCAL-FILE  9)
(define ZOOM-TO-FILE  10)

;; ------------------------------------------------------------

(define highlight
  (lambda (edit src-pos)
    ;;(pretty-debug-gui `(highlight ,src-pos))
    (let* ([src-end (send edit match-paren-forward src-pos)])
      ;;(send edit set-position src-pos src-end)
      (send (send edit get-frame) show #t)
      (send edit relocate-set-position src-pos)
      (send edit relocate-flash-on src-pos src-end)
)))

;; ------------------------------------------------------------
;; The brushes, pens for drawing arrows

(define arrow-brush.pen
  (cons
   (make-object wx:brush% (get-resource-maybe "arrow-color" "BLUE")
                wx:const-solid)
   (make-object wx:pen% (get-resource-maybe "arrow-color" "BLUE") 
                1 wx:const-solid)))

(define nosrc-arrow-brush.pen 
  (cons
   (make-object wx:brush% (get-resource-maybe "nosource-arrow-color" "RED") 
                wx:const-solid)
   (make-object wx:pen% (get-resource-maybe "nosource-arrow-color" "RED") 
                1 wx:const-solid)))

(define nodest-arrow-brush.pen
  (cons
   (make-object wx:brush% (get-resource-maybe "nodest-arrow-color" "RED") 
                wx:const-solid)  
   (make-object wx:pen% (get-resource-maybe "nodest-arrow-color" "RED") 
                1 wx:const-solid)))

;; ------------------------------------------------------------

(define flow-report-on #t)

(define (generic-flow-report msg)
  (when flow-report-on
    (wx:message-box 
      msg "Error"
      (bitwise-ior wx:const-ok wx:const-icon-exclamation))))

(define (generic-flow-report-filter msg)
  (generic-flow-report
    (format "~a~a!" 
      msg
      (if (analysis-filter-on?)
        (format " with values matching filter '~a'" (analysis-filter-on?))
        ""))))

(define (report-no-parents)
  (generic-flow-report-filter "No parents"))

(define (report-no-children)
  (generic-flow-report-filter "No children"))

(define (report-no-ancestors)
  (generic-flow-report-filter "No ancestors"))

(define (report-no-descendants)
  (generic-flow-report-filter "No descendants"))

(define (report-type-does-not-contain-filter)
  (generic-flow-report 
    (if (analysis-filter-on?)
      (format "Expression has no values matching filter '~a'!"  
        (analysis-filter-on?))
      "Expression has no values!")))

(define (report-no-path-to-source)
  (generic-flow-report 
    (if (analysis-filter-on?)
      (format "No path to source for values matching filter '~a'!"  
        (analysis-filter-on?))
      "No path to source!")))

(define lookup-ftype
  (lambda (ftype)
    (pretty-debug-gui `(lookup-ftype ,(FlowType->pretty ftype)))
    (assert (FlowType? ftype) 'lookup-ftype ftype)
    (let ([r (FlowType-type-annotation ftype)])
      (pretty-debug-gui `(lookup-ftype-returning ,r))
      (assert r 'lookup-ftype 'r= r ftype)
      r)))

;; ======================================================================
;; Edit class with flow arrows
;; sets mode to scheme mode

(define wx:const-break-special 8)

(define flow-arrow:media-edit%
  (class arrow:media-edit% (margin arg-main arg-canvas)
    (inherit 
      edit-sequence last-position change-style 
      find-wordbreak set-mode set-wordbreak-map
      relocate-change-style
      real-start-position
      last-line position-line line-start-position frame-pos->source-pos
      draw-arrows add-arrow delete-arrow
      margin-length
      get-keymap get-frame relocate-set-position
      get-start-position get-end-position)

    (public

      main
      canvas

      [list-type-annotations '()]
      [list-check-annotations '()]

      ;; ----------
      ;; dynamic allocation of jump positions

      [vec-free-jump-posns '()]
      [jump-posn-free?
        (lambda (i)
          (and 
            (>= i 0) 
            (< i (vector-length vec-free-jump-posns))
            (vector-ref vec-free-jump-posns i)))]
      [alloc-jump-posn
        ;; input and output in terms of relocate positions
        ;; not absolute positions
        (lambda (posn)
          (let ([line (position-line (real-start-position posn))])
            (unless (vector? vec-free-jump-posns)
              (set! vec-free-jump-posns (make-vector (last-line) #t)))
            (let* ([free-line
                     (recur loop ([i 0])
                       (cond
                         [(= i (vector-length vec-free-jump-posns)) 
                           (mrspidey:error "Out of jump points")]
                         [(jump-posn-free? (+ line i)) (+ line i)]
                         [(jump-posn-free? (- line i)) (- line i)]
                         [else (loop (add1 i))]))]
                    [_ (vector-set! vec-free-jump-posns free-line #f)]
                    [start (line-start-position free-line)]
                    [src (frame-pos->source-pos (+ margin-length start))])
              (pretty-debug-gui 
                `(alloc-jump-posn ,posn ,line ,free-line ,start ,src))
              (values
                src
                (lambda ()
                  (assert (not (vector-ref vec-free-jump-posns free-line)) 
                    'free-jump-posn free-line)
                  (vector-set! vec-free-jump-posns free-line #t))))))]

      ;; ----------

      [add-flow-arrow-part
        (lambda ( src-posn  src-clickback 
                  dest-posn dest-clickback
                  brush.pen )
          (pretty-debug-gui 
            `(add-flow-arrow-part ,src-posn ,dest-posn))
          (assert 
            (and 
              (or (number? src-posn) (eq? src-posn 'jump))
              (or (number? dest-posn) (eq? dest-posn 'jump))
              (or (number? src-posn) (number? dest-posn)))
            'add-flow-arrow-part
            src-posn dest-posn)
          (let*-vals
            ([(src-posn free-src start-dx start-dy)
               (if (number? src-posn)
                 (values 
                   src-posn (lambda () (void))
                   arrow-start-dx arrow-start-dy)
                 (let*-vals ([(p free) (alloc-jump-posn dest-posn)])
                   (values 
                     p free
                     jump-start-dx jump-start-dy)))]
              [(dest-posn free-dest end-dx end-dy)
                (if (number? dest-posn)
                  (values 
                    dest-posn (lambda () (void))
                    arrow-end-dx arrow-end-dy)
                  (let*-vals ([(p free) (alloc-jump-posn src-posn)])
                    (values 
                      p free
                      jump-end-dx jump-end-dy)))]
              [arrow 
                (add-arrow
                  src-posn start-dx start-dy
                  dest-posn end-dx end-dy
                  arrow-delta 
                  (car brush.pen)
                  (cdr brush.pen)
                  (dest-clickback dest-posn)
                  (src-clickback src-posn))])
            (lambda () (free-src) (free-dest) (delete-arrow arrow))))]

      ;; ----- shake it
      [shake-it
        (lambda ()
          (fluid-let ([flow-report-on #f])

            (let* ([n (random (length list-type-annotations))]
                    [ta (nth list-type-annotations n)])
              (printf "(send ta show-type-refresh)~n")
              (send ta show-type-refresh)

              (printf "(send ta copy-type-to-clipboard)~n")
              (send ta copy-type-to-clipboard)

              (printf "(send ta delete-type-refresh)~n")
              (send ta delete-type-refresh)

              (printf "(send ta parents-refresh)~n")
              (send ta parents-refresh)
              (delete-arrows)

              (printf "(send ta ancestors-refresh)~n")
              (send ta ancestors-refresh)
              (delete-arrows)

              (printf "(send ta shortest-path-source-refresh)~n")
              (send ta shortest-path-source-refresh)
              (delete-arrows)

              (printf "(send ta children-refresh)~n")
              (send ta children-refresh)
              (delete-arrows)

              (printf "(send ta descendants-refresh)~n")
              (send ta descendants-refresh)
              (delete-arrows))))]

      [send-ftype
        (lambda (ftype method . args)
          (pretty-debug-gui `(send-ftype ,(FlowType->pretty ftype)))
          (assert (symbol? method) 'send-ftype ftype method args)
          (apply (uq-ivar (lookup-ftype ftype) method) args))]

      [flush-type-cache
        (lambda ()
          (for-each 
            (lambda (obj) (send obj flush-type))
            list-type-annotations))]

      [delete-types
        (lambda ()
          (edit-sequence
            (lambda ()
              (for-each
                (lambda (obj) (send obj delete-type))
                list-type-annotations))))]

      [delete-arrows
        (lambda ()
          (send main delete-arrows))]

      [add-type-annotation
        (match-lambda
          [($ type-annotation start end-first finish ftype)
            '(pretty-debug-gui `(type-annotation 
                                  ,(zodiac:location-offset start)
                                  ,(zodiac:location-offset end-first)
                                  ,(zodiac:location-offset finish)))
            (let ([obj (make-object type-annotation% 
                         this
                         (zodiac:location-offset start)
                         (zodiac:location-offset end-first)
                         (add1 (zodiac:location-offset finish))
                         ftype)])
              (set-FlowType-type-annotation! ftype obj)
              (set! list-type-annotations (cons obj list-type-annotations)))])]

      [highlight
        (lambda (start-pos name delta)
          (cond
            [(string=? name "(")
              (let* ( [real-start (real-start-position start-pos)]
                      [rparen (mred:scheme-forward-match 
                                this real-start (last-position))])
                (change-style delta real-start (add1 real-start))
                (if (number? rparen)
                  (change-style delta (sub1 rparen) rparen)
                  (printf "Can't match paren from ~s ~s~n" start-pos rparen)))]
            [else
              (let* ([endbox (box (real-start-position start-pos))]
                      [_ (find-wordbreak '() endbox wx:const-break-special)]
                      [startbox (box (unbox endbox))]
                      [_ (find-wordbreak startbox '() wx:const-break-special)])
                '(pretty-debug-gui `(highlight start-pos ,start-pos endbox ,endbox
                                      startbox ,startbox))
                (change-style delta (unbox startbox) (unbox endbox)))]))]

      [add-check-annotation
        (match-lambda
          [(and annotation ($ check-annotation start name num))
            (set! list-check-annotations 
              (cons annotation list-check-annotations))
            (highlight (zodiac:location-offset start) name check-delta)])]

      [add-uncheck-annotation
        (match-lambda
          [($ uncheck-annotation start name)
            (highlight (zodiac:location-offset start) name uncheck-delta)])]

      [next-check-annotation
        (lambda ()
          (next-check-annotation-list
            (get-end-position) < list-check-annotations 0))]

      [prev-check-annotation
        (lambda ()
          (next-check-annotation-list 
            (get-start-position) > (reverse list-check-annotations) 
            (last-position)))]

      [next-check-annotation-list
        (lambda (start cmp list-annotations wrap)
          ;(pretty-print `(start ,start cmp ,cmp wrap ,wrap))
          (or
            (ormap
              (match-lambda
                [($ check-annotation start-ann)
                  ;(pretty-print `(loop start ,start start-ann ,(zodiac:location-offset start-ann) ,(real-start-position (zodiac:location-offset start-ann))))
                  (if (cmp start 
                        (real-start-position
                          (zodiac:location-offset start-ann)))
                    ;; after current start
                    (begin
                      (relocate-set-position
                        (zodiac:location-offset start-ann))
                      #t)
                    #f)])
              list-annotations)
            (if wrap
              (next-check-annotation-list wrap cmp list-annotations #f)
              (wx:bell))))]
      )

    (sequence
      (set! main arg-main)
      (set! canvas arg-canvas)
      (super-init margin)
      (let ([wb (make-object wx:media-wordbreak-map%)])
        (for i 0 256
             (unless (or (char-whitespace? (integer->char i))
                         (memv (integer->char i) '(#\( #\) #\;)))
               (send wb set-map i
                     (+ (send wb get-map i) wx:const-break-special))))
        (set-wordbreak-map wb))
      ;;(set-mode (make-object mred:scheme-mode%))
      (let ([keymap (get-keymap)])
        (send keymap add-key-function 
          "next-check-annotation"
          (lambda (edit event)
            (send edit next-check-annotation)))
        (send keymap map-function "tab" "next-check-annotation")

        (send keymap add-key-function 
          "prev-check-annotation"
          (lambda (edit event)
            (send edit prev-check-annotation)))
        (send keymap map-function "s:tab" "prev-check-annotation")
        (send keymap map-function "a:tab" "prev-check-annotation"))

      )))

;; ------------------------------------------------------------
;; A hyperlink object

(define cur-he #f)
(define te #f)

(define type-annotation% 
  (class null (arg-edit start-arg end-first finish-arg ftype-arg)
    (public

      edit             ;; flow-arrow:media-edit%
      start-pos 
      type-pos
      ftype

      [type #f]
      [type-snip #f]
      [src->arrows '()]  ;; Maps ftype of source to the arrow
      [dest->arrows '()] ;; ditto

      ;; ----- communication with arrows
      [add-source-arrow
       (lambda (src arrow)
         (set! src->arrows (cons (cons src arrow) src->arrows)))]
      [add-dest-arrow
       (lambda (dest arrow)
         (set! dest->arrows (cons (cons dest arrow) dest->arrows)))]

      [remove-source-arrow
       (lambda (arrow)
         (set! src->arrows 
             (filter-map
              (lambda (src.arrow)
                (if (eq? (cdr src.arrow) arrow) #f src.arrow))
              src->arrows)))]
      [remove-dest-arrow
       (lambda (arrow)
         (set! dest->arrows 
             (filter-map
              (lambda (dest.arrow)
                (if (eq? (cdr dest.arrow) arrow) #f dest.arrow))
              dest->arrows)))]
      [remove-arrows
        (for-each
          (lambda (arrow) (send arrow delete))
          (append (map cdr src->arrows) (map cdr dest->arrows)))]

      [file-visable?
        (lambda (filename)
          (send (ivar edit main) filename->frame filename))]


      ;; ----- Arrows
      [add-flow-arrow
       (lambda (src dest)
         (pretty-debug-gui
           `(type-annotation%:add-flow-arrow
              ,(FlowType->pretty src) ,(FlowType->pretty dest)))
         (send (ivar edit main) add-flow-arrow src dest))]

      ;; ------ Parents, ancestors
      [shortest-path-source-refresh
       (lambda ()
         ;; Show shortest path to a source
         (let ([path (analysis-shortest-path ftype file-visable?)])
           ;; Returns list of ancestor Tvar-nums, last element is ()
           (cond
             [(eq? path #f)
               (report-type-does-not-contain-filter)]
             [(null? path)
               (report-no-path-to-source)]
             [else
               (send edit edit-sequence
                 (lambda ()
                   (show-path path)
                   (send (ivar edit main) draw-arrows)))])))]
      [show-path
       (lambda (path)
         (unless (null? path)
           (let ([parent (car path)])
             (unless (null? parent)
               (add-flow-arrow parent ftype)
               (send edit send-ftype parent 'show-path (cdr path))))))]

      [calc-parents
       (lambda ()
         (analysis-parents ftype  file-visable?))]

      [add-parent-arrows
       (lambda ()
         (for-each (lambda (p) (add-flow-arrow p ftype)) (calc-parents)))]

      [parents-refresh
       (lambda ()
         ;; (pretty-print `(parents-refresh debugging ,debugging))
         (pretty-debug-gui `(parents-refresh ,ftype ,type-snip))
         (if (null? (calc-parents))
             (report-no-parents)
             (send edit edit-sequence
                   (lambda ()
                     (add-parent-arrows)
                     (send (ivar edit main) draw-arrows)))))]

      [ancestors-refresh
       (lambda ()
         (let ([arrows (analysis-ancestors ftype file-visable?)])
           (if (null? arrows)
             (report-no-ancestors)
             (send edit edit-sequence
               (lambda ()
                 (for-each 
                   (lambda (arrow) (apply add-flow-arrow arrow))
                   arrows)
                 (send (ivar edit main) draw-arrows))))))]

      ;; ------ Children, descendants

      [calc-children
       (lambda () (analysis-children ftype file-visable?))]

      [add-child-arrows
       (lambda ()
         (for-each (lambda (p) (add-flow-arrow ftype p)) (calc-children)))]

      [children-refresh
       (lambda ()
         (pretty-debug-gui `(children ,ftype ,type-snip))
         (if (null? (calc-children))
             (report-no-children)
             (send edit edit-sequence
                   (lambda ()
                     (add-child-arrows)
                     (send (ivar edit main) draw-arrows)))))]

      [descendants-refresh
       (lambda ()
         (let ([arrows (analysis-descendants ftype file-visable?)])
           (if (null? arrows)
             (report-no-descendants)
             (send edit edit-sequence
               (lambda ()
                 (for-each 
                   (lambda (arrow) (apply add-flow-arrow arrow))
                   arrows)
                 (send (ivar edit main) draw-arrows))))))]

      ;; ------ Type stuff

      [flush-type (lambda () (set! type #f))]
      [calc-type
       (lambda ()
         (unless type
             (assert (FlowType? ftype) 'type-annotation% ftype)
             (set! type (analysis-callback ftype)))
         type)]
     
      ;; ------ This snip stuff
      [delete-type-refresh
       (lambda ()
         (when type-snip
           (send edit edit-sequence delete-type)))]
      [delete-type
       (lambda ()
         (when type-snip
           (send edit relocate-delete-snip type-pos)
           (set! type-snip #f)

           ))]
      [arrow-zoom (lambda () (highlight))]
      [current-focus (lambda () (highlight))]
      [highlight
       (lambda () 
         (set! te edit)
         (let* ( [frame (send edit get-frame)]
                 [frame (send (ivar edit main) filename->frame
                          (send edit get-filename))])
           (assert (is-a? frame wx:frame%) 'highlight frame)
           (send frame show #t)
           (send edit relocate-set-position start-pos)
           (send edit relocate-flash-on start-pos type-pos #f #t 50000)))]

      [get-start-pos  (lambda () start-pos)]
      [get-type-pos (lambda () type-pos)]

      [move-before-snip
       (lambda ()
         (pretty-debug-gui `(move-before-snip))
         (send edit relocate-set-position start-pos)
         (send edit set-caret-owner '()))]

      [show-type
       (lambda ()
         (unless type-snip
             
           (let* ([type (calc-type)]
                   [_ (set! type-snip (make-object mred:media-snip%))]
                   [snip-edit (make-object mred:searching-edit%)]
                   [_ (send type-snip set-media snip-edit)])

             (dynamic-wind
              (lambda ()
                (send snip-edit begin-edit-sequence))
              (lambda ()
                ;; put type in the box
                (send snip-edit insert type) 
                ;; delete newline
                (send snip-edit delete)
                ;; set style
                (send snip-edit change-style base-delta 
                      0 (string-length type))
                (send snip-edit change-style type-delta 0 (string-length type))
                ;; add clickback
                (send snip-edit set-clickback 
                      0 (string-length type)
                      (lambda ignore
                        (popup-type/arrow-menu type-pos offset-menu-snip-y))
                      '()
                      #t))
              (lambda ()
                (send snip-edit end-edit-sequence)
                (send snip-edit lock #t))))
                        
           (send edit edit-sequence
                 (lambda ()
                   (send edit relocate-insert-snip type-snip type-pos)
                   (send edit relocate-change-style normal-delta 
                         type-pos (add1 type-pos))))))]

      [show-type-refresh
       (lambda ()
         (send edit edit-sequence
               (lambda ()
                 (show-type)
                 ;;(highlight)
                 )))]

      [copy-type-to-clipboard
       (lambda ()
         (let* ([snip-edit (send type-snip get-this-media)])
           (send snip-edit copy #f 0 0 (send snip-edit last-position))))]

      ;; ---------- Making the popup menus
      [popup-type/arrow-menu
        (lambda (where offset-menu)
          ;; where is source text position for menu
          (let ([menu (make-object wx:menu% '() menu-callback)]
                 [xb (box 0)]
                 [yb (box 0)])
            (if type-snip
              (send menu append
                SELECT-CLOSE-VALUE-SET
                "Close Value Set"
                "Close this value set invariant box")
              (send menu append
                SELECT-SHOW-VALUE-SET "Show Value Set"
                "Show the expressions value set invariant"))
            (send* menu
              (append-separator)
              (append SELECT-PARENTS "Parents"
                "Show the immediate sources of this type")
              (append SELECT-ANCESTORS "Ancestors"
                "Show all sources of this type")
              (append SELECT-PATH-SOURCE "Path to Source"
                "Show shortest path to a source constructor expression")
              (append-separator)
              (append SELECT-CHILDREN "Children"
                "Show the immediate expressions to which this type may flow")
              (append SELECT-DESCENDANTS "Descendants"
                "Show all expressions to which this type may flow")
              (append-separator))
            (when type-snip
              (send menu append
                SELECT-COPY-VALUE-SET
                "Copy Value Set" 
                "Copy the value set invariant to the clipboard")
              (send menu append
                SELECT-RECOMPUTE-VALUE-SET
                "Recompute Value Set" 
                "Recompute the expressions value set invariant"))
            (send menu append SELECT-CLOSE-MENU "Close Menu")    
       
            ;; Get global location of expr
            (send edit position-location 
              (send edit real-start-position where)
              xb yb)
            (pretty-debug-gui 
              `(source-pos ,where 
                 frame-pos ,(send edit real-start-position where)))

            (pretty-debug-gui `(position-location returns ,xb ,yb))
            ;; Convert to local location
            (send edit local-to-global xb yb)
            (pretty-debug-gui `(local-to-global returns ,xb ,yb))
            (send (ivar edit canvas) popup-menu menu 
              (unbox xb) (+ (unbox yb) offset-menu))))]

      [menu-callback 
        (lambda (menu command)
          (set! cur-he this)
          (move-before-snip)
          (let ([arg (send command get-command-int)])
            (pretty-debug-gui `(command ,arg))
            (wrap-busy-cursor
              (lambda ()
                (cond 
                  [(= arg SELECT-SHOW-VALUE-SET)   (show-type-refresh)]
                  [(= arg SELECT-CLOSE-VALUE-SET)  (delete-type-refresh)]
                  [(= arg SELECT-RECOMPUTE-VALUE-SET)
                    (delete-type-refresh)
                    (show-type-refresh)]
                  [(= arg SELECT-COPY-VALUE-SET)   (copy-type-to-clipboard)]
                  [(= arg SELECT-PARENTS)     (parents-refresh)]
                  [(= arg SELECT-ANCESTORS)   (ancestors-refresh)]
                  [(= arg SELECT-PATH-SOURCE) (shortest-path-source-refresh)]
                  [(= arg SELECT-CHILDREN)    (children-refresh)]
                  [(= arg SELECT-DESCENDANTS) (descendants-refresh)]
                  [(= arg SELECT-CLOSE-MENU)  (void)])))))]
      )

    (sequence

      (unless (FlowType? ftype-arg)
        (pretty-debug-gui `(bad-ftype ,ftype-arg ,(FlowType? ftype-arg))))
      (assert (FlowType? ftype-arg) 'type-annotation-init1 ftype-arg)
      (set! edit arg-edit)
      
      (set! start-pos start-arg)
      (set! type-pos finish-arg)
      (set! ftype ftype-arg)
      (assert (FlowType? ftype) 'type-annotation-init ftype ftype-arg)

      (send edit relocate-change-style type-link-delta start-arg end-first)
      (send edit relocate-set-clickback start-arg end-first 
            (lambda ignore
              (popup-type/arrow-menu start-arg offset-menu-exp-y))
            '()
            #t))))

;; ------------------------------------------------------------

(define last-arrow-popup (void))

(define flow-arrow%
  (class null (arg-main arg-src arg-dest)
    (public

      src                               ; points to type-annotation% object
      dest     
      main
      delete-arrow-thunks 

      ;; Deletion
      [delete-local
        (lambda ()
          ;; tell endpoints we're dead
          (let ( [src-ta  (lookup-ftype src)]
                 [dest-ta (lookup-ftype dest)])
            (when (is-a? src-ta type-annotation%)
              (send src-ta remove-dest-arrow this))
            (when (is-a? dest-ta type-annotation%)
              (send dest-ta remove-source-arrow this)))
          ;; delete
          (assert (list? delete-arrow-thunks) 1 delete-arrow-thunks)
          (for-each (lambda (th) (th)) delete-arrow-thunks)
          )]
      [delete
        (lambda () 
          (send main delete-arrow this))]
      [delete-refresh
        (lambda ()
          (pretty-debug-gui '(delete-refresh))
          (delete)
          (send main draw-arrows))]

      ;; ----------

      [update
        ;; Change if necy due to loaded file
        (lambda ()
          (let ( [src-ta  (lookup-ftype src)]
                 [dest-ta (lookup-ftype dest)])
            (pretty-debug-gui `(flow-arrow%:update ,src-ta ,dest-ta))
            (when (and 
                    (string? src-ta)
                    (send main filename->frame src-ta))
              ;; file loaded, so delete this arrow + show parents of dest
              (delete)
              (send dest-ta add-parent-arrows))
            (when (and 
                    (string? dest-ta)
                    (send main filename->frame dest-ta))
              ;; file loaded, so delete this arrow + show children of src
              (delete)
              (send src-ta add-child-arrows))))]

      ;; ----------

      [popup-load-menu
        (lambda (edit offsets where direction filename)
          ;; direction is "parent" or "child"
          (assert (not (send main filename->edit filename)))
          (let* ( [menu-callback 
                    (lambda (menu command)
                      (wrap-busy-cursor
                        (lambda ()
                          (send main add-frame 
                            (send main filename->fileinfo filename)
                            #t))))]
                  [menu (make-object wx:menu% '() menu-callback)]
                  [xb (box 0)]
                  [yb (box 0)])
            (send menu append
              LOAD-NONLOCAL-FILE
              (format "Load ~s containing ~a"
                (file-name-from-path filename) direction))
       
            ;; Get global location of expr
            (send edit position-location 
              (send edit real-start-position where)
              xb yb)
            (send edit local-to-global xb yb)
            (pretty-debug-gui `(local-to-global returns ,xb ,yb))

            (send (ivar edit canvas) popup-menu menu 
              (+ (unbox xb) (car offsets))
              (+ (unbox yb) (cdr offsets)))))]

      [popup-zoom-menu
        (lambda (edit offsets where to direction)

          ;; direction is "parent" or "child"
          (let* ( [menu-callback 
                    (lambda (menu command)
                      (wrap-busy-cursor
                        (lambda ()
                          (send to arrow-zoom))))]
                  [menu (make-object wx:menu% '() menu-callback)]
                  [xb (box 0)]
                  [yb (box 0)])
            (set! last-arrow-popup
              (lambda () 
                (send this popup-zoom-menu edit offsets where to direction)))
            (send menu append
              ZOOM-TO-FILE
              (format "Zoom to ~a in ~s" 
                direction
                (file-name-from-path
                  (send (ivar to edit) get-filename))))
       
            ;; Get global location of expr
            (send edit position-location 
              (send edit real-start-position where)
              xb yb)
            (send edit local-to-global xb yb)
            (pretty-debug-gui `(local-to-global returns ,xb ,yb))

            (send (ivar edit canvas) popup-menu menu 
              (+ (unbox xb) (car offsets))
              (+ (unbox yb) (cdr offsets)))))]
      )

    (sequence
      (assert (is-a? arg-main MrSpidey%))
      (assert (FlowType? arg-src) arg-src)
      (assert (FlowType? arg-dest) arg-dest)
      (pretty-debug-gui 
        `(flow-arrow% ,arg-main 
           ,(FlowType->pretty arg-src) ,(FlowType->pretty arg-dest)))
      (set! main arg-main)
      (set! src arg-src)
      (set! dest arg-dest)

      ;; 4 cases for arrow
      ;; local - simple case
      ;; source-half - just show arrow coming in from edge
      ;; sink-half - just show arrow going to edge
      ;; both-halves - show incoming and outgoing arrows in resp edits

      (let* ( [src-ta  (lookup-ftype src)]
              [dest-ta (lookup-ftype dest)])

        (when (or 
                (and 
                  (is-a? src-ta type-annotation%)
                  (assq dest-ta (ivar src-ta dest->arrows)))
                (and 
                  (is-a? dest-ta type-annotation%)
                  (assq src-ta (ivar dest-ta src->arrows))))

          ;; Arrow already there
          (raise (make-exn:flow-arrow-exists)))

        ;; Not already there - first tell endpoints we're here
        (when (is-a? src-ta type-annotation%)
          (send src-ta  add-dest-arrow   dest-ta this))
        (when (is-a? dest-ta type-annotation%)
          (send dest-ta add-source-arrow src-ta  this))

        (pretty-debug-gui
          `(src-ta ,src-ta
             dest-ta ,dest-ta
             srctadest->arrows 
             ,(and 
                (is-a? src-ta type-annotation%)
                (map car (ivar src-ta dest->arrows)))
             desttasrc->arrows 
             ,(and 
                (is-a? dest-ta type-annotation%)
                (map car (ivar dest-ta src->arrows)))))            
          
        (let* 
          (
            [ta->edit-or-false
              (lambda (ta)
                (cond
                  [(string? ta) 
                    ;; If ftype is from .za file,
                    ;; corresponding edit should not exist yet
                    (assert (not (send main filename->edit ta)))
                    #f]
                  [(is-a? ta type-annotation%)
                    ;; Is a type-annotation
                    (ivar ta edit)]))]
            [src-edit  (ta->edit-or-false src-ta)]
            [dest-edit (ta->edit-or-false dest-ta)])
          
          (set! delete-arrow-thunks
            (cond
              [(and src-edit (not dest-edit))
                ;; sink-half arrow
                (let* ( [clickback-where
                          (lambda (offsets)
                            (lambda (where)
                              (lambda (event) 
                                (cond
                                  [(send event button-down? 1)
                                    (popup-load-menu
                                      src-edit offsets where 
                                      "child" dest-ta)]
                                  [(send event button-down? 3) 
                                    (delete-refresh)
                                    #t]
                                  [else #f]))))])
                  (list
                    (send src-edit add-flow-arrow-part
                      (send src-ta get-start-pos) 
                      (clickback-where offsets-load-menu-src-known)
                      'jump 
                      (clickback-where offsets-load-menu-dest-jump)
                      nodest-arrow-brush.pen)))]

              [(and dest-edit (not src-edit))
                ;; sink-half arrow
                (let* ( [clickback-where
                          (lambda (offsets)
                            (lambda (where)
                              (lambda (event) 
                                (cond
                                  [(send event button-down? 1)
                                    (popup-load-menu
                                      dest-edit offsets where
                                      "parent" src-ta)]
                                  [(send event button-down? 3) 
                                    (delete-refresh)
                                    #t]
                                  [else #f]))))])
                  (list
                    (send dest-edit add-flow-arrow-part
                      'jump 
                      (clickback-where offsets-load-menu-src-jump)
                      (send dest-ta get-start-pos) 
                      (clickback-where offsets-load-menu-dest-known)
                      nosrc-arrow-brush.pen)))]

              [(and src-edit dest-edit)
                ;; local or non-local arrow
                (let* ( [clickback-zoom
                          (lambda (to)
                            (lambda (posn)
                              (lambda (event) 
                                (cond
                                  [(send event button-down? 1)
                                    (send to arrow-zoom)]
                                  [(send event button-down? 3) 
                                    (delete-refresh)
                                    #t]
                                  [else #f]))))])

                  (cond
                    [(eq? src-edit dest-edit)
                      ;; local arrow
                      (list 
                        (send src-edit add-flow-arrow-part
                          (send src-ta get-start-pos)
                          (clickback-zoom dest-ta)
                          (send dest-ta get-start-pos)
                          (clickback-zoom src-ta)
                          arrow-brush.pen))]

                    [else
                      ;; non-local arrow
                      (let* 
                        ( [src-posn (send src-ta get-start-pos)]
                          [dest-posn (send dest-ta get-start-pos)]
                          [clickback-where
                            (lambda (edit offsets to direction)
                              (lambda (where)
                                (lambda (event) 
                                  (cond
                                    [(send event button-down? 1)
                                      (popup-zoom-menu
                                        edit offsets where to direction)]
                                    [(send event button-down? 3) 
                                      (delete-refresh)
                                      #t]
                                    [else #f]))))])
                        (list
                          (send src-edit add-flow-arrow-part
                            src-posn
                            (clickback-where 
                              src-edit 
                              offsets-load-menu-src-known
                              dest-ta "child")
                            'jump
                            (clickback-where 
                              src-edit 
                              offsets-load-menu-dest-jump
                              dest-ta "child")
                            arrow-brush.pen)
                          (send dest-edit add-flow-arrow-part
                            'jump
                            (clickback-where 
                              dest-edit 
                              offsets-load-menu-src-jump
                              src-ta "parent")
                            dest-posn 
                            (clickback-where 
                              dest-edit 
                              offsets-load-menu-dest-known
                              src-ta "parent")
                            arrow-brush.pen)))]))]
              [else
                (assert #f 'flow-arrow% src-edit dest-edit)]
              ))
          (assert (list? delete-arrow-thunks) 2 delete-arrow-thunks)          
          )))))

;; ======================================================================


