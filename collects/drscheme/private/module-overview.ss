(module module-overview mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
	   (lib "file.ss")
           (lib "list.ss")
           (lib "moddep.ss" "syntax")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants")
           "drsig.ss"
           (lib "unitsig.ss"))
  
  (provide module-overview@)
  (define module-overview@
    (unit/sig drscheme:module-overview^
      (import)
      
      (define filename-constant (string-constant module-browser-filename-constant))
      (define font-size-gauge-label (string-constant module-browser-font-size-gauge-label))
      (define progress-label (string-constant module-browser-progress-label))
      (define adding-file (string-constant module-browser-adding-file))
      (define laying-out-graph-label (string-constant module-browser-laying-out-graph-label))
      (define open-file-format (string-constant module-browser-open-file-format))
      
      (preferences:set-default 'drscheme:module-overview:label-font-size 12 number?)
      (preferences:set-default 'drscheme:module-overview:window-height 500 number?)
      (preferences:set-default 'drscheme:module-overview:window-width 500 number?)
      
      (define (set-box/f b v) (when (box? b) (set-box! b v)))
      
      (define (module-overview parent)
        (let ([filename (get-file #f parent)])
          (when filename
            (module-overview/file filename parent))))
      
      (define (module-overview/file filename parent)
        
        (define (get-snip-hspace) (* 2 label-font-size))
        (define snip-vspace 2)
        (define snip-height #f)
        
        (define level-ht (make-hash-table))
        
        (define lines<%>
          (interface ()
            get-children 
            add-child 
            get-level
            set-level))
        
        (define snipclass (make-object snip-class%))
        
        (define (snip/lines-mixin %)
          (class* % (lines<%>)
            (field (children null))
            (define/public (get-children) children)
            (define/public (add-child child) (set! children (cons child children)))
            (define/public (remove-child child) 
              (set! children (remq child children))
              (set! syntax-children (remq child syntax-children)))
            
            (field (parents null))
            (define/public (get-parents) parents)
            (define/public (add-parent parent) (set! parents (cons parent parents)))
            (define/public (remove-parent parent) 
              (set! parents (remq parent parents))
              (set! syntax-parents (remq parent syntax-parents)))
            
            (field (syntax-children null))
            (define/public (is-a-syntax-child? child) (memq child syntax-children))
            (define/public (add-syntax-child child) 
              (set! syntax-children (cons child syntax-children)))
            
            (field (syntax-parents null))
            (define/public (is-a-syntax-parent? parent) (memq parent syntax-parents))
            (define/public (add-syntax-parent parent) 
              (set! syntax-parents (cons parent syntax-parents)))
            
            
            (field (level #f))
            (define/public (get-level) level)
            (define/public (set-level _l) 
              (when level
                (hash-table-put! level-ht level
                                 (remq this (hash-table-get level-ht level))))
              (set! level _l)
              (hash-table-put! level-ht level 
                               (cons this (hash-table-get level-ht level (lambda () null)))))
            
            (super-instantiate ())
            
            (inherit set-snipclass)
            (set-snipclass snipclass)))
        
        (define (blank-snip-mixin %)
          (class %
            (rename [super-get-extent get-extent])
            (define/override (get-extent dc x y w h descent space lspace rspace)
              (super-get-extent dc x y w h descent space lspace rspace)
              (set-box/f h 1))
            (super-instantiate ())))
        
        (define (boxed-word-snip-mixin %)
          (class %
            (init-field word filename)
            
            (define/public (get-filename) filename)
            (define/public (get-word) word)
            
            (field (snip-width 0)
                   (snip-height 0))
            
            (define/override (get-extent dc x y wb hb descent space lspace rspace)
              (let ([old-font (send dc get-font)])
                (send dc set-font label-font)
                (let-values ([(w h a d) (send dc get-text-extent word)])
                  (set! snip-width (+ w 4))
                  (set! snip-height (+ h 4))
                  (set-box/f wb snip-width)
                  (set-box/f hb snip-height)
                  (set-box/f descent 0)
                  (set-box/f space 0)
                  (set-box/f lspace 0)
                  (set-box/f rspace 0))
                (send dc set-font old-font)))
            (define/override (draw dc x y left top right bottom dx dy draw-caret)
              (let ([old-font (send dc get-font)])
                (send dc set-font label-font)
                (when (and (or (<= left x right)
                               (<= left (+ x snip-width) right))
                           (or (<= top y bottom)
                               (<= top (+ y snip-height) bottom)))
                  (send dc draw-rectangle x y snip-width snip-height)
                  (send dc draw-text word (+ x 2) (+ y 2)))
                (send dc set-font old-font)))
            (super-instantiate ())))
        
        (define snip/lines% (blank-snip-mixin (snip/lines-mixin snip%)))
        (define word-snip/lines% (boxed-word-snip-mixin (snip/lines-mixin snip%)))
        
        (define dark-syntax-pen (send the-pen-list find-or-create-pen "darkorchid" 2 'solid))
        (define dark-pen (send the-pen-list find-or-create-pen "blue" 2 'solid))
        (define light-syntax-pen (send the-pen-list find-or-create-pen "plum" 1 'solid))
        (define light-pen (send the-pen-list find-or-create-pen "light blue" 1 'solid))
        
        (define draw-lines-pasteboard%
          (class pasteboard:basic%
            (inherit find-first-snip find-next-selected-snip)
            
         (define/override (after-interactive-move event)
           (let loop ([snip (find-next-selected-snip #f)])
             (when snip
               (when (is-a? snip lines<%>)
                 (for-each
                  (lambda (child)
                    (invalidate-between snip child))
                  (send snip get-children))
                 (for-each
                  (lambda (parent)
                    (invalidate-between snip parent))
                  (send snip get-parents)))
               (loop (find-next-selected-snip snip)))))
         
         (define (invalidate-between from to)
           (let-values ([(xf yf wf hf) (get-position from)]
                        [(xt yt wt ht) (get-position to)])
             (invalidate-bitmap-cache (min xf xt)
                                      (min yf yt)
                                      (max (+ xf wf) (+ xt wt))
                                      (max (+ yf hf) (+ yt ht)))))
            
            (inherit dc-location-to-editor-location get-canvas)
            (rename [super-on-event on-event])
            (field (currently-over #f))
            (define/override (on-event evt)
              (cond
                [(send evt button-down? 'right)
                 (let ([ex (send evt get-x)]
                       [ey (send evt get-y)])
                   (let-values ([(x y) (dc-location-to-editor-location ex ey)])
                     (let ([snip (find-snip-under-mouse x y)])
                       (when snip
                         (let* ([right-button-menu (make-object popup-menu%)]
                                [open-file-item (instantiate menu-item% ()
                                                  (label 
                                                   (format open-file-format
                                                           (send snip get-word)))
                                                  (parent right-button-menu)
                                                  (callback
                                                   (lambda (x y)
                                                     (handler:edit-file
                                                      (send snip get-filename)))))])
                           (send (get-canvas) popup-menu right-button-menu
                                 (+ (send evt get-x) 1)
                                 (+ (send evt get-y) 1)))))))]
                [(send evt leaving?)
                 (change-currently-over #f)
                 (super-on-event evt)]
                [(send evt dragging?)
                 (let loop ([snip (find-next-selected-snip #f)])
                   (when snip
                     (when (is-a? snip lines<%>)
                       (for-each
                        (lambda (child)
                          (invalidate-between snip child))
                        (send snip get-children))
                       (for-each
                        (lambda (parent)
                          (invalidate-between snip parent))
                        (send snip get-parents)))
                     (loop (find-next-selected-snip snip))))
                 (super-on-event evt)]
                [(or (send evt entering?)
                     (send evt moving?))
                 (let ([ex (send evt get-x)]
                       [ey (send evt get-y)])
                   (let-values ([(x y) (dc-location-to-editor-location ex ey)])
                     (let ([snip-under-mouse (find-snip-under-mouse x y)])
                       (change-currently-over snip-under-mouse))))
                 (super-on-event evt)]
                [else 
                 (super-on-event evt)]))
            
            ;; find-snip-under-mouse : num num -> (union #f word-snip/lines%)
            (define (find-snip-under-mouse x y)
              (let loop ([snip (find-first-snip)])
                (cond
                  [snip
                   (let-values ([(sx sy sw sh) (get-position snip)])
                     (if (and (<= sx x (+ sx sw))
                              (<= sy y (+ sy sh))
                              (is-a? snip word-snip/lines%))
                         snip
                         (loop (send snip next))))]
                  [else #f])))
            
            (define (change-currently-over new-currently-over)
              (unless (eq? new-currently-over currently-over)
                (let ([old-currently-over currently-over])
                  (set! currently-over new-currently-over)
                  
                  (when label-message
                    (send label-message set-label
                          (if currently-over
                              (string-append 
                               filename-constant
                               (send currently-over get-filename))
                              "")))
                  
                  (when old-currently-over
                    (invalidate-to-children/parents old-currently-over))
                  (when currently-over
                    (invalidate-to-children/parents currently-over)))))
            
            ;; invalidate-to-children/parents : snip -> void
            ;; invalidates the region containing this snip and
            ;; all of its children and parents.
            (define (invalidate-to-children/parents snip)
              (let ([children (get-all-children snip)]
                    [parents (get-all-parents snip)])
                (let-values ([(fx fy fw fh) (get-position snip)])
                  (let loop ([snips (append children parents)]
                             [l fx]
                             [t fy]
                             [r (+ fx fw)]
                             [b (+ fy fh)])
                    (cond
                      [(null? snips) (invalidate-bitmap-cache l t (- r l) (- b t))]
                      [else
                       (let-values ([(sx sy sw sh) (get-position (car snips))])
                         (loop (cdr snips)
                               (min l sx)
                               (min t sy)
                               (max r (+ sx sw))
                               (max b (+ sy sh))))])))))
            
            (rename [super-on-paint on-paint])
            (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
              (when before?
                (let ([old-pen (send dc get-pen)])
                  
                  (let loop ([snip (find-first-snip)])
                    (when snip
                      (when (is-a? snip lines<%>)
                        (for-each (lambda (child) 
                                    (draw-connection 
                                     dc dx dy snip child #f
                                     (send snip is-a-syntax-child? child)
                                     left top right bottom))
                                  (send snip get-children)))
                      (loop (send snip next))))
                  
                  (when currently-over
                    (for-each
                     (lambda (child)
                       (let loop ([parent currently-over]
                                  [child child])
                         (draw-connection dc dx dy parent child #t
                                          (send currently-over is-a-syntax-child? child)
                                          left top right bottom)
                         (when (is-a? child snip/lines%)
                           (loop child (car (send child get-children))))))
                     (send currently-over get-children))
                    (for-each
                     (lambda (parent)
                       (let loop ([child currently-over]
                                  [parent parent])
                         (draw-connection dc dx dy child parent #t
                                          (send currently-over is-a-syntax-parent? parent)
                                          left top right bottom)
                         (when (is-a? parent snip/lines%)
                           (loop parent (car (send parent get-parents))))))
                     (send currently-over get-parents)))
                  
                  (send dc set-pen old-pen)))
              (super-on-paint before? dc left top right bottom dx dy draw-caret))
         
         (inherit invalidate-bitmap-cache)
         (define/private (draw-connection dc dx dy from to dark-lines? syntax-child?
                                          left top right bottom)
           (let-values ([(xf yf wf hf) (get-position from)]
                        [(xt yt wt ht) (get-position to)])
             (let ([x1 (+ xf (quotient wf 2))]
                   [y1 (+ yf (quotient hf 2))]
                   [x2 (+ xt (quotient wt 2))]
                   [y2 (+ yt (quotient ht 2))])
               
               (unless (or (and (x1 . <= . left)
                                (x2 . <= . left))
                           (and (x1 . >= . right)
                                (x2 . >= . right))
                           (and (y1 . <= . top)
                                (y2 . <= . top))
                           (and (y1 . >= . bottom)
                                (y2 . >= . bottom)))
                 
                 (send dc set-pen
                       (if dark-lines?
                           (if syntax-child? dark-syntax-pen dark-pen)
                           (if syntax-child? light-syntax-pen light-pen)))
                 
                 (send dc draw-line (+ dx x1) (+ dy y1) (+ dx x2) (+ dy y2))))))
            
            (define (should-hilite? snip)
              (let ([check-one-way
                     (lambda (way)
                       (let loop ([snip snip])
                         (or (eq? currently-over snip)
                             (and (is-a? snip snip/lines%)
                                  (loop (car (way snip)))))))])
                (or (check-one-way (lambda (snip) (send snip get-children)))
                    (check-one-way (lambda (snip) (send snip get-parents))))))
            
            (inherit get-snip-location)
            (field [lb (box 0)]
                   [tb (box 0)]
                   [rb (box 0)]
                   [bb (box 0)])
            (define/private (get-position snip)
              (get-snip-location snip lb tb #f)
              (get-snip-location snip rb bb #t)
              (values (unbox lb)
                      (unbox tb)
                      (- (unbox rb) (unbox lb))
                      (- (unbox bb) (unbox tb))))
            
            (super-instantiate ())))
        
        ;; get-all-relatives : (snip -> (listof snip)) snip -> (listof snip)
        ;; returns all editor-snip relatives (of a particular sort), including
        ;; any regular snip relatives along the way.
        (define (get-all-relatives get-relatives snip)
          (let loop ([flat-relatives (get-relatives snip)]
                     [relatives null])
            (cond
              [(null? flat-relatives) relatives]
              [else
               (let i-loop ([dummy (car flat-relatives)]
                            [acc relatives])
                 (cond
                   [(is-a? dummy word-snip/lines%)
                    (loop (cdr flat-relatives) (cons dummy acc))]
                   [else
                    (i-loop (car (get-relatives dummy))
                            (cons dummy acc))]))])))
        
        ;; get-all-children : snip -> (listof snip)
        (define (get-all-children snip)
          (get-all-relatives (lambda (snip) (send snip get-children)) snip))
        
        ;; get-all-parents : snip -> (listof snip)
        (define (get-all-parents snip)
          (get-all-relatives (lambda (snip) (send snip get-parents)) snip))
        
        
        ;; add-connection : string string -> void
        (define (add-connection filename-original filename-require for-syntax?)
          (let* ([original-key (string->symbol filename-original)]
                 [require-key (string->symbol filename-require)]
                 [original-snip (find/create-snip (filename->label filename-original)
                                                  filename-original
                                                  original-key)]
                 [require-snip (find/create-snip (filename->label filename-require)
                                                 filename-require
                                                 require-key)]
                 [original-level (send original-snip get-level)]
                 [require-level (send require-snip get-level)])
            (send original-snip add-child require-snip)
            (send require-snip add-parent original-snip)
            (when for-syntax?
              (send original-snip add-syntax-child require-snip)
              (send require-snip add-syntax-parent original-snip))
            (if (send original-snip get-level)
                (fix-snip-level require-snip (+ original-level 1))
                (fix-snip-level original-snip 0))))
        
        ;; fix-snip-level : snip number -> void
        ;; moves the snip (and any children) to at least `new-level'
        ;; doesn't move them if they are already past that level
        (define (fix-snip-level snip new-min-level)
          (let loop ([snip snip]
                     [new-min-level new-min-level])
            (let ([current-level (send snip get-level)])
              (when (or (not current-level)
                        (new-min-level . > . current-level))
                (send snip set-level new-min-level)
                (for-each
                 (lambda (child) (loop child (+ new-min-level 1)))
                 (send snip get-children))))))
        
        ;; get-snip-width : snip -> number
        ;; exracts the width of a snip
        (define (get-snip-width snip)
          (let ([lb (box 0)]
                [rb (box 0)])
            (send pasteboard get-snip-location snip lb #f #f)
            (send pasteboard get-snip-location snip rb #f #t)
         (- (unbox rb)
            (unbox lb))))
     
     ;; get-snip-height : snip -> number
     ;; exracts the width of a snip
     (define (get-snip-height snip)
       (let ([tb (box 0)]
             [bb (box 0)])
         (send pasteboard get-snip-location snip #f tb #f)
         (send pasteboard get-snip-location snip #f bb #t)
         (- (unbox bb)
            (unbox tb))))
     
     ;; find/create-snip : string string[filename] symbol -> snip
     ;; snip-table : hash-table[sym -o> snip]
     ;; finds the snip with this key, or creates a new
     ;; ones. For the same key, always returns the same snip.
     ;; uses snip-table as a cache for this purpose.
     (define snip-table (make-hash-table))
     (define (find/create-snip label filename key)
       (hash-table-get
        snip-table
        key
        (lambda () 
          (let* ([snip (instantiate word-snip/lines% ()
                         (word (format "~a" label))
                         (filename filename))])
            (send pasteboard insert snip)
            (hash-table-put! snip-table key snip)
            snip))))
     
     ;; filename->label : string -> string
     ;; constructs a label for the little boxes in terms
     ;; of the filename.
        (define re:no-ext (regexp "^(.*)\\.[^.]*$"))
     (define (filename->label filename)
       (let-values ([(base name dir?) (split-path filename)])
         (let ([m (regexp-match re:no-ext name)])
           (if m
               (cadr m)
               name))))
     
     ;; add-blank-snips : -> void
     (define (add-blank-snips)
       (let loop ([snip (send pasteboard find-first-snip)])
         (cond
           [(not snip) (void)]
           [else
            (let c-loop ([children (send snip get-children)])
              (cond
                [(empty? children) (void)]
                [else
                 (let ([child (car children)])
                   (cond
                     [(= (send child get-level) (+ (send snip get-level) 1))
                      (void)]
                     [else
                      (let* ([n (- (send child get-level)
                                   (send snip get-level)
                                   1)]
                             [syntax-child? (send snip is-a-syntax-child? child)]
                             ;; n must be 1 or bigger at this point
                             [blank-snips (build-blank-snips 
                                           n
                                           (+ (send snip get-level) 1)
                                           child
                                           syntax-child?)])
                        (send snip add-child (car blank-snips))
                        (send (car blank-snips) add-parent snip)
                        (when syntax-child?
                          (send snip add-syntax-child (car blank-snips))
                          (send (car blank-snips) add-syntax-parent snip))
                        (send snip remove-child (car children))
                        (send (car children) remove-parent snip))]))
                 (c-loop (cdr children))]))
            (loop (send snip next))])))
     
     ;; merge-blank-snips
     (define (merge-blank-snips)
       (hash-table-for-each
        level-ht
        (lambda (n snips)
          (let* ([blank (filter (lambda (x) (is-a? x snip/lines%)) snips)]
                 [words (filter (lambda (x) (is-a? x word-snip/lines%)) snips)]
                 [blank-count (length blank)]
                 [word-count (length words)]
                 [per-gap (quotient blank-count word-count)]
              [extra (modulo blank-count word-count)]
              [merged
               (let loop ([words words]
                          [blank blank]
                          [extra extra])
                 (cond
                   [(null? words) blank]
                   [else
                    (let ([this-time (if (zero? extra)
                                         per-gap
                                         (+ per-gap 1))])
                      (append (list (car words))
                              (first-n this-time blank)
                              (loop (cdr words)
                                    (take-n this-time blank)
                                    (if (zero? extra) 0 (- extra 1)))))]))])
            (hash-table-put! level-ht n merged)))))
     
     (define (first-n n l)
       (cond
         [(zero? n) null]
         [(empty? l) (error 'first-n "not enough")]
         [else (cons (car l) (first-n (- n 1) (cdr l)))]))
     
     (define (take-n n l)
       (cond
         [(zero? n) l]
         [(empty? l) (error 'take-n "not enough")]
         [else (take-n (- n 1) (cdr l))]))
     
     ;; builds a chain of `n' blank snips
     (define (build-blank-snips n level orig-child syntax-child?)
       (let loop ([n n]
                  [level level])
         (cond
           [(zero? n) 
            (send orig-child set-level level)
            null]
           [else
            (let ([new-snip (make-object snip/lines%)]
                  [rest (loop (- n 1) (+ level 1))])
              (send pasteboard insert new-snip)
              (send new-snip set-level level)
              (if (null? rest)
                  (begin (send new-snip add-child orig-child)
                         (send orig-child add-parent new-snip)
                         (when syntax-child?
                           (send new-snip add-syntax-child orig-child)
                           (send orig-child add-syntax-parent new-snip)))
                  (begin (send new-snip add-child (car rest))
                         (send (car rest) add-parent new-snip)
                         (when syntax-child?
                           (send new-snip add-syntax-child (car rest))
                           (send (car rest) add-syntax-parent new-snip))))
              (cons new-snip rest))])))
     
     ;; render-snips : -> void
     (define (render-snips)
       (let ([for-each-level
              (lambda (f)
                (hash-table-for-each
                 level-ht
                 f))]
             [max-h 0])
         
         (for-each-level
          (lambda (n v)
            (set! max-h (max max-h (apply + (map get-snip-height v))))))
         
         (let ([levels (quicksort (hash-table-map level-ht list)
                                  (lambda (x y) (<= (car x) (car y))))])
           (let loop ([levels levels]
                      [x 0])
             (cond
               [(null? levels) (void)]
               [else
                (let* ([level (car levels)]
                       [n (car level)]
                       [this-level-snips (cadr level)]
                       [this-h (apply + (map get-snip-height this-level-snips))]
                       [this-w (apply max (map get-snip-width  this-level-snips))])
                  (let loop ([snips this-level-snips]
                             [y (/ (- max-h this-h) 2)])
                    (unless (null? snips)
                      (let ([snip (car snips)])
                        (send pasteboard move-to 
                              snip
                              (+ x
                                 (floor
                                  (- (/ this-w 2) 
                                     (/ (get-snip-width snip) 2)))) 
                              y)
                        (loop (cdr snips)
                              (+ y snip-vspace (get-snip-height snip))))))
                  (loop (cdr levels)
                        (+ x (get-snip-hspace) this-w)))])))))
     
     ;; add-connections : string[filename] -> void
     ;; recursively adds a connections from this file and
     ;; all files it requires
     (define (add-connections filename)
       (define visited-hash-table (make-hash-table))
       (define progress-frame (parameterize ([current-eventspace (make-eventspace)])
                                (instantiate frame% ()
                                  (parent parent)
                                  (label progress-label)
                                  (width 600))))
       (define progress-message (instantiate message% ()
                                  (label "")
                                  (stretchable-width #t)
                                  (parent progress-frame)))
       (define sema (make-semaphore 1))
       (define done? #f)
       (thread
        (lambda ()
          (sleep 3)
          (semaphore-wait sema)
          (unless done?
            (send progress-frame show #t))
          (semaphore-post sema)))
       (send pasteboard begin-edit-sequence)
       (let loop ([filename (normalize-path filename)])
         (let ([visited-key (string->symbol filename)])
           (unless (hash-table-get visited-hash-table visited-key (lambda () #f))
             (send progress-message set-label (format adding-file filename))
             (hash-table-put! visited-hash-table visited-key #t)
             (let-values ([(base name dir?) (split-path filename)])
               (let ([module-code (get-module-code filename)])
                 (let-values ([(imports fs-imports) (module-compiled-imports module-code)])
                   (let ([requires (extract-filenames imports filename)]
                         [syntax-requires (extract-filenames fs-imports filename)])
                     (for-each (lambda (require) 
                                 (add-connection filename require #f)
                                 (loop require))
                               requires)
                     (for-each (lambda (syntax-require)
                                 (add-connection filename syntax-require #t)
                                 (loop syntax-require))
                               syntax-requires))))))))
       (send progress-message set-label laying-out-graph-label) 
       (render-snips)
       (semaphore-wait sema)
       (set! done? #t)
       (send progress-frame show #f)
       (semaphore-post sema)
       (send pasteboard end-edit-sequence))
     
     ;; extract-filenames : (listof (union symbol module-path-index)) string[directory] ->
     ;;                     (listof string[filename))
     (define (extract-filenames direct-requires base)
       (let loop ([direct-requires direct-requires])
         (cond
           [(null? direct-requires) null]
           [else (let ([dr (car direct-requires)])
                   (if (module-path-index? dr)
                       (cons (normal-case-path (normalize-path (resolve-module-path-index dr base)))
                             (loop (cdr direct-requires)))
                       (loop (cdr direct-requires))))])))
     
     (define label-font-size (preferences:get 'drscheme:module-overview:label-font-size))
     (define label-font
       (send the-font-list find-or-create-font label-font-size 'decorative 'normal 'normal  #f))
     
     (define font-label-size-callback-running? #f)
     (define (set-label-font-size)
       (unless font-label-size-callback-running?
         (set! font-label-size-callback-running? #t)
         (queue-callback
          (lambda ()
            (let ([new-font-size (send font-size-gauge get-value)])
              (set! label-font-size new-font-size)
              (preferences:set 'drscheme:module-overview:label-font-size 
                               new-font-size)
              (set! label-font
                    (send the-font-list find-or-create-font 
                          new-font-size 'decorative 'normal 'normal #f))
              (send pasteboard begin-edit-sequence)
              (let loop ([snip (send pasteboard find-first-snip)])
                (when snip
                  (let ([admin (send snip get-admin)])
                    (when admin
                      (send admin resized snip #t)))
                  (loop (send snip next))))
              (render-snips)
              (send pasteboard end-edit-sequence)
              (set! font-label-size-callback-running? #f))))))
     
     (define size-recording-frame%
       (class frame:basic%
         (rename [super-on-size on-size])
         (define/override (on-size w h)
           (preferences:set 'drscheme:module-overview:window-width w)
           (preferences:set 'drscheme:module-overview:window-height h)
           (super-on-size w h))
         (super-instantiate ())))

     (define frame (instantiate size-recording-frame% ()
                     (label (string-constant module-browser))
                     (width (preferences:get 'drscheme:module-overview:window-width))
                     (height (preferences:get 'drscheme:module-overview:window-height))
                     (alignment '(left center))))
     (define pasteboard (make-object draw-lines-pasteboard%))
     (define label-message (instantiate message% ()
                             (label "")
                             (parent (send frame get-area-container))
                             (stretchable-width #t)))
     
     (define font-size-gauge
       (instantiate slider% ()
         (label font-size-gauge-label)
         (min-value 1)
         (max-value 72)
         (init-value label-font-size)
         (parent (send frame get-area-container))
         (callback
          (lambda (x y)
            (set-label-font-size)))))

     (define ec (make-object canvas:basic% (send frame get-area-container) pasteboard))
     
     (add-connections filename)
     
     (send frame show #t)))))
