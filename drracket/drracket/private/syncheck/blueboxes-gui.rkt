#lang racket/base
(require browser/external
         data/interval-map
         drracket/private/rectangle-intersect
         framework
         framework/private/coroutine
         framework/private/logging-timer
         images/icons/misc
         net/url
         racket/class
         racket/gui/base
         racket/match
         racket/math
         racket/runtime-path
         racket/set
         scribble/blueboxes
         scribble/xref
         setup/collects
         setup/xref
         string-constants)
(provide docs-text-defs-mixin
         docs-text-ints-mixin
         docs-editor-canvas-mixin
         syncheck:add-docs-range
         syncheck:add-require-candidate
         syncheck:reset-docs-im
         syncheck:update-blue-boxes
         disable-blue-boxes)

(define sc-f2-to-lock (string-constant sc-f2-to-un/lock))
(define sc-read-more... (string-constant sc-read-more...))

(preferences:set-default 'drracket:syncheck:contracts-locked? #f boolean?)
(preferences:set-default 'drracket:syncheck:show-blueboxes? #t boolean?)

(define corner-radius 48)
(define bow-blue-box-color (make-object color% 232 240 252))
(define (get-blue-box-color) (if (preferences:get 'framework:white-on-black?)
                                 (send the-color-database find-color "navy") ;; MidnightBlue
                                 bow-blue-box-color))
(define bow-var-color (make-object color% 68 68 68))
(define wob-var-color
  (let ([g (send bow-var-color red)])
    (make-object color% (- 255 g) (- 255 g) (- 255 g))))
(define blue-box-label-text-color (make-object color% 128 128 128))
(define blue-box-margin 5)

(define box-gradient-stop-color-bow (make-object color% 252 252 252))
(define box-gradient-stop-color-wob (make-object color% 8 8 8))
(define (make-blue-box-gradient-pen x y w h)
  (define stops (list (list 0 (get-blue-box-color))
                      (list 1 (if (preferences:get 'framework:white-on-black?)
                                  box-gradient-stop-color-wob
                                  box-gradient-stop-color-bow))))
  (make-object brush% "black" 'solid #f
    (new linear-gradient%
         [x0 x] [y0 (+ y h)]
         [x1 (+ x w)] [y1 y]
         [stops stops])))

;; the multiplication by 1.5 is suspicious, but it makes things
;; look right under mac os x (with fairly standard font settings)
(define (get-label-font sl)
  (and (send sl find-named-style "Standard")
       (send (send sl find-named-style "Standard") get-font)))

(define (get-read-more-font sl)
  (define style (send sl find-named-style "Standard"))
  (define font-size (if style
                        (send (send style get-font) get-point-size)
                        12))
  (send the-font-list find-or-create-font
        font-size 'roman 'italic 'normal))
(define (get-read-more-underline-font sl)
  (define style (send sl find-named-style "Standard"))
  (define font-size (if style
                        (send (send style get-font) get-point-size)
                        12))
  (send the-font-list find-or-create-font
        font-size 'roman 'italic 'normal #t))

(define-local-member-name
  get-show-docs?
  get-current-strs
  syncheck:reset-docs-im
  syncheck:add-docs-range
  syncheck:add-require-candidate
  syncheck:update-blue-boxes
  get-docs-im
  get/start-docs-im
  get-require-candidates
  get-path->pkg-cache
  set-original-info-text
  add-linked
  update-the-strs
  disable-blue-boxes)

(define docs-ec-clipping-region #f)
(define docs-ec-last-cw #f)
(define docs-ec-last-ch #f)
(define docs-ec-last-hi #f)
(define docs-ec-last-vi #f)

(define (docs-editor-canvas-mixin %)
  (class %
    (inherit get-dc get-client-size get-editor
             horizontal-inset vertical-inset)
    (define/override (on-paint)
      (when (preferences:get 'drracket:syncheck:show-blueboxes?)
        (define e (get-editor))
        (when e
          (define dc (get-dc))
          (define-values (cw ch) (get-client-size))
          (define the-strs (send e get-current-strs))
          (cond
            [(and (send e get-show-docs?) the-strs)
             (define hi (horizontal-inset))
             (define vi (vertical-inset))
             (define font (send dc get-font))
             (define pen (send dc get-pen))
             (define brush (send dc get-brush))
             (define smoothing (send dc get-smoothing))
             (define std (send (send e get-style-list) find-named-style "Standard"))
             (when std (send dc set-font (send std get-font)))
             (define-values (box-width box-height label-overlap?)
               (get-blue-box-size dc (send e get-style-list) the-strs))
             (send dc set-pen "black" 1 'transparent)
             (let ([rect-x (- cw box-width)]
                   [rect-y 0])
               (send dc set-brush (make-blue-box-gradient-pen rect-x rect-y box-width box-height))
               (send dc draw-rectangle rect-x rect-y box-width box-height))
             (send dc set-smoothing 'aligned)
             
             ;; most of the time (unless the user is
             ;; resizing the window) we don't really 
             ;; need a new clipping region, so just
             ;; make a cache of size 1, keyed by the
             ;; client width, height, and vertical and 
             ;; horizontal insets.
             (unless (and (equal? cw docs-ec-last-cw)
                          (equal? ch docs-ec-last-ch)
                          (equal? hi docs-ec-last-hi)
                          (equal? vi docs-ec-last-vi))
               (set! docs-ec-last-cw cw)
               (set! docs-ec-last-ch ch)
               (set! docs-ec-last-hi hi)
               (set! docs-ec-last-vi vi)
               (define rgn1 (new region%))
               (define rgn2 (new region%))
               (define rgn3 (new region%))
               (define rgn4 (new region%))
               (send rgn1 set-rectangle 0 0 cw vi)
               (send rgn2 set-rectangle (- cw hi) 0 hi ch)
               (send rgn3 set-rectangle 0 (- ch vi) cw vi)
               (send rgn4 set-rectangle 0 0 hi ch)
               (send rgn1 union rgn2)
               (send rgn1 union rgn3)
               (send rgn1 union rgn4)
               (set! docs-ec-clipping-region rgn1))
             
             (define old-region (send dc get-clipping-region))
             (send dc set-clipping-region docs-ec-clipping-region)
             (draw-blue-box-shadow dc (- cw box-width) 0 box-width box-height)
             
             (send dc set-clipping-region old-region)
             (send dc set-pen pen)
             (send dc set-brush brush)
             (send dc set-font font)
             (send dc set-smoothing smoothing)]
            [the-strs
             (draw-closed dc cw 0)])))
      (super on-paint))
    (super-new)))

(define docs-text-info<%>
  (interface ()
    get-docs-im
    get/start-docs-im
    get-require-candidates
    get-path->pkg-cache
    update-the-strs
    disable-blue-boxes
    toggle-syncheck-docs))

(define docs-text-gui-mixin
  (mixin (color:text<%> docs-text-info<%>) ()
    (inherit get-canvas get-admin get-style-list
             dc-location-to-editor-location get-dc
             get-start-position get-end-position
             begin-edit-sequence end-edit-sequence
             invalidate-bitmap-cache get-text
             is-stopped? is-frozen? is-lexer-valid?
             classify-position get-token-range
             get/start-docs-im get-docs-im get-require-candidates get-path->pkg-cache)
    
    (define locked? (preferences:get 'drracket:syncheck:contracts-locked?))
    (define mouse-in-blue-box? #f)
    (define mouse-in-lock-icon? #f)
    (define mouse-in-read-more? #f)
    
    (define the-strs #f)
    (define/public (get-current-strs) the-strs)
    
    
    (define visit-docs-path #f)
    (define visit-docs-tag #f)
    (define/private (visit-docs-url)
      (when visit-docs-path
        (define url (path->url visit-docs-path))
        (define url2 (if visit-docs-tag
                         (make-url (url-scheme url)
                                   (url-user url)
                                   (url-host url)
                                   (url-port url)
                                   (url-path-absolute? url)
                                   (url-path url)
                                   (url-query url)
                                   visit-docs-tag)
                         url))
        (send-url (url->string url2))))
    
    (define/public (get-show-docs?) (and the-strs (or locked? mouse-in-blue-box?)))
    (define/augment (toggle-syncheck-docs)
      (begin-edit-sequence #t #f)
      (invalidate-blue-box-region)
      (cond
        [locked?
         (set! mouse-in-blue-box? #f)]
        [else
         (update-the-strs)])
      (update-locked (not locked?))
      (when last-evt-seen
        (update-mouse-in-blue-box (in-blue-box? last-evt-seen))
        (define-values (is-in-lock? is-in-read-more?) (in-lock/in-read-more? last-evt-seen))
        (update-mouse-in-lock-icon/read-more? is-in-lock? is-in-read-more?))
      (invalidate-blue-box-region)
      (end-edit-sequence))
    (define/public (update-mouse-in-blue-box b)
      (unless (equal? b mouse-in-blue-box?)
        (begin-edit-sequence #t #f)
        (invalidate-blue-box-region)
        (set! mouse-in-blue-box? b)
        (invalidate-blue-box-region)
        (trigger-buffer-changed-callback #:now? #t)
        (end-edit-sequence)))
    (define/public (update-locked b)
      (preferences:set 'drracket:syncheck:contracts-locked? b)
      (unless (equal? b locked?)
        (begin-edit-sequence #t #f)
        (invalidate-blue-box-region)
        (set! locked? b)
        (invalidate-blue-box-region)
        (end-edit-sequence)))
    (define/public (update-mouse-in-lock-icon/read-more? lk? rm?)
      (unless (and (equal? lk? mouse-in-lock-icon?)
                   (equal? rm? mouse-in-read-more?))
        (begin-edit-sequence #t #f)
        (invalidate-blue-box-region)
        (set! mouse-in-lock-icon? lk?)
        (set! mouse-in-read-more? rm?)
        (invalidate-blue-box-region)
        (end-edit-sequence)))
    
    (define/private (invalidate-blue-box-region)
      (define c (get-canvas))
      (when c (send c refresh))
      ;; the code below is what I'd like to do here,
      ;; but this doesn't redraw the margin (the part
      ;; of the editor-canvas that is always outside
      ;; of th editor) and it doesn't seem possible to
      ;; trigger a redraw of that part without also
      ;; triggering a redraw of the entire editor
      ;; so we just do that instead (above)

      #;
      (begin
        (define drawn-region (currently-draw-bluebox-region))
        (when drawn-region
          (invalidate-bitmap-cache (list-ref drawn-region 0)
                                   (list-ref drawn-region 1)
                                   (list-ref drawn-region 2)
                                   (list-ref drawn-region 3)))))

    (define/private (currently-drawn-bluebox-region)
      (define-values (br bt _1 _2) (get-box-upper-right-and-lock-coordinates))
      (cond
        [(and bt br)
         (cond
           [(get-show-docs?)
            (define-values (box-width box-height label-overlap?)
              (get-blue-box-size (get-dc) (get-style-list) the-strs))
            (define x (- br box-width shadow-size))
            (list (max x 0) (max bt 0)
                  (+ box-width shadow-size)
                  (+ box-height shadow-size))]
           [the-strs
            (define size (+ corner-radius shadow-size))
            (list (max 0 (- br size)) (max 0 bt) size size)]
           [else #f])]
        [else #f]))

    (define to-invalidate #f)
    (define/override (on-scroll-to)
      (super on-scroll-to)
      (set! to-invalidate (currently-drawn-bluebox-region)))
    (define/override (after-scroll-to)
      (super after-scroll-to)
      (define (maybe-invalidate)
        (when to-invalidate
          (invalidate-bitmap-cache
           (list-ref to-invalidate 0)
           (list-ref to-invalidate 1)
           (list-ref to-invalidate 2)
           (list-ref to-invalidate 3))
          (set! to-invalidate #f)))
      (maybe-invalidate)
      (set! to-invalidate (currently-drawn-bluebox-region))
      (maybe-invalidate))
    
    (define pref-changed-callback (λ (x y) (invalidate-blue-box-region)))
    (preferences:add-callback
     'drracket:syncheck:show-blueboxes?
     pref-changed-callback
     #t)

    (define bx (box 0))
    (define by (box 0))
    (define bw (box 0))
    (define bh (box 0))
    

    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (when (preferences:get 'drracket:syncheck:show-blueboxes?)
        (define-values (br bt bmp-x bmp-y) (get-box-upper-right-and-lock-coordinates))
        (when (and (not before?) br bt)
          (define canvas (get-canvas))
          (define hi (send canvas horizontal-inset))
          (define vi (send canvas vertical-inset))
          (cond
            [(get-show-docs?)
             (draw-open dc dx dy 
                        (get-style-list)
                        the-strs
                        br bt bmp-x bmp-y
                        mouse-in-blue-box?
                        mouse-in-lock-icon?
                        mouse-in-read-more?
                        locked?
                        
                        left top right bottom)]
            [the-strs
             (draw-closed dc (+ dx br) (+ dy bt))]))))
    
    ;; get-box-upper-right-and-lock-coordinates 
    ;;   : (or/c (-> (values #f #f #f #f)
    ;;           (-> (values num num #f #f))
    ;;           (-> (values num num num num)))
    (define/private (get-box-upper-right-and-lock-coordinates)
      (define admin (get-admin))
      (define canvas (get-canvas))
      (cond
        [(and admin canvas)
         (send admin get-view bx by bw bh)
         (define hi (send canvas horizontal-inset))
         (define vi (send canvas vertical-inset))
         (define view-left (unbox bx))
         (define view-top (unbox by))
         (define br (+ view-left (unbox bw) hi))
         (define bt (+ view-top (- vi)))
         (cond
           [the-strs
            (define-values (box-width box-height label-overlap?)
              (get-blue-box-size (get-dc) (get-style-list) the-strs))
            (define bmp-bluebox-x (+ br (- blue-box-margin box-width)))
            (define bmp-bluebox-y (+ bt (- box-height blue-box-margin lock-height)))
            (define view-bottom (+ view-top (unbox bh)))
            (define bmp-view-x (+ view-left blue-box-margin))
            (define bmp-view-y (- view-bottom blue-box-margin lock-height))
            (cond
              [(and (bmp-bluebox-x . <= . bmp-view-x)
                    (bmp-bluebox-y . >= . bmp-view-y))
               (values br bt bmp-view-x bmp-view-y)]
              [(bmp-bluebox-y . >= . bmp-view-y)
               (values br bt bmp-bluebox-x bmp-view-y)]
              [(bmp-bluebox-x . <= . bmp-view-x)
               (values br bt bmp-view-x bmp-bluebox-y)]
              [else
               (values br bt bmp-bluebox-x bmp-bluebox-y)])]
           [else
            (values br bt #f #f)])]
        [else (values #f #f #f #f)]))

    (define/override (adjust-cursor evt)
      (cond
        [(in-blue-box? evt)
         arrow-cursor]
        [else
         (super adjust-cursor evt)]))
    
    (define last-evt-seen #f)
    (define/override (on-event evt)
      (set! last-evt-seen evt)
      (update-mouse-in-blue-box (in-blue-box? evt))
      (define-values (is-in-lock? is-in-read-more?) (in-lock/in-read-more? last-evt-seen))
      (update-mouse-in-lock-icon/read-more? is-in-lock? is-in-read-more?)
      (cond
        [mouse-in-blue-box?
         (cond
           [(and mouse-in-lock-icon?
                 (send evt button-up? 'left))
            (update-locked (not locked?))]
           [(and mouse-in-read-more?
                 (send evt button-up? 'left))
            (visit-docs-url)])]
        [else
         (super on-event evt)]))
    
    (define timer (new logging-timer%
                       [notify-callback
                        (λ ()
                          (set! timer-running? #f)
                          (update-the-strs))]))
    (define timer-running? #f)
    (define/augment (after-set-position)
      (inner (void) after-set-position)
      (trigger-buffer-changed-callback))
    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      (trigger-buffer-changed-callback))
    (define/augment (after-delete start len)
      (inner (void) after-delete start len)
      (trigger-buffer-changed-callback))
    (define/augment (on-lexer-valid valid?)
      (inner (void) on-lexer-valid valid?)
      (when valid?
        (trigger-buffer-changed-callback)))
    (define/private (trigger-buffer-changed-callback #:now? [now? #f])
      (when (or locked?
                mouse-in-blue-box?
                (not the-strs))
        (set! update-the-strs-coroutine #f)
        (start-the-timer now?)))
    (define/private (start-the-timer now?)
      (unless timer-running?
        (set! timer-running? #t)
        (send timer start (if now? 10 300) #t)))

    (define update-the-strs-coroutine #f)

    (define/override (disable-blue-boxes)
      (invalidate-blue-box-region)
      (set! update-the-strs-coroutine #f)
      (set! the-strs #f))
    
    (define/override (update-the-strs)
      (unless update-the-strs-coroutine
        (set! update-the-strs-coroutine
              (coroutine
               #:at-least 12
               maybe-pause
               init-val
               (define sp (get-start-position))
               (and (= sp (get-end-position))
                    (compute-tag+rng maybe-pause sp)))))
      (define done? (coroutine-run update-the-strs-coroutine (void)))
      (cond
        [(and done? update-the-strs-coroutine)
         ;; check `update-the-strs-coroutine` because some
         ;; query to the buffer might have caused a callback
         ;; to call `trigger-buffer-changed-callback`
         ;; I'm not sure how this can happen specifically,
         ;; but we saw the symptom that this `coroutine-value`
         ;; was getting `#f` as an argument and, if that happens
         ;; then we want to start over anyway
         (define tag+rng (coroutine-value update-the-strs-coroutine))
         (set! update-the-strs-coroutine #f)
         (when tag+rng
           (match tag+rng
             [(list ir-start ir-end new-strs path url-tag)
              (when new-strs
                (begin-edit-sequence #t #f)
                (invalidate-blue-box-region)
                (set! the-strs new-strs)
                (set! visit-docs-path path)
                (set! visit-docs-tag url-tag)
                (when last-evt-seen
                  (update-mouse-in-blue-box (in-blue-box? last-evt-seen))
                  (define-values (is-in-lock? is-in-read-more?) (in-lock/in-read-more? last-evt-seen))
                  (update-mouse-in-lock-icon/read-more? is-in-lock? is-in-read-more?))
                (invalidate-blue-box-region)
                (end-edit-sequence))]
             [#f (void)]))]
        [else
         (start-the-timer #f)]))

    (define/private (compute-tag+rng maybe-pause pos)
      (define basic-info
        (or (interval-map-ref (get/start-docs-im) pos #f)
            (check-nearby-symbol pos maybe-pause)))
      (match basic-info
        [(list start end tag path url-tag)
         (define id (string->symbol (get-text start end)))
         (define the-blueboxes-cache (get-blueboxes-cache this))
         (cond
           [the-blueboxes-cache
            (define meth-tags
              (fetch-blueboxes-method-tags id #:blueboxes-cache the-blueboxes-cache))
            (cond
              [(and (not tag) (null? meth-tags))
               #f]
              [else
               (define id-strs
                 (and tag
                      (fetch-blueboxes-strs tag #:blueboxes-cache the-blueboxes-cache)))
               (cond
                 [id-strs
                  (define bbss
                    (for/list ([meth-tag (in-list meth-tags)])
                      (fetch-blueboxes-strs meth-tag #:blueboxes-cache the-blueboxes-cache)))
                  (define first-of-first-bbs
                    (for/or ([bbs (in-list bbss)])
                      (and (pair? bbs) (car bbs))))
                  (define without-first-bbss
                    (for/list ([bbs (in-list bbss)])
                      (if bbs (cdr bbs) '())))
                  (list start
                        end
                        (apply append
                               id-strs
                               (if first-of-first-bbs (list first-of-first-bbs) '())
                               without-first-bbss)
                        path
                        url-tag)]
                 [else #f])])]
           [else #f])]
        [#f #f]))
    
    (define/private (check-nearby-symbol pos maybe-pause)
      (define require-candidates (get-require-candidates))
      (cond
        [(or (is-stopped?)
             (is-frozen?)
             (not (is-lexer-valid?))
             (set-empty? require-candidates))
         #f]
        [else
         (define mps
           (for/list ([require-candidate (in-set require-candidates)])
             (path->module-path require-candidate #:cache (get-path->pkg-cache))))
         (let loop ([pos pos])
           (cond
             [(interval-map-ref (get/start-docs-im) pos #f) => values]
             [(member (classify-position pos) '(symbol keyword))
              (define-values (start end) (get-token-range pos))
              (cond
                [(and start end)
                 (define candidate (try-to-find-docs start end maybe-pause mps))
                 (or candidate (loop (- start 1)))]
                [else (loop (- pos 1))])]
             [(zero? pos) #f]
             [else
              (maybe-pause)
              (loop (- pos 1))]))]))

    (define/private (try-to-find-docs start end maybe-pause mps)
      (define id (string->symbol (get-text start end)))
      (define xref (load-collections-xref))
      (for/or ([mp (in-list mps)])
        (maybe-pause)
        (define definition-tag (xref-binding->definition-tag xref (list mp id) #f))
        (cond
          [definition-tag
            (define-values (path url-tag) (xref-tag->path+anchor xref definition-tag))
            (cond
              [path
               (list start
                     end
                     definition-tag
                     path
                     url-tag)]
              [else #f])]
          [else #f])))
    
    (define/augment (on-insert where len)
      (define docs-im (get-docs-im))
      (when docs-im
        (clear-im-range where len)
        (interval-map-expand! docs-im where (+ where len)))
      (inner (void) on-insert where len))
    
    (define/augment (on-delete where len)
      (define docs-im (get-docs-im))
      (when docs-im
        (clear-im-range where len)
        (interval-map-contract! docs-im where (+ where len)))
      (inner (void) on-delete where len))

    (define/private (clear-im-range where len)
      (define docs-im (get-docs-im))
      (when docs-im
        (for ([x (in-range len)])
          (define tag+rng (interval-map-ref docs-im (+ where x) #f))
          (when tag+rng
            (interval-map-remove! 
             docs-im 
             (list-ref tag+rng 0)
             (list-ref tag+rng 1))))))
    
    (define/private (in-blue-box? evt)
      (cond
        [(send evt leaving?) #f]
        [else
         (define dc-x (send evt get-x))
         (define dc-y (send evt get-y))
         (define-values (br bt bmp-x bmp-y) (get-box-upper-right-and-lock-coordinates))
         (cond
           [(and br bt the-strs)
            (define-values (ex ey) (dc-location-to-editor-location dc-x dc-y))
            (define-values (bw bh _) 
              (if (get-show-docs?)
                  (get-blue-box-size (get-dc) (get-style-list) the-strs)
                  (values corner-radius corner-radius #f)))
            (and (<= (- br bw) ex br)
                 (<= bt ey (+ bt bh)))]
           [else #f])]))
    
    (define/private (in-lock/in-read-more? evt)
      (cond
        [(send evt leaving?) (values #f #f)]
        [(and (get-show-docs?) (get-dc))
         (define dc-x (send evt get-x))
         (define dc-y (send evt get-y))
         (define-values (br bt bmp-x bmp-y) (get-box-upper-right-and-lock-coordinates))
    
         (define-values (read-more-w read-more-h read-more-d read-more-a)
           (send (get-dc) get-text-extent sc-read-more... 
                 (get-read-more-underline-font (get-style-list))))
         
         (cond
           [(and br bt bmp-x bmp-y)
            (define-values (ex ey) (dc-location-to-editor-location dc-x dc-y))
            (values (and (<= bmp-x ex (+ bmp-x lock-width))
                         (<= bmp-y ey (+ bmp-y lock-height)))
                    (and (<= (- br read-more-w read-more-gap) ex (- br read-more-gap))
                         (<= (+ bmp-y lock-height (- read-more-h))
                             ey
                             (+ bmp-y lock-height))))]
           [else (values #f #f)])]
        [else (values #f #f)]))

    (super-new)))

(define docs-text-original-info-mixin
  (mixin () (docs-text-info<%>)
    (define docs-im #f)
    (define require-candidates '())
    (define path->pkg-cache (make-hash))
    (define linked-texts '())
    (define/public (get-path->pkg-cache) path->pkg-cache)
    (define/public (syncheck:reset-docs-im)
      (set! docs-im #f)
      (set! require-candidates '())
      (set! path->pkg-cache (make-hash)))
    (define/public (get-docs-im) docs-im)
    (define/public (get/start-docs-im) 
      (cond
        [docs-im docs-im]
        [else
         (set! docs-im (make-interval-map))
         docs-im]))
    (define/public (syncheck:add-docs-range start end tag path url-tag)
      ;; the +1 to end is effectively assuming that there
      ;; are no abutting identifiers with documentation
      (define rng (list start (+ end 1) tag path url-tag))
      (interval-map-set! (get/start-docs-im) start (+ end 1) rng))
    (define/public (syncheck:add-require-candidate path)
      (set! require-candidates (set-add require-candidates path)))
    (define/public (get-require-candidates) require-candidates)
    (define/public (syncheck:update-blue-boxes other-text)
      (update-the-strs)
      (send other-text set-original-info-text this)
      (send other-text update-the-strs))
    (define/public (add-linked t)
      (unless (memq t linked-texts)
        (set! linked-texts (cons t linked-texts))))
    (define/public (update-the-strs) (void))
    (define/public (disable-blue-boxes) (void))
    (define/pubment (toggle-syncheck-docs)
      (inner (void) toggle-syncheck-docs)
      (for ([t (in-list linked-texts)])
        (send t toggle-syncheck-docs)))
    (super-new)))

;; this is used for the REPL -- so all of the interval map state
;; isn't correct, only the require candidates are right; so we just
;; return empty things here.
(define docs-text-linked-info-mixin
  (mixin () (docs-text-info<%>)
    (define original-info-text #f)
    (define/public (set-original-info-text info-text)
      ;; the twisty way this is set up means that
      ;; this method is called multiple times with
      ;; the same argument
      (set! original-info-text info-text)
      (send original-info-text add-linked this))
    (define/public (get-path->pkg-cache)
      (if original-info-text
          (send original-info-text get-path->pkg-cache)
          (make-hash)))
    (define empty-interval-map (make-interval-map))
    (define/public (get-docs-im) empty-interval-map)
    (define/public (get/start-docs-im) empty-interval-map)
    (define/public (get-require-candidates)
      (if original-info-text
          (send original-info-text get-require-candidates)
          '()))
    (define/public (update-the-strs) (void))
    (define/public (disable-blue-boxes) (void))
    (define/pubment (toggle-syncheck-docs)
      (inner (void) toggle-syncheck-docs))
    (super-new)))

(define (docs-text-defs-mixin %)
  (docs-text-gui-mixin
   (docs-text-original-info-mixin
    %)))

(define (docs-text-ints-mixin %)
  (docs-text-gui-mixin
   (docs-text-linked-info-mixin
    %)))

;; (is-a/c? docs-text-info<%>) -> (or/c #f blueboxes-cache)
;; when this returns #f the cache needs
;; to be computed so we start the computation of the cache
;; in another thread; when the cache is ready we queue a callback
;; that calls the `update-the-strs` callback on `a-docs-text-info`
(define (get-blueboxes-cache a-docs-text-info)
  (define resp-chan (make-channel))
  (channel-put get-blueboxes-cache-chan (list resp-chan a-docs-text-info))
  (channel-get resp-chan))

(define got-blueboxes-cache-chan (make-channel))
(define get-blueboxes-cache-chan (make-channel))

(void
 (thread
  (λ ()
    (struct init () #:transparent)
    (struct pending (to-update-the-strss) #:transparent)
    (struct computed (blueboxes-cache doc-state) #:transparent)

    ;; state: (or/c (struct/c init)
    ;;              (struct/c pending (non-empty-listof (is-a/c? docs-text-info<%>)))
    ;;              (struct/c computed blueboxes-cache? doc-state?))
    (let loop ([state (init)])
      (sync
       (handle-evt
        got-blueboxes-cache-chan
        (λ (blueboxes-cache+doc-state)
          (match state
            [(pending to-update-the-strss)
             (for ([to-update-the-strs (in-list to-update-the-strss)])
               (queue-callback
                (λ ()
                  (send to-update-the-strs update-the-strs))))
             (loop (computed (list-ref blueboxes-cache+doc-state 0)
                             (list-ref blueboxes-cache+doc-state 1)))]
            [_
             ;; shouldn't happen, but if it does, let someone know about it
             (queue-callback
              (λ ()
                (error 'blueboxes-gui.rkt
                       "got the cache result back in a state where we didn't ask for it!\n  state: ~s"
                       state)))])))
       (handle-evt
        get-blueboxes-cache-chan
        (λ (resp-chan+to-update-the-strs)
          (define resp-chan (list-ref resp-chan+to-update-the-strs 0))
          (define to-update-the-strs (list-ref resp-chan+to-update-the-strs 1))

          (define (start-blueboxes-computation)
            (thread
             (λ ()
               (define blueboxes-cache
                 (make-blueboxes-cache #t #:blueboxes-dirs (get-rendered-doc-directories #f #f)))
               (define doc-state (get-current-doc-state))
               (channel-put got-blueboxes-cache-chan (list blueboxes-cache doc-state)))))

          (match state
            [(init)
             (start-blueboxes-computation)
             (channel-put resp-chan #f)
             (loop (pending (list to-update-the-strs)))]
            [(pending to-update-the-strss)
             (channel-put resp-chan #f)
             (cond
               [(member to-update-the-strs to-update-the-strss)
                (loop state)]
               [else
                (loop (pending (cons to-update-the-strs to-update-the-strss)))])]
            [(computed blueboxes-cache doc-state)
             (cond
               [(doc-state-changed? doc-state)
                (start-blueboxes-computation)
                (channel-put resp-chan #f)
                (loop (pending (list to-update-the-strs)))]
               [else
                (channel-put resp-chan blueboxes-cache)
                (loop state)])]))))))))

(define arrow-cursor (make-object cursor% 'arrow))

(define (make-arrow-path init-angle)
  (struct turtle (x y θ))
  
  (define (move-then-turn p dist α)
    (define x (turtle-x p))
    (define y (turtle-y p))
    (define θ (turtle-θ p))
    (turtle (+ x (* dist (cos θ)))
            (+ y (* dist (sin θ)))
            (+ α θ)))
  
  (define arrow-tail-length 12)
  (define arrow-tail-width 8)
  (define arrow-head-side-len 24)
  
  ;; start at the center of the end of the arrow
  (define p1 (turtle 0 0 init-angle))
  (define p2 (move-then-turn p1 (/ arrow-tail-width 2) (/ pi 2)))
  (define p3 (move-then-turn p2 arrow-tail-length (- (/ pi 2))))
  (define p4 (move-then-turn p3 
                             (- (/ arrow-head-side-len 2) (/ arrow-tail-width 2))
                             (* pi 2/3)))
  (define p5 (move-then-turn p4 arrow-head-side-len (* pi 2/3)))
  (define p6 (move-then-turn p5 arrow-head-side-len (* pi 2/3)))
  (define p7 (move-then-turn p6
                             (- (/ arrow-head-side-len 2) (/ arrow-tail-width 2))
                             (- (/ pi 2))))
  (define p8 (move-then-turn p7 arrow-tail-length (/ pi 2)))
  (define p9 (move-then-turn p8 (/ arrow-tail-width 2) 0))
  
  (define dc-path (new dc-path%))
  (send dc-path move-to (turtle-x p1) (turtle-y p1))
  (for ([p (in-list (list p2 p3 p4 p5 p6 p7 p8 p9))])
    (send dc-path line-to (turtle-x p) (turtle-y p)))
  (send dc-path close)
  dc-path)

(define open-arrow-path (make-arrow-path (* pi 1/4)))
(define lock-closed (lock-icon #f))
(define lock-open (lock-icon #t))
(define lock-width (max (send lock-closed get-width)
                        (send lock-open get-width)))
(define lock-height (max (send lock-closed get-height)
                         (send lock-open get-height)))

(define (draw-closed dc x y)
  (define smoothing (send dc get-smoothing))
  (define pen (send dc get-pen))
  (define brush (send dc get-brush))
  
  (send dc set-pen "black" 1 'transparent)
  (send dc set-smoothing 'aligned)
  
  (define-values (ox oy) (send dc get-origin))
  (send dc set-origin 
        (+ ox x (- corner-radius) (- shadow-size)) 
        (+ oy y (- corner-radius) (- shadow-size)))
  (send dc set-brush closed-radial-gradient-brush)
  (send dc draw-arc 
        0 0
        (* (+ corner-radius shadow-size) 2) 
        (* (+ corner-radius shadow-size) 2)
        pi (* pi #e1.5))
  (send dc set-origin ox oy)

  (send dc set-brush (get-blue-box-color) 'solid)
  (send dc draw-arc 
        (- x corner-radius 1) (- y corner-radius 1)
        (+ (* corner-radius 2) 2) (+ (* corner-radius 2) 2)
        pi (* pi #e1.5))
 
  (send dc set-brush
        (if (preferences:get 'framework:white-on-black?)
            "black"
            "white")
        'solid)
  (send dc draw-path open-arrow-path (- x 6) (+ y 6))
    
  (send dc set-smoothing smoothing)
  (send dc set-brush brush)
  (send dc set-pen pen))

(define fade-out-color (make-object color% 255 255 255 .8))

(define (draw-open dc dx dy 
                   sl strs 
                   br bt bmp-x bmp-y
                   show-lock? mouse-in-lock-icon? mouse-in-read-more? locked?
                   left top right bottom)
  
  (define-values (box-width box-height label-overlap?)
    (get-blue-box-size dc sl strs))
  
  (when (let* ([wd-left (- br box-width shadow-size)]
               [wd-top bt]
               [wd-right (+ wd-left box-width shadow-size)]
               [wd-bottom (+ wd-top box-height shadow-size)])
          ;; wd for "will draw"
          (rectangles-intersect? wd-left wd-top wd-right wd-bottom
                                 left top right bottom))
    
    (define dx+br (+ dx br))
    (define dy+bt (+ dy bt))
    (define pen (send dc get-pen))
    (define brush (send dc get-brush))
    (define smoothing (send dc get-smoothing))
    (define text-foreground (send dc get-text-foreground))
    (define font (send dc get-font))
    
    (define std (send sl find-named-style "Standard"))
    (when std (send dc set-font (send std get-font)))
    
    (send dc set-smoothing 'aligned)
    (send dc set-pen "black" 1 'transparent)
    (let ([rect-x (+ dx (- br box-width))]
          [rect-y (+ dy bt)])
      (send dc set-brush (make-blue-box-gradient-pen rect-x rect-y box-width box-height))
      (send dc draw-rectangle rect-x rect-y box-width box-height))
    
    (send dc set-font (if mouse-in-read-more?
                          (get-read-more-underline-font sl)
                          (get-read-more-font sl)))
    (define-values (read-more-w read-more-h read-more-d read-more-a)
      (send dc get-text-extent sc-read-more... (get-read-more-underline-font sl)))
    (send dc set-text-foreground
          (if (preferences:get 'framework:white-on-black?)
              "lightblue"
              "darkblue"))
    (send dc draw-text 
          sc-read-more...
          (+ dx (- br read-more-w read-more-gap))
          (+ dy (- (+ bmp-y lock-height) read-more-h)))
    
    (when show-lock?
      (define icon (if locked? lock-closed lock-open))
      (send dc draw-bitmap icon (+ dx bmp-x) (+ dy bmp-y))
      (when mouse-in-lock-icon?
        (send dc set-font normal-control-font)
        (send dc set-text-foreground "black")
        (define-values (tw th _1 _2) (send dc get-text-extent sc-f2-to-lock))
        (send dc set-brush fade-out-color 'solid)
        (define txt-x (- dx+br box-width))
        (define txt-y (+ dy+bt box-height))
        (send dc draw-rectangle 
              txt-x txt-y
              (+ tw 8)
              (+ th 4))
        (send dc draw-text sc-f2-to-lock 
              (+ txt-x 4)
              (+ txt-y 2))))
    
    (when std (send dc set-font (send std get-font)))
    (define label-font (get-label-font sl))
    (define-values (label-w label-h label-d label-a)
      (send dc get-text-extent (list-ref strs 0) label-font 'grapheme))
    
    (send dc set-text-foreground
          (if (preferences:get 'framework:white-on-black?)
              wob-var-color
              bow-var-color))
    (for/fold ([y (if label-overlap? 
                      (+ blue-box-margin (extra-first-line-space dc sl strs))
                      (+ blue-box-margin label-h))])
              ([str (in-list (cdr strs))])
      (define-values (w h d a) (send dc get-text-extent str #f 'grapheme))
      (send dc draw-text str (+ (- dx+br  box-width) blue-box-margin) (+ dy+bt y) 'grapheme)
      (+ y h))
    
    (draw-blue-box-shadow dc (- dx+br box-width) dy+bt box-width box-height)
    
    (send dc set-text-foreground blue-box-label-text-color)
    (send dc set-font label-font)
    (send dc draw-text (list-ref strs 0)
          (- dx+br blue-box-margin label-w)
          (+ dy+bt blue-box-margin)
          'grapheme)
    
    (send dc set-text-foreground text-foreground)
    (send dc set-smoothing smoothing)
    (send dc set-brush brush)
    (send dc set-pen pen)
    (send dc set-font font)))

;; EFFECT: updates the pen & brush of the given dc (but restores origin)
(define (draw-blue-box-shadow dc box-x box-y box-width box-height)
  
  (define-values (ox oy) (send dc get-origin))
  (send dc set-pen "black" 1 'transparent)
  (send dc set-brush horizontal-gradient-brush)
  (send dc set-origin 
        (+ ox (- box-x shadow-size))
        (+ oy box-y))
  (send dc draw-rectangle 0 0 shadow-size box-height)
  
  (send dc set-brush vertical-gradient-brush)
  (send dc set-origin
        (+ ox box-x)
        (+ oy (+ box-y box-height)))
  (send dc draw-rectangle 0 0 box-width shadow-size)
  
  (send dc set-brush radial-gradient-brush)
  (send dc set-origin 
        (+ ox (- box-x shadow-size))
        (+ oy (- (+ box-y box-height) shadow-size)))
  (send dc draw-arc
        0 0
        (* 2 shadow-size)
        (* 2 shadow-size)
        pi
        (* pi 3/2))
  (send dc set-origin ox oy))

(define shadow-start-color (make-object color% 0 0 0 .5))
(define shadow-end-color (make-object color% 0 0 0 0))
(define shadow-size 10)

(define horizontal-gradient-brush
  (new brush%
       [gradient
        (new linear-gradient%
             [x0 0] [y0 0]
             [x1 shadow-size] [y1 0]
             [stops (list (list 0 shadow-end-color)
                          (list 1 shadow-start-color))])]))
(define vertical-gradient-brush
  (new brush%
       [gradient
        (new linear-gradient%
             [x0 0] [y0 0]
             [x1 0] [y1 shadow-size]
             [stops (list (list 0 shadow-start-color)
                          (list 1 shadow-end-color))])]))
(define radial-gradient-brush
  (new brush%
       [gradient
        (new radial-gradient%
             [x0 shadow-size] [y0 shadow-size] [r0 0]
             [x1 shadow-size] [y1 shadow-size] [r1 shadow-size]
             [stops (list (list 0 shadow-start-color)
                          (list 1 shadow-end-color))])]))

(define closed-radial-gradient-brush
  (new brush%
       [gradient
        (new radial-gradient%
             [x0 (+ corner-radius shadow-size)] 
             [y0 (+ corner-radius shadow-size)]
             [r0 corner-radius]
             [x1 (+ corner-radius shadow-size)]
             [y1 (+ corner-radius shadow-size)]
             [r1 (+ shadow-size corner-radius)]
             [stops (list (list 0 (make-object color% 0 0 0 0))
                          (list 0 shadow-start-color)
                          (list 1 shadow-end-color))])]))

(define (get-blue-box-size dc sl strs)
  (define std-font
    (and (send sl find-named-style "Standard")
         (send (send sl find-named-style "Standard") get-font)))
  (define-values (main-w main-h)
    (for/fold ([tot-w (+ blue-box-margin blue-box-margin)]
               [tot-h (+ blue-box-margin blue-box-margin)])
              ([str (in-list (cdr strs))])
      (define-values (w h d a) (send dc get-text-extent str std-font))
      (values (max tot-w (+ w blue-box-margin blue-box-margin))
              (+ tot-h h))))
  (define-values (label-w label-h _2 _3) 
    (send dc get-text-extent (list-ref strs 0) (get-label-font sl)))
  (define-values (first-line-w first-line-h _5 _6)
    (send dc get-text-extent 
          (if (null? (cdr strs)) "" (list-ref strs 1))
          std-font))
  (define-values (read-more-w read-more-h _7 _8)
    (send dc get-text-extent
          sc-read-more...
          (get-read-more-underline-font sl)))
  (cond
    [(<= (+ label-w first-line-w 10) main-w)
     ;; in this case, there is no overlap on the first line, so
     ;; we draw both on the first line (like they are drawn in the html)
     (values main-w 
             (+ main-h 
                (extra-first-line-space dc sl strs)
                read-more-h
                read-more-gap)
             #t)]
    [else
     ;; otherwise we make an extra line at the top for the first line
     (values (max main-w 
                  (+ label-w blue-box-margin blue-box-margin)
                  (+ read-more-w blue-box-margin blue-box-margin))
             (+ main-h label-h read-more-h read-more-gap)
             #f)]))

(define read-more-gap 4)

;; returns the extra gap to leave above the first line of the contract
;; so that the baseline of the label and the baseline of that first line
;; are at the same place (useful in the case where the label overlaps
;; with the first line of the contract)
(define (extra-first-line-space dc sl strs)
  (define-values (_1 label-h label-d _2) 
    (send dc get-text-extent (list-ref strs 0) (get-label-font sl)))
  (define-values (_3 first-line-h first-line-d _4)
    (send dc get-text-extent (if (null? (cdr strs)) "" (list-ref strs 1))))
  (max 0 (- (- label-h label-d)
            (- first-line-h first-line-d))))
