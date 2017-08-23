#lang typed/racket

(require typed/racket/gui
         typed/framework
         string-constants)

(provide insert-large-letters)

(: insert-large-letters (String Char (Instance Text:Basic<%>) (Instance Frame%) -> Void))
(define (insert-large-letters comment-prefix comment-character edit parent)
  (let ([str (make-large-letters-dialog comment-prefix comment-character #f)])
    (when (and str
               (not (equal? str "")))
      (render-large-letters comment-prefix comment-character (get-chosen-font) str edit)
      (void))))

(: get-default-font (-> (Instance Font%)))
(define (get-default-font)
  (send (assert (send (editor:get-standard-style-list)
                      find-named-style
                      "Standard"))
        get-font))

(: get-chosen-font (-> (Instance Font%)))
(define (get-chosen-font)
  (define-values (face size) (get-current-pref))
  (cond
    [face
     (let ([candidate
            (send the-font-list find-or-create-font
                  size
                  face
                  'default 'normal 'normal)])
       (if (equal? (send candidate get-face) face)
           candidate
           (get-default-font)))]
    [else
     (get-default-font)]))

(: get-current-pref : (-> (Values (U String False) Real)))
(define (get-current-pref)
  (define pref-val (preferences:get 'drracket:large-letters-font))
  (cond
    [(not pref-val)
     (define fnt (get-default-font))
     (values (send fnt get-face)
             (send fnt get-point-size))]
    [else
     ;; this cast (should) never fail because of the way
     ;; 'drracket:large-letters-font is initialized in main.rkt
     (define pref-val-typed (cast pref-val (Pairof (U String False) Real)))
     (values (car pref-val-typed) (cdr pref-val-typed))]))

(: set-current-pref (-> (U String False) Real Void))
(define (set-current-pref face size)
  (unless (<= 0 size 1024) (error 'set-current-pref "face size out of range, got ~s" size))
  (preferences:set 'drracket:large-letters-font (cons face size)))

(define columns-string "~a columns")

(: make-large-letters-dialog (String Char (Option (Instance Frame%)) -> (Option String)))
(define (make-large-letters-dialog comment-prefix comment-character parent)
  (define dlg (new (frame:focus-table-mixin dialog%)
                   [parent parent] 
                   [width 700]
                   [label (string-constant large-semicolon-letters)]))
  (define: text-field : (Instance Text-Field%) 
    (new text-field% 
         [parent dlg] 
         [label (string-constant text-to-insert)]
         [callback (λ (x y) (update-txt (send text-field get-value)))]))
  (: info-bar (Instance Horizontal-Panel%))
  (define info-bar (new horizontal-panel%
                        [parent dlg]
                        [stretchable-height #f]))
  (define: font-choice : (Instance Choice%)
    (let ([tmp-bdc (new bitmap-dc% [bitmap (make-bitmap 1 1 #f)])])
      (new choice%
           [label (string-constant font-name)]
           [parent info-bar]
           [choices (get-face-list)]
           [callback
            (λ (x y)
              (define choice (send font-choice get-selection))
              (when choice
                (define-values (current-font current-size) (get-current-pref))
                (set-current-pref (list-ref (get-face-list) choice) current-size)
                (update-txt (send text-field get-value))))])))
  (: font-size-field (Instance Text-Field%))
  (define font-size-field
    (new text-field%
         [parent info-bar]
         [stretchable-width #f]
         [callback (λ (x y)
                     (define n (string->number (send font-size-field get-value)))
                     (cond
                       [(and n (real? n) (<= 0 n 1024))
                        (send font-size-field set-field-background default-background)
                        (define-values (current-font current-size) (get-current-pref))
                        (set-current-pref current-font n)
                        (update-txt (send text-field get-value))]
                       [else
                        (send font-size-field set-field-background warning-background)]))]
         [label (string-constant font-size)]))

  (: default-background (Instance Color%))
  (define default-background (send font-size-field get-field-background))
  (: warning-background (Instance Color%))
  (define warning-background (assert (send the-color-database find-color "pink")))
  
  (: count (Instance Message%))
  (define count (new message% [label (format columns-string 1000)] [parent info-bar]))
  (: pane1 (Instance Horizontal-Pane%))
  (define pane1 (new horizontal-pane% (parent info-bar)))
  (: dark-msg (Object [set-bm ((Option (Instance Bitmap%)) -> Void)]))
  (define dark-msg (new bitmap-message% [parent info-bar]))
  (: pane2 (Instance Horizontal-Pane%))
  (define pane2 (new horizontal-pane% (parent info-bar)))
                                
  (: txt (Instance Text:Basic%))
  (define txt (new racket:text%))
  (: ec (Instance Editor-Canvas%))
  (define ec (new editor-canvas% [parent dlg] [editor txt]))
  (: button-panel (Instance Horizontal-Panel%))
  (define button-panel (new horizontal-panel%
                            [parent dlg]
                            [stretchable-height #f]
                            [alignment '(right center)]))
  (define: ok? : Boolean #f)
  (: ok Any)
  (: cancel Any)
  (define-values (ok cancel)
    (gui-utils:ok/cancel-buttons button-panel
                                 (λ (x y) (set! ok? #t) (send dlg show #f))
                                 (λ (x y) (send dlg show #f))))
  (: update-txt (String -> Any))
  (define (update-txt str)
    (send txt begin-edit-sequence)
    (send txt lock #f)
    (send txt delete 0 (send txt last-position))
    (let ([bm (render-large-letters comment-prefix comment-character (get-chosen-font) str txt)])
      (send ec set-line-count (+ 1 (send txt last-paragraph)))
      (send txt lock #t)
      (send txt end-edit-sequence)
      (send count set-label (format columns-string (get-max-line-width txt)))
      (send dark-msg set-bm (if (equal? str "") #f bm))))
  
  
  (let ()
    (define-values (face size) (get-current-pref))
    ;; CHANGE - get-face can return #f
    (when face
      (let loop ([i 0]
                 [faces (get-face-list)])
        (cond
          [(null? faces) (void)]
          [else (cond
                  [(equal? face (car faces))
                   (send font-choice set-selection i)]
                  [else
                   (loop (+ i 1) (cdr faces))])])))
    (send font-size-field set-value (~a size)))
  
  (send txt auto-wrap #f)
  (update-txt "")
  (send text-field focus)
  (send dlg show #t)
  (and ok? (send text-field get-value)))

(: get-max-line-width ((Instance Text:Basic<%>) -> Real))
(define (get-max-line-width txt)
  (let loop ([i (+ (send txt last-paragraph) 1)]
             [m 0])
    (cond
      [(zero? i) m]
      [else (loop (sub1 i)
                  (max m (- (send txt paragraph-end-position (- i 1))
                            (send txt paragraph-start-position (- i 1)))))])))

(define bitmap-message%
  (class canvas%
    (inherit min-width min-height get-dc refresh)
    (: bm (Option (Instance Bitmap%)))
    (define bm #f)
    (define/override (on-paint)
      (define bm* bm)
      (when bm*
        (let ([dc (get-dc)])
          (send dc draw-bitmap bm* 0 0)))
      (void))
    (: set-bm ((Option (Instance Bitmap%)) -> Void))
    (define/public (set-bm b)
      (set! bm b)
      (define bm* bm)
      (when bm*
        (min-width (send bm* get-width))
        (min-height (send bm* get-height)))
      (refresh))
    (super-new (stretchable-width #f)
               (stretchable-height #f)
               (style '(no-focus)))))

(: render-large-letters (-> String Char (Instance Font%) String (Instance Text:Basic<%>)
                            (Instance Bitmap%)))
(define (render-large-letters comment-prefix comment-character the-font str edit)
  (define bdc (new bitmap-dc% [bitmap (make-bitmap 1 1 #t)]))
  (define-values (tw raw-th td ta) (send bdc get-text-extent str the-font))
  (define th (let-values ([(_1 h _2 _3) (send bdc get-text-extent "X" the-font)])
               (max raw-th h)))
  (define tmp-color (make-object color%))
  
  (: get-char (Real Real -> Char))
  (define (get-char x y)
    (send bdc get-pixel x y tmp-color)
    (let ([red (send tmp-color red)])
      (if (= red 0)
          comment-character
          #\space)))  
  (define bitmap
    (make-object bitmap%
      (max 1 (exact-floor tw))
      (assert (max 1 (exact-floor th)) positive?)
      #t))
  
  (: fetch-line (Real -> String))
  (define (fetch-line y)
    (let loop ([x (send bitmap get-width)]
               [#{chars : (Listof Char)} null])
      (cond
        [(zero? x) (apply string chars)]
        [else (loop (- x 1) (cons (get-char (- x 1) y) chars))])))
  
  (send bdc set-bitmap bitmap)
  (send bdc clear)
  (send bdc set-font the-font)
  (send bdc draw-text str 0 0)
  
  (send edit begin-edit-sequence)
  (let ([start (send edit get-start-position)]
        [end (send edit get-end-position)])
    (send edit delete start end)
    (send edit insert "\n" start start)
    (let loop ([y (send bitmap get-height)])
      (unless (zero? y)
        (send edit insert (fetch-line (- y 1)) start start)
        (send edit insert comment-prefix start start)
        (send edit insert "\n" start start)
        (loop (- y 1)))))
  (send edit end-edit-sequence)
  (send bdc set-bitmap #f)
  bitmap)

(module+ main
  ;; this initialization code copied from main.rkt
  ;; but modified to remove the predicate; beware
  ;; that bad contents of the preferences file may
  ;; lead to asssertion failures. If they do, evaluate
  ;; (preferences:set 'drracket:large-letters-font #f)
  ;; to restore the preference setting to its default
  (preferences:set-default 'drracket:large-letters-font #f (λ (x) #t))
  (make-large-letters-dialog ";" #\; #f))
