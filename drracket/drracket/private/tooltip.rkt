#lang racket/base
(require racket/math
         racket/gui
         racket/class
         framework)

;; to bring back types, added "typed/" in front of the
;; racket/* imports, remove these macros and remove the
;; occurrences of "#;#;" that appear below
;; to get back to at least the most recently typed version
;; of this file, but there will be problems, including
;; getting the preferneces system to cooperate and the
;; font get-size method.
(define-syntax-rule (: . whatever) (void))
(define-syntax-rule (assert e) e)

(define (get-font-size)
  (editor:font-size-pref->current-font-size
   (preferences:get 'framework:standard-style-list:font-size)))

(provide tooltip-frame%)

(: tooltip-font (U #f (Instance Font%)))
(define tooltip-font #f)

(: get-tooltip-font (-> (Instance Font%)))
(define (get-tooltip-font)
  (cond
    [tooltip-font tooltip-font]
    [else
     (define default-font-size
       (let* ([txt (make-object text%)]
              [stl (send txt get-style-list)]
              [bcs (send stl basic-style)])
         (send bcs get-size)))
     (define ans (send the-font-list find-or-create-font
                       (* (/ (send small-control-font get-size) default-font-size)
                          (get-font-size))
                       (send small-control-font get-face)
                       (send small-control-font get-family)
                       (send small-control-font get-style)
                       (send small-control-font get-weight)
                       (send small-control-font get-underlined)
                       (send small-control-font get-smoothing)
                       (send small-control-font get-size-in-pixels)
                       (send small-control-font get-hinting)))
     (set! tooltip-font ans)
     ans]))

(define yellow-message%
  (class canvas%
    (inherit get-dc refresh get-client-size
             min-width min-height
             get-parent)


    (: labels (Listof (List String String)))
    ;; the first string indicates the amount of leading space
    ;; to give; it should be as much width as the first string
    ;; would require to draw. The second string is then actually drawn
    (define labels '(("" "")))

    (: set-lab (-> (Listof (List String String)) Void))
    (define/public (set-lab _ls)
      (unless (equal? labels _ls)
        (set! labels _ls)
        (update-size)
        (refresh)))
    (: update-size (-> Void))
    (define/private (update-size)
      (define dc (get-dc))
      (send dc set-font (get-tooltip-font))
      (define-values (w h)
        (for/fold ([w #;#;: Nonnegative-Real 0] [h #;#;: Nonnegative-Real 0])
                  ([space+label (in-list labels)])
          (define space (list-ref space+label 0))
          (define label (list-ref space+label 1))
          (define-values (space-w _1 _2 _3) (send dc get-text-extent space))
          (define-values (this-w this-h _4 _5) (send dc get-text-extent label))
          (values (max (+ space-w this-w) w)
                  (max this-h h))))
      (define parent (assert (get-parent)))
      (send parent begin-container-sequence)
      (min-width (+ 5 (exact-ceiling w)))
      (min-height (+ 5 (* (length labels) (exact-ceiling h))))
      (send parent end-container-sequence)
      (send parent reflow-container))

    ;; private field for gc reasons
    (define font-size-callback (λ (p v) (set! tooltip-font #f) (update-size)))
    (preferences:add-callback 'framework:standard-style-list:font-size
                              font-size-callback
                              #t)
    
    (define/override (on-paint)
      (define dc (get-dc))
      (send dc set-font (get-tooltip-font))
      (define-values (w h) (get-client-size))
      (define-values (_1 th _2 _3) (send dc get-text-extent "yX"))
      (send dc set-pen "black" 1 'transparent)
      (send dc set-brush "LemonChiffon" 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle 0 0 w h)
      (for ([space+label (in-list labels)]
            [i (in-naturals)])
        (define space (list-ref space+label 0))
        (define label (list-ref space+label 1))
        (define-values (space-w _1 _2 _3) (send dc get-text-extent space))
        (send dc draw-text label (+ 2 space-w) (+ 2 (* i th)))))
    (super-new [stretchable-width #f] [stretchable-height #f])))

(define tooltip-frame%
  (class frame%
    (inherit reflow-container move get-width get-height is-shown?)
    
    (init-field [frame-to-track #;#;: (Option (Instance Window<%>)) #f])
    (: timer (Option (Instance Timer%)))
    (define timer
      (let ([frame-to-track frame-to-track])
        (and frame-to-track
             (new timer%
                  [notify-callback
                   (λ ()
                     (unless (send frame-to-track is-shown?)
                       (show #f)
                       (send (assert timer) stop)))]))))
    
    
    (define/override (on-subwindow-event r evt)
      (and (is-shown?)
           (begin (show #f)
                  #t)))
    
    ;; ls may contain strings that have newlines; break up the strings here
    (: set-tooltip (-> (Listof String) Void))
    (define/public (set-tooltip ls)
      (define broken-up-lines
        (apply
         append
         (for/list #;#;: (Listof (Listof (List String String))) ([str #;#;: String (in-list ls)])
           (strings->strings+spacers (regexp-split #rx"\n" str)))))
      (send yellow-message set-lab broken-up-lines))
    
    (define/override (show on?)
      (when timer
        (cond
          [on? (send timer start 200 #f)]
          [else (send timer stop)]))
      (super show on?))

    (: show-over (-> Integer Integer Integer Integer [#:prefer-upper-left? Any] Void))
    (define/public (show-over x y w h #:prefer-upper-left? [prefer-upper-left? #f])
      (reflow-container)
      (define mw (get-width))
      (define mh (get-height))
      (define (upper-left must?)
        (define the-x (- x mw))
        (define the-y (- y mh))
        (if must?
            (move the-x the-y)
            (try-moving-to the-x the-y mw mh)))
      (define (lower-right must?)
        (define the-x (+ x w))
        (define the-y (+ y h))
        (if must?
            (move the-x the-y)
            (try-moving-to the-x the-y mw mh)))
      (if prefer-upper-left?
          (or (upper-left #t) (lower-right #f) (upper-left #t))
          (or (lower-right #t) (upper-left #f) (lower-right #t)))
      (show #t))
    
    (: try-moving-to (Integer Integer Integer Integer -> Boolean))
    (define/private (try-moving-to x y w h)
      (and (for/or #;#;: Boolean ([m #;#;: Natural (in-range 0 (get-display-count))])
             (define-values (mx my) (get-display-left-top-inset #:monitor m))
             (define-values (mw mh) (get-display-size #:monitor m))
             (and mx my mw mh
                  (<= (- mx) x (+ x w) (+ (- mx) mw))
                  (<= (- my) y (+ y h) (+ (- my) mh))))
           (begin (move x y)
                  #t)))

    (super-new [style '(no-resize-border no-caption float)]
               [label ""]
               [stretchable-width #f]
               [stretchable-height #f])
    (: yellow-message (Object [set-lab ((Listof (List String String)) -> Void)]))
    (define yellow-message (new yellow-message% [parent this]))))


(: strings->strings+spacers (-> (Listof String) (Listof (List String String))))
(define (strings->strings+spacers strs)
  (let loop ([strs strs]
             [prefix ""])
    (cond
      [(null? strs) '()]
      [else
       (define str (car strs))
       (define leading-spaces (car (assert (regexp-match #rx"^ *" str))))
       (define this-entry
         (cond
           [(<= (string-length prefix) (string-length leading-spaces))
            (list prefix (substring str (string-length prefix) (string-length str)))]
           [else
            (list (substring prefix 0 (string-length leading-spaces))
                  (substring str (string-length leading-spaces) (string-length str)))]))
       (define new-prefix
         (cond
           [(< (string-length prefix) (string-length leading-spaces))
            prefix]
           [else
            (string-append (substring prefix 0 (string-length leading-spaces))
                           (substring str (string-length leading-spaces) (string-length str)))]))
       (cons this-entry (loop (cdr strs) new-prefix))])))


(module+ test
  (require typed/rackunit)
  (check-equal? (strings->strings+spacers '()) '())
  (check-equal? (strings->strings+spacers '("x")) '(("" "x")))
  (check-equal? (strings->strings+spacers '("x" "x")) '(("" "x") ("" "x")))
  (check-equal? (strings->strings+spacers '("xx" " x")) '(("" "xx") ("x" "x")))
  (check-equal? (strings->strings+spacers '("abcdef"
                                            " pqrst"
                                            "  ijkl"
                                            "   mno"))
                '(("" "abcdef")
                  ("a" "pqrst")
                  ("ap" "ijkl")
                  ("api" "mno")))
  (check-equal? (strings->strings+spacers '("abcdef"
                                            " pqrst"
                                            "  ijkl"
                                            " mnozz"))
                '(("" "abcdef")
                  ("a" "pqrst")
                  ("ap" "ijkl")
                  ("a" "mnozz")))
  (check-equal? (strings->strings+spacers '("abcdef"
                                            " pqrst"
                                            " mnozz"
                                            "  ijkl"))
                '(("" "abcdef")
                  ("a" "pqrst")
                  ("a" "mnozz")
                  ("am" "ijkl")))
  (check-equal? (strings->strings+spacers '("abcdef"
                                            " pqrst"
                                            "  ijkl"
                                            "  mnoz"))
                '(("" "abcdef")
                  ("a" "pqrst")
                  ("ap" "ijkl")
                  ("ap" "mnoz")))
  (check-equal? (strings->strings+spacers '("   def"
                                            "abcxyz"))
                '(("" "   def")
                  ("" "abcxyz")))
  (check-equal? (strings->strings+spacers '("abcdef"
                                            "   xyz"
                                            "           pqrstuv"))
                '(("" "abcdef")
                  ("abc" "xyz")
                  ("abcxyz" "     pqrstuv"))))


(module+ main
  (require racket/gui/base)
  (define c%
    (class canvas%
      (inherit get-client-size client->screen)
      (: state (U False 'left 'right))
      (define state #f)
      (define/override (on-event evt)
        (define-values (w h) (get-client-size))
        (define new-state
          (cond
            [(not (<= 0 (send evt get-y) h)) #f]
            [(<= 0 (send evt get-x) (/ w 2))
             'left]
            [(<= (/ w 2) (send evt get-x) w)
             'right]
            [else
             #f]))
        (unless (equal? new-state state)
          (define old-state state)
          (set! state new-state)
          (send tooltip-frame show #f)
          (when state
            (send tooltip-frame set-tooltip
                  (case state
                    [(left) '("abcdef\n  ghij\n  klmn\n    op\n     q")]
                    [(right) '("right")]
                    [else '("whatever")]))
            (define-values (sx sy) (client->screen 10 10))
            (send tooltip-frame show-over sx sy 10 10))))
      (super-new)
      (define tooltip-frame (new tooltip-frame%))))
  (define f (new frame% [label ""] [width 200] [height 200]))
  (define c (new c% [parent f]))
  (send f show #t))
