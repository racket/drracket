#lang racket/base
(require racket/gui/base
         racket/contract
         racket/class
         (for-syntax racket/base)
         (prefix-in r: racket/base)
         framework)
(provide ellipsis-snip%)

(define ellipsis-snip%
  (class snip%
    (init-field extra [insertion-done? #f])
    (define str-snip (make-object string-snip% "..."))
    (inherit get-style)

    (define-syntax (forward stx)
      (syntax-case stx ()
        [(_ (def (id formals ...))
            (before ...)
            (after ...))
         (with-syntax ([(call-ids ...)
                        (for/list ([formal (in-list (syntax->list #'(formals ...)))])
                          (syntax-case formal ()
                            [(id default-value) (identifier? #'id) #'id]
                            [id (identifier? #'id) #'id]))])
           #'(def (id formals ...)
               (sync-style)
               before ...
               (send str-snip id call-ids ...)
               after ...))]))
    (define/private (sync-style)
      (send str-snip set-style (get-style)))
    
    (forward (define/override (get-extent dc x y wb hb db sb lb rb)) () ())
    (forward (define/override (draw dc x y left top right bottom dx dy draw-caret))
             {(unless insertion-done?
                (when inside?
                  (define pen (send dc get-pen))
                  (define brush (send dc get-brush))
                  (define wb (box 0.0))
                  (define hb (box 0.0))
                  (get-extent dc (- x dx) (- y dy) wb hb #f #f #f #f)
                  (send dc set-brush "pink" 'solid)
                  (send dc set-pen "black" 1 'transparent)
                  (send dc draw-rectangle x y (unbox wb) (unbox hb))
                  (send dc set-brush "red" 'solid)
                  (send dc draw-rectangle
                        x
                        (+ y (unbox hb) -1)
                        (unbox wb)
                        1)
                  (send dc set-pen pen)
                  (send dc set-brush brush)))}
             {})
    (forward (define/override (partial-offset dc x y len)) () ())
    (forward (define/override (split position first second)) () ())
    (forward (define/override (size-cache-invalid)) () ())

    (define inside? #f)
    (define/private (update-inside _inside?)
      (unless (equal? inside? _inside?)
        (set! inside? _inside?)
        (define admin (get-admin))
        (when admin
          (define wb (box 0.0))
          (define hb (box 0.0))
          (define dc (send admin get-dc))
          (when dc
            (get-extent dc 0 0 wb hb #f #f #f #f)
            (send admin needs-update this 0 0 (unbox wb) (unbox hb))))))
          
    (define/override (on-goodbye-event dc x y editorx editory event)
      (update-inside #f)
      (handle-event dc x y editorx editory event #t))
    (define/override (on-event dc x y editorx editory event)
      (update-inside #t)
      (handle-event dc x y editorx editory event #f))
    (define/private (handle-event dc x y editorx editory event goodbye?)
      (unless insertion-done?
        (define admin (get-admin))
        (when admin
          (define ed (send admin get-editor))
          (define the-cursor-to-use (if goodbye? #f arrow-cursor))
          (when (send event button-up? 'left)
            (unless goodbye?
              (do-insertion)
              (set! insertion-done? #t)
              (set! the-cursor-to-use #f)))
          (send ed set-cursor the-cursor-to-use))))
    
    (define/private (do-insertion)
      (update-inside #f)
      (define admin (get-admin))
      (define ed (send admin get-editor))
      (when (is-a? ed text:ports<%>)
        (define pos (send ed get-snip-position this))
        (when pos
          (send ed begin-edit-sequence)
          (define insertion-pos (+ pos (send str-snip get-count) 1))
          (let loop ([strs extra])
            (cond
              [(null? strs) (void)]
              [else
               (define str (car strs))
               (send ed insert/io str insertion-pos (send ed get-err-style-delta))
               (send ed insert/io "\n" insertion-pos (send ed get-err-style-delta))
               (loop (cdr strs))]))
          (send ed end-edit-sequence))))
    (define/override (copy) (new ellipsis-snip% 
                                 [extra extra]
                                 [insertion-done? insertion-done?]))
    (define/override (write f)
      (define bp (open-output-bytes))
      (r:write (list insertion-done? extra) bp)
      (define b (get-output-bytes bp))
      (send f put (bytes-length b) b))
    (super-new)
    (inherit set-flags get-flags get-admin set-snipclass set-count)
    (set-flags (list* 'handles-between-events 'handles-all-mouse-events (get-flags)))
    (set-snipclass snipclass)
    (set-count (send str-snip get-count))))

(define arrow-cursor (make-object cursor% 'arrow))

(define (set-box/f! b v) (when (box? b) (set-box! b v)))

(provide snipclass)
(define valid-data? (list/c boolean? (listof string?)))
(define snipclass
  (new (class snip-class%
         (define/override (read f)
           (define data (r:read (open-input-bytes (send f get-unterminated-bytes))))
           (cond
             [(valid-data? data)
              (new ellipsis-snip% 
                   [insertion-done? (list-ref data 0)]
                   [extra (list-ref data 1)])]
             [else
              (new ellipsis-snip% [extra '()])]))
         (super-new))))
(send snipclass set-version 2)
(send snipclass set-classname 
      (format "~s" '((lib "ellipsis-snip.rkt" "drracket" "private")
                     (lib "ellipsis-snip-wxme.rkt" "drracket" "private"))))
(send (get-the-snip-class-list) add snipclass)

(module+ main
  (define f (new frame% [label ""] [width 100] [height 100]))
  (define t (new text%))
  (send t insert (new ellipsis-snip% [extra '("a" "b" "c")]))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (send f show #t))
