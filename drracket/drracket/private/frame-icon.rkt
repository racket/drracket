#lang racket/base
(require racket/class racket/draw (for-syntax racket/base racket/match racket/string xml))
(provide todays-icon
         mb-scale-factor mb-main-drawing
         mb-flat-width mb-flat-height
         mb-plain-width mb-plain-height)

(define todays-icon
  (and (eq? (system-type) 'unix)
       (let ()
         ;; avoid building the mask unless we use it
         (define todays-icon
           (make-object bitmap% 
             (collection-file-path 
              (case (date-week-day (seconds->date (current-seconds)))
                [(6 0) "plt-logo-red-shiny.png"]
                [else "plt-logo-red-diffuse.png"])
              "icons")
             'png/mask))
         
         (define todays-icon-bw-mask 
           (and (send todays-icon ok?)
                (send todays-icon get-loaded-mask)
                (let* ([w (send todays-icon get-width)]
                       [h (send todays-icon get-height)]
                       [bm (make-object bitmap% w h #t)]
                       [color-mask (send todays-icon get-loaded-mask)]
                       [src-bytes (make-bytes (* w h 4) 0)]
                       [dest-bits (make-bytes (* w h 4) 255)]
                       [bdc (make-object bitmap-dc% bm)]
                       [black (send the-color-database find-color "black")]
                       [white (send the-color-database find-color "white")])
                  (send color-mask get-argb-pixels 0 0 w h src-bytes #t)
                  (for ([i (in-range 0 w)])
                    (for ([j (in-range 0 h)])
                      (let ([b (= (bytes-ref src-bytes (* 4 (+ i (* j h)))) 0)])
                        (send bdc set-pixel i j (if b white black)))))
                  (send bdc set-bitmap #f)
                  bm)))
         
         (when todays-icon-bw-mask
           (send todays-icon set-loaded-mask todays-icon-bw-mask))
         todays-icon)))

(begin-for-syntax
  (define (draw-svg)
    (define xexpr
      (with-handlers ([exn:fail? exn-message])
        (parameterize ([permissive-xexprs #t])
          (xml->xexpr
           (document-element
            (call-with-input-file (collection-file-path "racket-logo.svg" "icons")
              (λ (port)
                (read-xml port))))))))
    (cond
      [(string? xexpr)
       #`(values
          (λ (dc)
            (define str #,xexpr)
            (send dc clear)
            (send dc set-scale 1 1)
            (define-values (_1 h _2 _3) (send dc get-text-extent "x"))
            (define one-line 30)
            (let loop ([i 0]
                       [l 0])
              (cond
                [(<= (string-length str) (+ i one-line))
                 (send dc draw-text
                       (substring str i (string-length str))
                       0
                       (* l h))]
                [else
                 (send dc draw-text
                       (substring str i (+ i one-line))
                       0
                       (* l h))
                 (loop (+ i one-line) (+ l 1))])))
          300 300)]
      [else
       (draw-paths-code xexpr)]))
  
  (define (draw-paths-code xexpr)
    (define exps '())
    (define width 500)
    (define height 500)
    (let loop ([xexpr xexpr])
      (match xexpr
        [`(svg ,(? list? attrs) ,body ...)
         (define (handle-width/height str set)
           (when (regexp-match #rx"px$" str)
             (define n (string->number (regexp-replace #rx"px$" str "")))
             (when n (set n))))
         (for ([attr (in-list attrs)])
           (match attr
             [`(width ,str)
              (handle-width/height str (λ (x) (set! width x)))]
             [`(height ,str)
              (handle-width/height str (λ (x) (set! height x)))]
             [_ (void)]))
         (for-each loop body)]
        [`(path ((d ,d-attr) (fill ,fill-color) ,_ ...) ,_ ...)
         (define dc-path (d-attr->dc-path d-attr))
         (set! exps
               (cons #`(send dc set-brush
                             (make-object color%
                               #,(string->number (substring fill-color 1 3) 16)
                               #,(string->number (substring fill-color 3 5) 16)
                               #,(string->number (substring fill-color 5 7) 16))
                             'solid)
                     exps))
         (set! exps (cons (d-attr->dc-path d-attr) exps))]
        [`(,tag ,attrs ,body ...)
         (for-each loop body)]
        [_ (void)]))
    #`(values (λ (dc) #,@(reverse exps) (void))
              #,width
              #,height))

  (define (d-attr->dc-path exp)
    (define px #f)
    (define py #f)
    #`(let ([dc-path (new dc-path%)])
        #,@(for/list ([cmd (in-list (decompile-d-attr exp))])
             (match cmd
               [`(M ,x ,y)
                (set! px x)
                (set! py y)
                #`(send dc-path move-to #,px #,py)]
               [`(C ,cx1 ,cy1 ,cx2 ,cy2 ,x ,y)
                (begin0
                  #`(send dc-path curve-to #,cx1 #,cy1 #,cx2 #,cy2 #,x #,y)
                  (set! px x)
                  (set! py y))]
               [`(c ,dcx1 ,dcy1 ,dcx2 ,dcy2 ,dx ,dy)
                (begin0
                  #`(send dc-path curve-to
                          #,(+ px dcx1) #,(+ py dcy1)
                          #,(+ px dcx2) #,(+ py dcy2)
                          #,(+ px dx) #,(+ py dy))
                  (set! px (+ px dx))
                  (set! py (+ py dy)))]
               [`(l ,dx ,dy)
                (set! px (+ px dx))
                (set! py (+ py dy))
                #`(send dc-path line-to #,px #,py)]
               [`(L ,x ,y)
                (set! px x)
                (set! py y)
                #`(send dc-path line-to #,px #,py)]
               [`(z)
                #`(send dc-path close)]
               [_
                (void)]))
        (send dc draw-path dc-path)))

  (define (decompile-d-attr d-val)
    (define commands (cdr (string-split d-val #px"(?=[A-Za-z])")))
    (define (to-normal-values lst)
      (cons (string->symbol (car lst))
            (map string->number (cdr lst))))
    (for/list ([s (in-list commands)])
      (let* ([s (string-trim s)]
             [s (string-replace s "-" " -")]
             [s (string-replace s "," " ")]
             [s (regexp-replace #rx"^(.)([0-9])" s "\\1 \\2")])
        (to-normal-values (regexp-split #rx" " s))))))

(define-syntax (mb-dc-proc stx) (draw-svg))
(define-values (mb-main-drawing mb-plain-width mb-plain-height) mb-dc-proc)
(define mb-scale-factor 10/16)
(define mb-flat-width (inexact->exact (ceiling (* mb-plain-width mb-scale-factor))))
(define mb-flat-height (inexact->exact (ceiling (* mb-plain-height mb-scale-factor))))
