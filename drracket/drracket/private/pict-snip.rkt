#lang racket/base
(require racket/snip
         racket/class
         racket/match
         racket/draw
         file/convertible
         racket/format
         wxme
         (prefix-in r: racket/base))
(module+ test (require rackunit))

(provide pict-snip% snip-class reader)

(define convertible<%>
  (interface* () ([prop:convertible (lambda (v r d)
                                      (send v convert r d))])
              convert))

;; this snip is created on the user's space,
;; but its callbacks are invoked on DrRacket's.
(define pict-snip%
  (class* snip% (convertible<%>)
    (init-field w h d a recorded-datum)
    (define/override (get-extent dc x y [wb #f] [hb #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (set-box/f lspace 0)
      (set-box/f rspace 0)
      (set-box/f wb w)
      (set-box/f hb h)
      (set-box/f descent d)
      (set-box/f space a))
    (define proc #f)
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (unless proc
        (set! proc (with-handlers ((exn:fail? mk-error-drawer))
                     (recorded-datum->procedure recorded-datum))))
      (define-values (ox oy) (send dc get-origin))
      (send dc set-origin (+ ox x) (+ oy y))
      (proc dc)
      (send dc set-origin ox oy))
    (define/override (copy) (new pict-snip% [w w] [h h] [d d] [a a] 
                                 [recorded-datum recorded-datum]))
    
    (define/override (write f)
      (write-version-1-of-snip w h d a recorded-datum f))

    (define/override (find-scroll-step y)
      (inexact->exact (floor (/ y 12))))
    (define/override (get-num-scroll-steps)
      (add1 (find-scroll-step h)))
    (define/override (get-scroll-step-offset y)
      (inexact->exact (floor (* y 12))))

    (super-new)
    (inherit set-snipclass)
    (set-snipclass snip-class)

    (define/public (convert r d)
      (case r
        [(png-bytes)
         (define bm (make-bitmap (inexact->exact (ceiling w))
                                 (inexact->exact (ceiling h))))
         (define dc (send bm make-dc))
         (draw dc 0 0 0 0 w h 0 0 #f)
         (define b (open-output-bytes))
         (send bm save-file b 'png)
         (get-output-bytes b)]
        [(pdf-bytes)
         (define b (open-output-bytes))
         (define dc (new pdf-dc% 
                         [interactive #f]
                         [width w] [height h]
                         [output b]))
         (send dc start-doc "pict")
         (send dc start-page)
         (draw dc 0 0 0 0 w h 0 0 #f)
         (send dc end-page)
         (send dc end-doc)
         (get-output-bytes b)]
        [else d]))))

(define (set-box/f b v) (when (box? b) (set-box! b v)))

(define ((mk-error-drawer exn) dc)
  (define clr (send dc get-text-foreground))
  (send dc set-text-foreground "red")
  (send dc draw-text (exn-message exn) 0 0)
  (send dc set-text-foreground clr))

(define snip-class 
  (new (class snip-class%
         (inherit reading-version set-version)
         (define/override (read f)
           (define version (reading-version f))
           (case version
             [(0)
              (parse-version-0-pict-snip-from-bytes
               (send f get-unterminated-bytes))]
             [(1)
              (parse-version-1-pict-snip
               (λ () (send f get-unterminated-bytes))
               (λ () (send f get-exact)))]))
         (super-new)
         (set-version 1))))

(send snip-class set-classname (format "~s" (list '(lib "pict-snip.rkt" "drracket" "private")
                                                  '(lib "pict-snip.rkt" "drracket" "private"))))
(send (get-the-snip-class-list) add snip-class)

(define reader
  (new (class* object% (snip-reader<%>)
         (define/public (read-header version stream) (void))
         (define/public (read-snip text-only? version stream)
           (if text-only?
               #"#<pict-snip>"
               (or (case version
                     [(0)
                      (parse-version-0-pict-snip-from-bytes
                       (send stream read-raw-bytes 'pict-snip))]
                     [(1)
                      (parse-version-1-pict-snip
                       (λ () (send stream read-raw-bytes "drracket's pict-snip%"))
                       (λ () (send stream read-integer "drracket's pict-snip%")))])
                   (error 'pict-snip.rkt "could not read pict-snip from stream"))))
         (super-new))))

;; parse-pict-snip-from-bytes : bytes -> (or/c (is-a?/c pict-snip%) #f)
(define (parse-version-0-pict-snip-from-bytes bytes)
  (let/ec escape
    (define prt (open-input-bytes bytes))
    (define sexp (with-handlers ([exn:fail:read? (λ (x) (escape #f))])
                   (read prt)))
    (match sexp
      [`(,(? real? w) ,(? real? h) ,(? real? d) ,(? real? a) ,recorded-datum)
       (new pict-snip% [w w] [h h] [d d] [a a]
            [recorded-datum recorded-datum])]
      [else
       #f])))

(define (parse-version-1-pict-snip get-some-bytes get-a-number)
  (define prefix (bytes->string/utf-8 (get-some-bytes)))
  (define w (get-a-number))
  (define h (get-a-number))
  (define d (get-a-number))
  (define a (get-a-number))
  (define byteses
    (for/list ([_ (in-range (get-a-number))])
      (get-some-bytes)))
  (define rewriten-datum (read (open-input-bytes (get-some-bytes))))
  (define recorded-datum
    (let loop ([datum rewriten-datum])
      (cond
        [(pair? datum) (cons (loop (car datum)) (loop (cdr datum)))]
        [(equal? datum prefix)
         (begin0
           (car byteses)
           (set! byteses (cdr byteses)))]
        [else datum])))
  (new pict-snip% [w w] [h h] [d d] [a a]
       [recorded-datum recorded-datum]))

(define (write-version-1-of-snip w h d a recorded-datum f)
  (define unique-string (get-unique-string recorded-datum))
  (define unique-bytes (string->bytes/utf-8 unique-string))
  (send f put unique-bytes)
  (send f put w)
  (send f put h)
  (send f put d)
  (send f put a)
  (define-values (rewritten-datum byteses)
    (rewrite-recorded-datum recorded-datum unique-string))
  (send f put (length byteses))
  (for ([bytes (in-list byteses)])
    (send f put bytes))
  (define bp (open-output-bytes))
  (write rewritten-datum bp)
  (send f put (get-output-bytes bp)))

(define (rewrite-recorded-datum recorded-datum unique-string)
  (define byteses '())
  (define rewriten-datum
    (let loop ([recorded-datum recorded-datum])
      (match recorded-datum
        [(cons a b) (cons (loop a) (loop b))]
        [(? bytes?)
         (set! byteses (cons recorded-datum byteses))
         unique-string]
        [else recorded-datum])))
  (values rewriten-datum byteses))

(define (get-unique-string recorded-datum)
  (define prefix 0)
  (let loop ([recorded-datum recorded-datum])
    (cond
      [(pair? recorded-datum)
       (loop (car recorded-datum))
       (loop (cdr recorded-datum))]
      [(string? recorded-datum)
       (define m (regexp-match? #rx"^bmpref([0-9]+):" recorded-datum))
       (when m
         (define n (string->number (list-ref m 1)))
         (set! prefix (max (+ n 1) prefix)))]))
  (~a "bmpref" prefix ":"))

(module+ test
  (check-equal? (get-unique-string '(((1) 2))) "bmpref0")
  (check-equal? (get-unique-string '((("bmpref4") "bmpref1"))) "bmpref5"))
