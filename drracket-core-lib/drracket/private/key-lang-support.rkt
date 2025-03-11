#lang racket
(require racket/gui/base)
(module+ test (require rackunit))
(provide current-editor range)

(define key-lang-range<%>
  (interface ()
    create-range))

(struct range (start end) #:transparent #:mutable)

(define key-lang-range-mixin
  (mixin ((class->interface text%)) (key-lang-range<%>)

    (define ranges '())
    (define/public (create-range start end) (set! ranges (cons (range start end) ranges)))
    (define/public (after-insert i-start len)
      (for ([a-range (in-list ranges)])
        (copy-into! a-range (range-insertion a-range i-start len))))
    (define/public (after-delete d-start len)
      (for ([a-range (in-list ranges)])
        (copy-into! a-range (range-deletion a-range d-start len))))
    (super-new)))

(define (copy-into! dest-range src-range)
  (cond
    [src-range
     (set-range-start! dest-range (range-start src-range))
     (set-range-end! dest-range (range-end src-range))]
    [else
     (set-range-start! dest-range #f)
     (set-range-end! dest-range #f)]))

(define (range-deletion a-range d-start len)
  (match-define (range r-start r-end) a-range)
  (cond
    [(not r-start) #f]
    [(<= d-start r-start)
     (define d-end (+ d-start len))
     (cond
       [(<= d-end r-start) (range (- r-start len) (- r-end len))]
       [(< d-end r-end)
        (define orig-range-len (- r-end r-start))
        (define amount-of-range-deleted (- d-end r-start))
        (define new-range-len
          (- orig-range-len amount-of-range-deleted))
        (range d-start (+ d-start new-range-len))]
       [else #f])]
    [(< d-start r-end)
     (define d-end (+ d-start len))
     (range r-start (if (< d-end r-end)
                        (- r-end len)
                        d-start))]
    [else
     a-range]))
(module+ test
  (check-equal? (range-deletion (range 3 10) 1 2)
                (range 1 8))
  (check-equal? (range-deletion (range 3 10) 2 2)
                (range 2 8))
  (check-equal? (range-deletion (range 3 10) 1 6)
                (range 1 4))
  (check-equal? (range-deletion (range 3 10) 1 5)
                (range 1 5))
  (check-equal? (range-deletion (range 3 10) 1 4)
                (range 1 6))
  (check-equal? (range-deletion (range 3 10) 5 2)
                (range 3 8))
  (check-equal? (range-deletion (range 3 10) 5 10)
                (range 3 5))
  (check-equal? (range-deletion (range 3 10) 1 11)
                #f)
  (check-equal? (range-deletion (range 3 10) 3 10)
                #f)
  (check-equal? (range-deletion (range 3 10) 10 2)
                (range 3 10)))

(define (range-insertion a-range i-start len)
  (match-define (range r-start r-end) a-range)
  (cond
    [(not r-start) #f]
    [(<= i-start r-start)
     (range (+ r-start len) (+ r-end len))]
    [(< i-start r-end)
     (range r-start (+ r-end len))]
    [else a-range]))
(module+ test
  (check-equal? (range-insertion (range 3 10) 1 2)
                (range 5 12))
  (check-equal? (range-insertion (range 3 10) 3 2)
                (range 5 12))
  (check-equal? (range-insertion (range 3 10) 4 2)
                (range 3 12))
  (check-equal? (range-insertion (range 3 10) 10 2)
                (range 3 10)))

(define current-editor (make-parameter #f))
