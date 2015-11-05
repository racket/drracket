#lang racket
(require rackunit "harness.rkt")

(define prog
  #'(module anonymous-module racket
      (define (f x)
        (cond
          [(zero? x) x]
          [else
           (with-continuation-mark
             'x
             x
             (begin0
                 (begin
                   (unless (=
                            (length
                             (continuation-mark-set->list (current-continuation-marks) 'x))
                            1)
                     (error 'tests "too many continuation marks"))
                   (f (- x 1)))))]))
      (f 10)))

(check-not-exn
 (thunk (run-code-with-annotator prog)))
