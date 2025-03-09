#lang racket/base

(require (for-label racket
                    racket/gui/base)
         scribble/manual)

(provide HtDP
         drlang
         (all-from-out scribble/manual)
         (for-label (all-from-out racket 
                                  racket/gui/base)))

(define HtDP
  (italic "How to Design Programs"))

(define (drlang . s)
  (apply onscreen s))
