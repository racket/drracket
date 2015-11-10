#lang racket

(require rackunit "harness.rkt")

(define mod1
  #`(module anon racket
     (letrec ([test
               (letrec ()
                 (#,(break/test #'test))
                 (lambda () 1))])
       (test))))

(define mod2
  #`(module anon racket
      (define (thingy exp)
        (if (list? exp)
            (letrec ([mapped
                      (begin #,(break/test #'mapped)
                             (map thingy exp))])
              (apply (first mapped)
                     (rest mapped)))
            exp))))

(check-not-exn
 (thunk
  (run-code-with-annotator mod1)))
(check-not-exn
 (thunk
  (run-code-with-annotator mod2)))
