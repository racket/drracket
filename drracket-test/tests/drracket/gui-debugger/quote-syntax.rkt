#lang racket
(require rackunit "harness.rkt")

(define prog
  #'(module anon racket
      (quote-syntax a #:local)))

(check-not-exn
 (thunk (run-code-with-annotator prog)))
