#lang racket
(require "key-lang-support.rkt")
(provide #%app new-range)
(define (new-range start end)
  (send (current-editor) add-range (range start end)))
