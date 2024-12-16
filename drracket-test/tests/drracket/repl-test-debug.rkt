#lang racket/base
(require "private/drracket-test-util.rkt"
         "private/repl-test.rkt")
(fire-up-drracket-and-run-tests (λ () (run-test '(debug))))

(module+ test
  (module config info
    (define timeout 300)))
