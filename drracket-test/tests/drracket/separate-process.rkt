#lang at-exp racket/base
(require "private/module-lang-test-utils.rkt"
         "private/drracket-test-util.rkt"
         "module-lang-test-cases.rkt" ;; required for side-effect
         )

(fire-up-drracket-and-run-tests
 (λ ()
   (run-test #:separate-process? #t)))
