#lang racket/base
(require "private/drracket-test-util.rkt")

(void (putenv "PLTDRDEBUG" "yes"))

(fire-up-drracket-and-run-tests 
 (λ ()
   (wait-for-drracket-frame)))
