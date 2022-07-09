#lang at-exp racket/base
(require "private/drracket-test-util.rkt"
         framework/test
         racket/class
         racket/string
         rackunit)

(define (setup-racket/base-raw) (setup/rb "No debugging or profiling"))
(define (setup-racket/base-debug) (setup/rb "Debugging"))
(define (setup-racket/base-profile) (setup/rb "Debugging and profiling"))
(define (setup-racket/base-coverage) (setup/rb "Syntactic test suite coverage"))

(define (setup/rb which-rb)
  (set-module-language! #f)
  (test:set-radio-box-item! which-rb)
  (let ([f (test:get-active-top-level-window)])
    (test:button-push "OK")
    (wait-for-new-frame f)))

;; this test runs a program with an error and checks to make
;; sure that the stack dialog pops up and it either does or
;; does not have an errortrace-based stack, as appropriate
(define (check-errortrace-module errortrace-stack? setup-language)
  (define drracket-frame (wait-for-drracket-frame))

  (define ints-text (queue-callback/res (λ () (send drracket-frame get-interactions-text))))

  (setup-language)
  (clear-definitions drracket-frame)
  (insert-in-definitions
   drracket-frame
   @string-append{
    #lang errortrace racket/base
    (define (f)
      (raise (exn "message" (current-continuation-marks))))
    (f)
    })

  (do-execute drracket-frame)

  (define ints-content (queue-callback/res (λ () (send ints-text get-text))))
  (check string-contains?
         ints-content
         "message\n  errortrace...:\n")

  (define (not-string-contains? a b)
    (not (string-contains? a b)))

  (cond
    [errortrace-stack?
     (check string-contains?
            ints-content
            "omitted; see the stacktrace for more information")
     (check not-string-contains?
            ints-content
            "current-continuation-marks")]
    [else
     (check string-contains?
            ints-content
            "current-continuation-marks")
     (check not-string-contains?
            ints-content
            "omitted; see the stacktrace for more information")]))


(fire-up-drracket-and-run-tests
 (λ ()
   (check-errortrace-module #f setup-racket/base-raw)
   (check-errortrace-module #t setup-racket/base-debug)
   (check-errortrace-module #t setup-racket/base-profile)
   (check-errortrace-module #t setup-racket/base-coverage)
   ))
