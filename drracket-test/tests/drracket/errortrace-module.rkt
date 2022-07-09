#lang at-exp racket/base
(require "private/drracket-test-util.rkt"
         framework/test
         racket/class
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
(define (check-errortrace-module setup-language)
  (define drracket-frame (wait-for-drracket-frame))

  (define ints-text (queue-callback/res (λ () (send drracket-frame get-interactions-text))))

  (setup-language)
  (clear-definitions drracket-frame)
  (insert-in-definitions
   drracket-frame
   @string-append{
    #lang errortrace racket/base
    (error 'foobar)
    })

  (do-execute drracket-frame)

  (define ints-content (queue-callback/res (λ () (send ints-text get-text))))
  (check-regexp-match #rx"error: foobar\n  errortrace\\.\\.\\.:"
                      ints-content))

(fire-up-drracket-and-run-tests
 (λ ()
   (check-errortrace-module setup-racket/base-raw)
   (check-errortrace-module setup-racket/base-debug)
   (check-errortrace-module setup-racket/base-profile)
   (check-errortrace-module setup-racket/base-coverage)
   ))
