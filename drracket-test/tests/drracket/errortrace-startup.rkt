#lang racket/base

#|

this file must avoid depending on racket/gui/base
as the startup of drracket will discard the initial
namespace and then require racket/gui/base in a new one. To
make this work, we wait until the dynamic-require
that starts drracket up completes and then we load
racket/gui/base and racket/class so that we're using
the same ones that drracket is using.

|#

;; set state to tell DrRacket to turn on
;; compiler/cm to build zos and errortrace
(void (putenv "PLTDRDEBUG" "yes")
      (putenv "PLTNOTOOLS" "yes"))

;; start up DrRacket
(dynamic-require 'drracket #f)

;; grab some exports from racket/gui/base and racket/class to
;; see if the drracket frame appeared.
(define get-top-level-windows (dynamic-require 'racket/gui/base 'get-top-level-windows))
(define yield (dynamic-require 'racket/gui/base 'yield))
(define application-quit-handler (dynamic-require 'racket/gui/base 'application-quit-handler))
(define object-interface (dynamic-require 'racket/class 'object-interface))
(define method-in-interface? (dynamic-require 'racket/class 'method-in-interface?))

(define sema (make-semaphore 0))
(void
 (thread
  (λ ()
    (let loop ([i 10])
      (cond
        [(zero? i)
         (error 'errortrace-startup.rkt "never saw the drracket frame")]
        [else
         (define actives (get-top-level-windows))
         (define drracket-frame-found?
           (for/or ([active (in-list actives)])
             (and active
                  (method-in-interface? 'get-execute-button (object-interface active)))))
         (unless drracket-frame-found?
           (sleep 1)
           (loop (- i 1)))]))
    (semaphore-post sema))))

(void (yield sema))
(exit)
