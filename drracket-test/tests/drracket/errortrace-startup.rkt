#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/gui/base
         racket/class)

#|

this file cannot use anything except racket/gui/base
and things that it depends on during the thunk passed
to `fire-up-drracket-and-run-tests` or else errortrace
seems to get confused. I'm not sure exactly why

|#

(void (putenv "PLTDRDEBUG" "yes")
      (putenv "PLTNOTOOLS" "yes"))

(parameterize ([current-namespace (make-gui-namespace)])
  (fire-up-drracket-and-run-tests
   (λ ()
     (define (drracket-frame? frame)
       (method-in-interface? 'get-execute-button (object-interface frame)))
     (define (wait-for-drracket-frame-pred)
       (define actives (get-top-level-windows))
       (for/or ([active (in-list actives)])
         (and active
              (method-in-interface? 'get-execute-button (object-interface active))
              active)))
     (poll-until wait-for-drracket-frame-pred 400))))
