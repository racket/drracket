#lang racket

(require "private/drracket-test-util.rkt"
         framework/test
         racket/gui/base)

;; When a #lang's reader fails, DrRacket shows the error only in its
;; language-error panel, formatting it with the error display handler in
;; effect when DrRacket starts. Copy any error formatted for somewhere other
;; than stderr to stderr, so that a failure here shows the reader's error
;; and not only the later errors it leads to.
(define original-error-display-handler (error-display-handler))
(define original-error-port (current-error-port))
(error-display-handler
 (λ (msg exn)
   (original-error-display-handler msg exn)
   (unless (eq? (current-error-port) original-error-port)
     (parameterize ([current-error-port original-error-port])
       (original-error-display-handler msg exn)))))

(fire-up-drracket-and-run-tests
 #:prefs '([plt:framework-pref:framework:autosaving-on? #f])
 (λ ()
   (define drr (wait-for-drracket-frame))

   (set-module-language! #f)
   (test:set-check-box! #rx"Populate “compiled” directories" #f)
   (test:button-push "OK")
   (wait-for-drracket-frame) ;; make sure language dialog is closed

   (define init-str
     (queue-callback/res
      (λ ()
        (define t (send drr get-definitions-text))
        (define init-str (send t get-text))
        (send t set-position 0 (send t last-position))
        (send t insert "#lang at-exp racket/base")
        init-str)))
   (unless (string=? init-str "#lang racket\n")
     (error "wrong initial text: " init-str))

   ;; There's a 200-msec timer to update the #lang line, so wait
   (queue-callback/res
    (λ ()
      (sleep/yield 0.5)))

   (test:menu-select "Racket" "Run")

   (test:menu-select "File" "New Tab")
   (sync (system-idle-evt))

   (define str
     (queue-callback/res
      (λ ()
        (define t (send drr get-definitions-text))
        (sleep 3)
        (send t get-text))))

   (unless (string=? str "#lang at-exp racket/base\n")
     (error "wrong text: " str))))
