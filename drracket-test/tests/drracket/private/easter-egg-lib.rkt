#lang racket/base

#|

This file cannot depend on the framework.
It relies on first setting up the environment
and then loading the framework after that.

|#

(require "no-fw-test-util.rkt"
         racket/date
         racket/class
         racket/contract)

(provide 
 (contract-out
  [start-up-on-day
   (-> (and/c integer? (between/c 0 12))
       (and/c integer? (between/c 0 31))
       string?
       void?)]
  [start-up-and-check-car (-> void?)]))

(define (start-up-on-day month day what)
  (define the-seconds (find-seconds 1 0 0
                                    day month 
                                    (date-year (seconds->date (current-seconds)))))
  (printf "trying ~a, ~a/~a PLTDREASTERSECONDS=~a\n" what month day the-seconds)
  (unless (putenv "PLTDREASTERSECONDS" (number->string the-seconds))
    (error 'easter-egg-lib.rkt "putenv failed"))
  (start-up-and-check-car)
  
  ;; start up with (an approximation to) broken image files
  (unless (putenv "PLTDRBREAKIMAGES" "yes")
    (error 'easter-egg-lib.rkt "putenv.2 failed"))
  (printf "trying ~a, ~a/~a PLTDREASTERSECONDS=~a PLTDRBREAKIMAGES=yes\n" what month day the-seconds)
  (start-up-and-check-car)
  (environment-variables-set! (current-environment-variables)
                              #"PLTDRBREAKIMAGES"	 
                              #f))

(define (start-up-and-check-car)
  (fire-up-separate-drracket-and-run-tests
   (λ ()
     
     (define-syntax-rule
       (define/fw x)
       (define x (dynamic-require 'framework 'x)))
     
     (define/fw test:keystroke)
     (define/fw test:run-one)
     (define/fw test:use-focus-table)
     (define/fw test:get-active-top-level-window)
     (define/fw test:menu-select)
     (define/fw test:set-radio-box-item!)
     (define/fw test:button-push)
     (define/fw test:reraise-error)
     (define current-eventspace (dynamic-require 'racket/gui/base 'current-eventspace))
     
     (define (main)
       (queue-callback/res (λ () (test:use-focus-table #t)))
       (test:use-focus-table #t)
       (define drr-frame (wait-for-drracket-frame))
       (set-module-language! drr-frame)
       (queue-callback/res
        (λ () (send (send (send drr-frame get-definitions-text) get-canvas) focus)))
       (for ([x (in-string "(car 'x)")])
         (test:keystroke x))
       (let ([button (queue-callback/res (λ () (send drr-frame get-execute-button)))])
         (test:run-one (lambda () (send button command))))
       (wait-for-computation drr-frame)
       (define res 
         (queue-callback/res (λ () (send (send drr-frame get-interactions-text) get-text))))
       (unless (regexp-match #rx"contract violation.*expected: pair[?]" res)
         (eprintf "easter-egg-lib.rkt: interactions looks wrong; got: ~s\n" res)))

     (define (drracket-frame? frame)
       (method-in-interface? 'get-execute-button (object-interface frame)))
     
     (define (wait-for-drracket-frame [print-message? #f])
       (define (wait-for-drracket-frame-pred)
         (define active (test:get-active-top-level-window))
         (if (and active
                  (drracket-frame? active))
             active
             #f))
       (define drr-fr
         (or (wait-for-drracket-frame-pred)
             (begin
               (when print-message?
                 (printf "Select DrRacket frame\n"))
               (poll-until wait-for-drracket-frame-pred))))
       (when drr-fr
         (wait-for-events-in-frame-eventspace drr-fr))
       drr-fr)

     (define (wait-for-computation frame)
       (not-on-eventspace-handler-thread 'wait-for-computation)
       (queue-callback/res (λ () (verify-drracket-frame-frontmost 'wait-for-computation frame)))
       (define (computation-running?)
         (define-values (thd cust) (send (send frame get-current-tab) get-breakables))
         (and (or thd cust) #t))
       (define (wait-for-computation-to-start)
         (test:reraise-error)
         (computation-running?))
       (define (wait-for-computation-to-finish)
         (test:reraise-error)
         (not (computation-running?)))
       ;(poll-until wait-for-computation-to-start 60) ;; hm.
       (poll-until wait-for-computation-to-finish 60)
       (sync (system-idle-evt)))

     (define (verify-drracket-frame-frontmost function-name frame)
       (on-eventspace-handler-thread 'verify-drracket-frame-frontmost)
       (let ([tl (test:get-active-top-level-window)])
         (unless (and (eq? frame tl)
                      (drracket-frame? tl))
           (error function-name "drracket frame not frontmost: ~e (found ~e)" frame tl))))
     
     (define (set-module-language! drr-frame)
       (test:menu-select "Language" "Choose Language…")
       (define language-dialog (wait-for-new-frame drr-frame))
       (test:set-radio-box-item! #rx"The Racket Language")
       
       (with-handlers ([exn:fail? (lambda (x) (void))])
         (test:button-push "Show Details"))
       
       (test:button-push "Revert to Language Defaults")
       
       (test:button-push "OK")
       (define new-frame (wait-for-new-frame language-dialog))
       (unless (eq? new-frame drr-frame)
         (error 'set-module-level! 
                "didn't get drracket frame back, got: ~s (drr-frame ~s)\n"
                new-frame
                drr-frame)))
     
     (define (wait-for-new-frame old-frame)
       (wait-for-something
        (λ ()
          (define active (test:get-active-top-level-window))
          (and active 
               (not (eq? active old-frame))
               active))))
     
     (define (wait-for-something thing?)
       (define total-time-to-wait 20) ;; in seconds
       (define time-to-wait-in-one-iteration 1/10) ;; also in seconds
       (let loop ([n (/ total-time-to-wait time-to-wait-in-one-iteration)])
         (cond
           [(thing?) => values]
           [(zero? n)
            (error 'wait-for-something "~s didn't happen" thing?)]
           [else
            (sleep time-to-wait-in-one-iteration)
            (loop (- n 1))])))
     
     (main))))
