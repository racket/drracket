#lang racket
(require "../private/drracket-test-util.rkt"
         (prefix-in fw: framework)
         racket/gui/base
         rackunit)

(define (start-debugger-and-run-to-completion drr)
  (define debug-button
    (queue-callback/res (λ () (send drr get-debug-button))))
  (define resume-button
    (queue-callback/res (λ () (send drr get-resume-button))))

  ;; 1. click the debug button
  (fw:test:run-one (lambda () (send debug-button command)))
  
  ;; 2. wait for the go button to appear and be enabled
  (define (wait-for-debuggers-go-button)
    (queue-callback/res
     (λ ()
       (define visible?
         (let loop ([item resume-button])
           (cond
             [(is-a? item frame%) #t]
             [else
              (define parent (send item get-parent))
              (and parent (loop parent))])))
       (and visible?
            (send resume-button is-enabled?)))))

  (poll-until wait-for-debuggers-go-button)

  ;; 3. click the go button
  (fw:test:run-one (lambda () (send resume-button command)))

  ;; 4. wait for the computation to finish
  (wait-for-computation drr))


(fire-up-drracket-and-run-tests
 #:prefs '([plt:framework-pref:framework:autosaving-on? #f])
 (λ ()
   (define drr (wait-for-drracket-frame))
   (set-module-language!)
   (insert-in-definitions
    drr
    (~a "(define-syntax-rule\n"
        "  (THUNK (name x ...) code ...)\n"
        "  (define (name x ...)\n"
        "    (let ([x x] ...)\n"
        "      (λ () code ...))))\n"
        "\n"
        (~s '(THUNK (MAC) 12)) "\n"
        (~s '(THUNK (MAC.B) 21)) "\n"
        (~s '((MAC))) "\n"
        (~s '((MAC.B))) "\n"))

   (start-debugger-and-run-to-completion drr)
   
   (check-equal? (fetch-output drr) "12\n21")))
