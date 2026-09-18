#lang at-exp racket/base
(require "private/module-lang-test-utils.rkt"
         "private/drracket-test-util.rkt"
         "module-lang-test-cases.rkt" ;; required for side-effect
         )

(fire-up-drracket-and-run-tests run-test)

;; Test mode:
(module test racket/base
  (require racket/port syntax/location)
  (define-values (inp outp) (make-pipe))
  (define tee-error-port (open-output-bytes 'tee-stderr))
  (define stderr (current-error-port))
  (void
   (thread
    (λ () (copy-port inp tee-error-port stderr))))
  (exit-handler
   (let ([old-exit-hdlr (exit-handler)])
     (λ (code)
       (define stderr-content-length
         (bytes-length (get-output-bytes tee-error-port #t)))
       (cond
         [(and (number? code) (zero? code) (> stderr-content-length 0))
          (write-string "non-empty stderr\n" stderr)
          (old-exit-hdlr 1)]
         [else
          (old-exit-hdlr code)]))))
  (void (putenv "PLTDRTEST" "yes"))
  (eval-jit-enabled #f)
  (parameterize ([current-error-port outp])
    (dynamic-require (quote-module-path "..") #f))
  (module config info
    (define timeout 800)))
