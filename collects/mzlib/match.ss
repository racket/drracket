
(begin-elaboration-time 
 (require-library "invoke.ss"))

(begin-elaboration-time
 (define-values/invoke-unit (match:set-error 
			     match:set-error-control match:error-control-param
			     match:error match match-lambda match-lambda* 
			     match-letrec match-let match-let*)
   (require-library "matchr.ss")))

(define-macro match match)
(define-macro match-lambda match-lambda)
(define-macro match-lambda* match-lambda*)
(define-macro match-letrec match-letrec)
(define-macro match-let match-let)
(define-macro match-let* match-let*)
