
(begin-elaboration-time
 (define-values (define-values/invoke-unit
		  define-values/invoke-unit/sig
		  global-define-values/invoke-unit
		  global-define-values/invoke-unit/sig)
   (invoke-unit (require-library "invoker.ss"))))

(define-macro define-values/invoke-unit define-values/invoke-unit)
(define-macro define-values/invoke-unit/sig define-values/invoke-unit/sig)
(define-macro global-define-values/invoke-unit global-define-values/invoke-unit)
(define-macro global-define-values/invoke-unit/sig global-define-values/invoke-unit/sig)

 