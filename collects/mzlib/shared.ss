
(begin-elaboration-time 
 (require-library "functios.ss"))

(begin-elaboration-time 
 (require-library "invoke.ss"))

(begin-elaboration-time
 (define-values/invoke-unit (shared)
   (require-library "sharedr.ss")))

(define-macro shared shared)


