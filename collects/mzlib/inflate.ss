

(require-library "inflateu.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:inflate^
  mzlib:inflate@)

 
