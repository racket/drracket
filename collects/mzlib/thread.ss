
(require-library "threadu.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:thread^
  mzlib:thread@)
