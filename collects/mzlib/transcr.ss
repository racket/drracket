
(require-library "transcru.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:transcript^
  mzlib:transcript@)
