
(require-library "mathu.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:math^
  mzlib:math@)
