
(require-relative-library "heads.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:head^
  (require-relative-library "headr.ss"))
