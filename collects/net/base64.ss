
(require-relative-library "base64s.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:base64^
  (require-relative-library "base64r.ss"))
