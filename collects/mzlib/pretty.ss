
(require-library "prettyu.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:pretty-print^
  mzlib:pretty-print@)


