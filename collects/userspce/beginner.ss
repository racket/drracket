(begin-elaboration-time
 (require-library "coreflats.ss")
 (require-library "invoke.ss"))

(let ([u (require-library "coreflatr.ss")])
  (lambda ()
    (global-define-values/invoke-unit/sig mzlib:core-flat^ u)))
