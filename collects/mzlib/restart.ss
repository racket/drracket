
(require-library "cmdline.ss")
(require-library "restartu.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:restart^
  mzlib:restart@
  #f
  mzlib:command-line^)
