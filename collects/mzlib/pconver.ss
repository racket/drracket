
(require-library "pconveru.ss")
(require-library "string.ss")
(require-library "functio.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:print-convert^
  mzlib:print-convert@
  #f
  mzlib:string^
  mzlib:function^)
