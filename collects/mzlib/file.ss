
(require-library "fileu.ss")
(require-library "functio.ss")
(require-library "string.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:file^
  mzlib:file@
  #f
  mzlib:string^
  mzlib:function^)
