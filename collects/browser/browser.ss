
(require-relative-library "sig.ss")

(require-library "file.ss")
(require-library "functio.ss")
(require-library "string.ss")

(require-library "url.ss" "net")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig browser^
  (require-relative-library "browserr.ss")
  #f
  mzlib:function^
  mzlib:string^
  mzlib:file^
  mzlib:url^
  mred^)
