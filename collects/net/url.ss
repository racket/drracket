(require-library "macro.ss")
(require-library "match.ss")
(require-library "file.ss")

(require-library "urlu.ss" "net")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:url^
  (compound-unit/sig
    (import
      (FILE : mzlib:file^))
    (link
      (URL : mzlib:url^
	(mzlib:url@ FILE)))
    (export
      (open URL)))
  #f
  mzlib:file^)
