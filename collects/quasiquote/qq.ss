(require-library "urls.ss" "net")
(require-library "refer.ss")
(require-library "coreu.ss")
(require-library "qqu.ss" "quasiquote")

(define quasiquote:program@
  (compound-unit/sig
    (import)
    (link
      (MZLIB-CORE : mzlib:core^
	(mzlib:core@))
      (URL : mzlib:url^
	((require-library-unit/sig "urlr.ss" "net")
	  (MZLIB-CORE file)))
      (INTERFACE : quasiquote:graphical-interface^
	(quasiquote:graphical-interface@))
      (QUOTESTER : quasiquote:quotester^
	(quasiquote:quotester@ INTERFACE URL)))
    (export
      (open QUOTESTER))))

(define-values/invoke-unit/sig quasiquote:quotester^ quasiquote:program@)
