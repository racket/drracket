
(require-relative-library "sig.ss")

(require-library "file.ss")
(require-library "functio.ss")
(require-library "string.ss")

(require-library "url.ss" "net")

(invoke-open-unit/sig
 (require-relative-library "browserr.ss")
 #f
 mzlib:function^
 mzlib:string^
 mzlib:file^
 mzlib:url^
 mred^)
