
(require-relative-library "sig.ss")

(require-library "file.ss")
(require-library "functio.ss")
(require-library "string.ss")

(invoke-open-unit/sig
 (require-relative-library "browserr.ss")
 #f
 mzlib:function^
 mzlib:string^
 mzlib:file^
 mred^)

