(require-library "xmls.ss" "xml")
(define-values/invoke-unit/sig
 xml^
 (compound-unit/sig
   (import)
   (link
    (FUN : mzlib:function^ ((require-library "functior.ss")))
    (X : xml^ ((require-library "xmlr.ss" "xml") FUN)))
   (export (open X))))