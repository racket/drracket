
; For information about texpict, see texpicts.ss

(require-library "refer.ss")

(require-library "texpicts.ss" "texpict")

(begin-elaboration-time
 (require-library "invoke.ss"))


(define-values/invoke-unit/sig texpict^
  (require-library-unit/sig "texpictr.ss" "texpict"))
