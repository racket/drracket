(require-library "mails.ss" "net")
(require-library "mailu.ss" "net")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:sendmail^
  mzlib:sendmail@)
