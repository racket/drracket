
(require-library "inflates.ss")

(begin-elaboration-time
 (require-library "refer.ss"))

(define mzlib:inflate@ (require-library-unit/sig "inflater.ss"))
