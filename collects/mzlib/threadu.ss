
(require-library "threads.ss")

(begin-elaboration-time
 (require-library "refer.ss"))

(define mzlib:thread@ (require-library-unit/sig "threadr.ss"))
