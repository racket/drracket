
(begin-elaboration-time
 (require-relative-library "spidey.ss"))

(define-signature mzlib:thread^
  (consumer-thread
   merge-input
   with-semaphore
   semaphore-wait-multiple

   dynamic-disable-break
   dynamic-enable-break
   make-single-threader))
