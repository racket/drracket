(require-library "coreflats.ss")
(require-relative-library "ricedefs.ss")
(require-library "sig.ss" "mred")

(define-signature plt:userspace^
  ((open mred^)
   (open mzlib:core-flat^)))

