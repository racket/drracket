
(begin-elaboration-time
 (require-library "launchers.ss" "launcher")
 (require-library "dynexts.ss" "dynext")
 (require-library "functios.ss")
 (require-library "files.ss")
 (require-library "sig.ss" "compiler"))

(define-signature setup-option^
  (verbose
   make-verbose
   compiler-verbose
   clean
   make-zo
   make-so
   make-launchers
   call-install
   pause-on-errors
   specific-collections
   archives))
