
(begin-elaboration-time
 (require-library "strings.ss")
 (require-library "functios.ss"))

(define-signature mzlib:print-convert^
  (show-sharing
   constructor-style-printing
   quasi-read-style-printing
   abbreviate-cons-as-list
   whole/fractional-exact-numbers
   booleans-as-true/false
   
   print-convert
   print-convert-expr
   build-share
   get-shared
   current-read-eval-convert-print-prompt 
   install-converting-printer

   current-build-share-name-hook
   current-build-share-hook
   current-print-convert-hook))

