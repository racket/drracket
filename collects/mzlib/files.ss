
(begin-elaboration-time
 (require-library "functios.ss")
 (require-library "strings.ss"))

(define-signature mzlib:file^
  (find-relative-path
   explode-path
   normalize-path
   build-absolute-path
   build-relative-path
   filename-extension
   file-name-from-path
   path-only
   delete-directory/files
   make-directory*
   make-temporary-file
   find-library))
