(require-library "function.ss")
(require-library "file.ss")
(require-library "guis.ss" "tests" "utils")

(define-signature drscheme:test-util^
  (save-drscheme-window-as
   use-get/put-dialog
   do-execute
   test-util-error
   poll-until
   wait-for-computation
   wait-for-drscheme-frame
   wait-for-new-frame
   clear-definitions
   type-in-definitions
   type-in-interactions
   wait
   wait-pending
   get-sub-panel
   get-text-pos
   wait-for-button
   push-button-and-wait
   set-language-level!
   repl-in-edit-sequence?
   fetch-output
   has-error?))
