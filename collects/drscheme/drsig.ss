(define-signature drscheme:tool^
  ((struct tool (name file))
   tools
   load/invoke-tool))

(define-signature drscheme:export^
  (console))

(define-signature drscheme^
  ((open mred:application^)))

(define-signature drscheme:aries^
  (transform frame% console-edit%))

(define-signature drscheme:load/link-tool^
  (load/link-tool))

(define-signature drscheme:frame^
  (scheme-project-member-frame%))

(define-signature drscheme:edit^
  (console-edit%))

(define-signature drscheme:project^
  (scheme-project-frame%
   (open mred:application^)))

(define-signature drscheme:check^
  (advance-check%
   beginner-check%))

(define-signature drscheme:setup^
  (do-setup))

(define-signature drscheme:spawn^
  (spawned-process-console-frame%
   spawned-process-console-edit%))
