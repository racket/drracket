(define-signature drscheme:tool^
  ((struct tool (name file))
   tools
   load/invoke-tool))

(define-signature drscheme:basis^
  (level->number
   level-symbols
   level-strings
   add-basis))

(define-signature drscheme:export^
  (frame-group))

(define-signature drscheme:load/link-tool^
  (load/link-tool))

(define-signature drscheme:unit^
  (snip-class% snip%))

(define-signature drscheme:frame^
  (frame-group
   frame%
   unit-frame%))

(define-signature drscheme:compound-unit^
  (frame% snip%))

(define-signature drscheme:edit^
  (console-edit%))

(define-signature drscheme:project^
  (scheme-project-frame%))

(define-signature drscheme:check^
  (advance-check%
   beginner-check%))

(define-signature drscheme:setup^ 
  (do-setup))

(define-signature drscheme:rep^
  (edit%))

(define-signature drscheme:app^
  ((open mred:application^)
   about-drscheme))