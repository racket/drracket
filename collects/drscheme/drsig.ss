(define-signature drscheme:tool^
  ((struct tool (name file callback))
   tools))

(define-signature drscheme:basis^
  (level->number
   level-symbols
   level-strings
   add-basis))

(define-signature drscheme:load/link-tool^
  (load/link-tool))

(define-signature drscheme:unit^
  (frame% snip-class% snip%))

(define-signature drscheme:frame^
  (group
   frame%))

(define-signature drscheme:compound-unit^
  (frame% snip%))

(define-signature drscheme:edit^
  (edit%))

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

(define-signature drscheme:export^
  ((unit drscheme:frame : drscheme:frame^)
   (unit drscheme:unit : drscheme:unit^)
   (unit drscheme:compound-unit : drscheme:compound-unit^)))
