(define drscheme:toplevel-tools
  (list (list "Syntax Checker" (build-path "drscheme" "mrslatex"))
	(list "Stepper" (build-path "drscheme" "donkstub"))
	(list "Static Debugger" (build-path "drscheme" "spidstub"))

;;          this is the example tool.
	;(list "Toy" (build-path "drscheme" "toy"))

	))

(reference (begin-elaboration-time (build-path 'up "zodiac" "sigs.ss")))
(reference (begin-elaboration-time (build-path 'up "zodiac" "zsigs.ss")))
(reference (begin-elaboration-time (build-path 'up "lib" "sparams.ss")))
(reference (begin-elaboration-time (build-path 'up "lib" "ariess.ss")))
(reference (begin-elaboration-time (build-path 'up "lib" "gusrspcs.ss")))

(reference-library "pconvers.ss")

(define-signature drscheme:snip^ 
  (prompt-snip% equal-snip% separator-snip%))

(define-signature drscheme:interface^ 
  ((open zodiac:interface^)
   (struct zodiac-exn (start-location end-location type))))

(define-signature drscheme:language^
  ((open plt:parameters^)
   (struct setting (vocabulary-symbol
		    case-sensitive?
		    allow-set!-on-undefined?
		    unmatched-cond/case-is-error?
		    allow-improper-lists?
		    sharing-printing?
		    printing))
   fill-language-menu))

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

(define-signature drscheme:middle^
  ((unit drscheme:setup : drscheme:setup^)
   (unit drscheme:tool : drscheme:tool^)
   (unit drscheme:rep : drscheme:rep^)
   (unit drscheme:unit : drscheme:unit^)
   (unit drscheme:frame : drscheme:frame^)
   (unit drscheme:aries : plt:aries^)
   (unit drscheme:compound-unit : drscheme:compound-unit^)
   (unit zodiac : zodiac:system^)))
(define-signature drscheme:all^
  ((open drscheme:middle^)
   (open mred^)
   (open mred:application^)
   (open mzlib:core^)
   (open mzlib:trigger^)
   (open mzlib:print-convert^)))

(define-signature drscheme:export^
  ((unit drscheme:frame : drscheme:frame^)
   (unit drscheme:unit : drscheme:unit^)
   (unit drscheme:compound-unit : drscheme:compound-unit^)))
