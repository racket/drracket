(reference-library "zsigs.ss" "zodiac")
(reference-library "sigs.ss" "zodiac")
(reference-library "sparams.ss" "backward")
(reference-library "ariess.ss" "cogen")
(reference-library "gusrspcs.ss" "gusrspce")
(reference-library "pconvers.ss")

(define-signature drscheme:prefs^ ())

(define-signature drscheme:snip^ 
  (prompt-snip% equal-snip% separator-snip%))

(define-signature drscheme:interface^ 
  ((open zodiac:interface^)
   (struct zodiac-exn (message start-location end-location type))))

(define-signature drscheme:language^
  ((open plt:parameters^)
   (struct setting (vocabulary-symbol
		    case-sensitive?
		    allow-set!-on-undefined?
		    unmatched-cond/case-is-error?
		    allow-improper-lists?
		    sharing-printing?
		    abbreviate-cons-as-list?
		    signal-undefined
		    signal-not-boolean
		    eq?-only-compares-symbols?
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

(define-signature drscheme:parameters^
  (current-interactions-edit%
   current-definitions-edit%
   current-interactions-canvas%
   current-definitions-canvas%
   current-frame%))

(define-signature drscheme:unit^
  (frame% 
   snip-class% snip% make-bitmap
   definitions-canvas%
   definitions-edit%
   interactions-canvas%))

(define-signature drscheme:frame^
  (frame%))

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
  (edit%
   top-parameterization
   system-parameterization
   eval-thread-parameterization
   system-custodian
   (struct process/zodiac-finish (error?))))

(define-signature drscheme:app^
  ((open mred:application^)
   about-drscheme))

(define-signature drscheme:export^
  ((unit frame : drscheme:frame^)
   (unit unit : drscheme:unit^)
   (unit compound-unit : drscheme:compound-unit^)
   (unit parameters : drscheme:parameters^)
   (unit rep : drscheme:rep^)))

(begin-construction-time
 (define drscheme:tool-directories
   (quicksort (directory-list (collection-path "drscheme" "tools"))
	      string-ci<=?))
 `(begin
    ,@(let loop ([dirs drscheme:tool-directories])
	(cond
	  [(null? dirs) `(void)]
	  [else
	   (let ([f (find-library "sig.ss" "drscheme" "tools" (car dirs))])
	     (if (and f (file-exists? f))
		 `((reference ,f) ,@(loop (cdr dirs)))
		 (loop (cdr dirs))))]))))