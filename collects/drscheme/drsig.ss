(reference (begin-elaboration-time (build-path plt:home-directory "zodiac" "zsigs.ss")))
(reference (begin-elaboration-time (build-path plt:home-directory "zodiac" "sigs.ss")))
(reference (begin-elaboration-time (build-path plt:home-directory "lib" "sparams.ss")))
(reference (begin-elaboration-time (build-path plt:home-directory "lib" "ariess.ss")))
(reference (begin-elaboration-time (build-path plt:home-directory "lib" "gusrspcs.ss")))

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
  (current-interactions-edit%
   current-definitions-edit%
   current-interactions-canvas%
   current-definitions-canvas%
   current-frame%
   frame% snip-class% snip% make-bitmap))

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


(begin-elaboration-time
 (define drscheme:tool-directories
   (quicksort (directory-list (build-path plt:home-directory
					  "drscheme"
					  "tools"))
	      string-ci<=?))
 `(begin
    ,@(let loop ([dirs drscheme:tool-directories])
	(cond
	  [(null? dirs) `(void)]
	  [else
	   (let ([f (build-path plt:home-directory
				"drscheme" "tools" 
				(car dirs) "sig.ss")])
	     (if (file-exists? f)
		 `((reference ,f) ,@(loop (cdr dirs)))
		 (loop (cdr dirs))))]))))