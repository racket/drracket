(reference-library "match.ss")
(reference-library "sig.ss" "mred")
(reference-library "zsigs.ss" "zodiac")
(reference-library "sigs.ss" "zodiac")
(reference-library "sparams.ss" "backward")
(reference-library "ariess.ss" "cogen")
(reference-library "gusrspcs.ss" "gusrspce")
(reference-library "pconvers.ss")

(define-signature drscheme:get-collection^
  (get-file/collection))

(define-signature drscheme:main^ ())

(define-signature drscheme:init^
  (primitive-eval
   primitive-load
   system-eventspace
   top-parameterization
   system-parameterization
   eval-thread-parameterization
   system-custodian))

(define-signature drscheme:prefs^ ())

(define-signature drscheme:snip^ 
  (prompt-snip% equal-snip% separator-snip%))

(define-signature drscheme:face^
  (unitI compound-unitI 
	 unit-displayI 
	 unit-frameI compound-unit-frameI
	 unit-snipI compound-unit-snipI))

(define-signature drscheme:interface^ 
  ((open zodiac:interface^)
   (struct zodiac-exn (message start-location end-location type))))

(define-signature drscheme:language^
  ((struct setting (use-zodiac?
		    vocabulary-symbol
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
   use-zodiac
   setting-name
   install-language
   eq?-only-compares-symbols
   r4rs-style-printing
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

(define-signature drscheme:get/extend^
  (extend-interactions-edit%
   extend-definitions-edit%
   extend-interactions-canvas%
   extend-definitions-canvas%
   extend-unit-frame%
   get-interactions-edit%
   get-definitions-edit%
   get-interactions-canvas%
   get-definitions-canvas%
   get-unit-frame%))

(define-signature drscheme:unit^
  (frame% 
   snip-class% snip% make-bitmap
   definitions-canvas%
   definitions-edit%
   interactions-canvas%
   unit%
   make-unit))

(define-signature drscheme:frame^
  (make-frame%))

(define-signature drscheme:compound-unit^
  (frame% snip% compound-unit% make-compound-unit))

(define-signature drscheme:signature^
  (frame%))

(define-signature drscheme:program^
  (frame%))

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
   (struct process-finish (error?))))

(define-signature drscheme:exported-zodiac^ zodiac:system^)

(define-signature drscheme:zodiac^
  ((open drscheme:exported-zodiac^)
   current-vocabulary))

(define-signature drscheme:app^
  (about-drscheme))

(define-signature drscheme:export^
  ((unit frame : drscheme:frame^)
   (unit unit : drscheme:unit^)
   (unit compound-unit : drscheme:compound-unit^)
   (unit signature : drscheme:signature^)
   (unit program : drscheme:program^)
   (unit get/extend : drscheme:get/extend^)
   (unit rep : drscheme:rep^)))

(begin-construction-time
 (define drscheme:tool-directories (directory-list (collection-path "drscheme" "tools")))
 `(begin
    ,@(let loop ([dirs drscheme:tool-directories])
	(cond
	  [(null? dirs) `(void)]
	  [else
	   (let ([f (build-path (collection-path "drscheme") "tools" (car dirs) "sig.ss")])
	     (if (and f (file-exists? f))
		 `((reference ,f) ,@(loop (cdr dirs)))
		 (loop (cdr dirs))))]))))
