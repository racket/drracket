(require-library "match.ss")
(require-library "sig.ss" "mred")
(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")
(require-library "ariess.ss" "cogen")
(require-library "sig.ss" "userspce")
(require-library "gusrspcs.ss" "gusrspce")
(require-library "pconvers.ss")
(require-library "frameworks.ss" "framework")

(require-library "mred-interfaces.ss" "framework")

(require-library "errortrace.ss" "errortrace")
(error-print-width 80)

(define-signature drscheme:get-collection^
  (get-file/collection))

(define-signature drscheme:main^ ())

(define-signature drscheme:init^
  (original-output-port
   original-error-port
   primitive-eval
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

(define-signature drscheme:language^
  (fill-language-menu))

(define-signature drscheme:tool^
  ((struct tool (name file callback))
   tools))

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
   ;snip-class% snip%
   ;make-bitmap
   definitions-canvas%
   definitions-edit%
   interactions-canvas%
   unit%
   open-as-unit
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
   process-edit/zodiac
   process-edit/no-zodiac
   show-interactions-history))

(define-signature drscheme:app^
  (about-drscheme))

(define-signature drscheme:export^
  ((unit basis : userspace:basis^)
   (unit frame : drscheme:frame^)
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
