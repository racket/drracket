(require-library "refer.ss")
(require-library "match.ss")
(require-library "sig.ss" "mred")
(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")
(require-library "ariess.ss" "cogen")
(require-library "params.ss" "userspce")
(require-library "sig.ss" "userspce")
(require-library "turtles.ss" "graphics")
(require-library "pconvers.ss")

(require-library "frameworks.ss" "framework")

(require-library "sig.ss" "help")

(define-signature drscheme:get-collection^
  (get-file/collection))

(define-signature drscheme:main^ ())

(define-signature drscheme:init^
  (original-output-port
   original-error-port
   primitive-eval
   primitive-load
   system-custodian
   system-eventspace
   first-dir))

(define-signature drscheme:prefs^ ())

(define-signature drscheme:snip^ 
  (prompt-snip% equal-snip% separator-snip%))

(define-signature drscheme:language^
  (fill-language-menu))

(define-signature drscheme:tool^
  ((struct tool (name file callback))
   tools))

(define-signature drscheme:load/link-tool^
  (load/link-tool))

(define-signature drscheme:get/extend^
  (extend-interactions-text%
   extend-definitions-text%
   extend-interactions-canvas%
   extend-definitions-canvas%
   extend-unit-frame%
   get-interactions-text%
   get-definitions-text%
   get-interactions-canvas%
   get-definitions-canvas%
   get-unit-frame%))

(define-signature drscheme:graph^
  (graph-pasteboard%
   node-snip%))

(define-signature drscheme:unit^
  (frame% 
   make-bitmap
   definitions-canvas%
   definitions-text%
   interactions-canvas%
   open-drscheme-window))

(define-signature drscheme:frame^
  (<%>
   mixin
   basics-mixin
   basics<%>))

(define-signature drscheme:signature^
  (frame%))

(define-signature drscheme:program^
  (frame%))

(define-signature drscheme:text^
  (text%))

(define-signature drscheme:project^
  (scheme-project-frame%))

(define-signature drscheme:check^
  (advance-check%
   beginner-check%))

(define-signature drscheme:setup^ 
  (do-setup))

(define-signature drscheme:rep^
  (text%
   invoke-library
   process-text/zodiac
   process-text/no-zodiac
   show-interactions-history))

(define-signature drscheme:app^
  (about-drscheme))

(define-signature drscheme:export^
  ((unit interface : drscheme:interface^)
   (unit basis : userspace:basis^)
   (unit frame : drscheme:frame^)
   (unit unit : drscheme:unit^)
   (unit program : drscheme:program^)
   (unit get/extend : drscheme:get/extend^)
   (unit rep : drscheme:rep^)
   (unit help-desk : help:drscheme-interface^)))

(begin-elaboration-time
 (define drscheme:tool-directories (directory-list (collection-path "drscheme" "tools")))
 `(begin
    ,@(let loop ([dirs drscheme:tool-directories])
	(cond
	  [(null? dirs) `(void)]
	  [else
	   (let ([f (build-path (collection-path "drscheme") "tools" (car dirs) "sig.ss")])
	     (if (and f (file-exists? f))
		 `((require-library "sig.ss" "drscheme" "tools" ,(car dirs))
                   ,@(loop (cdr dirs)))
		 (loop (cdr dirs))))]))))
