(require-library "macro.ss")
(require-library "refer.ss")
;(require-library "match.ss")
(require-library "dates.ss")
(require-library "sig.ss" "mred")
(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")
(require-library "sig.ss" "stepper")
(require-library "params.ss" "userspce")
(begin-elaboration-time
 (require-library "sig.ss" "guserspce"))
(require-library "turtles.ss" "graphics")
(require-library "pconvers.ss")
(require-library "classd.ss")
(require-library "launchers.ss" "launcher")
(require-library "frameworks.ss" "framework")
(require-library "compiles.ss" "dynext")
(require-library "links.ss" "dynext")
(require-relative-library "load-handlers.ss")
(require-library "plt-installers.ss" "setup")
(require-library "get-infos.ss" "setup")

(require-library "sig.ss" "help")

(define-signature drscheme:get-collection^
  (get-file/collection))

(define-signature drscheme:main^ ())

(define-signature drscheme:init^
  )

(define-signature drscheme:snip^ 
  (prompt-snip%
   equal-snip% 
   separator-snip%
   whole/part-number-snip%))

(define-signature drscheme:language^
  (fill-language-menu
   settings-preferences-symbol
   language-dialog))

(define-signature drscheme:load/link-tool^
  (load/link-tool))

(define-signature drscheme:get/extend^
  (extend-interactions-text
   extend-definitions-text
   extend-interactions-canvas
   extend-definitions-canvas
   extend-unit-frame
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
   program-editor-mixin
   open-drscheme-window))

(define-signature drscheme:frame^
  (name-message%
   draw-button-label
   calc-button-min-sizes
   <%>
   mixin
   basics-mixin
   basics<%>))

(define-signature drscheme:signature^
  (frame%))

(define-signature drscheme:program^
  (frame%))

(define-signature drscheme:text^
  )

(define-signature drscheme:project^
  (scheme-project-frame%))

(define-signature drscheme:check^
  (advance-check%
   beginner-check%))

(define-signature drscheme:setup^ 
  (do-setup))

(define-signature drscheme:rep^
  (drs-bindings-keymap-mixin
   text%
   context<%>
   show-interactions-history))

(define-signature drscheme:app^
  )

(define-signature drscheme:draw-arrow^
  (draw-arrow))

(define-signature drscheme:export^
  ((unit snip : drscheme:snip^)
   (unit interface : drscheme:interface^)
   (unit basis : plt:basis^)
   (unit frame : drscheme:frame^)
   (unit unit : drscheme:unit^)
   (unit program : drscheme:program^)
   (unit get/extend : drscheme:get/extend^)
   (unit load-handler : drscheme:load-handler^)
   (unit rep : drscheme:rep^)
   (unit language : drscheme:language^)
   (unit help-desk : help:drscheme-interface^)
   (unit help-info : help:get-info^)))

(define-signature drscheme^
  ((open mzlib:core^)
   (open mzlib:print-convert^)
   (open framework^)
   (open setup:plt-installer^)
   (open setup:info^)

   (unit zodiac : zodiac:system^)
   (unit plt:aries : plt:aries^)

   (unit drscheme:init : drscheme:init^)
   (unit drscheme:text : drscheme:text^)
   (unit drscheme:export : drscheme:export^)
   (unit drscheme:app : drscheme:app^)
   (unit drscheme:main : drscheme:main^)))

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

