(module drsig mzscheme
  (require (lib "unitsig.ss")
           (lib "sig.ss" "userspce")
	   (lib "framework-sig.ss" "framework")
	   (lib "sig.ss" "stepper"))
  
  (provide drscheme:get-collection^
           drscheme:main^
           drscheme:init^
           drscheme:snip^
           drscheme:language^
           drscheme:load/link-tool^
           drscheme:get/extend^
           drscheme:graph^
           drscheme:unit^
           drscheme:frame^
           drscheme:signature^
           drscheme:program^
           drscheme:text^
           ;drscheme:project^  ;; ???
           ;drscheme:check^
           drscheme:rep^
           drscheme:app^
           drscheme:draw-arrow^
           drscheme:export^
           drscheme^)
  
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
    (text<%>
     text%))
  
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
    (check-new-version
     invite-tour
     about-drscheme))
  
  (define-signature drscheme:draw-arrow^
    (draw-arrow))
  
  (define-signature drscheme:load-handler^
    (provide
     process-text/zodiac
     process-text/no-zodiac
     process-text
     drscheme-load-handler))
 
  (define-signature drscheme:help-interface^
    (help-desk
     open-url
     open-users-url))
  
  (define-signature drscheme:export^
    ((unit snip : drscheme:snip^)
     (unit basis : plt:basis^)
     (unit frame : drscheme:frame^)
     (unit unit : drscheme:unit^)
     (unit program : drscheme:program^)
     (unit get/extend : drscheme:get/extend^)
     (unit load-handler : drscheme:load-handler^)
     (unit rep : drscheme:rep^)
     (unit language : drscheme:language^)
     (unit help-desk : drscheme:help-interface^)))
  
  (define-signature drscheme^
    ((open framework^)
     
     (unit plt:aries : plt:aries^)
     
     (unit drscheme:init : drscheme:init^)
     (unit drscheme:text : drscheme:text^)
     (unit drscheme:export : drscheme:export^)
     (unit drscheme:app : drscheme:app^)
     (unit drscheme:main : drscheme:main^))))
