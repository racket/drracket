(module drsig mzscheme
  (require (lib "unitsig.ss")
	   (lib "framework-sig.ss" "framework"))
  
  (provide drscheme:get-collection^
           drscheme:main^
           drscheme:init^
           drscheme:snip^
           drscheme:language^
           drscheme:language/internal^
           drscheme:tools^
           drscheme:get/extend^
           drscheme:unit^
           drscheme:frame^
           drscheme:program^
           drscheme:text^
           drscheme:rep^
           drscheme:app^
           drscheme:draw-arrow^
	   drscheme:load-handler^
           drscheme:help-interface^
           drscheme:language-tower^
           drscheme:teachpack^)
  
  (define-signature drscheme:get-collection^
    (get-file/collection))
  
  (define-signature drscheme:main^ ())
  
  (define-signature drscheme:init^
    (original-output-port
     original-error-port
     primitive-eval
     primitive-load
     error-display-handler-message-box-title
     system-custodian
     system-eventspace
     first-dir))
  
  (define-signature drscheme:snip^ 
    (prompt-snip%
     equal-snip% 
     separator-snip%
     whole/part-number-snip%))
  
  (define-signature drscheme:language^
    (add-language))
  
  (define-signature drscheme:language/internal^
    ((struct language-settings (language settings))
     get-default-language-settings
     (open drscheme:language^)
     get-languages
     fill-language-menu
     settings-preferences-symbol
     language-dialog))
  
  (define-signature drscheme:tools^
    ((struct successful-tool (spec bitmap name))
     get-successful-tools))

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
  
  (define-signature drscheme:program^
    (frame%))
  
  (define-signature drscheme:text^
    (text<%>
     text%))
  
  (define-signature drscheme:setup^ 
    (do-setup))
  
  (define-signature drscheme:rep^
    ((struct text/pos (text start end))
     drs-bindings-keymap-mixin
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
    ((struct process-finish ())
     process
     process-file
     process-text
     drscheme-load-handler))
 
  (define-signature drscheme:help-interface^
    (help-desk
     open-url
     open-users-url))
  
  (define-signature drscheme:language-tower^
    ((struct simple-settings (case-sensitive printing-style show-sharing insert-newlines))
     make-simple-settings/parse
     simple-settings->vector
     
     language<%>
     module-based-language<%>
     simple-module-based-language<%>
     simple-module-based-language%
     simple-module-based-language->module-based-language-mixin
     module-based-language->language-mixin))

  (define-signature drscheme:teachpack^
    (load-teachpacks
     install-teachpacks
     marshall-teachpack-cache
     unmarshall-teachpack-cache
     
     new-teachpack-cache
     teachpack-cache?
     teachpack-cache-filenames
     set-teachpack-cache-filenames!)))
