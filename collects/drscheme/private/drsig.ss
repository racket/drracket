
(module drsig mzscheme
  (require (lib "unitsig.ss")
	   (lib "framework-sig.ss" "framework"))
  
  (provide drscheme:debug^
           drscheme:module-language^
           drscheme:get-collection^
           drscheme:main^
           drscheme:init^
           drscheme:snip^
           drscheme:language-configuration^
           drscheme:language-configuration/internal^
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
           drscheme:help-desk^
           drscheme:language^
           drscheme:teachpack^
           drscheme:multi-file-search^
           drscheme:tool^
	   drscheme^)

  (define-signature drscheme:debug^
    (make-debug-error-display-handler
     make-debug-eval-handler
     hide-backtrace-window))
  
  (define-signature drscheme:module-language^
    (add-module-language))
  
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
     system-namespace
     first-dir
     all-toplevel-collections))
  
  (define-signature drscheme:snip^ 
    (whole/part-number-snip%
     special<%>))
  
  (define-signature drscheme:language-configuration^
    (add-language
     get-settings-preferences-symbol))
  
  (define-signature drscheme:language-configuration/internal^
    ((struct language-settings (language settings))
     
     add-info-specified-languages
     get-default-language-settings
     (open drscheme:language-configuration^)
     get-languages
     settings-preferences-symbol
     
     ;; for the language dialog
     language-dialog
     add-new-teachpack
     clear-all-teachpacks))
  
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
     definitions-text<%>
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
    (use-number-snip
     drs-bindings-keymap-mixin
     current-rep
     text%
     context<%>))
  
  (define-signature drscheme:app^
    (check-new-version
     about-drscheme
     invite-tour
     add-language-items-to-help-menu
     switch-language-to))
  
  (define-signature drscheme:draw-arrow^
    (draw-arrow))
  
  (define-signature drscheme:load-handler^
    ((struct process-finish ())
     process
     process-file
     process-text
     drscheme-load-handler))
 
  (define-signature drscheme:help-desk^
    (help-desk
     open-url
     open-users-url))
  
  (define-signature drscheme:language^
    ((struct text/pos (text start end))
     (struct simple-settings (case-sensitive printing-style show-sharing insert-newlines))
     simple-settings->vector
     open-input-text
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
     set-teachpack-cache-filenames!))

  (define-signature drscheme:multi-file-search^
    (multi-file-search))
  
  (define-signature drscheme:tool^
    ((unit drscheme:frame : drscheme:frame^)
     (unit drscheme:unit : drscheme:unit^)
     (unit drscheme:rep : drscheme:rep^)
     (unit drscheme:get/extend : drscheme:get/extend^)
     (unit drscheme:language-configuration : drscheme:language-configuration^)
     (unit drscheme:language : drscheme:language^)
     (unit drscheme:snip : drscheme:snip^)
     (open ((unit drscheme:help-desk : drscheme:help-desk^)))))

  (define-signature drscheme^
    ((unit drscheme:teachpack : drscheme:teachpack^)
     (unit drscheme:language-configuration : drscheme:language-configuration/internal^))))
