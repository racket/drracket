
(module link mzscheme
  (require (lib "unitsig.ss")
	   "drsig.ss"
	   "init.ss"
           "text.ss"
           "export.ss"
           "main-before.ss"
           "app.ss"
           "tool.ss"
           "main.ss")
  (provide drscheme@)
  
  (define drscheme@
    (compound-unit/sig
      (import)
      (link [init : drscheme:init^ (init@ mred)]
            [text : drscheme:text^ (text@ framework)]
            [export* : drscheme:export^ (export@ app text init cogen)]
            [main-before : drscheme:main-before^
                         (main-before@
                          app
                          (export* unit) (export* get/extend)
                          (export* language) (export* basis))]
            
            [cogen : plt:aries^
                   ((require-library-unit/sig "link.ss" "stepper-graphical") export*)]
            
            
            [app : drscheme:app^ (app@ (export* unit) (export* frame) (export* help-desk))]
            [tool : () (tool@ export*)]
            [main : drscheme:main^ (main@
                                    app
                                    (export* unit) (export* get/extend)
                                    (export* language) (export* basis))])
      (export (unit cogen plt:aries)
              (unit init drscheme:init)
              (unit text drscheme:text)
              (unit export* drscheme:export)
              (unit tool drscheme:tool)
              (unit app drscheme:app)
              (unit main drscheme:main)))))
