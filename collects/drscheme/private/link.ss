
(module link mzscheme
  (require (lib "unitsig.ss")
	   "drsig.ss"
	   "init.ss"
           "text.ss"
           "export.ss"
           "main-before.ss"
           "app.ss"
           "main.ss")
  (provide drscheme@)
  
  (define drscheme@
    (compound-unit/sig
      (import)
      (link [init : drscheme:init^ (init@)]
            [text : drscheme:text^ (text@)]
            [export* : drscheme:export^ (export@ app text init)]
            [main-before : ()
                         (main-before@
                          app
                          (export* unit) 
                          (export* get/extend)
                          (export* language))]

	    [app : drscheme:app^ (app@ (export* unit)
				       (export* frame)
				       (export* help-desk))]
            [main : drscheme:main^ (main@
                                    app
                                    (export* unit) 
                                    (export* get/extend)
                                    (export* language))])
      (export (unit init drscheme:init)
              (unit text drscheme:text)
              (unit export* drscheme:export)
              (unit app drscheme:app)
              (unit main drscheme:main)))))
