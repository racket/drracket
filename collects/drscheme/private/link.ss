(module link mzscheme
  (require (lib "unitsig.ss")
	   "drsig.ss"
	   "init.ss"
           "text.ss"
           "main-before.ss"
           "app.ss"
           "main.ss")
  (provide drscheme@)
  
  (define drscheme@
    (compound-unit/sig
      (import)
      (link [init : drscheme:init^ (init@)]
            [text : drscheme:text^ (text@)]
            [snip : drscheme:snip^ (snip@)]
            [load-handler : drscheme:load-handler^ (load-handler@)]
            [rep : drscheme:rep^ (rep@ init snip language app frame unit text load-handler help-interface)]
            [frame : drscheme:frame^ (frame@ unit app help-interface)]
            [unit : drscheme:unit^ (unit@ help-interface app frame text rep language get/extend snip)]
            [get/extend : drscheme:get/extend^ (get-extend@ unit frame rep)]
            [language : drscheme:language^ (language@ unit)]            
            [help-interface : drscheme:help-interface^ (help-interface@ frame language)]
            [main-before : () (main-before@ app unit get/extend language)]
	    [app : drscheme:app^ (app@ unit frame help-desk)]
            [main : drscheme:main^ (main@ app unit get/extend language)])
      (export))))
