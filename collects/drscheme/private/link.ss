(module link mzscheme
  (require "teachpack.ss"
	   "tools.ss"
           (lib "unitsig.ss")
	   "language-tower.ss"
           "language.ss"
           "drsig.ss"
	   "init.ss"
           "text.ss"
           "main-before.ss"
           "app.ss"
           "main.ss"
           "snip.ss"
           "rep.ss"
           "frame.ss"
           "unit.ss"
           "get-extend.ss"
           "help-interface.ss"
           "language-tower.ss")
  (provide drscheme@)
  
  (define drscheme@
    (compound-unit/sig
      (import)
      (link [init : drscheme:init^ (init@)]
            [text : drscheme:text^ (text@)]
            [snip : drscheme:snip^ (snip@)]
	    [teachpack : drscheme:teachpack^ (teachpack@)]
            [rep : drscheme:rep^ (rep@ init snip language app frame unit text help-interface)]
            [tower : drscheme:language-tower^ (language-tower@ rep)]
            [frame : drscheme:frame^ (frame@ unit app help-interface)]
            [unit : drscheme:unit^ (unit@ help-interface app frame text rep language get/extend snip)]
            [get/extend : drscheme:get/extend^ (get-extend@ unit frame rep)]
            [language : drscheme:language^ (language@ unit tower rep)]            
            [help-interface : drscheme:help-interface^ (help-interface@ frame language)]
	    [app : drscheme:app^ (app@ unit frame help-interface)]
            [main-before : () (main-before@ app unit get/extend language tower)]
            [tool : () (tools@ frame unit rep get/extend)]
            [main : drscheme:main^ (main@ app unit get/extend language)])
      (export))))
