(module link mzscheme
  (require "multi-file-search.ss"
           "debug.ss"
           "module-language.ss"
           "teachpack.ss"
	   "tools.ss"
           (lib "unitsig.ss")
	   "language.ss"
           "language-configuration.ss"
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
           "help-desk.ss")
  (provide drscheme@)
  
  (define drscheme@
    (compound-unit/sig
      (import)
      (link [init : drscheme:init^ (init@)]
            [text : drscheme:text^ (text@)]
            [snip : drscheme:snip^ (snip@)]
	    [teachpack : drscheme:teachpack^ (teachpack@ init)]
            [rep : drscheme:rep^
                 (rep@ init snip language-configuration language app 
                     frame unit text help-desk teachpack debug)]
            [language : drscheme:language^ (language@ rep snip debug)]
            [frame : drscheme:frame^ (frame@ unit app help-desk multi-file-search)]
            [unit : drscheme:unit^ 
                  (unit@ help-desk app frame text rep language-configuration get/extend snip teachpack)]
            [multi-file-search : drscheme:multi-file-search^ (multi-file-search@ frame)]
            [get/extend : drscheme:get/extend^ (get-extend@ unit frame rep)]
            [language-configuration : drscheme:language-configuration/internal^ 
                                    (language-configuration@ unit rep teachpack init language app)]
            [debug : drscheme:debug^
                   (debug@ rep frame unit language language-configuration)]
            [module-language : drscheme:module-language^ 
                             (module-language@ language-configuration language unit rep)]
            [help-desk : drscheme:help-desk^ (help-desk@ frame language-configuration)]
	    [app : drscheme:app^ (app@ unit frame language-configuration help-desk tool)]
            [main-before : () (main-before@ 
                               app unit get/extend language-configuration language teachpack
                               module-language)]
            [tool : drscheme:tools^ (tools@ frame unit rep get/extend language
                                          (language-configuration : drscheme:language-configuration^)
                                          help-desk init snip)]
            [main : drscheme:main^ (main@ app unit get/extend language-configuration language snip)])
      (export
       (unit teachpack drscheme:teachpack)
       (unit language-configuration drscheme:language-configuration)))))
