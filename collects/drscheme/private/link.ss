(module link mzscheme
  (require "module-overview.ss"
           "multi-file-search.ss"
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
            [tools : drscheme:tools^ (tools@ frame unit rep get/extend language
                                           (language-configuration : drscheme:language-configuration^)
                                           help-desk init snip debug)]
            [text : drscheme:text^ (text@)]
            [snip : drscheme:snip^ (snip@)]
	    [teachpack : drscheme:teachpack^ (teachpack@ init)]
            [rep : drscheme:rep^
                 (rep@ init snip language-configuration language app 
                     frame unit text help-desk teachpack debug)]
            [language : drscheme:language^ (language@ rep snip debug teachpack tools)]
            [frame : drscheme:frame^ (frame@ unit app help-desk multi-file-search)]
            [module-overview : drscheme:module-overview^ (module-overview@ frame)]
            [debug : drscheme:debug^
                   (debug@ rep frame unit language language-configuration)]
            [unit : drscheme:unit^ 
                  (unit@ help-desk app frame text rep language-configuration 
                       get/extend snip teachpack module-overview debug)]
            [multi-file-search : drscheme:multi-file-search^ (multi-file-search@ frame)]
            [get/extend : drscheme:get/extend^ (get-extend@ unit frame rep)]
            [language-configuration : drscheme:language-configuration/internal^ 
                                    (language-configuration@ unit rep teachpack
                                                             init language app
                                                             tools)]
            [module-language : drscheme:module-language^ 
                             (module-language@ language-configuration language unit rep)]
            [help-desk : drscheme:help-desk^ (help-desk@ frame language-configuration)]
	    [app : drscheme:app^ (app@ unit frame language-configuration help-desk tools)]
            [main : () (main@ 
                        app unit get/extend language-configuration language teachpack
                        module-language snip tools debug)])
      (export
       (unit teachpack drscheme:teachpack)
       (unit language-configuration drscheme:language-configuration)))))
