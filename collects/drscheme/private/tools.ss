(module tool mzscheme
  (require (lib "unitsig.ss")
           "drsig.ss")
  
  (provide tool@)
  (define tool@
    (unit/sig ()
      (import [drscheme:frame^ : drscheme:frame^]
              [drscheme:unit^ : drscheme:unit^]
              [drscheme:rep : drscheme:rep^]
              [drscheme:get/extend : drscheme:get/extend^])
      
      (void))))
