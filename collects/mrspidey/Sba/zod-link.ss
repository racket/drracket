;; zod-link.ss

(define mrspidey:zodiac@;; mrspidey:zodiac^
  (compound-unit/sig
    (import 
      (CDL : mrspidey:CDL^)
      (INTERACTION : mrspidey:interaction^))
    (link
      [PARAMETERS : 
        plt:parameters^
        (mrspidey:zodiac:parameters@)]
      [INTERFACE : 
        zodiac:interface^
        (mrspidey:zodiac:interface@ INTERACTION)]
      [Z : 
        zodiac:system^
        (zodiac:system@ INTERFACE PARAMETERS (CORE pretty-print@) (CORE file@))]
      [CORE : 
        mzlib:core^
        ((reference-library-unit/sig "corer.ss"))]
      [AUX : mrspidey:zodiac-aux^ (mrspidey:zodiac-aux@ CDL Z)])
    (export (open Z) (open AUX))))
