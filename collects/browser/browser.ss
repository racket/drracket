(module browser mzscheme
  (require (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           (lib "plt-installer.ss" "setup")
           "browser-sig.ss"
           "browser-unit.ss")
  
  (provide-signature-elements browser^)
  
  (define-values/invoke-unit/sig browser^ browser@ #f 
                                 mred^
                                 setup:plt-installer^))
