(module browser mzscheme
  (require (lib "unit-sig.ss")
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred")
           "browser-sig.ss"
           "browser-unit.ss")
  
  (provide-signature-elements browser^)
  
  (define-values/invoke-unit/sig browser^ browser@ #f 
                                 mred^
                                 setup:plt-installer^))
