(module plt-installer-sig mzscheme
  (require (lib "unitsig.ss"))
  (provide setup:plt-installer^)
  (define-signature setup:plt-installer^
    (run-installer
     on-installer-run)))
