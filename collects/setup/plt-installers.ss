(module plt-installer-sig mzscheme
  (provide setup:plt-installer^)

  (define-signature setup:plt-installer^
    (run-installer
     on-installer-run)))
