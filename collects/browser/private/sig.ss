(module sig mzscheme
  (require (lib "unitsig.ss"))
  
  (provide relative-btree^
           bullet^
           browser:html^)

  (define-signature browser:html^
    (html-convert
     html-status-handler))
  
  (define-signature bullet-snip^
    (bullet-snip%
     bullet-size
     get-bullet-width))
  
  
  (define-signature relative-btree^
    (make-btree
     
     btree-get
     btree-put!
     
     btree-shift!
     
     btree-for-each
     btree-map)))
