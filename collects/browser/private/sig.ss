(module sig mzscheme
  (require (lib "unitsig.ss"))
  
  (provide relative-btree^
           bullet^
           html-export^
           html^)

  (define-signature html-export^
    (html-img-ok
     html-eval-ok))
  
  (define-signature html^
    (html-convert
     html-status-handler
     (open html-export^)))
  
  (define-signature bullet^
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
