
(module htmltext mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           "private/sig.ss"
           "private/html.ss"
           "private/bullet.ss"
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred"))
           
  (define-values/invoke-unit/sig 
   html^
   (compound-unit/sig
     (import (MRED : mred^))
     (link [HTML : html^ (html@ BULLET MRED)]
           [BULLET : bullet^ (bullet@ MRED)])
     (export (open HTML)))
   #f
   mred^)
  
  (define html-text<%>
    (interface ((class->interface text%))
      get-url
      set-title
      add-link
      add-tag
      make-link-style
      add-scheme-callback))

  (define (render-html-to-text port text%-obj img-ok? eval-ok?)
    (unless (input-port? port)
      (raise-type-error 'render-html-to-text "input port" 0 (list port text%-obj)))
    (unless (text%-obj . is-a? . html-text<%>)
      (raise-type-error 'render-html-to-text "html-text<%> object" 0 (list port text%-obj)))
    (parameterize ([html-eval-ok eval-ok?]
                   [html-img-ok img-ok?])
      (html-convert port text%-obj)))
  
  (provide html-text<%>
           render-html-to-text))

