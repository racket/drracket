(module browser-sig mzscheme
  (require (lib "unitsig.ss")
           "private/sig.ss")
  
  (provide browser^)

  (define-signature browser^
    (open-url
     (struct exn:file-saved-instead (pathname))
     (struct exn:cancelled ())
     
     hyper-style-list
     hyper-text-mixin
     hyper-text%
     
     hyper-canvas-mixin
     hyper-canvas%
     
     hyper-panel-mixin
     hyper-panel%
     
     hyper-frame-mixin
     hyper-frame%
     
     editor->page
     page->editor
     
     bullet-size)))
