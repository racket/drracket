
(module htmltext mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           "private/sig.ss"
           "private/html.ss"
           "private/bullet.ss"
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred")
	   (lib "browser.ss" "net"))
           
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
      add-scheme-callback
      add-thunk-callback
      post-url))

  (define url-delta (make-object style-delta% 'change-underline #t))
  (send url-delta set-delta-foreground "blue")

  (define (html-text-mixin %)
    (unless (% . implementation? . (class->interface text%))
      (raise-type-error 'render-html-to-text "subclass of text%" %))
    (class* % (html-text<%>)
      (inherit change-style set-clickback)
      
      (define/public (get-url) #f)
      (define/public (set-title s) (void))
      (define/public (add-link pos end-pos url-string)
	(set-clickback pos end-pos (lambda (e start-pos eou-pos)
				     (send-url url-string))))
      (define/public (add-tag label pos) (void))
      (define/public (make-link-style pos endpos) 
	(change-style url-delta pos endpos))
      (define/public (add-scheme-callback pos endpos scheme) (void))
      (super-instantiate ())))

  (define (render-html-to-text port text%-obj img-ok? eval-ok?)
    (unless (input-port? port)
      (raise-type-error 'render-html-to-text "input port" 0 (list port text%-obj)))
    (unless (text%-obj . is-a? . html-text<%>)
      (raise-type-error 'render-html-to-text "html-text<%> object" 0 (list port text%-obj)))
    (parameterize ([html-eval-ok eval-ok?]
                   [html-img-ok img-ok?])
      (html-convert port text%-obj)))
  
  (provide html-text<%>
	   html-text-mixin
           render-html-to-text))

