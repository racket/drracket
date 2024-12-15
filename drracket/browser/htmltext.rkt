#lang racket/base
(require browser/external
         mred/mred-sig
         mred/mred-unit
         net/url
         net/url-sig
         racket/class
         racket/gui/base
         racket/unit
         "browser-sig.rkt"
         "private/bullet.rkt"
         "private/html.rkt"
         "private/sig.rkt")

(define-unit-from-context url@ url^)

(define-values/invoke-unit 
  (compound-unit/infer (import) (export html^)
                       (link standard-mred@ url@ html@))
  (import)
  (export html^))

(define html-text<%>
  (interface ((class->interface text%))
    get-url
    set-title
    add-link
    add-tag
    make-link-style
    add-racket-callback
    add-thunk-callback
    post-url))

(define url-delta (make-object style-delta% 'change-underline #t))
(void (send url-delta set-delta-foreground "blue"))

(define html-text-mixin
  (mixin ((class->interface text%)) (html-text<%>)
    (inherit change-style set-clickback)
    
    (define/public (get-url) #f)
    (define/public (set-title s) (void))
    (define/public (add-link pos end-pos url-string)
      (set-clickback pos end-pos (lambda (e start-pos eou-pos)
                                   (send-url url-string))))
    (define/public (add-tag label pos) (void))
    (define/public (make-link-style pos endpos) 
      (change-style url-delta pos endpos))
    (define/public (add-racket-callback pos endpos racket) (void))
    (define/public (add-thunk-callback pos endpos thunk)
      (set-clickback pos endpos (lambda (e start-pos eou-pos)
                                  (thunk))))
    (define/public (post-url url post-data)
      (message-box "HTML" 
                   (format "Cannot perform post: ~e"
                           post-data)
                   #f
                   '(stop ok)))
    (super-new)))

(define (render-html-to-text port text%-obj img-ok? eval-ok?)
  (unless (input-port? port)
    (raise-type-error 'render-html-to-text "input port" 0 (list port text%-obj)))
  (unless (text%-obj . is-a? . html-text<%>)
    (raise-type-error 'render-html-to-text "html-text<%> object" 0 (list port text%-obj)))
  (parameterize ([html-eval-ok eval-ok?]
                 [html-img-ok img-ok?])
    (dynamic-wind
     (lambda () (send text%-obj begin-edit-sequence #f))
     (lambda () (html-convert port text%-obj))
     (lambda () (send text%-obj end-edit-sequence)))))

(provide html-text<%>
         html-text-mixin
         render-html-to-text)

