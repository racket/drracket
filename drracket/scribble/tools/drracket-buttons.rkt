#lang racket/base

(require racket/runtime-path
         racket/gui/base
         racket/class
         mrlib/bitmap-label
         racket/system
         net/sendurl
         drracket/tool-lib)

(provide drracket-buttons)

(module test racket/base)

(define-runtime-path pdf-png-path "pdf.png")
(define-runtime-path html-png-path "html.png")
(define pdf.png (make-object bitmap% pdf-png-path 'png/mask))
(define html.png (make-object bitmap% html-png-path 'png/mask))

(define-namespace-anchor anchor)

(define original-error-display-handler (error-display-handler))

(define (make-render-button label bmp mode suffix number)
  (list 
   label
   bmp
   (λ (drs-frame)
     (define fn (send (send drs-frame get-definitions-text) get-filename))
     (define html? (equal? suffix #".html"))
     (cond
       [fn
        (parameterize ([drracket:rep:after-expression
                        (λ ()
                          (define submod-doc
                            (with-handlers ((exn:fail? (λ (x) #f)))
                              (dynamic-require (module-path-index-join
                                                '(submod "." doc)
                                                (eval #'(variable-reference->module-path-index
                                                         (#%variable-reference))))
                                               'doc)))
                          (define doc (or submod-doc
                                          (with-handlers ((exn:fail? (λ (x) #f))) (eval 'doc))))
                          ;; if (eval 'doc) goes wrong, then we assume that's because of
                          ;; an earlier failure, so we just don't do anything.
                          (when doc
                            (printf "scribble: loading xref\n")
                            (define xref ((dynamic-require 'setup/xref 'load-collections-xref)))
                            (printf "scribble: rendering\n")
                            (parameterize ([current-input-port (open-input-string "")])
                              ((dynamic-require 'scribble/render 'render) 
                               (list doc)
                               (list fn)
                               #:render-mixin (dynamic-require (if html? 
                                                                   'scribble/html-render
                                                                   'scribble/pdf-render)
                                                               'render-mixin)
                               #:xrefs (list xref)))
                            (define file-to-open (path-replace-suffix fn suffix))
                            (send-url/file (path->string file-to-open))))])
          (send drs-frame execute-callback))]
       [else
        (message-box "Scribble" "Cannot render buffer without filename")]))
   number))

(define drracket-buttons
  (let ([html-button
         (make-render-button "Scribble HTML" html.png "--html" #".html" 99)]
        [pdf-button
         (make-render-button "Scribble PDF" pdf.png "--pdf" #".pdf" 98)])
    (list html-button pdf-button)))
