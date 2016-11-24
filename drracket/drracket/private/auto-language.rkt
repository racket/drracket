#lang racket/base

(require framework/framework
	 racket/gui/base
         racket/class
         racket/contract)

(provide
 (contract-out
  #:∀ S
  [pick-new-language
   (-> (is-a?/c text%)
       (listof object?)
       (or/c #f object?)
       S
       (values (or/c #f object?)
               (or/c S #f)))]
  [looks-like-module?
   (-> (is-a?/c text%) boolean?)]))

(define (pick-new-language text all-languages module-language module-language-settings)
  (with-handlers ([exn:fail:read? (λ (x) (values #f #f))])
    (define found-language? #f)
    (define settings #f)
    (for ([lang (in-list all-languages)])
      (define lang-spec (send lang get-reader-module))
      (when lang-spec
        (let* ([lines (send lang get-metadata-lines)]
               [str (send text get-text
                          0
                          (send text paragraph-end-position (- lines 1)))]
               [sp (open-input-string str)])
          (when (regexp-match #rx"#reader" sp)
            (define spec-in-file (read sp))
            (when (equal? lang-spec spec-in-file)
              (set! found-language? lang)
              (set! settings (send lang metadata->settings str))
              (send text while-unlocked
                    (λ () 
                      (send text delete 0 (send text paragraph-start-position lines)))))))))
      
    ;; check to see if it looks like the module language.
    (unless found-language?
      (when module-language
        (when (looks-like-module? text)
          (set! found-language? module-language)
          (set! settings module-language-settings))))
    (values found-language?
            settings)))

(define (looks-like-module? text)
  (or (looks-like-new-module-style? text)
      (looks-like-old-module-style? text)
      (with-handlers ([exn:fail? (λ (x) #f)])
        (read-language (open-input-text-editor text 0 'end (λ (x) x) text #f) 
                       (λ () #f))
        #t)))

(define (looks-like-old-module-style? text)
  (with-handlers ([exn:fail:read? (λ (x) #f)])
    (define tp (open-input-text-editor text 0 'end (lambda (s) s) text #t))
    (define r1 (parameterize ([read-accept-reader #f]) (read tp)))
    (define r2 (parameterize ([read-accept-reader #f]) (read tp)))
    (and (eof-object? r2)
         (pair? r1)
         (eq? (car r1) 'module))))

(define (looks-like-new-module-style? text)
  (define tp (open-input-text-editor text 0 'end (lambda (s) s) text #t))
  (define l1 (with-handlers ([exn:fail? (lambda (exn) eof)])
               ;; If tp contains a snip, read-line fails.
               (read-line tp)))
  (and (string? l1)
       (regexp-match? #rx"#lang .*$" l1)))
