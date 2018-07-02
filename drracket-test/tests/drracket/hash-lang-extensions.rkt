#lang at-exp racket

(require "private/drracket-test-util.rkt"
         framework/test
         racket/gui/base)

(fire-up-drracket-and-run-tests
 #:prefs '([plt:framework-pref:framework:autosaving-on? #f])
 (λ ()
   (define drr (wait-for-drracket-frame))
   (define defs (queue-callback/res (λ () (send drr get-definitions-text))))
   (define ints (queue-callback/res (λ () (send drr get-interactions-text))))
     
   (set-module-language! #f)
   (test:set-check-box! "Populate “compiled” directories (for faster loading)" #f)
   (test:button-push "OK")
   (wait-for-drracket-frame) ;; make sure language dialog is closed
   
   (clear-definitions drr)
   (define dir (make-temporary-file "drracket-hash-lang-extensions-~a" 'directory))
   (define (save-a-file filename . content)
     (call-with-output-file (build-path dir filename)
       (λ (port)
         (for ([s (in-list content)]) (display s port)))
       #:exists 'truncate))
   
     @save-a-file["x.rkt"]{
 #lang reader "lang.rkt"
 (module m racket/base
 1
 (define x 1)
 2
 3
 4
 5
 6
 7
 8
 )
}

   @save-a-file["lang.rkt"]{
#lang racket
(provide read-syntax read get-info)
(define (get-info in mod line col pos)
  (λ (key default)
    (if (equal? key 'drracket:indentation)
        (λ (txt pos)
          (define para (send txt position-paragraph pos))
          (cond
            [(<= para 10)
             para]
            [else (error 'tmp2.rkt "random crash to practice flushing the cache")]))
        default)))
}

   

   (define (find-reload-button)
     (queue-callback/res
      (λ ()
        (let loop ([obj drr])
          (cond
            [(is-a? obj area-container<%>)
             (for/or ([child (in-list (send obj get-children))])
               (loop child))]
            [(is-a? obj button%)
             (and (string? (send obj get-label))
                  (regexp-match? #rx"^Reload" (send obj get-label))
                  obj)]
            [else #f])))))

   (let/ec k
     (dynamic-wind
      void
      (λ ()
        (define (failed fmt . args)
          (apply eprintf fmt args)
          (k (void)))


;                                                          
;                                                          
;                                                          
;                                                          
;               ;;;                     ;;;        ;;;;    
;               ;;;                     ;;;       ;;;;;;   
;  ;;; ;; ;;;;  ;;;   ;;;    ;;;;;   ;; ;;;       ;;  ;;   
;  ;;;;; ;; ;;; ;;;  ;;;;;  ;;;;;;; ;;;;;;;       ;;;;;    
;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;  ;;; ;;; ;;;        ;;;     
;  ;;;  ;;;;;;; ;;; ;;; ;;;   ;;;;; ;;; ;;;       ;;;;;;;; 
;  ;;;  ;;;     ;;; ;;; ;;; ;;; ;;; ;;; ;;;      ;;;  ;;;  
;  ;;;   ;;;;;; ;;;  ;;;;;  ;;; ;;; ;;;;;;;      ;;;;;;;;; 
;  ;;;    ;;;;  ;;;   ;;;    ;;;;;;  ;; ;;;       ;;;;;  ; 
;                                                          
;                                                          
;                                                          
;                                                          
;                                           
;                                           
;                                           
;                                           
;  ;;;             ;;;                   ;  
;                  ;;;                 ;;;  
;  ;;; ;;; ;;   ;; ;;;   ;;;;  ;;; ;;  ;;;; 
;  ;;; ;;;;;;; ;;;;;;;  ;; ;;; ;;;;;;; ;;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  
;  ;;; ;;; ;;; ;;; ;;; ;;;;;;; ;;; ;;; ;;;  
;  ;;; ;;; ;;; ;;; ;;; ;;;     ;;; ;;; ;;;  
;  ;;; ;;; ;;; ;;;;;;;  ;;;;;; ;;; ;;; ;;;; 
;  ;;; ;;; ;;;  ;; ;;;   ;;;;  ;;; ;;;  ;;; 
;                                           
;                                           
;                                           
;                                           

   
        (queue-callback/res (λ ()
                              (send defs load-file (build-path dir "x.rkt"))

                              ;; force this now so we don't have to wait for the
                              ;; timer to expire
                              (send defs move-to-new-language)))
        (test:menu-select "Edit" "Select All")
        (test:keystroke #\tab)

        (define reload-button (find-reload-button))
        (unless reload-button (failed "didn't find reload button\n"))

        (define lang-specified-indentation
          @string-append{
#lang reader "lang.rkt"
 (module m racket/base
  1
   (define x 1)
    2
     3
      4
       5
        6
         7
          8
   )
 })

        (let ([got (queue-callback/res (λ () (send defs get-text)))])
          (unless (equal? got lang-specified-indentation)
            (failed "after indenting, content was wrong:\n~s\nbut expected\n~s\n"
                    got lang-specified-indentation)))


        ;; now, in the "broken" mode, we reindent all and expect
        ;; to see racket-style indentation
        (test:menu-select "Edit" "Select All")
        (test:keystroke #\tab)

        (define racket-style-indentation
          @string-append{
#lang reader "lang.rkt"
(module m racket/base
  1
  (define x 1)
  2
  3
  4
  5
  6
  7
  8
  )
})

        (let ([got (queue-callback/res (λ () (send defs get-text)))])
          (unless (equal? got racket-style-indentation)
            (failed "indenting after runtime error, content was wrong:\n~s\nbut expected\n~s\n"
                    got racket-style-indentation)))
     
        (test:button-push reload-button)
        (when (find-reload-button) (failed "after pushing reload button, it didn't go away"))


;                                                                           
;                                                                           
;                                                                           
;                                                                           
;                       ;;;                    ;;;                 ;;;   ;  
;                       ;;;                    ;;;                     ;;;  
;  ;;; ;; ;;;;  ;;; ;;  ;;;      ;;;;  ;;; ;;; ;;; ;;  ;;; ;; ;;;  ;;; ;;;; 
;  ;;;;; ;; ;;; ;;;;;;; ;;;     ;;; ;; ;;; ;;; ;;;;;;; ;;;;;;;;;;; ;;; ;;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;;     ;;;    ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  
;  ;;;  ;;;;;;; ;;; ;;; ;;;      ;;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  
;  ;;;  ;;;     ;;; ;;; ;;;        ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  
;  ;;;   ;;;;;; ;;;;;;; ;;;     ;; ;;; ;;;;;;; ;;;;;;; ;;; ;;; ;;; ;;; ;;;; 
;  ;;;    ;;;;  ;;; ;;  ;;;      ;;;;   ;; ;;; ;;; ;;  ;;; ;;; ;;; ;;;  ;;; 
;               ;;;                                                         
;               ;;;                                                         
;                                                                           
;                                                                           

   
        @save-a-file["lang.rkt"]{
#lang racket
(provide read-syntax read get-info)
(define (get-info in mod line col pos)
  (λ (key default)
    (if (equal? key 'drracket:submit-predicate)
        (λ (port only-whitespace?) (regexp-match? port #rx"x+"))
        default)))
}

        (test:menu-select "Racket" "Reload #lang Extensions")

        (do-execute drr)
        
        (insert-in-interactions drr "x")
        (define output-start (+ 1 (queue-callback/res (λ () (send ints last-position)))))
        (test:keystroke #\return)
        (wait-for-computation drr)
        (define output-end
          (queue-callback/res
           (λ () (send ints paragraph-end-position
                       (- (send ints last-paragraph) 1)))))
        (define actual-output (fetch-output drr output-start output-end))
        (unless (equal? actual-output "1")
          (failed "with repl submit; expected 1, got ~s" actual-output)))
      (λ ()
        (delete-directory/files dir))))))
