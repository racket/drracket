#lang racket/base

(require drracket/check-syntax
         racket/class
         rackunit)

(check-true
 (let ()
   (define add-arrow-called? #f)
   
   (define annotations
     (new (class (annotations-mixin object%)
            (super-new)
            (define/override (syncheck:find-source-object stx)
              (if (eq? 'the-source (syntax-source stx))
                  'yep
                  #f))
            (define/override (syncheck:add-arrow . args)
              (set! add-arrow-called? #t)))))
   
   (define-values (add-syntax done)
     (make-traversal (make-base-namespace)
                     (current-directory)))
   
   (parameterize ([current-annotations annotations]
                  [current-namespace (make-base-namespace)])
     (add-syntax (expand
                  (read-syntax
                   'the-source
                   (open-input-string
                    (format "~s"
                            `(module m racket/base
                               (define x 4)
                               x
                               (let ([y 1]) y)))))))
     (done))
   add-arrow-called?))

(check-true
 (let ()
   (define add-arrow-called? #f)
   
   (define annotations
     (new (class (annotations-mixin object%)
            (super-new)
            (define/override (syncheck:find-source-object stx)
              (if (eq? 'the-source (syntax-source stx))
                  'yep
                  #f))
            (define/override (syncheck:add-arrow . args)
              (set! add-arrow-called? #t)))))
   
   (define-values (add-syntax done)
     (make-traversal (make-base-namespace) #f))
   
   (parameterize ([current-annotations annotations]
                  [current-namespace (make-base-namespace)])
     (add-syntax (expand
                  (read-syntax
                   'the-source
                   (open-input-string
                    (format "~s"
                            `(module m racket/base
                               (define x 4)
                               x
                               (let ([y 1]) y)))))))
     (done))
   add-arrow-called?))

(check-not-exn
 (λ ()
   (define annotations
     (new (class (annotations-mixin object%)
            (super-new)
            (define/override (syncheck:find-source-object stx)
              stx))))
   
   (define base-namespace (make-base-namespace))
   (define-values (add-syntax done)
     (make-traversal base-namespace #f))
   
   (parameterize ([current-annotations annotations]
                  [current-namespace base-namespace])
     (eval '(require (for-syntax racket/base)))
     (add-syntax
      (expand
       '(let-syntax ([m (λ (_) #`(let ([x 1]) x))])
          (m))))
     (done))))


(define-syntax-rule (define-get-arrows get-what-arrows method-header arrow-info)
  (define (get-what-arrows str)
    (define tail-arrows '())
    
    (define annotations
      (new (class (annotations-mixin object%)
             (super-new)
             (define/override (syncheck:find-source-object stx)
               (if (eq? 'the-source (syntax-source stx))
                   'yep
                   #f))
             (define/override method-header
               (set! tail-arrows (cons arrow-info tail-arrows))))))
    
    (define-values (add-syntax done)
      (make-traversal (make-base-namespace) #f))
    
    (parameterize ([current-annotations annotations]
                   [current-namespace (make-base-namespace)])
      (add-syntax (expand
                   (parameterize ([read-accept-reader #t])
                     (read-syntax 'the-source (open-input-string str)))))
      (done))
    (reverse tail-arrows)))

;                                                                       
;                                                                       
;                                                                       
;                                                                       
;    ;          ;;; ;;;                                                 
;  ;;;              ;;;                                                 
;  ;;;;  ;;;;;  ;;; ;;;      ;;;;;  ;;; ;;;; ;; ;;;   ;;; ;;; ;;; ;;;;  
;  ;;;; ;;;;;;; ;;; ;;;     ;;;;;;; ;;;;;;;;;; ;;;;;  ;;; ;;; ;;;;;; ;; 
;  ;;;  ;;  ;;; ;;; ;;;     ;;  ;;; ;;;  ;;;  ;;; ;;;  ;;;;;;;;; ;;;    
;  ;;;    ;;;;; ;;; ;;;       ;;;;; ;;;  ;;;  ;;; ;;;  ;;;; ;;;;  ;;;;  
;  ;;;  ;;; ;;; ;;; ;;;     ;;; ;;; ;;;  ;;;  ;;; ;;;  ;;;; ;;;;    ;;; 
;  ;;;; ;;; ;;; ;;; ;;;     ;;; ;;; ;;;  ;;;   ;;;;;    ;;   ;;  ;; ;;; 
;   ;;;  ;;;;;; ;;; ;;;      ;;;;;; ;;;  ;;;    ;;;     ;;   ;;   ;;;;  
;                                                                       
;                                                                       
;                                                                       
;                                                                       

(define-get-arrows get-tail-arrows 
  (syncheck:add-tail-arrow parent-src parent-pos child-src child-pos)
  (list parent-pos child-pos))

(check-equal? (get-tail-arrows "#lang racket/base\n(if 1 2 3)")
              '((18 24) (18 26)))
(check-equal? (get-tail-arrows "#lang racket/base\n(λ (x) 1 2)")
              '((18 29)))
(check-equal? (get-tail-arrows "#lang racket/base\n(case-lambda [(x) 1 2][(y z) 3 4 5 6])")
              '((18 38) (18 53)))
(check-equal? (get-tail-arrows "#lang racket/base\n(let ([x 3]) (#%expression (begin 1 2)))")
              '((18 45) (45 54)))
(check-equal? (get-tail-arrows "#lang racket/base\n(begin0 1)")
              '((18 26)))
(check-equal? (get-tail-arrows "#lang racket/base\n(begin0 1 2)")
              '())
(check-equal? (get-tail-arrows "#lang racket/base\n(letrec ([x (lambda (y) x)]) (x 3))")
              '((30 42) (18 47)))
(check-equal? (get-tail-arrows "#lang racket/base\n(with-continuation-mark 1 2 3)")
              '((18 46)))
(check-equal? (get-tail-arrows "#lang racket\n(define (f x) (match 'x ['x (f x)]))")
              '((13 27) (27 41)))



;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                                                                  
;  ;;;     ;;;             ;;; ;;;                                                                 
;  ;;;                     ;;;                                                                     
;  ;;; ;;  ;;; ;;; ;;   ;; ;;; ;;; ;;; ;;   ;; ;;;      ;;;;;  ;;; ;;;; ;; ;;;   ;;; ;;; ;;; ;;;;  
;  ;;;;;;; ;;; ;;;;;;; ;;;;;;; ;;; ;;;;;;; ;;;;;;;     ;;;;;;; ;;;;;;;;;; ;;;;;  ;;; ;;; ;;;;;; ;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;     ;;  ;;; ;;;  ;;;  ;;; ;;;  ;;;;;;;;; ;;;    
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;       ;;;;; ;;;  ;;;  ;;; ;;;  ;;;; ;;;;  ;;;;  
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;     ;;; ;;; ;;;  ;;;  ;;; ;;;  ;;;; ;;;;    ;;; 
;  ;;;;;;; ;;; ;;; ;;; ;;;;;;; ;;; ;;; ;;; ;;;;;;;     ;;; ;;; ;;;  ;;;   ;;;;;    ;;   ;;  ;; ;;; 
;  ;;; ;;  ;;; ;;; ;;;  ;; ;;; ;;; ;;; ;;;  ;; ;;;      ;;;;;; ;;;  ;;;    ;;;     ;;   ;;   ;;;;  
;                                              ;;;                                                 
;                                          ;;;;;;                                                  
;                                                                                                  
;                                                                                                  


(define-get-arrows get-binding-arrows 
  (syncheck:add-arrow start-source-obj	 
                      start-left	 
                      start-right	 
                      end-source-obj	 
                      end-left	 
                      end-right	 
                      actual?	 
                      phase-level)
  (list (list start-left start-right) (list end-left end-right)))
(check-equal? (get-binding-arrows "#lang racket/base\n(require (only-in racket/base))")
              '(((6 17) (19 26))     ;; to 'require'
                ((6 17) (28 35))))   ;; to 'only-in'

