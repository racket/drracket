#lang racket/base

(require drracket/check-syntax
         racket/class
         racket/set
         rackunit
         syntax/modread)

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
              '((18 28)))
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
(check-equal? (apply set (get-binding-arrows
                          "#lang racket/base\n(require (only-in racket/base))"))
              (set '((6 17) (19 26))     ;; to 'require'
                   '((6 17) (28 35))))   ;; to 'only-in'

(define-get-arrows get-binding-arrows/pxpy
  (syncheck:add-arrow/name-dup/pxpy start-source-obj	 
                                    start-left	 
                                    start-right
                                    start-px
                                    start-py
                                    end-source-obj	 
                                    end-left	 
                                    end-right
                                    end-px
                                    end-py
                                    actual?	 
                                    phase-level
                                    require-arrows?
                                    name-dup?)
  (list (list start-left start-right start-px start-py)
        (list end-left end-right end-px end-py)))
(check-equal? (apply set (get-binding-arrows/pxpy
                          "#lang racket/base\n(require (only-in racket/base))"))
              (set '((6 17 .5 .5) (19 26 .5 .5))     ;; to 'require'
                   '((6 17 .5 .5) (28 35 .5 .5))))

(check-equal? (apply set (get-binding-arrows/pxpy
                          "#lang racket\n(define/contract (f x) any/c f)"))
              (set '((6 12 0.5 0.5) (14 29 0.5 0.5)) ;; point to define/contract
                   '((6 12 0.5 0.5) (36 41 0.5 0.5)) ;; point to any/c
                   '((31 32 0.5 0.5) (42 43 0.5 0.5))))  ;; from f to f


(let ([prefix
       (string-append
        "#lang racket\n"
        "(define-syntax (def-a/boxed-a stx)\n"
        "  (syntax-case stx ()\n"
        "    [(_ base-name base-val)\n"
        "     (let ()\n"
        "       (define base-len (string-length (symbol->string (syntax-e #'base-name))))\n"
        "       (define boxed-name\n"
        "         (datum->syntax #'base-name\n"
        "                        (string->symbol (format \"~a-boxed\" (syntax-e #'base-name)))\n"
        "                        #'base-name))\n"
        "       #`(begin\n"
        "           (define base-name base-val)\n"
        "           (define #,(syntax-property\n"
        "                      boxed-name 'sub-range-binders\n"
        "                      (list (vector (syntax-local-introduce boxed-name)\n"
        "                                    0 base-len 0.5 0.5\n"
        "                                    (syntax-local-introduce #'base-name)\n"
        "                                    0 base-len 0.5 0.5)))\n"
        "             (box base-val))))]))\n"
        "\n")])
  (define all-arrows
    (get-binding-arrows/pxpy
     (string-append
      prefix
      "(def-a/boxed-a bar2 22)\n"
      "bar2-boxed\n")))
  (check-equal?
   (for/set ([arrow (in-list all-arrows)]
             #:when
             ;; make sure the arrow doesn't originate in the prefix
             ;; (not interested in testing those arrows)
             (let* ([start (list-ref arrow 0)]
                    [offset (list-ref start 0)])
               (> offset (string-length prefix))))
     (define (subtract-prefix arr)
       (list (- (list-ref arr 0) (string-length prefix))
             (- (list-ref arr 1) (string-length prefix))
             (list-ref arr 2)
             (list-ref arr 3)))
     (list (subtract-prefix (list-ref arrow 0))
           (subtract-prefix (list-ref arrow 1))))
   (set '((15 19 0.5 0.5) (24 28 0.5 0.5)))))

;; Unused requires
(let ()
  (define-values (add-syntax done)
    (make-traversal (make-base-namespace) #f))

  (define collector%
    (class (annotations-mixin object%)
      (super-new)
      (define unused-requires (set))

      (define/override (syncheck:find-source-object stx) stx)
      (define/override (syncheck:add-arrow start-text start-pos-left start-pos-right
                                       end-text end-pos-left end-pos-right
                                       actual? level)
        (void))

      (define/override (syncheck:add-unused-require _ left right)
          (set! unused-requires
                (set-add unused-requires
                         (list left right))))
      (define/public (get-unused-requires) unused-requires)))

  (define annotations (new collector%))
  (parameterize ([current-annotations annotations]
                 [current-namespace (make-base-namespace)])
    (add-syntax (expand
                 (read-syntax
                   'the-source
                   (open-input-string
                    (format "~s"
                            `(module m racket/base
                               (require racket/match)
                               (+ 1 2)))))))
    (done))
  (check-equal?
   (send annotations get-unused-requires)
   (set '(31 43))))

;; make sure that the default arity of
;; syncheck:add-prefixed-require-reference
;; is correct
(let ()
  (define build-trace%
    (class (annotations-mixin object%)
      (define/override (syncheck:find-source-object stx)
        (and (equal? src (syntax-source stx))
             src))
      (super-new)))

  (define-values (in out) (make-pipe))
  (thread
   (λ ()
     (displayln "#lang racket/base\n" out)
     (writeln '(require (prefix-in : racket/string)) out)
     (writeln '(:string-prefix? "abcdefg" "abc") out)
     (close-output-port out)))
  (define src "prefix.rkt")
  (port-count-lines! in)
  (define ns (make-base-namespace))
  (define-values (add-syntax done)
    (make-traversal ns (current-directory)))
  (parameterize ([current-annotations (new build-trace%)]
                 [current-namespace ns])
    (define stx (with-module-reading-parameterization
                  (λ () (read-syntax src in))))
    (add-syntax (expand stx))
    (done)))

(let ()
  (define build-trace%
    (class (annotations-mixin object%)
      (define/override (syncheck:find-source-object stx)
        (and (equal? src (syntax-source stx))
             src))
      (define/override (syncheck:add-prefixed-require-reference . args)
        (set! args-seen (cons args args-seen)))
      (super-new)))

  (define args-seen '())

  (define-values (in out) (make-pipe))
  (thread
   (λ ()
     (displayln "#lang racket/base\n" out)
     (writeln '(require (prefix-in : racket/string)) out)
     (writeln '(:string-prefix? "abcdefg" "abc") out)
     (close-output-port out)))
  (define src "prefix.rkt")
  (port-count-lines! in)
  (define ns (make-base-namespace))
  (define-values (add-syntax done)
    (make-traversal ns (current-directory)))
  (define prefixes-seen '())
  (parameterize ([current-annotations (new build-trace%)]
                 [current-namespace ns])
    (define stx (with-module-reading-parameterization
                  (λ () (read-syntax src in))))
    (add-syntax (expand stx))
    (done))

  (check-equal? args-seen
                 (list (list "prefix.rkt" 41 54
                             ': "prefix.rkt" 39 40))))

