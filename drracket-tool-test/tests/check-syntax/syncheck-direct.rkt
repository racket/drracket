#lang racket/base

(require drracket/check-syntax
         (only-in drracket/private/syncheck/traversals
                  [build-trace% basic-build-trace%])
         racket/class
         racket/match
         racket/set
         rackunit
         syntax/modread
         racket/file
         racket/format
         racket/runtime-path)

(define-syntax-rule
  (define-get-arrows get-what-arrows method-header arrow-info)
  (define (get-what-arrows str)
    (define-get-arrows/proc
      (λ (%)
        (class %
          (inherit add-item)
          (define/override method-header
            (add-item arrow-info))
          (super-new)))
      str)))

(define-runtime-path here ".")

(define (define-get-arrows/proc mixin str)
  (define results '())

  (define annotations
    (new (mixin (class (annotations-mixin object%)
                  (super-new)
                  (define/public (add-item x)
                    (when x
                      (set! results (cons x results))))
                  (define/override (syncheck:find-source-object stx)
                    (if (eq? 'the-source (syntax-source stx))
                        'yep
                        #f))))))

  (define-values (add-syntax done)
    (make-traversal (make-base-namespace) #f))

  (parameterize ([current-annotations annotations]
                 [current-namespace (make-base-namespace)]
                 [current-load-relative-directory here])
    (add-syntax (expand
                 (parameterize ([read-accept-reader #t])
                   (define p (open-input-string str))
                   (port-count-lines! p)
                   (read-syntax 'the-source p))))
    (done))
  (apply set results))

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
              (set '(18 24) '(18 26)))
(check-equal? (get-tail-arrows "#lang racket/base\n(λ (x) 1 2)")
              (set '(18 27)))
(check-equal? (get-tail-arrows "#lang racket/base\n(case-lambda [(x) 1 2][(y z) 3 4 5 6])")
              (set '(18 38) '(18 53)))
(check-equal? (get-tail-arrows "#lang racket/base\n(let ([x 3]) (#%expression (begin 1 2)))")
              (set '(18 45) '(45 54)))
(check-equal? (get-tail-arrows "#lang racket/base\n(begin0 1)")
              (set '(18 26)))
(check-equal? (get-tail-arrows "#lang racket/base\n(begin0 1 2)")
              (set))
(check-equal? (get-tail-arrows "#lang racket/base\n(letrec ([x (lambda (y) x)]) (x 3))")
              (set '(30 42) '(18 47)))
(check-equal? (get-tail-arrows "#lang racket/base\n(with-continuation-mark 1 2 3)")
              (set '(18 46)))
(check-equal? (get-tail-arrows "#lang racket\n(define (f x) (match 'x ['x (f x)]))")
              (set '(13 27) '(27 41)))
(check-equal? (get-tail-arrows "#lang racket/base\n(if '🏴‍☠️ 2 3)")
              (set '(18 28) '(18 30)))



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
  (and actual? ;; skip the purple `?` arrows
       (list (list start-left start-right) (list end-left end-right))))

(define-get-arrows get-purple-binding-arrows
  (syncheck:add-arrow start-source-obj
                      start-left
                      start-right
                      end-source-obj
                      end-left
                      end-right
                      actual?
                      phase-level)
  (and (not actual?) ;; only the purple `?` arrows
       (list (list start-left start-right) (list end-left end-right))))

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

(check-equal? (get-binding-arrows "#lang racket/base\n(require (only-in racket/base))")
              (set '((6 17) (19 26))     ;; to 'require'
                   '((6 17) (28 35))))   ;; to 'only-in'

(check-equal? (get-binding-arrows
               (string-append
                "(module m racket/base\n"
                "  (define x 4)\n"
                "  x\n"
                "  (let ([y 1]) y))\n"))
              (set
               '((10 21) (25 31))
               '((10 21) (34 34))
               '((10 21) (44 47))
               '((10 21) (52 52))
               '((32 33) (39 40))
               '((50 51) (56 57))))
(check-equal? (get-binding-arrows
               (string-append
                "(module m racket/base\n"
                "  (define 🏴‍☠️ 4)\n"
                "  🏴‍☠️\n"
                "  (let ([🏴‍☠️ 1]) 🏴‍☠️))\n"))
              (set
               '((10 21) (25 31))
               '((10 21) (37 37))
               '((10 21) (50 53))
               '((10 21) (61 61))
               '((32 36) (42 46))
               '((56 60) (65 69))))

(check-equal? (get-binding-arrows/pxpy
               "#lang racket/base\n(require (only-in racket/base))")
              (set '((6 17 .5 .5) (19 26 .5 .5))     ;; to 'require'
                   '((6 17 .5 .5) (28 35 .5 .5))))

(check-equal? (get-binding-arrows/pxpy "#lang racket\n(define/contract (f x) any/c f)")
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
   (for/set ([arrow (in-set all-arrows)]
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

(check-equal? (get-binding-arrows/pxpy
               (~s
                '(module m racket/base
                   (require racket/require
                            (multi-in racket (list match))
                            racket/set
                            (rename-in racket/list [first 1st]))
                   first
                   1st)))
              (set '((31 45 0.5 0.5) (47 55 0.5 0.5))
                   '((10 21 0.5 0.5) (89 98 0.5 0.5))
                   '((10 21 0.5 0.5) (23 30 0.5 0.5))
                   '((64 68 0.5 0.5) (125 130 0.5 0.5))
                   '((118 121 0.5 0.5) (131 134 0.5 0.5))
                   ;'((118 121 0.5 0.5) (125 130 0.5 0.5))
                   '((99 110 0.5 0.5) (131 134 0.5 0.5))))

(check-equal? (get-binding-arrows
               (string-append
                "#lang racket\n"
                "(class object%\n"
                "  (super-new)\n"
                "  (define-syntax (mac stx)\n"
                "    #'1))"))
              (set '((6 12) (14 19))
                   '((6 12) (20 27))
                   '((6 12) (31 40))
                   '((6 12) (45 58))))

(check-equal? (get-binding-arrows
               (string-append
                "#lang racket\n"
                "(unit (import) (export)\n"
                "      (define color-scheme-colors 3)\n"
                "      (set! color-scheme-colors color-scheme-colors))\n"))
              (set '((6 12) (14 18))
                   '((6 12) (20 26))
                   '((6 12) (29 35))
                   '((6 12) (44 50))
                   '((6 12) (71 71))
                   '((6 12) (81 85))
                   '((51 70) (86 105))
                   '((51 70) (106 125))))

(check-equal? (get-binding-arrows
               (string-append
                "#lang racket\n"
                "(require (for-meta 2 racket/base))\n"
                "(begin-for-syntax\n"
                "  (begin-for-syntax\n"
                "    (module a racket/base)))\n"))
              (set
               '((6 12) (14 21))
               '((6 12) (23 31))
               '((6 12) (49 65))
               '((6 12) (69 85))
               '((34 45) (91 97))))

(check-equal? (get-binding-arrows
               (string-append
                "#lang racket\n"
                "(define fff 1)\n"
                "\n"
                "(define-syntax (m stx)\n"
                "  (syntax-case stx ()\n"
                "    [(_ x)\n"
                "     #`#'#,(syntax-property\n"
                "            #'x\n"
                "            'identifiers-as-disappeared-uses? #t)]))\n"
                "\n"
                "(m fff)\n"))
              (set
               '((6 12) (14 20))      ;; define
               '((6 12) (25 25))      ;; #%datum in 1
               '((6 12) (30 43))      ;; define-syntax
               '((6 12) (55 66))      ;; syntax-case
               '((6 12) (90 92))      ;; #`
               '((6 12) (96 96))      ;; #%app in syntax-property
               '((6 12) (97 112))     ;; syntax-property
               '((6 12) (125 127))    ;; #' in front of x
               '((6 12) (141 142))    ;; ' in front of identifiers-as-disappeared-uses?
               '((6 12) (175 175))    ;; #%datum in front of #t
               '((45 46) (184 185))   ;; m -> m
               '((47 50) (67 70))     ;; stx -> stx
               '((82 83) (127 128))   ;; x -> x
               '((21 24) (186 189)))) ;; fff -> fff

(check-equal? (get-purple-binding-arrows
               (string-append
                "#lang racket\n"
                "(define (f x)\n"
                "  (quote-syntax (f x)\n"
                "                #:local))\n"))
              (set '((22 23) (44 45))
                   '((24 25) (46 47))))

(check-equal? (get-purple-binding-arrows
               (string-append
                "#lang racket/base\n"
                "(#%require (portal x (+ 2 λ)))\n"
                "#'x\n"))
              (set '((6 17) (40 41))
                   '((6 17) (44 45))
                   '((37 38) (51 52))))

(check-equal? (get-purple-binding-arrows
               (string-append
                "#lang racket/base\n"
                "(require (for-syntax racket/base))\n"
                "(#%require (for-meta 1 (portal x (+ 2 λ))))\n"
                "(define-syntax m #'x)\n"))
              (set '((6 17) (91 92))
                   '((6 17) (87 88))
                   '((39 50) (87 88))
                   '((39 50) (91 92))
                   '((84 85) (116 117))))


(check-equal? (get-binding-arrows
               (string-append
                "#lang racket/base\n"
                "(require \"soup.rkt\")\n"
                "(soup-ref kettle)\n"
                "kettle\n"))
              (set
               '((6 17) (19 26))
               '((27 37) (40 48))
               '((27 37) (49 55))
               '((27 37) (57 63))))

(check-equal? (get-binding-arrows
               (string-append
                "#lang racket/base\n"
                "(define x 1)\n"
                "(module+ m x)\n"))
              (set '((6 17) (19 25))
                   '((6 17) (28 28))
                   '((6 17) (32 39))
                   '((26 27) (42 43))))

(check-equal? (get-binding-arrows
               (string-append
                "#lang racket/base\n"
                "(define x 1)\n"
                "x\n"
                "(module+ m\n"
                "  x\n"
                "  (define x 1))\n"))
              (set
               '((6 17) (19 25))
               '((6 17) (28 28))
               '((6 17) (34 41))
               '((6 17) (60 60))
               '((6 17) (51 57))
               '((26 27) (31 32))
               '((58 59) (46 47))))

(check-equal? (get-binding-arrows
               (string-append
                "#lang racket/base\n"
                "(require racket/list)\n"
                "first\n"
                "(module n racket\n"
                "  first\n"
                "  (module+ m\n"
                "    (require (rename-in racket/list [first ABC]))))\n"
                "(module o racket\n"
                "  first)\n"))
              (set
               '((6 17) (47 53))
               '((6 17) (19 26))
               '((6 17) (137 143))
               '((27 38) (40 45))
               '((56 62) (65 70))
               '((56 62) (74 81))
               '((56 62) (89 96))
               '((56 62) (98 107))
               '((146 152) (155 160))))


(check-equal?
 (get-binding-arrows (string-append
                      "#lang racket/base\n"
                      "(require racket/list)\n"
                      "first\n"
                      "(module+ m\n"
                      "  (require racket/list)\n"
                      "  first)\n"))
 (set
  '((6 17) (19 26))
  '((6 17) (47 54))
  '((6 17) (60 67))
  '((27 38) (83 88))
  '((27 38) (40 45))
  '((68 79) (83 88))))

(check-equal?
 (get-binding-arrows (string-append
                      "#lang racket/base\n"
                      "(module n racket/base\n"
                      "  (unless #f 42))\n"))
 (set
  '((6 17) (19 25))
  '((28 39) (53 53))
  '((28 39) (43 49))
  '((28 39) (50 50))))


(check-equal?
 (get-binding-arrows
  (string-append
   "#lang racket/base\n"
   "(module m racket/base\n"
   "  (define lam 1)\n"
   "  (provide lam)\n"
   "  lam)\n"))
 (set '((6 17) (19 25))
      '((28 39) (43 49))
      '((28 39) (54 54))
      '((28 39) (60 67))
      '((50 53) (75 78))
      '((50 53) (68 71))))

(check-equal?
 (get-binding-arrows
  (string-append
   "#lang racket/base\n"
   "(module m racket/base\n"
   "  (define lam 1)\n"
   "  (module+ n (provide lam)))\n"))
 (set '((6 17) (19 25))
      '((28 39) (43 49))
      '((28 39) (54 54))
      '((28 39) (60 67))
      '((28 39) (71 78))
      '((50 53) (79 82))))

(check-equal?
 (get-binding-arrows
  (string-append
   "#lang racket/base\n"
   "(require racket/list)\n"
   "(let ([first #f])\n"
   "  (local-require racket/list)\n"
   "  first)\n"))
 (set
  '((6 17) (53 53))
  '((6 17) (19 26))
  '((6 17) (41 44))
  '((6 17) (61 74))
  '((75 86) (90 95))

  ;; this one is wrong but there doesn't seem to be
  ;; enough information in the fully-expanded form
  ;; to drop it.
  '((27 38) (90 95))))

(check-equal?
 (get-binding-arrows
  (string-append
   "#lang racket/base\n"
   "(require racket/list)\n"
   "first\n"
   "(require (rename-in racket/list [first 1st]))\n"
   "1st\n"))
 (set
  '((6 17) (19 26))
  '((6 17) (47 54))
  '((6 17) (56 65))
  '((27 38) (40 45))
  '((66 77) (92 95)) ;; sketchy; should we eliminate?
  '((85 88) (92 95))))

;                                                       
;                                                       
;                                                       
;                                                       
;                               ;;;                     
;                                                       
;  ;;; ;; ;;;;   ;; ;;; ;;; ;;; ;;; ;;; ;; ;;;;   ;;;;  
;  ;;;;; ;; ;;; ;;;;;;; ;;; ;;; ;;; ;;;;; ;; ;;; ;;; ;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;; ;;; ;;;    
;  ;;;  ;;;;;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;;;;;;  ;;;;  
;  ;;;  ;;;     ;;; ;;; ;;; ;;; ;;; ;;;  ;;;        ;;; 
;  ;;;   ;;;;;; ;;;;;;; ;;;;;;; ;;; ;;;   ;;;;;; ;; ;;; 
;  ;;;    ;;;;   ;; ;;;  ;; ;;; ;;; ;;;    ;;;;   ;;;;  
;                   ;;;                                 
;                   ;;;                                 
;                                                       
;                                                       

(define-get-arrows get-unused-requires
  (syncheck:add-unused-require _ left right)
  (list left right))

(check-equal? (get-unused-requires
               "(module m racket/base (require racket/match) (+ 1 2)))")
              (set '(31 43)))

(check-equal? (get-unused-requires
               "(module m racket/base (require racket/match) #'match))")
              (set))

(define-get-arrows get-prefixed-require-reference
  (syncheck:add-prefixed-require-reference req-src
                                           req-pos-left
                                           req-pos-right
                                           prefix
                                           prefix-src
                                           prefix-left
                                           prefix-right)
  (list req-pos-left
        req-pos-right
        prefix
        prefix-left
        prefix-right))

(check-equal?
 (get-prefixed-require-reference
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (prefix-in : racket/string))\n"
   "(:string-prefix? \"abcdefg\" \"abc\")\n"))
 (set (list 41 54 ': 39 40)))


(define-get-arrows get-require-arrows
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
  ;; #t means an arrow from `require` (but not the #lang built-in require)
  (and (equal? require-arrows? #t)
       (list start-left end-left)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require racket/string)\n"
   "(string-prefix? \"abcdefg\" \"abc\")\n"))
 (set '(28 44)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require racket/string)\n"
   "(string-prefix? \"abcdefg\" \"abc\")\n"
   "(string-prefix? \"abcdefg\" \"abc\")\n"))
 (set '(28 44) '(28 77)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (only-in racket/list first [second deuxième]))\n"
   "first deuxième\n"))
 (set '(37 75) '(37 81)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (except-in racket/list first))\n"
   "second\n"))
 (set '(39 59)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (prefix-in : racket/string))\n"
   "(:string-prefix? \"abcdefg\" \"abc\")\n"))
 (set '(39 58) '(41 59)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (rename-in racket/list [second deuxième]))\n"
   "deuxième\n"))
  (set '(39 71)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (combine-in racket/list racket/string))\n"
   "first string-prefix?\n"))
 (set '(52 74) '(40 68)))

#;
(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (relative-in racket \"list.rkt\"))\n"
   "first\n"))
 (set 'dunno!))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (for-meta 1 racket/base))\n"
   "(begin-for-syntax +)\n"))
 (set '(40 72)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (only-in racket/base #%app))\n"
   "(+ 1 2)\n"))
 (set '(37 57)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "\n"
   "(require (only-in racket/base #%datum))\n"
   "(+ 1 2)\n"))
 (set '(37 62)
      '(37 64)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "(require racket/list)\n"
   "first\n"
   "(module+ m\n"
   "  (require racket/list)\n"
   "  first)\n"))
 (set '(27 83) '(68 83) '(27 40)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "(require (prefix-in x: racket/list) racket/list)\n"
   "x:first\n"
   "first\n"))
 (set '(54 75) '(38 67) '(41 69)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "(require racket/list)\n"
   "(let ([first #f])\n"
   "  (local-require racket/list)\n"
   "  first)\n"))
 (set '(75 90)

      ;; this one is wrong but there doesn't seem to be
      ;; enough information in the fully-expanded form
      ;; to drop it.
      '(27 90)))

(check-equal?
 (get-require-arrows
  (string-append
   "#lang racket/base\n"
   "(require racket/list)\n"
   "first\n"
   "(require (rename-in racket/list [first 1st]))\n"
   "1st\n"))
 (set '(27 40)

      ;; this arrow is sketchy. Can we get rid of it?
      '(66 92)))


;                                                 
;                                                 
;                                                 
;                                                 
;                                             ;;; 
;                                             ;;; 
;  ;;; ;;; ;;; ;;  ;;; ;;;  ;;;;    ;;;;   ;; ;;; 
;  ;;; ;;; ;;;;;;; ;;; ;;; ;;; ;;  ;; ;;; ;;;;;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;    ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;;  ;;;;  ;;;;;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;;    ;;; ;;;     ;;; ;;; 
;  ;;;;;;; ;;; ;;; ;;;;;;; ;; ;;;  ;;;;;; ;;;;;;; 
;   ;; ;;; ;;; ;;;  ;; ;;;  ;;;;    ;;;;   ;; ;;; 
;                                                 
;                                                 
;                                                 
;                                                 

(define-get-arrows get-text-type
  (syncheck:add-text-type _ start end type)
  (and (equal? type 'unused-identifier)
       (list start end)))

(check-equal?
 (get-text-type
  (string-append
   "#lang racket/base\n"
   "(require racket/dict)\n"))
 (set '(27 38)))

(check-equal?
 (get-text-type
  (string-append
   "#lang racket/base\n"
   "(require (combine-in racket/list racket/dict))\n"
   "first"))
 (set '(51 62)))

(check-equal? (get-text-type
               (~s
                '(module m racket/base
                   (require racket/require
                            (multi-in racket (list match))
                            racket/set
                            (rename-in racket/list [first 1st]))
                   first
                   1st)))
              (set '(69 74)           ;racket/match from multi-in
                   '(77 87)))         ;racket/set




;                                                                                                             
;                                                                                                             
;                                                                                                             
;                                                                                                             
;      ;;;          ;;;;            ;;;                               ;  ;                                 ;  
;      ;;;         ;;;                                                ;;;;                               ;;;  
;   ;; ;;;   ;;;;  ;;;; ;;; ;;      ;;; ;;; ;;; ;;; ;; ;;;  ;;; ;;   ; ;;;;  ;;;;;  ;;; ;;;; ;;;   ;;;;  ;;;; 
;  ;;;;;;;  ;; ;;; ;;;; ;;;;;;;     ;;; ;;; ;;; ;;;;;;;;;;; ;;;;;;;  ; ;;;; ;;;;;;; ;;;;;;;;;;;;  ;; ;;; ;;;; 
;  ;;; ;;; ;;; ;;; ;;;  ;;; ;;;     ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ; ;;;  ;;  ;;; ;;;  ;;; ;;; ;;; ;;; ;;;  
;  ;;; ;;; ;;;;;;; ;;;  ;;; ;;;     ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ; ;;;    ;;;;; ;;;  ;;; ;;; ;;;;;;; ;;;  
;  ;;; ;;; ;;;     ;;;  ;;; ;;;     ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ; ;;;  ;;; ;;; ;;;  ;;; ;;; ;;;     ;;;  
;  ;;;;;;;  ;;;;;; ;;;  ;;; ;;;     ;;; ;;;;;;; ;;; ;;; ;;; ;;;;;;; ;  ;;;; ;;; ;;; ;;;  ;;;;;;;  ;;;;;; ;;;; 
;   ;; ;;;   ;;;;  ;;;  ;;; ;;;     ;;;  ;; ;;; ;;; ;;; ;;; ;;; ;;  ;   ;;;  ;;;;;; ;;;   ;; ;;;   ;;;;   ;;; 
;                                  ;;;;                     ;;;                              ;;;              
;                                  ;;;                      ;;;                          ;;;;;;               
;                                                                                                             
;                                                                                                             


(define-get-arrows add-definition-target
  (syncheck:add-definition-target source start-pos end-pos id mods)
  (list id mods))
(define-get-arrows add-definition-target/phase-level+space
  (syncheck:add-definition-target/phase-level+space source start-pos end-pos id mods phase-level)
  (list id mods phase-level))

;; ensure that we get two different
;; identifiers for the two different `x`s
;; that show up
(check-equal? (set-count
               (add-definition-target
                (string-append
                 "#lang racket/base\n"
                 "(define-syntax-rule (m) (begin (define x 10) x))\n"
                 "(m)\n"
                 "(m)\n")))
              3)

(define example-module-defining-x-in-multiple-phases
#<<--
#lang racket/base
(require (for-syntax racket/base))

(module m racket/base
  (require (for-syntax racket/base))

  (define x 0)
  (provide x)

  (begin-for-syntax
    (define x 1)
    (provide x)))
--
  )


(check-equal? (add-definition-target example-module-defining-x-in-multiple-phases)
              (set (list 'x '(m))))

(check-equal? (add-definition-target/phase-level+space example-module-defining-x-in-multiple-phases)
              (set (list 'x '(m) 1)
                   (list 'x '(m) 0)))

(define-get-arrows add-jump-to-definition
  (syncheck:add-jump-to-definition source start-pos end-pos id filename mods)
  (list id mods))
(define-get-arrows add-jump-to-definition/phase-level+space
  (syncheck:add-jump-to-definition/phase-level+space source start-pos end-pos id filename mods phase-level)
  (list id mods phase-level))

(define example-module-with-multiple-phases
#<<--
#lang racket/base
(require (for-syntax racket/base racket/list) racket/list)
(begin-for-syntax (void drop))
(take)
--
  )

(check-equal? (for/set ([x (in-set (add-jump-to-definition example-module-with-multiple-phases))]
                        #:when (member (car x) '(drop take)))
                x)
              (set (list 'drop '())
                   (list 'take '())))

(check-equal? (for/set ([x (in-set (add-jump-to-definition/phase-level+space example-module-with-multiple-phases))]
                        #:when (member (car x) '(drop take)))
                x)
              (set (list 'drop '() 1)
                   (list 'take '() 0)))

(check-equal? (for/set ([x (in-set
                            (add-jump-to-definition/phase-level+space
                             (string-append
                              "#lang racket/base\n"
                              "(require (for-space qq racket/list) racket/list (for-syntax racket/base))\n"
                              "(define-syntax (in-qq stx) (syntax-case stx () [(_ x) ((make-interned-syntax-introducer 'qq) #'x)]))\n"
                              "(in-qq first)\n")))]
                        #:when (equal? (car x) 'first))
                x)
              (set (list 'first '() (cons 0 'qq))))


;                                                                                           
;                                                                                           
;                                                                                           
;                                                                                           
;                                                                                           
;                                                                                           
;        ;;;                                                                                
;        ;;;                                                                                
;        ;;;                                                                                
;     ;; ;;;    ;;;;      ;;;;     ;;;;;        ;;; ;;;  ;;;     ;;;;    ;;; ;;;   ;;;  ;;; 
;    ;;;;;;;   ;;;;;;    ;;;;;;   ;;;  ;;       ;;;;;;;;;;;;;   ;;;;;;   ;;;;;;;;  ;;;  ;;; 
;   ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;           ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;; 
;   ;;;  ;;;  ;;;  ;;;  ;;;       ;;;;;;        ;;;  ;;;  ;;;  ;;;;;;;;  ;;;  ;;;  ;;;  ;;; 
;   ;;;  ;;;  ;;;  ;;;  ;;;        ;;;;;;       ;;;  ;;;  ;;;  ;;;;;;;;  ;;;  ;;;  ;;;  ;;; 
;   ;;;  ;;;  ;;;  ;;;  ;;;  ;;;      ;;;       ;;;  ;;;  ;;;  ;;;       ;;;  ;;;  ;;;  ;;; 
;    ;;;;;;;   ;;;;;;    ;;;;;;   ;;  ;;;       ;;;  ;;;  ;;;   ;;;;;;;  ;;;  ;;;  ;;;;;;;; 
;     ;; ;;;    ;;;;      ;;;;     ;;;;;        ;;;  ;;;  ;;;    ;;;;;   ;;;  ;;;   ;;; ;;; 
;                                                                                           
;                                                                                           
;                                                                                           
;                                                                                           
;                                                                                           


(define-get-arrows add-docs-menu
  (syncheck:add-docs-menu src start end id label definition-tag path tag)
  id)

(check-equal? (add-docs-menu
               (string-append
                "#lang racket/base\n"
                "(+ 1 2)"))
              (set '#%datum '#%app '+))

(check-equal? (add-docs-menu
               (string-append
                "#lang racket/base\n"
                "(require (for-syntax racket/base))\n"
                "(begin-for-syntax\n"
                "  (λ (car cdr)"
                "    #t))"))
              (set '#%datum 'for-syntax 'λ 'require 'begin-for-syntax))

(check-equal? (add-docs-menu
               (string-append
                "#lang racket/base\n"
                "(require (for-syntax racket/match racket/base))\n"
                "(begin-for-syntax\n"
                "  (match 1 [_ 1]))"))
              (set '#%datum 'for-syntax 'require 'match 'begin-for-syntax))


;                                 
;                                 
;                                 
;                                 
;              ;;;                
;                                 
;  ;;; ;; ;;;  ;;;  ;;;;    ;;;   
;  ;;;;;;;;;;; ;;; ;;; ;;  ;;;;;  
;  ;;; ;;; ;;; ;;; ;;;    ;;;  ;; 
;  ;;; ;;; ;;; ;;;  ;;;;  ;;;     
;  ;;; ;;; ;;; ;;;    ;;; ;;;  ;; 
;  ;;; ;;; ;;; ;;; ;; ;;;  ;;;;;  
;  ;;; ;;; ;;; ;;;  ;;;;    ;;;   
;                                 
;                                 
;                                 
;                                 


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

;; ensure that the submodules argument gets correctly sent along
;; in a syncheck:add-jump-to-definition/phase-level+space call
(let ()
  (define root-dir (make-temporary-file "test-from-syncheck-direct-rkt~a" 'directory))
  (define src (build-path root-dir "a.rkt"))
  (call-with-output-file src
    (λ (port)
      (displayln "#lang racket/base\n" port)
      (writeln '(require (submod "b.rkt" the-mod)) port)
      (writeln 'the-name port)))
  (call-with-output-file (build-path root-dir "b.rkt")
    (λ (port)
      (displayln "#lang racket/base\n" port)
      (writeln '(module the-mod racket (provide the-name) (define the-name 1)) port)))

  (define content
    (parameterize ([current-directory root-dir])
      (show-content "a.rkt")))

  (check-true
   (for/or ([item (in-list content)])
     (match item
       [(vector 'syncheck:add-jump-to-definition/phase-level+space _ _ 'the-name _ '(the-mod) _)
        #t]
       [_ #f])))
  (delete-directory/files root-dir))

;; make sure that `make-traversal` is called with
;; the containing directory by `show-content`
(let ()
  (define root-dir (make-temporary-file "test-from-syncheck-direct-rkt~a" 'directory))
  (parameterize ([current-directory root-dir])
    (make-directory "test"))
  (define tmp-dir (build-path root-dir "test"))
  (define src (build-path tmp-dir "prefix.rkt"))
  (call-with-output-file src
    (λ (port)
      (displayln "#lang racket/base\n" port)
      (writeln '(require "x.rkt") port)
      (writeln 'x port)))
  (call-with-output-file (build-path tmp-dir "x.rkt")
    (λ (port)
      (displayln "#lang racket/base\n" port)
      (writeln '(define x 1) port)
      (writeln '(provide x) port)))

  (define content
    (parameterize ([current-directory tmp-dir])
      (show-content src)))

  ;; test show-content on relative path
  (check-not-exn
   (λ ()
     (parameterize ([current-directory root-dir])
       (show-content "test/prefix.rkt"))))

  ;; test make-traversal on relative path
  (parameterize ([current-directory root-dir])
    (define ns (make-base-namespace))
    (define-values (add-syntax done)
      (make-traversal ns "test"))
    (parameterize ([current-annotations (new basic-build-trace% [src src])]
                   [current-load-relative-directory tmp-dir]
                   [current-namespace ns])
      (define stx (call-with-input-file src
                    (λ (port)
                      (port-count-lines! port)
                      (with-module-reading-parameterization
                        (λ ()
                          (read-syntax src port))))))
      (add-syntax (expand stx))
      (done)
      (define trace (send (current-annotations) get-trace))

      ;; if it's successful, we should see add-require-open-menu to x.rkt
      (check-true
       (for/or ([entry (in-list trace)])
         (match entry
           [(vector 'syncheck:add-require-open-menu _ _ path)
            (equal? path (build-path tmp-dir "x.rkt"))]
           [_ #f])))))


  (define paths
    (let loop ([content content])
      (cond
        [(vector? content)
         (apply
          set-union
          (for/list ([x (in-vector content)])
            (loop x)))]
        [(pair? content)
         (set-union (loop (car content))
                    (loop (cdr content)))]
        [(path? content)
         (set content)]
        [else
         (set)])))

  (define (path-extension-of? p1 p2)
    (define p1-eles (explode-path p1))
    (define p2-eles (explode-path p2))
    (and (<= (length p1-eles) (length p2-eles))
         (for/and ([p1-ele (in-list p1-eles)]
                   [p2-ele (in-list p2-eles)])
           (equal? p1-ele p2-ele))))

  (for ([path (in-set paths)])
    (when (path-extension-of? tmp-dir path)
      (check-pred file-exists? path)))

  (delete-directory/files root-dir))

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
