;; templates.ss
;; Handles the constructor environment,
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------
;;
;; ======================================================================
;; Constructor environments: symbol -> template
;; First a helper function

(define (constructor->template con . modes)
  (let* ( [n (length modes)]
          [assign (make-vector n #f)]
          [ref (make-vector n #f)])
    (recur loop ([i 0][n 0][modes modes])
      (match modes
        [() (make-template con n i ref assign '() eqv?)]
        [(#t . rest)
          (vector-set! ref n n)
          (vector-set! assign n i)
          (loop (add1 i) (add1 n) rest)]
        [(#f . rest)
          (vector-set! ref n n)
          (vector-set! assign n #f)
          (loop i (add1 n) rest)]))))

;; ======================================================================
;; The constructor environment

(define constructor-env (void))

(define (set-constructor-env! c)
  (set! constructor-env (make-hash-table))
  (hash-table-for-each c
    (lambda (key val)
      (hash-table-put! constructor-env key val))))

(define (extend-constructor-env! template)
  (let* ( [type (template-type template)]
          [old (hash-table-get constructor-env type (lambda () #f))])
    (if old
      (match (list template old)
        [( ($ template _ n+1 n-1 ref1 assign1 super1 _) 
           ($ template _ n+2 n-2 ref2 assign2 super2 _))
          (unless (and (= n+1 n+2)
                    (= n-1 n-2)
                    (equal? assign1 assign2)
                    (equal? ref1 ref2)
                    (equal? super1 super2))
            (pretty-print-debug
              `( (,n+1 ,n-1 ,ref1 ,assign1 ,(map template-type super1))
                 (,n+2 ,n-2 ,ref2 ,assign2 ,(map template-type super2))))
            (mrspidey:error 
              (format "New definition of template ~s does not match old" 
                type)))
          old])
      (begin
        (hash-table-put! constructor-env type template)
        template))))

(define (add-constructor! con . modes)
  (extend-constructor-env! (apply constructor->template con modes)))

(define (constructor-alias! new-con old-con)
  (hash-table-put! constructor-env new-con 
    (lookup-template-or-error old-con)))

(define (record-super-constructor! super-C C)
  (record-super-constructor-of-template! 
    super-C (lookup-template-or-error C)))

(define (record-super-constructor-of-template! super-C T)
  (record-super-template! (lookup-template-or-error super-C) T))

(define (record-super-template! super-T T)
  (set-template-super-templates! 
    T
    (cons super-T (template-super-templates T))))

;; ======================================================================
;; Default templates

;(define-typed-structure (lam-info nargs restarg))

(define lam-misc-eq?
  (match-lambda*
   [(('lam-info nargs1 restarg1) ('lam-info nargs2 restarg2))
    (and (= nargs1 nargs2) (eqv? restarg1 restarg2))
    #t]
   [(x y) (eq? x y)]))

(define template-lam
  (make-template
   'lambda
    1
    1
    (vector #f 0)
    (vector 0 #f)
    '()
    lam-misc-eq?))

(define filter-not-lam
  (create-filter #f (list template-lam)))

(define template-lam++ ;; monotonic in both positions
  (make-template
   'lambda
    2
    0
    (vector 0 1)
    (vector)
    '()
    lam-misc-eq?))

;; ----------------------------------------------------------------------

(define template-top-s (void))
(define template-cons  (void))
(define template-nil   (void))
(define template-num   (void))
(define template-sym   (void))
(define template-str   (void))
(define template-char  (void))
(define template-void  (void))
(define template-undefined (void))
(define template-true  (void))
(define template-false (void))
(define template-promise (void))
(define template-unit    (void))
(define template-structure (void))
(define template-mvalues (void))
(define template-internal-class   (void))
(define template-all-ivars (void))
(define template-dots   (void))
(define template-ivarset (void))

(define (init-default-constructor-env!)
  (pretty-debug ' (init-default-constructor-env!))
  ;; These are things needed by the analysis
  (set! constructor-env (make-hash-table))
  (hash-table-put! constructor-env 'lambda template-lam)
  (set! template-top-s (add-constructor! 'top-s))
  (set! template-cons  
    (if (st:cons-mutable)
      (add-constructor! 'cons #t #t)
      (add-constructor! 'cons #f #f)))
  (set! template-nil   (add-constructor! 'nil  ))
  (set! template-num   (add-constructor! 'num  ))
  (set! template-sym   (add-constructor! 'sym  ))
  (set! template-str   (add-constructor! 'str  ))
  (set! template-char  (add-constructor! 'char ))
  (set! template-void  (add-constructor! 'void ))
  (set! template-undefined (add-constructor! 'undefined))
  (set! template-true  (add-constructor! 'true ))
  (set! template-false (add-constructor! 'false))
  (set! template-promise   (add-constructor! 'promise #f))
  (set! template-unit      (add-constructor! 'unit-result* #f))
  (set! template-structure (add-constructor! 'structure:))
  (set! template-mvalues   (add-constructor! 'mvalues #f))
  (set! template-internal-class 
    (extend-constructor-env! 
      (make-template
        'internal-class
        4 4
        (vector 0  1  2  #f #f  3 #f #f 4)
        (vector #f #f #f  0  1 #f  2  3 #f)
        '() eq?)))
  (set! template-all-ivars
    (extend-constructor-env!
      (make-template
        'all-ivars
        1
        0
        (vector 0)
        (vector #f)
        '()
        eq?)))
  (set! template-dots   (add-constructor! '...))
  (set! template-ivarset (add-constructor! 'ivarset #t))
  )

; ======================================================================
; The "template-prompt"

(define saved-constructor-env (make-hash-table))

(define (init-constructor-env!)
  (set! constructor-env saved-constructor-env)
  (set! unit-import-export-env (vector (make-hash-table) (make-hash-table)))
  (set! object-ivar-template-env (make-hash-table))
  )

; ======================================================================
; Unit templates

(define unit-import-export-env (vector (make-hash-table) (make-hash-table)))

(define (get-unit-import-export-template ndx sym thunk)
  (hash-table-get
   (vector-ref unit-import-export-env ndx)
   sym
   (lambda ()
     (let ([template (thunk)])
       (hash-table-put!
         (vector-ref unit-import-export-env ndx)
         sym template)
       template))))

(define (get-unit-import-template sym)
  (get-unit-import-export-template 
   0 sym
   (lambda ()
     (extend-constructor-env!
       (make-template
         (symbol-append 'unit-import- sym)
         0
         1
         (vector #f)
         (vector 0)
         '()
         eq?)))))

(define (get-unit-export-template sym)
  (assert (symbol? sym) 'get-unit-export-template)
  (get-unit-import-export-template 
   1 sym
   (lambda ()
     (extend-constructor-env!
       (make-template
         (symbol-append 'unit-export- sym)
         1
         0
         (vector 0)
         (vector #f)
         '()
         eq?)))))

; ======================================================================
; Object templates

(define object-ivar-template-env (make-hash-table))

(define (get-ivar-template sym)
  (assert (symbol? sym) 'get-ivar-template sym)
  (hash-table-get
    object-ivar-template-env
    sym
    (lambda ()
      (let ([template (make-template
                        (symbol-append 'ivar- sym)
                        1
                        0
                        (vector 0)
                        (vector #f)
                        '()
                        eq?)])
        (hash-table-put! object-ivar-template-env sym template)
        (record-super-template! template-all-ivars template)
        (extend-constructor-env! template)
        template))))

;; ======================================================================

(define (type-constructor? C)
  (if 
    (hash-table-get constructor-env C (lambda () #f))
    #t #f))

(define (lookup-template C) 
  (hash-table-get constructor-env C (lambda () #f)))

(define (lookup-template-or-error C) 
  (or (lookup-template C)
    (mrspidey:error (format "Unknown type constructor ~s" C))))

(define is-template?
  (lambda (name)
    (lambda (t)
      (eq? t (lookup-template name)))))

;; ======================================================================









