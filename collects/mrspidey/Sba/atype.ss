;; atype.ss - handles annotated types
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

;; ----------------------------------------------------------------------
;; FlowType      - expr, edgeto, edgefrom, type-annotation
;; Tvar          - sub-structure of FlowType, is type variable
;; fo-FlowType   - FlowType with def field containing fo-Atype
;; fo-Atype      - atprim etc, see below
;; Atype         - Tvar or fo-Atype
;; ----------------------------------------------------------------------

(define-const-typed-structure (fo-FlowType struct:FlowType)
  ( (: def fo-Atype)
    (: tvar Tvar)))

(define (FlowType->Atype ftype)
  (cond
    [(Tvar? ftype) ftype]
    [(fo-FlowType? ftype) (fo-FlowType-def ftype)]
    [else
      (mrspidey:internal-error 'FlowType->Atype "Bad ftype ~s" ftype)]))

(define (create-fo-FlowType fo-atype)
  (assert (fo-Atype? fo-atype) 'create-fo-FlowType fo-atype)
  (if (and 
        (not (st:use-fo-ftype))
        (or
          (atconst? fo-atype)
          (and (atvalues? fo-atype)
            (andmap Tvar? (atvalues-values fo-atype)))))
    (Atype->Tvar fo-atype)
    (add-FlowType! 
      (make-fo-FlowType num-ftype #f '() '() #f '() #f fo-atype #f))))
  
;; ----------

(define-type fo-Atype
  (union atconst schema atprim atthunk atstruct atvalues
         atunit atlunit))

(define (fo-Atype? x)
  (or (atconst? x) 
      (schema? x)
      (atprim? x)
      (atthunk? x)
      (atstruct? x)
      (atvalues? x)
      (atunit? x)
      (atlunit? x)))

(define (poly-atype? x)
  (or (schema? x)
      (atthunk? x)))

;; --------------------
;; atconst

(define-const-typed-structure
  atconst ((: c (union num bool))))

;; --------------------
;; schema

(define-const-typed-structure
  schema ((: tvar Tvar)
          (: tvar* (listof Tvar))
          (: edges (listof (cons Tvar Tvar)))))

;; --------------------
;; atprim

(define-const-typed-structure
  atprim ((: sym sym)
           (: type sexp)
           (: domain-filters (listof filter))
           predicate-fn
           attrs
           orig-type
           ))

;; filter-domains is a list of filters for various args,
;; if no error raised
;; inj-predicate is a fn : list-tvar x list-tvar x tvar x bool -> Tvar 
;; used for (if (pred ... x ...) ...)

;; --------------------
;; atthunk

(define-const-typed-structure
  atthunk ((: thunk (-> FlowType))))

;; --------------------
;; atstruct

(define-const-typed-structure
  atstruct ((: struct:sym sym)
            (: super-constructors (listof sym))
            parent-gen-args
            parent-match-args
            parent-field-types
            parent-list-mutable))

;; ----------------------------------------------------------------------
;; atvalues

(define-const-typed-structure
  atvalues ((: values (listof FlowType))))

(define (wrap-value x) 
  (let* ([ftype (cond 
                  [(FlowType? x) x]
                  [(fo-Atype? x) (create-fo-FlowType x)]
                  [else (mrspidey:internal-error 
                          'wrap-value "Bad x ~s" x)])]
          [ftype (if (or (FlowType-expr ftype)
                         (FlowType-values-ftype ftype))
                   (copy-ftype ftype)
                   ftype)]
          [r (create-fo-FlowType (make-atvalues (list ftype)))])
    (set-FlowType-values-ftype! ftype r)
    (pretty-debug-atype
      `(wrap-value ,(FlowType-name ftype) ,(FlowType-name r)))
    r))

(define (extract-1st-value ftype)
  (car (multiple-value-components ftype 1)))

(define (multiple-value-components ftype n)
  ;; returns list of n value components
  (assert (integer? n) 'multiple-value-components)
  (cond
   [(zero? n) '()]
   [(fo-FlowType? ftype)
    (match (fo-FlowType-def ftype)
      [($ atvalues l) 
       (recur loop ([n n][l l])
         (cond [(zero? n) '()]
               [(null? l) (cons (mk-tvar-empty) (loop (sub1 n) '()))]
               [else (let* ( [a (car l)]
                             [b (copy-ftype a) ])
                       ;(if (FlowType-expr a) (copy-ftype a) a)
                       (when need-explanation (add-FlowType-arrow! ftype b))
                       (cons b (loop (sub1 n) (cdr l))))]))]
      [_ #f])]
   [else
     (let ( [tvar-mv (FlowType->Tvar ftype)]
            [tvar (mk-Tvar 'get-mvalues)])
       (new-con! tvar-mv (create-con template-mvalues 0 tvar #t))
       (recur loop ([tvar tvar][n n])
         (let ([a (mk-Tvar 'car-value)])
           (new-con! tvar (make-con-car a))
           (when need-explanation (add-FlowType-arrow! tvar-mv a))
           (cons a
             (if (= n 1)
               '()
               (let ([d (mk-Tvar 'cdr-value)])
                 (new-con! tvar (make-con-cdr d))
                 (loop d (sub1 n))))))))]))

;; ----------------------------------------------------------------------
;; Annotated (lazy) unit types

(define-const-typed-structure
  atunit
  ((: imports (listof (cons sym (listof Tvar))))
   (: exports (listof (cons sym FlowType)))
   (: result FlowType)
   (: expr zodiac:parsed)))

(define-const-typed-structure
  atlunit ((! ui (union false atunit))))

;; ----------------------------------------------------------------------

;; (: FlowType->Tvar (FlowType -> Tvar))

(define (FlowType->Tvar ftype)
  (let ([tvar
          (cond
            [(Tvar? ftype) ftype]
            [(fo-FlowType? ftype)
              (or (fo-FlowType-tvar ftype)
                (let ([tvar (Atype->Tvar (fo-FlowType-def ftype))])
                  (when need-explanation (add-FlowType-arrow! ftype tvar))
                  ;(set-fo-FlowType-tvar! ftype tvar)
                  tvar))]
            [else
              (mrspidey:internal-error 'FlowType->Tvar "Bad FlowType ~s" ftype)])])
    (pretty-debug-atype
      `(FlowType->Tvar ,(FlowType-name ftype) ,(Tvar-name tvar)))
    tvar
))

(define Atype->Tvar 
  (lambda (atype)
    (let ([tvar
            (match atype
              [(? Tvar? tvar) tvar]
              [($ atconst c)
                (let ([tvar (mk-Tvar 'atconst->tvar)])
                  (new-AV! tvar (traverse-const-exact c))
                  tvar)]
              [($ atstruct) (mk-tvar-void)]
              [($ atthunk thunk) (thunk)]
              [(and pi ($ atprim name type))
                (let ([tvar (mk-Tvar name)])
                  (tschema->con type tvar name pi)
                  tvar)]
              [($ schema tvar tvar* edges)
                (copy-constraint-set tvar tvar* edges)]
              [($ atunit imports exports result)
                (pretty-debug-atype
                  `(Atype->Tvar (atunit ,imports ,exports ,result)))
                (let ([tvar-u (mk-Tvar 'inst-unit)])
                  (for-each-with-n
                    (lambda (import n)
                      (map (lambda (tvar)
                             (new-AV! tvar-u
                               (create-AV (get-unit-import-template n)
                                 '() (vector) (vector tvar))))
                        (cdr import)))
                    imports)
                  (for-each
                    (match-lambda
                      [(sym . ftype)
                        (new-AV! tvar-u
                          (create-AV (get-unit-export-template sym)
                            '() (vector (FlowType->Tvar ftype)) (vector)))])
                    exports)
                  (new-AV! tvar-u
                    (create-AV template-unit '() 
                      (vector (FlowType->Tvar result)) 
                      (vector)))
                  tvar-u)]
              [(and atlunit ($ atlunit))
                (Atype->Tvar (atlunit->atunit atlunit))]
              [($ atvalues ftype*)
                (let ([tvar-list
                        (foldr
                          (lambda (ftype tvar-rest)
                            (let* ([tvar (mk-Tvar 'atvalues->tvar)])
                              (new-AV! tvar 
                                (make-AV-cons (FlowType->Tvar ftype)
                                  tvar-rest))
                              tvar))
                          (mk-tvar-nil)
                          ftype*)]
                       [tvar (mk-Tvar 'atvalues->values)])
                  (new-AV! tvar 
                    (create-AV template-mvalues '() 
                      (vector tvar-list) (vector)))
                  tvar)]
              [x (mrspidey:internal-error 'Atype->Tvar "Bad Atype ~s" x)])])
      (pretty-debug-atype
        `(atype->tvar ,(Atype->pretty atype) ,(Tvar-name tvar)))
      tvar)))

;; ----------------------------------------------------------------------

(define FlowType->pretty
  (lambda (ftype)
    (cond
     [(Tvar? ftype) (Tvar-name ftype)]
     [(fo-FlowType? ftype)
       (Atype->pretty (fo-FlowType-def ftype))])))

(define Atype->pretty
  (match-lambda
    [($ atconst c) `(const ,c)]
    [($ atthunk thunk) 'thunk]
    [(and pi ($ atprim name _ _ _ attrs type)) `(prim ,name ,type)]
    [($ schema tvar tvar* edges) 
      `(schema ,(Tvar-name tvar) ,(map Tvar-name tvar*))]
    [($ atunit imports exports result)
      `(atunit 
         (imports ,@(map (match-lambda
                           [(sym . tvar*)
                             (cons sym (map Tvar-name tvar*))])
                      imports))
         (exports ,@(map (match-lambda
                           [(sym . ftype)
                             (list sym (FlowType->pretty ftype))])
                      exports))
         (result ,(FlowType->pretty result)))]
    [($ atlunit thunk) `(lazy-unit)]
    [($ atstruct A B C D E F) (list 'struct: A B C D E F)]
    [($ atvalues ftype*) `(values ,@(map FlowType->pretty ftype*))]
    [x
      (mrspidey:internal-error 'Atype->pretty "Bad fo-Atype ~s" x)]))

(define FlowType->SDL
  (lambda (ftype)
    (pretty-debug-sdl2
      `(FlowType->SDL ,(FlowType-name ftype) ,(FlowType->pretty ftype)))
    (cond
     [(Tvar? ftype) (Tvar->SDL ftype)]
     [(fo-FlowType? ftype)
      (match (fo-FlowType-def ftype)
        [($ atconst c) `(const ,c)]
        [($ atthunk thunk) `(thunk ,(Tvar->SDL (thunk)))]
        [($ atprim name _ _ _ _ type) `(prim ,type)]
        [($ schema tvar tvar* edges) `(schema ,(Tvar->SDL tvar))]
        [(and def ($ atunit imports exports result))
         `(unit ,(Tvar->SDL (FlowType->Tvar ftype)))]
        [(and ($ atlunit thunk) lui)
         `(lazy ,(FlowType->SDL (create-fo-FlowType (atlunit->atunit lui))))]
        [($ atstruct) 'struct:]
        [($ atvalues (ftype)) (FlowType->SDL ftype)]
        [($ atvalues ftype*) `(values ,@(map FlowType->SDL ftype*))]
        [x (mrspidey:internal-error 'FlowType->SDL "Bad fo-Atype ~s" x)])]
     [else (mrspidey:internal-error 'FlowType->SDL "Bad ftype ~s" ftype)])))

(define (copy-ftype ftype)
  (cond
    [(Tvar? ftype)
      (let ([tvar (mk-Tvar 'copy)])
        (new-edge! ftype tvar)
        tvar)]
    [(fo-FlowType? ftype)
      (let ([nu-ftype (create-fo-FlowType (fo-FlowType-def ftype))])
        (when need-explanation (add-FlowType-arrow! ftype nu-ftype))
        nu-ftype)]
    [else (mrspidey:internal-error 'link-parsed-ftype!
            "Bad ftype ~s, maybe old language" ftype)]))

;; ----------------------------------------------------------------------


