;; kernel-aux.ss
;; Helper functions for building constraints
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
;;
(define (make-constructed-AV-template template . args)
  (match-let* ( [($ template type num+ num- ref assign) template]
                [fields+ (make-vector num+)]
                [fields- (make-vector num-)])
    (for-each-with-n
      (lambda (arg n)
        (if (vector-ref assign n)
          ;; Mutable - fill next two fields
          (let ([tvar (mk-Tvar 'mut-field)])
            (new-edge! arg tvar)
            (vector-set! fields- (vector-ref assign n) tvar)
            (when (vector-ref ref n)
              (vector-set! fields+ (vector-ref ref n) tvar)))
          ;; Immutable - fill one field
          (when (vector-ref ref n)
              (vector-set! fields+ (vector-ref ref n) arg))))
      args)
    (create-AV template '() fields+ fields-)))

(define (make-constructed-AV C . args)
  (apply make-constructed-AV-template (lookup-template C) args))

(define (make-constructed-Tvar C . args)
  (let ([tvar (mk-Tvar 'constructed-Tvar)]
        [AV (apply make-constructed-AV C args)])
    (new-AV! tvar AV)
    tvar))

;; ----------------------------------------------------------------------

(define (make-AV-cons a d)
  (if (st:cons-mutable)
    (let* ( [fields+ (make-vector 2)]
            [fields- (make-vector 2)])
      (let ([tvar (mk-Tvar 'mut-field)])
        (new-edge! a tvar)
        (vector-set! fields+ 0 tvar)
        (vector-set! fields- 0 tvar))
      (let ([tvar (mk-Tvar 'mut-field)])
        (new-edge! d tvar)
        (vector-set! fields+ 1 tvar)
        (vector-set! fields- 1 tvar))
      (create-AV template-cons '() fields+ fields-))
    (create-AV template-cons '() (vector a d) (vector))))


(define (make-con-car tvar) (create-con template-cons 0 tvar #t))
(define (make-con-cdr tvar) (create-con template-cons 1 tvar #t))
(define (make-con-dom tvar) (create-con template-lam 0 tvar #f))
(define (make-con-rng tvar) (create-con template-lam 0 tvar #t))

(define (make-AV-vec a) 
  (cond
    [(lookup-template 'vect)
      =>
      (lambda (template-vect)
        (make-constructed-AV-template template-vect a AV-numb))]
    [(lookup-template 'vec)
      =>
      (lambda (template-vec)
        (make-constructed-AV-template template-vec a))]))

(define (make-AV-lam dom rng nargs restarg)
  (create-AV 
    template-lam 
    (list 'lam-info nargs restarg)
    (vector rng) 
    (vector dom)))

(define AV-nil   (void))
(define AV-numb  (void))
(define AV-sym   (void))
(define AV-str   (void))
(define AV-char  (void))
(define AV-true  (void))
(define AV-false (void))
(define AV-void  (void))
(define AV-undefined (void))
(define AV-top-s (void))

(define (mk-tvar-nil)   (mk-Tvar-init-AV 'nil   AV-nil))
(define (mk-tvar-numb)  (mk-Tvar-init-AV 'num   AV-numb))
(define (mk-tvar-sym)   (mk-Tvar-init-AV 'sym   AV-sym))
(define (mk-tvar-str)   (mk-Tvar-init-AV 'str   AV-str))
(define (mk-tvar-char)  (mk-Tvar-init-AV 'char  AV-char))
(define (mk-tvar-true)  (mk-Tvar-init-AV 'true  AV-true))
(define (mk-tvar-false) (mk-Tvar-init-AV 'false AV-false))
(define (mk-tvar-empty) (mk-Tvar 'empty))
(define (mk-tvar-void)
  (if (st:see-void) 
    (mk-Tvar-init-AV 'void  AV-void)
    (mk-Tvar 'void)))
(define (mk-tvar-undefined) (mk-Tvar-init-AV 'undefined AV-undefined))

(define (init-common-AV!)
  (unless (template? template-nil)
    (mrspidey:internal-error
      "template-nil not a template, language probably not specified"))
  (set! AV-nil    (make-constructed-AV-template template-nil))
  (set! AV-numb   (make-constructed-AV-template template-num))
  (set! AV-sym    (make-constructed-AV-template template-sym))
  (set! AV-str    (make-constructed-AV-template template-str))
  (set! AV-char   (make-constructed-AV-template template-char))
  (set! AV-true   (make-constructed-AV-template template-true))
  (set! AV-false  (make-constructed-AV-template template-false))
  (set! AV-void   (make-constructed-AV-template template-void))
  (set! AV-undefined (make-constructed-AV-template template-undefined))
  (set! AV-top-s  (make-constructed-AV-template template-top-s))
  )

;; ======================================================================

(define traverse-simple-const
  ;; Returns an AV, or #f
  (match-lambda
   [(or ($ zodiac:char _ _ _ c) (? char? c))
    (if (st:constants)
        (create-AV template-char c (vector) (vector))
        AV-char)]
   [(or ($ zodiac:symbol _ _ _ sym) (? symbol? sym))
    (if (st:constants)
        (create-AV template-sym sym (vector) (vector))
        AV-sym)]
   [(or ($ zodiac:number _ _ _ num) (? number? num))
    (if (st:constants)
        (create-AV template-num num (vector) (vector))
        AV-numb)]
   [(or ($ zodiac:string _ _ _ str) (? string? str)) AV-str]
   [(or ($ zodiac:boolean _ _ _ #t) #t) AV-true]
   [(or ($ zodiac:boolean _ _ _ #f) #f) AV-false]
   [(or ($ zodiac:list _ _ _ ()) ()) AV-nil]
   [_ #f]))

(define traverse-const-exact
  ;; Returns an AV
  (lambda (V)
    (or (traverse-simple-const V)
        (match V
          [(or ($ zodiac:list _ _ _ l)
               (? pair? l)
               (? null? l))
           (recur loop ([l l])
             (match l
               [(a . d)
                (let ([tvar-a (mk-Tvar 'car)]
                      [tvar-d (mk-Tvar 'cdr)])
                  (new-AV! tvar-a (traverse-const-exact a))
                  (new-AV! tvar-d (loop d))
                  (make-AV-cons tvar-a tvar-d))]
               [() AV-nil]
               [x (traverse-const-exact x)]))]
          [($ zodiac:improper-list _ _ _ l)
           (recur loop ([l l])
             (match l
               [(x) (traverse-const-exact x)]
               [(a . d)
                (let ([tvar-a (mk-Tvar 'car)]
                      [tvar-d (mk-Tvar 'cdr)])
                  (new-AV! tvar-a (traverse-const-exact a))
                  (new-AV! tvar-d (loop d))
                  (make-AV-cons tvar-a tvar-d))]))]
          [($ zodiac:vector _ _ _ v)
           (let ([tvar-e (mk-Tvar 'vec-field)])
             (for-each
              (lambda (e) (new-AV! tvar-e (traverse-const-exact e)))
              v)
             (make-AV-vec tvar-e))]
          [(? vector? v)
           (let ([tvar-e (mk-Tvar 'vec-field)])
             (for-each
              (lambda (e) (new-AV! tvar-e (traverse-const-exact e)))
              (vector->list v))
             (make-AV-vec tvar-e))]
          [($ zodiac:box _ _ _ b)
           (let ([tvar-e (mk-Tvar 'box-field)])
             (new-AV! tvar-e (traverse-const-exact b))
             (make-constructed-AV 'box tvar-e))]
          [(? box? b)
           (let ([tvar-e (mk-Tvar 'box-field)])
             (new-AV! tvar-e (traverse-const-exact (unbox b)))
             (make-constructed-AV 'box tvar-e)) ]
          [(? void?) 
            (make-constructed-AV 'void)]
          [obj (error 'traverse-const-exact "Bad const ~s" obj)]))))

;; ======================================================================
;; Transitive closure of edgeto
;; Could use faster algorithm here

(define (Tvar-transitive-edgeto Tvar)
  (let*-vals ( [(reached? set-reached!) (alloc-Tvar-field)]
               [edgeto '()])
    (recur loop ([Tvar Tvar])
      (unless (reached? Tvar)
        (set-reached! Tvar #t)
        (set! edgeto (cons Tvar edgeto))
        (for-each loop (Tvar-edgeto Tvar))
        (for-each
          (match-lambda
            [($ con-filter _ _ to) (loop to)]
            [_ (void)])
          (Tvar-constraints Tvar))))
    (pretty-debug 
      `(Tvar-trans-edgeto ,(Tvar-name Tvar) ,(map Tvar-name edgeto)))
    edgeto))

(define (Tvar-transitive-edgefrom Tvar)
  (let*-vals ( [(reached? set-reached!) (alloc-Tvar-field)]
               [edgefrom '()])
    (recur loop ([Tvar Tvar])
      (unless (reached? Tvar)
        (set-reached! Tvar #t)
        (set! edgefrom (cons Tvar edgefrom))
        (for-each loop (Tvar-edgefrom Tvar))))
    (pretty-debug 
      `(Tvar-trans-edgefrom ,(Tvar-name Tvar) ,(map Tvar-name edgefrom)))
    edgefrom))

;; ======================================================================

(define (copy-constraint-set tvar tvar* edges)
  ;; copies all Tvars in list tvar*
  ;; edges: (listof (cons Tvar Tvar))

  (pretty-debug
    `(copy-constraint-set ,(Tvar-name (car tvar*)) ,(Tvar-name (rac tvar*))))

  (let*-vals
    ( [(tvar-nutvar set-tvar-nutvar!) (alloc-Tvar-field)]
      [(AV-nuAV set-AV-nuAV!) (alloc-AV-field)]
      [Tvar->nuTvar (lambda (tvar) (or (tvar-nutvar tvar) tvar))]
      [copy-AV 
        (lambda (AV)
          (or 
            (AV-nuAV AV)
            (match AV
              [(and AV ($ AV _ template misc fields+ fields-))
                (if (or 
                      (vector-ormap tvar-nutvar fields+)
                      (vector-ormap tvar-nutvar fields-))
                  (let* ( [nu-AV (create-AV 
                                   template misc 
                                   (vector-map Tvar->nuTvar fields+)
                                   (vector-map Tvar->nuTvar fields-))])
                    (set-AV-nuAV! AV nu-AV)
                    nu-AV)
                  (begin
                    (set-AV-nuAV! AV AV)
                    AV))])))])

    (for-each 
      (lambda (Tvar) (set-tvar-nutvar! Tvar (mk-Tvar 'copy-constraint-set)))
      tvar*)

    (for-each 
      (lambda (source)
        (let ([dest (Tvar->nuTvar source)])
          ;; --- AV
          (for-each
            (lambda (AV) (new-AV! dest (copy-AV AV)))
            (Tvar-objs source))
          ;; --- Constraints
          (for-each
            (match-lambda
              [($ con _ template field-no Tvar sign)
                (new-con! dest 
                  (create-con template field-no (Tvar->nuTvar Tvar) sign))]
              [($ con-filter _ filter Tvar)
                (new-con! dest
                  (create-con-filter filter (Tvar->nuTvar Tvar)))])
            (Tvar-constraints source))
          ;; --- Edges
          (for-each
            (lambda (Tvar2) (new-edge! dest (Tvar->nuTvar Tvar2)))
            (Tvar-edgeto source))))
      tvar*)

    (for-each
      (match-lambda
        [(from . to)
          (new-edge! (Tvar->nuTvar from) (Tvar->nuTvar to))])
      edges)

    (Tvar->nuTvar tvar)))

; ======================================================================
; Non-Terminals

(define-typed-structure NT (tvar type rhs*))

(define mk-Tvar-NTs!
  (lambda (Tvar)
    (set-Tvar-L!  Tvar (make-NT Tvar 'L '()))
    (set-Tvar-U!  Tvar (make-NT Tvar 'U '()))
    ))

(define mk-AV-NTs!
  (lambda (AV)
    (set-AV-U!  AV (make-NT AV 'U '()))
    ))

(define (chk-Tvar-L tvar)
  (or (Tvar-L tvar)
    (begin
      (mk-Tvar-NTs! tvar)
      (Tvar-L tvar))))

(define (chk-Tvar-U tvar)
  (or (Tvar-U tvar)
    (begin
      (mk-Tvar-NTs! tvar)
      (Tvar-U tvar))))

(define (chk-AV-U AV)
  (or (AV-U AV)
    (begin
      (mk-AV-NTs! AV)
      (AV-U AV))))

(define (alloc-NT-field)
  (let* ( [table (make-hash-table)]
          [get-info
            (lambda (nt)
              (hash-table-get table nt (lambda () #f)))]
          [set-info!
            (lambda (nt v)
              (hash-table-put! table nt v))])
    (values get-info set-info!)))

(define nt->sym
  (match-lambda
    [($ NT x type) 
      (symbol-append type ': 
        (if (Tvar? x) (Tvar-name x)
          (symbol-append 'AV ': (AV-num x))))]
    [x `(BAD-NT!!!! ,x)]))

(define (AV->rep AV) (symbol-append 'AV ': (AV-num AV)))

; ======================================================================

(define select-L
  (lambda (nt*)
    (filter-map (match-lambda [($ NT x 'L) x][_ #f]) nt*)))

'(define select-LI
  (lambda (nt*)
    (filter-map (match-lambda [($ NT x 'LI) x][_ #f]) nt*)))

(define select-U
  (lambda (nt*)
    (filter-map (match-lambda [($ NT x 'U) x][_ #f]) nt*)))

'(define select-UI
  (lambda (nt*)
    (filter-map (match-lambda [($ NT x 'UI) x][_ #f]) nt*)))

;; ======================================================================
