;; gram.ss

; ======================================================================

(define debugging-gram #f)
(defmacro pretty-debug-gram args `(when debugging-gram (pretty-print ,@args)))

; ======================================================================
; VIEWING CONSTRAINTS AS GRAMMARS
; ======================================================================
; Non-Terminals

(define-structure (NT AVS type) 
  ([rhs* '()][prop #f][tag #f][sources #f][num #f][edgefrom '()]))

(define mk-AVS-NTs!
  (lambda (AVS)
    (set-AVS-L!  AVS (make-NT AVS 'L ))
    (set-AVS-U!  AVS (make-NT AVS 'U ))
    (set-AVS-LI! AVS (make-NT AVS 'LI))
    (set-AVS-UI! AVS (make-NT AVS 'UI))))

(define same-nt-type?
  (match-lambda*
   [(($ NT _ t1) ($ NT _ t2)) (eq? t1 t2)]))

(define nt-chg-AVS
  (match-lambda*
   [(f ($ NT x type)) (make-NT (f x) type)]))

(define drop-I
  (match-lambda
   [(and nt ($ NT AVS type _))
    (case type
      [(or 'L 'U) nt]
      [LI (AVS-L AVS)]
      [UI (AVS-U AVS)])]))

(define invert-nt
  (match-lambda
   [($ NT AVS 'L)  (AVS-UI AVS)]
   [($ NT AVS 'LI) (AVS-U  AVS)]
   [($ NT AVS 'U)  (AVS-LI AVS)]
   [($ NT AVS 'UI) (AVS-L  AVS)]))

; ======================================================================
; Right hand side of a production

(define-structure (rhs* grsym misc nts))
(define (make-rhs grsym nt) (make-rhs* grsym '() (list nt)))

(define-structure (grsym ineq fn sign template field-no))
;; ineq is '<= or '>=
;; fn   is 'inj, 'inj-tst or 'ext
;; sign is #t (monotonic) or #f (antimonotonic)
;; field-no may be #f

(define (make-grsym->=inj+     t f) (make-grsym '>= 'inj     #t t f))
(define (make-grsym->=inj-     t f) (make-grsym '>= 'inj     #f t f))
(define (make-grsym->=inj?     t f) (make-grsym '>= 'inj     '? t f))

(define (make-grsym-<=inj-tst+ t f) (make-grsym '<= 'inj-tst #t t f))
(define (make-grsym-<=inj-tst- t f) (make-grsym '<= 'inj-tst #f t f))

(define (make-grsym-<=ext+     t f) (make-grsym '<= 'ext     #t t f))
(define (make-grsym->=ext-     t f) (make-grsym '>= 'ext     #f t f))

(define (make-grsym->=ext+     t f) (make-grsym '>= 'ext     #t t f))
(define (make-grsym-<=ext-     t f) (make-grsym '<= 'ext     #f t f))

;;(define (make-grsym-filter  filter) (make-grsym '<= 'filter #t filter #f))
(define (make-grsym-filter  filter) (make-grsym '>= 'filter #t filter #f))
(define grsym-crossover             '>=crossover)
   
;; a grsym can also be '>=epsilon or '<=epsilon

(define grsym-eq?
  (match-lambda*
   [(($ grsym i1 f1 s1 t1 n1) ($ grsym i2 f2 s2 t2 n2))
    (and (eq? i1 i2) (eq? f1 f2) (eq? t1 t2) (eqv? n1 n2))]
   [(x y) (eq? x y)]))

(define invert-grsym
  (match-lambda
   [($ grsym '>= 'inj sign template field-no)
    (make-grsym '<= 'ext sign template field-no)]
   [($ grsym '<= 'inj-tst sign template field-no)
    (make-grsym '>= 'ext sign template field-no)]
   [($ grsym '>= 'filter #t filter #f)
    (make-grsym '<= 'unfilter #t filter #f)]
   ['>=crossover '<=crossover]
   [(and g ($ grsym))
    (error 'invert-grsym "Bad grsym ~s" (grsym->rep g))]))

(define grsym-epsilon-or-filter?
  (match-lambda
   [(? symbol?) #t]
   [($ grsym _ 'filter) #t]
   [_ #f]))

; ======================================================================
; Parameters for creating grammar

(define-structure 
  (parameters-grammar 
   interp-sign                          ; applied to sign of each field
   prims                                ; #t => treat prims as constants
   filters                              ; #t => filters are not epsilon edges
   assignable-fields                    ; #t => assignable fields in grammar
   structure-opaque                     ; #t => structures are constants
   ))

; ======================================================================
;; Prepare for creating grammar

(define (prep-gram!)
  (for-each mk-AVS-NTs! list-AVS))

; ======================================================================
;; Initializing the NT fields of an AVS

(define (add-rhs! NT rhs)
  (pretty-debug-gram
    (match rhs
      [($ rhs* grsym _ nts)
       (assert (list? nts))
       (list `(add-rhs! ,(nt->sym NT) ,(grsym->rep grsym) 
                        ,(map nt->sym nts)))]))
 (set-NT-rhs*! NT (cons rhs (NT-rhs* NT))))

; ----------------------------------------------------------------------

(define (for-each-calc-prods-L L paras fn)
  (match L
    [($ NT AVS 'L)
     (for-each
      (match-lambda
       [($ AV _ (and template ($ template _ signs _ _ _ _ structure?))
           misc fields)
        (if (or (zero? (vector-length fields))
                (and structure?
                     (parameters-grammar-structure-opaque paras)))
            (fn (make-rhs* (make-grsym '>= 'inj #t template '?) misc '()))
            (for i 0 (vector-length fields)
                 (let ([field (vector-ref fields i)]
                       [interp-sign (parameters-grammar-interp-sign paras)]
                       [sign (interp-sign (vector-ref signs i))])
                   (fn (make-rhs*
                        (make-grsym '>= 'inj (eq? sign '>=inj+) template i)
                        misc
                        (case sign
                          [>=inj+ (list (AVS-L field))]
                          [>=inj-
                           (if (or #f ;; mk-assignable-part-fields?
                                   (eq? template template-lam))
                               (map AVS-U (AVS-transitive-edgeto field))
                               '())]))))))])
      (get-AVS-objs AVS))]))

; ----------------------------------------------------------------------

(define (for-each-calc-prods-U U paras fn)
  (match U
    [($ NT AVS 'U)
     (let ([interp-sign (parameters-grammar-interp-sign paras)])
       (for-each
        (match-lambda
         [($ con _ (and template ($ template type signs)) field-no AVS2 misc)
          (case (interpret-sign (vector-ref signs field-no))
            [>=inj+
             (fn (make-rhs*
                  (make-grsym-<=inj-tst+ template field-no)
                  misc
                  (list (AVS-U AVS2))))]
            [>=inj-
             (fn (make-rhs*
                  (make-grsym-<=inj-tst- template field-no)
                  misc
                  (list (AVS-U AVS2))))])]
         [($ con-filter _ the-filter AVS2)
          (fn (make-rhs*
               (make-grsym-filter the-filter)
               '()
               (list (AVS-U AVS2))))])
        (AVS-constraints AVS)))
     (for-each
      (lambda (AVS2) (fn (make-rhs* '<=epsilon '() (list (AVS-U AVS2)))))
      (AVS-transitive-edgeto AVS))]))

; ----------------------------------------------------------------------

(define (for-each-calc-prods nt paras fn)
  (match nt
    [($ NT AVS 'L) (for-each-calc-prods-L nt paras fn)]
    [($ NT AVS 'U) (for-each-calc-prods-U nt paras fn)]))

; ======================================================================






(define (calc-productions! 
         live-nts live-nt?
         mk-L? mk-U? mk-LI? mk-UI?
         L->LI
         mk-assignable-part-fields?
         treat-all-mono)

  ;; live-AVS, crossover-AVS, live-nt and live-nt? 
  ;;    all come from calc-live-AVS-nt
  ;; mk-...     : bool -- controls which NTs to produce
  ;; L->LI, ... : bool -- controls L->LI production rule

  (let ([interpret-sign
         (if treat-all-mono
             (lambda (x) '>=inj+)
             (lambda (x) x))])

    (for-each (lambda (nt) (set-NT-rhs*! nt '()))
              live-nts)

    ;; ------ Now fill NTs from AV, constraints and edges

    (for-each
     (lambda (AVS)
       (pretty-debug-gram `(Prods for AVS ,(name-for-AVS AVS)))

       ;; ------ Invert AV
       (when (and mk-L? (live-nt? (AVS-L AVS)))
         (AVs->prods AVS interpret-sign live-nt?))

       ;; ------ Invert constraints
       (when (and mk-U? (live-nt? (AVS-U AVS)))
         (for-each
          (match-lambda
           [($ con _ (and template ($ template type signs)) field-no AVS2 misc)
            (case (interpret-sign (vector-ref signs field-no))
              [>=inj+
               (add-rhs! (AVS-U AVS)
                         (make-rhs*
                          (make-grsym-<=inj-tst+ template field-no)
                          misc
                          (filter live-nt? 
                                  (map AVS-U (AVS-transitive-edgeto AVS2)))))]
              [>=inj-
               (add-rhs! (AVS-U AVS)
                         (make-rhs*
                          (make-grsym-<=inj-tst- template field-no)
                          misc
                          (filter live-nt? (list (AVS-L AVS2)))))])]
           [($ con-filter _ the-filter AVS2)
            (add-rhs! (AVS-U AVS)
                      (make-rhs*
                       (make-grsym-filter the-filter)
                       '()
                       (filter live-nt? 
                               (map AVS-U (AVS-transitive-edgeto AVS2)))))
            '(for-each
              (lambda (to)
                (when (live-nt? (AVS-L to))
                  (add-rhs! (AVS-L to)
                            (make-rhs*
                             (make-grsym-filter the-filter)
                             '()
                             (list (AVS-L AVS))))))
              (AVS-transitive-edgeto AVS2))
            ])
          (AVS-constraints AVS)))
       )
     live-AVS)

    ;; ------ Add L->LI productions
    (when L->LI
      (for-each
       (lambda (AVS)
         (for-each 
          (lambda (to)
            (when (and (NT? (AVS-L to)) (live-nt? (AVS-L to)))
              (add-rhs! (AVS-L to)
                        (make-rhs* grsym-crossover '() (list (AVS-LI AVS))))))
          (AVS-transitive-edgeto AVS)))
       crossover-AVS))

    ;; ------ convert L, U productions into LI, UI productions
    (pretty-debug '(inverting productions))

    (for-each
     (lambda (nt)
       (for-each
        (match-lambda
         [($ rhs* grsym misc to-nts)
          (unless (eq? grsym '<=crossover)
            (let* ([inv-grsym (invert-grsym grsym)]
                   [inv-nt (invert-nt nt)]
                   [inv-rhs (make-rhs* inv-grsym misc (list inv-nt))])
              (for-each (lambda (to) (add-rhs! (invert-nt to) inv-rhs))
                        to-nts)))])
        (NT-rhs* nt)))
     live-nts)

    (let ([live-nts
           (apply append
                  (map
                   (match-lambda
                    [($ NT AVS 'L) (list (AVS-L AVS) (AVS-UI AVS))]
                    [($ NT AVS 'U) (list (AVS-U AVS) (AVS-LI AVS))])
                   live-nts))])

      ;; Now group together all productions for given NT with given grsym
      (for-each
       (lambda (nt)
         (let ([rhs* (NT-rhs* nt)])
           (set-NT-rhs*! nt '())
           (for-each (lambda (rhs) 
                       (assert (list? (rhs*-nts rhs)))
                       (add-rhs! nt rhs))
                     (group-rhs* rhs*))))
       live-nts)

      (list live-nts))))
