;; min-test.ss

(define debug-nfa #t)
(define pretty-debug-nfa (lambda (x) (when debug-nfa (pretty-print x))))

(define debug-min #f)
(define pretty-debug-min (lambda (x) (when debug-nfa (pretty-print x))))

; ======================================================================
; VIEWING CONSTRAINTS AS GRAMMARS
; ======================================================================
; Non-Terminalss

; x is an AVS
(define-structure (L  x))
(define-structure (LI x))
(define-structure (U  x))
(define-structure (UI x))

(define eq-nt?
  (match-lambda*
   [(($ L x1)  ($ L x2))  (eq? x1 x2)]
   [(($ LI x1) ($ LI x2)) (eq? x1 x2)]
   [(($ U x1)  ($ U x2))  (eq? x1 x2)]
   [(($ UI x1) ($ UI x2)) (eq? x1 x2)]
   [_ #f]))

(define nt?
  (match-lambda
   [(or ($ L) ($ U) ($ LI) ($ UI)) #t]
   [_ #f]))

(define same-nt-type?
  (match-lambda*
   [(($ L x1)  ($ L x2))  #t]
   [(($ LI x1) ($ LI x2)) #t]
   [(($ U x1)  ($ U x2))  #t]
   [(($ UI x1) ($ UI x2)) #t]
   [_ #f]))

(define nt->AVS
  (match-lambda
   [($ L x1)  x1]
   [($ LI x1) x1]
   [($ U x1)  x1]
   [($ UI x1) x1]))

(define nt-chg-AVS
  (match-lambda*
   [(f ($ L  x)) (make-L  (f x))]
   [(f ($ LI x)) (make-LI (f x))]
   [(f ($ U  x)) (make-U  (f x))]
   [(f ($ UI x)) (make-UI (f x))]))

(define drop-I
  (match-lambda
   [(and nt (or ($ L _) ($ U _))) nt]
   [($ LI x) (make-L x)]
   [($ UI x) (make-U x)]))

(define mem-nt?
  (lambda (nt l)
    (and (not (null? l))
         (or (eq-nt? nt (car l))
             (mem-nt? nt (cdr l))))))

(define (set-NT-prop! NT val)
  (let* ([AVS (nt->AVS NT)]
         [old (get-AVS-prop AVS 'NT-prop #f)]
         [old (if old old 
                  (let ([v (vector #f #f #f #f)])
                    (add-AVS-prop! AVS 'NT-prop v)
                    v))])
    (vector-set!
     old
     (match NT
       [($ L x1)  0] [($ LI x1) 1] [($ U x1)  2] [($ UI x1) 3])
     val)))

(define (get-NT-prop NT)
  (let* ([AVS (nt->AVS NT)]
         [old (get-AVS-prop AVS 'NT-prop #f)]
         [old (if old old 
                  (let ([v (vector #f #f #f #f)])
                    (add-AVS-prop! AVS 'NT-prop v)
                    v))])
    (vector-ref
     old
     (match NT
       [($ L x1)  0] [($ LI x1) 1] [($ U x1)  2] [($ UI x1) 3]))))

; ======================================================================
; Right hand side of a production

(define-structure (rhs* grsym nts))
(define (make-rhs grsym nt) (make-rhs* grsym (list nt)))

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

;; a grsym can also be '>=epsilon or '<=epsilon

(define grsym-eq?
  (match-lambda*
   [(($ grsym i1 f1 s1 t1 n1) ($ grsym i2 f2 s2 t2 n2))
    (and (eq? i1 i2) (eq? f1 f2) (eq? t1 t2) (eqv? n1 n2))]
   [(x y) (eq? x y)]))

; ======================================================================
;; Initializing the L, ... fields of an AVS

(define (add-AVS-LI! AVS x) (set-AVS-LI! AVS (cons x (AVS-LI AVS))))
(define (add-AVS-UI! AVS x) (set-AVS-UI! AVS (cons x (AVS-UI AVS))))
(define (add-AVS-L!  AVS x) (set-AVS-L!  AVS (cons x (AVS-L  AVS))))
(define (add-AVS-U!  AVS x) (set-AVS-U!  AVS (cons x (AVS-U  AVS))))

(define zeroary-op-AVS (void))

(define (calc-productions! list-AVS use-AVS?
                           tree zeroary
                           epsilon-L epsilon-U epsilon-LI epsilon-UI
                           L->LI U->UI)
  ;; tree       : bool -- controls if produce RTG or regular grammar
  ;; epsilon-...: bool -- control epsilon transitions
  ;; L->LI, ... : bool -- controls L->LI production rule

  (for-each
   (lambda (AVS)
     (set-AVS-L!  AVS '())
     (set-AVS-U!  AVS '())
     (set-AVS-LI! AVS '())
     (set-AVS-UI! AVS '())
     (set-AVS-edgefrom! AVS '()))
   list-AVS)

  ;; ------ Invert edges
  (for-each
   (lambda (AVS)
     (for-each
      (lambda (to) (when (use-AVS? to) (add-AVS-edgefrom! to AVS)))
      (AVS-edgeto AVS)))
   list-AVS)

  ;; ------ Now make L, etc, from AV, constraints and edges
  (for-each
   (lambda (AVS)

     ;; ------ Invert original AV
     (for-each
      (match-lambda
       [($ AV _ (and template ($ template type signs)) misc fields)
        (let ([l (vector-length fields)])
          (cond
           [tree
            ;; Make rhs
            (add-AVS-L! AVS
                        (make-rhs* (make-grsym->=inj? template #f)
                                   (map
                                    (lambda (f sign)
                                      (case sign
                                        [>=inj+ (make-L f)]
                                        [>=inj- (make-U f)]))
                                    (vector->list fields)
                                    (vector->list signs))))]
           ;; o/w not tree
           [(= l 0)
            ;; Terminal
            (add-AVS-L! AVS 
                        (make-rhs* (make-grsym->=inj+ template #f) '()))]
           [else
            ;; Non-terminal - return all prods
            (for i 0 l
                 (let ([f (vector-ref fields i)])
                   (when (use-AVS? f)
                     (case (vector-ref signs i)
                       [>=inj+
                        (add-AVS-L! AVS 
                                    (make-rhs (make-grsym->=inj+ template i) 
                                              (make-L f)))]
                       [>=inj-
                        (add-AVS-L! AVS
                                    (make-rhs (make-grsym->=inj- template i) 
                                              (make-U f)))]))))])

          (if (and zeroary (zero? l))
              ;; Fake a unary op
              (add-AVS-UI! zeroary-op-AVS
                           (make-rhs (make-grsym-<=ext+ template 0)
                                     (make-UI AVS)))
          
              (for i 0 l
                   (let ([f (vector-ref fields i)])
                     (when (use-AVS? f)
                       (case (vector-ref signs i)
                         [>=inj+
                          (add-AVS-UI! 
                           f
                           (make-rhs (make-grsym-<=ext+ template i)
                                     (make-UI AVS)))]
                         [>=inj-
                          (add-AVS-LI!
                           f
                           (make-rhs (make-grsym->=ext- template i) 
                                     (make-UI AVS)))]))))))])
      (AVS-orig-objs AVS))

     ;; ------ Invert constraints
     (for-each
      (match-lambda
       [($ con _ (and template ($ template type signs)) field-no AVS2)
        (when (use-AVS? AVS2)
          (case (vector-ref signs field-no)
            [>=inj+
             (add-AVS-U! AVS
                         (make-rhs (make-grsym-<=inj-tst+ template field-no)
                                   (make-U AVS2)))
             (add-AVS-LI! AVS2
                          (make-rhs (make-grsym->=ext+ template field-no)
                                    (make-LI AVS)))]
            [>=inj-
             (add-AVS-U! AVS
                         (make-rhs (make-grsym-<=inj-tst- template field-no)
                                   (make-L AVS2)))
             (add-AVS-UI! AVS2
                          (make-rhs (make-grsym-<=ext- template field-no)
                                    (make-LI AVS)))]))]
       [($ con-filter _ filter AVS2)
        (printf "Warning: con-filter~n")])
      (AVS-constraints AVS))

     (for-each
      (lambda (to)
        (when epsilon-L  (add-AVS-L!  to  (make-rhs '>=epsilon (make-L  AVS))))
        (case epsilon-LI
          [#t (add-AVS-LI! to  (make-rhs '>=epsilon (make-LI AVS)))]
          ['concrete
           (add-AVS-LI! to  (make-rhs '>=concrete-epsilon (make-LI AVS)))])
        (when epsilon-U  (add-AVS-U!  AVS (make-rhs '<=epsilon (make-U  to))))
        (when epsilon-UI (add-AVS-UI! AVS (make-rhs '<=epsilon (make-UI to)))))
      (AVS-edgeto AVS))

     (when L->LI (add-AVS-L! AVS (make-rhs '>=epsilon (make-LI AVS))))
     (when U->UI (add-AVS-U! AVS (make-rhs '<=epsilon (make-UI AVS))))
     )
   list-AVS))

;; ----------------------------------------------------------------------
;; Restricting the grammer to certain NTs

(define (restrict-nts! list-AVS use-nt?)
  ;;(display `(restrict-nts! ,(map nt->sym list-nt))) (newline)
  (let* ([filter-nt
          (lambda (rhs*)
            (filter-map
             (match-lambda
              [(and rhs ($ rhs* grsym nts))
               (if (or (null? nts) (ormap use-nt? nts))
                   rhs
                   #f)])
             rhs*))])
    (for-each 
     (lambda (AVS)
       (for-each
        (match-lambda
         [(set-AVS-NT! make-NT AVS-NT)
          (set-AVS-NT! AVS (if (use-nt? (make-NT AVS))
                               (filter-nt (AVS-NT  AVS))
                               '()))])
        (list (list set-AVS-L!  make-L  AVS-L)
              (list set-AVS-U!  make-U  AVS-U)
              (list set-AVS-LI! make-LI AVS-LI)
              (list set-AVS-UI! make-UI AVS-UI))))
     list-AVS)))

;; ----------------------------------------------------------------------
;; for-each-prods
;; For this to work, must have AVS-L etc setup.

(define (for-each-prods f nt)
  (match nt
    [($ L  x) (for-each f (AVS-L  x))]
    [($ U  x) (for-each f (AVS-U  x))]
    [($ LI x) (for-each f (AVS-LI x))]
    [($ UI x) (for-each f (AVS-UI x))]))

;; ======================================================================

(define (add-prod nt grsym nt*)
  (assert (andmap nt? nt*))
  (let ([rhs (make-rhs* grsym nt*)])
    (match nt
      [($ L  x) (add-AVS-L!  x rhs)]
      [($ LI x) (add-AVS-LI! x rhs)]
      [($ U  x) (add-AVS-U!  x rhs)]
      [($ UI x) (add-AVS-UI! x rhs)])))

;; ----------------------------------------------------------------------
;; Assumes grammar is result of N->D
;; So is a D-RTG, and only has productions on L,U
;; not on LI,UI

(define (convert-productions-to-AV-etc! list-AVS)
  ;;(pretty-print `(cvt-prods ,(map name-for-AVS list-AVS)))
  (let* ([AVS-empty (mk-AVS-tmp 'empty)]
         [add-AV-field!
          (lambda (AVS template field-no AVS2)
            (or
             ;; Is the zeroary-op-AVS field
             (>= field-no (vector-length (template-signs template)))
             ;; Reuse old AV, if possible
             (ormap
              (match-lambda
               [($ AV _ (? (lambda (t) (eq? t template))) _ fields)
                (cond
                 [(eq? (vector-ref fields field-no) AVS-empty)
                  ;;; empty, fill it
                  (vector-set! fields field-no AVS2)
                  #t]
                 [(eq? (vector-ref fields field-no) AVS2)
                  ;; Already in there
                  #t]
                 [else;; this won't work
                  #f])]
               [_ #f])
              (get-AVS-objs AVS))
             ;; No AV in there, create and add one
             (let* ([l (vector-length (template-signs template))]
                    [fields (make-vector l AVS-empty)])
               (vector-set! fields field-no AVS2)
               (add-nohash-orig-AV! 
                AVS (create-AV-nolist template '() fields)))))])

    ;; Examine each production rule, and convert it.
    (for-each
     (lambda (AVS)
       ;;(pretty-print `(convert-prods AVS ,(AVS-num AVS)))
       ;; L-AVS
       (for-each
        (match-lambda
         [($ rhs* (or '>=epsilon '>=concrete-epsilon) ((or ($ L x) ($ LI x))))
          (unless (eq? AVS x) (add-edge! x AVS))]
         [($ rhs* ($ grsym '>= 'inj sign template field-no) ())
          (add-nohash-orig-AV! AVS (create-AV-nolist template '() (vector)))]
         [($ rhs* ($ grsym '>= 'inj sign template #f)
             ((or ($ L x*) ($ U x*)) ...))
          (map-with-n (lambda (x n) (add-AV-field! AVS template n x))
                      x*)]
         [($ rhs* ($ grsym '>= 'inj sign template field-no)
             ((or ($ L x) ($ U x))))
          (add-AV-field! AVS template field-no x)]

         [($ rhs* ($ grsym '>= ext #t template field-no) 
             ((or ($ LI x) ($ L x))))
          (add-con! x (create-con template field-no AVS))]
         [($ rhs* ($ grsym '>= ext #f template field-no)  
             ((or ($ UI x) ($ U x))))
          (add-AV-field! x template field-no AVS)]

         [($ rhs* grsym nts)
          (error 'prod-to-AV "Bad L/LI rhs ~s ~s" 
                 (grsym->rep grsym) (map nt->sym nts))]
         [rhs (error 'prod-to-AV "Bad L/LI rhs ~s" rhs)])
        (append (AVS-L AVS) (AVS-LI AVS)))

       ;; U-AVS
       (for-each
        (match-lambda
         [($ rhs* '<=epsilon ((or ($ U x) ($ UI x))))
          (unless (eq? AVS x) (add-edge! AVS x))]
         [($ rhs* ($ grsym '<= 'inj-tst #f template field-no) (($ L x)))
          (add-con! AVS (create-con template field-no x))]
         [($ rhs* ($ grsym '<= 'inj-tst #t template field-no) (($ U x)))
          (add-con! AVS (create-con template field-no x))]

         [($ rhs* ($ grsym '<= 'ext #f template field-no) 
             ((or ($ L x) ($ LI x))))
          (add-con! x (create-con template field-no AVS))]
         [($ rhs* ($ grsym '<= 'ext #t template field-no) 
             ((or ($ U x) ($ UI x))))
          (add-AV-field! x template field-no AVS)]

         [($ rhs* grsym nts)
          (error 'prod-to-AV "Bad U/UI rhs ~s ~s" 
                 (grsym->rep grsym) (map nt->sym nts))]
         [rhs (error 'prod-to-AV "Bad U/UI rhs ~s" rhs)])
        (append (AVS-U AVS) (AVS-UI AVS))))
       
     list-AVS)

    ;; Now prop AVs
    (for-each
     (lambda (AVS)
       (for-each (lambda (AV) (prop-AV! AVS AV))
                 (AVS-orig-objs AVS)))
     list-AVS)

    ))

; ======================================================================
; COMPRESSING GRAMMARS
; ======================================================================

(define (epsilon-close-nt nt)
  (epsilon-close-nts (list nt)))

(define (epsilon-close-nts nts)
  (let ([done '()])
    (letrec ([traverse 
              (lambda (nt)
                (unless (mem-nt? nt done)
                  (set! done (cons nt done))
                  (for-each-prods
                   (match-lambda
                    [($ rhs* (or '>=epsilon '<=epsilon) (nt2))
                     (traverse nt2)]
                    [_ (void)])
                   nt)))])
      (for-each traverse nts)
      done)))

; --------------------

(define for-each-prods-joined
  (lambda (fn nt*)
    (let* ([rhs* '()])
      (for-each
       (lambda (nt)
         (for-each-prods (lambda (rhs) (set! rhs* (cons rhs rhs*)))
                         nt))
       nt*)
      (recur loop ([rhs* rhs*])
        (match rhs*
          [() (void)]
          [(($ rhs* grsym _) . _)
           (match-let
               ([(nt** . rest)
                 (filter-map-split
                  (match-lambda
                   [($ rhs* grsym2 nt*)
                    (if (grsym-eq? grsym grsym2) nt* #f)])
                  rhs*)])
             (fn grsym nt**)
             (loop rest))])))))

; --------------------

(define (grammar-calc-reached roots)
  (let ([reached '()]
        [tag (gensym)])
    (letrec 
        ([traverse
          (lambda (nt)
            (unless (eq? (get-NT-prop nt) tag)
              (set-NT-prop! nt tag)
              (set! reached (cons nt reached))
              (for-each-prods
               (match-lambda
                [($ rhs* _ nt*) 
                 (for-each traverse nt*)])
               nt)))])
      (for-each traverse roots)
      (list reached (lambda (nt) (eq? (get-NT-prop nt) tag))))))

; --------------------

(define (grammar-calc-nonempty num-NT NT->num num->NT final)
  (let* ([table (make-vector num-NT '())])
    ;; entry is #t if nonempty, 
    ;; or else list of NT to make nonempty if proved nonempty
    (letrec ([set-nonempty! 
              (lambda (n)
                '(pretty-print `(set-nonempty! ,(nt->sym (num->NT n))))
                (let ([l  (vector-ref table n)])
                  (vector-set! table n #t)
                  (when (list? l)
                    (for-each set-nonempty! l))))]
             [add-depends
              (lambda (i j)
                '(pretty-print `(add-depends ,(nt->sym (num->NT i))
                                            ,(nt->sym (num->NT j))))
                (if (list? (vector-ref table j))
                    (vector-set! table j (cons i (vector-ref table j)))
                    (set-nonempty! i)))])

      ;; fill out table
      (for i 0 num-NT
           (let ([nt (num->NT i)])
             ;;(pretty-print `(i ,i nt ,(nt->sym nt) ,(mem-nt? nt final)))
             (if (mem-nt? nt final)
                 (set-nonempty! i)
                 ;; Look at prods
                 (for-each-prods 
                  (match-lambda
                   [($ rhs* grsym nt*)
                    ;;(pretty-print `(-> ,(grsym->rep grsym) ,(nt->sym nt)))
                    (if (null? nt*)
                        (set-nonempty! i)
                        (for-each (lambda (nt) (add-depends i (NT->num nt)))
                                  nt*))])
                  nt))))

      ;; return list of nonempty nts, plus membership predicate
      ;; 
      (list (filter-map
             (lambda (x) x)
             (map-with-n
              (lambda (entry n)
                (if (eq? entry #t)
                    (num->NT n)
                    #f))
              (vector->list table)))
            ;; nonempty-nt?
            (lambda (nt) (if (and (NT->num nt)
                                  (eq? (vector-ref table (NT->num nt)) #t))
                             #t #f))
            ))))

; ======================================================================
; COMPRESSING CONSTRAINTS
; ======================================================================

(define select-L
  (lambda (nt*)
    (filter-map (match-lambda [($ L x) x][_ #f]) nt*)))

(define select-LI
  (lambda (nt*)
    (filter-map (match-lambda [($ LI x) x][_ #f]) nt*)))

(define select-U
  (lambda (nt*)
    (filter-map (match-lambda [($ U x) x][_ #f]) nt*)))

(define select-UI
  (lambda (nt*)
    (filter-map (match-lambda [($ UI x) x][_ #f]) nt*)))

; --------------------

(define (epsilon-close-forwards AVS)
  (let ([done '()])
    (recur traverse ([AVS AVS])
      (unless (memq AVS done)
        (set! done (cons AVS done))
        (for-each traverse (AVS-edgeto AVS))))
    done))

(define (epsilon-close-backwards AVS)
  (let ([done '()])
    (recur traverse ([AVS AVS])
      (unless (memq AVS done)
        (set! done (cons AVS done))
        (for-each traverse (AVS-edgefrom AVS))))
    done))

; ======================================================================
; Could use list-AVS to be more efficient here!

(define calc-NT<->num
  (lambda (list-nt)
    (let* ([num-NT 0]
           [num->NT (make-vector (* 4 (length list-nt)) #f)]
           [AVS->L-NT  (make-vector num-AVS #f)]
           [AVS->LI-NT (make-vector num-AVS #f)]
           [AVS->U-NT  (make-vector num-AVS #f)]
           [AVS->UI-NT (make-vector num-AVS #f)]
           [add-NT! (lambda (NT)
                      (vector-set! num->NT num-NT NT)
                      (begin0
                       num-NT
                       (set! num-NT (add1 num-NT))))])
      (for-each
       (lambda (nt)
         (match nt
           [($ L  AVS) (vector-set! AVS->L-NT   (AVS-num AVS) (add-NT! nt))]
           [($ LI AVS) (vector-set! AVS->LI-NT  (AVS-num AVS) (add-NT! nt))]
           [($ U  AVS) (vector-set! AVS->U-NT   (AVS-num AVS) (add-NT! nt))]
           [($ UI AVS) (vector-set! AVS->UI-NT  (AVS-num AVS) (add-NT! nt))]))
       list-nt)
      (list num-NT
            (match-lambda 
             [($ L  x) (vector-ref AVS->L-NT  (AVS-num x))]
             [($ LI x) (vector-ref AVS->LI-NT (AVS-num x))]
             [($ U  x) (vector-ref AVS->U-NT  (AVS-num x))]
             [($ UI x) (vector-ref AVS->UI-NT (AVS-num x))])
            (lambda (num) (vector-ref num->NT num))))))

; ----------------------------------------------------------------------

(define leq->equiv
  (lambda (list-e e->num e-leq?)
    ;; Takes a list of elements in list-e, 
    ;; with e-leq? a partial order
    ;; Calculates an equivalence relation
    (match-let*
        ([e-eq? (lambda (i j) 
                   (let ([r (and (e-leq? i j) (e-leq? j i))])
                     '(when r (printf "~s=~s~n" (nt->sym i) (nt->sym j)))
                     r))]
         [AVS-leq? (lambda (x y)
                     (and (NT-leq? (make-L  x) (make-L  y))
                          (NT-leq? (make-LI x) (make-LI y))
                          (NT-leq? (make-U  y) (make-U  x))
                          (NT-leq? (make-UI y) (make-UI x))))]
         [AVS->rep-AVS (make-vector num-AVS #f)]
         [list-rep-AVS '()]
         [old-AVS->AVS (lambda (old-AVS)
                         (let ([AVS (vector-ref AVS->rep-AVS 
                                                (AVS-num old-AVS))])
                           AVS))]
         [old-nt->nt (lambda (nt) (nt-chg-AVS old-AVS->AVS nt))]
         [AV-leq?
          (match-lambda*
           [(($ AV _ template1 misc1 fields1) 
             ($ AV _ template2 misc2 fields2)
             AVS-leq?)
            (and (eq? template1 template2)
                 (eq? misc1 misc2)
                 (andmap2 AVS-leq?
                          (vector->list fields1) 
                          (vector->list fields2)))])])

      ;; Have equivalence relation in AVS-eq?
      ;; want to create mapping AVS -> new AVS
      (printf "Calculating AVS -> new AVS~n")
      (for-each
       (lambda (AVS)
         (ormap
          (lambda (rep)
            (cond
             [(eq? AVS rep)
              ;; This AVS not included in any representative AVS
              (let ([rep-AVS (mk-AVS-nolist 'eqvcl)])
                (vector-set! AVS->rep-AVS (AVS-num AVS) rep-AVS)
                (set! list-rep-AVS (cons (cons AVS rep-AVS) list-rep-AVS))
                #t)]
             [(AVS-eq? AVS rep)
              ;; This NT is equivalent to rep
              (let ([rep-AVS (vector-ref AVS->rep-AVS (AVS-num rep))])
                                        ;(assert (eq? rep ))
                (vector-set! AVS->rep-AVS (AVS-num AVS) rep-AVS)
                (set! list-rep-AVS (cons (cons AVS rep-AVS) list-rep-AVS)))
              #t]
             [else;; continue
              #f]))
          list-AVS))
       list-AVS)

      '(begin
         (display 
          `(AVS->rep-AVS 
            ,(map (lambda (AVS) 
                    (cons (AVS-num AVS)
                          (AVS-num (vector-ref AVS->rep-AVS (AVS-num AVS))))) 
                  list-AVS)))
         (newline))
      (printf "Num rep AVS=~s~n" (length list-rep-AVS))

      ;; Now fill in the AV's and constraints in the representative AVS
      ;; Maybe want to remove duplicate constraints
      ;; Not removing duplicate AV's from different AVSs in same rep-AVS

      (printf "Copying grammar~n")

      (for-each
       (match-lambda
        [(old-AVS . rep-AVS)
         (for-each
          (match-lambda
           [(get-nt set-nt!)
            (for-each
             (match-lambda
              [($ rhs* grsym nt*)
               (or 
                ;; Check if already there
                (ormap (match-lambda
                        [($ rhs*
                            (? (lambda (grsym2) (grsym-eq? grsym grsym2)))
                            (? (lambda (nt2*) (andmap2 eq-nt? nt* nt2*))))
                         #t]
                        [_ #f])
                       (get-nt rep-AVS))
                ;; Add it
                (set-nt! rep-AVS
                         (cons (make-rhs* 
                                grsym 
                                (map old-nt->nt nt*))
                               (get-nt rep-AVS))))])
             (get-nt old-AVS))])
          (list (list AVS-L  set-AVS-L!)
                (list AVS-LI set-AVS-LI!)
                (list AVS-U  set-AVS-U!)
                (list AVS-UI set-AVS-UI!)))])
       list-rep-AVS)

      ;; Done, return mapping
      old-nt->nt)))

; ----------------------------------------------------------------------

(define apply-equivalence-relation
  (lambda (list-nt num-NT NT->num num->NT NT-leq?)
    ;; NT-leq? is ordering on NTs
    (match-let*
        ([list-AVS (list->set (map nt->AVS list-nt))]
         [NT-eq? (lambda (i j) 
                   (let ([r (and (NT-leq? i j) (NT-leq? j i))])
                     '(when r (printf "~s=~s~n" (nt->sym i) (nt->sym j)))
                     r))]
         [AVS-eq? (lambda (x y)
                    (let ([r
                           (and (NT-eq? (make-L  x) (make-L  y))
                                (NT-eq? (make-LI x) (make-LI y))
                                (NT-eq? (make-U  x) (make-U  y))
                                (NT-eq? (make-UI x) (make-UI y)))])
                      '(when r (printf "~s=~s~n" 
                                       (name-for-AVS x) (name-for-AVS y)))
                      r))]
         [AVS-leq? (lambda (x y)
                     (and (NT-leq? (make-L  x) (make-L  y))
                          (NT-leq? (make-LI x) (make-LI y))
                          (NT-leq? (make-U  y) (make-U  x))
                          (NT-leq? (make-UI y) (make-UI x))))]
         [AVS->rep-AVS (make-vector num-AVS #f)]
         [list-rep-AVS '()]
         [old-AVS->AVS (lambda (old-AVS)
                         (let ([AVS (vector-ref AVS->rep-AVS 
                                                (AVS-num old-AVS))])
                           AVS))]
         [old-nt->nt (lambda (nt) (nt-chg-AVS old-AVS->AVS nt))]
         [AV-leq?
          (match-lambda*
           [(($ AV _ template1 misc1 fields1) 
             ($ AV _ template2 misc2 fields2)
             AVS-leq?)
            (and (eq? template1 template2)
                 (eq? misc1 misc2)
                 (andmap2 AVS-leq?
                          (vector->list fields1) 
                          (vector->list fields2)))])])

      ;; Have equivalence relation in AVS-eq?
      ;; want to create mapping AVS -> new AVS
      (printf "Calculating AVS -> new AVS~n")
      (for-each
       (lambda (AVS)
         (ormap
          (lambda (rep)
            (cond
             [(eq? AVS rep)
              ;; This AVS not included in any representative AVS
              (let ([rep-AVS (mk-AVS-nolist 'eqvcl)])
                (vector-set! AVS->rep-AVS (AVS-num AVS) rep-AVS)
                (set! list-rep-AVS (cons (cons AVS rep-AVS) list-rep-AVS))
                #t)]
             [(AVS-eq? AVS rep)
              ;; This NT is equivalent to rep
              (let ([rep-AVS (vector-ref AVS->rep-AVS (AVS-num rep))])
                                        ;(assert (eq? rep ))
                (vector-set! AVS->rep-AVS (AVS-num AVS) rep-AVS)
                (set! list-rep-AVS (cons (cons AVS rep-AVS) list-rep-AVS)))
              #t]
             [else;; continue
              #f]))
          list-AVS))
       list-AVS)

      '(begin
         (display 
          `(AVS->rep-AVS 
            ,(map (lambda (AVS) 
                    (cons (AVS-num AVS)
                          (AVS-num (vector-ref AVS->rep-AVS (AVS-num AVS))))) 
                  list-AVS)))
         (newline))
      (printf "Num rep AVS=~s~n" (length list-rep-AVS))

      ;; Now fill in the AV's and constraints in the representative AVS
      ;; Maybe want to remove duplicate constraints
      ;; Not removing duplicate AV's from different AVSs in same rep-AVS

      (printf "Copying grammar~n")

      (for-each
       (match-lambda
        [(old-AVS . rep-AVS)
         (for-each
          (match-lambda
           [(get-nt set-nt!)
            (for-each
             (match-lambda
              [($ rhs* grsym nt*)
               (or 
                ;; Check if already there
                (ormap (match-lambda
                        [($ rhs*
                            (? (lambda (grsym2) (grsym-eq? grsym grsym2)))
                            (? (lambda (nt2*) (andmap2 eq-nt? nt* nt2*))))
                         #t]
                        [_ #f])
                       (get-nt rep-AVS))
                ;; Add it
                (set-nt! rep-AVS
                         (cons (make-rhs* 
                                grsym 
                                (map old-nt->nt nt*))
                               (get-nt rep-AVS))))])
             (get-nt old-AVS))])
          (list (list AVS-L  set-AVS-L!)
                (list AVS-LI set-AVS-LI!)
                (list AVS-U  set-AVS-U!)
                (list AVS-UI set-AVS-UI!)))])
       list-rep-AVS)

      ;; Done, return mapping
      old-nt->nt)))

; ----------------------------------------------------------------------

(define (make-minimization-algorithm table-builder-helper)
  (lambda (list-nt roots final)
    (match-let*
        ([(num-NT NT->num num->NT) (calc-NT<->num list-nt)]
         [table-leq (make-vector (* num-NT num-NT) (lambda () (void)))]
         ;; table-leq[x][y] = #f => not(x<=y)
         ;; otherwise a thunk to perform if not(x<=y)
         [NT->rep-NT (make-vector num-NT)]
         [list-rep-NT '()])
      (letrec
          ([lookup (lambda (x y) (vector-ref table-leq (+ (* num-NT x) y)))]
           [set-table-leq!
            (lambda (x y v) (vector-set! table-leq (+ (* num-NT x) y) v))]

           [record-not-leq
            (lambda (x y)
              '(pretty-print 
                `(record-not-leq ,(nt->sym (num->NT x))
                                 ,(nt->sym (num->NT y))))
              (when (not (= x y))
                (match (lookup x y)
                  [#f (void)]
                  [thunk
                   (set-table-leq! x y #f)
                   (thunk)])))]

           [record-not-leq-action
            (lambda (d-p d-q thunk)
              '(pretty-print 
                `(record-not-leq-action ,(nt->sym (num->NT d-p))
                                        ,(nt->sym (num->NT d-q))))
              (unless (eq? d-p d-q)
                (match (lookup d-p d-q)
                  [#f (thunk)]
                  [entry (set-table-leq! d-p d-q
                                         (lambda () (thunk) (entry)))])))]

           [epsilon-close (make-vector num-NT #f)]
           [nt->grsym->fields (make-vector num-NT #f)])

        (printf "Calculating epsilon-close~n")
        (for i 0 num-NT
             (vector-set! epsilon-close i 
                          (epsilon-close-nt (num->NT i))))

        (printf "Calculating (x,c) -> { z* | x ->* y, y->c(z*) }~n")
        (for i 0 num-NT
             (let* ([alist '()])
               (for-each-prods-joined
                (lambda (grsym nt**)
                  '(printf "nt ~s grsym ~a nt** ~s~n"
                          (nt->sym (num->NT i))
                          (grsym->rep grsym)
                          nt**)
                  (set! alist (cons (cons grsym nt**) alist)))
                (epsilon-close-nt (num->NT i)))
               (vector-set! nt->grsym->fields i alist)))

        (printf "Marking final NT's as distinct~n")
        (for-each
         (lambda (f)
           (let ([n-f (NT->num f)])
             (when n-f
               (for i 0 num-NT
                    (unless (= n-f i)
                      (record-not-leq n-f i)
                      (record-not-leq i n-f))))))
         final)
                     
        (printf "Filling not-leq table, num-NT=~s~n" num-NT)
        ;; Fill out table-leq
        (for i 0 num-NT
             ;;(printf "i ~s ~s~n" i (nt->sym (num->NT i)))
             (when (zero? (modulo i 25)) (printf ".") (flush-output))
             (for-each-prods
              (match-lambda
               [($ rhs* (or '>=epsilon '<=epsilon) (nt))
                (for j 0 num-NT
                     (record-not-leq-action 
                      (NT->num nt) j
                      (lambda () (record-not-leq i j))))]
               [($ rhs* grsym nt*)
                (for j 0 num-NT
                     '(printf "i ~s j ~s grsym ~s nt ~s ~s ~n"
                              (nt->sym (num->NT i))
                              (nt->sym (num->NT j))
                              grsym
                              (map nt->sym nt*)
                              (map NT->num nt*))
                     (unless (= i j)
                       (let* ([nt** (or (ormap
                                         (match-lambda
                                          [(grsym2 . nt**)
                                           (if (grsym-eq? grsym grsym2)
                                               nt**
                                               #f)])
                                         (vector-ref nt->grsym->fields j))
                                        '())]
                              [n (length nt**)])
                         '(printf "i ~s grsym ~a j ~s nt** ~s~n"
                                 (nt->sym (num->NT i))
                                 (grsym->rep grsym)
                                 (nt->sym (num->NT j))
                                 nt**)
                         (cond
                          [(zero? n) (record-not-leq i j)]
                          [(null? nt*) ; is a terminal
                           (void)]
                          [else
                           (table-builder-helper
                            i j nt* nt** n NT->num
                            record-not-leq record-not-leq-action)]))))])
              (num->NT i)))
        (newline)

        '(begin
           (display (map (lambda (x) (if x #t #f)) (vector->list table-leq)))
           (newline)
           (for-each 
            (lambda (nt1)
              (for-each
               (lambda (nt2)
                 (when (and (not (eq? nt1 nt2))
                            (same-nt-type? nt1 nt2)
                            (lookup (NT->num nt1) (NT->num nt2)))
                   (printf "~s <= ~s~n" (nt->sym nt1) (nt->sym nt2))))
               list-nt))
            list-nt)
           (newline))
      
        ;; Apply the equivalence relation
        (printf "Applying the eqv rel~n")
        (apply-equivalence-relation 
         list-nt
         num-NT NT->num num->NT
         (lambda (i j) 
           (let ([num-i (NT->num i)]
                 [num-j (NT->num j)])
             (cond
              [(and (not num-i) (not num-j)) #t]
              [(or  (not num-i) (not num-j)) #f]
              [else (lookup (NT->num i) (NT->num j))]))))))))

; ----------------------------------------------------------------------

(define (DFA-min list-nt roots final)
  (printf "Calculating min DFA~n")
  ((make-minimization-algorithm
    (lambda (i j nt* nt** n NT->num record-not-leq record-not-leq-action)
      ;; i -> c(nt*)
      ;; j -> c(nt*) forall nt* in nt**
      (for-each
       (lambda (nt*2)
         (for-each
          (lambda (nt nt2)
            (record-not-leq-action (NT->num nt) (NT->num nt2)
                                   (lambda () (record-not-leq i j))))
          nt* nt*2))
       nt**)))
   list-nt roots final))

(define (NFA-min list-nt roots final)
  (printf "Calculating min NFA~n")
  ((make-minimization-algorithm
    (lambda (i j nt* nt** n NT->num record-not-leq record-not-leq-action)
      ;; i -> c(nt*)
      ;; j -> c(nt*) forall nt* in nt**
      (recur loop ([nt* nt*][nt** nt**])
        (unless (null? nt*)
          (let* ([f (car nt*)]
                 [f* (map car nt**)]
                 [c n]
                 [fn (lambda ()
                       (set! c (sub1 c))
                       (when (zero? c) (record-not-leq i j)))])
            (for-each
             (lambda (f2)
               (record-not-leq-action (NT->num f) (NT->num f2) fn))
             f*)
            (loop (cdr nt*) (map cdr nt**)))))))
   list-nt roots final))

(define (RTG-min list-nt roots final)
  (printf "Calculating min NFA~n")
  ((make-minimization-algorithm
    (lambda (i j nt* nt** n NT->num record-not-leq record-not-leq-action)
      ;; i -> c(nt*)
      ;; j -> c(nt*) forall nt* in nt**
      (let ([c n])
        (for-each
         (lambda (nt*2)
           (let ([nt*2-done #f])
             (for-each
              (lambda (nt nt2)
                (record-not-leq-action
                 (NT->num nt)
                 (NT->num nt2)
                 (lambda ()
                   (unless nt*2-done
                       (set! nt*2-done #t)
                     (set! c (sub1 c))
                     (when (zero? c) (record-not-leq i j))))))
              nt* nt*2)))
         nt**))))
   list-nt roots final))

; ======================================================================;
; N->D section
; ======================================================================;

(define (N->D roots final tidy?)
  ;; n is an old nt
  ;; d is new nt
  (printf "Calculating N->D~n")
  (letrec
      ([n*->d '()]                      ; Maps set of NFA nts to a DFA nt
       [n*<=
        (lambda (n1* n2*)
          (andmap (lambda (n1) (mem-nt? n1 n2*)) n1*))]        
       [n*=
        (lambda (n1* n2*)
          (and (n*<= n1* n2*) (n*<= n2* n1*)))]
       [lookup
        (lambda (n*)
          (recur loop ([l n*->d])
            (cond
             [(null? l) #f]
             [(n*= n* (caar l)) (cdar l)]
             [else (loop (cdr l))])))]
       [AVS->nu-AVS                     ; Use same d for L-AVS and U-AVS
        (let ([l '()])
          (lambda (AVS)
            (match (assq AVS l)
              [(_ . nuAVS) nuAVS]
              [#f (let ([nuAVS (mk-AVS-nolist 'dfa-nuAVS)])
                    (set! l (cons (cons AVS nuAVS) l))
                    nuAVS)])))]
       [make-d-for-nt*
        (match-lambda
         [(($ L x)) (make-L (AVS->nu-AVS x))] 
         [(($ U x)) (make-U (AVS->nu-AVS x))] 
         [(($ L _) ...) (make-L (mk-AVS-nolist 'dfa-L))]
         [(($ U _) ...) (make-U (mk-AVS-nolist 'dfa-U))])]
       [count 0]
       [traverse
        (lambda (n*)
          (when (zero? (modulo count 25)) (printf ".") (flush-output))
          (set! count (add1 count))
          (let* ([n* (epsilon-close-nts n*)]
                 [n*-noI (map drop-I n*)])
            '(pretty-print `(traverse ,(map nt->sym n*)))
            (or (lookup n*-noI)
                ;; Need to traverse
                ;; assume n* epsilon-closed
                (let* ([d (make-d-for-nt* n*-noI)])
                  (set! n*->d (cons (cons n*-noI d) n*->d))
                  (for-each-prods-joined
                   (lambda (grsym nt**)
                     (if (and (not tidy?)
                              (not (null? nt**))
                              (not (null? (car nt**)))
                              (not (null? (cdar nt**))))
                         ;; Don't want to tidy multi-arity constructor
                         (for-each
                          (lambda (nt*) 
                            (add-prod d grsym 
                                      (map (lambda (nt) (traverse (list nt)))
                                           nt*)))
                          nt**)
                         ;; Yeah, tidy it
                         (recur loop ([nt** nt**][d* '()])
                           (match nt**
                             [(() ...)
                              ;; All done - add the resulting production
                              (add-prod d grsym (reverse d*))]
                             [((nt* . nt**) ...)
                              ;; nt* is all nts for a particular field
                              (loop nt** (cons (traverse nt*) d*))]))))
                   n*)
                  d))))])

    (for-each (lambda (nt) (traverse (list nt)))
              roots)
    (newline)

    '(begin
      (printf "Table:~n")
      (pretty-print
       (map (match-lambda
             [(n* . d) (cons (map nt->sym n*) (nt->sym d))])
            n*->d)))

    (printf "Calculating new roots, final~n")
    ;; Return new roots, and nu final
    (list
     ;; new roots
     (map (lambda (nt) (lookup (map drop-I (epsilon-close-nt nt))))
          roots)
     ;; new final
     (map (lambda (nt)
            (let* ([nt-noI (drop-I nt)]
                   [nu-nt (make-d-for-nt* (list nt-noI))])
              (for-each
               (match-lambda
                [(n* . d)
                 (when (mem-nt? nt-noI n*)
                   (add-prod d 
                              (match d
                                [($ L) '>=epsilon]
                                [($ U) '<=epsilon])
                              (list nu-nt)))])
               n*->d)
              nu-nt))
          final))))

;; ======================================================================
;; A series of composable grammar manipulation stages
;; Each takes and returns (roots final list-NT list-AVS . rest)
;; May just return (roots final)

(define (stage-restrict-reached roots final list-NT list-AVS)
  (printf "Calculating reached~n")
  (match-let*
      ([(reached-nt reached-nt?) (grammar-calc-reached roots)])
    (printf "Restricting NTs~n")
    (restrict-nts! list-AVS reached-nt?)
    ;;(display (map nt->sym reached-nt)) (newline)
    ;;(st:prods)
    ;;(newline)
    (list roots final reached-nt list-AVS)))

(define (stage-restrict-nonempty roots final list-NT list-AVS)
  (match-let*
      ([_ (printf "Calculating NT<->num~n")]
       [(num-NT NT->num num->NT) (calc-NT<->num list-NT)]
       [_ (printf "Calculating nonempty~n")]
       [(nonempty-nt nonempty-nt?)
        (grammar-calc-nonempty num-NT NT->num num->NT final)])
     (printf "Restricting NTs~n")
     (restrict-nts! list-AVS nonempty-nt?)
     ;;(display (map nt->sym nonempty-nt)) (newline)
     ;;(st:prods)
     ;;(newline)
     (list roots final nonempty-nt list-AVS)))

(define (stage-N->D roots final list-NT list-AVS)
  (match-let*
      ([(nuroots nufinal) (N->D roots final #t)])
    (list nuroots nufinal)))

(define (stage-N->D-notidy roots final list-NT list-AVS)
  (match-let*
      ([(nuroots nufinal) (N->D roots final #f)])
    (list nuroots nufinal)))

(define (stage-DFA-min roots final list-NT list-AVS)
  (match-let*
      ([old-nt->nu-nt (DFA-min list-NT roots final)])
    (list (map old-nt->nu-nt roots)
          (map old-nt->nu-nt final))))

(define (stage-NFA-min roots final list-NT list-AVS)
  (match-let*
      ([old-nt->nu-nt (NFA-min list-NT roots final)])
    (list (map old-nt->nu-nt roots)
          (map old-nt->nu-nt final))))

(define (stage-RTG-min roots final list-NT list-AVS)
  (match-let*
      ([old-nt->nu-nt (NFA-min list-NT roots final)])
    (list (map old-nt->nu-nt roots)
          (map old-nt->nu-nt final))))

(define (stage-kill-live-AVS roots final list-NT list-AVS)
  (for-each
   (lambda (AVS)
     (set-AVS-edgefrom! AVS '())
     (set-AVS-edgeto! AVS '())
     (set-AVS-objs! AVS '())
     (set-AVS-orig-objs! AVS '())
     (set-AVS-constraints! AVS '()))
   list-AVS)
  (list roots final))

(define (stage-invert-grammar roots final list-NT list-AVS)
  (convert-productions-to-AV-etc! list-AVS)
  (calc-productions! list-AVS 
                     (lambda (AVS) (memq AVS list-AVS))
                     #t
                     #t #t #t #t
                     #f #f)
  (list (append (map make-LI list-AVS)
                (map make-UI list-AVS))
        (map make-UI (select-L roots))))
  
(define (stage-nothing roots final list-NT list-AVS)
  (list roots final))

(define (calc-roots-final-L->R B list-AVS use-AVS?) 
  (calc-productions! list-AVS use-AVS?
                     #t #f
                     #t #t #t #t
                     #t #f)
  (list (map make-L B)
        (append (map make-UI B))))

(define (calc-roots-final-LU->center B list-AVS use-AVS?) 
  (calc-productions! list-AVS use-AVS?
                     #t
                     #t #t #t #t
                     #t #f)
  (match-let*
      ([L-B (map make-L B)]
       [(reached _) (grammar-calc-reached L-B)]
       ;[_ (pretty-print `(reached ,(map nt->sym reached)))]
       [reached-L (select-L reached)]
       ;[_ (pretty-print `(reached-L ,(map name-for-AVS reached-L)))]
       [reached-U (select-U reached)]
       ;[_ (pretty-print `(reached-U ,(map name-for-AVS reached-U)))]
       [cross-over (intersect reached-L reached-U)]
       [final (append (map make-L cross-over) 
                      (map make-U cross-over))])
    ;(pretty-print `(cross-over ,(map name-for-AVS cross-over)))
    ;(pretty-print `(final ,(map nt->sym final)))
    (list L-B final)))

(define (calc-roots-final-center->out B list-AVS use-AVS?) 
  (set! zeroary-op-AVS (mk-AVS-tmp 'zeroary-op))
  (calc-productions! list-AVS use-AVS?
                     #t #t
                     #t #t #t #t
                     #f #f)
  (match-let*
      ([L-B (map make-L B)]
       [(reached _) (grammar-calc-reached L-B)]
       ;[_ (pretty-print `(reached ,(map nt->sym reached)))]
       [reached-L (select-L reached)]
       ;[_ (pretty-print `(reached-L ,(map name-for-AVS reached-L)))]
       [reached-U (select-U reached)]
       ;[_ (pretty-print `(reached-U ,(map name-for-AVS reached-U)))]
       [cross-over (intersect reached-L reached-U)]
       [roots (append (list (make-UI zeroary-op-AVS))
                      (map make-LI cross-over)
                      (map make-UI cross-over))]
       [final (map make-UI B)])
    ;(pretty-print `(cross-over ,(map name-for-AVS cross-over)))
    ;(pretty-print `(roots ,(map nt->sym roots)))
    ;(pretty-print `(final ,(map nt->sym final)))
    (list roots final)))

(define (calc-roots-final-center->out-concrete-epsilon B list-AVS use-AVS?) 
  (set! zeroary-op-AVS (mk-AVS-tmp 'zeroary-op))
  (calc-productions! list-AVS use-AVS?
                     #t #t
                     #t #t 'concrete #t
                     #f #f)
  (match-let*
      ([L-B (map make-L B)]
       [(reached _) (grammar-calc-reached L-B)]
       ;[_ (pretty-print `(reached ,(map nt->sym reached)))]
       [reached-L (select-L reached)]
       ;[_ (pretty-print `(reached-L ,(map name-for-AVS reached-L)))]
       [reached-U (select-U reached)]
       ;[_ (pretty-print `(reached-U ,(map name-for-AVS reached-U)))]
       [cross-over (intersect reached-L reached-U)]
       [roots (append (list (make-UI zeroary-op-AVS))
                      (map make-LI cross-over)
                      (map make-UI cross-over))]
       [final (map make-UI B)])
    ;(pretty-print `(cross-over ,(map name-for-AVS cross-over)))
    ;(pretty-print `(roots ,(map nt->sym roots)))
    ;(pretty-print `(final ,(map nt->sym final)))
    (list roots final)))


(define (get-AVS-select-L roots final) (select-L roots))

(define (get-AVS-select-LIUI roots final) 
  (select-UI final))

; ----------------------------------------------------------------------

(define calc-size
  (lambda (str roots)
    (match-let*
        ([(reached-nt reached-nt?) (grammar-calc-reached roots)]
         [reached-AVS (list->set (map nt->AVS reached-nt))])
      (printf "==========> ~a: #nt=~s  #AVS=~s~n"
              str
              (length reached-nt)
              (length reached-AVS))
      ;;(printf "~s~n" (map name-for-AVS list-AVS))
      (list reached-nt reached-AVS))))

; --------

(define resultshow (void))
(define resultprod (void))

(define (minimize-constraints list-AVS use-AVS? B strategy)

  (printf "#### STRATEGY: ~s~n" strategy)

  (match-let*
      ([((calc-roots-final get-AVS) . stages) strategy]
       [(roots final) ((eval calc-roots-final) B list-AVS use-AVS?)])
  
    (recur loop ([roots roots]
                 [final final]
                 [rest '()]
                 [stages stages]
                 [sizes '()])

      (match stages
        [() 
         (match-let*
             ([(list-NT list-AVS) (calc-size "At end" roots)])
           (printf "Creating AV, constraints, edges from grammar~n")
           (convert-productions-to-AV-etc! list-AVS)
           (set! resultshow (lambda () (for-each show-AVS list-AVS)))
           (set! resultprod (lambda () (for-each prods-AVS list-AVS)))
           (list ((eval get-AVS) roots final)
                 (reverse (cons (list (length list-NT) (length list-AVS))
                                sizes))))]

        [(stage . rest-stages)
         (match-let*
             ([(list-NT list-AVS) 
               (calc-size (format "Before ~a" stage) roots)])
           (match (apply (eval stage) roots final list-NT list-AVS rest)
             [(roots final . _)
              (loop roots final '() rest-stages
                    (cons (list (length list-NT) (length list-AVS))
                          sizes))]))]))))

(define (minimize-all-constraints B stages)
  (let ([n num-AVS])
    (minimize-constraints list-AVS
                          (lambda (AVS) (< (AVS-num AVS) n))
                          B stages)))

; ======================================================================

(define (st:L files) 
  (st:analyze files)
  (calc-productions! list-AVS (lambda (AVS) #t)
                     #t
                     #t #t #t #t
                     #t #f))

(define (test-min-strategy files strategy)
  (printf "TEST-MIN: ~s~n" files)
  (st:flow-sensitive #f)
  (st:if-split #f)
  (match-let*
      ([(_ in out) 
        (parameterize 
         ([mrspidey:progress-handler (mrspidey:text-progress)]
          [mrspidey:error-handler    mrspidey:text-error])
         (time (sba-analyze-a-file-in-out (files->file-thunk* files))))]
       [io (map cdr (append in out))])
    (for-each display (reverse summary))
    (printf "Orig # AVSs: ~s~n" num-AVS)
    (for-each 
     (match-lambda
      [(sym . AVS) 
       (printf "Def: ~s,  AVS-num=~s~n" sym (AVS-num AVS))
       (pretty-print (AVS->SDL AVS))])
     out)
    (match-let
        ([(nu-defs . sizes) (time (minimize-all-constraints io strategy))])
    
      (printf "~n=========== Summary of final types =========~n")
      (for-each (lambda (AVS) 
                  (printf "AVS-num=~s~n" (AVS-num AVS))
                  (pretty-print (AVS->SDL AVS)))
                nu-defs)
      sizes)))


(define (test-min files) (test-min-strategy files default-strategy))

; ======================================================================

(define (to) (test-min "test/one.ss"))
(define (ts) (test-min "test/sum.ss"))
(define (tt) (test-min "test/test.ss"))

(define test-files
  (map (lambda (s) (string-append "mod/" s ".ss"))
       '(
         "TC-env"
         "TC-parse"
         "TC-test"
         "TC-type"
         "TC-eval"

         "mod-env"
         "2-1-env-list"
         "2-22-p"
         "2-22-tc2"
         "mod-TC-typechk"
         "shaft"
         "physics"
         "mod-gauss"
         ;;"mod-TC-parse"
         ;;"pcf-parse"
         )))

;     mod-interp - match
;     pcf-tc.ss - syntax error
;     mod-pp - match
;     "elevator" - match
;     mod-slatex.ss - too big
;     mod-TC.ss - too big

; ======================================================================

(define strategy-L->R-NFA-min
  '((calc-roots-final-L->R get-AVS-select-L)
    stage-restrict-reached
    stage-restrict-nonempty
    ;;stage-kill-live-AVS
    stage-N->D-notidy
    stage-NFA-min))

(define strategy-L->R-RTG-min
  '((calc-roots-final-L->R get-AVS-select-L)
    stage-restrict-reached
    stage-restrict-nonempty
    ;;stage-kill-live-AVS
    ;;stage-N->D
    stage-RTG-min))

(define strategy-fast  
  '((calc-roots-final-center->out-concrete-epsilon get-AVS-select-LIUI)
    stage-restrict-reached
    stage-restrict-nonempty
    ;;stage-kill-live-AVS
    ;stage-N->D-notidy
    ;stage-RTG-min
    stage-DFA-min
    )) 

(define default-strategy 
  '((calc-roots-final-L->R get-AVS-select-L)
    ;;stage-restrict-reached
    ;;stage-restrict-nonempty
    ;;stage-kill-live-AVS
    ;;stage-N->D
    ;;stage-RTG-min
    ))
;(define default-strategy strategy-fast)

(define strategy-options
  '(((calc-roots-final-L->R get-AVS-select-L)
     (calc-roots-final-center->out get-AVS-select-LIUI)
     )
    stage-restrict-reached
    stage-restrict-nonempty
    (stage-N->D stage-N->D-notidy stage-nothing)
    (stage-RTG-min stage-NFA-min stage-DFA-min stage-nothing)
    ))

(define strategy-options
  '(((calc-roots-final-center->out-concrete-epsilon get-AVS-select-LIUI)
     (calc-roots-final-L->R get-AVS-select-L)
     (calc-roots-final-center->out get-AVS-select-LIUI)
     )
    stage-restrict-reached
    stage-restrict-nonempty
    (stage-DFA-min stage-RTG-min stage-NFA-min stage-nothing)
    ))
    
(define test-strategies
  (recur loop ([opt strategy-options])
    (match opt
      [() '(())]
      [((? pair? opt1) . rest)
       (let ([rest (loop rest)])
         (apply append
                (map (lambda (r)
                       (map (lambda (o) (cons o r)) opt1))
                     rest)))]
      [(x . rest) (map (lambda (r) (cons x r))
                       (loop rest))])))

; ======================================================================

(define (test-min-files)
  (mapLR
   (lambda (file)
     (printf "============================================================~n")
     (let ([r (test-min file)])
       (pretty-print r)
       r))
   test-files))

(define (test-min-strategies)
  (map (lambda (s)
         (printf "#########################################################~n")
         (set! default-strategy s)
         (list s (test-min-files)))
       test-strategies))

;(trace NFA-min)
;(trace calc-LI-UI!)
;(trace make-minimization-algorithm)
;(trace minimize-constraints)
;(trace grammar-calc-nonempty)

(define (summarize l)
  (map
   (match-lambda
    [(desc nums)
     (let ([last-pairs (map (lambda (x) (rac (car x))) nums)])
       (list desc 
             (apply + (map car last-pairs))
             (apply + (map cadr last-pairs))))])
   l))
