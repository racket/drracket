; ======================================================================
; copy-live-constraints
;
; Input:  lower : list Tvar
;         upper : list Tvar
;
; Copies live constraints
; returns list-nu Tvar->nu
; ======================================================================
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

(define (copy-live-constraints lower upper)

  (pretty-debug-min 
    `(copy-live-constraints ,(map Tvar-name lower) ,(map Tvar-name upper)))

  (let*-vals 
    ( [(live-nts live-nt? live-tvars live-tvar? _ _ _)
        (calc-live-tvars-nts lower upper #f)])

    (copy-constraints-equiv! 
      (append lower upper live-tvars)
      live-tvar? live-nts
      (lambda (tvar) (list tvar))
      (lambda (AV) (list AV)))))

; ======================================================================

(define (copy-live-constraints-noe lower upper)

  (pretty-debug-min 
    `(copy-live-constraints-noe 
       ,(map Tvar-name lower) ,(map Tvar-name upper)))

  (let*-vals 
    ( [(live-nts live-nt? live-tvars live-tvar? _ _ _)
        (calc-live-tvars-nts lower upper #t)]
      [(get-nu-Tvar set-nu-Tvar!) (alloc-Tvar-field)]
      [list-nuTvar
        (map (lambda (Tvar) 
               (let ([nu (mk-Tvar 'min-live)])
                 (set-nu-Tvar! Tvar nu)
                 nu))
          live-tvars)]
      [dummy-tvars
        (copy-constraints-noe! live-tvar? live-nts get-nu-Tvar lower)])

    (values
      (append dummy-tvars list-nuTvar)
      get-nu-Tvar)))

;; ======================================================================
; calc-live-tvars-nts
;
; Calculates live Tvars and NTs
; Input:  lower : list Tvar
;         upper : list Tvar
;
; Uses find-nonempty-nts-rhs-nts to find nonempty nts and sources
; Walk "forward" in grammar using rhs-nts from crossover and AV-w/-const
; to calculate live
;
; Returns (values live-nts live-nt? live-tvars live-tvar? 
;          AV-w/-const crossover rhs-nts)

(define (calc-live-tvars-nts lower upper inline-epsilon)

  (pretty-debug-min 
    `(calc-live-tvars-nts  ,inline-epsilon
       ,(map Tvar-name lower) ,(map Tvar-name upper)))

  (let*-vals 
    ( [t (lambda (s) 
           (when timing-min 
             (min-record-progress (cons 'calc-live-tvars-nts s))))]
      [_ (t 0)]
      [(rhs-nts crossover AV-w/-const) 
        (find-nonempty-nts-rhs-nts lower upper inline-epsilon)]
      [_ (t 1)]
      [(live-nt? set-live-nt! get-live-nts) (field->set alloc-NT-field)]
      [_ (t 2)]
        
      ;; --------------------------------------------------
      ;; Walk forward from crossover and AV-w/-const, recording nts
        
      [_ (letrec ([mark-live
                    (lambda (nt)
                      (unless (eq? nt #t)
                        ;;(pretty-print `(mark-live ,(nt->sym nt)))
                        (unless (live-nt? nt)
                          (set-live-nt! nt)
                          (when (rhs-nts nt)
                            (for-each mark-live (rhs-nts nt))))))])

           (for-each
             (lambda (Tvar)
               (mark-live (Tvar-U Tvar))
               (mark-live (Tvar-L Tvar)))
             crossover)

           (for-each 
             (lambda (AV) (mark-live (AV-U AV)))
             AV-w/-const)

           (pretty-debug-min `(live-nt ,(map nt->sym (get-live-nts)))))]
      [_ (t 3)]

      ;; --------------------------------------------------
      ;; Have in (get-live-nts) all nts that are reached and non-empty
      ;; Calc live Tvar

      [(live-tvar? set-live-tvar! get-live-tvars) 
        (field->set alloc-Tvar-field)]
      [_ (t 4)]
      [_ (begin
           (for-each
             (match-lambda
               [($ NT tvar) (when (Tvar? tvar) (set-live-tvar! tvar))])
             (get-live-nts))
           (for-each set-live-tvar! lower)
           (for-each set-live-tvar! upper))]
      [_ (t 5)]
      [_ (pretty-debug-min `(live-Tvar ,(map Tvar-name (get-live-tvars))))])

    (pretty-debug-min
      `(calc-live-tvars-nts-results
         ,(map (lambda (nt)
                 `(nt-rhs ,(nt->sym nt)
                    ,(map (lambda (nt) (or (eq? nt #t) (nt->sym nt)))
                       (rhs-nts nt))))
            (get-live-nts))))

    (values
      (get-live-nts) live-nt?
      (get-live-tvars) live-tvar? 
      AV-w/-const
      crossover
      rhs-nts
      )))

;; ======================================================================

(define (follow-antimono-fields template)
  (or 
    (eq? template template-lam)
    (eq? template template-unit)
    (eq? template template-internal-class)
    (st:minimize-respect-assignable-part-of-fields)))

;; ======================================================================
; find-nonempty-nts-rhs-nts
;
; Calculates non-empty Tvars, NTs.
; For each non-empty NT, finds all rhs-nts NT' such that NT -> x.NT'

; Input:  lower : (listof Tvar)
;         upper : (listof Tvar)
;
; Returns (values rhs-nts crossover-tvars AV-w/-const)
;
; Walks "backwards" in grammer to find nonempty NTs
; Keeps track of rhs-nts on the way
;
; crossover is the set of Tvars tvar such that 
; both L(tvar_L) and L(tvar_U) are nonempty

;; #### Always looks at assignable fields ...

(define (find-nonempty-nts-rhs-nts lower upper inline-epsilon)
  (pretty-debug-min
    `(find-nonempty-tvars-nts-rhs-nts ,inline-epsilon
       ,(map Tvar-name lower) ,(map Tvar-name upper)))

  (let*-vals ( [(reached-tvar? set-reached-tvar!) (alloc-Tvar-field)]
               [(reached-AV? set-reached-AV!)     (alloc-AV-field)]
               [(rhs-nts set-rhs-nts!)            (alloc-NT-field)]
               [add-rhs-nt! 
                 (lambda (nt src) (set-rhs-nts! nt (cons src (rhs-nts nt))))]
               [crossover '()]
               ;; crossover is set of tvars with tvar_L and tvar_U nonempty
               [AV-w/-const '()]
               )
    (letrec
      ( [add-crossover! 
          (lambda (tvar)
            (set! crossover (cons tvar crossover)))]
        [walk-AV
          (lambda (AV src)
            (pretty-debug-min `(walk-AV ,(AV-num AV) ,(nt->sym src)))
            (if (reached-AV? AV)
              (add-rhs-nt! (AV-U AV) src)
              ;; Walk it
              (begin
                (set-reached-AV! AV #t)
                (mk-AV-NTs! AV)
                (let ([nt (AV-U AV)])
                  (set-rhs-nts! nt (list src))
                  (match AV
                    [($ AV _ (and template ($ template _ _ _ ref))
                       misc fields+ fields-)
                      (when (or 
                              (zero? (vector-length ref))
                              ;; the following are lazy
                              ;; ie (box empty) = empty
                              ;; but (empty -> empty) != empty
                              (eq? template template-lam)
                              (eq? template template-unit)
                              (eq? template template-internal-class)
                              (eq? template template-ivarset)
                              )
                        (set! AV-w/-const (cons AV AV-w/-const)))
                      (vector-for-each
                        (lambda (f) (walk-U f nt))
                        fields+)
                      (when (follow-antimono-fields template)
                        (pretty-debug-min
                          `(walking-U 
                             ,(eq? template template-lam)
                             ,(st:minimize-respect-assignable-part-of-fields)))
                        (vector-for-each 
                          (lambda (f) (walk-L f nt))
                          fields-))])))))]
        [reach-tvar
          (lambda (tvar)
            (unless (reached-tvar? tvar) 
              (set-reached-tvar! tvar #t)
              (mk-Tvar-NTs! tvar)))]
        [walk-U
          (lambda (tvar src)
            (let 
              ([f
                 (lambda (tvar)
                   (reach-tvar tvar)
                   (pretty-debug-min `(walk-U ,(Tvar-name tvar) ,(nt->sym src)))
                   (let ([nt (Tvar-U tvar)])
                     (if (rhs-nts nt)
                       (add-rhs-nt! nt src)
                       ;; Walk it
                       (begin
                         (set-rhs-nts! nt (list src))
                         (when (rhs-nts (Tvar-L tvar)) (add-crossover! tvar))
                         (pretty-debug-min `(walk-U ,(Tvar-name tvar)))
                         (for-each
                           (lambda (AV) (walk-AV AV nt))
                           (get-Tvar-objs tvar))
                         '(unless inline-epsilon
                            (for-each
                              (lambda (from)
                                (walk-U from nt))
                              (Tvar-edgefrom tvar)))))))])

              '(if inline-epsilon
                 ;; really want to walk all tvar2 st tvar2_U -> tvar_U
                 (for-each f (Tvar-transitive-edgefrom tvar))
                 (f tvar))
              (f tvar)))]

        [walk-L
          (lambda (tvar src)
            (let 
              ([f (lambda (tvar)
                    (reach-tvar tvar)
                    (pretty-debug-min
                      `(walk-L ,(Tvar-name tvar) ,(nt->sym src)))
                    (let ([nt (Tvar-L tvar)])
                      (if (rhs-nts nt)
                        (add-rhs-nt! nt src)
                        ;; Walk it
                        (begin
                          (set-rhs-nts! nt (list src))
                          (when (rhs-nts (Tvar-U tvar)) (add-crossover! tvar))
                          (pretty-debug-min `(walk-L ,(Tvar-name tvar)))

                          (unless inline-epsilon
                            (for-each
                              (lambda (to)
                                ;; Have to_L -> tvar_L
                                (walk-L to nt))
                              (Tvar-edgeto tvar)))
                          (for-each
                            (match-lambda
                              [($ con _ _ field-no tvar2 sign misc)
                                ;; Have tvar2_L -> rng(tvar_L)
                                ;; or   tvar2_U -> dom(tvar_L)
                                (if sign
                                  (walk-L tvar2 nt)
                                  (walk-U tvar2 nt))]
                              [($ con-filter _ filter tvar2)
                                ;; Have tvar2_L -> tvar_L
                                (unless inline-epsilon (walk-L tvar2 nt))])
                            (Tvar-constraints tvar))))))])
              '(if inline-epsilon
                 ;; really want to walk all tvar2 st tvar2_L -> tvar_L
                 (for-each f (Tvar-transitive-edgeto tvar))
                 (f tvar))
              (f tvar)))])

      (for-each (lambda (tvar) (walk-U tvar #t)) upper)
      (for-each (lambda (tvar) (walk-L tvar #t)) lower)

      (min-record-progress 'find-nonempty-nts-rhs-nts-done)
      (: rhs-nts (NT -> (union NT true)))

      (pretty-debug-min
        `(find-nonempty-nts-rhs-nts-returns
           ,(map AV-num AV-w/-const)
           ,(map Tvar-name crossover)))

      (values
        (lambda (nt) 
          (let ([r (rhs-nts nt)])
            (pretty-debug-min
              `(rhs-nts ,(nt->sym nt)
                 ,(map (lambda (nt) (or (eq? nt #t) (nt->sym nt))) r)))
            r))
        crossover 
        AV-w/-const))))
        
;; ======================================================================
; copy-constraints!
; 
; tvar->equiv and AV->equiv describe an equivalence relation on tvars and AVs
;
; Copies the contents of a set of Tvars
; Takes care not to duplicate AVs
; returns (values nu-tvars tvar->nu-tvar)
; live-tvars should include lower and upper, and may contain duplicates

(define (copy-constraints-equiv!
          live-tvars live-tvar? live-nts 
          tvar->equiv AV->equiv)

  (pretty-debug-min `(copy-constraints-equiv! ,(map nt->sym live-nts)))
  (let*-vals 
    ( [t (lambda (s) 
           (when timing-min 
             (min-record-progress (cons 'copy-constraint-equivs! s))))]
      [_ (t 0)]

      ;; --- Allocate new tvar
      [(Tvar-nuTvar set-Tvar-nuTvar!) (alloc-Tvar-field)]
      [list-nu-tvars '()]
      [_ (for-each
           (lambda (tvar)
             (unless (Tvar-nuTvar tvar)
               (let ([nu (mk-Tvar 'copy-constraints!)])
                 (set! list-nu-tvars (cons nu list-nu-tvars))
                 (for-each
                   (lambda (tvar2) (set-Tvar-nuTvar! tvar2 nu))
                   (tvar->equiv tvar)))))
           live-tvars)]
      [_ (t 1)]

      [Tvar->nuTvar 
        (lambda (Tvar)
          (if (live-tvar? Tvar)
            (let ([nu (Tvar-nuTvar Tvar)])
              (assert (Tvar? nu) 'Tvar->nuTvar 
                nu (live-tvar? Tvar) (memq Tvar live-tvars))
              nu)
            (let ([dummy (mk-Tvar 'dummy)])
              (set! list-nu-tvars (cons dummy list-nu-tvars))
              dummy)))]

      [(AV-nuAV set-AV-nuAV!) (alloc-AV-field)]
      [copy-AV 
        (lambda (AV)
          (or 
            (AV-nuAV AV)
            (match AV
              [(and AV ($ AV _ template misc fields+ fields-))
                (let* 
                  ( [nu-fields+ (vector-map Tvar->nuTvar fields+)]
                    [nu-fields- (vector-map Tvar->nuTvar fields-)]
                    [nu-AV (create-AV template misc nu-fields+ nu-fields-)])
                  (set-AV-nuAV! AV nu-AV)
                  (for-each
                    (lambda (eq-AV) 
                      (when (and
                              (not (AV-nuAV eq-AV))
                              (eq? (AV-template eq-AV) template)
                              (vector-andmap2 eq? nu-fields+ 
                                (vector-map Tvar->nuTvar (AV-fields+ eq-AV)))
                              '(vector-andmap2 eq? nu-fields-
                                 (vector-map Tvar->nuTvar (AV-fields- eq-AV))))
                        (set-AV-nuAV! eq-AV nu-AV)))
                    (AV->equiv AV))
                  nu-AV)])))]
      [_ (t 1.1)])

    (for-each
      (match-lambda
        [($ NT source LU)
          (when (Tvar? source)
            (let ([dest (Tvar->nuTvar source)])
              (case LU
                [(U)
                  ;; --- AVs
                  (for-each (lambda (AV) (new-AV! dest (copy-AV AV)))
                    (Tvar-objs source))]
                [(L)
                  ;; --- Constraints
                  (for-each
                    (match-lambda
                      [($ con _ template field-no Tvar sign)
                        (when (live-tvar? Tvar)
                          (new-con! dest 
                            (create-con template field-no 
                              (Tvar->nuTvar Tvar) sign)))]
                      [($ con-filter _ filter Tvar)
                        (when (live-tvar? Tvar)
                          ;; (new-con! dest
                          ;;  (create-con-filter filter (Tvar->nuTvar Tvar))
                          (new-edge! dest (Tvar->nuTvar Tvar)))])
                    (Tvar-constraints source))

                  ;; --- Edges
                  (for-each
                    (lambda (Tvar2)
                      (when (live-tvar? Tvar2)
                        (let ([nu (Tvar->nuTvar Tvar2)])
                          (unless (eq? dest nu)
                            (new-edge! dest nu)))))
                    (Tvar-edgeto source))])))])
      live-nts)
    (t 2)

    (pretty-debug-min
      `(copy-constraints-equiv!
         old->new
         ,(map 
            (lambda (tvar) 
              (list (Tvar-name tvar) (Tvar-name (Tvar-nuTvar tvar))))
            live-tvars)))


    (values list-nu-tvars Tvar-nuTvar))) 

;; ======================================================================
; copy-constraints-noe!
; 
; Copies the contents of a set of Tvars
; Does closure under epsilon
; Takes care not to add duplicate AVs
; But may make many copies of the same AV in different Tvar

(define (copy-constraints-noe! live-tvar? live-nts Tvar-nuTvar lower)
  (pretty-debug-min
    `(copy-constraints-noe!
       live-nts
       ,(map nt->sym live-nts)
       lower
       ,(map Tvar-name lower)
       conversion
       ,(map 
          (lambda (tvar) 
            (list (Tvar-name tvar) (Tvar-name (Tvar-nuTvar tvar))))
          (filter Tvar? (map NT-tvar live-nts)))))
  '(let*-vals
    ( [(AV-nuAV set-AV-nuAV!) (alloc-AV-field)]
      [dummy-tvars '()]
      [mk-dummy-tvar
        (lambda ()
          (let ([dummy (mk-Tvar 'dummy)])
            (set! dummy-tvars (cons dummy dummy-tvars))
            dummy))]
      [Tvar->nuTvar 
        (lambda (Tvar)
          (if (live-tvar? Tvar)
            (Tvar-nuTvar Tvar)
            (mk-dummy-tvar)))]
      [table '()]
      [Tvar*->nuTvar
        (lambda (Tvar*)
          (let* ( [Tvar* (if (Tvar? Tvar*) (list Tvar*) Tvar*)]
                  [nu-Tvar* 
                    (list->set 
                      (filter-map
                        (lambda (Tvar)
                          (if (live-tvar? Tvar) (Tvar->nuTvar Tvar) #f))
                        Tvar*))])
            (match nu-Tvar*
              [() (mk-dummy-tvar)]
              [(Tvar) Tvar]
              [_ (or (ormap
                       (match-lambda
                         [(Tvar2* . nu) 
                           (if (set-eq? Tvar2* nu-Tvar*) nu #f)])
                       table)
                   (let ([nu (mk-Tvar 'multiple)])
                     (set! table (cons (cons nu-Tvar* nu) table))
                     (for-each (lambda (to) (new-edge! nu to)) nu-Tvar*)
                     nu))])))]
      [tag-reached-AV (gensym)])

    (for-each
      (match-lambda
        [($ NT source LU)
          (when (Tvar? source)
            (pretty-debug-min `(copying ,(Tvar-name source)))
            (let ([dest (Tvar->nuTvar source)])
              (case LU
                [(U)
                  ;; --- AVs
                  (for-each
                    (match-lambda
                      [(and AV 
                         ($ AV _ 
                           (and template ($ template _ signs)) misc fields))

                        (pretty-debug-min `(AV ,(Tvar-name dest)
                                             ,(vector-map Tvar-name fields)))

                        (new-AV! dest
                          (or (AV-nuAV AV)
                            (let* ([nu-fields
                                     (vector-map
                                       (lambda (sign field)
                                         (if sign
                                           (Tvar->nuTvar field)
                                           (Tvar*->nuTvar 
                                             (Tvar-transitive-edgeto field))))
                                       signs fields)]
                                    [nu-AV (create-AV template misc nu-fields)])
                              (set-AV-nuAV! AV nu-AV)
                              nu-AV)))])
                    (Tvar-objs source))]

                [(L)
                  ;; --- Constraints
                  (for-each
                    (match-lambda
                      [($ con _ template field-no Tvar)
                        (for-each
                          (lambda (Tvar)
                            (when (live-tvar? Tvar)
                              (new-con! dest
                                (create-con template field-no
                                  (Tvar->nuTvar Tvar)))))
                          (if (vector-ref (template-signs template) field-no)
                            (Tvar-transitive-edgeto Tvar)
                            (list Tvar)
                            ;;(Tvar-transitive-edgefrom Tvar)
                            ))]
                      [($ con-filter _ filter Tvar) 
                        ;; Was treated as edge
                        (void)])
                    (Tvar-constraints source))])))])
      live-nts)

    (for-each 
      (lambda (L)
        (let ([nu-L (Tvar->nuTvar L)])
          (for-each
            (lambda (to)
              (when (live-tvar? to)
                (let ([nu-to (Tvar->nuTvar to)])
                  (unless (eq? nu-L nu-to)
                    (new-edge! nu-L nu-to)))))
            (Tvar-transitive-edgeto L))))
      lower)

    (append dummy-tvars (map cdr table))))

;; ======================================================================
