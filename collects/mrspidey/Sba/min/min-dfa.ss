; ======================================================================
; minimize-constraints-dfa-min-fast
;
; Input:  lower : list Tvar
;         upper : list Tvar
;
; see practical.tex
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

(define (minimize-constraints-dfa-min-fast
          lower upper 
          helper-fn)

  (pretty-debug-dfa-min 
    `(minimize-constraints-dfa-min-fast
       lower ,(map Tvar-name lower)
       upper ,(map Tvar-name upper)))

  (let*-vals
    ([t (lambda (s) 
          (when timing-min 
            (min-record-progress 
              (cons 'minimize-constraints-dfa-min-fast s))))]
      [_ (t 0)]

      [(live-tvars live-tvar? live-nts tvar->equiv AV->equiv)
        ((copy-live-constraints-few-e #t #t) lower upper)]
      [_ (t 1)]

      ;; Figure out representative elements for Tvars,
      ;; and live AV
      [(tvar->rep set-tvar-rep!) (alloc-Tvar-field)]
      [(is-rep? add-rep! get-list-rep) (field->set alloc-Tvar-field)]
      [(live-AV? set-live-AV! get-live-AVs) (field->set alloc-AV-field)]
      [_ (for-each
           (lambda (tvar)
             (for-each set-live-AV! (get-Tvar-objs tvar))
             (unless (tvar->rep tvar)
               (add-rep! tvar)
               (for-each 
                 (lambda (eq) (set-tvar-rep! eq tvar))
                 (tvar->equiv tvar))
               (assert (tvar->rep tvar))))
           live-tvars)]
      [live-AVs (get-live-AVs)]
      [rep-tvars (get-list-rep)]
      [_ (t 2)]

      ;; states = rep-tvars + live-AVs + dummy-tvar
      [dummy-tvar (mk-Tvar 'Grammar-dummy)]
      [all-states (append rep-tvars (list dummy-tvar) live-AVs)]
      [state->sym (lambda (state) 
                    (cond
                      [(Tvar? state) (Tvar-name state)]
                      [(AV? state) (AV->rep state)]
                      [else (error 'state->sym "Bad state ~s" state)]))]

      [grsym-eq?
        (lambda (g1 g2)
          ;; (andmap eq? g1 g2)
          (recur loop ([g1 g1][g2 g2])
            (if (null? g1)
              (null? g2)
              (if (null? g2)
                #f
                (and (eq? (car g1) (car g2))
                  (loop (cdr g1) (cdr g2)))))))]

      ;; mapping from grsyms to numbers
      [num-grsym 0]
      [grsym-table '()]
      [grsym->number
        (lambda (g1)
          (or
            (ormap
              (match-lambda
                [(n . g2) (and (grsym-eq? g1 g2) n)]
                [_ #f])
              grsym-table)
            (begin
              (set! grsym-table (cons (cons num-grsym g1) grsym-table))
              (begin0
                num-grsym
                (set! num-grsym (add1 num-grsym))))))]

      [(tvar-EF-epsilon set-tvar-EF-epsilon!)
        (alloc-Tvar-field (lambda () '()))]
      [(tvar-EF set-tvar-EF!)
        (alloc-Tvar-field (lambda () '()))]
      [(AV-EF-epsilon set-AV-EF-epsilon!)
        (alloc-AV-field (lambda () '()))]
      [(AV-EF set-AV-EF!)
        (alloc-AV-field (lambda () '()))]

      [add-tvar-EF! 
        (lambda (tvar g1 s1)
          (let* ( [tvar (tvar->rep tvar)]
                  [n1 (grsym->number g1)]
                  [x (tvar-EF tvar)])
            (unless (ormap 
                      (match-lambda [(n2 . s2) (and (= n1 n2) (eq? s1 s2))])
                      x)
              (set-tvar-EF! tvar (cons (cons n1 s1) x)))))]
      [add-AV-EF! 
        (lambda (AV g1 s1)
          (let ( [x (AV-EF AV)]
                 [n1 (grsym->number g1)])
            (unless (ormap 
                      (match-lambda [(n2 . s2) (and (= n1 n1) (eq? s1 s2))])
                      x)
              (set-AV-EF! AV (cons (cons n1 s1) x)))))]
      [add-tvar-EF-epsilon!
        (lambda (tvar s1)
          (let* ( [tvar (tvar->rep tvar)]
                  [x (tvar-EF-epsilon tvar)])
            (unless (memq s1 x)
              (set-tvar-EF-epsilon! tvar (cons s1 x)))))]
      [add-AV-EF-epsilon!
        (lambda (AV s1)
          (let ([x (AV-EF-epsilon AV)])
            (unless (memq s1 x)
              (set-AV-EF-epsilon! AV (cons s1 x)))))]
      [state-EF-epsilon
        (lambda (state)
          (if (AV? state) 
            (AV-EF-epsilon state)
            (tvar-EF-epsilon state)))]
      [state-EF
        (lambda (state)
          (if (AV? state) 
            (AV-EF state)
            (tvar-EF state)))]
      [_ (t 2.1)]

      ;; helper-fn sets things up for inv-delta-traversal
      ;; Sets U field of each state to:
      ;;     (listof (cons (listof any) state))

      [extra-seperate (helper-fn live-tvars live-tvar? tvar->rep live-AVs
                        add-tvar-EF! add-tvar-EF-epsilon!
                        add-AV-EF! add-AV-EF-epsilon!)]
      [_ (t 3)]

      ;; Show productions
      [_ (pretty-debug-dfa-min
           `(productions
              ,(map 
                 (lambda (state)
                   (list (state->sym state)
                     (map state->sym (state-EF-epsilon state))
                     (map
                       (match-lambda [(g . s) (cons g (state->sym s))])
                       (state-EF state))))
                 all-states)))]

      ;; Vector of buckets for productions
      [buckets (make-vector num-grsym '())]
      [_ (min-record-progress (cons 'num-buckets num-grsym))]

      ;; inv-delta-traversal: 
      ;;          P(states) x (P(states) -> void) -> void
      ;; inv-delta-traversal(X, split)
      ;; => no equiv class partially in X
      ;; => Call split with each "partition of equiv classes" ...
      ;; For effeciency, we call split with a "generator" for the set C_a
      ;; The generator is passed a function add-split!, and then
      ;; calls add-split! on each state in C_a

      ;; --- Set things up for Hopcrofts Algorithm

      ;; Condition 1, thms hopcroft lub and glb
      ;; Also, to make up for grammar being "incomplete",
      ;; seperate states into equiv classes so states in same equiv class
      ;; have productions on same grammar symbols

      [seperate-grsyms (make-vector num-grsym '())]
      [seperate-epsilon '()]
      [_ (for-each
           (lambda (state)
             (when (not (null? (state-EF-epsilon state)))
               (set! seperate-epsilon (cons state seperate-epsilon)))
             (for-each
               (match-lambda
                 [(g . s)
                   (vector-set! seperate-grsyms g
                     (cons s (vector-ref seperate-grsyms g)))])
               (state-EF state)))
           all-states)]
      [_ (pretty-debug-dfa-min
           `(seperate-epsilon ,(map state->sym seperate-epsilon)))]
      [_ (pretty-debug-dfa-min
           `(seperate-grsyms 
              ,(map 
                 (lambda (s) (map state->sym s))
                 (vector->list seperate-grsyms))))]
      [seperate (append
                  (list rep-tvars)
                  (list live-AVs)
                  extra-seperate
                  ;(map list lower)
                  ;(map list upper)
                  (list seperate-epsilon)
                  (vector->list seperate-grsyms))]
      [seperate (filter 
                  (lambda (l) (not (null? l)))
                  seperate)]
      [_ (pretty-debug-dfa-min
           `(seperate ,(map (lambda (s) (map state->sym s)) seperate)))]
      [_ (t 4)]

      [inv-delta-traversal
        (lambda (states split)
          (pretty-debug-dfa-min 
            `(inv-delta-trav 
               ,(map (lambda (state)
                       `(,(state->sym state)
                          (epsilon 
                            ,(map state->sym (state-EF-epsilon state)))
                          ,(map
                             (match-lambda [(g . s) (cons g (state->sym s))])
                             (state-EF state))))
                  states)))
          ;; Do epsilons
          (split 
            (lambda (add-split!)
              (for-each
                (lambda (state)
                  (for-each 
                    add-split!
                    (state-EF-epsilon state)))
                states)))

          ;; Put rest into buckets
          (let ([bucket-ndxs '()])
            (for-each
              (lambda (state)
                (for-each
                  (match-lambda
                    [(n . s)
                      (let ([old (vector-ref buckets n)])
                        (vector-set! buckets n (cons s old))
                        (when (null? old)
                          (set! bucket-ndxs (cons n bucket-ndxs))))])
                  (state-EF state)))
              states)
            ;; Run thru buckets ...
            (for-each
              (lambda (i)
                (let ([s* (vector-ref buckets i)])
                  (split (lambda (add-split!) (for-each add-split! s*)))
                  (vector-set! buckets i '())))
              bucket-ndxs)))]

      [(state-aux set-state-aux!) (alloc-NT-field)]
      [(state->rep state->equiv equiv-state? list-rep-states)
        (Hopcroft-calc-equivalences 
          all-states
          seperate
          inv-delta-traversal
          state-aux set-state-aux! state->sym)]
      [_ (t 5)]

      [_ (begin
           (pretty-debug-dfa-min
             `(list-rep-state ,(map state->sym list-rep-states)))
           (pretty-debug-dfa-min
             `(state->equiv
                ,(map (lambda (state) 
                        (map state->sym (state->equiv state)))
                   list-rep-states))))]

      ;; ----------------------------------------
      ;; Have equivalence relation on states
      ;; Combine w/ previous relation on Tvars
      ;; Convert to equivalence relation on Tvars

      [tvar->equiv
        (lambda (tvar)
          (let ([r (apply append (map tvar->equiv (state->equiv tvar)))])
            (pretty-debug-dfa-min 
              `(tvar->equiv ,(state->sym tvar) = ,(map state->sym r)))
            r))]

      [AV->equiv
        (lambda (x)
          (pretty-debug-dfa-min `(AV->equiv ,(state->sym x)))
          (let ([e (state->equiv x)])
            (pretty-debug-dfa-min 
              `(AV->equiv ,(state->sym x) = ,(map state->sym e)))
            e))]
      [_ (t 6)])

    (copy-constraints-equiv!
      live-tvars
      live-tvar?
      live-nts
      tvar->equiv
      AV->equiv)))

;; ======================================================================

(define (minimize-constraints-dfa-min-lub lower upper)
  (minimize-constraints-dfa-min-fast lower upper 

    ;; helper-fn
    ;; helper-fn sets things up for inv-delta-traversal
    ;; Sets U field of each state to:
    ;;     (listof (cons (listof any) state))

    (lambda (live-tvars live-tvar? tvar->rep live-AVs
              add-tvar-EF! add-tvar-EF-epsilon!
              add-AV-EF! add-AV-EF-epsilon!)

      (for-each
        (lambda (tvar)

          ;; --- Condition 2
          ;; If [a <= b] => forall a'~a exists b'~b st [a' <= b']
          (for-each
            (lambda (from) 
              (when (live-tvar? from)
                (add-tvar-EF-epsilon! tvar (tvar->rep from))))
            (Tvar-edgefrom tvar))
          (for-each
            (lambda (AV) (add-tvar-EF-epsilon! tvar AV))
            (get-Tvar-objs tvar))

          ;; --- Conditions 4 and 5
          (for-each
            (match-lambda
              [($ con _ template field alpha sign)
                (when (live-tvar? alpha)
                  (if sign
                    ;; [rng(tvar) <= alpha]
                    (add-tvar-EF! alpha
                      (list 'R4 template field) (tvar->rep tvar))
                    (when #t ;; (follow-antimono-fields template)
                      ;; Condition 5
                      ;; At bottom we put beta (or tvar) in own equiv class
                      ;; [alpha <= dom(tvar)]
                      (add-tvar-EF! tvar 
                        (list 'R5 template field) (tvar->rep alpha)))))]
              [_ (void)])
            (Tvar-constraints tvar)))
        live-tvars)

      (for-each
        (match-lambda
          [(and AV ($ AV _ template _ fields+ fields-))
            ;; --- Condition 3
            (for i 0 (vector-length fields+)
              (let ([field (vector-ref fields+ i)])
                (when (live-tvar? field)
                  (add-AV-EF! AV
                    (list 'R3 template i) (tvar->rep field)))))])
        live-AVs)

      ;; extra partitions
      (append
        ;; Put each beta st [alpha <= dom(beta)] in separate equiv class
        (map list
          (map tvar->rep
            (filter
              (lambda (tvar)
                (ormap
                  (match-lambda
                    [($ con _ template field alpha #f)
                      (live-tvar? alpha)]
                    [_ #f])
                  (Tvar-constraints tvar)))
              live-tvars)))

        ;; put AVs into equiv class according to templates
        ;; h maps templates to list of AVs
        (let ([h (make-hash-table)])
          (for-each
            (lambda (AV)
              (let ([t (AV-template AV)])
                (hash-table-put! h t
                  (cons AV (hash-table-get h t (lambda () ()))))))
            live-AVs)
          (hash-table-map h
            (lambda (t AVs) AVs))))
        
      )))

;; ======================================================================

(define (minimize-constraints-dfa-min-glb lower upper)
  (minimize-constraints-dfa-min-fast lower upper 

    ;; helper-fn
    ;; helper-fn sets things up for inv-delta-traversal
    ;; Sets U field of each state to:
    ;;     (listof (cons (listof any) state))

    (lambda (live-tvars live-tvar? tvar->rep live-AVs
              add-tvar-EF! add-tvar-EF-epsilon!
              add-AV-EF! add-AV-EF-epsilon!)

        (for-each
          (lambda (tvar)

            ;; --- Condition 2
            ;; If [a <= b] => forall b'~b exists a'~a st [a' <= b']
            (for-each
              (lambda (from) 
                (when (live-tvar? from)
                  (add-tvar-EF-epsilon! from (tvar->rep tvar))))
              (Tvar-edgefrom tvar))
            (for-each
              (lambda (AV) (add-AV-EF-epsilon! AV (tvar->rep tvar)))
              (get-Tvar-objs tvar))

            ;; --- Condition 4
            (for-each
              (match-lambda
                [($ con _ template field alpha #t)
                  (when (live-tvar? alpha)
                    ;; [rng(tvar) <= alpha]
                    (add-tvar-EF! tvar
                      (list 'R4 template field) (tvar->rep alpha)))]
                [_ (void)])
              (Tvar-constraints tvar)))
          live-tvars)

        (for-each
          (match-lambda
            [(and AV ($ AV _ template _ fields+ fields-))
              ;; --- Condition 3
              (for i 0 (vector-length fields+)
                (let ([field (vector-ref fields+ i)])
                  (when (live-tvar? field)
                    (add-tvar-EF! field (list 'R3 template i) AV))))
              ;; --- Condition 5
              (when (follow-antimono-fields template)
                (for i 0 (vector-length fields-)
                  (let ([field (vector-ref fields- i)])
                    (when (live-tvar? field)
                      ;; [dom(AV) <= field]
                      ;; At bottom we put AV in own equiv class
                      (add-AV-EF! AV 
                        (list 'R5 template i) (tvar->rep field))))))])
          live-AVs)

        ;; Conditions 5,6, thm 1.2
        ;; extra partitioning
      (append
        ;; Conditions 5, thm 1.2
        (filter-map
          (match-lambda
            [(and AV ($ AV _ template _ _ fields-))
              (and 
                (follow-antimono-fields template)
                (> (vector-length fields-) 0)
                (list AV))])
          live-AVs)
        ;; Conditions 6, thm 1.2
        (let ([h (make-hash-table)])
          (for-each
            (match-lambda
              [(and AV ($ AV _ template))
                (hash-table-put! h template
                  (cons AV 
                    (hash-table-get h template (lambda () '()))))])
            live-AVs)
          (hash-table-map h
            (lambda (template AVs) AVs)))))))

;; ======================================================================




