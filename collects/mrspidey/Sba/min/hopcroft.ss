;; dfa-min.ss
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
;; ======================================================================
;; CHANGES
;; 01.11.96: Commented out hack to put all partitions on worklist initially
;; ======================================================================

(define-typed-structure state-aux (eqvcl prev next touched))
(define-typed-structure dfa-eqvcl (states count on-worklist touched))

;(define inv-delta-traversal-timer (make-timer))
;(define Fn-a-timer     (make-timer))
;(define finish-a-timer (make-timer))

;; ======================================================================

(define (Hopcroft-calc-equivalences States seperate
                                    inv-delta-traversal
                                    state-aux set-state-aux! state->rep)

  ;; states must have an aux field with set-state-aux! and state-aux fns
  ;; seperate is a list of list of States
  ;; any partition must be included in some element of seperate
  
  ;; inv-delta-traversal: P(states) (Sigma P(states) -> void) -> void
  ;; inv-delta-traversal(X, Fn)
  ;; => Fn is called with all pairs (a,C) where C = { s | delta(s,sigma) in X}

  (pretty-debug-dfa-min 
   `(States ,(map state->rep States)))
  (pretty-debug-dfa-min
   `(seperate ,(map (lambda (c) (map state->rep c)) seperate)))

  (for-each 
   (lambda (s) (set-state-aux! s (make-state-aux (void) (void) (void) #f)))
   States)
  
  (for-each
   (lambda (c)
     (for-each
       (lambda (s) (assert (state-aux? (state-aux s)) 1 (state->rep s)))
       c))
   seperate)

  (let* 
      ( [t (lambda (s) 
          (when timing-min (min-record-progress (cons 'hopcroft s))))]
        [_ (t 0)]
        [state-prev  (lambda (s) (state-aux-prev  (state-aux s)))]
        [state-next  (lambda (s) (state-aux-next  (state-aux s)))]
        [state-eqvcl (lambda (s) (state-aux-eqvcl (state-aux s)))]
        [state-touched (lambda (s) (state-aux-touched (state-aux s)))]
        [set-state-prev! (lambda (s p) (set-state-aux-prev!  (state-aux s) p))]
        [set-state-next! (lambda (s p) (set-state-aux-next!  (state-aux s) p))]
        [set-state-eqvcl! (lambda (s p) (set-state-aux-eqvcl! (state-aux s) p))]
        [set-state-touched! 
          (lambda (s p) (set-state-aux-touched! (state-aux s) p))]

        [partition '()]
        [mk-empty-eqvcl 
          (lambda () 
            (let ([X (make-dfa-eqvcl '() 0 #f '())])
              (set! partition (cons X partition))
              X))]

        [add-to-eqvcl!
          (lambda (X s)
            (let ([first (dfa-eqvcl-states X)])
              (set-state-next! s first)
              (unless (null? first) (set-state-prev! first s))
              (set-dfa-eqvcl-states! X s)
              (set-state-prev! s X)
              (set-state-eqvcl! s X)
              (set-dfa-eqvcl-count! X (add1 (dfa-eqvcl-count X)))))]
        [remove-from-eqvcl!
          (lambda (X s)
            (assert (eq? (state-eqvcl s) X))
            (let ([prev (state-prev s)]
                   [next (state-next s)])
              (unless (null? next) (set-state-prev! next prev))
              (if (dfa-eqvcl? prev)
                (set-dfa-eqvcl-states! prev next)
                (set-state-next! prev next))
              (set-dfa-eqvcl-count! X (sub1 (dfa-eqvcl-count X)))))]
        [eqvcl->list
          (lambda (c)
            (recur loop ([state (dfa-eqvcl-states c)])
              (if (null? state)
                '()
                (cons state (loop (state-next state))))))]

        [worklist '()]
        [add-to-worklist!
          (lambda (X)
            (assert (not (dfa-eqvcl-on-worklist X)))
            (set! worklist (cons X worklist))
            (set-dfa-eqvcl-on-worklist! X #t))]

        [split
          (lambda (generate-split-states)

            ;; Change so each partition totally included or excluded in states
            ;; See Cliff Click's thesis, page ~40.
             
            (pretty-debug-dfa-min `(split))
 
            (let* ([touched '()])
              (begin0
                (generate-split-states
                  (lambda (s)
                    (pretty-debug-dfa-min
                      `(add-split ,(state->rep s) ,(state-aux s)))
                    (assert (state-aux? (state-aux s))
                      2 (state->rep s) (state-aux s))
                    (let* ([s-eqvcl (state-eqvcl s)])
                      (when (null? (dfa-eqvcl-touched s-eqvcl))
                        (set! touched (cons s-eqvcl touched))
                        (assert (not (state-touched s))))
                      (unless (state-touched s)
                        ;;(assert (not (memq s (dfa-eqvcl-touched s-eqvcl))))
                        (set-dfa-eqvcl-touched!
                          s-eqvcl 
                          (cons s (dfa-eqvcl-touched s-eqvcl)))
                        (set-state-touched! s #t)))))
                (for-each
                  (lambda (Z)    

                    (pretty-debug-dfa-min
                      `(touched
                         ,(map state->rep (eqvcl->list Z))
                         ,(map state->rep (dfa-eqvcl-touched Z))
                         ,(= (dfa-eqvcl-count Z) 
                            (length (dfa-eqvcl-touched Z)))))
                    (if (= (dfa-eqvcl-count Z) 
                          (length (dfa-eqvcl-touched Z)))

                      ;; No need to change, just clear touched
                      (for-each (lambda (s) (set-state-touched! s #f))
                        (dfa-eqvcl-touched Z))

                      ;; Need to make new eqvcl 
                      (let ([nu (mk-empty-eqvcl)])
                        ;;(printf ".") (flush-output)
                        (for-each
                          (lambda (s) 
                            (remove-from-eqvcl! Z s)
                            (set-state-touched! s #f)
                            (add-to-eqvcl! nu s))
                          (dfa-eqvcl-touched Z))
                        (assert (not (null? (dfa-eqvcl-states Z))))
                        (assert (not (null? (dfa-eqvcl-states nu))))
                        (cond
                          [(dfa-eqvcl-on-worklist Z)
                            (add-to-worklist! nu)]
                          [(< (dfa-eqvcl-count nu) (dfa-eqvcl-count Z))
                            (add-to-worklist! nu)
                            ;; Kludge cause not complete grammar
                            (add-to-worklist! Z)
                            ]
                          [else
                            (add-to-worklist! Z)
                            ;; Kludge cause not complete grammar
                            (add-to-worklist! nu)
                            ])))
                    (set-dfa-eqvcl-touched! Z '()))
                  touched))))])

    (t 1)
    (let ([base-eqvcl (mk-empty-eqvcl)])
      (for-each (lambda (s) (add-to-eqvcl! base-eqvcl s))
                States))

    (t 2)
    (for-each
      (lambda (nts) (split (lambda (add-split!) (for-each add-split! nts))))
      seperate)

    
    (t 3)
    ;; ### HACK ###
    ;; Put all partitions on the worklist 
    '(for-each
       (lambda (eqvcl)
         (unless (dfa-eqvcl-on-worklist eqvcl)
           (add-to-worklist! eqvcl)))
       partition)
    ;; ### END HACK ###

    (t 4)
    (recur loop ()
      ;; partition a partition of States union Final
      ;; All equiv states in same eqvcl
      ;; If s1,s2 in eqvcl B with si->a.si', and si' in Bi
      ;; then either B1=B2, or (Bi,a) in worklist for one of i=1,2

      (unless (null? worklist)
        (let* ([X (car worklist)])
          (set! worklist (cdr worklist))
          (set-dfa-eqvcl-on-worklist! X #f)
          (inv-delta-traversal (eqvcl->list X) split)
          (loop))))
    (t 5)

    ;; All done
    ;; Return mapping from states to representative states,
    ;; and a mapping from states to a list of equiv states
    ;; and an equiv predicate on states
    ;; and a list of representative states

    (values
     (lambda (s) (dfa-eqvcl-states (state-eqvcl s)))
     (lambda (s) (eqvcl->list (state-eqvcl s)))
     (lambda (s1 s2) (eq? (state-eqvcl s1) (state-eqvcl s2)))
     (filter-map (lambda (X) 
                   (let ([rep (dfa-eqvcl-states X)])
                     (if (null? rep) #f rep)))
                 partition))))

;; ----------------------------------------------------------------------
;; Prototypical inputs

'(define-type dfa-state (box void))
'(MrSpidey-test
  (calc-equivalences (type: (listof dfa-state))
                     (type: (listof dfa-state))
                     (type: (_ (-> (list (dfa-state -> void) (-> void)))
                               -> void))
                     unbox
                     set-box!))
;(trace calc-equivalences)

