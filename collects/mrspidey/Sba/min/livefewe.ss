;; File: min-live-few-e.ss
;; Can optimize later when sure only want min-live-few-e-L
;; ======================================================================
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
;; CHANGES:
;;
;; (null? (Tvar-constraints tvar)) 
;; -> (andmap con-filter? (Tvar-constraints tvar))
;;
;; rhs-nts not filter w/ live-nt?
;;
;; ======================================================================
(define-typed-structure Tvar-class (parent L-unif U-unif children))

;; tvars is binary tree w/ tvar at leaves 

; ======================================================================
; minimize-constraints-live-few-e
;
; Input:  lower : list Tvar
;         upper : list Tvar

(define (copy-live-constraints-few-e L-unif U-unif)
  (lambda (lower upper)

    (pretty-debug-few 
      `(copy-live-constraints-few-e
         lower ,(map Tvar-name lower)
         upper ,(map Tvar-name upper)))

    ;; Calc edgefrom
    ;; Calculate live Tvar and live NT
    ;; Only use Tvar-orig-objs
    ;; Flag NTs with non-epsilon edges

    (let*-vals
      ( [t (lambda (s) 
             (when timing-min
               (min-record-progress (cons 'copy-live-constraints-few-e s))))]
        [_ (t 0)]
       
        ;; --- Calc empty and live
        ;; not empty == reached
        [(live-nts live-nt? live-tvars live-tvar? _ _ rhs-nts)
          (calc-live-tvars-nts lower upper #f)]
        [_ (t 1)]
        [_ (pretty-debug-few `(live-nts ,(map nt->sym live-nts)))]
        [_ (pretty-debug-few `(live-tvars ,(map Tvar-name live-tvars)))]
        [rhs-nts
          (lambda (nt)
            (filter (lambda (x) (or (eq? x #t) (live-nt? x))) (rhs-nts nt)))]

        ;; Now unify to equiv classes.
        ;; Define AV `a' to be L-unifiable iff 
        ;;     con(a) = 0     and 
        ;;     #outedge(a)=1  and
        ;;     # prods for LaU = 1
        ;;  But aU -> bU is not counted in rhs-nts(aU)
        ;;     => require rhs-nts(aU) = 0
        ;;
        ;; Define AV `b' to be U-unifiable iff
        ;;     AV(a) = AV(b) for all predecessors a of b
        ;;     #inedge(b)<=1  and
        ;;     # prods for LbL = 1
        ;;
        ;; If a <= b and either a L-unif or b U-unif, then unify

        ;; Unify to equivalence classes
        [(tvar-class set-tvar-class!) (alloc-Tvar-field)]
        [_ (t 3)]
        [_ (for-each 
             (lambda (tvar)
               (pretty-debug-few `(Tvar ,(Tvar-name tvar)))
               (let* ([pretty-and
                        (lambda args
                          (let ([r (andmap (lambda (x) x) args)])
                            (pretty-debug-few `((and ,@args) ,r))
                            r))]
                       [L-u (pretty-and
                              L-unif
                              (andmap
                                (match-lambda
                                  [($ con _ _ _ tvar) (not (live-tvar? tvar))]
                                  [($ con-filter) #t])
                                (Tvar-constraints tvar))
                              (let ([out 
                                      (append
                                        (filter-map
                                          (match-lambda
                                            [($ con) #f]
                                            [($ con-filter _ _ tvar)
                                              (and (live-tvar? tvar) tvar)])
                                          (Tvar-constraints tvar))
                                        (filter live-tvar? 
                                          (Tvar-edgeto tvar)))])
                                (or (null? out) (null? (cdr out))))
                              (let ([nt (Tvar-U tvar)])
                                (or (not (live-nt? nt))
                                  (let ([s (rhs-nts nt)])
                                    (pretty-debug-few
                                      `(rhs-nts 
                                         ,(map
                                            (lambda (nt)
                                              (or (eq? nt #t) (nt->sym nt)))
                                            s)))
                                    (null? s)
                                    ;;(or (null? s) (null? (cdr s)))
                                    )))
                              ; Ignore lower, upper
                              ; Already counted in rhs-nts
                              ;(not (memq tvar lower))
                              ;(not (memq tvar upper))
                              )]
                       [U-u 
                         (let ( [in (Tvar-edgefrom tvar)])
                           (pretty-and
                             U-unif
                             (or (null? in)
                               (and (null? (cdr in))
                                 (<= (length (Tvar-objs tvar))
                                   (length (Tvar-objs (car in))))))
                             (let ([nt (Tvar-L tvar)])
                               (or (not (live-nt? nt))
                                 (let ([s (rhs-nts nt)])
                                   (pretty-debug-few
                                     `(rhs-nts 
                                        ,(map
                                           (lambda (nt)
                                             (or (eq? nt #t) (nt->sym nt)))
                                           s)))
                                   (or (null? s) (null? (cdr s)))
                                   )))
                                        ;(not (memq tvar lower))
                                        ;(not (memq tvar upper))
                             ))])
                   
                 (set-tvar-class! tvar (make-Tvar-class #f L-u U-u tvar))))
             live-tvars)]
        [_ (t 4)]
        [get-class
          (lambda (tvar)
            (recur loop ([c (tvar-class tvar)])
              (assert (or (Tvar-class? (Tvar-class-parent c))
                        (eq? #f (Tvar-class-parent c))))
              (if (Tvar-class-parent c)
                (loop (Tvar-class-parent c))
                c)))]
        [_ (for-each
             (lambda (b)
               (for-each
                 (lambda (a)
                   (when (live-tvar? a)
                     (match (get-class a)
                       [(and cl-a
                          ($ Tvar-class _ L-unif-a U-unif-a kids-a))
                         (assert (eq? (Tvar-class-parent cl-a) #f) 
                           (Tvar-class-parent cl-a))
                         (match (get-class b)
                           [(and cl-b 
                              ($ Tvar-class _ L-unif-b U-unif-b kids-b))
                             (pretty-debug-few
                               `(epsilon ,(Tvar-name a)
                                  ,(Tvar-name b)
                                  ,L-unif-a
                                  ,L-unif-b
                                  ,U-unif-a
                                  ,U-unif-b))
                             (when (or L-unif-a U-unif-b)
                               ;; Unify
                               (let ([nu-cl
                                       (make-Tvar-class #f
                                         (and L-unif-a L-unif-b)
                                         (and U-unif-a U-unif-b)
                                         (cons kids-a kids-b))])

                                 (assert (eq? (Tvar-class-parent cl-a) #f) 2)
                                 (assert (eq? (Tvar-class-parent cl-b) #f))
                                 (set-Tvar-class-parent! cl-a nu-cl)
                                 (set-Tvar-class-parent! cl-b nu-cl)
                                 (assert (eq? (get-class a) (get-class b)))
                                 ))])])))
                 (Tvar-edgefrom b)))
             live-tvars)]
        [_ (t 4.5)]
        )
      ;; --- Have equiv classes, now copy
      ;; --- return arguments for copy-constraints-equiv!
      (values
        (append lower upper live-tvars)
        live-tvar?
        live-nts
        (lambda (tvar)
          (recur loop ([k (Tvar-class-children (get-class tvar))][a '()])
            (if (Tvar? k)
              (cons k a)
              (loop (car k) (loop (cdr k) a)))))
        (lambda (AV) (list AV))))))

;; ======================================================================

