; min.ss
; Purpose: Minimize a set of constraints.
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
; minimize-constraints-&-compare
; minimizes a constraint system
; Input:  which : 'live or 'dfa-min
;         lower : list Tvar
;         upper : list Tvar
;         l1 ... l2 : ftypes in uncompresssed set of constraints

; Returns (list-ftype Tvar->nuTvar)
;; ----------------------------------------

(define global-lower (void))
(define global-upper (void))
(define global-all-which (void))

(define (minimize-constraints-&-compare all-which lower upper l1 l2)
  (set! global-lower lower)
  (set! global-upper upper)
  (set! global-all-which all-which)
  (when (st:compare-min-algs)
    ;; Compare algorithms
    (let* ( [alg-width 32]
            [calc-sizes
             (lambda (rep)
               (pretty-debug-min2 `(calc-sizes ,(map Tvar-name rep)))
               (let*-vals
                 ( [n-f (length rep)]
                   [AV-added (apply append (map get-Tvar-objs rep))]
                   [n-AV-added (length AV-added)]
                   [n-AV (length (list->set AV-added))]
                   [n-edge (length (apply append (map Tvar-edgeto rep)))]
                   [n-con (length (apply append (map Tvar-constraints rep)))]
                   [size (+ n-edge n-con n-AV-added)])
                 (list size n-f n-AV n-AV-added n-edge n-con)))]
            [show-sizes
              (lambda (alg orig-size t size n-f n-AV n-AV-added n-edge n-con)
                (if (zero? size)
                  (error 'minimize-constraints-&-compare
                     "Algorithm ~s returns empty constraint system~n" alg)
                  (printf 
                    "~a: ~a ~a ~a ~a ~a ~a ~a ~a~n"
                    (padr alg alg-width)
                    (padl
                      (/ (round (* (exact->inexact (/ orig-size size)) 10)) 10)
                      6)
                    (padl size 5)
                    (padl n-f 5)
                    (padl n-AV 3)
                    (padl n-AV-added 5)
                    (padl n-edge 5)
                    (padl n-con 4)
                    (padl t 5))))]
            [orig-sizes (calc-sizes (filter Tvar? (get-prefix l1 l2)))]
            [orig-size (car orig-sizes)])
      '(pretty-debug `(minimize-constraints-&-compare 
                        ,(map Tvar-name lower) ,(map Tvar-name upper)))
      (printf "~n~a:  factor  size  Tvar  AV  AV-a  edge  con time~n" 
        (padr "Algorithm" alg-width))
      (apply show-sizes "" orig-size 0 orig-sizes)
      (for-each
        (lambda (p)
          (let*-vals
            ( [thunk 
                (lambda ()
                  (let-values
                    ([(rep _) 
                       (minimize-constraints p lower upper l1 l2)])
                    rep))]
              [(rep t real-t) (time-apply thunk)]
              [sizes (calc-sizes rep)])
            (apply show-sizes p orig-size t sizes)))
        min-algs-to-compare)))

  (minimize-constraints all-which lower upper l1 l2))

;; ----------

(define min-algs-to-compare
  '(                                    ;(none)
     (nonempty)
     (nonempty live)
     (live)
     ;(live-no-epsilon)
     (live-few-e)
                                        ;(live-few-e-L)
                                        ;(live-few-e-U)
     ;(live-no-epsilon dfa-min)
     ;(live-no-epsilon dfa-min-inv)
     ;(live-few-e dfa-min)
     ;(live-few-e dfa-min-inv)
     ;(live-few-e dfa-min-inv dfa-min)
     ;(live-few-e dfa-min dfa-min-inv)

     ;(live-no-epsilon dfa-min-1)
     ;(live-no-epsilon dfa-min-2)

     ;(live-few-e dfa-min-1)
     ;(live-few-e dfa-min-2)
     ;(live-few-e dfa-min-1 live-few-e dfa-min-2)
     ;(live-few-e dfa-min-2 live-few-e dfa-min-1)

     (dfa-min-lub)
     (dfa-min-glb)
     (dfa-min-lub dfa-min-glb)
     (dfa-min-glb dfa-min-lub)
     (dfa-min-lub dfa-min-glb dfa-min-lub)
     (dfa-min-glb dfa-min-lub dfa-min-glb)
     ;(live-few-e dfa-min-1 live-no-epsilon dfa-min-2 live-no-epsilon dfa-min-1)
     ;(live-few-e dfa-min-2 live-no-epsilon dfa-min-1 live-no-epsilon dfa-min-2)
                                        ;(live-no-epsilon min-table)
     ))

;; ----------------------------------------------------------------------

(define min-time-base 0)
(define min-time-record '())
(define (min-record-progress what)
  (set! min-time-record
    (cons (cons what (current-process-milliseconds)) min-time-record)))
(define (show-record-progress)
  (pretty-debug
    (map (match-lambda
           [(what . when) (list what (- when min-time-base))])
      (reverse min-time-record))))
;; ----------

(define (minimize-constraints all-which lower upper l1 l2)
  (pretty-debug-min2
    `(input 
       all-which ,all-which
       lower ,(map Tvar-name lower)
       upper ,(map Tvar-name upper)))

  (set! min-time-record '())
  (set! min-time-base (current-process-milliseconds))
  (recur loop ( [which all-which]
                [lower lower]
                [upper upper]
                [Tvar->nuTvar (lambda (x) x)])
    (match which
      [(which . (and rest (_ . _)))
        (let*-vals
          ([(rep-Tvar1 Tvar->nuTvar1)
             (minimize-constraints-nocount which lower upper l1 l2)])
          (loop rest 
            (map Tvar->nuTvar1 lower)
            (map Tvar->nuTvar1 upper)
            (lambda (Tvar) (Tvar->nuTvar1 (Tvar->nuTvar Tvar)))))]
      [(or (which) (? symbol? which))
        (let*-vals
          ([(rep-Tvar Tvar-nuTvar1) 
             (minimize-constraints-nocount which lower upper l1 l2)])

          (when timing-min (show-record-progress))

          (pretty-debug-min2
            `(output lower ,(map Tvar-name (map Tvar-nuTvar1 lower))
               upper ,(map Tvar-name (map Tvar-nuTvar1 upper))))

          (values 
            rep-Tvar
            (lambda (Tvar) (Tvar-nuTvar1 (Tvar->nuTvar Tvar)))))])))

;; ----------------------------------------------------------------------

(define (minimize-constraints-nocount which lower upper l1 l2)
  (pretty-debug-min
    `(minimize-constraints-nocount which ,which
       lower ,(map Tvar-name lower)
       upper ,(map Tvar-name upper)))
  (check-kernel-ok)
  (pretty-debug-min `(minimize-constraints-nocount kernel-ok))
  (let*-values
    ([(A B)
       (case which
         [(none)             (values
                             (filter Tvar? (get-prefix l1 l2))
                             (lambda (Tvar) Tvar))]
         [(nonempty)         (let*-vals
                             ([(live-tvars _ _) 
                                (find-nonempty-tvars lower upper)])
                             (values 
                               live-tvars                             
                               (lambda (tvar) tvar)))]
         [(nonempty-copy)    (copy-nonempty-tvars lower upper)]
         [(live)             (copy-live-constraints lower upper)]
         [(live-no-epsilon)  (copy-live-constraints-noe lower upper)]

         [(live-few-e)
           (call-with-values
             (lambda () ((copy-live-constraints-few-e #t #t) lower upper))
             copy-constraints-equiv!)]
         [(live-few-e-L)
           (call-with-values
             (lambda () ((copy-live-constraints-few-e #t #f) lower upper))
             copy-constraints-equiv!)]
         [(live-few-e-U)
           (call-with-values
             (lambda () ((copy-live-constraints-few-e #f #t) lower upper))
             copy-constraints-equiv!)]
         ;[dfa-min          (minimize-constraints-dfa-min lower upper)]
         ;[dfa-min-inv      (minimize-constraints-dfa-min-inv lower upper)]
         ;[dfa-min-1        (minimize-constraints-dfa-min-1 lower upper)]
         ;[dfa-min-2        (minimize-constraints-dfa-min-2 lower upper)]
         [(dfa-min-lub)      (minimize-constraints-dfa-min-lub lower upper)]
         [(dfa-min-glb)      (minimize-constraints-dfa-min-glb lower upper)]
         ;[min-table        (min-table lower upper)]
         [else (error 'minimize-constraints-nocount "Bad which ~s" which)])])
    (check-kernel-ok)
    (pretty-debug-min `(minimize-constraints-nocount kernel-ok2))
    (min-record-progress 'minimize-constraints-nocount-returning)
    (values A B)))

;; ======================================================================




