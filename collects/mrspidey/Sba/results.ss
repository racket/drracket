; results.ss
; Shows results of set-based analysis
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

;--------------------------------------------------------------------
; Configuration

(define show-val-args #t)
(define show-elements #t)
(define show-AV #t)
(define show-edgeto #t)
(define show-constraint #t)

;--------------------------------------------------------------------
; Showing results

(define st:show (lambda l (for-each show-ftype (apply select l))))

(define show-ftype
                                        ; Show value of ftype
  (lambda (ftype)
    (let* ([num (FlowType-num ftype)]
            [arrowto (FlowType-arrowto ftype)]
            [arrowfrom (FlowType-arrowfrom ftype)]
            [var (FlowType-name ftype)]
            [done #f]
            [makespaces
              (lambda (s)
                (if done
                  (printf "~a [~a]" (padr "" 20) s)
                  (begin
                    (set! done #t)
                    (printf 
                      "~a [~a]" 
                      (padr (symbol->string var) 20)
                      s))))])
      (when (not (null? arrowto))
        (makespaces "arrowto")
        (for-each
          (lambda (to) (printf "| ~a " (FlowType-name to)))
          arrowto)
        (newline))
      (when (not (null? arrowfrom))
        (makespaces "arrowfrom")
        (for-each
          (lambda (from) (printf "| ~a " (FlowType-name from)))
          arrowfrom)
        (newline))
      (when (FlowType-type-annotation ftype)
        (makespaces "visable")
        (if (string? (FlowType-type-annotation ftype))
          (display (FlowType-type-annotation ftype)))
        (newline))
      (when (FlowType-values-ftype ftype)
        (makespaces "values-type")
        (printf "~s~n" (FlowType-name (FlowType-values-ftype ftype))))

      (if (Tvar? ftype)
        (let* ( [Tvar ftype]
                [objs (Tvar-objs Tvar)]
                [constraints (Tvar-constraints Tvar)]
                [edgeto (Tvar-edgeto Tvar)]
                [edgefrom (Tvar-edgefrom Tvar)])

          (if (and show-AV (not (null? (get-Tvar-objs Tvar))))
            (begin
              (makespaces "AV    ")
              (begin0
                (show-sba-objs (get-Tvar-objs Tvar))
                (newline)))
            '()) 
          (if (and show-AV (not (null? (Tvar-orig-objs Tvar))))
            (begin
              (makespaces "origAV")
              (begin0
                (show-sba-objs (Tvar-orig-objs Tvar))
                (newline)))
            '())
          (if #t
            (begin
              (makespaces "num   ")
              (printf "~s~n" (FlowType-num Tvar)))
            '())
          (if (and show-constraint (not (null? (Tvar-constraints Tvar))))
            (begin
              (makespaces "constr")
              ;; Show each constraint
              (map 
                (match-lambda 
                  [($ con num template field-no Tvar sign)
                    (printf "[~s ~s ~s -> ~s] "
                      (if (template? template)
                        (template-type template)
                        template)
                      field-no sign
                      (Tvar-name Tvar))]
                  [($ con-filter num ($ filter bool templates) Tvar)
                    (printf "[filter ~s ~s -> ~s] "
                      (map template-type templates)
                      bool (Tvar-name Tvar))]
                  [($ con-top-s)
                    (printf "[top-s] ")])
                (Tvar-constraints Tvar))
              (newline)
              '())
            '())
              
          (if (and show-edgeto (not (null? edgeto)))
            (begin
              (makespaces "edgeto")
              (map (lambda (to) 
                     (printf "| ~a " (Tvar-name to)))
                edgeto)
              (newline)
              edgeto)
            '())
          (if (and show-edgeto (not (null? edgefrom)))
            (begin
              (makespaces "edgefr")
              (map (lambda (from) 
                     (printf "| ~a " (Tvar-name from)))
                edgefrom)
              (newline)
              edgefrom)
            '())
          (for-each
            (lambda (name get-nt)
              (if (and (get-nt Tvar)
                    (not (null? (get-nt Tvar)))
                    (not (null? (NT-rhs* (get-nt Tvar)))))
                (begin
                  (makespaces name)
                  (for-each (match-lambda
                              [($ rhs grsym nt)
                                (printf "| ~a ~a "
                                  (grsym->rep grsym)
                                  (if (null? nt)
                                    '()
                                    (nt->sym nt)))])
                    (NT-rhs* (get-nt Tvar)))
                  (newline))
                '()))
            (list "L     " "U     " "PL    " "PU    "
              )
            (list Tvar-L Tvar-U Tvar-PL Tvar-PU
              )))

        (begin
          (makespaces "fo-Atype")
          (printf "~s~n" (FlowType->pretty ftype)))))))

(define show-sba-objs
  (lambda (objs)
    (recur loop2 ([objs objs][dep '()])
      (match objs
        [()  dep]
        [(AV . rest)
          (match-let
            ([(print-representation . new-dep)
               (print-rep-AV AV)])
            (printf "| ~s ~a " (AV-num AV) print-representation)
            (loop2 rest (append new-dep dep)))]))))

(define print-rep-AV
  (match-lambda
    [($ AV _ template misc fields+ fields-)
      (cons
        (list
          (template-type template)
          misc
          (map Tvar-name (vector->list fields+))
          (map Tvar-name (vector->list fields-)))
        '())]))

; ----------------------------------------------------------------------

(define select
  (lambda l
    (if (null? l)
      list-ftype
      ;; list of names and nums
      (apply append
        (map
          (lambda (v)
            (filter-map
              (lambda (Tvar)
                (if (or (and (number? v) 
                          (= (FlowType-num Tvar) v)))
                  Tvar
                  #f))
              list-ftype))
          l)))))

; ----------------------------------------------------------------------

(define (prods-NT nt)
  (let ([named #f])
    (for-each
      (match-lambda
        [($ rhs* grsym nt)
          (printf "~a ->  ~a ~s~n" 
            (padr (if named 
                    ""
                    (begin
                      (set! named #t)
                      (symbol->string (nt->sym nt))))
              20)
            (padr (grsym->rep grsym) 25)
            (nt->sym nt))])
      (NT-rhs* nt))))

(define (prods-Tvar Tvar)
  ;;(printf "Tvar: ~s~n" (Tvar-name Tvar))
  (for-each
    (lambda (nt) 
      (if (NT? nt) 
        (prods-NT nt)
        (unless (null? nt)
          (printf "Warning: Bad nt ~s~n" nt))))
    (list (Tvar-L Tvar) (Tvar-LI Tvar) (Tvar-U Tvar) (Tvar-UI AVS))))

(define (st:prods . l) (for-each prods-Tvar (apply select l)))

; ----------------------------------------------------------------------

(define show-stat
  (lambda ()
    (printf "STATISTICS:~n")
    (let*
      ( [list-ftype (filter Tvar? list-ftype)]
        [numTvar (length list-ftype)]
        [avg (lambda (x) (/ (truncate (* (/ x numTvar) 100)) 100.0))]
        [show (lambda (name f)
                (let* ([l (map f list-ftype)]
                        [num (apply + l)]
                        [avg (avg num)]
                        [min (apply min l)]
                        [max (apply max l)])
                  (printf "   ~a: Total ~s Avg ~s min ~s max ~s~n"
                    name num avg min max)))])
      (show "edgeto  " (lambda (Tvar) (length (Tvar-edgeto Tvar))))
      ;;(show "edgefrom" (lambda (Tvar) (length (Tvar-edgefrom Tvar))))
      (show "con     " (lambda (Tvar) (length (Tvar-constraints Tvar))))
      (show "AV      " 
        (lambda (Tvar)
          (let ([objs (Tvar-objs Tvar)])
            (if (Tvar? objs) 0 (length (Tvar-objs Tvar))))))
      (printf "num-AV  : ~s~n" num-AV)
      (printf "num-Tvar : ~s~n" num-ftype)
      (printf "num-AV-a: ~s~n" num-AV-in-Tvar)
      ;;(printf "Tvar-tmp : ~s~n" counter-Tvar-tmp)
      (match (hash-table-info)
        [(size entries clashes)
          (printf "Hashtbl : size ~s entries ~s clashes ~s~n" 
            size entries clashes)])
      )))

(define show-stat-small
  (lambda ()
    (printf 
      "STATS: Size ~s HWM ~s num-Tvar ~s num-AV ~s num-con ~s num-edge ~s num-AV-a ~s~n"
      (constraint-system-size) 
      max-constraint-system-size
      num-ftype num-AV num-con
      num-edge num-AV-in-Tvar)
    (constraint-system-size)))

(define (empty-vars)
  (filter
    (lambda (Tvar)
      (if (null? (get-Tvar-objs Tvar))
        (Tvar-name Tvar)
        #f))
    list-ftype))
