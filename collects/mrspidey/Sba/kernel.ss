;; kernel.ss
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
;; ----------------------------------------------------------------------=
;; FlowType == value flow graph node

(define-const-typed-structure
  FlowType
  ( (: num num)
    (! expr (union zodiac:parsed string false symbol))
    (! arrowto (listof FlowType))
    (! arrowfrom (listof FlowType))
    (! type-annotation any) ;; type-annotation% or file.ss (if from .za file)
    (! proplist (listof (cons sym any)))
    (! values-ftype (union false FlowType))
    ))

 (define num-ftype  0)
 (define list-ftype '())
 (define num-edge 0)

 (define (init-FlowType!)
   (set! num-ftype  0)
   (set! list-ftype '())
   (set! num-edge 0))

 ;;------------------------------------------------------------
;; Extension functions

(define (add-FlowType-arrow! from to)
  (set-FlowType-arrowto! from (cons to (FlowType-arrowto from)))
  (set-FlowType-arrowfrom! to (cons from (FlowType-arrowfrom to))))

(define (add-FlowType! ftype)
  (set! num-ftype (add1 num-ftype))
  (set! list-ftype (cons ftype list-ftype))
  ftype)

;;------------------------------------------------------------
;; Property list stuff
      
(define (add-FlowType-prop! ftype prop val)
  (set-FlowType-proplist! ftype 
                          (cons (cons prop val) (FlowType-proplist ftype))))

(define (get-FlowType-prop ftype prop default)
  (recur loop ([l (FlowType-proplist ftype)])
    (match l
      [() default]
      [((a . d) . rest)
       (if (eq? a prop)
           d
           (loop rest))]
      [_ default])))

(define (FlowType-name ftype)
  (symbol-append (get-FlowType-prop ftype 'symbol 'anon)
    ':
    (FlowType-num ftype)))

;; ======================================================================
;; ======================================================================

(define-typed-structure (Tvar struct:FlowType) 
  ( (: objs (listof AV))
    (: orig-objs (listof AV))
    (: constraints (listof (union con con-filter)))
    (: leq-top-s bool)
    (: edgeto (listof Tvar))
    (: edgefrom (listof Tvar))
    (: L NT)
    (: U NT)
    (: PL NT)
    (: PU NT)
    ))

(define-const-typed-structure AV 
  ((: num num)
   (: template template)
   misc
   (: fields+ (vec Tvar))
   (: fields- (vec Tvar))
   (! U NT)
   (! PU any)
    wb
))

(define-const-typed-structure template
  ( (: type   sym)
    (: num+ int)
    (: num- int)
    (: ref (vec num))
    (: assign (vec num))
    (! super-templates (listof template))
    (: misc-eq? (any -> any))))

 ;; ref and assign are for constructors
 ;; ref maps a constructor-field to the AV-field+ to get value from
 ;; assign maps a constructor-field to the AV-field- to add value to,
 ;; or #f if immutable
 ;; super-templates field records that eg num is a supertype of apply+
 ;; misc-eq? is an equality function on the misc field of AVs
 ;; sym-ndx maps symbols to the index for that symbol
 ;;  eg. map ivar names


 ;; Each elem of AV-fields is an Tvar

(define-const-typed-structure con
  ( (: num      num)
    (: template template)
    (: field-no num)
    (: tvar     Tvar)
    (: sign     bool)
    misc))

(define-const-typed-structure con-filter
  ((: num    num)
   (: filter filter)
   (: tvar    Tvar)))

(define-const-typed-structure filter
  ((: sign      bool)
   (: templates (listof template))))

(define (create-filter sign templates)
  (assert (and (boolean? sign)
            (list? templates)
            (andmap template? templates))
    'create-filter sign templates)
  (make-filter sign templates))

;; Says whether to include or exclude certain templates

(define-const-typed-structure con-top-s
  ())

(define con-top-s (make-con-top-s))

(define mt-vector (vector))
;----------------------------------------------------------------------

 (define num-con  0)  
 (define num-AV   0)
 (define num-AV-in-Tvar 0)
 (define max-constraint-system-size 0)
 (define (constraint-system-size) (+ num-edge num-con num-AV-in-Tvar))

 (define (SBA-entry->hash entry)
   (hash-fn (FlowType-num (car entry))
            (let ([d (cdr entry)])
              (if (Tvar? d)
                  (FlowType-num d)
                  (AV-num d)))))

 (define (init-kernel!)
   (init-FlowType!)
   (set! num-con  0)
   (set! num-AV   0)
   (set! num-AV-in-Tvar 0)
   (set! max-constraint-system-size 0)
   (init-hash-table SBA-entry->hash 0)
   )

 ;;------------------------------------------------------------
 ;; Creation functions

 (define (create-AV template misc fields+ fields-) 
   (assert (and 
             (template? template)
             (vector? fields+)
             (vector? fields-)
             (vector-andmap Tvar? fields+)
             (vector-andmap Tvar? fields-))
           `(create-AV ,template ,misc ,fields+ ,fields-))
   (let ([AV (make-AV num-AV template misc fields+ fields- #f #f (make-weak-box 1))])
     (set! num-AV (add1 num-AV))
     AV))

 (define mk-Tvar-nolist
   (lambda (sym)
     (let ([tvar (make-Tvar
                   ;; FlowType fields
                   num-ftype #f '() '() #f `((symbol . ,sym)) #f 
                   ;; Tvar fields
                   '() '() '() #f '() '()
                   #f #f #f #f
                   )])
       (set! num-ftype (add1 num-ftype))
       tvar)))

 (define mk-Tvar
   (lambda (sym)
     (let ([tvar (mk-Tvar-nolist sym)])
       (set! list-ftype (cons tvar list-ftype))
       tvar)))

 (define (create-con template field-no tvar sign) 
   (create-con-misc template field-no tvar sign '()))

 (define (create-con-misc template field-no tvar sign misc) 
   (make-con num-con template field-no tvar sign misc))

 (define (create-con-filter filter tvar) 
   (assert (filter? filter))
   (make-con-filter num-con filter tvar))
      
;; ----------------------------------------------------------------------
;; Functions for extending graph
;; the add-* functions extend the data structures, but don't propogate
;; the check-add-* only adds if item not in data structure, but don't propogate
;; the extend-* functions also propogate values & constraints appropriately
;; the new-* functions are normally bound to extend-*, but sometines add-*
;; ----------------------------------------------------------------------

(define add-edge!
   (lambda (from to)
     (assert (and (Tvar? from) (Tvar? to)) 'add-edge! from to)
     (set-Tvar-edgeto! from (cons to (Tvar-edgeto from)))
     (set-Tvar-edgefrom! to (cons from (Tvar-edgefrom to)))
     (set! num-edge (add1 num-edge))
     (add-entry (hash-fn (FlowType-num from) (FlowType-num to))
                (cons from to))))
      
(define add-AV!
  (lambda (tvar AV)
    (set! num-AV-in-Tvar (add1 num-AV-in-Tvar))
    (set-Tvar-objs! tvar (cons AV (Tvar-objs tvar)))
    (add-entry (hash-fn (FlowType-num tvar) (AV-num AV))
      (cons tvar AV))))

(define add-con!
  (lambda (tvar con)
    (set! num-con (add1 num-con))
    (set-Tvar-constraints! tvar (cons con (Tvar-constraints tvar)))))

'(define add-nohash-con! add-con!)

;; ----------------------------------------------------------------------
;; the check-add-* functions check if a constraint is present,
;; and if not, add it, but do not propogate
 
(define check-add-edge!
   (lambda (from to)
     (assert (and (Tvar? from) (Tvar? to)) 'extend-edge! from to)
     (unless
       (or (Tvar-edge? from to) (eq? from to))
       (add-edge! from to))))

 (define check-add-AV!
   (lambda (tvar AV)
     (assert (AV? AV) `(extend-AV! ,tvar ,AV))
     (set-Tvar-orig-objs! tvar (cons AV (Tvar-orig-objs tvar)))))

 (define check-add-con!
   (lambda (tvar con)
     (set! num-con (add1 num-con))
     (set-Tvar-constraints! tvar (cons con (Tvar-constraints tvar)))
     (match con
       [($ con-filter _ _ dest)
         (set-Tvar-edgefrom! dest (cons tvar (Tvar-edgefrom dest)))]
       [_ (void)])))

;; ----------------------------------------------------------------------
;; the extend-* functions also propogate values & constraints appropriately
 
(define extend-edge!
   (lambda (from to)
     (assert (and (Tvar? from) (Tvar? to)) 'extend-edge! from to)
     (unless
       (or (Tvar-edge? from to) (eq? from to))
       (add-edge! from to)
       ;; Propogate all AV's
       (for-each (lambda (AV) (prop-AV! to AV))
                 (Tvar-objs from)))))

(define extend-AV!
  (lambda (tvar AV)
    (assert (AV? AV) `(extend-AV! ,tvar ,AV))
    (set-Tvar-orig-objs! tvar (cons AV (Tvar-orig-objs tvar)))
    (prop-AV! tvar AV)))

(define prop-AV!
  (lambda (tvar AV)
    (assert (and (Tvar? tvar) (AV? AV)) `(prop-AV! ,tvar ,AV))
    (unless (Tvar-AV-mem? tvar AV)
      (add-AV! tvar AV)
      ;; Apply all constraints
      (for-each 
        (lambda (con) (SBA-constraint tvar con AV))
        (Tvar-constraints tvar))
      ;; Propogate
      (for-each
        (lambda (to) (prop-AV! to AV))
        (Tvar-edgeto tvar)))))

 (define extend-con!
   (lambda (tvar con)
     (set! num-con (add1 num-con))
     (set-Tvar-constraints! tvar (cons con (Tvar-constraints tvar)))
     (match con
       [($ con-filter _ _ dest)
         (set-Tvar-edgefrom! dest (cons tvar (Tvar-edgefrom dest)))]
       [_ (void)])
     ;; Apply to all AV's
     (for-each (lambda (AV) (SBA-constraint tvar con AV))
               (get-Tvar-objs tvar))))

;; ----------------------------------------------------------------------
;; the new-* functions are normally equiv to extend-*,
;; but can be set to add-* via a parameter 

(define new-edge! extend-edge!)
(define new-AV!   extend-AV!)
(define new-con!  extend-con!)

(define (new-leq-top-s! tvar)
  ;; tvar <= top-s, is constraint
  (unless (Tvar-leq-top-s tvar)
    (set-Tvar-leq-top-s! tvar #t)
    (new-con! tvar con-top-s)))

(define (new-geq-top-s! tvar)
  ;; top-s <= tvar, is AV
  (new-AV! tvar AV-top-s))

;; ----------------------------------------------------------------------

(define keep-S-closed
  (let ([closed #t])
    (case-lambda
      [() closed]
      [(x)
        (set! closed x)
        (if x
          (begin
            (set! new-edge! extend-edge!)
            (set! new-AV! extend-AV!)
            (set! new-con! extend-con!))
          (begin
            (set! new-edge! check-add-edge!)
            (set! new-AV! check-add-AV!)
            (set! new-con! check-add-con!)))])))
    
(define new-create-AV!
  (lambda (tvar template misc fields)
    (new-AV! tvar (create-AV template misc fields))))

(define new-bidir-edge! 
  (lambda (from to)
    (new-edge! from to)
    (new-edge! to from)))

(define new-edge-para
  (case-lambda
    [() new-edge!]
    [(x) (set! new-edge! x)]))

(define (close-constraints tvars)
  (pretty-debug `(close-constraints ,(map Tvar-name tvars)))
  (for-each
    (lambda (tvar)
      (for-each
        (lambda (AV)
          (for-each
            (lambda (to) (prop-AV! to AV))
            (Tvar-edgeto tvar))
          (for-each
            (lambda (con) (SBA-constraint tvar con AV))
            (Tvar-constraints tvar)))
        (get-Tvar-objs tvar)))
    tvars))

 ;;------------------------------------------------------------
 ;; Handling constraints

(define gSBA-constraint (void))

(define SBA-constraint
  (lambda (tvar con AV)
    (set! gSBA-constraint (list tvar con AV))
    (match con
      ;; Regular constraints
      [($ con _ template field-no tvar-con sign)
        (match AV
          [($ AV _ template2 misc fields+ fields-)
            (when (or 
                    (eq? template template2)
                    (memq template (template-super-templates template2)))
              ;;(pretty-print `(,con ,AV))
              (if sign
                (when (< field-no (vector-length fields+))
                  ;; Propogate field from AV into tvar-con
                  (new-edge! (vector-ref fields+ field-no) tvar-con))
                (when (< field-no (vector-length fields-))
                  ;; Propogate field from tvar-con into AV
                  (new-edge! tvar-con (vector-ref fields- field-no)))))
            (when (eq? template2 template-top-s)
              (if sign
                (new-geq-top-s! tvar-con)
                (new-leq-top-s! tvar-con)))])]

      ;; Filter constraints
      [($ con-filter _ ($ filter bool templates) tvar-con)
        '(printf "template ~s templates ~s memq ~s bool ~s add ~s~n" 
           (template-type (AV-template AV))
           (map template-type templates)
           (memq (AV-template AV) templates)
           bool
           (case (memq (AV-template AV) templates)
             [#f (not bool)]
             [else bool]))
        (let* ( [AV-t (AV-template AV)]
                [found (ormap
                         (lambda (t2)
                           (or 
                             (eq? AV-t t2)
                             (memq t2 (template-super-templates AV-t))))
                         templates)])
          '(pretty-print-debug
             `(con-filter found ,found 
                memq ,(memq AV-t templates)
                AV-t ,(template-type AV-t)
                ,(map template-type templates)))
          (when (or (case found
                      [(#f) (not bool)]
                      [else bool])
                  (eq? AV-t template-top-s))
            ;; Add AV to tvar-con
            (prop-AV! tvar-con AV)))]
      [($ con-top-s)
        (match AV
          [($ AV _ template2 misc fields+ fields-)
            (vector-for-each new-leq-top-s! fields+)
            (vector-for-each new-geq-top-s! fields-)])])))
      
 ;;------------------------------------------------------------
 ;; Functions/predicates for examining the graph

 (define (Tvar-AV-mem? tvar AV)
   (hash-find (hash-fn (FlowType-num tvar) (AV-num AV))
              (lambda (entry) 
                (and (eq? (car entry) tvar)
                     (eq? (cdr entry) AV)))))

 (define (Tvar-edge? from to)
   (hash-find (hash-fn (FlowType-num from) (FlowType-num to))
              (lambda (entry)
                (and (eq? (car entry) from)
                     (eq? (cdr entry) to)))))
      
 (define get-Tvar-objs Tvar-objs)
      
;; ------------------------------------------------------------

(define (really-check-kernel-ok)
  (dynamic-let 
    ([st:check-kernel #t]) 
    (check-kernel-ok)))

(define check-kernel-ok:tvar #f)

(define (check-kernel-ok)
  ;; Sanity check
  (when (st:check-kernel)
    (assert (= num-ftype (length list-ftype)))
    (let ([Tvar-edge?
            (lambda (a b)
              (or (eq? a b)
                (Tvar-edge? a b)))])

      (printf "check-kernel-ok: Consistency tests~n")
      (for-each
        (lambda (tvar)
          (when (Tvar? tvar)
            (assert (= (length (Tvar-objs tvar))
                      (length (list->set (Tvar-objs tvar)))))
            (assert (= (length (Tvar-edgeto tvar))
                      (length (list->set (Tvar-edgeto tvar)))))
            (assert (= (length (Tvar-constraints tvar))
                      (length (list->set (Tvar-constraints tvar)))))
            (for-each (lambda (AV) (assert (Tvar-AV-mem? tvar AV) (Tvar-name tvar)))
              (Tvar-objs tvar))
            (for-each (lambda (to) (assert (Tvar-edge? tvar to)))
              (Tvar-edgeto tvar))))
        list-ftype)
   
      ;; Now check kernel is closed under S
      (printf "check-kernel-ok: Closure tests~n")
      (for-each
        (lambda (tvar)
          (when (Tvar? tvar)
            (for-each 
              (lambda (AV)
                ;; First check AV prop'd
                (for-each 
                  (lambda (to)
                    (when (Tvar? to)
                      (assert (Tvar-AV-mem? to AV)
                        (Tvar-name tvar) (Tvar-name to)
                        (template-type (AV-template AV)))))
                  (Tvar-edgeto tvar))
                ;; Check AV applied to all constraints
                (for-each
                  (match-lambda
                    [($ con _  template field-no tvar-con sign)
                      (match AV
                        [($ AV _ template2 misc fields+ fields-)
                          (when 
                            (or 
                              (eq? template template2)
                              (memq template (template-super-templates template2)))
                            (if sign
                              (when (< field-no (vector-length fields+))
                                ;; Propogate field from AV into tvar-con
                                (assert
                                  (Tvar-edge?
                                    (vector-ref fields+ field-no) tvar-con)
                                  `(Tvar-edge?
                                     ,(Tvar-name (vector-ref fields+ field-no))
                                     ,(Tvar-name tvar-con))
                                  (Tvar-name tvar) field-no
                                  (template-type template)))
                              (when (< field-no (vector-length fields-))
                                ;; Propogate field from tvar-con into AV
                                (assert
                                  (Tvar-edge?
                                    tvar-con (vector-ref fields- field-no))
                                  (Tvar-name tvar) 
                                  field-no
                                  (Tvar-name tvar-con)
                                  sign
                                  (template-type template)))))]

                        [_ 
                          ;; Constraint does not apply to this AV
                          (void)])]
                    [($ con-filter _ ($ filter bool templates) tvar-con)
                      ;; ignore for now
                      (void)])
                  (Tvar-constraints tvar)))
              (get-Tvar-objs tvar))))
        (reverse list-ftype))

      ;; Now check all reachable tvars are in list-ftype
      (printf "check-kernel-ok: list-ftype tests~n")
      (let*-vals 
        ( [(get set) (alloc-Tvar-field (lambda () #f))]
          [ok-tvar? (lambda (tvar)
                      (unless (get tvar)
                        (printf
                          "Tvar ~s reachable from ~s but not in list-ftype ~s~n"
                          (FlowType-num tvar)
                          (FlowType-num check-kernel-ok:tvar)
                          (memq tvar list-ftype))))]
          [ok-AV? 
            (match-lambda
              [($ AV _ _ _ fields+ fields-)
                (vector-for-each ok-tvar? fields+)
                (vector-for-each ok-tvar? fields-)])])

        (for-each
          (lambda (ftype)
            (when (Tvar? ftype) (set ftype #t)))
          list-ftype)

        (for-each
          (lambda (ftype)
            (when (Tvar? ftype)
              (set! check-kernel-ok:tvar ftype)
              (for-each ok-tvar? (Tvar-edgeto ftype))
              (for-each ok-tvar? (Tvar-edgefrom ftype))
              (for-each ok-AV?   (Tvar-objs ftype))
              (for-each ok-AV?   (Tvar-orig-objs ftype))
              (for-each
                (match-lambda
                  [($ con _ _ _ tvar) (ok-tvar? tvar)]
                  [($ con-filter _ _ tvar) (ok-tvar? tvar)])
                (Tvar-constraints ftype))))
          list-ftype))

      )))

(define (check-unreachable ftypes unreachable)
  (let* ( [check-ok
            (lambda (ftype) 
              (when (memq ftype unreachable)
                (error 'check-unreachable "Ftype ~s in old ~s"
                  (FlowType-name ftype)
                  (map FlowType-name unreachable))))])
    (for-each
      (lambda (ftype)
        (when (Tvar? ftype)
          (for-each check-ok (Tvar-edgeto ftype))
          (for-each check-ok (Tvar-edgefrom ftype))
          (for-each
            (match-lambda
              [($ AV _ _ _ fields+ fields-)
                (vector-for-each check-ok fields+)
                (vector-for-each check-ok fields-)])
            (Tvar-objs ftype))
          (for-each
            (match-lambda
              [($ con _ _ _ tvar) (check-ok tvar)]
              [($ con-filter _ _ tvar) (check-ok tvar)])
            (Tvar-constraints ftype))))
      ftypes)))

 ;; ------------------------------------------------------------

(define-structure (kernel-state
                    num-ftype list-ftype num-edge 
                    num-con num-AV num-AV-in-Tvar 
                    closed? new-edge! hash-table-state))
                    
(define (save-kernel-state)
  (make-kernel-state
    num-ftype list-ftype num-edge 
    num-con num-AV num-AV-in-Tvar 
    (keep-S-closed) new-edge! 
    (capture-hash-table-state)))

(define restore-kernel-state!
  (match-lambda
    [($ kernel-state
       saved-num-ftype saved-list-ftype saved-num-edge 
       saved-num-con saved-num-AV saved-num-AV-in-Tvar 
       closed? saved-new-edge! 
       hash-table-state)
      (let ([old-size (constraint-system-size)])
        (set! num-ftype saved-num-ftype)
        (set! list-ftype saved-list-ftype)
        (set! num-edge saved-num-edge)
        (set! num-con saved-num-con)
        (set! num-AV saved-num-AV)
        (set! num-AV-in-Tvar saved-num-AV-in-Tvar)
        (keep-S-closed closed?)
        (set! new-edge! saved-new-edge!)
        (restore-hash-table-state! hash-table-state)

        (set! max-constraint-system-size
          (max max-constraint-system-size
            (+ (constraint-system-size) old-size)))
        )]))

(define free-kernel-state!
  (match-lambda
    [($ kernel-state
       saved-num-ftype saved-list-ftype saved-num-edge 
       saved-num-con saved-num-AV saved-num-AV-in-Tvar 
       closed? saved-new-edge! 
       hash-table-state)

      (free-hash-table-state! hash-table-state)
      (for-each 
        (lambda (ftype)
          (set-FlowType-expr! ftype 'zerod1!)
          (set-FlowType-arrowto! ftype 'zerod2!)
          (set-FlowType-arrowfrom! ftype 'zerod3!)
          (set-FlowType-type-annotation! ftype 'zerod4!)
          (set-FlowType-proplist! ftype 'zerod5!)
          (set-FlowType-values-ftype! ftype 'zerod6!)
          (when (Tvar? ftype) 
            (set-Tvar-objs! ftype 'zerod7!)
            (set-Tvar-orig-objs! ftype 'zerod8!)
            (set-Tvar-constraints! ftype 'zerod9!)
            (set-Tvar-edgeto! ftype 'zeroda!)
            (set-Tvar-edgefrom! ftype 'zerodb!)
            (set-Tvar-L! ftype 'zerodc!)
            (set-Tvar-U! ftype 'zerodd!)
            (set-Tvar-PL! ftype 'zerode!)
            (set-Tvar-PU! ftype 'zerodf!)))
        saved-list-ftype)]))

;; ----------------------------------------------------------------------

(define-structure (prompt-kernel-state saved-state prompt-hash-state ftypes))

(define (prompt-kernel-state)
  (make-prompt-kernel-state
    (save-kernel-state)
    (prompt-hash-table-state)
    (map
      (lambda (ftype)
        (list 
          ftype
          (match ftype
            [($ FlowType _ _ arrowto arrowfrom _ _ values-ftype)
              (list arrowto arrowfrom values-ftype)])
          (match ftype
            [($ Tvar _ _ _ _ _ _ _
               objs orig-objs con edgeto edgefrom)
              (list objs orig-objs con edgeto edgefrom)]
            [_ #f])))
      list-ftype)))

(define unprompt-kernel-state!
  (match-lambda
    [($ prompt-kernel-state saved-state prompt-hash-state ftypes)
      (restore-kernel-state! saved-state)
      (unprompt-hash-table-state! prompt-hash-state)
      (for-each
        (match-lambda
          [(ftype (arrowto arrowfrom values-ftype) tvar-info)
            (set-FlowType-arrowto! ftype arrowto)
            (set-FlowType-arrowfrom! ftype arrowfrom)
            (set-FlowType-values-ftype! ftype values-ftype)
            (match tvar-info
              [#f (void)]
              [(objs orig-objs con edgeto edgefrom)
                (set-Tvar-objs! ftype objs)
                (set-Tvar-orig-objs! ftype orig-objs)
                (set-Tvar-constraints! ftype con)
                (set-Tvar-edgeto! ftype edgeto)
                (set-Tvar-edgefrom! ftype edgefrom)])])
        ftypes)]))

;; ------------------------------------------------------------
;; Auxiliary info on Tvars

(define alloc-Tvar-field
  (case-lambda
    [(default-fn)
      (let* ( [table (make-hash-table)]
              [get-info
                (lambda (tvar) 
                  (assert (FlowType? tvar) tvar 'get-info)
                  (hash-table-get table tvar default-fn))]
              [set-info!
                (lambda (tvar v)
                  (assert (FlowType? tvar) tvar 'set-info!)
                  (hash-table-put! table tvar v))])
        (values get-info set-info!))]
    [() (alloc-Tvar-field (lambda () #f))]))

(define alloc-AV-field
  (case-lambda
    [(default-fn)
      (let* ( [table (make-hash-table)]
              [get-info
                (lambda (AV) 
                  (hash-table-get table AV default-fn))]
              [set-info!
                (lambda (AV v)
                  (hash-table-put! table AV v))])
        (values get-info set-info!))]
    [() (alloc-AV-field (lambda () #f))]))

(define (field->set alloc-field)
  (let*-vals ( [(get-info set-info!) (alloc-field)]
               [list-obj '()]
               [in? (lambda (obj) (get-info obj))]
               [add! (lambda (obj) 
                       (unless (in? obj)
                         (set-info! obj #t)
                         (set! list-obj (cons obj list-obj))))]
               [get-list (lambda () list-obj)])
    (values in? add! get-list)))

;; ----------------------------------------------------------------------
 
(define mk-Tvar-init-AV
  (lambda (sym AV)
    (let ([r (mk-Tvar sym)])
      (new-AV! r AV)
      r)))

(define (Tvar-name tvar) (FlowType-name tvar))

;; ======================================================================

