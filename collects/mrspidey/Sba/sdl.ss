;; sdl.ss
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
; ======================================================================
; Tvar->SDL compresses, calcs prods and creates sdl

;(define Tvar 'dummy-Tvar)

(define sdl-tvar (void))

(define (Tvar->SDL Tvar)
  (set! sdl-tvar Tvar)
  (pretty-debug-sdl2 `(Tvar->SDL ,(Tvar-name Tvar)))
  (let*-vals
    ( [(Tvar approximated)
        (if (eq? (st:sdl-fo) 'basic-types)
          (copy-constraint-fo-w/-limits Tvar)
          (values Tvar #f))]
      [_ (pretty-debug-sdl2 `(Tvar->SDL copied ,(Tvar-name Tvar)))]
      [nuTvar
        (let*-vals
          ([(live-Tvar cvt) 
             (dynamic-let
               ([st:minimize-respect-assignable-part-of-fields
                  (st:show-assignable-part-of-fields)])
               (pretty-debug-sdl2
                 `(st:minimize-respect-assignable-part-of-fields
                    ,(st:minimize-respect-assignable-part-of-fields)))
               (minimize-constraints 
                 (st:sdl-constraint-simplification)
                 '() (list Tvar)
                 list-ftype '()))]
            [nuTvar (cvt Tvar)])
          (pretty-debug-sdl2 `(nuTvar ,(Tvar-name nuTvar)))
          nuTvar)]
      [sdl (raw-Tvar->SDL nuTvar #f)])
    (if approximated
      `(approx ,sdl)
      sdl)))

;; (define (Tvar->SDL tvar) (Tvar-name tvar))

;; ----------------------------------------------------------------------

(define (copy-constraint-fo-w/-limits tvar)

  (let*-vals
    ( [mk-tvar-dots  
        (lambda ()
          (mk-Tvar-init-AV 'dots
            (create-AV template-dots '() mt-vector mt-vector)))]
      [tvar-L-dots (mk-tvar-dots)]
      [tvar-U-dots (mk-tvar-dots)]
      [approximated #f]
      [ref-tvar-L-dots (lambda () (set! approximated #t) tvar-L-dots)]
      [ref-tvar-U-dots (lambda () tvar-U-dots)]
      [(tvar-reached? tvar-reached! list-tvar-reached)
        (field->set alloc-Tvar-field)]
      [(AV-reached? AV-reached! list-AV-reached)
        (field->set alloc-AV-field)]
      [(tvar-nu tvar-nu!) (alloc-Tvar-field)]
      [(AV-nu AV-nu!)     (alloc-AV-field)]
      [stack (list tvar)]
      )

    ;; doesn't do st:sdl-fo-depth-limit or st:sdl-fo-size-limit
     
    (recur reach ([tvar tvar])
      (unless (tvar-reached? tvar)
        (tvar-reached! tvar)
        (for-each
          (match-lambda
            [(and AV ($ AV _ template _ fields+ fields-))
              (unless (AV-reached? AV)
                (AV-reached! AV)
                (unless
                  (or 
                    (and
                      (not (st:sdl-fo-ivars))
                      (memq template-all-ivars
                        (template-super-templates template)))
                    (and
                      (not (st:sdl-fo-struct-fields))
                      (memq template-structure
                        (template-super-templates template))))
                  (vector-for-each reach fields+)
                  (when (eq? template template-lam)
                    (vector-for-each reach fields-))))])
          (get-Tvar-objs tvar))))

    (for-each
      (lambda (tvar) (tvar-nu! tvar (mk-Tvar 'copy-constraint-fo-w/-limits)))
      (list-tvar-reached))
    (for-each
      (match-lambda
        [(and AV ($ AV _ (? (eqc? template-lam)) misc #(rng) #(dom)))
          (AV-nu! AV 
            (create-AV template-lam++ misc
              (vector-map
                (lambda (tvar) 
                  (if (tvar-reached? tvar) (tvar-nu tvar) (ref-tvar-L-dots)))
                (vector dom rng))
              (vector)))]
        [(and AV ($ AV _ template misc fields+ fields-))
          (AV-nu! AV 
            (create-AV template misc
              (vector-map
                (lambda (tvar) 
                  (if (tvar-reached? tvar) (tvar-nu tvar) (ref-tvar-L-dots)))
                fields+)
              (vector-map
                (lambda (_) (ref-tvar-U-dots))
                fields-)))])
      (list-AV-reached))
    (for-each
      (lambda (tvar) 
        (let ([nu (tvar-nu tvar)])
          (for-each
            (lambda (AV)
              (new-AV! nu (AV-nu AV)))
            (get-Tvar-objs tvar))))
      (list-tvar-reached))
    (assert (tvar-reached? tvar) 'copy-sdl-size-k)
    (pretty-debug-sdl2
      `(copy-constraint-fo-w/-limits 
         src ,(Tvar-name tvar)
         result ,(Tvar-name (tvar-nu tvar))
         size ,(length (list-AV-reached)) ,(length (list-tvar-reached))))
    (values
      (tvar-nu tvar)
      approximated)))

;; ----------------------------------------------------------------------
; raw-Tvar->SDL calcs prods and creates sdl
;
; We use that Tvar-L, Tvar-U and AV-U fields for eq?-ness.
; But by Tvar-L, we really mean prefixes of Tvar-L, etc.

(define (raw-Tvar->SDL tvar mono)
  ;; All reachable tvars are shown
  (pretty-debug-sdl2 `(raw-Tvar->SDL ,(Tvar-name tvar)))
  ;;(find-nonempty-tvars '() (list tvar))
  (let*-vals
    ( [mono-convert
        (if mono
          (match-lambda
            [($ NT tvar 'L) (chk-Tvar-U tvar)]
            [nt nt])
          (lambda (nt) nt))]
      [(env-crossover env-named-lfp) 
        (choose-named-nts (chk-Tvar-U tvar) mono-convert)])
    (letrec
      ([sdl-ref
         (lambda (nt) 
           (let ([nt (mono-convert nt)])
             (or (ormap
                   (match-lambda [(z . Y) (and (eq? nt z) Y)])
                   env-named-lfp)
               (mk-sdl nt))))]
        [mk-sdl
          (lambda (nt)
            (let ([sdl (NT->SDL nt sdl-ref #t)])
              (if (bound-in-env? env-crossover (NT-tvar nt))
                ((if (eq? (NT-type nt) 'U) absunion absintersect)
                  sdl
                  (lookup env-crossover (NT-tvar nt)))
                sdl)))])
      (let* ([binds
               (map
                 (match-lambda 
                   [(y . name) (list name (mk-sdl y))])
                 env-named-lfp)]
              [body (sdl-ref (chk-Tvar-U tvar))]
              [result (expand-output-type `(rec ,binds ,body))])
        result))))

;; ======================================================================
 
(define choose-named-nts
  ;; Returns (values env-crossover env-named-lfp)
  ;; env-crossover maps Tvar to Xn
  ;; env-named-lfp maps NT to Yn

  (lambda (nt mono-convert)
    (let* ( [traversed '()]
            [named-lfp '()]
            [stack '()])

      (letrec
        ( [traverse-convert
            (lambda (nt)
              (let* ( [nt (mono-convert nt)])
                (unless (memq nt named-lfp)
                  (let* ( [children '()]
                          [ref (lambda (nt)
                                 (set! children (cons nt children))
                                 'blah)])
                    (NT->SDL nt ref #f)
                    (pretty-debug-sdl 
                      `(traversing ,(nt->sym nt) 
                         children ,(map nt->sym children)
                         traversed  ,(map nt->sym traversed)
                         stack  ,(map nt->sym stack)
                         ))

                    (let ([trivial 
                            (or (null? children)
                              (match nt
                                [($ NT tvar 'U)
                                  (and (Tvar? tvar)
                                    (match (Tvar-objs tvar)
                                      [() #t]
                                      [(($ AV _ _ _ fields+)) 
                                        (zero? (vector-length fields+))]
                                      [_ #f]))]
                                [_ #f]))])
                      (pretty-debug `(trivial ,(nt->sym nt) ,trivial))
                      (when (case (st:naming-strategy)  
                              [(recursive) (memq nt stack)]
                              [(multiple)  (and (memq nt traversed)
                                             (not trivial))]
                              [(nontrivial) (not trivial)]
                              [(all) #t])
                        ;; Name this element
                        (pretty-debug-sdl
                          `(naming ,(nt->sym nt) 
                             ,(if (memq nt traversed) #t #f)
                             children ,(map nt->sym children)
                             ,trivial))
                        (set! named-lfp (cons nt named-lfp))))
                  (unless (memq nt traversed)
                    (set! traversed (cons nt traversed))
                    ;;(pretty-print '(traversing children))
                    (let ([oldstk stack])
                      (set! stack (cons nt stack))
                      (for-each
                        traverse-convert 
                        children)
                      (set! stack oldstk)))))))])
        (traverse-convert nt)
        (pretty-debug-sdl '(Traversed))

        (let* ([crossover
                 (intersect (select-L traversed) (select-U traversed))]
                [env-crossover
                  (map-with-n
                    (lambda (y n) (cons y (symbol-append 'X (add1 n))))
                    (reverse crossover))]
                [env-named-lfp
                  (map-with-n
                    (lambda (y n) 
                      (cons y (symbol-append 'Y (add1 n) ;;(nt->sym y)
                                )))
                    named-lfp)])

          (pretty-debug-sdl
            `(choose-named-nts
               select-L ,(map Tvar-name (select-L traversed))
               select-U ,(map (lambda (x) (and (Tvar? x) (Tvar-name x)))
                           (select-U traversed))
               crossover ,(map Tvar-name crossover)
               named-lfp ,(map nt->sym named-lfp)
               traversed ,(map nt->sym traversed)))

          (values env-crossover env-named-lfp))))))

; ======================================================================
 
(define (NT->SDL nt ref-nt show?)
  (pretty-debug-sdl `(NT->SDL ,(nt->sym nt) ,show?))
  (let ([r
          (match nt
            [($ NT (? Tvar? tvar) 'U)
              (when (st:listify-etc)
                (listify-Tvar! tvar)
                (boolify-Tvar! tvar)
                (atomify-Tvar! tvar)
                (sexpify-Tvar! tvar)
                (classify-objectify-Tvar! tvar))
              (absUnion
                (map
                  (lambda (AV) (ref-nt (chk-AV-U AV)))
                  (get-Tvar-objs tvar)))]
            [($ NT (? Tvar? tvar) 'L)
              (absintersect
                (absIntersect
                  (recur loop ([con* (filter con? (Tvar-constraints tvar))])
                    (match con*
                      [() '()]
                      [(($ con _ (and template ($ template _ _ _ ref assign))
                          _ _ sign)
                         . _)
                        (let*-vals
                          ([ (i-s-tvar* rest)
                             (filter-map-split
                               (match-lambda
                                 [($ con _ 
                                    (? (lambda (t) (eq? t template)))
                                    f tvar s)
                                   (cons (cons f s) tvar)]
                                 [_ #f])
                               con*)]
                            [ref-i+
                              (lambda (i)
                                (absIntersect
                                  (filter-map
                                    (match-lambda
                                      [(j-s . tvar2) 
                                        (and 
                                          (equal? (car j-s) i)
                                          (cdr j-s)
                                          (ref-nt (chk-Tvar-L tvar2)))])
                                    i-s-tvar*)))]
                            [ref-i-
                              (lambda (i)
                                (absUnion
                                  (filter-map
                                    (match-lambda
                                      [(j-s . tvar2) 
                                        (and 
                                          (equal? (car j-s) i)
                                          (cdr j-s)
                                          (ref-nt (chk-Tvar-U tvar2)))])
                                    i-s-tvar*)))]
                            [this 

                              (cond
                                [(eq? template template-lam)
                                  (list (ref-i- 0) '*->* (ref-i+ 1))]
                                [(eq? template template-lam++)
                                  (list (ref-i+ 0) '*->* (ref-i+ 1))]
                                [(st:show-assignable-part-of-fields)
                                  (list
                                    (template-type template)
                                    (map ref-i+ 
                                      (filter number? (vector->list ref)))
                                    (map ref-i-
                                      (filter number? (vector->list assign))))]
                                [(and 
                                   (eq? (vector-length ref) 1)
                                   (not (vector-ref ref 0)))
                                  ;; Show single antimono field
                                  (list (template-type template) (ref-i- 0))]
                                [else
                                  ;; Only show ref'able fields
                                  (cons
                                    (template-type template)
                                    (map ref-i+
                                      (filter number? (vector->list ref))))])])

                          (cons this (loop rest)))])))
                (absIntersect
                  (filter-map
                    (match-lambda
                      [($ con-filter _ _ tvar2)
                        (ref-nt (chk-Tvar-L tvar2))]
                      [_ #f])
                    (Tvar-constraints tvar)))
                (absIntersect
                  (map
                    (lambda (tvar2) (ref-nt (chk-Tvar-L tvar2)))
                    (Tvar-edgeto tvar))))]

            [($ NT
               (and AV ($ AV _ (and template ($ template _ _ _ ref assign)) 
                         misc fields+ fields-))
               'U)
              (let ([ref-i+
                      (lambda (i)
                        (ref-nt (chk-Tvar-U (vector-ref fields+ i))))]
                     [ref-i-
                       (lambda (i)
                         (ref-nt (chk-Tvar-L (vector-ref fields- i))))])

                (cond 
                  [(or 
                     (eq? template template-lam)
                     (eq? template template-lam++))

                    (if (and 
                          (atprim? misc) 
                          (not (eq? (st:primitive-types) 'inferred)))

                      ;; Is a primitive
                      (when show?
                        (case (st:primitive-types)
                          [(prim) `(prim ,(atprim-sym misc))]
                          [(given) (atprim-orig-type misc)]))

                      ;; Print as lambda
                      (let*-vals ( [(dom rng)
                                     (if (eq? template template-lam)
                                       (values (ref-i- 0) (ref-i+ 0))
                                       (values (ref-i+ 0) (ref-i+ 1)))])
                        (when show?
                          (match
                            (match misc
                              [('lam-info nargs restarg)
                                (pretty-domain-list dom nargs restarg)]
                              [_ (pretty-domain-list dom 0 #t)])
                            [(args ())
                              (append args (list '->* rng))]
                            [(args restarg)
                              (append args (list restarg '*->* rng))]))))]

                  ;; Not a lambda
                  [(memq (template-type template) '(object class))
                    (cons 
                      (template-type template)
                      (map-with-n
                        (lambda (ivar-sym n) (list ivar-sym (ref-i+ n)))
                        misc))]

                  [(st:show-assignable-part-of-fields)
                    (list
                      (template-type template)
                      (map ref-i+ (filter number? (vector->list ref)))
                      (map ref-i- (filter number? (vector->list assign))))]
                  [(and 
                     (eq? (vector-length ref) 1)
                     (not (vector-ref ref 0)))
                    (list (template-type template) (ref-i- 0))]
                  [(and 
                     (eq? (vector-length ref) 0)
                     (eq? (vector-length assign) 0)
                     (st:constants)
                     (or (number? misc) (symbol? misc) (char? misc)))
                    (if (symbol? misc) (list 'quote misc) misc)]
                  [else
                    (cons 
                      (template-type template)
                      (map ref-i+ (filter number? (vector->list ref))))]))])])

    (pretty-debug-sdl `(NT->SDL ,(nt->sym nt) = ,r))
    r))

;; ----------------------------------------------------------------------

(define (pretty-domain-list dom nargs restarg)
  (recur loop ([args '()][dom dom][nargs nargs])
    (match dom
      [('cons arg restarg)
       (loop (cons arg args) restarg (sub1 nargs))]
      [('nil)
       (list (reverse args) '())]
      ['_
       (cond
        [(> nargs 0)
         (loop (cons '_ args) '_ (sub1 nargs))]
        [restarg 
         (list (reverse args) '_)]
        [else
         (list (reverse args) '())])]
      [dom (list (reverse args) dom)])))

(define (pretty-range-list rng nrngs)
  (list '() rng))

; ======================================================================

(define template-listof (constructor->template 'listof #f))

(define template-bool (constructor->template 'bool))

(define template-atom (constructor->template 'atom))

(define template-sexp (constructor->template 'sexp))

; ----------------------------------------------------------------------

(define (listify-Tvar! tvar)
  ;; If Tvar contains (union nil (cons x tvar))
  ;; then replace by (list x)
  ;;(pretty-print (get-Tvar-objs tvar))
  ;; (show-sba-Tvar tvar)
  (match (get-Tvar-objs tvar)
    [(or 
       ( ($ AV _ (? (is-template? 'nil)))
         ($ AV _ (? (is-template? 'cons)) _ 
           #(a (? (lambda (d) (eq? d tvar))))))
       ( ($ AV _ (? (is-template? 'cons)) _ 
           #(a (? (lambda (d) (eq? d tvar)))))
         ($ AV _ (? (is-template? 'nil)))))
      (set-Tvar-objs! tvar '())
      (new-AV! tvar (create-AV template-listof '() (vector a) (vector)))]
    [_ (void)]))

(define (boolify-Tvar! tvar)
  ;; If Tvar contains (union true false)
  ;; then replace those by bool
  (let* ([bool-things '(true false)])
    (when
      (andmap
        (lambda (bool-thing)
          (ormap
            (lambda (AV) (eq? (template-type (AV-template AV)) bool-thing))
            (get-Tvar-objs tvar)))
        bool-things)
      ;; Do substitution
      (set-Tvar-objs! tvar
        (filter
          (lambda (AV) 
            (not (memq (template-type (AV-template AV)) bool-things)))
          (get-Tvar-objs tvar)))
      (new-AV! tvar (create-AV template-bool '() (vector) (vector))))))

(define (atomify-Tvar! tvar)
  ;; If Tvar contains (union nil num sym str char bool)
  ;; then replace those by atom
  (let* ([atom-things '(nil num sym str char bool)])
    (when
      (andmap
        (lambda (atom-thing)
          (ormap
            (lambda (AV) (eq? (template-type (AV-template AV)) atom-thing))
            (get-Tvar-objs tvar)))
        atom-things)
      ;; Do substitution
      (set-Tvar-objs! tvar
        (filter
          (lambda (AV) 
            (not (memq (template-type (AV-template AV)) atom-things)))
          (get-Tvar-objs tvar)))
      (new-AV! tvar (create-AV template-atom '() (vector) (vector))))))

(define (sexpify-Tvar! tvar)
  ;; If Tvar l contains (union atom (box l) (vec l) (cons l l)), 
  ;; then replace those by sexp
  (let* ([sexp-things '(atom box cons vec)])
    (when
      (andmap
        (lambda (sexp-thing)
          (ormap
            (match-lambda
              [($ AV _ ($ template type _ ref) _ fields+)
                (and (eq? type sexp-thing)
                  (or (zero? (vector-length fields+))
                    (eq?  (vector-ref fields+ 0) tvar)))])
            (get-Tvar-objs tvar)))
        sexp-things)
      ;; Do substitution
      (printf "Do substitution")
      (set-Tvar-objs! tvar
        (filter
          (lambda (AV) 
            (not (memq (template-type (AV-template AV)) sexp-things)))
          (get-Tvar-objs tvar)))
      (new-AV! tvar (create-AV template-sexp '() (vector) (vector))))))

(define (classify-objectify-Tvar! tvar)
  ;; For each ivarset in tvar,
  ;; build an object AV 

  (pretty-debug-sdl `(classify-objectify-Tvar! ,(FlowType-name tvar)))

  (letrec
    ([make-thingy
       (lambda (tvar-get-ivars tvar-put-thingy name 
                 ivar-syms parent-ivars)
         (recur loop ([ivar-syms ivar-syms][parent-ivars parent-ivars])
           ;; to ivar parents
           (for-each
             (match-lambda
               [($ AV _ 
                  (? (lambda (t) (eq? t template-ivarset)))
                  ivar-extra-syms
                  #(parent-parent-ivars))                
                 (loop
                   (append ivar-syms ivar-extra-syms)
                   parent-parent-ivars)]
               [_ (void)])
             (Tvar-objs parent-ivars))
           ;; make thingy for this
           (when (or 
                   (null? (Tvar-objs parent-ivars))
                   (ormap
                     (lambda (AV)
                       (not (eq? (AV-template AV) template-ivarset)))
                     (Tvar-objs parent-ivars)))
             (pretty-debug-sdl2 `(all-ivars ,name ,ivar-syms))
             (let* 
               ( [ivar-syms (list->set ivar-syms)]
                 [ivar-syms
                   (sort 
                     (lambda (s1 s2)
                       (string<? 
                         (symbol->string s1)
                         (symbol->string s2)))
                     ivar-syms)]
                 [n+ (length ivar-syms)]
                 [template-thingy
                   (make-template 
                     name n+ 0
                     (list->vector
                       (recur loop ([n 0])
                         (if (= n n+) '() (cons n (loop (add1 n))))))
                     (vector)
                     '()
                     eq?)]
                 [fields+
                   (list->vector
                     (map
                       (lambda (ivar-sym)
                         (let ([tvar-field (mk-Tvar 'object-field)])
                           (new-con! tvar-get-ivars
                             (create-con
                               (get-ivar-template ivar-sym)
                               0 tvar-field #t))
                           tvar-field))
                       ivar-syms))])
               (pretty-debug-sdl2 `(ivarset ,ivar-syms))
               (new-AV! tvar-put-thingy
                 (create-AV template-thingy 
                   ivar-syms fields+ (vector)))))))])

    (for-each
      (match-lambda
        [($ AV _
           (? (lambda (t) (eq? t template-internal-class)))
           misc
           #(_ alpha_o _ _ alpha_v))

          (make-thingy alpha_o tvar 'class '() alpha_v)]
          
        [($ AV _ 
           (? (lambda (t) (eq? t template-ivarset)))
           ivar-syms
           #(parent-ivars))

          (make-thingy tvar tvar 'object ivar-syms parent-ivars)]

        [_ (void)])
      (Tvar-objs tvar))

    ;; Now drop any ivar, ivarset and class from the tvar
    (set-Tvar-objs! tvar
      (filter
        (lambda (AV)
          (let ([t (AV-template AV)])
            (not
              (or
                (memq template-all-ivars (template-super-templates t))
                (eq? t template-ivarset)
                (eq? t template-internal-class)))))
        (Tvar-objs tvar)))))

; ----------------------------------------------------------------------

'(define (copy-depth-k tvar k)
  (let*-vals
    ( [tvar-dots 
        (mk-Tvar-init-AV 'dots
          (create-AV template-dots '() mt-vector mt-vector))]
      [tvar-empty
        (mk-Tvar 'depth-k-empty)]
      [(tvar-reached? tvar-reached! list-tvar-reached)
        (field->set alloc-Tvar-field)]
      [(AV-reached? AV-reached! list-AV-reached)
        (field->set alloc-AV-field)]
      [(tvar-nu tvar-nu!) (alloc-Tvar-field)]
      [(AV-nu AV-nu!)     (alloc-AV-field)]
      )

    (recur loop ([tvar tvar][k k])
      (when
        (and 
          (not (zero? k))
          (not (tvar-reached? tvar)))
        (tvar-reached! tvar)
        (for-each
          (match-lambda
            [(and AV ($ AV _ _ _ fields+))
              (AV-reached! AV)
              (vector-for-each
                (lambda (tvar2) (loop tvar2 (sub1 k)))
                fields+)])
          (get-Tvar-objs tvar))))

    (for-each
      (lambda (tvar) (tvar-nu! tvar (mk-Tvar 'copy-depth-k)))
      (list-tvar-reached))
    (for-each
      (match-lambda
        [(and AV ($ AV _ template misc fields+ fields-))
          (AV-nu! AV 
            (create-AV template misc
              (vector-map
                (lambda (tvar) 
                  (if (tvar-reached? tvar) (tvar-nu tvar) tvar-dots))
                fields+)
              (vector-map
                (lambda (_) tvar-empty)
                fields-)))])
      (list-AV-reached))
    (for-each
      (lambda (tvar) 
        (let ([nu (tvar-nu tvar)])
          (for-each
            (lambda (AV)
              (new-AV! nu (AV-nu AV)))
            (get-Tvar-objs tvar))))
      (list-tvar-reached))
    (pretty-debug-sdl2
      `(copy-depth-k 
         src ,(Tvar-name tvar)
         depth ,k
         result ,(Tvar-name (tvar-nu tvar))
         size ,(length (list-AV-reached)) ,(length (list-tvar-reached))))
    (tvar-nu tvar)))

;; ----------------------------------------------------------------------
    
'(define (copy-sdl-size-k tvar k)

   (let*-vals
     ( [tvar-dots 
         (mk-Tvar-init-AV 'dots
           (create-AV template-dots '() mt-vector mt-vector))]
       [tvar-empty
         (mk-Tvar 'depth-k-empty)]
       [(tvar-reached? tvar-reached! list-tvar-reached)
         (field->set alloc-Tvar-field)]
       [(AV-reached? AV-reached! list-AV-reached)
         (field->set alloc-AV-field)]
       [(tvar-nu tvar-nu!) (alloc-Tvar-field)]
       [(AV-nu AV-nu!)     (alloc-AV-field)]
       [stack (list tvar)]
       )

     (recur loop ()
       (unless (>= 0 k)
         (match stack
           [(tvar . rest)
             (set! stack rest)
             (if (tvar-reached? tvar)
               (loop)
               (begin
                 (set! k (sub1 k))
                 (tvar-reached! tvar)
                 (for-each
                   (match-lambda
                     [(and AV ($ AV _ _ _ fields+))
                       (unless (AV-reached? AV)
                         (set! k (sub1 k))
                         (AV-reached! AV)
                         (set! stack 
                           (append stack (reverse (vector->list fields+)))))])
                   (get-Tvar-objs tvar))
                 (loop)))]
           [() (void)])))

     (for-each
       (lambda (tvar) (tvar-nu! tvar (mk-Tvar 'copy-depth-k)))
       (list-tvar-reached))
     (for-each
       (match-lambda
         [(and AV ($ AV _ template misc fields+ fields-))
           (AV-nu! AV 
             (create-AV template misc
               (vector-map
                 (lambda (tvar) 
                   (if (tvar-reached? tvar) (tvar-nu tvar) tvar-dots))
                 fields+)
               (vector-map
                 (lambda (_) tvar-empty)
                 fields-)))])
       (list-AV-reached))
     (for-each
       (lambda (tvar) 
         (let ([nu (tvar-nu tvar)])
           (for-each
             (lambda (AV)
               (new-AV! nu (AV-nu AV)))
             (get-Tvar-objs tvar))))
       (list-tvar-reached))
     (assert (tvar-reached? tvar) 'copy-sdl-size-k k)
     (pretty-debug-sdl2
       `(copy-depth-k 
          src ,(Tvar-name tvar)
          depth ,k
          result ,(Tvar-name (tvar-nu tvar))
          size ,(length (list-AV-reached)) ,(length (list-tvar-reached))))
     (tvar-nu tvar)))



;; ----------------------------------------------------------------------
    
        
      
