; ASSUME DEFINED VARIABLES NEVER ASSIGNED

; traverse.ss
; Traverses source program and produces constraints.
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
;; ------------------------------------------------------------

;; Environments are:
;; (Name -> (or FlowType mutable-binding))

;; ----------------------------------------------------------------------

(define (top-level-traverse-defs defs env)
  ;;(assert (atenv:ok? env) 'top-level-traverse-defs)
  (begin0
    (traverse-defs defs env)
    (unless (null? defs)
      (mrspidey:zprogress "Analyzing" (zodiac:zodiac-finish (rac defs))))
    ))

(define traverse:bad-expr (void))

(define (traverse-defs defs env)
  ;;(assert (atenv:ok? env) 'traverse-defs)
  ;; returns (values env refs result)
  (pretty-debug-traverse `(->traverse-defs))
  ;; First alloc void for all defines
  (let* ( [lvrs (apply append
                  (map (match-lambda
                         [($ zodiac:define-values-form _ _ _ _ vars) vars]
                         [_ '()])
                    defs))]
          [names (map zodiac:varref-binding lvrs)]
          [nuenv (atenv:extend-undefineds env names)]
          ;; bubble define-struct stuff to top
          [defs 
            (append
              (filter
                (match-lambda
                  [($ zodiac:define-values-form _ _ _ _ _ 
                     ($ zodiac:struct-form))
                    #t]
                  [_ #f])
                defs)
              (filter
                (match-lambda
                  [($ zodiac:define-values-form _ _ _ _ _ 
                     ($ zodiac:struct-form))
                    #f]
                  [_ #t])
                defs))])
    (recur loop ([defs defs]
                 [env nuenv]
                 [refs '()]
                 [result (wrap-value (mk-tvar-void))])
      (match defs
        [() (atenv:flush! env)
          (values env refs result)]
        [(first . rest)
         (let-values
           ([(env nu-refs result) (traverse-def first env)])
           (loop rest env (append nu-refs refs) result))]))))

;; ----------------------------------------------------------------------

(define (traverse-def def env)
  ;; returns (values env refs result) 
  ;; (: env (listof (cons Name FlowType)))
  ;; (: refs (listof (cons Name Tvar)))
  ;; (: result FlowType) -- multiple value list

  (pretty-debug-traverse-small
   `(->traverse-def
     ,(zodiac:location-offset (zodiac:zodiac-start def))
     ,(zodiac:stripper def)))

  (mrspidey:zprogress "Analyzing" (zodiac:zodiac-start def))

  (let* ([tvar-E (mk-Tvar 'def)])
    (link-parsed-ftype! def tvar-E)
    (new-AV! tvar-E AV-void)
    (match def
      ;; --- define
      [($ zodiac:define-values-form  _ _ _ _ lvrs exp)
       (let*-vals ([(ftype env refs) (traverse-exp exp env)]
                   [ftypes (multiple-value-components ftype (length lvrs))]
                   [ftypes (map link-parsed-ftype! lvrs ftypes)]
                   [names (map zodiac:varref-binding lvrs)]
                   [env (atenv:change-bindings env names ftypes)])
                  (values env refs (wrap-value (mk-tvar-void))))]
      ;; --- define-type
      [($ zodiac:define-type-form _ s _ _ sym type)
       (let ([tvar (mk-Tvar 'define-type)])
         (schema->con
           (expand-input-type-err type def)
           tvar 'define-type '())
         (add-global-tdef! sym tvar)
         (values env '() (wrap-value (mk-tvar-void))))]
      ;; --- define-constructor
      [($ zodiac:define-constructor-form _ _ _ _ name modes)
       (apply add-constructor! name modes)
       (values env '() (wrap-value (mk-tvar-void)))]
      ;; --- Exp
      [exp
       (let-values ([(result env refs) (traverse-exp exp env)])
                   (values env refs result))])))

;; ----------------------------------------------------------------------

(define values-trap-pt (void))

(define (traverse-exp E env)
  (: env (listof (cons Name (or mutable-binding FlowType))))

  ;; returns (values FlowType env refs)
  ;; (: refs (listof (cons Name Tvar)))
  ;; FlowType is annotated type for list of results

  (letrec*
   ([refs '()]
    [extend-ref! 
     (lambda (name tvar) (set! refs (cons (cons name tvar) refs)))]
    [extend-refs!
     (lambda (refs)
       (for-each
        (match-lambda
         [(name . tvar) (extend-ref! name tvar)])
        refs))]
    ;; ---
    [pattern-match (lambda (tvar-formal tvars-actuals)
                     (match tvars-actuals
                       [(? Tvar?) (new-edge! tvar-formal tvars-actuals)]
                       [(tvar1 . rest)
                        (let ([tvar-cdr (mk-Tvar 'arg-cdr)])
                          (new-con! tvar-formal (make-con-car tvar1))
                          (new-con! tvar-formal (make-con-cdr tvar-cdr))
                          (pattern-match tvar-cdr rest))]
                       [() (void)]))]
    [handle-lambda-form
     (lambda (args body env1)
       (: args zodiac:arglist)
            
       (let*-vals
        ( [env2 (atenv:unflush env1)]
          [tvar-arg (mk-Tvar 'args)]
          [improper? 
            (or (zodiac:sym-arglist? args) (zodiac:ilist-arglist? args))]
          ;;[_ (pretty-print `(improper ,improper? ,args))]
          [args (zodiac:arglist-vars args)]
          [env3 
            (recur loop ([env env2][args args][tvar tvar-arg])
              (cond
                [(null? args) env]
                [(and improper? (null? (cdr args)))
                  (atenv:extend env (car args) tvar)]
                [else
                  (let ( [tvar-car (mk-Tvar 'arg-car)]
                         [tvar-cdr (mk-Tvar 'arg-cdr)])
                    (new-con! tvar (make-con-car tvar-car))
                    (new-con! tvar (make-con-cdr tvar-cdr))
                    (loop (atenv:extend env (car args) tvar-car)
                      (cdr args)
                      tvar-cdr))]))]
         [(ftype-rv env4) (traverse body env3)]
         [AV (make-AV-lam tvar-arg (FlowType->Tvar ftype-rv)
                          (if improper? (sub1 (length args)) (length args))
                          improper?)]
         [tvar-E (mk-Tvar 'lam)])
        (atenv:flush! env4)
        (new-AV! tvar-E AV)
        tvar-E))]

     [handle-application
       (lambda (E env flush?)
         (match E
           [($ zodiac:app _ _ _ _ fn args)
             (set! values-trap-pt "|")
             (let*-vals 
               ( [(ftype-fn env1 pi) 
                   ;; Traverse fn specially if fo prim ref 
                   ;; don't instantiate yet, wait till have # args
                   (match fn
                     [($ zodiac:varref)
                       (=> fail)
                       (if (st:special-fo-prims)
                         (match (atenv:lookup env (zodiac:varref-binding fn))
                           [#f 
                             (set! values-trap-pt "A")
                             (fail)]
                           [ftype
                             (set! values-trap-pt "B")
                             (match (FlowType->Atype ftype)
                               [(and pi ($ atprim sym tschema))
                                 (set! values-trap-pt "C")
                                 (zodiac:set-parsed-atprim! fn pi)
                                 (let ([tvar-fn (mk-Tvar sym)])
                                   (tschema->con-for-nargs tschema 
                                     tvar-fn sym '() (length args))
                                   (link-parsed-ftype! fn (wrap-value tvar-fn))
                                   (set! values-trap-pt "D")
                                   (values tvar-fn env pi))]
                               [_ 
                                 (set! values-trap-pt "E")
                                 (fail)])])
                         (begin (set! values-trap-pt "F") (fail)))]
                     [_ 
                       (set! values-trap-pt "G")
                       (let-values ([(ftype env) (traverse1 fn env)])
                         (set! values-trap-pt "H")
                         (values ftype env #f))])]
                 [_ (set! values-trap-pt "K")]
                 [(ftype-arg* env2) (traverse* args env1)]
                 [ftype-arg* (map extract-1st-value ftype-arg*)]
                 [env3 (if (and flush? (not pi))
                         (atenv:unflush (atenv:flush! env2))
                         env2)]

                 [tvar-fn (FlowType->Tvar ftype-fn)]
                 [tvar-arg* (map FlowType->Tvar ftype-arg*)]
                 [tvar-arg
                   (foldr
                     (lambda (tvar-arg tvar-rest-args)
                       (let* ([tvar (mk-Tvar 'arg)])
                         (new-AV! tvar 
                           (make-AV-cons tvar-arg tvar-rest-args))
                         tvar))
                     (mk-tvar-nil)
                     tvar-arg*)]
                 [tvar-E (mk-Tvar 'expr)])
               ;;(link-parsed-ftype! fn (wrap-value tvar-fn))
               (pretty-debug-traverse
                 `(tvar-fn ,(Tvar-name tvar-fn)
                    tvar-arg ,(Tvar-name tvar-arg)))
               (zodiac:set-app-tvar-args! E tvar-arg)
               (new-con! tvar-fn (make-con-dom tvar-arg))
               (new-con! tvar-fn (make-con-rng tvar-E))
               (values tvar-E
                 (if (and (st:flow-sensitive) pi)
                   (flow-sensitive-env pi args env3)
                   env3)))]))]

    ;; ---
    [traverse*
     (lambda (E* env)
       ;; Returns (values (listof FlowType) env)
       (pretty-debug-traverse `(->traverse* ,(length E*)))
       (match
           (foldl
            (match-lambda*
             [(E (ftype* . env))
              (let-values ([(ftype nuenv) (traverse E env)])
                          (cons (cons ftype ftype*) nuenv))])
            (cons '() env)
            E*)
         [(ftype* . env)
          (pretty-debug-traverse 
           `(<-traverse* ,(map FlowType->pretty (reverse ftype*))))
          (values (reverse ftype*) env)]))]
    ;; ---
    [made-unit (lambda (atype)
                 (cond
                  [(not (st:fo-units))      (Atype->Tvar atype)]
                  [(not (st:lazy-fo-units)) (atlunit->atunit atype)]
                  [else                     atype]))]
    [traverse1 (lambda (E env)
                 (let*-vals ([(ftype env) (traverse E env)]
                             [ftype (extract-1st-value ftype)])
                            (values ftype env)))]
    [traverse
     (lambda (E env)
       ;;(assert (atenv:ok? env) 'traverse)

       (pretty-debug-traverse-small
        `(->traverse
           ,(zodiac:location-offset (zodiac:zodiac-start E))
           ,(zodiac:stripper E)
           ,(atenv->pretty env)))
       (let*-vals 
        ([trav (lambda (E) (traverse E env))]
         [(result env-result)
          (match E
            ;; (: result (union Tvar fo-FlowType fo-Atype))

            [($ zodiac:varref)
              (let ([name (zodiac:varref-binding E)])
                (match (atenv:lookup env name)
                  [#f (let ([tvar (mk-Tvar 'import-var)])
                        (extend-ref! name tvar)
                        (values (wrap-value tvar) env))]
                  [ftype 
                    ;; Instantiate schemas and atprims here
                    (let* ( [atype (FlowType->Atype ftype)]
                            [ftype
                              (if (or (atprim? atype) (schema? atype))
                                (FlowType->Tvar ftype)
                                ftype)])
                      (when (atprim? atype) 
                        (zodiac:set-parsed-atprim! E atype))
                      (values (wrap-value ftype) env))]))]

            [($ zodiac:quote-form _ _ _ _ 
                (or ($ zodiac:boolean _ _ _ c)
                    ($ zodiac:number _ _ _ c)))
             (values (wrap-value (make-atconst c)) env)]
               
            [($ zodiac:quote-form _ _ _ _ c)
             (values (wrap-value (traverse-const c)) env)]

            ;; --- local set!
            [($ zodiac:set!-form _ _ _ _ lvr exp)
              (let*-vals ([(ftype env1) (trav exp)]
                         [ftype (extract-1st-value ftype)]
                         [ftype (link-parsed-ftype! lvr ftype)]
                         [name (zodiac:varref-binding lvr)]
                         [env2 (atenv:change-binding env1 name ftype)])
                        (values (wrap-value (mk-tvar-void)) env2))]

            [($ zodiac:if-form _ _ _ _ test then else)
              ;; old version
              '(let*-vals
                 ([(ftype env1) (trav test)]
                   [env2 (atenv:capture-locs env1 
                           (zodiac:free-vars E (atenv:domain env1)))]
                   [(env-then env-else) 
                     (if (st:if-split)
                       (if-splitting-env test env2)
                       (values env2 env2))]
                   [tvar-E (mk-Tvar 'expr)]
                   [do-branch 
                     (lambda (exp env)
                       (let-values ([(ftype env2) (traverse exp env)])
                         (atenv:flush! env2)
                         (new-edge! (FlowType->Tvar ftype) tvar-E)))])
                 (when (or (not (st:if-split))
                         (match (FlowType->Atype ftype)
                           [($ atconst #f) #f]
                           [_ #t]))
                   (do-branch then env-then))
                 (when (or (not (st:if-split))
                         (match (FlowType->Atype ftype)
                           [($ atconst #f) #t]
                           [($ atconst _) #f]
                           [_ #t]))
                   (do-branch else env-else))

                 (values tvar-E (atenv:unflush env2)))

              ;; new version
              (let*-vals
                ( [(ftype env1) (trav test)]
                  [env-dom (atenv:domain env1)]
                  [env1 (atenv:capture-locs env1 
                          (zodiac:free-vars then env-dom))]
                  [env1 (atenv:capture-locs env1
                          (zodiac:free-vars else env-dom))]
                  [(env-then env-else) 
                    (if (st:if-split)
                      (if-splitting-env test env1)
                      (values env1 env1))]
                  [tvar-E (mk-Tvar 'expr)]
                  [do-branch 
                    (lambda (exp env)
                      (let-values ([(ftype env2) (traverse exp env)])
                        (new-edge! (FlowType->Tvar ftype) tvar-E)
                        env2))]
                  [env-then-done
                    (and (or (not (st:if-split))
                           (match (FlowType->Atype ftype)
                             [($ atconst #f) #f]
                             [_ #t]))
                      (do-branch then env-then))]
                  [env-else-done
                    (and (or (not (st:if-split))
                           (match (FlowType->Atype ftype)
                             [($ atconst #f) #t]
                             [($ atconst _) #f]
                             [_ #t]))
                      (do-branch else env-else))])

                (values tvar-E
                  (cond
                    [(and 
                       (eq? env-then env-then-done)
                       (eq? env-else env-else-done))
                      env1]
                    [(not env-else-done) env-then-done]
                    [(not env-then-done) env-else-done]
                    [else
                      (atenv:flush! env-then-done)
                      (atenv:flush! env-else-done)
                      (atenv:unflush env1)])))]

            ;; ### SPECIAL CODE FOR STRUCT-REF
            [($ zodiac:app _ _ _ _
               ($ zodiac:varref _ _ _ _ '#%struct-ref)
               ( struct-exp
                 ($ zodiac:quote-form _ _ _ _ ($ zodiac:number _ _ _ n))))
              (assert (and (integer? n) (>= n 0)))
              ;;(printf "Special struct-ref~n")
              (let*-vals 
                ( [(ftype-struct env1) (traverse1 struct-exp env)]
                  [tvar-struct (FlowType->Tvar ftype-struct)]
                  [tvar-elem (mk-Tvar 'struct-ref)])
                (new-con! tvar-struct
                  (create-con template-structure n tvar-elem #t))
                (values (wrap-value tvar-elem) env1))]

            ;; ### SPECIAL CODE FOR ivar
            [($ zodiac:app _ _ _ _
               (and ref ($ zodiac:varref _ _ _ _ (or '#%uq-ivar 'uq-ivar)))
               ( obj-exp
                 (and sym-exp
                   ($ zodiac:quote-form _ _ _ _ ($ zodiac:symbol _ _ _ sym)))))
              ;;(printf "Special uq-ivar~n")
              (let*-vals 
                ( [(ftype-obj env1) (traverse1 obj-exp env)]
                  [tvar-obj (FlowType->Tvar ftype-obj)]
                  [tvar-ivar (mk-Tvar 'ivar-ref)])
                (new-con! tvar-obj
                  (create-con (get-ivar-template sym) 0 tvar-ivar #t))
                ;(trav ref)
                ;(trav sym-exp)
                (values (wrap-value tvar-ivar) env1))]

            ;; ### SPECIAL CODE FOR UNIT/SIG
            [($ zodiac:app _ _ _ _
               (and ref ($ zodiac:varref _ _ _ _ '#%make-unit-with-signature))
               (unit-exp _ _))
              (traverse unit-exp env)]
            [($ zodiac:app _ _ _ _
               (and ref 
                 ($ zodiac:varref _ _ _ _ '#%verify-linkage-signature-match))
               args)
              (values (wrap-value (mk-tvar-void)) env)]
            [($ zodiac:app _ _ _ _
               (and ref 
                 ($ zodiac:varref _ _ _ _ '#%unit-with-signature-unit))
               (unit-exp))
              (traverse unit-exp env)]


            
            [($ zodiac:app _ _ _ _ fn args)
              (handle-application E env #t)]
                       
            [($ zodiac:letrec*-values-form _ _ _ _ varss exps body)
             ;; First init each new var
             (recur loop
                 ([env (atenv:extend-undefineds env (apply append varss))]
                  [varss varss]
                  [exps exps])
               (if (null? exps)
                   (traverse body env)
                   (let*-vals
                    ([(ftype nuenv) (traverse (car exps) env)]
                     [vars (car varss)]
                     [ftypes (multiple-value-components ftype (length vars))]
                      ;; overwrite void binding
                     [ftypes (map link-parsed-ftype! vars ftypes)]
                     [nuenv2 (atenv:change-bindings nuenv vars ftypes)])
                    (loop nuenv2 (cdr varss) (cdr exps)))))]

            [($ zodiac:let-values-form _ _ _ _ varss exps body)
             (let*-vals 
              ([(ftype* nuenv) (traverse* exps env)]
               [nuenv2
                (foldr2
                 (lambda (vars ftype env)
                   (assert (list? vars) 'let-valeus-form)
                   (atenv:extend* 
                    env vars
                    (multiple-value-components ftype (length vars))))
                 nuenv varss ftype*)])
              (traverse body nuenv2))]

            [($ zodiac:case-lambda-form _ _ _ _ args bodies)
             (let ( [tvar-E (mk-Tvar 'expr)]
                    [env1 (atenv:capture-locs env 
                            (zodiac:free-vars E (atenv:domain env)))])
               (for-each
                (lambda (args body) 
                  (new-edge! 
                    (FlowType->Tvar (handle-lambda-form args body env1))
                    tvar-E))
                 args bodies)
               (values (wrap-value tvar-E) env1))]

            ;;[($ zodiac:delay-form _ _ _ _ expr)
            ;; (let*-vals
            ;;  ([env1 (atenv:capture-locs env (zodiac:free-vars E))]
            ;;   [env2 (atenv:unflush env1)]
            ;;   [(ftype-expr env3) (traverse1 expr env2)]
            ;;   [AV (make-constructed-AV-template 
            ;;        template-promise (FlowType->Tvar ftype-expr))]
            ;;   [tvar-E (mk-Tvar 'expr)])
            ;;  (atenv:flush! env3)
            ;;  (new-AV! tvar-E AV)
            ;;  (values (wrap-value tvar-E) env1))]

            [($ zodiac:begin-form _ _ _ _ bodies)
              (recur loop ([env env][bodies bodies])
                (match bodies
                  ;;[() (values (wrap-value (mk-tvar-void)) env)]
                  [(a . d)
                    (let*-vals ([(ftype nuenv) (traverse a env)])
                      (if (null? d)
                        (values ftype nuenv)
                        (loop nuenv d)))]))]
            [($ zodiac:begin0-form _ _ _ _ (start . rest))
              (let*-vals ([(ftype nuenv) (traverse start env)])
                (recur loop ([env nuenv][rest rest])
                  (match rest
                    [() (values ftype env)]
                    [(a . d)
                      (let*-vals ([(ftype nuenv) (traverse a env)])
                        (loop nuenv d))])))]

            ;; --------------------------------------------------------
            ;; MzScheme special forms

            [($ zodiac:unit-form)
              (let* ([env1 (atenv:unflush env)])
                ;; Assume no refs inside unit to imports of enclosing unit
                (values (wrap-value (made-unit (create-atlunit-unit env1 E))) 
                  env))]

            [($ zodiac:compound-unit-form _ s _ _ imports links exports)
             (let*-vals 
              ([time-E (zodiac-time E)]
               [exprs (map cadr links)]
               [(ftype* env) (traverse* exprs env)]
               [time* (map (lambda (e) (max time-E (zodiac-time* e)))
                           exprs)]
               ;; Assume E is closed               
               [ftype* (mapLR extract-1st-value ftype*)])
              (values 
               (wrap-value (made-unit (create-atlunit-cmpd E time* ftype*)))
               env))]

            ;; ### Doesn't deal in first-order fashion
            [($ zodiac:invoke-unit-form _ _ _ _ exp vars)
              (let*-vals ( [(ftype env1) (traverse1 exp env)]
                           ;;[tvar (FlowType->Tvar ftype)]
                           [(ftype* env2) (traverse* vars env1)]
                           [ftypes.times
                             (map (lambda (f) (cons f (current-seconds)))
                               ftype*)]
                           [env1 (atenv:flush! env1)]
                           [env2 (atenv:unflush env1)]
                           [atype-U (apply-unit ftype ftypes.times)]
                           [tvar-E 
                             (match atype-U
                               [($ atunit _ _ result) result]
                               [(? Tvar? tvar-u)
                                 (let ([tvar (mk-Tvar 'invoke-unit-result)])
                                   (new-con! tvar-u
                                     (create-con template-unit 0 tvar #t))
                                   tvar)])])
                (values tvar-E env2))]

            ;; --------------------------------------------------------
            ;; MzScheme special forms

            [($ zodiac:class*/names-form)
              (handle-class*/names-form E env 
                traverse traverse* handle-application)]

            ;; --------------------

            [($ zodiac:poly-form _ _ _ _ exp)   
              (unless (zodiac:parsed-value? exp)
                (mrspidey:error "poly annotation on non-value" exp))
              (let*-vals
                ( [name-for-edge 
                    (match-lambda
                      [(f . t) (cons (Tvar-name f) (Tvar-name t))])]
                  [analyze 
                    (lambda ()
                      (let*-vals 
                        ( [l1 list-ftype]
                          [base-num num-ftype]
                          ;; Capture edges from external mono AVSs 
                          [edges '()]
                          [orig-new-edge! (new-edge-para)]
                          [capture-edge! 
                            (lambda (from to)
                              (unless (eq? from to)
                                (if (< (FlowType-num from) base-num)
                                  ;; Edge from outside constraint set
                                  (set! edges (cons (cons from to) edges))
                                  (orig-new-edge! from to))))]
                          [(env refs tvar)
                            (dynamic-let
                              ([new-edge-para capture-edge!])
                              (let*-vals 
                                ( [(ftype env refs) (traverse-exp exp env)]
                                  [ftype1 (extract-1st-value ftype)]
                                  [tvar (FlowType->Tvar ftype1)])
                                (values env refs tvar)))]
                          [l2 list-ftype])

                        (pretty-debug 
                          `(Poly-def-result 
                             ,(FlowType-name tvar) 
                             num-AVS
                             ,(- (FlowType-num (car l2)) 
                                (FlowType-num (car l1)))
                             ,(FlowType-name (car l2))
                             ,(FlowType-name (car l1))
                             external-edges
                             ,(map name-for-edge edges)))
                        (values env refs tvar l1 l2 edges)))]
                  [handle-edges! (lambda (edges)
                                   (for-each 
                                     (match-lambda 
                                       [(from . to) (new-edge! from to)])
                                     edges))]
                  [(def env)
                    (case (st:polymorphism)
                      [(reanalyze)
                        (mrspidey:error "Reanalyze does not work - env problem")
                        (make-atthunk
                          (lambda ()
                            (let*-vals
                              ([(env refs tvar l1 l2 edges) (analyze)])
                              (for-each
                                (match-lambda
                                  [(binding . _)
                                    (mrspidey:warning "Reference to ~s inside poly form with (st:polymorphism 'reanalyze)" exp 3)])
                                refs)
                              (handle-edges! edges)
                              tvar)))]
                      [(copy-con)
                        (let*-vals ([(env refs tvar l1 l2 edges) (analyze)])
                          (extend-refs! refs)
                          (values
                            (make-schema tvar
                              (filter Tvar? (get-prefix l2 l1))
                              edges)
                            env))]
                      [(compress)
                        (let*-vals
                          ( [(env refs tvar l1 l2 edges) (analyze)]
                            [_ (extend-refs! refs)]
                            ;; Tracked all incoming edges in edges
                            ;; These correspond to upper bindings
                            [U-tvar* (list tvar)]
                            [L-tvar* (map cdr edges)]
                            [old-num-ftype num-ftype]
                            [old-num-AV num-AV]
                            [old-num-edge num-edge]
                            [old-num-con num-con]
                            [(rep-tvar tvar->nu) 
                              (minimize-constraints-&-compare
                                (st:constraint-simplification-poly)
                                L-tvar* U-tvar*
                                l2 l1)]
                            ;; Update edges to point to the compressed set
                            [nu-edges
                              (filter-map
                                (match-lambda
                                  [(and edge (from . to))
                                    (let ([nu-to (tvar->nu to)])
                                      (if nu-to
                                        (cons from (tvar->nu to))
                                        (begin
                                          '(pretty-debug-traverse
                                             `(dropping ,(name-for-edge edge)))
                                          #f)))])
                                edges)])

                          (when (> (- num-ftype old-num-ftype) 250)
                            (printf "Poly def AVS ~s AV ~s edge ~s con ~s~n"
                              (- num-ftype old-num-ftype)
                              (- num-AV old-num-AV)
                              (- num-edge old-num-edge)
                              (- num-con old-num-con)))
                          
                          (values
                            (make-schema (tvar->nu tvar) rep-tvar nu-edges)
                            env))]
                      [(none)
                        (let*-vals ([(env refs tvar l1 l2 edges) (analyze)])
                          (extend-refs! refs)
                          (handle-edges! edges)
                          (values tvar env))])])
                (values (wrap-value def) env))]
 
            [($ zodiac:struct-form _ s _ _ 
               ($ zodiac:symbol _ _ _ tag)
               parent
               (($ zodiac:symbol _ _ _ fields) ...))
              (let*-vals
                ([(parent-ftype env1)
                   (match parent
                     [#f (values
                           (create-fo-FlowType
                             (make-atstruct 'tag '(structure:) 
                               '() '() '() '()))
                           env)]
                     [exp (traverse1 parent env)])])
                (match (FlowType->Atype parent-ftype)
                  [(and atstruct ($ atstruct))
                    (values (handle-struct-form tag #f atstruct fields)
                      env)]
                  [_ 
                    (pretty-debug-traverse `(FlowType->pretty parent-ftype))
                    (mrspidey:warning 
                      (format 
                        "Expression does not analyze to a first-order struct")
                      (zodiac:zodiac-start parent)
                      0)
                    (values (mk-Tvar 'empty) env)]))]

            ;; --------------------------------------------------------
            ;; MrSpidey special forms

            [($ zodiac::-form _ _ _ _ exp type)
             (let*-vals 
              ([(ftype nuenv) (traverse1 exp env)])
              (pretty-debug `(:: ,ftype ,nuenv))
              (match exp
                [($ zodiac:varref _ _ _ _ sym)
                  (let ([name (zodiac:varref-binding  exp)])
                    (if (and (st:flow-sensitive) (Tvar? ftype))
                      (let* ( [type type]
                              [type (match type
                                      [('exact type) type]
                                      [type type])]
                              [etype (expand-input-type type)]
                              [templates (type->templates etype)]
                              [sym (zodiac:binding-var name)])
                        (if templates
                          (let* ( [filter (create-filter #t templates)]
                                  [nutvar (mk-Tvar sym)])
                            (new-con! ftype (create-con-filter filter nutvar))
                            (values (wrap-value nutvar)
                              (atenv:change-binding nuenv name nutvar)))
                          (values (wrap-value ftype) nuenv)))
                      (values (wrap-value ftype) nuenv)))]
                [_ (values (wrap-value ftype) nuenv)]))]

            [($ zodiac:st:control-form _ _ _ _ para val)
             (mrspidey:control-fn para val)
             (values (wrap-value (mk-tvar-void)) env)]

            [($ zodiac:type:-form _ s _ _ type attrs)
              (let ([type (expand-input-type-err type E)])
                (values
                  (wrap-value (apply primitive->atprim 'user type attrs))
                  env))]

            [($ zodiac:reference-unit-form)
              (values 
                (wrap-value (made-unit (create-atlunit-reference E)))
                env)]
            
            [($ zodiac:invoke-open-unit-form)
              (mrspidey:error 
                "MrSpidey does not support invoke-open-unit" E)]

            [($ zodiac:define-values-form)
              (mrspidey:error 
                "MrSpidey does not support internal defines" E)]

            [E 
              (set! traverse:bad-expr E)
              (mrspidey:error "Bad expr in traverse" E)])]

         ;; --------------------

          ;; env-result may contain extra bindings, 
          ;; but they have no effect

         [_ (: result (union Tvar fo-FlowType fo-Atype))]
         [_ (pretty-debug-traverse 
              `(traverse result ,(FlowType->pretty result)))]
         [ftype (link-parsed-ftype! E result)])

         (pretty-debug-traverse-small
           `(<-traverse 
              ,(zodiac:location-offset (zodiac:zodiac-start E))
              ,(zodiac:stripper E)
              ,(atenv->pretty env-result)
              ,(FlowType->pretty ftype)))

        (assert (FlowType? ftype) 'end-traverse ftype)
        (values ftype env-result)))])

   (let-values ([(ftype env) (traverse E env)])
               (values ftype env refs))))

;; ----------------------------------------------------------------------

(define (handle-class*/names-form E start-env 
          traverse traverse* handle-application)
  (let ([handle-paroptarglist
          (lambda (env init-arglist tvar-arg)
            (pretty-debug-object
              `(handle-paroptarglist ,(atenv->pretty env) 
                 ,init-arglist
                 ,(Tvar-name tvar-arg)))
            ;; Bindings are already in atlist
            (let*
              ( [improper? 
                  (or (zodiac:sym-paroptarglist? init-arglist)
                    (zodiac:ilist-paroptarglist? init-arglist))]
                [args (zodiac:paroptarglist-vars init-arglist)]
                [initialize-arg
                  (lambda (env arg tvar)
                    (match arg
                      [(and bind ($ zodiac:binding))
                        (atenv:change-binding env bind tvar)]
                      [((and bind ($ zodiac:binding)) . exp)
                        (let*-vals
                          ( [(ftype env) (traverse exp env)]
                            [ftype (extract-1st-value ftype)])
                          (new-edge! (FlowType->Tvar ftype) tvar)
                          (atenv:change-binding env bind tvar))]))])
              (recur loop ([env env][args args][tvar tvar-arg])
                (cond
                  [(null? args) env]
                  [(and improper? (null? (cdr args)))
                    (initialize-arg env (car args) tvar)]
                  [else
                    (let ( [tvar-car (mk-Tvar 'arg-car)]
                           [tvar-cdr (mk-Tvar 'arg-cdr)])
                      (new-con! tvar (make-con-car tvar-car))
                      (new-con! tvar (make-con-cdr tvar-cdr))
                      (loop 
                        (initialize-arg env (car args) tvar-car)
                        (cdr args)
                        tvar-cdr))]))))])

  (match E
    [($ zodiac:class*/names-form _ _ _ _ 
       this-name 
       super-init-name
       super-expr
       _
       (and init-arglist ($ zodiac:paroptarglist init-vars))
       clauses)
      
      (pretty-debug-object `(init-arglist ,init-arglist))
      
      (let*-vals
        ( ;; --- Work from top down according to class.dvi
          ;; --- Traverse super-exprs
          [(super-ftype env-after-super) (traverse super-expr start-env)]
          [tvar-super (FlowType->Tvar (extract-1st-value super-ftype))]

          ;; extract portions of tvar-super
          [f (lambda (sign)
               (lambda (ndx)
                 (let ([tvar (mk-Tvar 'class-field)])
                   (new-con! tvar-super 
                     (create-con template-internal-class 
                       ndx tvar sign))
                   tvar)))]
          [super-u ((f #t) 0)]
          [super-o ((f #t) 1)] 
          [super-i ((f #t) 2)]
          [super-f ((f #t) 3)]
          [super-b ((f #f) 0)]
          [super-a ((f #f) 1)]
          [super-g ((f #f) 2)]
          [super-t ((f #f) 3)]
          [super-v ((f #t) 4)]

          ;; Transfers control just like lambdas, capture it here
          [captured-env 
            (atenv:capture-locs env-after-super
              (zodiac:free-vars E (atenv:domain env-after-super)))]
          [env (atenv:extend captured-env super-init-name super-i)]
          [env (atenv:unflush env)]

          [super-init-name?
            (lambda (var)
              (and (zodiac:varref? var)
                (eq? (zodiac:varref-binding var) 
                  super-init-name)))]                
          
          ;; Fields for class

          [this-u (mk-Tvar 'this-u)]
          [this-o (mk-Tvar 'this-o)]
          [tvar-args (mk-Tvar 'this-tvar-args)]
          [this-i (mk-Tvar-init-AV 'this-i
                    (make-AV-lam tvar-args (mk-tvar-void) 0 #t))]
          [this-b (mk-Tvar 'this-b)]
          [this-a (mk-Tvar 'this-a)]
          [this-f (mk-Tvar 'this-f)]
          [this-g (mk-Tvar 'this-g)]
          [this-t (mk-Tvar 'this-t)]
          [this-v (mk-Tvar 'this-v)]
          [tvar-class
            (mk-Tvar-init-AV 'class
              (create-AV template-internal-class '()
                (vector this-u this-o this-i this-f this-v)
                (vector this-b this-a this-g this-t)))]

          ;; Anything not clause dependant

          [_ (new-edge! super-u this-u)]
          [_ (new-edge! this-t super-t)]

          ;; Now to do the init args and clauses

          ;; --- Build up environment
          [env (atenv:extend env this-name this-t)]
          ;; args
          [env (foldr 
                 (lambda (var env)
                   (atenv:extend env 
                     (if (zodiac:binding? var)
                       var
                       (car var))
                     (mk-Tvar 'init-var)))
                 env
                 init-vars)]

          ;; --- helper stuff
          [tvar-undef (mk-Tvar-init-AV 'delta AV-undefined)]
          [ivar-put
            (lambda (to sym tvar)
              (new-AV! to
                (create-AV
                  (get-ivar-template sym)
                  '() (vector tvar) (vector))))]
          [ivar-get
            (lambda (from sym tvar)
              (new-con! from
                (create-con
                  (get-ivar-template sym)
                  0 tvar #t)))]

          ;; Traverse clauses, alloc a-j etc, extend env 
          ;; and find super-init call

          [super-init-expr #f]
          [_ (pretty-debug-object `(calc clause-info))]
          [clause-info
            (map-clause
              (lambda (public? define? override?
                        export internal expr import)
                (let* ( [a-j (mk-Tvar 'a-j)]
                        [b-j (mk-Tvar 'b-j)]
                        [r-j (mk-Tvar 'r-j)]
                        [g-j (mk-Tvar 'g-j)])
                  (match expr
                    ;; straight application
                    [($ zodiac:app _ _ _ _ (? super-init-name?) _)
                      (set! super-init-expr expr)]
                    ;; apply
                    [($ zodiac:app _ _ _ _ 
                       (and fn ($ zodiac:varref _ _ _ _ 'apply))
                       ((? super-init-name?) . _))
                      (match (FlowType->Atype 
                               (atenv:lookup env (zodiac:varref-binding fn)))
                        [($ atprim 'apply) 
                          (set! super-init-expr expr)]
                        [_ (void)])]
                    [_ (void)])
                  (when internal
                    (zodiac:set-binding-mutated! internal #t)
                    (set! env (atenv:extend-mutated env internal b-j g-j))
                    (link-parsed-ftype! internal r-j))
                  (list
                    public? define? override?
                    export internal expr import
                    a-j b-j r-j g-j)))
              clauses)]
          [_ (pretty-debug-object `(got clause-info))]


          ;; Ok, we have the full environment
          ;; Handle the argument list

          [env (handle-paroptarglist env init-arglist tvar-args)]

          ;; --- create constraints for each clause
          [super-init-called #f]
          [public-templates          '()]
          [keyw-local-templates      '()]
          [before-not-flow-templates '()]
          [after-not-flow-templates  '()]

          [extend-before-not-flow-templates!
            (lambda (sym)
              (set! before-not-flow-templates
                (cons (get-ivar-template sym) 
                  before-not-flow-templates)))]
          [extend-after-not-flow-templates!
            (lambda (sym)
              (set! after-not-flow-templates
                (cons (get-ivar-template sym) 
                  after-not-flow-templates)))]

          [_
            (for-each
              (match-lambda
                [( public? define? override?
                   export internal expr import
                   a-j b-j r-j g-j)
                  (pretty-debug-object
                    `(clause (,public? ,define? ,override?)
                       ,export ,internal ,expr ,import
                       ,(map FlowType->pretty (list a-j b-j r-j g-j))))
                  (new-edge! a-j b-j)
                  (when public?
                    (ivar-put this-u export tvar-undef)
                    (ivar-put this-o export r-j)
                    (ivar-get this-b export b-j)
                    (ivar-get this-a export a-j)
                    (ivar-get this-g export g-j)
                    (ivar-put this-f export g-j)
                    (set! public-templates
                      (cons (get-ivar-template export) 
                        public-templates)))
                  (when (not public?)
                    (new-edge! tvar-undef b-j)
                    (new-edge! r-j a-j))
                  (when (not define?)
                    (ivar-get super-o import r-j))
                  (when override?
                    (extend-after-not-flow-templates! export)
                    (if (and super-init-expr (not super-init-called))
                      (begin
                        ;; super-init will be called after this init
                        (ivar-put super-b export a-j)
                        (ivar-put super-a export a-j)
                        (extend-before-not-flow-templates! export))
                      (begin
                        ;; super-init may be called before this init
                        (ivar-put super-a export b-j))))
                  (when (and public? (not override?))
                    (when define?
                      ;; Prev value hidden, but define super-b, super-a
                      (ivar-put super-b export tvar-undef)
                      (let ([t (mk-Tvar 't)])
                        (ivar-get super-o export t)
                        (ivar-put super-a export t))
                      (extend-before-not-flow-templates! export)
                      (extend-after-not-flow-templates! export)
                      (set! keyw-local-templates
                        (cons (get-ivar-template export) 
                          keyw-local-templates))))
                  (when define?
                    (if (eq? expr super-init-expr)

                      (let*-vals
                        ([(ftype nu-env) (handle-application expr env #f)])
                        (link-parsed-ftype! expr (wrap-value (mk-tvar-void)))
                        (pretty-debug-object '(super-init called))
                        (set! super-init-called #t)
                        (set! env nu-env)
                        (new-AV! r-j AV-void)

                        ;; Update env that super-init called
                        (for-each
                          (match-lambda
                            [( _ #f _ _ internal _ import a-j b-j r-j g-j)
                              (set! env 
                                (atenv:change-binding env internal a-j))]
                            [_ (void)])
                          clause-info))
                                   
                      (let*-vals 
                        ( [(ftype nu-env) (traverse expr env)]
                          [ftype (extract-1st-value ftype)])
                        (pretty-debug-atenv 
                          `(env-before-traverse ,(atenv->pretty env)))
                        (set! env nu-env)
                        (pretty-debug-atenv 
                          `(env-after-traverse ,(atenv->pretty env)))
                        (new-edge! (FlowType->Tvar ftype) r-j)
                        (when internal
                          (set! env
                            (atenv:change-binding env internal a-j))))))

                  (pretty-debug-atenv 
                    `(end-clause ,(atenv->pretty env)))
                  ])

              clause-info)]
          
          ;; record ivarset
          [public-ivars
            (filter-map
              (match-lambda
                [( public? define? override?
                   export internal expr import
                   a-j b-j r-j g-j)
                  (and public? export)])
              clause-info)]
          [_ (new-AV! this-v
               (create-AV 
                 template-ivarset public-ivars
                 (vector super-v) mt-vector))]

          [_ (begin
               (new-con! super-o
                 (create-con-filter
                   (create-filter #f public-templates)
                   this-o))
               (new-con! super-f
                 (create-con-filter
                   (create-filter #f public-templates)
                   this-f))
               (new-con! this-b
                 (create-con-filter
                   (create-filter #f before-not-flow-templates)
                   super-b))
               (new-con! this-a
                 (create-con-filter
                   (create-filter #f after-not-flow-templates)
                   super-a))
               (new-con! this-g
                 (create-con-filter
                   (create-filter #f keyw-local-templates)
                   super-g)))])

        (pretty-debug-object `(keyw-local-templates ,keyw-local-templates))
        (pretty-debug-object `(tvar-class ,(FlowType->pretty tvar-class)))

        (atenv:flush! env)

        (values (wrap-value tvar-class) captured-env))])))

;; ----------------------------------------

(define (map-clause fn clauses)
  (apply append
    (map
      (match-lambda
        [($ zodiac:public-clause exports internals exprs)
          (map 
            (lambda (export internal expr)
              (fn #t #t #t (zodiac:read-object export) internal expr #f))
            exports internals exprs)]
        [($ zodiac:private-clause internals exprs)
          (map 
            (lambda (internal expr)
              (fn #f #t #f   #f internal expr #f))
            internals exprs)]
        [($ zodiac:inherit-clause internals imports)
          (map 
            (lambda (internal import)
              (fn #t #f #f   
                (zodiac:read-object import) internal #f 
                (zodiac:read-object import)))
            internals imports)]
        [($ zodiac:rename-clause internals imports)
          (map 
            (lambda (internal import)
              (fn #f #f #f   #f internal #f (zodiac:read-object import)))
            internals imports)]
        [($ zodiac:sequence-clause exprs)
          (map 
            (lambda (expr)
              (fn #f #t #f   #f #f expr #f))
            exprs)])
      clauses)))

;; ----------------------------------------------------------------------

(define (handle-struct-form tag const atstruct fields)
  (match atstruct
    [($ atstruct 
        parent-struct:sym
        super-constructors 
        parent-gen-args
        parent-match-args
        parent-field-types
        parent-list-mutable)
     (match-let*
         ([constructor (symbol-append 'structure: tag)]
          [gen-args (map (lambda (_) (gensym)) fields)]
          [match-args 
           (map (lambda (field gen-arg)
                  (match field
                    [((or '! ':) _ type) `(intersect ,gen-arg ,type)]
                    [_ gen-arg]))
                fields gen-args)]
          [field-names
           (map (lambda (field)
                  (match field
                    [((or ': '!) (? symbol? name) _) name]
                    [(? symbol? name) name]
                    [_ (mrspidey:error
                        (format "Bad define-typed-structure field ~s" 
                                field))]))
                fields)]
          [list-mutable
           (map (lambda (field)
                  (match field
                    [('! (? symbol? name)) #t]
                    [_ (not const)]))
                fields)]
          [field-types 
           (map 
            (match-lambda [((or '! ':) _ type) (expand-input-type type)]
                          [_ 'top])
            fields)]

          [all-gen-args (append parent-gen-args gen-args)]
          [all-match-args (append parent-match-args match-args)]
          [all-field-types (append parent-field-types field-types)]
          [all-list-mutable (append parent-list-mutable list-mutable)]
          [_-parent-args (map (lambda (_) '_) parent-gen-args)]

          [gen-arg (gensym)]
          [defs `()]
          [add-def! 
           (lambda (fo-Atype)
             (set! defs (cons (create-fo-FlowType fo-Atype) defs)))]
          [add! 
           (lambda (type . attrs)
             (add-def! 
              (apply primitive->atprim 'define-struct type attrs)))])

       ;; (pretty-print match-args)
       ;; (pretty-print gen-args)

       (add-def! (make-atstruct
                  (symbol-append 'struct: tag)
                  (cons constructor super-constructors)
                  all-gen-args all-match-args
                  all-field-types all-list-mutable))
    
       (let ([template 
               (apply constructor->template constructor all-list-mutable)])
         (for-each 
           (lambda (sc) (record-super-constructor-of-template! sc template))
           super-constructors)
         (extend-constructor-env! template))

       (add! 
        `(forall ,all-gen-args 
                 (,@all-match-args -> (,constructor ,@all-gen-args))))
       (add! `(_ -> bool) `(predicate ,constructor))
       (for n 0 (length fields)
            (let* ([gen-arg (list-ref gen-args n)]
                   [field-name (list-ref field-names n)])
              (add!
               `(forall (,gen-arg)
                        ((,constructor ,@_-parent-args
                                       ,@(map-with-n
                                          (lambda (field m) 
                                            (if (= n m) gen-arg '_))
                                          fields))
                         -> ,gen-arg)))
              (when (list-ref list-mutable n)
                (add! `(forall (,gen-arg)
                               ((,constructor ,@_-parent-args
                                              ,@(map-with-n
                                                 (lambda (field m)
                                                   (if (= n m)
                                                       `(! ,gen-arg)
                                                       '_))
                                                 fields))
                                ,(list-ref match-args n)
                                -> void))))))

       (create-fo-FlowType (make-atvalues (reverse defs))))]))

;; ======================================================================

(define (zodiac:parsed-1st-ftype exp)
  (extract-1st-value (zodiac:parsed-ftype exp)))

;; ----------

(define (if-splitting-env test env)
  ;; Returns (env-true env-false)
  ;; Assumes test expression already traversed
  ;; Can't lookup things in env, cause not defined for imported vars
  (match test
    ;; arg
    [($ zodiac:varref _ _ _ _ sym)
      (let ([arg (zodiac:varref-binding  test)])
        (match (zodiac:parsed-1st-ftype test)
          [(? Tvar? tvar)
            (let* ([tvar-then (mk-Tvar sym)]
                    [tvar-else (mk-Tvar sym)]
                    [templates (list (lookup-template 'false))])
              (new-con! tvar
                (create-con-filter (create-filter #f templates) tvar-then))
              (new-con! tvar
                (create-con-filter (create-filter #t templates) tvar-else))
              (values (atenv:change-binding env arg tvar-then)
                (atenv:change-binding env arg tvar-else)))]
          ;; Annotated type so don't change
          [_ (values env env)]))]
        
    ;; (not (pred? args ...))
    [($ zodiac:app _ _ _ _ fn (pred-exp))
     (=> fail)
     (match (zodiac:parsed-atprim fn)
       [($ atprim 'not)
         ;; Use recursive call and reverse
         (let-values
             ([(env-true env-false) (if-splitting-env pred-exp env)])
             (values env-false env-true))]
       [_ (fail)])]

    ;; (pred? args ...)
    [($ zodiac:app _ _ _ _ fn args)
     (=> fail)
     (match (zodiac:parsed-atprim fn)
       [($ atprim _ type _ predicate-fn)
        ;;(pretty-print `(if (pred ,sym ,type ,predicate-fn ...)))
        (recur loop ([args-before '()]
                     [args args]
                     [env-true env]
                     [env-false env])
          (match args
            [() (values env-true env-false)]
            [((and arg ($ zodiac:varref)) . rest-args)
              (=> fail)
              (let ([barg (zodiac:varref-binding arg)])
                (match (zodiac:parsed-1st-ftype arg)
                  [(? fo-FlowType?) (fail)]
                  [(? Tvar? tvar)
                    (let*
                      ( [tvars-before (map zodiac:parsed-1st-ftype args-before)]
                        [tvars-after (map zodiac:parsed-1st-ftype rest-args)]
                        [tvar-true 
                          (predicate-fn tvars-before tvars-after tvar #t)]
                        [tvar-false
                          (predicate-fn tvars-before tvars-after tvar #f)])
                      ;;(pretty-print-debug (list tvar-true tvar-false))
                      (loop (append args-before (list arg))
                        rest-args
                        (if tvar-true
                          (atenv:change-binding env-true barg tvar-true)
                          env-true)
                        (if tvar-false
                          (atenv:change-binding env-false barg tvar-false)
                          env-false)))]))]
            [(arg . rest-args)
             (loop (append args-before (list arg))
                   rest-args env-true env-false)]))]
       [_
        ;; Not a primitive
        (fail)])]

    [_ (values env env)]))

;; ----------------------------------------------------------------------

(define (flow-sensitive-env pi args env)
  (match pi
    [($ atprim _ _ domain-filters)
     ;; Walk domain-filters and args
     (recur loop 
         ([env env]
          [args args]
          [domain-filters domain-filters])
       (match (list args domain-filters)
         [( ((and arg ($ zodiac:varref _ _ _ _ arg-sym))
              . args-rest)
            ((? filter? filter) . filters-rest))
           (=> fail)
           (let ([barg (zodiac:varref-binding arg)])
             (match (FlowType->Tvar (zodiac:parsed-1st-ftype arg))
               [(? fo-FlowType?)
                 ;; Assigned, so don't track
                 (fail)]
               [(? Tvar? tvar)
                 ;; Have arg and domain
                 (let ([nu-tvar (mk-Tvar arg-sym)])
                   (new-con! tvar (create-con-filter filter nu-tvar))
                   (loop (atenv:change-binding env barg nu-tvar) 
                     args-rest filters-rest))]))]
             
          [( (_ . args-rest) (_ . filters-rest))
           (loop env args-rest filters-rest)]
             
          [_ env]))]
     [#f env]))

;; ======================================================================
;; Constants -> constraints
;; All fns return Tvar

(define (traverse-const V)
  ;;(set! gV V)
  ;;(pretty-print (zodiac:stripper V))
  ;; Returns nothing
  (let ([s (zodiac:const-size V)])
    (if (>= s (st:const-merge-size))
        (traverse-consts-tidy (list V))
        (let ([Tvar (mk-Tvar 'traverse-const)])
          (new-AV! Tvar (traverse-const-exact V))
          Tvar))))

(define traverse-consts-tidy
  ;; Takes a list of constants
  ;; Returns a Tvar
  (lambda (V*)
    (match-let*
      ( [Tvar (mk-Tvar 'traverse-consts-tidy)]
        [vec-elems
          (apply append
            (map
              (match-lambda
                [($ zodiac:vector _ _ _ v) v]
                [(? vector? v) (vector->list v)]
                [_ '()])
              V*))]
        [elems.cdr*
          (filter-map
            (match-lambda
              [(or ($ zodiac:list _ _ _ l)
                 (? pair? l)
                 (? null? l))
                (recur loop ([l l][elems '()])
                  (cond
                    [(pair? l) (loop (cdr l) (cons (car l) elems))]
                    [else (cons elems l)]))]
              [($ zodiac:improper-list _ _ _ l)
                (recur loop ([l l][elems '()])
                  (cond
                    [(and (pair? l) (null? (cdr l)))
                      (cons elems (car l))]
                    [(pair? l)
                      (loop (cdr l) (cons (car l) elems))]
                    [else (cons elems l)]))]
              [_ #f])
            V*)]
        [elems (apply append (map car elems.cdr*))]
        [cdrs (map cdr elems.cdr*)])

      (unless (null? vec-elems)
        (new-AV! Tvar
          (make-AV-vec (traverse-consts-tidy vec-elems))))
      (unless (null? elems)
        (let* ([tvar-a (traverse-consts-tidy elems)]
                [tvar-d (traverse-consts-tidy cdrs)]
                [AV (make-AV-cons tvar-a tvar-d)])
          (new-AV! tvar-d AV)
          (new-AV! Tvar AV)))
      
      (for-each
        (lambda (V)
          (let ([x (traverse-simple-const V)])
            (when x (new-AV! Tvar x))))
        V*)

      Tvar)))

;; ======================================================================

