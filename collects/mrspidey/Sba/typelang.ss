;; typelang.ss
;; Parses a type to produce a set of constraints
;; Also tests an Tvar for membership in a type
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
;; type ::= ...
;; schema ::= type | (forall vars type)
;; tschema ::= schema | (case-> schema ...) 
;; lambda types are
;;      (a b c -> d e f)
;;      (a b c rest *-> d e f)
;;      (a b c ->* results)
;;      (a b c rest *->* results)
;;      (lambda domain range)
;; ======================================================================
; Union, Intersect etc

(define absUnion
  (lambda (exps)
    (match 
        (apply append
               (map
                (match-lambda
                 ['empty '()]
                 [('union . exps) exps]
                 [exp (list exp)])
                ;;(list->set-equal? exps)
                exps
                ))
      [() 'empty]
      [(e) e]
      [exps `(union ,@exps)])))

(define absunion (lambda exps (absUnion exps)))

(define absIntersect
  (lambda (exps)
    (match 
        (apply append
               (map
                (match-lambda
                 ['_ '()]
                 [('intersect . exps) exps]
                 [exp (list exp)])
                (list->set-equal? exps)))
      [() '_]
      [(e) e]
      [exps `(intersect ,@exps)])))

(define absintersect (lambda exps (absIntersect exps)))

;; ======================================================================

(define (output-type-constructor? C)
  (or
    (memq C '(bool listof list atom sexp values))
    (type-constructor? C)))

(define (output-type-expander type)
  (all-output-type-expanders
    (or
      (compat-type-once type output-type-expander)
      type)))

;---
(define output-type-expanders '())

(define (all-output-type-expanders type)
  (foldl
    (lambda (expander type) (expander type))
    type
    output-type-expanders))

;---

(define (install-output-type-expander! expander)
  (set! output-type-expanders (cons expander output-type-expanders)))

(define (init-output-type-expander!)
  
  (install-output-type-expander!
    (match-lambda 
      [((? output-type-constructor? C)) C]
      [('cons t 'nil) `(list ,t)]
      [('cons t ('list . t*)) `(list ,t ,@t*)]
      [('lambda dom rng) (list dom '*->* rng)]
      [('unit-result* ('values rng)) `(unit-result ,rng)]
      [('union X) X]
      [(? list? t)
        (=> fail)
        (match (reverse t)
          [( ('values x) 
             (and arrow (or '*->* '->*))
             . domains)
            (append (reverse domains) 
              (list (case arrow
                      [(*->*) '*->]
                      [(->*) '->])
                x))]
          [_ (fail)])]
      [(('list . args) '*-> . results) `(,@args -> . ,results)]
      [(('list . args) '*->* . results) `(,@args ->* . ,results)]
      [('union . X)
        (=> fail)
        (cond
          [(and (memq 'true X) (memq 'false X))
            `(union bool ,@(remq 'true (remq 'false X)))]
          [(and (memq 'bool X)
             (or (memq 'true X) (memq 'false X)))
            `(union ,@(remq 'true (remq 'false X)))]
          [else (fail)])]
      [(or ('union 'true 'false) ('union 'false 'true)) 'bool]
      
      [('mvalues ('list . x)) `(values ,@x)]
      [('rec () body) body]
      [(or 
         ('union 'nil ('cons X ('listof Y)))
         ('union ('cons X ('listof Y)) 'nil))
        (=> fail)
        (if (equal? X Y) `(listof ,Y) (fail))]
      [('epsilon X) X]
      [((and combine (or 'union 'intersect)) . parts)
        (=> fail)
        (if (not (st:sdl-tidy))
          (fail)
          (let ([combine-fn (case combine
                              [(intersect) absintersect]
                              [(union) absunion])])
            (pretty-debug-sdl `(combine ,combine parts ,parts))
            (apply combine-fn
              (recur loop ([parts parts])
                (match parts
                  [(a . d)
                    (let ([d (loop d)])
                      (if (member a d)
                        d
                        (match a
                          [((? output-type-constructor? C) . C-args)
                            (let*-vals
                              ( [len-C-args (length C-args)]
                                [(args* rest)
                                 (filter-map-split
                                   (match-lambda
                                     [(C2 . args)
                                       (and 
                                         (eq? C C2)
                                         (= len-C-args (length args))
                                         args)]
                                     [_ #f])
                                   parts)])
                              (pretty-debug-sdl `(args* ,args* rest ,rest))
                              (if (null? (cdr args*))
                                (cons a d)
                                (cons (cons C (apply map
                                                combine-fn
                                                args*)) 
                                  rest)))]
                          [_ (cons a d)])))]
                  [() '()])))))]
      [type type]))
  )



;; ======================================================================
;; For input-type-expander, recursion is done in expand-input-type

(define input-type-expander 'input-type-expander)

(define (init-input-type-expander!)
  (set! input-type-expander (lambda (x) x))
  (let ([loop (lambda (x) x)])
    (install-input-type-expander!
      (match-lambda
        ['atom      (loop '(union nil num sym str char true false))]
        ['sexp      (let ([u (gensym)])
                      (loop `(MU ,u (union atom (box ,u) (cons ,u ,u) (vec ,u)))))]
        ['bool      (loop '(union false true))]
        [('arg a b) (loop (list 'cons a b))]
        ['noarg     (loop 'nil)]
        ['null      (loop 'nil)]
        [('arglistof t)
          (let ((u (gensym)))
            (loop `(MU ,u (union noarg (arg ,t ,u)))))]
        [('arglist) (loop 'noarg)]
        [('arglist t . t*) (loop `(arg ,t (list ,@t*)))]
        [('listof t)
          (let ((u (gensym)))
            (loop `(MU ,u (union nil (cons ,t ,u)))))]
        [('list) (loop 'nil)]
        [('list t . t*) (loop `(cons ,t (list ,@t*)))]

        [('MU (? typevar? a) t)
          (loop `(rec ([,a ,t]) ,a))]
        [(? type-constructor? C) (loop (list C))]
        [(? (lambda (t) (and (list? t) (memq '-> t)))
           t)
          ;; Make the *-> function
          (recur loop2 ([wrapper-fn (lambda (x) x)]
                         [t t])
            (match t
              [('-> . results)
                (loop `(,(wrapper-fn 'noarg) *-> ,@results))]
              [('optional y . rest)
                (loop2 (lambda (x) (wrapper-fn `(union noarg (arg ,y ,x))))
                  rest)]
              [(y . rest)
                (loop2 (lambda (x) (wrapper-fn `(arg ,y ,x)))
                  rest)]))]
        [(? (lambda (t) (and (list? t) (memq '*-> t)))
           t)
          ;; Make the *->* function
          (recur loop2 ( [wrapper-fn (lambda (x) x)]
                         [t t])
            (match t
              [('*-> . _) (mrspidey:error "*-> type w/o domain")]
              [(rest '*-> . results)
                (loop `(,(wrapper-fn rest) 
                         *->*
                         (mvalues
                           ,(foldr (lambda (arg rest) `(cons ,arg ,rest))
                              '(nil)
                              results))))]
              [(y . rest)
                (loop2 (lambda (x) (wrapper-fn `(arg ,y ,x)))
                  rest)]))]
        [(? (lambda (t) (and (list? t) (memq '->* t))) t)
          (match (reverse t)
            [(result '->* . domain)
              `( (list ,@(reverse domain)) *->* ,result)]
            [_ (mrspidey:error "Bad type ~s" t)])]
        [(dom '*->* rng) (loop `(lambda ,dom ,rng))]
        [type type]))))

(define (install-input-type-expander! expander)
  (let ([old-expander input-type-expander])
    (set! input-type-expander
        (lambda (type) 
          ;;(pretty-print-debug `(input-type-expander ,type))
          (old-expander (expander type))))))

;; ======================================================================

(define typevar?
  (lambda (v)
    (and (symbol? v)
         (not (output-type-constructor? v))
         (not (memq v '(_ union rec -> *-> ->* *->*))))))

(define (compat-type-once type cl-fn)
  (match type
    [(and c (or (? number?) (? char?) ((or 'quote '#%quote) (? symbol?)))) c]
    [('rec bind t2)
      `(rec ,(map (match-lambda [(a t) (list a (cl-fn t))]) bind) 
         ,(cl-fn t2))]
    [('forall vars type)
      `(forall ,vars ,(cl-fn type))]
    [('case-> . (and types (? list?) (_ . _))) 
      `(case-> ,@(map cl-fn types))]
    [('union . (? list? union)) `(union ,@(map cl-fn union))]
    [('scheme: E) type]
    [('intersect . (? list? union)) `(intersect ,@(map cl-fn union))]
    [(? symbol? a) a]
    [((? (lambda (t) (memq t '(object class))) t) . ivar-types)
      (cons t
        (map
          (match-lambda
            [(ivar type) (list ivar (cl-fn type))])
          ivar-types))]
    ;; check constructors
    [((? output-type-constructor? C)  . args)
      `(,C ,@(map (match-lambda
                    [('! arg) (list '! (cl-fn arg))]
                    [arg (cl-fn arg)])
               args))]
    [(? (lambda (t) 
          (and (list? t)
            (or (memq '-> t) (memq '*-> t) (memq '->* t) (memq '*->* t)))) 
       t)
      (map cl-fn t)]
    [_ #f]))

(define (expand-input-type type)
  ;; Do rewriting transformations
  ;; (pretty-print-debug `(expand-type ,type))
  (let ([t2 (input-type-expander type)])
    (if (eq? type t2)
        (or (compat-type-once type expand-input-type)
            (mrspidey:error (format "Invalid type syntax ~s" type)))
        (expand-input-type t2))))

(define (expand-input-type-err type at)
  ;; Do rewriting transformations
  ;; (pretty-print-debug `(expand-type ,type))
  (let ([t2 (input-type-expander type)])
    (if (eq? type t2)
        (or (compat-type-once type 
              (lambda (type) (expand-input-type-err type at)))
            (mrspidey:error (format "Invalid type syntax ~s" type) at))
        (expand-input-type-err t2 at))))

(define (expand-output-type type)
  ;; Do rewriting transformations
  (pretty-debug `(expand-output-type ,type))
  (letrec
    ([local-expand-output-type
       (lambda (type)
         (let* ([type (or (compat-type-once type local-expand-output-type)
                        type
                        (mrspidey:error (format "Bad output type ~s" type)))])
           (let ([t2 (output-type-expander type)])
             (if (equal? type t2)
               type 
               (local-expand-output-type t2)))))])
    (if (st:expand-output-type)
      (match (local-expand-output-type type)
        [('values x) x]
        [('rec bindings ('values x)) `(rec ,bindings ,x)]
        [x x])
      type)))

;; ----------------------------------------------------------------------

(define (type->templates con-exp)
  ;; Assumes con-exp already expanded
  (match con-exp
    [('union . E*)
     (let* ([T* (map type->templates E*)])
       (if (andmap (lambda (x) x) T*)
           (apply union T*)
           #f))]
    [('intersect . E*)
     (let* ([T* (map type->templates E*)]
            [T* (filter (lambda (x) x) T*)])
       (apply intersect T*))]
    [('forall _ type) (type->templates type)]
    [(or (C . _) C)
     (if (type-constructor? C)
         (list (lookup-template C))
         #f)]))

;; ----------------------------------------------------------------------

(define ( generic-type->con type prefix forall-env sign)
  ;; Assumes type is already expanded
  (letrec
      ([mk-Tvar-tmp (lambda () (mk-Tvar (symbol-append prefix ':tc)))]
       [parse-type-generic
        (lambda (t env sign)
          ;; sign is #t for +ive, #f for -ive
          ;; (pretty-print-debug `(parse-type-generic ,t ,env ,sign))
          (let ([new-sign-edge!
                 (lambda (from to)
                   (if sign
                       (new-edge! from to)
                       (new-edge! to from)))]
                [parse-type 
                 (lambda (t) 
                   (let ([r (parse-type-generic t env sign)])
                     ;;(pretty-print-debug `(parse-type-generic ,t ,(Tvar-name r)))
                     r))])
            (match t

              ;; Recursive definitions
              [('rec bind t2)
                (let* ( [a* (map car bind)]
                        [env env]
                        [mk-tvar*
                          (map 
                            (lambda (a rhs) 
                              (letrec
                                ([f (lambda (sign)
                                      (pretty-debug `(rec ,a ,sign))
                                      (let* 
                                        ( [sym (symbol-append prefix ': a)]
                                          [tvar (mk-Tvar sym)]
                                          [_ (set! f
                                               (lambda (sign2)
                                                 (unless (eq? sign sign2)
                                                   (mrspidey:error
                                                     (format "rec-bound variable ~s referenced in covariant and contravariant positions in type ~s" a type)))
                                                 tvar))]
                                          [tvar2
                                            (parse-type-generic rhs env sign)])
                                        (if sign
                                          (new-edge! tvar2 tvar)
                                          (new-edge! tvar tvar2))
                                        tvar))])
                                (lambda (sign) (f sign))))
                            a* (map cadr bind))])
                  (set! env (extend-env* env a* mk-tvar*))
                  (parse-type-generic t2 env sign))]

              ;; Unions
              [('union . (? list? union))
               (let ([Tvar (mk-Tvar-tmp)])
                 (for-each
                  (lambda (t)
                    (new-sign-edge! (parse-type-generic t env sign) Tvar))
                  union)
                 Tvar)]

              ;; Scheme
              ;[('scheme: E)
              ;  (assert (eq? sign #t) 'scheme:)
              ;  (let*-vals
              ;    ( [p (open-output-string)]
              ;      [_ '(pretty-print E p)]
              ;      [p (close-output-port)]
              ;      [s (get-output-string p)]
              ;      [p (open-input-string s)]
              ;      [E ((zodiac:read p))]
              ;      [defs (my-scheme-expand-program (list E))]
              ;      [(ftype _ _) (traverse-exp (car defs) atenv:empty)]
              ;      [tvar (FlowType->Tvar ftype)])
              ;    tvar)]

              [('intersect . (? list? intersect))
               (when sign
                 (mrspidey:error "Intersection in covariant position"))
               (let ([Tvar (mk-Tvar-tmp)])
                 (for-each
                  (lambda (t)
                    (new-edge! Tvar (parse-type-generic t env sign)))
                  intersect)
                 Tvar)]

              ;; type variables
              ['_ 
               (when sign 
                 (mrspidey:error "_ type in covariant position"))
               (mk-Tvar-tmp)]

              ['empty
               (unless sign
                   (mrspidey:error 
                    (format "empty in contravariant position in ~s" type)))
               (mk-Tvar-tmp)]

              [(? symbol? a)
                (cond
                  [(lookup-or-#f env a) 
                    => (lambda (mk-tvar) (mk-tvar sign))]
                  [else
                    (or
                      (lookup-or-#f forall-env a)
                      (let ([Tvar (mk-Tvar a)])
                        (if sign 
                          (add-global-tref! a Tvar)
                          (add-global-tbang! a Tvar))
                        Tvar))])]

              [('lambda arg res)
               (let ([Tvar (mk-Tvar-tmp)])
                 (if sign
                     ;; make the AV
                     (new-AV! Tvar
                              (make-AV-lam 
                               (parse-type-generic arg env #f)
                               (parse-type-generic res env #t)
                               0 #t))
                     ;; extract the various components
                     (begin
                       (new-con! Tvar
                         (make-con-dom (parse-type-generic arg env #t)))
                       (new-con! Tvar
                         (make-con-rng
                           (parse-type-generic res env #f)))))
                 Tvar)]

              ;; constants
              [(or (? number? c) (? char? c) 
                   ((or 'quote '#%quote) (? symbol? c)))
               (let ([Tvar (mk-Tvar-tmp)])
                 (if sign (new-AV! Tvar (traverse-simple-const c)))
                 Tvar)]

              ;; check constructors
              [((? type-constructor? C) . args)
               (match-let*
                   ([(and template ($ template type n+ n- ref assign))
                     (lookup-template C)]
                    [Tvar (mk-Tvar-tmp)])
                 (unless (= (vector-length ref) (length args))
                   (mrspidey:error
                    (format "Constructor ~s given ~s arg(s), expected ~s" 
                            C (length args) (vector-length ref))))
                 (if sign
                     ;; make the AV, unless void and (st:see-void) is false
                     (unless (and (eq? C 'void) (not (st:see-void)))
                       (let ([args 
                              (map 
                               (lambda (a) (parse-type-generic a env sign)) 
                               args)])
                         ;;(pretty-print-debug `(,C ,@(map Tvar-name args)))
                         (new-AV! 
                          Tvar
                          (apply make-constructed-AV C args))))
                     ;; DO SOMETHING FOR NULL ARGS

                     ;; extract the various components
                     ;; need to do unification on mutable fields,
                     ;; unless pattern matching arg is (! a)
                     (for-each-with-n
                      (lambda (arg n)
                        (match arg
                          [(or '_ 'top)(void)]
                          [('! a)
                            (match (vector-ref assign n)
                              [#f
                                (mrspidey:error
                                  "! used on immutable field in type")]
                              [n
                                ;; Inject only
                                (new-con!
                                  Tvar
                                  (create-con template n
                                    (parse-type-generic a env #t) #f))])]
                          [arg
                            ;; Extract 
                            (new-con!
                              Tvar
                              (create-con template (vector-ref ref n)
                                (parse-type-generic arg env #f) #t))]))
                      args))
                 Tvar)]
              [_
               ;; Didn't match any type
               (mrspidey:error (format "invalid type syntax ~s" t))])))])

    (let ([Tvar (parse-type-generic type empty-env sign)])
      Tvar)))

;; ----------------------------------------------------------------------

(define (split-schema type)
  (match type
    [('forall vars type)
     (values vars type)]
    [type (values '() type)]))

(define (schema->env+type type)
  (let-values ([(vars type) (split-schema type)])
    (values
     (map (lambda (v) (cons v (mk-Tvar v))) vars)
     type)))

(define (schema->con schema Tvar prefix top-misc)
  (let-values ([(forall-env type) (schema->env+type schema)])

    (new-edge! (generic-type->con type prefix forall-env #t) Tvar)

    ;; Put top-misc in misc field of each AV
    (for-each
     (lambda (AV) (set-AV-misc! AV top-misc))
     (get-Tvar-objs Tvar))))

;; ----------------------------------------------------------------------

(define (tschema->con tschema Tvar prefix top-misc)
  (schema->con
   (match tschema
     [('case-> . schema*) (rac schema*)]
     [schema schema])
   Tvar prefix top-misc))

;; ----------------------------------------------------------------------

(define (tschema->con-for-nargs tschema Tvar prefix top-misc n)
  ;; Assumes type is already expanded

  (match tschema
    [('case-> . schema*)
      (or
        (ormap
          (lambda (schema) 
            (let-values ([(forall-env type) (schema->env+type schema)])
              (if (function-takes-nargs? type n)
                (schema->con schema Tvar prefix top-misc)
                #f)))
          schema*)
        (schema->con (rac schema*) Tvar prefix top-misc))]
    [schema 
      (schema->con schema Tvar prefix top-misc)]))

;; ----------------------------------------------------------------------

(define (function-takes-nargs? type nargs)
  (match type
    [('lambda dom rng)
      (recur loop ([dom dom][n nargs])
        (match dom
          [('nil) (zero? n)]
          [('cons para rest) (loop rest (sub1 n))]
          [_ #f]))]
    [_ #f]))
 
;; ----------------------------------------------------------------------

'(define (Tvar-in-type? ftype type)
  (let* ([o (old-Tvar-in-type? ftype type)]
         [Tvar2 (mk-Tvar 'in-type?)]
         [_ (tschema->con type Tvar 'in-type? '())]
         [n (Tvar-containment? ftype ftype2)])
    (unless (eq? o n)
      (mrspidey:warning
       (format "Tvar-in-type old ~s new ~s type ~s"     o n type))
      o)))

(define Tvar-in-type?
  (lambda (Tvar type forall-vars unbound)

    ;; (pretty-print `(Tvar-in-type? ,(Tvar-name Tvar) ,type ,forall-vars))
    ;; Checks if Tvar is contained in the type
    ;; Assumes type is already expanded
    
    (letrec
        ([visit-alloc (lambda () (make-vector num-AV #f))]
         [add-visit! 
          (lambda (v AV)
            (vector-set! v (AV-num AV) #t))]
         [visited?
          (lambda (v AV) 
            (vector-ref v (AV-num AV)))]
          [Tvar-in-type?
           (lambda (Tvar type env)
            ;; env maps type vars to AV-membership test function
            (andmap (lambda (AV) (AV-in-type? AV type env))
                    (get-Tvar-objs Tvar)))]
         [AV-in-type?
          (lambda (AV type env)
            ;; env maps type vars to AV-membership test function
            (match type

              ;; Recursive definitions
              [('rec bind t2)
               (let* 
                   ([a* (map car bind)]
                    [t* (map cadr bind)]
                    [envbox (box #f)]
                    [nuenv
                     (extend-env*
                      env a*
                      (map (lambda (t)
                             (let ([v (visit-alloc)])
                               (lambda (AV)
                                 (or (visited? v AV)
                                     (begin
                                       (add-visit! v AV)
                                       (AV-in-type? AV t (unbox envbox)))))))
                           t*))])
                 (set-box! envbox nuenv)
                 (AV-in-type? AV t2 nuenv))]

              [('case-> . types) (AV-in-type? AV (rac types) env)]

              [('union . (? list? union))
               (ormap (lambda (type) (AV-in-type? AV type env))
                      union)]
              [('intersect . (? list? intersect))
               (andmap (lambda (type) (AV-in-type? AV type env))
                       intersect)]
              ['_ #t]
              [(? symbol? a) 
               (let ([fn (or (lookup-or-#f env a)
                             (and (memq a forall-vars) (lambda (AV) #t))
                             (let ([type:a a])
                               (ormap
                                (match-lambda
                                 [(sym . Tvar)
                                  (if (eq? sym type:a)
                                      (lambda (AV)
                                        (let ([Tvar-t (mk-Tvar 'in-type?)])
                                          (new-AV! Tvar-t AV)
                                          (Tvar-containment? Tvar-t Tvar)))
                                      #f)])
                                global-tdef-env))
                             (unbound a))])
                 (fn AV))]
              [('lambda arg res)
                (and (eq? (AV-template AV) template-lam)
                  (Tvar-in-type?
                    (vector-ref (AV-fields+ AV) 0)
                    res
                    env))]
              [(? number? n)
               (and (eq? (template-type (AV-template AV)) 'num)
                    (number? (AV-misc AV))
                    (= (AV-misc AV) n))]
              [(? char? c)
               (and (eq? (template-type (AV-template AV)) 'char)
                    (char=? (AV-misc AV) c))]
              [((or 'quote '#%quote) (? symbol? s))
               (and (eq? (template-type (AV-template AV)) 'sym)
                    (eq? (AV-misc AV) s))]

              [((? type-constructor? C) . args)
                (match-let 
                  ([(and template ($ template type n+ n- ref assign))
                     (lookup-template C)])
                  (cond
                    [(or (eq? (AV-template AV) template)
                       (memq template 
                         (template-super-templates (AV-template AV)))) 
                      (recur loop ([args args][n 0])
                        (if (null? args)
                          #t
                          (and 
                            (or
                              (not (vector-ref ref n));; Can't reference
                              (Tvar-in-type? 
                                (vector-ref (AV-fields+ AV) (vector-ref ref n))
                                (match args
                                  [(('! a) . _) a]
                                  [(a . _) a])
                                env))
                            (loop (cdr args) (add1 n)))))]
                    [else
                      '(pretty-print-debug
                         `(fail ,(AV-template AV)
                            ,C
                            ,template
                            ,(template-super-templates (AV-template AV))
                            ,(memq template 
                               (template-super-templates (AV-template AV)))))
                      #f]))]
              [_ 
               ;; Didn't match any type
               (mrspidey:error (format "Invalid type syntax ~s" type))]))])
      (Tvar-in-type? Tvar type empty-env))))

; ======================================================================

'(define (FV-tschema tschema)
  ;; returns (FV-ref FV-bang)
  (let ([FV-ref '()]
        [FV-bang '()])
    (letrec 
        ([FV-type
          (lambda (type env sign)
            (match type
              [('rec bind t2)
               (let ([env (append (map car bind) env)])
                 (FV-type* (map cadr bind) env sign)
                 (FV-type t2 env sing))]
              [((or 'union 'intersect (? type-constructor?)) . (? list? args))
               (FV-type* args env sign)]
              [(or '_ 'empty (? number?) (? char?) 
                 ((or 'quote '#%quote) (? symbol?)))
               (void)]
              [(or (? symbol? a) ('! (? symbol? a)))
               (if sign
                   (set! FV-ref (set-add a FV-ref))
                   (set! FV-bang (set-add a FV-bang)))]
              [x
               ;; Didn't match any type
               (mrspidey:internal-error 'FV-type 
                                        "invalid type syntax ~s" x)]))]
         [FV-type*
          (lambda (types env sign)
            (for-each (lambda (t) (FV-type t env sign)) types))]
         [FV-schema
          (match-lambda
           [('forall vars type)
            (setdiff2 (FV-type type) vars)]
           [type (FV-type type vars #t)])]
         [FV-tschema
          (match-lambda
           [('case-> . schemas)
            (apply union (map FV-schema schemas))]
           [schema (FV-schema schema)])])
      (FV-tschema tschema)
      (list FV-ref FV-bang))))

; ======================================================================

(define (primitive->atprim name orig-type-schema . attrs)
  ;;(pretty-debug `(primitive->atprim ,name))
  (match-let*
    ( [type-schema (expand-input-type orig-type-schema)]
      [type (match type-schema
              [('forall _ type) type]
              [type type])])

    (make-atprim
      name 
      type-schema

      ;; Calculate filter-domains
      (match type
        [('lambda domain _)
          ;; Now walk domain and args
          (recur loop ([domain domain])
            (match domain
              [('cons domain d-rest)
                (cons
                  (let ([templates (type->templates domain)])
                    (if templates
                      (create-filter #t templates)
                      #f))
                  (loop d-rest))]
              [_ '()]))]
        [_ '()])

      ;; Calculate predicate-fn
      (foldl
        (lambda (attr predicate-fn)
          (let* 
            ([mk-predicate-fn
               (lambda (filter-then filter-else)
                 (lambda (b a Tvar sign)
                   '(printf "mk-predicate-fn ~s ~s ~s ~s ~s ~s~n"
                      filter-then filter-else b a Tvar sign)
                   (let ([filter (if sign filter-then filter-else)])
                     (if filter
                       (let ([nu-Tvar (mk-Tvar 'filter)])
                         (new-con! Tvar
                           (create-con-filter filter
                             nu-Tvar))
                         nu-Tvar)
                       Tvar))))]
              [get-templates
                (lambda (C*) 
                  (map
                    (lambda (C)
                      (or (lookup-template C)
                        (mrspidey:error 
                          (format "Unknown constructor ~s in primitive def" C))))
                    C*))]
              [nu-fn
                (match attr
                  [('predicate-fn exp) exp]
                  [('predicate . C*)
                    (let ([templates (get-templates C*)])
                      (mk-predicate-fn (create-filter #t templates)
                        (create-filter #f templates)))]
                  [('predicate* true-info false-info)
                    (apply 
                      mk-predicate-fn
                      (map
                        (match-lambda
                          ['_ #f]
                          [(bool . C*)
                            (let ([templates (get-templates C*)])
                              (create-filter bool templates))]
                          [_ (error 'predicate* "Bad syntax")])
                        (list true-info false-info)))]
                  [_ #f])])
            (if nu-fn
              ;; compose
              (lambda (b a Tvar sign)
                (nu-fn b a
                  (predicate-fn b a Tvar sign)
                  sign))
              predicate-fn)))
        (lambda (b a Tvar bool) Tvar)                 
        attrs)
     
      attrs
      orig-type-schema)))

; ======================================================================


