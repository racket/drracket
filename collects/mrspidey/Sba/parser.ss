;; parser.ss
;;
;; Takes a zodiac:sexp, and an env: sym -> zodiac:bound,
;; and produces a zodiac:ast and a list of unbound zodiac:bounds
;; Only handles R4RS primitive syntax
;; ======================================================================

(define keywords
  '(begin cond delay if set! set!-values quote begin lambda case-lambda 
     poly
     let-values letrec*-values
     unit compound-unit invoke-unit
     : type:-quote
     cache-exp-quote cache-inv-quote
     struct typed-structure const-typed-structure
     ))

(define define-keywords
  '(define-values
     define-type 
     define-constructor 
     ))

;; ----------------------------------------------------------------------

(define (top-level-parse-defs defs)
  (set! unbound-env '())
  (let-values ([(defs nuenv) (parse-defs defs empty-env)])
    (values defs (map cdr unbound-env))))

(define (top-level-parse-exp exp)
  (set! unbound-env '())
  (let ([ast (parse exp empty-env)])
    (values ast (map cdr unbound-env))))

(define unbound-env '())

;; ----------------------------------------------------------------------

(define syntax-error
  (case-lambda
   [(exp) (syntax-error exp "Bad syntax")]
   [(exp s)
    (mrspidey:error
     (format "File ~s, Line ~s: ~a ~s" 
             (file-name-from-path
              (zodiac:location-file (zodiac:zodiac-start exp)))
             (zodiac:location-line (zodiac:zodiac-start exp))
             s (zodiac:stripper exp)))]))

(define syntax-error-no-exp
  (case-lambda
   [(exp) (syntax-error-no-exp exp "Bad syntax")]
   [(exp s)
    (mrspidey:error
     (format "File ~s, Line ~s: ~as" 
             (file-name-from-path
              (zodiac:location-file (zodiac:zodiac-start exp)))
             (zodiac:location-line (zodiac:zodiac-start exp))
             s))]))

(define assert-syn
  (case-lambda
   [(exp x) (assert-syn exp x "Bad syntax")]
   [(exp x s) (unless x (syntax-error exp s))]))

(define parse-body
  (lambda (o s f body env)
    (assert (list? body))
    (recur loop ([rest body])
      (match rest
        [(exp) (parse exp env)]
        [(exp . rest)
         (zodiac:make-begin-form o s f (box 0)
                                 (parse exp env)
                                 (loop rest))]))))

(define do-bindings
  (lambda (env syms)
    (assert (list? syms))
    (let* ([bindings
            (map 
             (match-lambda
              [($ zodiac:symbol o s f sym)
               (let ([bound (zodiac:make-bound o s f (box 0) sym sym)])
                 (cons sym bound))]
              [exp (syntax-error exp "Bad binding syntax")])
             syms)]
           [syms (map car bindings)]
           [bounds (map cdr bindings)]
           [nuenv (extend-env* env syms bounds)])
      (values bounds nuenv))))

(define do-bindingss
  (lambda (env symss)
    (recur loop ([env env][symss symss][boundss '()])
      (if (null? symss)
          (values (reverse boundss) env)
          (let-values 
           ([(bounds env) (do-bindings env (car symss))])
           (loop env (cdr symss) (cons bounds boundss)))))))

(define parse
  ;; parses an expression
  (lambda (exp env)
    (pretty-debug-front `(parse ,(zodiac:stripper exp)))
    (letrec ([lookup-sym
              (lambda (sym s f env)
                (let ([bound
                        (or (lookup-or-#f env sym)
                          (lookup-or-#f unbound-env sym)
                          ;; Add to unbound-env
                          (let ([n (zodiac:my-create-bound sym s f)])
                            (set! unbound-env (cons (cons sym n) unbound-env))
                            n))])
                  ;;(set-bound-refs! bound (add1 (bound-refs bound)))
                  bound))]
             [parse-sym-env
              (lambda (env)
                (match-lambda
                 [($ zodiac:symbol o s f sym)
                  ;;(printf "ref sym ~s~n" sym)
                  (zodiac:make-lexical-varref o s f (box 0) 
                                              sym (lookup-sym sym s f env))]
                 [exp (syntax-error exp "Expected a variable:")]))]
             [parse-sym (parse-sym-env env)]
             [call-void
              (lambda (o s f)
                (zodiac:make-app 
                 o s f (box 0) 
                 (zodiac:make-lexical-varref o s f (box 0) 
                                             'void (lookup-sym 'void s f '()))
                 '()))]
             [parse-exps
              (lambda (exps env)
                (map (lambda (e) (parse e env)) exps))]
             [handle-args
              (lambda (args env)
                (match args
                  [($ zodiac:list _ _ _ args) (do-bindings env args)]
                  [($ zodiac:symbol)          
                   (let-values
                       ([(bounds nuenv) (do-bindings env (list args))])
                     (values (car bounds) nuenv))]
                  [($ zodiac:improper-list _ _ _ l)
                   (recur loop ([args '()][l l])
                     (match l
                       [(arg)
                        (let-values
                            ([(bounds nuenv) 
                              (do-bindings env (cons arg args))])
                          (values (append (reverse (cdr bounds)) (car bounds))
                                  nuenv))]
                       [(arg . rest) (loop (cons arg args) rest)]))]))])

      (pretty-debug-front `(parse ,(zodiac:stripper exp)))
      (match exp
        ;; Identifiers
        [($ zodiac:symbol o s f sym) (parse-sym exp)]

        ;; Scalars, vectors
        [(or ($ zodiac:scalar o s f) ($ zodiac:vector o s f))
         (zodiac:make-quote-form o s f (box 0) exp)]

        ;; Special forms
        [($ zodiac:list o s f 
            (and l (($ zodiac:symbol _ _ _ sym) . body)))
         (=> fail)
         (let ([sym (strip-hash-percent sym)])
           (cond
            [(memq sym keywords) 
             (match (cons sym body)
               [('cond)
                (call-void o s f)]
               [('delay exp)
                (zodiac:make-delay-form o s f (box 0) (parse exp env))]
               [('if test then else)
                (zodiac:make-if-form o s f (box 0) 
                                     (parse test env)
                                     (parse then env)
                                     (parse else env))]
               [('if test then)
                (zodiac:make-if-form o s f (box 0) 
                                     (parse test env)
                                     (parse then env)
                                     (call-void o s f))]
               [(or 
                  ('set! var exp)
                  ('set!-values ($ zodiac:list _ _ _ (var)) exp))

                (let ([var (parse-sym var)])
                  (when (zodiac:lexical-varref? var) 
                    (zodiac:set-bound-mutated!
                     (zodiac:lexical-varref-binding var)
                     #t))
                  (zodiac:make-set!-form o s f (box 0)
                                         var (parse exp env)))]
               [('quote sexp)
                (assert-syn exp (= (length body) 1))
                (zodiac:make-quote-form o s f (box 0) sexp)]
               [('begin . exps)
                (recur loop ([exps exps])
                  (match exps
                    [() (call-void o s f)]
                    [(exp) (parse exp env)]
                    [(exp . exps)
                     (zodiac:make-begin-form o s f (box 0)
                                             (parse exp env)
                                             (loop exps))]))]
               [('lambda args . bodies)
                (let-values
                 ([(bounds nuenv) (handle-args args env)])
                 (zodiac:make-lambda-form o s f (box 0) 
                                          bounds 
                                          (parse-body o s f bodies nuenv)))]
               [('case-lambda . cases)
                (zodiac:make-case-lambda-form
                 o s f (box 0)
                 (mapLR
                  (match-lambda
                   [($ zodiac:list _ _ _ (args . body))
                    (let-values
                     ([(bounds nuenv) (handle-args args env)])
                     (cons bounds (parse-body o s f body nuenv)))]
                   [_ (syntax-error-no-exp exp "Bad case lambda")])
                  cases))]

               [('let-values
                 ($ zodiac:list _ _ _
                    (($ zodiac:list _ _ _ 
                        (($ zodiac:list _ _ _ varss)
                         exps))
                     ...))
                 . bodies)
                (let-values
                 ([(boundss nuenv) (do-bindingss env varss)])
                 (zodiac:make-let-values-form 
                  o s f (box 0)
                  boundss
                  (parse-exps exps env)
                  (parse-body o s f  bodies nuenv)))]

               [('letrec*-values
                 ($ zodiac:list _ _ _
                    (($ zodiac:list _ _ _ 
                        (($ zodiac:list _ _ _ varss)
                         exps))
                     ...))
                 . bodies)
                (let-values
                 ([(boundss nuenv) (do-bindingss env varss)])
                  (for-each 
                    (lambda (name) (zodiac:set-bound-mutated! name #t))
                    (apply append boundss))
                 (zodiac:make-letrec-values-form 
                  o s f (box 0)
                  boundss
                  (parse-exps exps nuenv)
                  (parse-body o s f  bodies nuenv)))]

               ;; ---------------------------------------------------------
               ;; MzScheme specific code

               ;; --- units ---
               [('unit ($ zodiac:list _ _ _ 
                          (($ zodiac:symbol _ _ _ 'import) 
                           i-ids ...))
                       ($ zodiac:list _ _ _
                          (($ zodiac:symbol _ _ _ 'export) 
                           (or ($ zodiac:list _ _ _ 
                                  (i-eids
                                   ($ zodiac:symbol _ _ _ e-eids)))
                               (and i-eids
                                    ($ zodiac:symbol _ _ _ e-eids)))
                           ...))
                       . body)
                (let*-vals ([(i-names env1) (do-bindings env i-ids)]
                            [(defs env2) (parse-defs body env1)]
                            [e-vars 
                             (map (parse-sym-env env2) i-eids)]
                            [exports (map cons e-vars e-eids)])
                           (zodiac:make-unit-form o s f (box 0)
                                                  i-names exports defs))]

               ;; --- compound-units ---
               [('compound-unit
                 ($ zodiac:list _ _ _ 
                    (($ zodiac:symbol _ _ _ 'import) 
                     ($ zodiac:symbol _ _ _ imports) ...))
                 ($ zodiac:list _ _ _ 
                    (($ zodiac:symbol _ _ _ 'link) . links))
                 ($ zodiac:list _ _ _ 
                    (($ zodiac:symbol _ _ _ 'export) . exports))) 
                (let*-vals 
                 ([links
                   (map
                    (match-lambda
                     [($ zodiac:list _ _ _
                         (($ zodiac:symbol _ _ _ tag)
                          ($ zodiac:list _ _ _ (exp . imports))))
                      (list tag
                            (parse exp env)
                            (apply append
                                   (map
                                    (match-lambda
                                     [($ zodiac:symbol _ _ _ sym)
                                      (list (cons #f sym))]
                                     [($ zodiac:list _ _ _
                                         (($ zodiac:symbol _ _ _ tag)
                                          ($ zodiac:symbol _ _ _ syms)
                                          ...))
                                      (map (lambda (sym) (cons tag sym))
                                           syms)]
                                     [_ (syntax-error exp "Bad link imp")])
                                    imports)))]
                     [_ (syntax-error exp "Bad link")])
                    links)]
                  [exports
                   (apply 
                    append
                    (map
                     (match-lambda
                      [($ zodiac:list _ _ _
                          (($ zodiac:symbol _ _ _ tag) 
                           (or ($ zodiac:list _ _ _ 
                                  (($ zodiac:symbol _ _ _ i-eids)
                                   ($ zodiac:symbol _ _ _ e-eids)))
                               (and ($ zodiac:symbol _ _ _ i-eids)
                                    ($ zodiac:symbol _ _ _ e-eids))) ...))
                       (map 
                        (lambda (i-eid e-eid) (list tag i-eid e-eid))
                        i-eids e-eids)]
                      [_ (syntax-error exp "Bad link export")])
                     exports))])
                 (zodiac:make-compound-unit-form
                  o s f (box 0) imports links exports))]

               ;; --- invoke-unit ---
               [('invoke-unit exp . ((and vars ($ zodiac:symbol)) ...))
                (zodiac:make-invoke-unit-form
                 o s f (box 0)
                 (parse exp env) 
                 (map (lambda (var) (parse-sym var env)) vars))]

               ;; --- structures ---
               [((or 'struct 'typed-structure 'const-typed-structure) 
                 first
                 ($ zodiac:list _ _ _ fields))
                (let*-vals
                 ([(tag parent)
                   (match first
                     [($ zodiac:symbol _ _ _ tag) (values tag #f)]
                     [($ zodiac:list _ _ _
                         (($ zodiac:symbol _ _ _ tag) parent))
                      (values tag (parse parent env))])]
                  [fields (map zodiac:stripper fields)])
                 (zodiac:make-struct-form
                  o s f (box 0)
                  tag (eq? sym 'const-typed-structure) parent fields))]

               ;; -------------------------------------------------------
               ;; MrSpidey specific code

               [('poly exp)
                (zodiac:make-poly-form 
                 o s f (box 0) (parse exp env))]
               [(': exp type)
                (zodiac:make-:-form o s f (box 0) 
                                    (parse exp env)
                                    (zodiac:stripper type))]

               [('type:-quote
                  ($ zodiac:list _ _ _ 
                    (($ zodiac:symbol _ _ _ (or 'quote '#%quote))
                      ($ zodiac:list _ _ _ (type  . attrs)))))
                 (zodiac:make-type:-form o s f (box 0) 
                   (zodiac:stripper type)
                   (map zodiac:stripper attrs))]

               [('st:control para val)
                (zodiac:make-st:control-form o s f (box 0)
                                             (zodiac:stripper para)
                                             (zodiac:stripper val))]

               [((or 'cache-exp-quote 'cache-inv-quote)
                 ($ zodiac:list _ _ _ 
                    (($ zodiac:symbol _ _ _ (or 'quote '#%quote)) exp))
                 ($ zodiac:string _ _ _ za))
                (when (string=? za (regexp-replace ".za$" za ""))
                  (syntax-error exp "Cache filename must end in .za"))
                (zodiac:make-cache-form 
                 o s f (box 0) exp 
                 (normalize-path za)
                 (case sym
                   [cache-exp-quote 'exp]
                   [cache-inv-quote 'inv]
                   [else 
                    (mrspidey:internal-error 'parser "Bad sym ~s" sym)])
                 (current-directory))]
               
               ;; -------------------------------------------------------
               [else (syntax-error exp)])]
            [(memq sym define-keywords) 
             (syntax-error-no-exp exp "Defines not allowed")]
            [else (fail)]))]

        ;; Don't handle vectors or improper lists
        ;; Applications
              
        [($ zodiac:list o s f (and l (fn . exps)))
         (zodiac:make-app o s f (box 0) 
                          (parse fn env)
                          (parse-exps exps env))]
        [($ zodiac:list o s f ())
         (zodiac:make-quote-form o s f (box 0) '())]
        [_ (syntax-error exp)]))))




(define (parse-defs defs env)
  (let*-vals
    ([varss (mapLR 
              (match-lambda
                [($ zodiac:list _ _ _ 
                   (($ zodiac:symbol _ _ _ '#%define-values)
                     ($ zodiac:list _ _ _ vars)
                     exp))
                  vars]
                [_ '()])
              defs)]
      [(boundss nuenv) (do-bindingss env varss)]
      [_ (for-each 
           (lambda (name) (zodiac:set-bound-mutated! name #t))
           (apply append boundss))]
      [defs
        (map
          (lambda (def bounds)
            (pretty-debug-front `(parse-def ,(zodiac:stripper def)))
            (mrspidey:zprogress "Parsing" (zodiac:zodiac-start def))
            (match def
              ;; Special forms
              [($ zodiac:list o s f 
                 (and l (($ zodiac:symbol _ _ _ sym) . body)))
                (=> fail)
                (let ([sym (strip-hash-percent sym)])
                  (cond
                    [(not (memq sym define-keywords)) (fail)]
                    [else
                      (match (cons sym body)

                        [('define-values ($ zodiac:list _ _ _ vars) exp)
                          (zodiac:make-define-values-form 
                            o s f (box 0)
                            (map 
                              (match-lambda
                                [(and bound ($ zodiac:bound o s f _ sym))
                                  (zodiac:make-lexical-varref o s f (box 0) 
                                    sym bound)])
                              bounds)
                            (parse exp nuenv))]

                        ;; ----------------------------------------------------
                        ;; MrSpidey specific code

                        [('define-type ($ zodiac:symbol _ _ _ sym) type)
                          (zodiac:make-define-type-form 
                            o s f (box 0) 
                            sym (zodiac:stripper type))]

                        [('define-constructor ($ zodiac:symbol _ _ _ sym) 
                           . modes)
                          (let ([modes (map zodiac:stripper modes)])
                            (assert-syn def (andmap boolean? modes))
                            (zodiac:make-define-constructor-form o s f (box 0) 
                              sym modes))]

                        ;; ----------------------------------------------------
                        [else (fail)])]))]

              [exp (parse exp nuenv)]))
          defs boundss)])
    (unless (null? defs)
      (mrspidey:zprogress "Parsing" (zodiac:zodiac-finish (rac defs))))
    (values defs nuenv)))

;; ----------------------------------------------------------------------


