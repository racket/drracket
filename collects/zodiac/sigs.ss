; $Id: sigs.ss,v 1.72 2000/01/10 22:51:13 clements Exp $

(begin-elaboration-time (require-library "macro.ss"))
(begin-elaboration-time (require-library "prettys.ss"))
(begin-elaboration-time (require-library "files.ss"))
(begin-elaboration-time (require-library "refer.ss"))
(require-library "refer.ss")

(require-library "zsigs.ss" "zodiac")

(define-signature zodiac:misc^
  (pretty-print debug-level symbol-append flush-printf print-and-return 
   attributes-resetters))

(define-signature zodiac:correlate^
  (make-correlator add-to-correlator find-in-correlator))

(define-signature zodiac:sexp^
  (structurize-syntax sexp->raw sanitized-sexp->raw
    syntax-null? syntax-car syntax-cdr syntax-map
    set-macro-origin
    new-mark mark-expression add/remove-mark expose-list))

(define-signature zodiac:pattern^
  (make-match&env match-against penv-merge pexpand extend-penv
    match-and-rewrite))

(define-signature zodiac:interface^
  (static-error internal-error))

(define-signature zodiac:expander^
  (expand expand-program expand-expr
    m3-elaboration-evaluator
    m3-macro-body-evaluator
    add-system-macro-form add-user-macro-form
    add-micro-form add-macro-form
    add-list-micro add-ilist-micro add-lit-micro add-sym-micro
    get-list-micro get-ilist-micro get-lit-micro get-sym-micro
    make-attributes get-attribute put-attribute
    extend-env copy-env retract-env print-env make-empty-environment
    resolve resolve-in-env
    macro-resolution? micro-resolution?
    (struct top-level-resolution ())
    introduce-identifier introduce-fresh-identifier introduce-bound-id
    create-vocabulary append-vocabulary
    add-on-demand-form find-on-demand-form 
    set-subexpr-vocab!
    (struct vocabulary-record
      (name this rest symbol-error literal-error list-error ilist-error))))

(define-signature zodiac:scheme-core^
  (name-eq? marks-equal?
    parsed->raw extend-parsed->raw
    lexically-resolved? in-lexically-extended-env
    add-primitivized-micro-form add-primitivized-macro-form
    generate-name
    elaboration-evaluator user-macro-body-evaluator
    scheme-expand scheme-expand-program
    common-vocabulary
    beginner-vocabulary
    intermediate-vocabulary
    advanced-vocabulary
    full-vocabulary
    scheme-vocabulary
    reset-previous-attribute
    set-top-level-status get-top-level-status at-top-level?
    set-internal-define-status get-internal-define-status at-internal-define?
    as-nested
    process-top-level-resolution ensure-not-macro/micro
    check-for-signature-name
    (struct parsed (back))
    (struct varref (var))
    (struct top-level-varref ())          create-top-level-varref
    (struct top-level-varref/bind (slot)) create-top-level-varref/bind
    (struct top-level-varref/bind/unit (unit?)) create-top-level-varref/bind/unit
    (struct bound-varref (binding))   create-bound-varref
    (struct lexical-varref ())        create-lexical-varref
    (struct lambda-varref ())         create-lambda-varref
    (struct app (fun args))           create-app
    (struct binding (var orig-name))  create-binding+marks
    (struct lexical-binding ())       create-lexical-binding+marks
    (struct lambda-binding ())        create-lambda-binding+marks
    (struct form ())
    valid-syntactic-id? valid-syntactic-id/s?
    distinct-valid-syntactic-id/s?
    valid-id? valid-id/s?
    distinct-valid-id/s?
    optarglist-pattern
    (struct optarglist-entry (var+marks))
    (struct initialized-optarglist-entry (expr))
    (struct optarglist (vars))
    (struct sym-optarglist ())
    (struct list-optarglist ())
    (struct ilist-optarglist ())
    nonempty-arglist-decls-vocab lambda-nonempty-arglist-decls-vocab
    proper-arglist-decls-vocab lambda-proper-arglist-decls-vocab
    full-arglist-decls-vocab lambda-full-arglist-decls-vocab
    optarglist-decls-vocab
    make-optargument-list
    paroptarglist-pattern
    (struct paroptarglist-entry (var+marks))
    (struct initialized-paroptarglist-entry (expr))
    (struct paroptarglist (vars))
    (struct sym-paroptarglist ())
    (struct list-paroptarglist ())
    (struct ilist-paroptarglist ())
    paroptarglist-decls-vocab
    make-paroptargument-list
    arglist-pattern
    (struct arglist (vars))
    (struct sym-arglist ())
    (struct list-arglist ())
    (struct ilist-arglist ())
    make-argument-list))

(define-signature zodiac:scheme-main^
  (create-const
    (struct struct-form (type super fields))        create-struct-form
    (struct if-form (test then else))               create-if-form
    (struct quote-form (expr))                      create-quote-form
    (struct begin-form (bodies))                    create-begin-form
    (struct begin0-form (bodies))                   create-begin0-form
    (struct let-values-form (vars vals body))       create-let-values-form
    (struct letrec-values-form (vars vals body))    create-letrec-values-form
    (struct define-values-form (vars val))          create-define-values-form
    (struct set!-form (var val))                    create-set!-form
    (struct case-lambda-form (args bodies))         create-case-lambda-form
    (struct with-continuation-mark-form (key val body)) create-with-continuation-mark-form
    generate-struct-names
    expands<%>))

(define-signature zodiac:scheme-objects^
  (create-class*/names-form
    create-interface-form
    (struct supervar-varref ())  create-supervar-varref
    (struct superinit-varref ()) create-superinit-varref
    (struct public-varref ())    create-public-varref
    (struct override-varref ())  create-override-varref
    (struct private-varref ())   create-private-varref
    (struct inherit-varref ())   create-inherit-varref
    (struct rename-varref ())    create-rename-varref
    (struct supervar-binding ())  create-supervar-binding+marks
    (struct superinit-binding ()) create-superinit-binding+marks
    (struct public-binding ())    create-public-binding+marks
    (struct override-binding ())  create-override-binding+marks
    (struct private-binding ())   create-private-binding+marks
    (struct inherit-binding ())   create-inherit-binding+marks
    (struct rename-binding ())    create-rename-binding+marks
    (struct class*/names-form
      (this super-init super-expr interfaces init-vars inst-clauses))
    (struct interface-form (super-exprs variables))
    (struct public-clause (exports internals exprs))
    (struct override-clause (exports internals exprs))
    (struct private-clause (internals exprs))
    (struct inherit-clause (internals imports))
    (struct rename-clause (internals imports))
    (struct sequence-clause (exprs))))

(define-signature zodiac:scheme-units^
  (create-unit-form
    create-compound-unit-form
    create-invoke-unit-form
    (struct unit-form (imports exports clauses))
    (struct compound-unit-form (imports links exports))
    (struct invoke-unit-form (unit variables))
    unit-clauses-vocab-delta update-unresolved-attribute
    inside-unit? check-export
    process-unit-top-level-resolution
    ))

(define-signature zodiac:scheme-objects+units^
  ())

(define-signature zodiac:scheme-mrspidey^
  (mrspidey-vocabulary
    (struct poly-form (exp))
    (struct :-form (exp type))
    (struct type:-form (type attrs))
    (struct st:control-form (para val))
    (struct reference-unit-form (file kind signed?))
    (struct define-type-form (sym type))
    (struct define-constructor-form (sym modes))
    create-poly-form
    create-:-form
    create-type:-form
    create-st:control-form
    create-reference-unit-form
    create-define-type-form
    create-define-constructor-form))

(define-signature zodiac:back-protocol^
  (make-empty-back-box register-client))

(define-signature zodiac:system^
  ((open zodiac:structures^)
    (open zodiac:scanner-parameters^)
    (open zodiac:reader-structs^)
    (open zodiac:reader-code^)
    (open zodiac:sexp^)
    (open zodiac:pattern^)
    (open zodiac:correlate^)
    (open zodiac:back-protocol^)
    (open zodiac:expander^)
    (open zodiac:scheme-core^)
    (open zodiac:scheme-main^)
    (open zodiac:scheme-objects^)
    (open zodiac:scheme-units^)
    (open zodiac:scheme-objects+units^)
    (open zodiac:scheme-mrspidey^)))
