; $Id: scm-unit.ss,v 1.87 2000/01/10 22:51:12 clements Exp $

(unit/sig zodiac:scheme-units^
  (import zodiac:misc^ (z : zodiac:structures^)
    (z : zodiac:scanner-parameters^)
    (z : zodiac:reader-structs^)
    (z : zodiac:reader-code^)
    zodiac:sexp^ (pat : zodiac:pattern^) zodiac:scheme-core^
    zodiac:scheme-main^ zodiac:scheme-objects^ zodiac:back-protocol^
    zodiac:expander^ zodiac:interface^)

  (define-struct (unit-form struct:parsed)
    (imports exports clauses))

  (define-struct (compound-unit-form struct:parsed)
    (imports links exports))

  (define-struct (invoke-unit-form struct:parsed)
    (unit variables))

  (define create-unit-form
    (lambda (imports exports clauses source)
      (make-unit-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	imports exports clauses)))

  (define create-compound-unit-form
    (lambda (imports links exports source)
      (make-compound-unit-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	imports links exports)))

  (define create-invoke-unit-form
    (lambda (unit variables source)
      (make-invoke-unit-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	unit variables)))

  ; --------------------------------------------------------------------

  (define (make-put-get-remove attr)
    (define put
      (lambda (attributes v)
	(put-attribute 
	 attributes attr
	 (cons v
	       (get-attribute attributes attr
			      (lambda () null))))))
    (define get
      (lambda (attributes)
	(car (get-attribute attributes attr))))
    (define remove
      (lambda (attributes)
	(put-attribute 
	 attributes attr
	 (cdr (get-attribute attributes attr)))))
    (values put get remove))

  (define-values (put-c-unit-vocab-attribute
		  get-c-unit-vocab-attribute
		  remove-c-unit-vocab-attribute)
    (make-put-get-remove 'c-unit-link-import/body-vocab))


  (define-values (put-c-unit-current-link-tag-attribute
		  get-c-unit-current-link-tag-attribute
		  remove-c-unit-current-link-tag-attribute)
    (make-put-get-remove 'c-unit-current-link-tag-attribute))
		  
  (define-values (put-c-unit-expand-env
		  get-c-unit-expand-env
		  remove-c-unit-expand-env)
    (make-put-get-remove 'c-unit-expand-env))
		  
  (define-values (put-vars-attribute
		  get-vars-attribute
		  remove-vars-attribute)
    (make-put-get-remove 'unit-vars))
  (define (make-vars-attribute attributes)
    (put-vars-attribute attributes (make-hash-table)))

  (define-struct unresolved (id varref))

  (define make-unresolved-attribute
    (lambda (attributes)
      (put-attribute attributes 'unresolved-unit-vars
	(cons '()
	  (get-attribute attributes
	    'unresolved-unit-vars (lambda () '()))))))

  (define get-unresolved-attribute
    (lambda (attributes)
      (car (get-attribute attributes 'unresolved-unit-vars))))

  (define update-unresolved-attribute
    (lambda (attributes id varref)
      (let ((new-value (make-unresolved id varref))
	    (current (get-attribute attributes 'unresolved-unit-vars
		       (lambda () '())))) ; List of lists to accomodate
					; nested units
	(unless (null? current)
	  (put-attribute attributes 'unresolved-unit-vars
	    (cons
	      (cons new-value (car current))
	      (cdr current)))))))

  (define remove/update-unresolved-attribute
    (lambda (attributes unresolveds)
      (let ((left-unresolveds
	     (cdr (get-attribute attributes
				 'unresolved-unit-vars))))
	(if (null? left-unresolveds)
	    (begin
	      (put-attribute attributes 'unresolved-unit-vars null)
	      (unless (null? unresolveds)
		(let ([id (unresolved-id (car unresolveds))])
		  (check-for-signature-name id attributes)
		  (static-error
		    "unit" 'term:unit-unbound-id
		    (unresolved-id (car unresolveds))
		    "unbound identifier ~a"
		    (z:read-object id)))))
	    (put-attribute attributes 'unresolved-unit-vars
			   (cons (append unresolveds (car left-unresolveds))
				 (cdr left-unresolveds)))))))

  ; --------------------------------------------------------------------

  (define-struct unit-id (id))
  (define-struct (import-id struct:unit-id) ())
  (define-struct (export-id struct:unit-id) (defined?))
  (define-struct (internal-id struct:unit-id) ())
  (define-struct (link-id struct:unit-id) ())

  (define register-links
    (lambda (ids attributes)
      (map
	(lambda (id)
	  (let ((id-table (get-vars-attribute attributes))
		 (id-name (z:read-object id)))
	    (let ((entry (hash-table-get id-table id-name
			   (lambda () #f))))
	      (cond
		((not entry)
		  (hash-table-put! id-table id-name
		    (make-link-id id)))
		((link-id? entry)
		  (static-error
		    "unit linkage" 'term:unit-link-duplicate-tag
		    id "duplicate link tag name"))
		(else
		  (internal-error entry "Invalid in register-links"))))))
	ids)))

  (define check-link
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (link-id? entry)))))

  (define check-import
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (import-id? entry)))))

  (define inside-unit?
    (lambda (attributes)
      (not (null? (get-attribute attributes 'unit-vars
		    (lambda () null))))))

  (define check-export
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (export-id? entry)))))

  (define register-import
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (cond
	    ((not entry)
	      (hash-table-put! id-table id-name
		(make-import-id id)))
	    ((import-id? entry)
	      (static-error
		"unit" 'term:unit-duplicate-import
		id "duplicate import identifier ~a" id-name))
	    ((export-id? entry)
	      (static-error
		"unit" 'term:unit-import-exported
		id "exported identifier ~a being imported" id-name))
	    ((internal-id? entry)
	      (static-error
		"unit" 'term:unit-defined-imported
		id "defined identifier ~a being imported" id-name))
	    (else
	      (internal-error entry
		"Invalid in register-import/export")))))))
    
  (define register-definitions
    (lambda (ids attributes)
      (map
	(lambda (id)
	  (let ((id-table (get-vars-attribute attributes))
		 (id-name (z:read-object id)))
	    (let ((entry (hash-table-get id-table id-name
			   (lambda () #f))))
	      (cond
		((not entry)
		  (hash-table-put! id-table id-name
		    (make-internal-id id)))
		((import-id? entry)
		  (static-error
		    "unit" 'term:unit-redefined-import
		    id "redefined imported identifier ~a" id-name))
		((export-id? entry)
		  (if (export-id-defined? entry)
		    (static-error
		      "unit" 'term:unit-duplicate-definition
		      id "redefining exported identifier ~a" id-name)
		    (set-export-id-defined?! entry #t)))
		((internal-id? entry)
		  (static-error
		    "unit" 'term:unit-duplicate-definition
		    id "duplicate internal definition for ~a" id-name))
		(else
		  (internal-error entry
		    "Invalid entry in register-definitions"))))))
	ids)))

  (define register-export
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (cond
	    ((not entry)
	      (hash-table-put! id-table id-name
		(make-export-id id #f)))
	    ((import-id? entry)
	      (static-error
		"unit" 'term:unit-import-exported
		id "imported identifier ~a being exported" id-name))
	    ((export-id? entry)
	      (static-error
		"unit" 'term:unit-duplicate-export
		id "duplicate export identifier ~a" id-name))
	    ((internal-id? entry)
	      (internal-error entry
		"Should not have had an internal-id in register-export"))
	    (else
	      (internal-error entry
		"Invalid in register-import/export")))))))

  (define verify-export
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (cond
	    ((not entry)
	      (static-error
		"unit" 'term:unit-export-not-defined
		id "Exported identifier ~a not defined" id-name))
	    ((import-id? entry)
	      (static-error
		"unit" 'term:unit-import-exported
		id "imported identifier ~a being exported" id-name))
	    ((export-id? entry)
	      (unless (export-id-defined? entry)
		(static-error
		  "unit" 'term:unit-export-not-defined
		  id "exported identifier ~a not defined" id-name)))
	    ((internal-id? entry)
	      (internal-error entry
		"Should not have had an internal-id in verify-export"))
	    (else
	      (internal-error entry
		"Invalid in register-import/export")))))))

  (define get-unresolved-vars
    (lambda (attributes)
      (let ((id-table (get-vars-attribute attributes))
	    (top-level-space (get-attribute attributes 'top-levels))
	    (unresolveds (get-unresolved-attribute attributes)))
	(let loop ((remaining unresolveds)
		    (unr null))
	  (if (null? remaining) unr
	    (let* ((u (car remaining))
		   (uid (unresolved-id u)))
	      (let ((entry (hash-table-get id-table
			     (z:read-object uid) (lambda () #f))))
		(cond
		  ((or (internal-id? entry) (export-id? entry))
		    ; Need to set the box here
		    (when (top-level-varref/bind? (unresolved-varref u))
		      (let* ([id  (unit-id-id entry)]
			     [box (and top-level-space
				       (hash-table-get top-level-space
					 (z:read-object uid)
					 (lambda ()
					   (internal-error
					    entry
					    "Can't find box in get-unresolved-vars"))))])
			(set-top-level-varref/bind-slot!
			 (unresolved-varref u)
			 box)
			(set-top-level-varref/bind/unit-unit?!
			 (unresolved-varref u)
			 #t)))
		    (loop (cdr remaining) unr))
		  ((import-id? entry)
		    (loop (cdr remaining) unr))
		  ((not entry)
		    (loop (cdr remaining) (cons u unr)))
		  (else
		    (internal-error entry
		      "Invalid in get-unresolved-vars"))))))))))

  ; ----------------------------------------------------------------------

  (define c/imports-vocab
    (create-vocabulary 'c/imports-vocab #f
      "malformed import declaration"
      "malformed import declaration"
      "malformed import declaration"
      "malformed import declaration"))

  (add-sym-micro c/imports-vocab
    (lambda (expr env attributes vocab)
      (register-import expr attributes)
      (create-lexical-binding+marks expr)))

  ; ----------------------------------------------------------------------

  (define unit-register-exports-vocab
    (create-vocabulary 'unit-register-exports-vocab #f
      "malformed export declaration"
      "malformed export declaration"
      "malformed export declaration"
      "malformed export declaration"))

  (add-sym-micro unit-register-exports-vocab
    (lambda (expr env attributes vocab)
      (register-export expr attributes)))

  (add-list-micro unit-register-exports-vocab
    (let* ((kwd '())
	    (in-pattern '(internal-id external-id))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((internal (pat:pexpand 'internal-id p-env kwd))
		     (external (pat:pexpand 'external-id p-env kwd)))
		(valid-syntactic-id? internal)
		(valid-syntactic-id? external)
		(register-export internal attributes))))
	  (else
	    (static-error
	      "unit export" 'term:unit-export
	      expr "malformed declaration"))))))

  ;; ----------------------------------------------------------------------

  (define unit-generate-external-names-vocab
    (create-vocabulary 'unit-generate-external-names-vocab #f
      "malformed export declaration"
      "malformed export declaration"
      "malformed export declaration"
      "malformed export declaration"))

  (add-sym-micro unit-generate-external-names-vocab
    (lambda (expr env attributes vocab)
      expr))

  (add-list-micro unit-generate-external-names-vocab
    (let* ((kwd '())
	    (in-pattern '(internal-id external-id))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (pat:pexpand 'external-id p-env kwd)))
	  (else
	    (static-error
	      "unit export" 'term:unit-export
	      expr "malformed declaration"))))))

  ;; --------------------------------------------------------------------

  (define unit-verify-exports-vocab
    (create-vocabulary 'unit-verify-exports-vocab #f
      "malformed export declaration"
      "malformed export declaration"
      "malformed export declaration"
      "malformed export declaration"))

  (add-sym-micro unit-verify-exports-vocab
    (lambda (expr env attributes vocab)
      (verify-export expr attributes)
      (let ((expand-vocab (get-attribute attributes 'exports-expand-vocab)))
	(cons (process-unit-top-level-resolution expr attributes)
	      expr))))

  (add-list-micro unit-verify-exports-vocab
    (let* ((kwd '())
	    (in-pattern '(internal-id external-id))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((internal (pat:pexpand 'internal-id p-env kwd))
		     (external (pat:pexpand 'external-id p-env kwd)))
		(verify-export internal attributes)
		(let ((expand-vocab (get-attribute attributes
				      'exports-expand-vocab)))
		  (cons (process-unit-top-level-resolution internal attributes)
			external)))))
	  (else
	    (static-error
	      "unit export" 'term:unit-export
	      expr "malformed declaration"))))))

  ; ----------------------------------------------------------------------

  (define (fixup-shadowed-varrefs exprs exports env attributes vocab)
    (let ([shadowed (let loop ([exports exports])
		      (if (null? exports)
			  null
			  (let ([r (resolve (car exports) env vocab)]
				[rest (loop (cdr exports))])
			    (if (binding? r)
				(cons (cons
				       r 
				       (lambda ()
					 (process-unit-top-level-resolution 
					  (car exports)
					  attributes)))
				      rest)
				rest))))])
      (if (null? shadowed)
	  exprs
	  (begin
	    (map
	     (lambda (expr)
	       (fixup expr shadowed))
	     exprs)))))

  ;; Yuck - traverse and patch expressions to fix varrefs pointing to
  ;; lexical bindings that are shadowed by unit definitions.

  (define (fixup expr binding-map)
    (let fix ([expr expr])
      (if (bound-varref? expr)
	  (let ([fixed (assoc (bound-varref-binding expr) binding-map)])
	    (if fixed
		((cdr fixed))
		expr))
	  (begin
	    (cond
	     [(not expr) expr]
	     [(varref? expr) expr]
	     [(quote-form? expr) expr]
	     [(app? expr)
	      (set-app-fun! expr (fix (app-fun expr)))
	      (set-app-args! expr (map fix (app-args expr)))]
	     [(struct-form? expr)
	      (set-struct-form-super! expr (fix (struct-form-super expr)))]
	     [(if-form? expr)
	      (set-if-form-test! expr (fix (if-form-test expr)))
	      (set-if-form-then! expr (fix (if-form-then expr)))
	      (set-if-form-else! expr (fix (if-form-else expr)))]
	     [(begin-form? expr)
	      (set-begin-form-bodies! expr (map fix (begin-form-bodies expr)))]
	     [(begin0-form? expr)
	      (set-begin0-form-bodies! expr (map fix (begin0-form-bodies expr)))]
	     [(let-values-form? expr)
	      (set-let-values-form-vals! expr (map fix (let-values-form-vals expr)))
	      (set-let-values-form-body! expr (fix (let-values-form-body expr)))]
	     [(letrec-values-form? expr)
	      (set-letrec-values-form-vals! expr (map fix (letrec-values-form-vals expr)))
	      (set-letrec-values-form-body! expr (fix (letrec-values-form-body expr)))]
	     [(define-values-form? expr)
	      (set-define-values-form-val! expr (fix (define-values-form-val expr)))]
	     [(set!-form? expr)
	      (set-set!-form-var! expr (fix (set!-form-var expr)))
	      (set-set!-form-val! expr (fix (set!-form-val expr)))]
	     [(case-lambda-form? expr)
	      (set-case-lambda-form-bodies! expr (map fix (case-lambda-form-bodies expr)))]
	     [(with-continuation-mark-form? expr)
	      (set-with-continuation-mark-form-key! expr (fix (with-continuation-mark-form-key expr)))
	      (set-with-continuation-mark-form-val! expr (fix (with-continuation-mark-form-val expr)))
	      (set-with-continuation-mark-form-body! expr (fix (with-continuation-mark-form-body expr)))]
	     [(class*/names-form? expr)
	      (for-each
	       (lambda (clause)
		 (cond
		  [(public-clause? clause)
		   (set-public-clause-exprs! clause (map fix (public-clause-exprs clause)))]
		  [(private-clause? clause)
		   (set-private-clause-exprs! clause (map fix (private-clause-exprs clause)))]
		  [(sequence-clause? clause)
		   (set-sequence-clause-exprs! clause (map fix (sequence-clause-exprs clause)))]
		  [else (void)]))
	       (class*/names-form-inst-clauses expr))]
	     [(interface-form? expr)
	      (set-interface-form-super-exprs! expr (map fix (interface-form-super-exprs expr)))]
	     [(unit-form? expr)
	      (set-unit-form-clauses! expr (map fix (unit-form-clauses expr)))]
	     [(compound-unit-form? expr)
	      (for-each
	       (lambda (link)
		 (set-car! (cdr link) (fix (cadr link))))
	       (compound-unit-form-links expr))]
	     [(invoke-unit-form? expr)
	      (set-invoke-unit-form-unit! expr (fix (invoke-unit-form-unit expr)))
	      (set-invoke-unit-form-variables! expr (map fix (invoke-unit-form-variables expr)))]
	     [else
	      (internal-error expr "Cannot fix unknown form: ~s" expr)])
	    expr))))

  ; ----------------------------------------------------------------------

  (define unit-micro
      (let* ((kwd `(import export))
	      (in-pattern `(_
			     (import imports ...)
			     (export exports ...)
			     clauses ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ([top-level? (get-top-level-status attributes)]
		      [internal? (get-internal-define-status attributes)]
		      [old-top-level (get-attribute attributes 'top-levels)]
		      [old-delay (get-attribute attributes 'delay-sig-name-check?)]
		      [unit-clauses-vocab
		       (append-vocabulary unit-clauses-vocab-delta
					  vocab 'unit-clauses-vocab)])
		  (dynamic-wind
		   void
		   (lambda ()
		     (set-top-level-status attributes #t)
		     (set-internal-define-status attributes #f)
		     (put-attribute attributes 'top-levels (make-hash-table))
		     (put-attribute attributes 'delay-sig-name-check? #t)
		     (let ((in:imports (pat:pexpand '(imports ...) p-env kwd))
			   (in:exports (pat:pexpand '(exports ...) p-env kwd))
			   (in:clauses (pat:pexpand '(clauses ...) p-env kwd)))
		       (make-vars-attribute attributes)
		       (make-unresolved-attribute attributes)
		       (let* ((proc:imports (map (lambda (e)
						   (expand-expr e env
								attributes c/imports-vocab))
						 in:imports))
			      (_ (extend-env proc:imports env))
			      (_ (put-attribute attributes 'exports-expand-vocab
						unit-clauses-vocab))
			      (_ (for-each (lambda (e)
					     (expand-expr e env attributes
							  unit-register-exports-vocab))
					   in:exports))
			      (proc:clauses (map (lambda (e)
						   (expand-expr e env
								attributes
								unit-clauses-vocab))
						 in:clauses))
			      (_ (retract-env (map car proc:imports) env))
			      (proc:exports (map (lambda (e)
						   (expand-expr e env
								attributes
								unit-verify-exports-vocab))
						 in:exports))
			      (proc:exports-externals
			       (map (lambda (e)
				      (expand-expr e env attributes
						   unit-generate-external-names-vocab))
				    in:exports))
			      (unresolveds (get-unresolved-vars attributes))
			      (fixed-proc:clauses (fixup-shadowed-varrefs
						   proc:clauses
						   (hash-table-map
						    (get-vars-attribute attributes)
						    (lambda (key val) (unit-id-id val)))
						   env
						   attributes
						   vocab)))

			 (put-attribute attributes 'delay-sig-name-check? old-delay)

			 (distinct-valid-syntactic-id/s? proc:exports-externals)
			 (remove-vars-attribute attributes)
			 (remove/update-unresolved-attribute attributes
							     unresolveds)
			 (set-top-level-status attributes top-level?)
			 (set-internal-define-status attributes internal?)
			 (put-attribute attributes 'exports-expand-vocab #f)

			 (create-unit-form
			  (map car proc:imports)
			  proc:exports
			  fixed-proc:clauses
			  expr))))
		   (lambda () (put-attribute attributes 'top-levels old-top-level))))))
	    (else
	     (static-error
	       "unit" 'kwd:unit
	       expr "malformed expression"))))))

  (add-primitivized-micro-form 'unit full-vocabulary unit-micro)
  (add-primitivized-micro-form 'unit scheme-vocabulary unit-micro)

  ; ----------------------------------------------------------------------

  (define c-unit-link-import-vocab
    (create-vocabulary 'c-unit-link-import-vocab #f
      "malformed link import declaration"
      "malformed link import declaration"
      "malformed link import declaration"
      "malformed link import declaration"))

  (add-sym-micro c-unit-link-import-vocab
    (lambda (expr env attributes vocab)
      (if (check-import expr attributes)
	(list (expand-expr expr env attributes
		(get-c-unit-vocab-attribute attributes)))
	(static-error
	  "compound-unit linkage" 'term:c-unit-not-import
	  expr "~a: not an imported identifier" (z:read-object expr)))))

  (add-list-micro c-unit-link-import-vocab
    (let* ((kwd '())
	    (in-pattern '(tag id ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((tag (pat:pexpand 'tag p-env kwd))
		     (ids (pat:pexpand '(id ...) p-env kwd)))
		(when #f ; we allow self-import, now
		  (when (eq? (z:read-object tag)
			     (get-c-unit-current-link-tag-attribute
			      attributes))
		    (static-error
		      "compound-unit linkage" 'term:unit-link-self-import-tag
		      expr "self-import of tag ~a" (z:read-object tag))))
		(map (lambda (id) (cons tag id)) ids))))
	  (else
	    (static-error
	      "compound-unit linkage" 'term:c-unit-linkage
	      expr "invalid syntax"))))))

  (define c-unit-link-body-vocab
    (create-vocabulary 'c-unit-link-body-vocab #f
      "malformed link body declaration"
      "malformed link body declaration"
      "malformed link body declaration"
      "malformed link body declaration"))

  (add-list-micro c-unit-link-body-vocab
    (let* ((kwd '())
	    (in-pattern '(sub-unit-expr imported-var ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((sub-unit-expr (pat:pexpand 'sub-unit-expr p-env kwd))
		     (imported-vars
		       (pat:pexpand '(imported-var ...) p-env kwd)))
		(cons (expand-expr sub-unit-expr
				   (get-c-unit-expand-env attributes)
				   attributes
				   (get-c-unit-vocab-attribute attributes))
		  (map (lambda (imported-var)
			 (expand-expr imported-var env attributes
			   c-unit-link-import-vocab))
		    imported-vars)))))
	  (else
	    (static-error
	      "compound-unit linkage" 'term:c-unit-linkage
	      expr "malformed body"))))))

  (define c-unit-exports-vocab
    (create-vocabulary 'c-unit-exports-vocab #f
      "malformed unit export declaration"
      "malformed unit export declaration"
      "malformed unit export declaration"
      "malformed unit export declaration"))

  (add-sym-micro c-unit-exports-vocab
    (lambda (expr env attributes vocab)
      (cons expr expr)))

  (add-list-micro c-unit-exports-vocab
    (let* ((kwd '())
	    (in-pattern '(internal-id external-id))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((internal-id (pat:pexpand 'internal-id p-env kwd))
		     (external-id (pat:pexpand 'external-id p-env kwd)))
		(valid-syntactic-id? internal-id)
		(valid-syntactic-id? external-id)
		(cons internal-id external-id))))
	  (else
	    (static-error
	      "compound-unit" 'term:c-unit-export
	      expr "malformed export clause"))))))

  (define c-unit-export-clause-vocab
    (create-vocabulary 'c-unit-export-clause-vocab #f
      "malformed export clause declaration"
      "malformed export clause declaration"
      "malformed export clause declaration"
      "malformed export clause declaration"))

  (add-list-micro c-unit-export-clause-vocab
    (let* ((kwd '())
	    (in-pattern '(tag exports ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((tag (pat:pexpand 'tag p-env kwd))
		     (exports (pat:pexpand '(exports ...) p-env kwd)))
		(valid-syntactic-id? tag)
		(if (check-link tag attributes)
		  (map (lambda (e)
			 (cons tag
			   (expand-expr e env attributes
			     c-unit-exports-vocab)))
		    exports)
		  (static-error
		    "compound-unit" 'term:c-unit-invalid-tag
		    tag "not a valid tag")))))
	  (else
	    (static-error
	      "compound-unit" 'term:c-unit-export
	      expr "malformed export clause"))))))

  (define compound-unit-micro
      (let* ((kwd `(import link export))
	      (in-pattern `(_
			     (import imports ...)
			     (link
			       (link-tag link-body) ...)
			     (export export-clause ...)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((in:imports (pat:pexpand '(imports ...) p-env kwd))
		      (in:link-tags (pat:pexpand '(link-tag ...) p-env kwd))
		      (in:link-bodies
		       (pat:pexpand '(link-body ...) p-env kwd))
		      (in:export-clauses
		       (pat:pexpand '(export-clause ...) p-env kwd)))
		  (distinct-valid-syntactic-id/s? in:link-tags)
		  (make-vars-attribute attributes)
		  (put-c-unit-vocab-attribute attributes vocab)
		  (put-c-unit-expand-env attributes (copy-env env))
		  (let* ((proc:imports (map (lambda (e)
					      (expand-expr e env
							   attributes c/imports-vocab))
					    in:imports))
			 (_ (extend-env proc:imports env))
			 (_ (register-links in:link-tags attributes))
			 (raw-link-clauses (map z:read-object in:link-tags))
			 (proc:link-clauses
			  (map (lambda (link-tag link-body)
				 (let ((this-tag (z:read-object link-tag)))
				   (put-c-unit-current-link-tag-attribute
				    attributes this-tag)
				   (let ((expanded-body
					  (as-nested
					   attributes
					   (lambda ()
					     (expand-expr link-body env
							  attributes
							  c-unit-link-body-vocab)))))
				     (let ((unit-expr (car expanded-body))
					   (unit-args (apply append
							     (cdr expanded-body))))
				       (let loop ((args unit-args))
					 (if (null? args)
					     (begin
					       (remove-c-unit-current-link-tag-attribute
						attributes)
					       (cons link-tag
						     (cons unit-expr unit-args)))
					     (begin
					       (if (pair? (car args))
						   (let ((arg (caar args)))
						     (if (z:symbol? arg)
							 (when (not (memq (z:read-object arg)
									  raw-link-clauses))
							   (static-error
							     "compound-unit"
							     'term:c-unit-invalid-tag
							     arg
							     "not a valid tag"))
							 (static-error
							   "compound-unit"
							   'term:c-unit-invalid-tag
							   arg
							   "tag must be a symbol"))))
					       (loop (cdr args)))))))))
			       in:link-tags in:link-bodies))
			 (proc:export-clauses
			  (apply append
				 (map (lambda (e)
					(expand-expr e env
						     attributes c-unit-export-clause-vocab))
				      in:export-clauses)))
			 (_ (retract-env (map car proc:imports) env)))
		    (remove-c-unit-vocab-attribute attributes)
		    (remove-c-unit-expand-env attributes)
		    (remove-vars-attribute attributes)
		    (create-compound-unit-form
		      (map car proc:imports)
		      proc:link-clauses
		      proc:export-clauses
		      expr)))))
	    (else
	      (static-error
		"compound-unit" 'kwd:compound-unit
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'compound-unit full-vocabulary compound-unit-micro)
  (add-primitivized-micro-form 'compound-unit scheme-vocabulary compound-unit-micro)

  ; --------------------------------------------------------------------

  (define invoke-unit-micro
      (let* ((kwd '())
	      (in-pattern `(_ unit vars ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((unit (pat:pexpand 'unit p-env kwd))
		       (vars (pat:pexpand '(vars ...) p-env kwd)))
		  (valid-syntactic-id/s? vars)
		  (let* ((expr-expr
			  (as-nested
			   attributes
			   (lambda ()
			     (expand-expr unit env attributes vocab))))
			 (var-exprs
			  (map (lambda (e)
				 (expand-expr e env
					      attributes vocab))
			       vars)))
		    (create-invoke-unit-form
		      expr-expr
		      var-exprs
		      expr)))))
	    (else
	      (static-error
		"invoke-unit" 'kwd:invoke-unit
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'invoke-unit full-vocabulary invoke-unit-micro)
  (add-primitivized-micro-form 'invoke-unit scheme-vocabulary invoke-unit-micro)

  ; --------------------------------------------------------------------

  (extend-parsed->raw unit-form?
    (lambda (expr p->r)
      `(unit (import ,@(map p->r (unit-form-imports expr)))
	 (export ,@(map (lambda (e)
			  `(,(p->r (car e)) ,(sexp->raw (cdr e))))
		     (unit-form-exports expr)))
	 ,@(map p->r (unit-form-clauses expr)))))

  (extend-parsed->raw compound-unit-form?
    (lambda (expr p->r)
      `(compound-unit
	 (import ,@(map p->r (compound-unit-form-imports expr)))
	 (link
	   ,@(map (lambda (link-clause)
		    (let ((tag (car link-clause))
			   (sub-unit (cadr link-clause))
			   (imports (map (lambda (import)
					   (if (lexical-varref? import)
					     (p->r import)
					     `(,(sexp->raw (car import))
						,(sexp->raw (cdr import)))))
				      (cddr link-clause))))
		      `(,(sexp->raw tag)
			 (,(p->r sub-unit)
			   ,@imports))))
	       (compound-unit-form-links expr)))
	 (export
	   ,@(map (lambda (export-clause)
		    `(,(sexp->raw (car export-clause))
		       (,(sexp->raw (cadr export-clause))
			 ,(sexp->raw (cddr export-clause)))))
	       (compound-unit-form-exports expr))))))

  (extend-parsed->raw invoke-unit-form?
    (lambda (expr p->r)
      `(invoke-unit ,(p->r (invoke-unit-form-unit expr))
	 ,@(map p->r (invoke-unit-form-variables expr)))))

  ; ----------------------------------------------------------------------

  (define unit-clauses-vocab-delta
    (create-vocabulary 'unit-clauses-vocab-delta))

  (let* ((kwd '())
	  (in-pattern-1 `(_ (var ...) val))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd)))
    (let ((define-values-helper
	    (lambda (handler)
	      (lambda (expr env attributes vocab)
		(unless (at-top-level? attributes)
		  (static-error
		    "definition" 'term:def-not-at-top-level
		    expr
		    "must be at the top level"))
		(cond
		  ((pat:match-against m&e-1 expr env)
		    =>
		    (lambda (p-env)
		      (let* ((top-level? (get-top-level-status
					   attributes))
			      (_ (set-top-level-status
				   attributes))
			      (vars (pat:pexpand '(var ...)
				      p-env kwd))
			      (_ (map valid-syntactic-id? vars))
			      (_ (for-each
				   (lambda (var)
				     (let ((r (resolve var env vocab)))
				       (when (or (micro-resolution? r)
					       (macro-resolution? r))
					 (unless (check-export var attributes)
					   (static-error
					     "keyword"
					     'term:cannot-bind-kwd
					     var
					     "cannot bind keyword ~s"
					     (z:symbol-orig-name var))))))
				   vars))
			      (out (handler expr env attributes
				     vocab p-env vars)))
			(set-top-level-status attributes
			  top-level?)
			out)))
		  (else (static-error
			  "define-values" 'kwd:define-values
			  expr "malformed definition")))))))

      (add-primitivized-micro-form 'define-values unit-clauses-vocab-delta
	(define-values-helper
	  (lambda (expr env attributes vocab p-env vars)
	    (register-definitions vars attributes)
	    (let* ((id-exprs (map (lambda (v)
				    (let ((parsed
					   (expand-expr v env attributes
							define-values-id-parse-vocab)))
				      parsed))
				  vars))
		   (expr-expr (expand-expr
			       (pat:pexpand 'val p-env kwd)
			       env attributes vocab)))
	      (create-define-values-form id-exprs expr-expr expr)))))))

  (define define-values-id-parse-vocab
    (create-vocabulary 'define-values-id-parse-vocab #f
      "malformed in identifier position"
      "malformed in identifier position"
      "malformed in identifier position"
      "malformed in identifier position"))

  (add-sym-micro define-values-id-parse-vocab
    (let ((top-level-resolution (make-top-level-resolution 'dummy #f)))
      (lambda (expr env attributes vocab)
	(let ((id (z:read-object expr)))
	  (let ((top-level-space (get-attribute attributes 'top-levels)))
	    (if top-level-space
		(begin
		  (let ((ref
			 (create-top-level-varref/bind/unit
			  id
			  (hash-table-get top-level-space id
					  (lambda ()
					    (let ((b (box '())))
					      (hash-table-put! top-level-space id b)
					      b)))
			  expr)))
		    ;; Define a unit-bound variable => mark this and pre-existing as unit
		    (set-top-level-varref/bind/unit-unit?! ref #t)
		    (let ((b (top-level-varref/bind-slot ref)))
		      (map (lambda (r) (set-top-level-varref/bind/unit-unit?! r #t)) (unbox b))
		      (set-box! b (cons ref (unbox b))))
		    ref))
		(create-top-level-varref id expr)))))))

  (add-primitivized-micro-form 'set! unit-clauses-vocab-delta
    (let* ((kwd '())
	    (in-pattern `(_ var val))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(let ((p-env (pat:match-against m&e expr env)))
	  (if p-env
	    (let* ((top-level? (get-top-level-status attributes))
		    (_ (set-top-level-status attributes))
		    (var-p (pat:pexpand 'var p-env kwd))
		    (_ (valid-syntactic-id? var-p))
		    (id-expr (expand-expr var-p env attributes vocab))
		    (expr-expr (expand-expr
				 (pat:pexpand 'val p-env kwd)
				 env attributes vocab)))
	      (when (check-import var-p attributes)
		(static-error
		  "set!" 'term:no-set!-imported
		  var-p "cannot mutate imported identifier"))
	      (set-top-level-status attributes top-level?)
	      (create-set!-form id-expr expr-expr expr))
	    (static-error
	      "set!" 'kwd:set!
	      expr "malformed expression"))))))

  (define process-unit-top-level-resolution
    (lambda (expr attributes)
      (let ([varref
	     (process-top-level-resolution expr attributes)])
	(let ([id (z:read-object expr)])
	  (unless (built-in-name id)
	    (update-unresolved-attribute attributes expr varref)))
	varref)))
	  
  (add-sym-micro unit-clauses-vocab-delta
    (let ((top-level-resolution (make-top-level-resolution 'dummy #f)))
      (lambda (expr env attributes vocab)
	(let loop ((r (resolve expr env vocab)))
	  (cond
	    ((or (macro-resolution? r) (micro-resolution? r))
	      (if (check-export expr attributes)
		(loop top-level-resolution)
		(static-error
		  "keyword" 'term:keyword-out-of-context expr
		  "invalid use of keyword ~s" (z:symbol-orig-name expr))))
	    ((lambda-binding? r)
	      (create-lambda-varref r expr))
	    ((lexical-binding? r)
	      (create-lexical-varref r expr))
	    ((top-level-resolution? r)
	      (check-for-signature-name expr attributes)
	      (process-unit-top-level-resolution expr attributes))
	    (else
	      (internal-error expr "Invalid resolution in unit delta: ~s"
		r)))))))
  
  ; --------------------------------------------------------------------

  (include "scm-hanc.ss")

  ; --------------------------------------------------------------------

  (define reference-unit-maker
    (lambda (form-name form-name-str kwd:form-name sig?)
      (let ([micro
	     (let* ((kwd '())
		    (in-pattern `(_ filename))
		    (m&e (pat:make-match&env in-pattern kwd)))
	       (lambda (expr env attributes vocab)
		 (cond
		  ((pat:match-against m&e expr env)
		   =>
		   (lambda (p-env)
		     (let ((filename (pat:pexpand 'filename p-env kwd)))
		       (let ((f (expand-expr filename env attributes vocab)))
			 (if (and (quote-form? f)
				  (z:string? (quote-form-expr f)))
			     (expand-expr
			      (structurize-syntax
			       `(let ((result (#%load/use-compiled
					       ,(quote-form-expr f))))
				  (unless (,(if sig?
						'#%unit/sig?
						'#%unit?)
					   result)
				    (#%raise
				     (#%make-exn:unit
				      ,(format
					"~s: result from ~s is not ~aunit"
					form-name
					(sexp->raw (quote-form-expr f))
					(if sig? "signed " ""))
				      (#%current-continuation-marks))))
				  result)
			       expr '(-1)
			       #f
			       (z:make-origin 'micro expr))
			      env attributes vocab)
			     (static-error
			       form-name-str kwd:form-name
			       filename "does not yield a filename"))))))
		  (else
		   (static-error
		     form-name-str kwd:form-name
		     expr "malformed expression")))))])
	(add-primitivized-micro-form form-name full-vocabulary micro)
	(add-on-demand-form 'micro form-name common-vocabulary micro))))

  (reference-unit-maker 'require-unit "require-unit" 'kwd:require-unit #f)
  (reference-unit-maker 'require-unit/sig
    "require-unit/sig" 'kwd:require-unit/sig #t)

  (define reference-library-unit-maker
    (lambda (form-name form-name-str kwd:form-name sig? relative?)
      (let ([micro
	     (let* ((kwd '())
		    (in-pattern '(_ filename collections ...))
		    (m&e (pat:make-match&env in-pattern kwd)))
	       (lambda (expr env attributes vocab)
		 (cond
		  ((pat:match-against m&e expr env)
		   =>
		   (lambda (p-env)
		     (let ((filename (pat:pexpand 'filename p-env kwd))
			   (collections (pat:pexpand '(collections ...)
						     p-env kwd)))
		       (let ((f (expand-expr filename env attributes vocab))
			     (cs (map (lambda (c)
					(expand-expr c env attributes vocab))
				      collections)))
			 (unless (and (quote-form? f)
				      (z:string? (quote-form-expr f)))
			   (static-error
			     form-name-str kwd:form-name
			     filename "does not yield a filename"))
			 (for-each
			  (lambda (c collection)
			    (unless (and (quote-form? c)
					 (z:string? (quote-form-expr c)))
			      (static-error
				form-name-str kwd:form-name
				collection "does not yield a string")))
			  cs collections)
			 (let ((raw-f (z:read-object (quote-form-expr f)))
			       (raw-cs (map (lambda (c)
					      (z:read-object
					       (quote-form-expr c)))
					    cs)))
			   (unless (relative-path? raw-f)
			     (static-error
			       form-name-str kwd:form-name
			       f
			       "library path ~s must be a relative path"
			       raw-f))
			   (expand-expr
			    (structurize-syntax
			     `(let ((result (,(if relative?
						  '#%require-relative-library
						  '#%require-library)
					     ,(quote-form-expr f)
					     ,@(map quote-form-expr cs))))
				(unless (,(if sig? '#%unit/sig? '#%unit?)
					 result)
				  (#%raise
				   (#%make-exn:unit
				    ,(format
				      "~s: result from ~s in collection ~a not a ~aunit"
				      form-name
				      raw-f
				      (if (null? raw-cs)
					  '"mzlib"
					  raw-cs)
				      (if sig? "signed " ""))
				    (#%current-continuation-marks))))
				result)
			     expr '(-1)
			     #f
			     (z:make-origin 'micro expr))
			    env attributes vocab))))))
		  (else
		   (static-error
		     form-name-str kwd:form-name
		     expr "malformed expression")))))])
	(add-primitivized-micro-form form-name full-vocabulary micro)
	(add-on-demand-form 'micro form-name common-vocabulary micro))))

  (reference-library-unit-maker 'require-library-unit 
    "require-library-unit" 'kwd:require-library-unit #f #f)
  (reference-library-unit-maker 'require-library-unit/sig
    "require-library-unit/sig" 'kwd:require-library-unit/sig #t #f)
  (reference-library-unit-maker 'require-relative-library-unit
    "require-relative-library-unit" 'kwd:require-relative-library-unit #f #t)
  (reference-library-unit-maker 'require-relative-library-unit/sig
    "require-relative-library-unit/sig"
    'kwd:require-relative-library-unit/sig #t #t)

  (define (reset-unit-attributes attr)
    (put-attribute attr 'c-unit-link-import/body-vocab null)
    (put-attribute attr 'c-unit-current-link-tag-attribute null)
    (put-attribute attr 'c-unit-expand-env null)
    (put-attribute attr 'unit-vars null)
    (put-attribute attr 'unresolved-unit-vars null)
    (put-attribute attr 'exports-expand-vocab #f))

  (attributes-resetters (cons reset-unit-attributes (attributes-resetters)))
  
  )
