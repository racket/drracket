; $Id: scm-obj.ss,v 1.44 1999/05/21 12:53:29 mflatt Exp $

(unit/sig zodiac:scheme-objects^
  (import zodiac:misc^ (z : zodiac:structures^) (z : zodiac:reader-structs^)
    zodiac:sexp^ (pat : zodiac:pattern^) zodiac:scheme-core^
    zodiac:scheme-main^ zodiac:back-protocol^
    zodiac:expander^ zodiac:interface^)

  (define-struct (class*/names-form struct:parsed)
    (this super-init super-expr interfaces init-vars inst-clauses))

  (define-struct (interface-form struct:parsed)
    (super-exprs variables))

  (define create-class*/names-form
    (lambda (this super-init super-expr interfaces
	      init-vars inst-clauses source)
      (make-class*/names-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	this super-init super-expr interfaces init-vars inst-clauses)))

  (define create-interface-form
    (lambda (super-exprs variables source)
      (make-interface-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	super-exprs variables)))

  (define-struct (supervar-binding struct:binding) ())
  (define-struct (superinit-binding struct:binding) ())
  (define-struct (public-binding struct:binding) ())
  (define-struct (override-binding struct:binding) ())
  (define-struct (private-binding struct:binding) ())
  (define-struct (inherit-binding struct:binding) ())
  (define-struct (rename-binding struct:binding) ())

  (define create-supervar-binding+marks
    (create-binding+marks make-supervar-binding))
  (define create-superinit-binding+marks
    (create-binding+marks make-superinit-binding))
  (define create-public-binding+marks
    (create-binding+marks make-public-binding))
  (define create-override-binding+marks
    (create-binding+marks make-override-binding))
  (define create-private-binding+marks
    (create-binding+marks make-private-binding))
  (define create-inherit-binding+marks
    (create-binding+marks make-inherit-binding))
  (define create-rename-binding+marks
    (create-binding+marks make-rename-binding))

  (define-struct (supervar-varref struct:bound-varref) ())
  (define-struct (superinit-varref struct:bound-varref) ())
  (define-struct (public-varref struct:bound-varref) ())
  (define-struct (override-varref struct:bound-varref) ())
  (define-struct (private-varref struct:bound-varref) ())
  (define-struct (inherit-varref struct:bound-varref) ())
  (define-struct (rename-varref struct:bound-varref) ())

  (define create-supervar-varref
    (create-bound-varref make-supervar-varref))
  (define create-superinit-varref
    (create-bound-varref make-superinit-varref))
  (define create-public-varref
    (create-bound-varref make-public-varref))
  (define create-override-varref
    (create-bound-varref make-override-varref))
  (define create-private-varref
    (create-bound-varref make-private-varref))
  (define create-inherit-varref
    (create-bound-varref make-inherit-varref))
  (define create-rename-varref
    (create-bound-varref make-rename-varref))

  (define-struct public-clause (exports internals exprs))
  (define-struct override-clause (exports internals exprs))
  (define-struct private-clause (internals exprs))
  (define-struct inherit-clause (internals imports))
  (define-struct rename-clause (internals imports))
  (define-struct sequence-clause (exprs))

  ; --------------------------------------------------------------------

  (define interface-micro
      (let* ((kwd '())
	      (in-pattern `(_
			     (super-interfaces ...)
			     variables ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((super-interfaces
			(pat:pexpand '(super-interfaces ...) p-env kwd))
		       (variables
			 (pat:pexpand '(variables ...) p-env kwd)))
		  (distinct-valid-syntactic-id/s? variables)
		  (let* ((proc:super-interfaces
			  (as-nested
			   attributes
			   (lambda ()
			     (map (lambda (e)
				    (expand-expr e env
						 attributes vocab))
				  super-interfaces)))))
		    (create-interface-form
		      proc:super-interfaces
		      variables
		      expr)))))
	    (else
	      (static-error
		"interface" 'kwd:interface
		expr "malformed declaration"))))))

  (add-primitivized-micro-form 'interface full-vocabulary interface-micro)
  (add-primitivized-micro-form 'interface scheme-vocabulary interface-micro)

  ; ----------------------------------------------------------------------

  (define sym-micro
    (lambda (expr env attributes vocab)
      (let ((r (resolve expr env vocab)))
	(cond
	  ((lambda-binding? r)
	    (create-lambda-varref r expr))
	  ((lexical-binding? r)
	    (create-lexical-varref r expr))
	  ((top-level-resolution? r)
	   (check-for-signature-name expr attributes)
	   (process-top-level-resolution expr attributes))
	  ((public-binding? r)
	    (create-public-varref r expr))
	  ((override-binding? r)
	    (create-override-varref r expr))
	  ((private-binding? r)
	    (create-private-varref r expr))
	  ((inherit-binding? r)
	    (create-inherit-varref r expr))
	  ((rename-binding? r)
	    (create-rename-varref r expr))
	  ((supervar-binding? r)
	    (create-supervar-varref r expr))
	  ((superinit-binding? r)
	    (create-superinit-varref r expr))
	  ((or (macro-resolution? r) (micro-resolution? r))
	    (static-error
	      "keyword" 'term:keyword-out-of-context expr
	      "invalid use of keyword ~s" (z:symbol-orig-name expr)))
	  (else
	    (internal-error expr "Invalid resolution in obj: ~s" r))))))

  (add-sym-micro full-vocabulary sym-micro)
  (add-sym-micro scheme-vocabulary sym-micro)

					; ----------------------------------------------------------------------

  (define-struct ivar-entry (bindings))
  (define-struct (public-entry struct:ivar-entry) (exports exprs))
  (define-struct (override-entry struct:ivar-entry) (exports exprs))
  (define-struct (private-entry struct:ivar-entry) (exprs))
  (define-struct (inherit-entry struct:ivar-entry) (imports))
  (define-struct (rename-entry struct:ivar-entry) (imports))

  (define-struct sequence-entry (exprs))

					; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  (define make-void-init-expr
    (lambda (expr)
      (structurize-syntax '(#%void) expr '(-1))))

					; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  (define ivar-decls-vocab
    (create-vocabulary 'ivar-decls-vocab #f
      "malformed ivar declaration"
      "malformed ivar declaration"
      "malformed ivar declaration"
      "malformed ivar declaration"))

  (define public-ivar-decl-entry-parser-vocab
    (create-vocabulary 'public-ivar-decl-entry-parser-vocab #f
      "malformed public declaration"
      "malformed public declaration"
      "malformed public declaration"
      "malformed public declaration"))

  (define override-ivar-decl-entry-parser-vocab
    (create-vocabulary 'override-ivar-decl-entry-parser-vocab #f
      "malformed override declaration"
      "malformed override declaration"
      "malformed override declaration"
      "malformed override declaration"))

  (add-sym-micro public-ivar-decl-entry-parser-vocab
    (lambda (expr env attributes vocab)
      (list
	(create-public-binding+marks expr)
	expr
	(make-void-init-expr expr))))

  (define (mk-public/override-micro kind-sym kind-str 
				    ivar-decl-entry-parser-vocab
				    create-binding+marks
				    make-entry)
    (add-list-micro ivar-decl-entry-parser-vocab
      (let* ((kwd '())
	     (in-pattern-1 '((internal-var var) expr))
	     (in-pattern-2 '(var expr))
	     (in-pattern-3 '(var))
	     (m&e-1 (pat:make-match&env in-pattern-1 '()))
	     (m&e-2 (pat:make-match&env in-pattern-2 '()))
	     (m&e-3 (pat:make-match&env in-pattern-3 '())))
	(lambda (expr env attributes vocab)
	  (cond
	   ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (let ((internal-var (pat:pexpand 'internal-var p-env kwd))
		     (var (pat:pexpand 'var p-env kwd))
		     (expr (pat:pexpand 'expr p-env kwd)))
		(valid-syntactic-id? internal-var)
		(valid-syntactic-id? var)
		(list (create-binding+marks internal-var) var expr))))
	  ((pat:match-against m&e-2 expr env)
	    =>
	    (lambda (p-env)
	      (let ((var (pat:pexpand 'var p-env kwd))
		     (expr (pat:pexpand 'expr p-env kwd)))
		(valid-syntactic-id? var)
		(list (create-binding+marks var) var expr))))
	  ((pat:match-against m&e-3 expr env)
	    =>
	    (lambda (p-env)
	      (let ((var (pat:pexpand 'var p-env kwd)))
		(valid-syntactic-id? var)
		(list
		  (create-binding+marks var)
		  var
		  (make-void-init-expr expr)))))
	  (else
	    (static-error
	      "ivar" 'term:invalid-ivar-decl
	      expr (format "malformed ~a declaration" kind-str)))))))

    (let* ((kwd `(,kind-sym))
	   (in-pattern `(,kind-sym ivar-decl ...))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (add-micro-form kind-sym ivar-decls-vocab
	(lambda (expr env attributes vocab)
	  (cond
	   ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((decls
		      (map (lambda (decl)
			     (expand-expr decl env attributes
			       ivar-decl-entry-parser-vocab))
			(pat:pexpand '(ivar-decl ...) p-env kwd))))
		(make-entry
		  (map car decls)
		  (map cadr decls)
		  (map caddr decls)))))
	   (else
	    (static-error
	      "ivar" 'term:invalid-ivar-clause
	      expr (format "malformed ~a clause" kind-str))))))))

  (mk-public/override-micro 'public "public"
			    public-ivar-decl-entry-parser-vocab
			    create-public-binding+marks
			    make-public-entry)

  (mk-public/override-micro 'override "override"
			    override-ivar-decl-entry-parser-vocab
			    create-override-binding+marks
			    make-override-entry)

					; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  (define private-ivar-decl-entry-parser-vocab
    (create-vocabulary 'private-ivar-decl-entry-parser-vocab #f
      "malformed private declaration"
      "malformed private declaration"
      "malformed private declaration"
      "malformed private declaration"))

  (add-sym-micro private-ivar-decl-entry-parser-vocab
    (lambda (expr env attributes vocab)
      (cons (create-private-binding+marks expr)
	(make-void-init-expr expr))))

  (add-list-micro private-ivar-decl-entry-parser-vocab
    (let* ((kwd '())
	    (in-pattern-1 '(var expr))
	    (in-pattern-2 '(var))
	    (m&e-1 (pat:make-match&env in-pattern-1 '()))
	    (m&e-2 (pat:make-match&env in-pattern-2 '())))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (let ((var (pat:pexpand 'var p-env kwd))
		     (expr (pat:pexpand 'expr p-env kwd)))
		(valid-syntactic-id? var)
		(cons (create-private-binding+marks var) expr))))
	  ((pat:match-against m&e-2 expr env)
	    =>
	    (lambda (p-env)
	      (let ((var (pat:pexpand 'var p-env kwd)))
		(valid-syntactic-id? var)
		(cons (create-private-binding+marks var)
		  (make-void-init-expr expr)))))
	  (else
	    (static-error
	      "ivar" 'term:invalid-ivar-decl
	      expr "malformed declaration"))))))

  (let* ((kwd '(private))
	  (in-pattern '(private ivar-decl ...))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (add-micro-form 'private ivar-decls-vocab
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((decls
		      (map (lambda (decl)
			     (expand-expr decl env attributes
			       private-ivar-decl-entry-parser-vocab))
			(pat:pexpand '(ivar-decl ...) p-env kwd))))
		(make-private-entry
		  (map car decls)
		  (map cdr decls)))))
	  (else
	    (static-error
	      "private" 'kwd:class-private
	      expr "malformed declaration"))))))

					; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  (define inherit-ivar-decl-entry-parser-vocab
    (create-vocabulary 'inherit-ivar-decl-entry-parser-vocab #f
      "malformed inherit declaration"
      "malformed inherit declaration"
      "malformed inherit declaration"
      "malformed inherit declaration"))

  (add-sym-micro inherit-ivar-decl-entry-parser-vocab
    (lambda (expr env attributes vocab)
      (cons
	(create-inherit-binding+marks expr)
	expr)))

  (add-list-micro inherit-ivar-decl-entry-parser-vocab
    (let* ((kwd '())
	    (in-pattern '(internal-var var))
	    (m&e (pat:make-match&env in-pattern '())))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((internal-var (pat:pexpand 'internal-var p-env kwd))
		     (var (pat:pexpand 'var p-env kwd)))
		(valid-syntactic-id? internal-var)
		(valid-syntactic-id? var)
		(cons
		  (create-inherit-binding+marks internal-var)
		  var))))
	  (else
	    (static-error
	      "ivar" 'term:invalid-ivar-decl
	      expr "malformed declaration"))))))

  (let* ((kwd '(inherit))
	  (in-pattern '(inherit ivar-decl ...))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (add-micro-form 'inherit ivar-decls-vocab
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((decls
		      (map (lambda (decl)
			     (expand-expr decl env attributes
			       inherit-ivar-decl-entry-parser-vocab))
			(pat:pexpand '(ivar-decl ...) p-env kwd))))
		(make-inherit-entry
		  (map car decls)
		  (map cdr decls)))))
	  (else
	    (static-error
	      "inherit" 'kwd:class-inherit
	      expr "malformed declaration"))))))

					; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  (define rename-ivar-decl-entry-parser-vocab
    (create-vocabulary 'rename-ivar-decl-entry-parser-vocab #f
      "malformed rename declaration"
      "malformed rename declaration"
      "malformed rename declaration"
      "malformed rename declaration"))

  (add-list-micro rename-ivar-decl-entry-parser-vocab
    (let* ((kwd '())
	    (in-pattern-1 '(var inherited-var))
	    (m&e-1 (pat:make-match&env in-pattern-1 '())))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (let ((var (pat:pexpand 'var p-env kwd))
		     (inherited-var (pat:pexpand 'inherited-var p-env kwd)))
		(valid-syntactic-id? var)
		(valid-syntactic-id? inherited-var)
		(cons (create-rename-binding+marks var) inherited-var))))
	  (else
	    (static-error
	      "ivar" 'term:invalid-ivar-decl
	      expr "malformed declaration"))))))

  (let* ((kwd '(rename))
	  (in-pattern '(rename ivar-decl ...))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (add-micro-form 'rename ivar-decls-vocab
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((decls
		      (map (lambda (decl)
			     (expand-expr decl env attributes
			       rename-ivar-decl-entry-parser-vocab))
			(pat:pexpand '(ivar-decl ...) p-env kwd))))
		(make-rename-entry
		  (map car decls)
		  (map cdr decls)))))
	  (else
	    (static-error
	      "rename" 'kwd:class-rename
	      expr "malformed declaration"))))))

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  (let* ((kwd '(sequence))
	  (in-pattern '(sequence expr ...))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (add-micro-form 'sequence ivar-decls-vocab
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (make-sequence-entry
		(pat:pexpand '(expr ...) p-env kwd))))
	  (else
	    (static-error
	      "sequence" 'kwd:class-sequence
	      expr "malformed declaration"))))))

  ; ----------------------------------------------------------------------

  (define class-micro
    (let* ((kwd '())
	    (in-pattern `(kwd super args insts ...))
	    (out-pattern '(class*/names (this super-init)
			    super () args insts ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let* ((kwd-pos (pat:pexpand 'kwd  p-env kwd))
			(captured-this
			  (introduce-fresh-identifier 'this kwd-pos))
			(captured-super-init
			  (introduce-fresh-identifier 'super-init kwd-pos))
			(new-p-env (pat:extend-penv
				     'this captured-this
				     (pat:extend-penv
				       'super-init
				       captured-super-init
				       p-env))))
		  (expand-expr
		    (structurize-syntax
		      (pat:pexpand out-pattern new-p-env kwd)
		      expr '(-1)
		      #f
		      (z:make-origin 'micro expr))
		    env attributes vocab))))
	    (else
	      (static-error
		"class" 'kwd:class
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'class full-vocabulary class-micro)
  (add-primitivized-micro-form 'class scheme-vocabulary class-micro)

  (define class*-micro
    (let* ((kwd '())
	    (in-pattern `(kwd super interfaces args insts ...))
	    (out-pattern '(class*/names (this super-init)
			    super interfaces args insts ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let* ((kwd-pos (pat:pexpand 'kwd p-env kwd))
			(captured-this
			  (introduce-fresh-identifier 'this kwd-pos))
			(captured-super-init
			  (introduce-fresh-identifier 'super-init kwd-pos))
			(new-p-env (pat:extend-penv
				     'this captured-this
				     (pat:extend-penv
				       'super-init
				       captured-super-init
				       p-env))))
		  (expand-expr
		    (structurize-syntax
		      (pat:pexpand out-pattern new-p-env kwd)
		      expr '(-1)
		      #f
		      (z:make-origin 'micro expr))
		    env attributes vocab))))
	    (else
	      (static-error
		"class*" 'kwd:class*
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'class* full-vocabulary class*-micro)
  (add-primitivized-micro-form 'class* scheme-vocabulary class*-micro)

  (define class*/names-micro
    (let* ((kwd '())
	    (in-pattern `(kwd (this super-init)
			   super-expr
			   (interface ...)
			   ,paroptarglist-pattern
			   inst-vars ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((in:this (pat:pexpand 'this p-env kwd))
		       (in:superinit (pat:pexpand 'super-init
				       p-env kwd))
		       (in:super-expr (pat:pexpand 'super-expr
					p-env kwd))
		       (in:interfaces (pat:pexpand '(interface ...)
					p-env kwd))
		       (in:initvars (pat:pexpand `,paroptarglist-pattern
				      p-env kwd))
		       (in:ivars (pat:pexpand '(inst-vars ...)
				   p-env kwd)))
		  (valid-syntactic-id? in:this)
		  (valid-syntactic-id? in:superinit)
		  (as-nested
		   attributes
		   (lambda ()
		     (let* ((proc:superinit
			     (create-superinit-binding+marks
			      in:superinit))
			    (proc:super-expr
			     (expand-expr in:super-expr env
					  attributes vocab))
			    (proc:interfaces
			     (map (lambda (e)
				    (expand-expr e env
						 attributes vocab))
				  in:interfaces))
			    (proc:this (create-lexical-binding+marks
					in:this))
			    (proc:initvar-info
			     (expand-expr in:initvars env attributes
					  paroptarglist-decls-vocab))
			    (proc:ivar-info
			     (map (lambda (iv-decl)
				    (expand-expr iv-decl env attributes
						 ivar-decls-vocab))
				  in:ivars)))
		       (let ((proc:initvars
			      (map paroptarglist-entry-var+marks
				   (paroptarglist-vars
				    proc:initvar-info)))
			     (proc:ivars
			      (apply append
				     (map (lambda (i)
					    (if (ivar-entry? i)
						(ivar-entry-bindings i)
						'()))
					  proc:ivar-info))))
			 (let ((extensions
				(cons proc:this
				      (cons proc:superinit
					    proc:ivars))))
			   (let* ((new-names (map car extensions))
				  (parsed-initvars
				   (make-paroptargument-list
				    proc:initvar-info
				    env attributes vocab)))
			     (distinct-valid-id/s? (append new-names
							   (map car
								proc:initvars)))
			     (let ((external-ivars
				    (apply append
					   (map
					    (lambda (e)
					      (cond
					       ((public-entry? e)
						(public-entry-exports e))
					       ((override-entry? e)
						(override-entry-exports e))
					       (else null)))
					    proc:ivar-info))))
			       (distinct-valid-syntactic-id/s? external-ivars)
			       (void))
			     (extend-env extensions env)
			     (let
				 ((result
				   (create-class*/names-form
				    (car proc:this)
				    (car proc:superinit)
				    proc:super-expr
				    proc:interfaces
				    parsed-initvars
				    (let ((expand-exprs
					   (lambda (exprs)
					     (map (lambda (expr)
						    (expand-expr expr env
								 attributes vocab))
						  exprs))))
				      (map
				       (lambda (e)
					 (cond
					  ((public-entry? e)
					   (make-public-clause
					    (public-entry-exports e)
					    (map car (ivar-entry-bindings e))
					    (expand-exprs
					     (public-entry-exprs e))))
					  ((override-entry? e)
					   (make-override-clause
					    (override-entry-exports e)
					    (map car (ivar-entry-bindings e))
					    (expand-exprs
					     (override-entry-exprs e))))
					  ((private-entry? e)
					   (make-private-clause
					    (map car (ivar-entry-bindings e))
					    (expand-exprs
					     (private-entry-exprs e))))
					  ((inherit-entry? e)
					   (make-inherit-clause
					    (map car
						 (ivar-entry-bindings e))
					    (inherit-entry-imports e)))
					  ((rename-entry? e)
					   (make-rename-clause
					    (map car (ivar-entry-bindings e))
					    (rename-entry-imports e)))
					  ((sequence-entry? e)
					   (make-sequence-clause
					    (expand-exprs
					     (sequence-entry-exprs e))))
					  (else
					   (internal-error e
							   "Invalid entry in class*/names maker"))))
				       proc:ivar-info))
				    expr)))
			       (retract-env (append
					     (map car proc:initvars)
					     new-names)
					    env)
			       result))))))))))
	    (else
	      (static-error
		"class*/names" 'kwd:class*/names
		expr "malformed expression"))))))


  (add-primitivized-micro-form 'class*/names full-vocabulary class*/names-micro)
  (add-primitivized-micro-form 'class*/names scheme-vocabulary class*/names-micro)

  ; ----------------------------------------------------------------------

  (define ivar-micro
      (let* ((kwd '())
	      (in-pattern '(_ object name))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((object (pat:pexpand 'object p-env kwd))
		       (name (pat:pexpand 'name p-env kwd)))
		  (valid-syntactic-id? name)
		  (as-nested
		   attributes
		   (lambda ()
		     (expand-expr
		      (structurize-syntax
		       `(#%ivar/proc ,object (quote ,name))
		       expr '(-1)
		       #f
		       (z:make-origin 'micro expr))
		      env attributes vocab))))))
	    (else
	      (static-error
		"ivar" 'kwd:ivar
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'ivar full-vocabulary ivar-micro)
  (add-primitivized-micro-form 'ivar scheme-vocabulary ivar-micro)

  (define send-macro
      (let* ((kwd '())
	      (in-pattern '(_ object name arg ...))
	      (out-pattern '((ivar object name) arg ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error
	      "send" 'kwd:send
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'send full-vocabulary send-macro)
  (add-primitivized-macro-form 'send scheme-vocabulary send-macro)

  (define send*-macro
      (let* ((kwd '())
	      (in-pattern '(_ object (n0 a0 ...) ...))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern '(begin
			      (send object n0 a0 ...)
			      ...)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error
	      "send*" 'kwd:send*
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'send* full-vocabulary send*-macro)
  (add-on-demand-form 'macro 'send* common-vocabulary send*-macro)

  (define make-generic-micro
    (let* ((kwd '())
	    (in-pattern '(_ ci name))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((ci (pat:pexpand 'ci p-env kwd))
		     (name (pat:pexpand 'name p-env kwd)))
		(valid-syntactic-id? name)
		(as-nested
		  attributes
		  (lambda ()
		    (expand-expr
		      (structurize-syntax
			`(#%make-generic/proc ,ci (quote ,name))
			expr '(-1)
			#f
			(z:make-origin 'micro expr))
		      env attributes vocab))))))
	  (else
	    (static-error
	      "make-generic" 'kwd:make-generic
	      expr "malformed expression"))))))

  (add-primitivized-micro-form 'make-generic full-vocabulary make-generic-micro)
  (add-primitivized-micro-form 'make-generic scheme-vocabulary make-generic-micro)

  ; ----------------------------------------------------------------------

  (define set!-micro
      (let* ((kwd '())
	      (in-pattern `(_ var val))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (let ((p-env (pat:match-against m&e expr env)))
	    (if p-env
	      (let* ((var-p (pat:pexpand 'var p-env kwd))
		     (_ (valid-syntactic-id? var-p))
		     (id-expr (expand-expr var-p env attributes vocab))
		     (expr-expr (as-nested
				 attributes
				 (lambda ()
				   (expand-expr
				    (pat:pexpand 'val p-env kwd)
				    env attributes vocab)))))
		(when (or (inherit-varref? id-expr)
			(rename-varref? id-expr))
		  (static-error
		    "set!" 'term:no-set!-inherited/renamed
		    var-p
		    "cannot mutate inherited or renamed variables"))
		(create-set!-form id-expr expr-expr expr))
	      (static-error
		"set!" 'kwd:set!
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'set! full-vocabulary set!-micro)
  (add-primitivized-micro-form 'set! scheme-vocabulary set!-micro)

  ; --------------------------------------------------------------------

  (extend-parsed->raw class*/names-form?
    (lambda (expr p->r)
      `(class*/names
	 (,(p->r (class*/names-form-this expr))
	   ,(p->r (class*/names-form-super-init expr)))
	 ,(p->r (class*/names-form-super-expr expr))
	 ,(map p->r (class*/names-form-interfaces expr))
	 ,(p->r (class*/names-form-init-vars expr))
	 ,@(map (lambda (clause)
		  (cond
		    ((public-clause? clause)
		      `(public
			 ,@(map (lambda (internal export expr)
				  `((,(p->r internal) ,(sexp->raw export))
				     ,(p->r expr)))
			     (public-clause-internals clause)
			     (public-clause-exports clause)
			     (public-clause-exprs clause))))
		    ((override-clause? clause)
		      `(override
			 ,@(map (lambda (internal export expr)
				  `((,(p->r internal) ,(sexp->raw export))
				     ,(p->r expr)))
			     (override-clause-internals clause)
			     (override-clause-exports clause)
			     (override-clause-exprs clause))))
		    ((private-clause? clause)
		      `(private
			 ,@(map (lambda (internal expr)
				  `(,(p->r internal) ,(p->r expr)))
			     (private-clause-internals clause)
			     (private-clause-exprs clause))))
		    ((inherit-clause? clause)
		      `(inherit
			 ,@(map (lambda (internal inherited)
				  `(,(p->r internal) ,(sexp->raw inherited)))
			     (inherit-clause-internals clause)
			     (inherit-clause-imports clause))))
		    ((rename-clause? clause)
		      `(rename
			 ,@(map (lambda (internal inherited)
				  `(,(p->r internal) ,(sexp->raw inherited)))
			     (rename-clause-internals clause)
			     (rename-clause-imports clause))))
		    ((sequence-clause? clause)
		      `(sequence
			 ,@(map p->r (sequence-clause-exprs clause))))))
	     (class*/names-form-inst-clauses expr)))))

  (extend-parsed->raw interface-form?
    (lambda (expr p->r)
      `(interface ,(map p->r (interface-form-super-exprs expr))
	 ,@(map sexp->raw (interface-form-variables expr)))))

  )
