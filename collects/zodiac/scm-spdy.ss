; $Id: scm-spdy.ss,v 1.42 1999/01/16 15:47:07 mflatt Exp $

(unit/sig zodiac:scheme-mrspidey^
  (import zodiac:misc^ (z : zodiac:structures^)
    (z : zodiac:scanner-parameters^)
    (z : zodiac:reader-structs^)
    (z : zodiac:reader-code^)
    zodiac:sexp^ (pat : zodiac:pattern^) zodiac:scheme-core^
    zodiac:scheme-main^ zodiac:back-protocol^
    zodiac:expander^ zodiac:interface^
    (mzlib : mzlib:file^))

  (define-struct (poly-form struct:parsed) (exp))
  (define-struct (:-form struct:parsed) (exp type))
  (define-struct (type:-form struct:parsed) (type attrs))
  (define-struct (st:control-form struct:parsed) (para val))
  (define-struct (reference-unit-form struct:parsed)
    (file kind signed?))
  (define-struct (define-type-form struct:parsed) (sym type))
  (define-struct (define-constructor-form struct:parsed) (sym modes))

  (define create-poly-form
    (lambda (exp source)
      (make-poly-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	exp)))

  (define create-:-form
    (lambda (exp type source)
      (make-:-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	exp type)))

  (define create-type:-form
    (lambda (type attrs source)
      (make-type:-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	type attrs)))

  (define create-st:control-form
    (lambda (para val source)
      (make-st:control-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	para val)))

    (define create-reference-unit-form
    (lambda (file kind signed? source)
      (make-reference-unit-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	file kind signed?)))

  (define create-define-type-form
    (lambda (sym type source)
      (make-define-type-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	sym type)))

  (define create-define-constructor-form
    (lambda (sym modes source)
      (make-define-constructor-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	sym modes)))

  ; --------------------------------------------------------------------

  (define mrspidey-vocabulary
    (create-vocabulary 'mrspidey-vocabulary full-vocabulary))

  ; --------------------------------------------------------------------

  (add-primitivized-micro-form 'polymorphic mrspidey-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ p-expr))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((p-expr (pat:pexpand 'p-expr p-env kwd)))
		(create-poly-form
		  (expand-expr p-expr env attributes vocab)
		  expr))))
	  (else
	    (static-error expr "Malformed poly"))))))

  (add-primitivized-micro-form ': mrspidey-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ :-expr type))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((:-expr (pat:pexpand ':-expr p-env kwd))
		     (type (pat:pexpand 'type p-env kwd)))
		(create-:-form
		  (expand-expr :-expr env attributes vocab)
		  (sexp->raw type)
		  expr))))
	  (else
	    (static-error expr "Malformed :"))))))

  (add-primitivized-micro-form 'type: mrspidey-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ type attr ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((type (pat:pexpand 'type p-env kwd))
		     (attrs (pat:pexpand '(attr ...) p-env kwd)))
		(create-type:-form
		  (sexp->raw type)
		  (map sexp->raw attrs)
		  expr))))
	  (else
	    (static-error expr "Malformed type:"))))))

  (add-primitivized-micro-form 'mrspidey:control mrspidey-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ para val))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((para (pat:pexpand 'para p-env kwd))
		     (val (pat:pexpand 'val p-env kwd)))
		(create-st:control-form
		  (sexp->raw para)
		  (sexp->raw val)
		  expr))))
	  (else
	    (static-error expr "Malformed mrspidey:control"))))))

  (add-primitivized-micro-form 'define-type mrspidey-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ sym type))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((sym (pat:pexpand 'sym p-env kwd))
		     (type (pat:pexpand 'type p-env kwd)))
		(valid-syntactic-id? sym)
		(create-define-type-form
		  (z:read-object sym)
		  (sexp->raw type)
		  expr))))
	  (else
	    (static-error expr "Malformed define-type"))))))

  (add-primitivized-micro-form 'define-constructor mrspidey-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ sym modes ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((sym (pat:pexpand 'sym p-env kwd))
		     (modes (pat:pexpand '(modes ...) p-env kwd)))
		(valid-syntactic-id? sym)
		; Cormac has an (assert-syn def (andmap boolean? modes))
		; here.  I only do the andmap z:boolean? part since
		; I have no idea what (assert-syn def ...) does.
		(map (lambda (mode)
		       (unless (z:boolean? mode)
			 (static-error mode "Malformed mode")))
		  modes)
		(create-define-constructor-form
		  (z:read-object sym)
		  (map sexp->raw modes)
		  expr))))
	  (else
	    (static-error expr "Malformed define-constructor"))))))

  (add-primitivized-micro-form 'reference-file mrspidey-vocabulary
    (let* ((kwd '())
	    (in-pattern `(_ file))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((file (pat:pexpand 'file p-env kwd)))
		(let ((f (expand-expr file env attributes vocab)))
		  (if (and (quote-form? f)
			(z:string? (quote-form-expr f)))
		    (let* ((raw-filename (z:read-object (quote-form-expr f))))
		      (let-values (((base name dir?)
				     (split-path raw-filename)))
			(when dir?
			  (static-error file
			    "Cannot include a directory"))
			(let* ((original-directory
				 (current-load-relative-directory))
				(p (with-handlers
				     ((exn:i/o:filesystem?
					(lambda (exn)
					  (static-error file
					    "Unable to open file ~a"
					    raw-filename))))
				     (open-input-file
				       (if (complete-path? raw-filename)
					 raw-filename
					 (build-path
					   (or original-directory
					     (current-directory))
					   raw-filename))))))
			  (dynamic-wind
			    (lambda ()
			      (when (string? base)
				(current-load-relative-directory
				  (if (complete-path? base)
				    base
				    (build-path (or original-directory
						  (current-directory))
				      base)))))
			    (lambda ()
			      (let ((reader
				      (z:read p
					(z:make-location
					  (z:location-line
					    z:default-initial-location)
					  (z:location-column
					    z:default-initial-location)
					  (z:location-offset
					    z:default-initial-location)
					  (build-path
					    (current-load-relative-directory)
					    name)))))
				(let ((code
					(let loop ()
					  (let ((input (reader)))
					    (if (z:eof? input)
					      '()
					      (cons input
						(loop)))))))
				  (if (null? code)
				    (static-error expr "Empty file")
				    (expand-expr
				      (structurize-syntax
					`(begin ,@code)
					expr '(-1))
				      env attributes vocab)))))
			    (lambda ()
			      (current-load-relative-directory original-directory)
			      (close-input-port p))))))
		    (static-error file "Does not yield a filename"))))))
	  (else
	    (static-error expr "Malformed reference-file"))))))

  (define reference-library/relative-maker
    (lambda (form-name make-raw-filename)
      (let* ((kwd '())
	      (in-pattern '(_ filename collections ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((filename (pat:pexpand 'filename p-env kwd))
		       (collections (pat:pexpand '(collections ...) p-env kwd)))
		  (let ((f (expand-expr filename env attributes vocab))
			 (cs (map (lambda (c)
				    (expand-expr c env attributes vocab))
			       collections)))
		    (unless (and (quote-form? f)
			      (z:string? (quote-form-expr f)))
		      (static-error filename "Does not yield a filename"))
		    (for-each
		      (lambda (c collection)
			(unless (and (quote-form? c)
				  (z:string? (quote-form-expr c)))
			  (static-error collection "Does not yield a string")))
		      cs collections)
		    (let* ((raw-f (z:read-object (quote-form-expr f)))
			    (raw-cs (map (lambda (c)
					   (z:read-object (quote-form-expr c)))
				      cs))
			    (raw-filename
			      (if (relative-path? raw-f)
				(or (make-raw-filename raw-f raw-cs expr)
				  (static-error filename
				    "No such library file found"))
				(static-error f
				  "Library path ~s must be a relative path"
				  raw-f))))
			(let-values (((base name dir?)
				       (split-path raw-filename)))
			  (when dir?
			    (static-error filename
			      "Cannot include a directory"))
			  (let ((original-directory
				  (current-load-relative-directory))
				 (original-collections
				   (current-require-relative-collection))
				 (p (with-handlers
				      ((exn:i/o:filesystem?
					 (lambda (exn)
					   (static-error filename
					     "Unable to open file ~a"
					     raw-filename))))
				      (open-input-file raw-filename))))
			    (dynamic-wind
			      (lambda ()
				(current-require-relative-collection
				  (if (null? raw-cs) '("mzlib") raw-cs))
				(when (string? base)
				  (current-load-relative-directory base)))
			      (lambda ()
				(let ((reader
					(z:read p
					  (z:make-location
					    (z:location-line
					      z:default-initial-location)
					    (z:location-column
					      z:default-initial-location)
					    (z:location-offset
					      z:default-initial-location)
					    (build-path
					      (current-load-relative-directory)
					      name)))))
				  (let ((code
					  (let loop ()
					    (let ((input (reader)))
					      (if (z:eof? input)
						'()
						(cons input
						  (loop)))))))
				    (if (null? code)
				      (static-error expr "Empty file")
				      (expand-expr
					(structurize-syntax
					  `(begin ,@code)
					  expr '(-1))
					env attributes vocab)))))
			      (lambda ()
				(current-load-relative-directory
				  original-directory)
				(current-require-relative-collection
				  original-collections)
				(close-input-port p))))))))))
	    (else
	      (static-error expr (string-append "Malformed "
				   (symbol->string form-name)))))))))

  (add-primitivized-micro-form 'require-library mrspidey-vocabulary
    (reference-library/relative-maker 'require-library
      (lambda (raw-f raw-cs expr)
	(apply mzlib:find-library raw-f raw-cs))))

  (add-primitivized-micro-form 'require-relative-library mrspidey-vocabulary
    (reference-library/relative-maker 'require-relative-library
      (lambda (raw-f raw-cs expr)
	(apply mzlib:find-library raw-f
	  (append (or (current-require-relative-collection)
		    (static-error expr
		      "No current collection for library \"~a\"" raw-f))
	    raw-cs)))))

  (define reference-unit-maker
    (lambda (form-name signed?)
      (add-primitivized-micro-form form-name mrspidey-vocabulary
	(let* ((kwd '())
		(in-pattern `(_ file))
		(m&e (pat:make-match&env in-pattern kwd)))
	  (lambda (expr env attributes vocab)
	    (cond
	      ((pat:match-against m&e expr env)
		=>
		(lambda (p-env)
		  (let ((file (pat:pexpand 'file p-env kwd)))
		    (let ((f (expand-expr file env attributes vocab)))
		      (if (and (quote-form? f)
			    (z:string? (quote-form-expr f)))
			(create-reference-unit-form
			  (structurize-syntax
			    (path->complete-path (z:read-object
						   (quote-form-expr f))
			      (or (current-load-relative-directory)
				(current-directory)))
			    expr)
			  'exp
			  signed?
			  expr)
			(static-error file "Does not yield a filename"))))))
	      (else
		(static-error expr "Malformed ~a" form-name))))))))

  (reference-unit-maker 'require-unit #f)
  (reference-unit-maker 'require-unit/sig #t)

  (define reference-library-unit-maker
    (lambda (form-name sig? relative?)
	(add-primitivized-micro-form form-name mrspidey-vocabulary
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
			  (static-error filename "Does not yield a filename"))
			(for-each
			  (lambda (c collection)
			    (unless (and (quote-form? c)
				      (z:string? (quote-form-expr c)))
			      (static-error collection
				"Does not yield a string")))
			  cs collections)
			(let ((raw-f (z:read-object (quote-form-expr f)))
			       (raw-cs (map (lambda (c)
					      (z:read-object
						(quote-form-expr c)))
					 cs)))
			  (unless (relative-path? raw-f)
			    (static-error f
			      "Library path ~s must be a relative path"
			      raw-f))
			  (create-reference-unit-form
			    (structurize-syntax
			      (path->complete-path
				(or (apply mzlib:find-library raw-f
				      (if relative?
					(append (or (current-require-relative-collection)
						  null)
					  raw-cs)
					raw-cs))
				  (static-error expr
				    "Unable to locate library ~a in collection path ~a"
				    raw-f
				    (if (null? raw-cs) "mzlib" raw-cs)))
				(or (current-load-relative-directory)
				  (current-directory)))
			      expr)
			    'exp
			    sig?
			    expr))))))
		(else
		  (static-error expr
		    (string-append "Malformed ~a" form-name)))))))))

  (reference-library-unit-maker 'require-library-unit #f #f)
  (reference-library-unit-maker 'require-library-unit/sig #t #f)
  (reference-library-unit-maker 'require-relative-library-unit #f #t)
  (reference-library-unit-maker 'require-relative-library-unit/sig #t #t)

'  (add-primitivized-micro-form 'references-unit-imports mrspidey-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ file))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((file (pat:pexpand 'file p-env kwd)))
		(create-reference-unit-form
		  file
		  (current-directory)
		  'imp
		  expr))))
	  (else
	    (static-error expr "Malformed require-unit-imports"))))))

  ; --------------------------------------------------------------------

  (extend-parsed->raw poly-form?
    (lambda (expr p->r)
      `(polymorphic ,(p->r (poly-form-exp expr)))))

  (extend-parsed->raw :-form?
    (lambda (expr p->r)
      `(: ,(p->r (:-form-exp expr)) ,(:-form-type expr))))

  (extend-parsed->raw type:-form?
    (lambda (expr p->r)
      `(type: ,(type:-form-type expr) ,@(type:-form-attrs expr))))

  (extend-parsed->raw st:control-form?
    (lambda (expr p->r)
      `(mrspidey:control ,(st:control-form-para expr)
	 ,(st:control-form-val expr))))

  (extend-parsed->raw reference-unit-form?
    (lambda (expr p->r)
      (case (reference-unit-form-kind expr)
	((exp) `(,(if (reference-unit-form-signed? expr)
		    'require-unit/sig
		    'require-unit)
		  ,(sexp->raw (reference-unit-form-file expr))))
	((imp) `(require-unit-imports
		  ,(sexp->raw (reference-unit-form-file expr))))
	(else (internal-error 'require-unit-form "Invalid kind")))))

  (extend-parsed->raw define-type-form?
    (lambda (expr p->r)
      `(define-type ,(define-type-form-sym expr)
	 ,(define-type-form-type expr))))

  (extend-parsed->raw define-constructor-form?
    (lambda (expr p->r)
      `(define-constructor-form ,(define-constructor-form-sym expr)
	 ,@(define-constructor-form-modes expr))))

  )
