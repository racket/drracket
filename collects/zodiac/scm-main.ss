; $Id: scm-main.ss,v 1.205 2000/04/30 22:31:01 clements Exp $

(unit/sig zodiac:scheme-main^
  (import zodiac:misc^ zodiac:structures^
    (z : zodiac:scanner-parameters^)
    (z : zodiac:reader-structs^)
    (z : zodiac:reader-code^)
    zodiac:sexp^ (pat : zodiac:pattern^) zodiac:scheme-core^
    zodiac:back-protocol^ zodiac:expander^ zodiac:interface^)

  ; ----------------------------------------------------------------------

  (define-struct (if-form struct:form) (test then else))
  (define-struct (set!-form struct:form) (var val))
  (define-struct (define-values-form struct:form) (vars val))
  (define-struct (let-values-form struct:form) (vars vals body))
  (define-struct (letrec-values-form struct:form) (vars vals body))
  (define-struct (quote-form struct:form) (expr))
  (define-struct (begin-form struct:form) (bodies))
  (define-struct (begin0-form struct:form) (bodies))
  (define-struct (case-lambda-form struct:form) (args bodies))
  (define-struct (struct-form struct:form) (type super fields))
  (define-struct (with-continuation-mark-form struct:form) (key val body))

  ; ----------------------------------------------------------------------

  (define create-const
    (lambda (c s)
      (make-quote-form (zodiac-origin s)
	(zodiac-start s) (zodiac-finish s)
	(make-empty-back-box) c)))

  (define expands<%> (interface () expand))

  (add-lit-micro
   common-vocabulary
   (lambda (expr env attributes vocab)
     (if (z:external? expr)
	 (let ([obj (z:read-object expr)])
	   (if (is-a? obj expands<%>)
	       (expand-expr (send obj expand expr) env attributes vocab)
	       (create-const expr expr)))
	 (create-const expr expr))))

  ; ----------------------------------------------------------------------

  (define create-case-lambda-form
    (lambda (args bodies source)
      (make-case-lambda-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) args bodies)))

  (define create-if-form
    (lambda (test then else source)
      (make-if-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) test then else)))

  (define create-begin-form
    (lambda (bodies source)
      (make-begin-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) bodies)))

  (define create-begin0-form
    (lambda (bodies source)
      (make-begin0-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) bodies)))

  (define create-quote-form
    (lambda (expr source)
      (make-quote-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) expr)))

  (define create-set!-form
    (lambda (var val source)
      (make-set!-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) var val)))

  (define create-define-values-form
    (lambda (vars val source)
      (make-define-values-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) vars val)))

  (define create-let-values-form
    (lambda (vars vals body source)
      (make-let-values-form  (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) vars vals body)))

  (define create-letrec-values-form
    (lambda (vars vals body source)
      (make-letrec-values-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) vars vals body)))

  (define create-struct-form
    (lambda (type super fields source)
      (make-struct-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) type super fields)))

  (define create-with-continuation-mark-form
    (lambda (key val body source)
      (make-with-continuation-mark-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) key val body)))

  ; ----------------------------------------------------------------------

  (extend-parsed->raw if-form?
    (lambda (expr p->r)
      `(if ,(p->r (if-form-test expr))
	 ,(p->r (if-form-then expr))
	 ,(p->r (if-form-else expr)))))

  (extend-parsed->raw set!-form?
    (lambda (expr p->r)
      `(set! ,(p->r (set!-form-var expr))
	 ,(p->r (set!-form-val expr)))))

  (extend-parsed->raw define-values-form?
    (lambda (expr p->r)
      `(define-values ,(map p->r (define-values-form-vars expr))
	 ,(p->r (define-values-form-val expr)))))

  (extend-parsed->raw let-values-form?
    (lambda (expr p->r)
      `(let-values
	 ,(map (lambda (vars val)
		 (list (map p->r vars) (p->r val)))
	    (let-values-form-vars expr) (let-values-form-vals expr))
	 ,(p->r (let-values-form-body expr)))))

  (extend-parsed->raw letrec-values-form?
    (lambda (expr p->r)
      `(letrec-values 
	 ,(map (lambda (vars val)
		 (list (map p->r vars) (p->r val)))
	    (letrec-values-form-vars expr) (letrec-values-form-vals expr))
	 ,(p->r (letrec-values-form-body expr)))))

  (extend-parsed->raw quote-form?
    (lambda (expr p->r)
      `(quote ,(sexp->raw (quote-form-expr expr)))))

  (extend-parsed->raw begin-form?
    (lambda (expr p->r)
      `(begin ,@(map p->r (begin-form-bodies expr)))))

  (extend-parsed->raw begin0-form?
    (lambda (expr p->r)
      `(begin0 ,@(map p->r (begin0-form-bodies expr)))))

  (extend-parsed->raw case-lambda-form?
    (lambda (expr p->r)
      `(case-lambda
	 ,@(map (lambda (arg body)
		  `(,(p->r arg) ,(p->r body)))
	     (case-lambda-form-args expr)
	     (case-lambda-form-bodies expr)))))

  (extend-parsed->raw struct-form?
    (lambda (expr p->r)
      `(struct
	 ,(if (struct-form-super expr)
	    (list (sexp->raw (struct-form-type expr))
	      (p->r (struct-form-super expr)))
	    (sexp->raw (struct-form-type expr)))
	 ,(map sexp->raw (struct-form-fields expr)))))

  (extend-parsed->raw with-continuation-mark-form?
    (lambda (expr p->r)
      `(with-continuation-mark
	   ,(p->r (with-continuation-mark-form-key expr))
	   ,(p->r (with-continuation-mark-form-val expr))
	 ,(p->r (with-continuation-mark-form-body expr)))))

  ; ----------------------------------------------------------------------

  (define (get-expr-pattern begin?)
    (if begin?
	(if (eq? begin? 'optional)
	    '(expr ...)
	    '(expr0 expr ...))
	'(expr)))

  (define parse-expr
    (lambda (who-str kwd:who expr bodies env attributes vocab source)
      ;; Do internal definition parsing
      (let*-values
	  (((internal-define-vocab)
	    (append-vocabulary internal-define-vocab-delta
			       vocab 'internal-define-vocab))
	   ((definitions parsed-first-term rest-terms bindings)
	    (let loop ((seen null) (rest bodies) (prev #f) (bindings null) (vars-seen null))
	      (if (null? rest)
		  (static-error
		    "internal definition" 'term:internal-def-not-foll-by-expr
		    prev
		    (if (null? seen)
		      (static-error
			who-str kwd:who
			expr "malformed expression")
		      (if (null? (cdr seen))
			"internal definition not followed by expression"
			"internal definitions not followed by expression")))
		  (let ((first (car rest)))
		    (let* ((internal? (get-internal-define-status attributes))
			   (_ (set-internal-define-status attributes #t))
			   (e-first (expand-expr first env
						 attributes
						 internal-define-vocab))
			   (_ (set-internal-define-status attributes internal?)))
		      (cond
		       [(internal-definition? e-first)
			(let ((def-vars (internal-definition-vars e-first)))
			  (let* ((new-vars+marks
				  (map create-lexical-binding+marks
				       def-vars)))
			    (for-each
			     (lambda (v)
			       (when (memq (z:read-object v)
					   vars-seen)
				 (static-error
				   "internal definition"
				   'term:duplicate-internal-def
				   v
				   "duplicate definition for identifier ~a"
				   (z:read-object v))))
			     def-vars)
			    (extend-env new-vars+marks env)
			    (loop (cons e-first seen)
				  (cdr rest)
				  first
				  (cons new-vars+marks bindings)
				  (append vars-seen
					  (map z:read-object def-vars)))))]
		       [(internal-begin? e-first)
			(loop seen 
			      (append (internal-begin-exprs e-first) (cdr rest))
			      first
			      bindings vars-seen)]
		       [else
			(values (reverse seen)
				e-first
				(cdr rest)
				bindings)])))))))
	(if (null? definitions)

	    ;; No internal defines
	    (if (null? rest-terms)
		parsed-first-term
		(create-begin-form
		 (cons parsed-first-term
		       (map (lambda (e)
			      (expand-expr e env attributes
					   vocab))
			    rest-terms))
		 expr))

	    ;; Found internal defines
	    (begin0
	     (create-letrec-values-form
	      (reverse (map (lambda (vars+marks)
			      (map car vars+marks))
			    bindings))
	      (map (lambda (def)
		     (expand-expr (internal-definition-val def)
				  env attributes vocab))
		   definitions)
	      (if (null? rest-terms)
		  parsed-first-term
		  (create-begin-form
		   (cons parsed-first-term
			 (map (lambda (e)
				(expand-expr e env attributes vocab))
			      rest-terms))
		   expr))
	      expr)
	     (for-each (lambda (new-vars+marks)
			 (retract-env (map car new-vars+marks) env))
		       bindings))))))

  ; ----------------------------------------------------------------------

  (define (make-lambda-error-micro who)
    (lambda (expr env attributes vocab)
      (static-error
	"lambda" 'term:case/lambda-only-in-def
	expr "allowed only in a definition")))

  (define (make-case-lambda-micro begin? arglist-decls-vocab)
    (let* ((kwd `(else))
	    (in-pattern `(_
			   (args ,@(get-expr-pattern begin?))
			   ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((args (pat:pexpand '(args ...) p-env kwd))
		     (bodies (pat:pexpand `((,@(get-expr-pattern begin?)) ...)
			       p-env kwd)))
		(let ((arglists+exprs
		       (map
			(lambda (arg body)
			  (distinct-valid-syntactic-id/s? arg)
			  (let* ((arglist
				  (expand-expr arg env attributes
					       arglist-decls-vocab))
				 (arg-vars+marks
				  (arglist-vars arglist)))
			    (extend-env arg-vars+marks env)
			    (begin0
			     (cons
			      (make-argument-list arglist)
			      (as-nested
			       attributes
			       (lambda ()
				 (parse-expr "case-lambda" 'kwd:case-lambda
				   expr body env attributes vocab expr))))
			     (retract-env (map car arg-vars+marks) env))))
			args bodies)))
		  (create-case-lambda-form
		    (map car arglists+exprs)
		    (map cdr arglists+exprs)
		    expr)))))
	  (else
	    (static-error
	      "case-lambda" 'kwd:case-lambda
	      expr "malformed expression"))))))

  (define beginner+lambda-vocabulary
    (create-vocabulary 'beginner+lambda-vocabulary
		       beginner-vocabulary))
  (set-subexpr-vocab! beginner+lambda-vocabulary beginner-vocabulary)

  (add-primitivized-micro-form
   'case-lambda
   beginner+lambda-vocabulary
   (make-case-lambda-micro #f lambda-nonempty-arglist-decls-vocab))
  (add-primitivized-micro-form
   'case-lambda
   beginner-vocabulary
   (make-lambda-error-micro 'case-lambda))
  (add-primitivized-micro-form
   'case-lambda
   intermediate-vocabulary
   (make-case-lambda-micro #f lambda-nonempty-arglist-decls-vocab))
  (add-primitivized-micro-form
   'case-lambda
   advanced-vocabulary
   (make-case-lambda-micro #f lambda-full-arglist-decls-vocab))
  (add-primitivized-micro-form
   'case-lambda
   scheme-vocabulary
   (make-case-lambda-micro #t lambda-full-arglist-decls-vocab))

  (define (make-lambda-macro begin?)
    (let* ((kwd '())
	    (in-pattern `(_ args ,@(get-expr-pattern begin?)))
	    (out-pattern `(case-lambda
			    (args ,@(get-expr-pattern begin?))))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error
	    "lambda" 'kwd:lambda
	    expr "malformed expression")))))

  (add-primitivized-macro-form 
   'lambda
   beginner+lambda-vocabulary
   (make-lambda-macro #f))
  (add-primitivized-micro-form
   'lambda
   beginner-vocabulary
   (make-lambda-error-micro 'lambda))
  (add-primitivized-macro-form
   'lambda
   intermediate-vocabulary
   (make-lambda-macro #f))
  (add-primitivized-macro-form
   'lambda
   advanced-vocabulary
   (make-lambda-macro #f))
  (add-primitivized-macro-form
   'lambda
   scheme-vocabulary
   (make-lambda-macro #t))

  (define-struct internal-definition (vars val))
  (define-struct internal-begin (exprs))

  (define internal-define-vocab-delta
    (create-vocabulary 'internal-define-vocab-delta))

  (add-primitivized-micro-form 'define-values internal-define-vocab-delta
    (let* ((kwd '())
	   (in-pattern `(_ (var ...) val))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(unless (at-internal-define? attributes)
	  (static-error
	    "internal definition" 'term:define-internal-invalid-posn
	    expr "invalid position"))
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let* ((vars (pat:pexpand '(var ...) p-env kwd))
		      (_ (map valid-syntactic-id? vars))
		      (val (pat:pexpand 'val p-env kwd)))
		(for-each (lambda (var)
			    (let ((r (resolve var env vocab)))
			      (when (or (micro-resolution? r)
				      (macro-resolution? r))
				(static-error
				  "keyword" 'term:cannot-bind-kwd
				  var
				  "cannot bind keyword ~s"
				  (z:symbol-orig-name var)))))
		  vars)
		(make-internal-definition vars val))))
	  (else
	    (static-error expr
	      "internal definition" 'kwd:define
	      "malformed definition"))))))

  (add-primitivized-micro-form 'begin internal-define-vocab-delta
    (let* ((kwd '())
	   (in-pattern `(_ expr ...))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(if (at-internal-define? attributes)

	    ;; Parse begin in internal define context
	    (cond
	     ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let* ((exprs (pat:pexpand '(expr ...) p-env kwd)))
		  (make-internal-begin exprs))))
	     (else
	      (static-error
		"internal begin" 'kwd:begin
		expr "malformed expression")))

	    ;; Chain to regular begin:
	    (begin-micro expr env attributes vocab)))))
  
  (define begin-micro
    (let* ((kwd '())
	   (in-pattern `(_ b ...))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	 ((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let* ([bodies (pat:pexpand '(b ...) p-env kwd)]
		   [top? (get-top-level-status attributes)]
		   [as-nested (if top? (lambda (x y) (y)) as-nested)])
	      (if (and (pair? bodies) (null? (cdr bodies)))
		  (as-nested
		   attributes
		   (lambda ()
		     (expand-expr (car bodies) env attributes vocab)))
		  (if (and (not top?)
			   (null? bodies))
		      (static-error
			"begin" 'kwd:begin
			expr "malformed expression")
		      (as-nested
		       attributes
		       (lambda ()
			 (create-begin-form
			  (map (lambda (e)
				 (expand-expr e env attributes vocab))
			       bodies)
			  expr))))))))
	 (else
	  (static-error
	    "begin" 'kwd:begin
	    expr "malformed expression"))))))

  (add-primitivized-micro-form 'begin advanced-vocabulary begin-micro)
  (add-primitivized-micro-form 'begin scheme-vocabulary begin-micro)

  (define begin0-micro
      (let* ((kwd '())
	      (in-pattern `(_ b0 b ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((bodies (pat:pexpand '(b ...) p-env kwd))
		      (body0 (pat:pexpand 'b0 p-env kwd)))
		  (let ([first (as-nested 
				attributes
				(lambda () (expand-expr body0 env attributes vocab)))])
		    (if (null? bodies)
			first
			(let ([rest (as-nested
				     attributes
				     (lambda ()
				       (map
					(lambda (expr)
					  (expand-expr expr env attributes vocab))
					bodies)))])
			  (create-begin0-form
			   (cons first rest)
			   expr)))))))
	    (else
	     (static-error
	       "begin0" 'kwd:begin0
	       expr "malformed expression"))))))

  (add-primitivized-micro-form 'begin0 advanced-vocabulary begin0-micro)
  (add-primitivized-micro-form 'begin0 scheme-vocabulary begin0-micro)

  (define (make-if-micro one-arm-ok?)
    (let* ((kwd '())
	    (in-pattern-1 `(_ test then))
	    (in-pattern-2 `(_ test then else))
	    (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	    (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (unless one-arm-ok?
		(static-error
		  "if" 'term:if-must-have-else
		  expr "must have an else clause"))
	      (as-nested
	       attributes
	       (lambda ()
		 (set-macro-origin
		  (expand-expr
		   (structurize-syntax
		    (pat:pexpand '(if test then (#%void)) p-env kwd)
		    expr '(-1)
		    #f
		    (make-origin 'micro expr))
		   env attributes vocab)
		  (syntax-car expr))))))
	  ((pat:match-against m&e-2 expr env)
	    =>
	    (lambda (p-env)
	      (as-nested
	       attributes
	       (lambda ()
		 (let* ((test-exp (expand-expr
				   (pat:pexpand 'test p-env kwd)
				   env attributes vocab))
			(then-exp (expand-expr
				   (pat:pexpand 'then p-env kwd)
				   env attributes vocab))
			(else-exp (expand-expr
				   (pat:pexpand 'else p-env kwd)
				   env attributes vocab)))
		   (create-if-form test-exp then-exp else-exp expr))))))
	  (else
	    (static-error
	      "if" 'kwd:if
	      expr "malformed expression"))))))

  (add-primitivized-micro-form 'if beginner-vocabulary (make-if-micro #f))
  (add-primitivized-micro-form 'if advanced-vocabulary (make-if-micro #t))
  (add-primitivized-micro-form 'if scheme-vocabulary (make-if-micro #t))

  (define with-continuation-mark-micro
    (let* ((kwd '())
	   (in-pattern `(_ key val body))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	 ((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (as-nested
	     attributes
	     (lambda ()
	       (let* ((key-exp (expand-expr
				(pat:pexpand 'key p-env kwd)
				env attributes vocab))
		      (val-exp (expand-expr
				(pat:pexpand 'val p-env kwd)
				env attributes vocab))
		      (body-exp (expand-expr
				 (pat:pexpand 'body p-env kwd)
				 env attributes vocab)))
		 (create-with-continuation-mark-form key-exp val-exp body-exp expr))))))
	 (else
	  (static-error
	    "with-continuation-mark" 'kwd:with-continuation-mark
	    expr "malformed expression"))))))
  
  (add-primitivized-micro-form 'with-continuation-mark scheme-vocabulary with-continuation-mark-micro)

  ; Don't "simplify" this.  If replaced with a pattern match, it will
  ; die when passed a quote form whose underlying object is an actual
  ; Scheme value (as opposed to a struct:read), because the matcher
  ; will attempt to extract the source locations of the underlying
  ; object, which will fail in this case.

  (define (make-quote-micro non-sym-ok?)
    (lambda (expr env attributes vocab)
      (if (and (z:list? expr)
	    (= 2 (z:sequence-length expr)))
	(let ((contents (expose-list expr)))
	  (if (and (z:symbol? (car contents))
		(or (eq? 'quote (z:read-object (car contents)))
		  (eq? '#%quote (z:read-object (car contents)))))
	    (if (or non-sym-ok?
		  (z:symbol? (cadr contents)))
	      (create-quote-form (cadr contents) expr)
	      (let*-values ([(v) (sexp->raw (cadr contents))]
			    [(v prefix)
			     ;; Strip leading quotes, because user most likely typed ''x
			     ;; instead of '(quote x)
			     (let loop ([v v][prefix ""])
			       (cond
				[(and (pair? v)
				      (eq? (car v) 'quote)
				      (pair? (cdr v))
				      (null? (cddr v)))
				 (loop (cadr v) (string-append "'" prefix))]
				[else (values v prefix)]))])
		(static-error
		  "quote" 'term:quote-not-on-symbol
		  expr "misused: '~a~s is not a symbol" prefix v)))
	    (static-error
	      "quote" 'kwd:quote
	      expr "malformed expression")))
	(static-error
	  "quote" 'kwd:quote
	  expr "malformed expression"))))

  (add-primitivized-micro-form 'quote beginner-vocabulary (make-quote-micro #f))
  (add-primitivized-micro-form 'quote intermediate-vocabulary (make-quote-micro #t))
  (add-primitivized-micro-form 'quote scheme-vocabulary (make-quote-micro #t))

  (define (make-set!-micro dont-mutate-lambda-varrefs?)
    (let* ((kwd '())
	   (in-pattern `(_ var val))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(let ((p-env (pat:match-against m&e expr env)))
	  (if p-env
	      (let* ((var-p (pat:pexpand 'var p-env kwd))
		     (_ (valid-syntactic-id? var-p))
		     (id-expr (expand-expr var-p env attributes
					   vocab))
		     (expr-expr (as-nested
				 attributes
				 (lambda ()
				   (expand-expr
				    (pat:pexpand 'val p-env kwd)
				    env attributes vocab)))))
		(when (and (lambda-varref? id-expr)
			dont-mutate-lambda-varrefs?)
		  (static-error
		    "set!" 'term:set!-no-mutate-lambda-bound
		    expr "cannot mutate procedure-bound identifiers"))
		(create-set!-form id-expr expr-expr expr))
	      (static-error
		"set!" 'kwd:set!
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'set! 
			       advanced-vocabulary
			       (make-set!-micro #t))
  (add-primitivized-micro-form 'set!
			       scheme-vocabulary
			       (make-set!-micro #f))
  
  (define set!-values-micro
      (let* ((kwd '())
	      (in-pattern '(_ (vars ...) val))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let* ((vars (pat:pexpand '(vars ...) p-env kwd))
		       (val (pat:pexpand 'val p-env kwd)))
		  (map valid-syntactic-id? vars)
		  (let ((new-names (map generate-name vars)))
		    (expand-expr
		      (structurize-syntax
			`(let-values ((,new-names ,val))
			   ,@(map (lambda (var new-name)
				    `(set! ,var ,new-name))
			       vars new-names)
			   (#%void))
			expr '(-1)
			#f
			(make-origin 'micro expr))
		      env attributes vocab)))))
	    (else
	      (static-error
		"set!-values" 'kwd:set!-values
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'set!-values advanced-vocabulary set!-values-micro)
  (add-primitivized-micro-form 'set!-values scheme-vocabulary set!-values-micro)

  (define (make-local-extract-vocab)
    (create-vocabulary 'local-extract-vocab #f
      "invalid expression for local clause"
      "invalid expression for local clause"
      "invalid expression for local clause"
      "invalid expression for local clause"))

  (define nobegin-local-extract-vocab (make-local-extract-vocab))
  (define full-local-extract-vocab (make-local-extract-vocab))

  (define (make-local-micro begin? local-extract-vocab)
      (let* ((kwd '())
	      (in-pattern `(_ (defs ...) ,@(get-expr-pattern begin?)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (let ((p-env (pat:match-against m&e expr env)))
	    (if p-env
	      (let ((top-level? (get-top-level-status attributes))
		    (internal? (get-internal-define-status attributes)))
		(set-top-level-status attributes #t)
		(set-internal-define-status attributes #f)
		(let*
		  ((defs (pat:pexpand '(defs ...) p-env kwd))
		    (vars+exprs
		      (map
			(lambda (e)
			  (let ((out
				  (expand-expr e env
				    attributes
				    local-extract-vocab)))
			    out))
			defs)))
		  (set-top-level-status attributes)
		  (begin0
		    (set-macro-origin
		      (expand-expr
			(structurize-syntax
			  `(letrec-values
			     ,(map (lambda (vars+expr)
				     `(,(car vars+expr) ,(cdr vars+expr)))
				vars+exprs)
			     ,@(pat:pexpand (get-expr-pattern begin?) p-env kwd))
			  expr '(-1)
			  #f
			  (make-origin 'micro expr))
			env attributes vocab)
		      (syntax-car expr))
		    (set-top-level-status attributes top-level?)
		    (set-internal-define-status attributes internal?))))
	      (static-error
		"local" 'kwd:local
		expr "malformed expression"))))))

  (add-primitivized-micro-form
   'local
   intermediate-vocabulary
   (make-local-micro #f nobegin-local-extract-vocab))
  (add-on-demand-form
   'micro
   'local
   intermediate-vocabulary
   (make-local-micro #f nobegin-local-extract-vocab))

;  (add-primitivized-micro-form
;   'local
;   advanced-vocabulary
;   (make-local-micro #t full-local-extract-vocab))
;  (add-on-demand-form
;   'micro
;   'local
;   advanced-vocabulary
;   (make-local-micro #t full-local-extract-vocab))

  (add-on-demand-form
   'micro
   'local
   scheme-vocabulary
   (make-local-micro #t full-local-extract-vocab))

  (define (make-define-forms begin?)
    (let* ((kwd '())
	   (in-pattern-1 `(_ (fun . args) ,@(get-expr-pattern begin?)))
	   (out-pattern-1 `(define-values (fun) (lambda args ,@(get-expr-pattern begin?))))
	   (in-pattern-2 `(_ var val))
	   (out-pattern-2 `(define-values (var) val))
	   (in-pattern-3 `(_ (fun . args) b0 b1 ...)) ;; for error reporting
	   (in-pattern-4 `(_ (fun . args))) ;; for error reporting
	   (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	   (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	   (m&e-3 (pat:make-match&env in-pattern-3 kwd))
	   (m&e-4 (pat:make-match&env in-pattern-4 kwd)))
      (values
       (lambda (expr env)
	 (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	   (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	   (or (and (not begin?)
		 (or (pat:match-against m&e-3 expr env)
		   (pat:match-against m&e-4 expr env))
		 (static-error
		   "define" 'term:define-illegal-implicit-begin
		   expr "body must have exactly one expression"))
	     (static-error
	       "define" 'kwd:define
	       expr "malformed definition"))))
       (lambda (expr env attributes vocab)
	 (cond
	  ((pat:match-against m&e-1 expr env)
	   =>
	   (lambda (p-env)
	     (let ((fun (pat:pexpand 'fun p-env kwd))
		   (expr (pat:pexpand `(lambda args ,@(get-expr-pattern begin?))
				      p-env kwd)))
	       (valid-syntactic-id? fun)
	       (cons (list fun) expr))))
	  ((pat:match-against m&e-2 expr env)
	   =>
	   (lambda (p-env)
	     (let ((var (pat:pexpand 'var p-env kwd))
		   (val (pat:pexpand 'val p-env kwd)))
	       (valid-syntactic-id? var)
	       (cons (list var) val))))
	  (else
	   (static-error
	     "local define" 'kwd:define
	     expr "malformed definition")))))))

  (define-values
    (nobegin-define-form nobegin-local-define-form) (make-define-forms #f))
  (define-values
    (full-define-form full-local-define-form) (make-define-forms #t))

  (add-primitivized-macro-form 'define beginner-vocabulary nobegin-define-form)
;  (add-primitivized-macro-form 'define advanced-vocabulary full-define-form)
  (add-primitivized-macro-form 'define scheme-vocabulary full-define-form)

  (add-primitivized-micro-form 'define
			       full-local-extract-vocab
			       full-local-define-form)
  (add-primitivized-micro-form 'define
			       nobegin-local-extract-vocab
			       nobegin-local-define-form)

  (let* ((kwd '())
	 (in-pattern-1 `(_ (var ...) val))
	 (m&e-1 (pat:make-match&env in-pattern-1 kwd)))
    (let ((define-values-helper
	    (lambda (internal-ok? handler)
	      (lambda (expr env attributes vocab)
		(unless (at-top-level? attributes)
		  (static-error
		    "definition"
		    (if internal-ok?
		      'term:invalid-intl-defn-posn
		      'term:def-not-at-top-level)
		    expr
		    (if internal-ok?
		      "invalid position for internal definition"
		      "must be at the top level")))
		(cond
		  ((pat:match-against m&e-1 expr env)
		    =>
		    (lambda (p-env)
		      (let* ((vars (pat:pexpand '(var ...)
						p-env kwd))
			     (_ (map valid-syntactic-id? vars))
			     (val (pat:pexpand 'val p-env kwd))
			     (out (as-nested
				   attributes
				   (lambda ()
				     (handler expr env
					      attributes vocab vars val)))))
			out)))
		  (else (static-error
			  "define-values" 'kwd:define-values
			  expr
			  "malformed definition")))))))
      (let ([make-dv-micro
	     (lambda (internal-ok? use-beg-lambda-vocab?)
	       (define-values-helper
		 internal-ok?
		 (lambda (expr env attributes vocab vars val)
		   (let* ((id-exprs (map (lambda (v)
					   (expand-expr v env
							attributes vocab))
					 vars))
			  (expr-expr (as-nested
				      attributes
				      (lambda ()
					(expand-expr val env
						     attributes
						     (if use-beg-lambda-vocab?
							 beginner+lambda-vocabulary
							 vocab))))))
		     (create-define-values-form id-exprs
						expr-expr expr)))))])
	(add-primitivized-micro-form 'define-values
				     beginner-vocabulary
				     (make-dv-micro #f #t))
	(add-primitivized-micro-form 'define-values
				     intermediate-vocabulary
				     (make-dv-micro #f #f))
	(add-primitivized-micro-form 'define-values
				     advanced-vocabulary
				     (make-dv-micro #f #f))
	(add-primitivized-micro-form 'define-values 
				     scheme-vocabulary
				     (make-dv-micro #t #f)))
      (let ([int-dv-micro (define-values-helper
			    #t
			    (lambda (expr env attributes vocab vars val)
			      (cons vars val)))])
	(add-primitivized-micro-form 'define-values nobegin-local-extract-vocab int-dv-micro)
	(add-primitivized-micro-form 'define-values full-local-extract-vocab int-dv-micro))))

  (define extract-type&super
    (let* ((kwd '())
	   (ts-pattern '(type super))
	   (m&e-ts (pat:make-match&env ts-pattern kwd)))
      (lambda (type-spec env allow-supertype?)
	(if allow-supertype?
	    (cond
	     ((pat:match-against m&e-ts type-spec env)
	      =>
	      (lambda (tsp-env)
		(let* ((type (pat:pexpand 'type tsp-env '()))
		       (super (pat:pexpand 'super tsp-env '())))
		  (and (or (z:symbol? type)
			   (static-error
			     "structure definition" 'term:struct-not-id
			     type "not an identifier"))
		       (values type super)))))
	     ((z:symbol? type-spec)
	      (values type-spec #f))
	     (else
	      (static-error
		"super-structure definition" 'term:super-struct-invalid
		type-spec "invalid specification")))
	    (begin
	      (unless (z:symbol? type-spec)
		(static-error
		  "super-structure definition" 'term:super-struct-not-id
		  type-spec "not an identifier"))
	      (values type-spec #f))))))

  (define (make-struct-micro allow-supertype?)
      (let* ((kwd '())
	      (in-pattern `(_ type-spec (fields ...)))
	      (m&e-in (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e-in expr env)
	      =>
	      (lambda (p-env)
		(let* ((fields (pat:pexpand '(fields ...) p-env kwd))
			(type-spec (pat:pexpand 'type-spec p-env kwd)))
		  (distinct-valid-syntactic-id/s? fields)
		  (let-values (((type super)
				(extract-type&super type-spec env allow-supertype?)))
		    (create-struct-form
		      type
		      (and super (as-nested attributes (lambda () (expand-expr super env attributes vocab))))
		      fields
		      expr)))))
	    (else
	      (static-error
		"struct" 'kwd:struct
		expr "malformed definition"))))))

  (add-primitivized-micro-form 'struct beginner-vocabulary (make-struct-micro #f))
  (add-primitivized-micro-form 'struct advanced-vocabulary (make-struct-micro #t))
  (add-primitivized-micro-form 'struct scheme-vocabulary (make-struct-micro #t))

  (define generate-struct-names
    (opt-lambda (type fields source
		  (omit-selectors? #f) (omit-setters? #f))
      (let ((name (lambda parts
		    (structurize-syntax
		      (apply symbol-append parts)
		      source))))
	(let ((type (z:read-object type))
	       (fields (map z:read-object fields)))
	  (cons
	    (name "struct:" type)
	    (cons
	      (name "make-" type)
	      (cons
		(name type "?")
		(apply append
		  (map (lambda (field)
			 (append
			   (if omit-selectors?
			     '()
			     (list (name type "-" field)))
			   (if omit-setters?
			     '()
			     (list (name "set-" type "-" field "!")))))
		    fields)))))))))

    (let* ((kwd '())
	   (in-pattern '(_ type-spec (fields ...)))
	   (m&e-in (pat:make-match&env in-pattern kwd)))
      (let ((make-ds-micro
	     (lambda (handler allow-supertype?)
	       (lambda (expr env attributes vocab)
		 (cond
		  ((pat:match-against m&e-in expr env)
		   =>
		   (lambda (p-env)
		     (let ((fields (pat:pexpand '(fields ...) p-env kwd))
			   (type-spec (pat:pexpand 'type-spec p-env kwd)))
		       (distinct-valid-syntactic-id/s? fields)
		       (let*-values
			   (((type super) (extract-type&super type-spec env allow-supertype?))
			    ((names) (generate-struct-names type fields expr))
			    ((struct-expr)
			     `(struct ,type-spec ,fields)))
			 (handler expr env attributes vocab
				  names struct-expr)))))
		  (else
		   (static-error
		     "define-struct" 'kwd:define-struct
		     expr "malformed definition")))))))
	(let ([top-level-handler
	       (lambda (expr env attributes vocab names struct-expr)
		 (expand-expr
		  (structurize-syntax
		   `(define-values ,names ,struct-expr)
		   expr '(-1)
		   #f
		   (make-origin 'micro expr))
		  env attributes vocab))]
	      [internal-handler
	       (lambda (expr env attributes vocab names struct-expr)
		 (cons names struct-expr))])

	  (add-primitivized-micro-form 'define-struct beginner-vocabulary 
				       (make-ds-micro top-level-handler #f))
	  (add-primitivized-micro-form 'define-struct advanced-vocabulary
				       (make-ds-micro top-level-handler #t))
	  (add-primitivized-micro-form 'define-struct scheme-vocabulary
				       (make-ds-micro top-level-handler #t))

	  (add-primitivized-micro-form 'define-struct nobegin-local-extract-vocab
				       (make-ds-micro internal-handler #f))
	  (add-primitivized-micro-form 'define-struct full-local-extract-vocab
				       (make-ds-micro internal-handler #t)))))

    (let* ((kwd '())
	   (in-pattern '(_ (type-spec fields ...)))
	   (out-pattern '(define-struct type-spec (fields ...)))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (add-primitivized-macro-form 'define-structure intermediate-vocabulary
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	      (static-error
		"define-structure" 'kwd:define-structure
		expr "malformed definition"))))
      (let ([int-ds-macro (lambda (expr env)
			    (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
				(static-error
				  "define-structure" 'kwd:define-structure
				  expr "malformed definition")))])
	(add-primitivized-macro-form 'define-structure nobegin-local-extract-vocab int-ds-macro)
	(add-primitivized-macro-form 'define-structure full-local-extract-vocab int-ds-macro)))

    (define (make-let-struct-micro begin? allow-supertype?)
      (let* ((kwd '())
	     (in-pattern `(_ type-spec (fields ...) ,@(get-expr-pattern begin?)))
	     (m&e-in (pat:make-match&env in-pattern kwd)))
	(let ((ls-core
	       (lambda (handler)
		 (lambda (expr env attributes vocab)
		   (cond
		    ((pat:match-against m&e-in expr env)
		     =>
		     (lambda (p-env)
		       (handler expr env attributes vocab p-env)))
		    (else
		     (static-error
		       "let-struct" 'kwd:let-struct
		       expr "malformed expression")))))))
	  (ls-core
	   (lambda (expr env attributes vocab p-env)
	     (let* ((fields (pat:pexpand '(fields ...) p-env kwd))
		    (type-spec (pat:pexpand 'type-spec p-env kwd))
		    (body (pat:pexpand `(,@(get-expr-pattern begin?)) p-env kwd)))
	       (distinct-valid-syntactic-id/s? fields)
	       (let-values (((type super)
			     (extract-type&super type-spec env allow-supertype?)))
		 (expand-expr
		  (structurize-syntax
		   `(let-values
			((,(generate-struct-names type fields expr)
			  (struct ,type-spec ,fields)))
		      ,@body)
		   expr '(-1)
		   #f
		   (make-origin 'micro expr))
		  env attributes vocab))))))))

    (add-primitivized-micro-form 'let-struct
				 intermediate-vocabulary
				 (make-let-struct-micro #f #f))
;    (add-primitivized-micro-form 'let-struct
;				 advanced-vocabulary
;				 (make-let-struct-micro #t #t))
    (add-primitivized-micro-form 'let-struct
				 scheme-vocabulary
				 (make-let-struct-micro #t #t))
    
  ; ----------------------------------------------------------------------

    ; Sometimes a single source symbol appears twice in an expansion.
    ; When that happens, we mark all but the first occurrence as a
    ; "duplicate" so that syntax-processing tools can correlate
    ; identifiers in elaboated syntax to source syntax.

    (define (dup-symbol s)
      (z:make-symbol
       (make-origin 'duplicated (zodiac-origin s))
       (zodiac-start s)
       (zodiac-finish s)
       (z:read-object s)
       (z:symbol-orig-name s)
       (z:symbol-marks s)))
    
  (define (make-let-macro begin? named?)
      ;; >> Broken by current embedded define hacks! <<
      ;; e.g., (let ([a 7]) (let-macro a void (a))
      (let* ((kwd '())
	     
	     (in-pattern-1 `(_ fun ((v e) ...) ,@(get-expr-pattern begin?)))
	     (out-pattern-1 `((letrec ((fun (lambda (v ...) ,@(get-expr-pattern begin?))))
				fun-copy) ; fun-copy is fun with a different source
			      e ...))
	     
	     (in-pattern-2 `(_ ((v e) ...) ,@(get-expr-pattern begin?)))
	     (out-pattern-2 `(let-values (((v) e) ...) ,@(get-expr-pattern begin?)))

	     (m&e-1 (and named? (pat:make-match&env in-pattern-1 kwd)))
	     (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (let ((p-env (and named? (pat:match-against m&e-1 expr env))))
	    (if (and p-env (z:symbol? (pat:pexpand 'fun p-env kwd)))
		(let* ([fun (pat:pexpand 'fun p-env kwd)]
		       [fun-copy (dup-symbol fun)])
		  (pat:pexpand out-pattern-1
			       (pat:extend-penv 'fun-copy fun-copy p-env)
			       kwd))
		(or (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
		    (static-error
		      "let" 'kwd:let
		      expr "malformed expression")))))))

  (add-primitivized-macro-form 'let
			       intermediate-vocabulary
			       (make-let-macro #f #f))
  (add-primitivized-macro-form 'let
			       advanced-vocabulary
			       (make-let-macro #f #t))
  (add-primitivized-macro-form 'let scheme-vocabulary (make-let-macro #t #t))

  ; Turtle Macros for Robby
  (let ([add-patterned-macro
	 (lambda (formname form-string kwd:form-name in-pattern out-pattern)
	   (add-macro-form
	    formname
	    intermediate-vocabulary
	    (let* ((kwd (list formname))
		   (m&e (pat:make-match&env in-pattern kwd)))
	      (lambda (expr env)
		(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
		    (static-error
		      form-string kwd:form-name
		      expr "malformed expression"))))))])
    (add-patterned-macro 'tprompt "tprompt" 'kwd:tprompt      
			 '(tprompt E ...)
			 '(tpromptfn (lambda () E ...)))
    (add-patterned-macro 'split "split" 'kwd:split
			 '(split E ...)
			 '(splitfn (lambda () E ...)))
    (add-patterned-macro 'split* "split*" 'kwd:split*
			 '(split* E ...)
			 '(split*fn (list (lambda () E) ...))))
  
  (define (make-let*-macro begin?)
      (let* ((kwd '())
	      (in-pattern-1 `(_ () ,@(get-expr-pattern begin?)))
	      (out-pattern-1 `(let-values () ,@(get-expr-pattern begin?)))
	      (in-pattern-2 `(_ ((v0 e0) (v1 e1) ...) ,@(get-expr-pattern begin?)))
	      (out-pattern-2 `(let ((v0 e0)) (let* ((v1 e1) ...) ,@(get-expr-pattern begin?))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error
	      "let*" 'kwd:let*
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'let*
			       intermediate-vocabulary
			       (make-let*-macro #f))
;  (add-primitivized-macro-form 'let*
;			       advanced-vocabulary
;			       (make-let*-macro #t))
  (add-primitivized-macro-form 'let*
			       scheme-vocabulary
			       (make-let*-macro #t))

  (define delay-macro
      (let* ((kwd '())
	      (in-pattern '(_ expr))
	      (out-pattern '(#%make-promise (lambda () expr)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error
	      "delay" 'kwd:delay
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'delay advanced-vocabulary delay-macro)
  (add-primitivized-macro-form 'delay scheme-vocabulary delay-macro)

  (define (make-time-macro begin?)
    (let* ((kwd '())
	    (in-pattern
	      (if begin?
		'(_ e0 e1 ...)
		'(_ e0)))
	    (out-pattern
	      `(let-values (((v cpu user gc)
			      (#%time-apply (lambda (dont-care)
					      ,@(if begin?
						  '(e0 e1 ...)
						  '(e0)))
				(#%cons (#%quote dont-care) #%null))))
		 (#%begin
		   (#%printf
		     "cpu time: ~s real time: ~s gc time: ~s~n"
		     cpu user gc)
		   (#%apply #%values v))))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error
	    "time" 'kwd:time
	    expr "malformed expression")))))
  
  (add-primitivized-macro-form 'time intermediate-vocabulary
    (make-time-macro #f))
  (add-primitivized-macro-form 'time scheme-vocabulary
    (make-time-macro #t))

  (define break-list
    (lambda (elements counter)
      (let loop ((rev-head '()) (tail elements) (counter counter))
	(if (null? counter)
	  (values (reverse rev-head) tail)
	  (loop (cons (car tail) rev-head) (cdr tail) (cdr counter))))))

  (define (make-let-values-micro begin?)
      (let* ((kwd '())
	      (in-pattern `(_ (((v ...) e) ...) ,@(get-expr-pattern begin?)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((vars (pat:pexpand '((v ...) ...) p-env kwd))
		       (vals (pat:pexpand '(e ...) p-env kwd))
		       (body (pat:pexpand `(,@(get-expr-pattern begin?))
			       p-env kwd)))
		  (as-nested
		   attributes
		   (lambda ()
		     (let* ((all-vars (apply append vars))
			    (_ (distinct-valid-syntactic-id/s? all-vars))
			    (expanded-vals
			     (map (lambda (e)
				    (expand-expr e env attributes vocab))
				  vals))
			    (new-vars+marks
			     (map create-lexical-binding+marks all-vars))
			    (new-vars
			     (map car new-vars+marks))
			    (_
			     (extend-env new-vars+marks env)))
		       (begin0
			(create-let-values-form
			 (let loop ((var-lists vars)
				    (new-vars new-vars))
			   (if (null? var-lists)
			       '()
			       (let-values (((head tail)
					     (break-list new-vars
							 (car var-lists))))
				 (cons head
				       (loop (cdr var-lists) tail)))))
			 expanded-vals
			 (parse-expr "let-values" 'kwd:let-values
			   expr body env attributes vocab expr)
			 expr)
			(retract-env new-vars env))))))))
	    (else
	      (static-error
		"let-values" 'kwd:let-values
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'let-values
			       intermediate-vocabulary
			       (make-let-values-micro #f))
;  (add-primitivized-micro-form 'let-values
;			       advanced-vocabulary
;			       (make-let-values-micro #t))
  (add-primitivized-micro-form 'let-values
			       scheme-vocabulary
			       (make-let-values-micro #t))

  (define (make-let*-values-micro begin?)
      (let* ((kwd '())
	      (in-pattern-1 `(_ () ,@(get-expr-pattern begin?)))
	      (out-pattern-1 `(let-values () ,@(get-expr-pattern begin?)))
	      (in-pattern-2 `(_ ((v0 e0) (v1 e1) ...)
			       ,@(get-expr-pattern begin?)))
	      (out-pattern-2 `(let-values ((v0 e0))
				(let*-values ((v1 e1) ...)
				  ,@(get-expr-pattern begin?))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error
	      "let*-values" 'kwd:let*-values
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'let*-values
			       intermediate-vocabulary
			       (make-let*-values-micro #f))
;  (add-primitivized-macro-form 'let*-values
;			       advanced-vocabulary
;			       (make-let*-values-micro #t))
  (add-primitivized-macro-form 'let*-values
			       scheme-vocabulary
			       (make-let*-values-micro #t))

  (define (make-letrec-values-micro begin?)
      (let* ((kwd '())
	      (in-pattern `(_ (((v ...) e) ...) ,@(get-expr-pattern begin?)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((vars (pat:pexpand '((v ...) ...) p-env kwd))
		       (vals (pat:pexpand '(e ...) p-env kwd))
		       (body (pat:pexpand `(,@(get-expr-pattern begin?))
			       p-env kwd)))
		  (let*
		    ((all-vars (apply append vars))
		      (_ (distinct-valid-syntactic-id/s? all-vars))
		      (new-vars+marks
			(map create-lexical-binding+marks all-vars))
		      (new-vars
			(map car new-vars+marks))
		      (_
			(extend-env new-vars+marks env))
		      (expanded-vals
		       (as-nested
			attributes
			(lambda ()
			  (map (lambda (e)
				 (expand-expr e env attributes vocab))
			       vals))))
		      (result
			(create-letrec-values-form
			  (let loop ((var-lists vars)
				      (new-vars new-vars))
			    (if (null? var-lists)
			      '()
			      (let-values (((head tail)
					     (break-list new-vars
					       (car var-lists))))
				(cons head
				  (loop (cdr var-lists) tail)))))
			  expanded-vals
			  (as-nested
			   attributes
			   (lambda ()
			     (parse-expr "letrec-values" 'kwd:letrec-values
			       expr body env attributes vocab expr)))
			  expr))
		      (_ (retract-env new-vars env)))
		    result))))
	    (else
	      (static-error
		"letrec-values" 'kwd:letrec-values
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'letrec-values
			       intermediate-vocabulary
			       (make-letrec-values-micro #f))
;  (add-primitivized-micro-form 'letrec-values
;			       advanced-vocabulary
;			       (make-letrec-values-micro #t))
  (add-primitivized-micro-form 'letrec-values
			       scheme-vocabulary
			       (make-letrec-values-micro #t))

  (define (make-letrec-macro begin?)
      (let* ((kwd '())
	      (in-pattern `(_ ((v e) ...) ,@(get-expr-pattern begin?)))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern `(letrec-values (((v) e) ...) ,@(get-expr-pattern begin?))))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error
	      "letrec" 'kwd:letrec
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'letrec
			       intermediate-vocabulary
			       (make-letrec-macro #f))
;  (add-primitivized-macro-form 'letrec
;			       advanced-vocabulary
;			       (make-letrec-macro #t))
  (add-primitivized-macro-form 'letrec
			       scheme-vocabulary
			       (make-letrec-macro #t))

  (define (make-or-macro boolean-result? one-or-zero-ok?)
    (let* ((kwd '())
	   (in-pattern-1 '(_))
	   (out-pattern-1 '#f)
	   (in-pattern-2 '(_ e))
	   (out-pattern-2 (if (not boolean-result?)
			      'e
			      '(if e #t #f)))
	   (in-pattern-3 '(_ e0 e1))
	   (out-pattern-3 (if (not boolean-result?)
			      '(let ((t e0)) (if t t e1))
			      '(if e0 #t (if e1 #t #f))))
	   (in-pattern-4 '(_ e0 e1 ...))
	   (out-pattern-4 (if (not boolean-result?)
			      '(let ((t e0)) (if t t (or e1 ...)))
			      '(if e0 #t (or e1 ...))))
	   (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	   (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	   (m&e-3 (pat:make-match&env in-pattern-3 kwd))
	   (m&e-4 (pat:make-match&env in-pattern-4 kwd)))
      (lambda (expr env)
	(let ((p-env (and one-or-zero-ok?
			  (pat:match-against m&e-1 expr env))))
	  (if p-env
	      (pat:pexpand out-pattern-1 p-env kwd)
	      (or (and one-or-zero-ok? 
		       (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env))
		  (and (not one-or-zero-ok?)
		       (pat:match-and-rewrite expr m&e-3 out-pattern-3 kwd env))
		  (pat:match-and-rewrite expr m&e-4 out-pattern-4 kwd env)
		  (static-error
		    "or" 'kwd:or
		    expr "malformed expression")))))))

  (add-primitivized-macro-form 'or beginner-vocabulary (make-or-macro #t #f))
  (add-primitivized-macro-form 'or advanced-vocabulary (make-or-macro #f #f))
  (add-primitivized-macro-form 'or scheme-vocabulary (make-or-macro #f #t))

  (add-primitivized-macro-form
    'nor
    beginner-vocabulary
    (let* ((kwd '())
	   (in-pattern '(_ e0 e1 ...))
	   (out-pattern '(#%not (or e0 e1 ...)))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error
	      "nor" 'kwd:nor
	      expr "malformed expression")))))

  (define (make-and-macro boolean-result? one-or-zero-ok?)
    (let* ((kwd '())
	   (in-pattern-1 '(_))
	   (out-pattern-1 '#t)
	   (in-pattern-2 '(_ e))
	   (out-pattern-2 'e)
	   (in-pattern-3 '(_ e0 e1))
	   (out-pattern-3 (if (not boolean-result?)
			      '(if e0 e1 #f)
			      '(if e0 (if e1 #t #f) #f)))
	   (in-pattern-4 '(_ e0 e1 ...))
	   (out-pattern-4 '(if e0 (and e1 ...) #f))
	   (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	   (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	   (m&e-3 (pat:make-match&env in-pattern-3 kwd))
	   (m&e-4 (pat:make-match&env in-pattern-4 kwd)))
      (lambda (expr env)
	(or (and one-or-zero-ok?
		 (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env))
	    (and one-or-zero-ok?
		 (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env))
	    (and (not one-or-zero-ok?)
		 (pat:match-and-rewrite expr m&e-3 out-pattern-3 kwd env))
	    (pat:match-and-rewrite expr m&e-4 out-pattern-4 kwd env)
	    (static-error
	      "and" 'kwd:and
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'and beginner-vocabulary (make-and-macro #t #f))
  (add-primitivized-macro-form 'and advanced-vocabulary (make-and-macro #f #f))
  (add-primitivized-macro-form 'and scheme-vocabulary (make-and-macro #f #t))

  (add-primitivized-macro-form
   'nand
   beginner-vocabulary
   (let* ((kwd '())
	  (in-pattern '(_ e0 e1 ...))
	  (out-pattern '(#%not (and e0 e1 ...)))
	  (m&e (pat:make-match&env in-pattern kwd)))
     (lambda (expr env)
       (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	   (static-error
	     "nand" 'kwd:nand
	     expr "malformed expression")))))

  (define recur-macro
      (let* ((kwd '())
	      (in-pattern '(_ fun ((v e) ...) b ...))
	      (out-pattern '(let fun ((v e) ...) b ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error
	      "recur" 'kwd:recur
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'recur advanced-vocabulary recur-macro)
  (add-on-demand-form 'macro 'recur common-vocabulary recur-macro)

  (define rec-macro
      (let* ((kwd '())
	      (in-pattern '(_ looper body))
	      (out-pattern '(letrec ((looper body)) looper-copy))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (let ((p-env (pat:match-against m&e expr env)))
	    (or (and p-env
		     (let ([looper (pat:pexpand 'looper p-env kwd)])
		       (and (valid-syntactic-id? looper)
			    (pat:pexpand
			     out-pattern
			     (pat:extend-penv 'looper-copy 
					      (dup-symbol looper)
					      p-env)
			     kwd))))
		(static-error
		  "rec" 'kwd:rec
		  expr "malformed expression"))))))

  (add-primitivized-macro-form 'rec advanced-vocabulary rec-macro)
  (add-on-demand-form 'macro 'rec common-vocabulary rec-macro)

  (define-struct cond-clause (text question answer else? =>? or?))

  (define (make-cond-clause-vocab)
    (let([qa-error-msg "clause is not in question-answer format"])
      (create-vocabulary 'cond-clause-vocab #f
			 qa-error-msg    ; symbol
			 qa-error-msg    ; literal
			 qa-error-msg    ; list
			 qa-error-msg))) ; ilist

  (define nobegin-cond-clause-vocab (make-cond-clause-vocab))
  (define full-cond-clause-vocab (make-cond-clause-vocab))

  (define (make-cond-list-micro begin? answerless?)
    (let* ((kwd '(else =>))
	    (in-pattern-1 (if (not begin?)
			    '(else answer)
			    '(else answer ...)))
	    (get-pattern-1 (if (not begin?)
			       'answer
			       '(begin answer ...)))
	    (in-pattern-3 '(question => answer))
	    (in-pattern-2 '(question => answer ...))
	    (in-pattern-5 (if (not answerless?)
			    '(question => answer) ; will not match
			    '(question)))
	    (in-pattern-4 (if (not begin?)
			    '(question answer)
			    '(question answer ...)))
	    (get-pattern-4 (if (not begin?)
			     'answer 
			     '(begin answer ...)))
	    (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	    (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	    (m&e-3 (pat:make-match&env in-pattern-3 kwd))
	    (m&e-4 (pat:make-match&env in-pattern-4 kwd))
	    (m&e-5 (pat:make-match&env in-pattern-5 kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (let ((answer (pat:pexpand get-pattern-1 p-env kwd)))
		(make-cond-clause expr #f answer #t #f #f))))
	  ((pat:match-against m&e-3 expr env)
	    =>
	    (lambda (p-env)
	      (let ((question (pat:pexpand 'question p-env kwd))
		     (answer (pat:pexpand 'answer p-env kwd)))
		(make-cond-clause expr question answer #f #t #f))))
	  ((pat:match-against m&e-2 expr env)
	    =>
	    (lambda (p-env)
	      (static-error
		"cond" 'term:cond-=>-not-foll-by-1-rcvr
		expr "=> not followed by exactly one receiver")))
	  ((pat:match-against m&e-5 expr env)
	    =>
	    (lambda (p-env)
	      (let ((question (pat:pexpand 'question p-env kwd)))
		(make-cond-clause expr question #f #f #f #t))))
	  ((pat:match-against m&e-4 expr env)
	    =>
	    (lambda (p-env)
	      (let ((question (pat:pexpand 'question p-env kwd))
		     (answer (pat:pexpand get-pattern-4 p-env kwd)))
		(make-cond-clause expr question answer #f #f #f))))
	  (else (static-error
		  "cond" 'term:cond-clause-not-in-q/a-fmt
		  expr "clause is not in question-answer format"))))))

  (add-list-micro nobegin-cond-clause-vocab (make-cond-list-micro #f #f))
  (add-list-micro full-cond-clause-vocab (make-cond-list-micro #t #t))

  (define (make-cond-micro cond-clause-vocab allow-empty?)
    (let* ((kwd '())
	   (in-pattern '(_ bodies ...))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((bodies (pat:pexpand '(bodies ...) p-env kwd)))
		(let ((exp-bodies
		       (as-nested
			attributes
			(lambda ()
			  (map (lambda (e)
				 (expand-expr e env attributes
					      cond-clause-vocab))
			       bodies)))))
		  (let ((had-no-clauses? (null? exp-bodies)))
		    (expand-expr
		      (structurize-syntax
			(let loop ((exps exp-bodies))
			  (if (null? exps)
			    (if (compile-allow-cond-fallthrough)
			      '(#%void)
			      `(#%raise
				 (#%make-exn:else
				  ,(if (and had-no-clauses? (not allow-empty?))
				       "cond must contain at least one clause"
				       "no matching cond clause")
				  (#%current-continuation-marks))))
			    (let ((first (car exps))
				   (rest (cdr exps)))
			      (cond
				((cond-clause-=>? first)
				  `(let ((test ,(cond-clause-question first)))
				     (if test
				       (,(cond-clause-answer first) test)
				       ,(loop rest))))
				((cond-clause-else? first)
				  (if (null? rest)
				    (cond-clause-answer first)
				    (static-error
				      "cond" 'term:cond-else-only-in-last
				      (cond-clause-text first)
				      "else allowed only in last position")))
				((cond-clause-or? first)
				  `(or ,(cond-clause-question first)
				     ,(loop rest)))
				(else
				  `(if ,(cond-clause-question first)
				     ,(cond-clause-answer first)
				     ,(loop rest)))))))
			expr '(-1)
			#f
			(make-origin 'micro expr))
		      env attributes vocab))))))
	  (else
	    (static-error
	      "cond" 'kwd:cond
	      expr "malformed expression"))))))

  (add-primitivized-micro-form 'cond beginner-vocabulary (make-cond-micro nobegin-cond-clause-vocab #f))
  (add-primitivized-micro-form 'cond scheme-vocabulary (make-cond-micro full-cond-clause-vocab #t))

  (define case-macro
      (let* ((kwd-1 '(else))
	     (in-pattern-1 `(_ val (else ,@(get-expr-pattern #t))))
	     (out-pattern-1 `(begin val ,@(get-expr-pattern #t)))
	     (kwd-2 '())
	     (in-pattern-2 '(_ val))
	     (out-pattern-2-signal-error
	      `(#%raise (#%make-exn:else
			 "no matching else clause"
			 (#%current-continuation-marks))))
	     (out-pattern-2-no-error
	      '(begin val (#%void)))
	     (in-pattern-3 `(_ val ((keys ...) ,@(get-expr-pattern #t)) rest ...))
	     (out-pattern-3 `(let ((tmp val))
			       (if (#%memv tmp (quote (keys ...)))
				   (begin ,@(get-expr-pattern #t))
				   (case tmp rest ...))))
	     (m&e-1 (pat:make-match&env in-pattern-1 kwd-1))
	     (m&e-2 (pat:make-match&env in-pattern-2 kwd-2))
	     (m&e-3 (pat:make-match&env in-pattern-3 kwd-2)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd-1 env)
	    (if (compile-allow-cond-fallthrough)
	      (pat:match-and-rewrite expr m&e-2
		out-pattern-2-no-error kwd-2 env)
	      (pat:match-and-rewrite expr m&e-2
		out-pattern-2-signal-error kwd-2 env))
	    (pat:match-and-rewrite expr m&e-3 out-pattern-3 kwd-2 env)
	    (static-error
	      "case" 'kwd:case
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'case advanced-vocabulary case-macro)
  (add-primitivized-macro-form 'case scheme-vocabulary case-macro)

  (define evcase-macro
      (let* ((kwd-1 '(else))
	     (in-pattern-1 `(_ val (else ,@(get-expr-pattern #t))))
	     (out-pattern-1 `(begin val ,@(get-expr-pattern #t)))
	     (kwd-2 '())
	     (in-pattern-2 '(_ val))
	     (out-pattern-2-signal-error
	      `(#%raise (#%make-exn:else
			 "no matching else clause"
			 (#%current-continuation-marks))))
	     (out-pattern-2-no-error
	      '(begin val (#%void)))
	     (kwd-3 '(else))
	     (in-pattern-3 `(_ val (else ,@(get-expr-pattern #t)) rest))
	     (kwd-4 '())
	     (in-pattern-4 `(_ val (test-expr ,@(get-expr-pattern #t)) rest ...))
	     (out-pattern-4 `(let ((tmp val))
			       (if (#%eqv? tmp test-expr)
				   (begin ,@(get-expr-pattern #t))
				   (evcase tmp rest ...))))
	     (m&e-1 (pat:make-match&env in-pattern-1 kwd-1))
	     (m&e-2 (pat:make-match&env in-pattern-2 kwd-2))
	     (m&e-3 (pat:make-match&env in-pattern-3 kwd-3))
	     (m&e-4 (pat:make-match&env in-pattern-4 kwd-4)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd-1 env)
	      (if (compile-allow-cond-fallthrough)
		  (pat:match-and-rewrite expr m&e-2
					 out-pattern-2-no-error kwd-2 env)
		  (pat:match-and-rewrite expr m&e-2
					 out-pattern-2-signal-error kwd-2 env))
	      (let ((penv (pat:match-against m&e-3 expr env)))
		(if penv
		    (static-error
		      "evcase" 'kwd:evcase
		      expr "else used before last branch")
		    (or (pat:match-and-rewrite expr m&e-4 out-pattern-4 kwd-4 env)
			(static-error
			  "evcase" 'kwd:evcase
			  expr "malformed expression"))))))))

  (add-primitivized-macro-form 'evcase advanced-vocabulary evcase-macro)
  (add-on-demand-form 'macro 'evcase common-vocabulary evcase-macro)

  (define when-macro
      (let* ((kwd '())
	      (in-pattern `(_ test ,@(get-expr-pattern #t)))
	      (out-pattern `(if test (begin ,@(get-expr-pattern #t)) (#%void)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error
	      "when" 'kwd:when
	      expr "malformed expression")))))


  (add-primitivized-macro-form 'when advanced-vocabulary when-macro)
  (add-primitivized-macro-form 'when scheme-vocabulary when-macro)

  (define unless-macro
      (let* ((kwd '())
	      (in-pattern `(_ test ,@(get-expr-pattern #t)))
	      (out-pattern `(if test (#%void) (begin ,@(get-expr-pattern #t))))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error
	      "unless" 'kwd:unless
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'unless advanced-vocabulary unless-macro)
  (add-primitivized-macro-form 'unless scheme-vocabulary unless-macro)

  (let ((rewriter
	 (lambda (call/cc the-kwd kwd-text kwd:the-kwd)
	   (let* ((kwd '())
		  (in-pattern `(_ var ,@(get-expr-pattern #t)))
		  (out-pattern `(,call/cc (lambda (var) ,@(get-expr-pattern #t))))
		  (m&e (pat:make-match&env in-pattern kwd)))
	     (lambda (expr env)
	       (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
		   (static-error
		     kwd-text kwd:the-kwd
		     expr "malformed expression")))))))
    (add-primitivized-macro-form 'let/cc advanced-vocabulary
      (rewriter '#%call/cc 'let/cc "let/cc" 'kwd:let/cc))
    (add-primitivized-macro-form 'let/cc scheme-vocabulary
      (rewriter '#%call/cc 'let/cc "let/cc" 'kwd:let/cc))

    (add-primitivized-macro-form 'let/ec advanced-vocabulary
      (rewriter '#%call/ec 'let/ec "let/ec" 'kwd:let/ec))
    (add-primitivized-macro-form 'let/ec scheme-vocabulary
      (rewriter '#%call/ec 'let/ec "let/ec" 'kwd:let/ec)))
  
  (define do-macro
      (let* ((in-kwd '())
	      (in-pattern `(_ (var-init-step ...)
			     (test seq ...)
			     ,@(get-expr-pattern 'optional)))
	      (out-pattern `(letrec ((loop
				       (lambda (var ...)
					 (if test
					   (begin (#%void) seq ...)
					   (begin ,@(get-expr-pattern 'optional)
					     (loop step ...))))))
			      (loop init ...)))
	      (in-m&e (pat:make-match&env in-pattern in-kwd))
	      (vis-kwd '())
	      (vis-pattern-1 '(var init step))
	      (vis-m&e-1 (pat:make-match&env vis-pattern-1 vis-kwd))
	      (vis-pattern-2 '(var init))
	      (vis-m&e-2 (pat:make-match&env vis-pattern-2 vis-kwd)))
	(lambda (expr env)
	  (cond
	    ((pat:match-against in-m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((var-init-steps (pat:pexpand '(var-init-step ...)
					p-env in-kwd))
		       (test (pat:pexpand 'test p-env in-kwd))
		       (seqs (pat:pexpand '(seq ...) p-env in-kwd))
		       (body (pat:pexpand `(,@(get-expr-pattern 'optional))
			       p-env in-kwd)))
		  (let
		    ((normalized-var-init-steps
		       (map
			 (lambda (vis)
			   (cond
			     ((pat:match-against vis-m&e-1 vis vis-kwd)
			       =>
			       (lambda (p-env)
				 `(,(pat:pexpand 'var p-env vis-kwd)
				    ,(pat:pexpand 'init p-env vis-kwd)
				    ,(pat:pexpand 'step p-env vis-kwd))))
			     ((pat:match-against vis-m&e-2 vis vis-kwd)
			       =>
			       (lambda (p-env)
				 `(,(pat:pexpand 'var p-env vis-kwd)
				    ,(pat:pexpand 'init p-env vis-kwd)
				    ,(pat:pexpand 'var p-env vis-kwd))))
			     (else
			       (static-error
				 "do" 'kwd:do
				 vis
				 "malformed var-init-step"))))
			 var-init-steps)))
		    (let ((vars (map car normalized-var-init-steps))
			   (inits (map cadr normalized-var-init-steps))
			   (steps (map caddr normalized-var-init-steps)))
		      (structurize-syntax
			`(letrec ((loop
				    (lambda (,@vars)
				      (if ,test
					(begin (#%void) ,@seqs)
					(begin ,@body
					  (loop ,@steps))))))
			   (loop ,@inits))
			expr '(-1)
			#f
			(make-origin 'macro expr)))))))
	    (else
	      (static-error
		"do" 'kwd:do
		expr "malformed expression"))))))

  (add-primitivized-macro-form 'do advanced-vocabulary do-macro)
  (add-primitivized-macro-form 'do scheme-vocabulary do-macro)

  (define fluid-let-macro
      (let* ((kwd '())
	     (in-pattern `(_ ((var val) ...) ,@(get-expr-pattern #t)))
	     (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((vars (pat:pexpand '(var ...) p-env kwd))
		      (vals (pat:pexpand '(val ...) p-env kwd))
		      (body (pat:pexpand (get-expr-pattern #t) p-env kwd)))
		  (distinct-valid-syntactic-id/s? vars)
		  (let* ((new-vars (map generate-name vars)))
		    (expand-expr
		      (structurize-syntax
			(if (null? vars)
			  `(let-values () ,@body)
			  `(let ,(map list new-vars vars)
			     (#%dynamic-wind
			       (lambda ()
				 ,@(map (lambda (var val)
					  `(set! ,var ,val))
				     vars vals))
			       (lambda ()
				 ,@body)
			       (lambda ()
				 ,@(map (lambda (var tmp)
					  `(set! ,(dup-symbol var) ,tmp))
				     vars new-vars)))))
			expr '(-1)
			#f
			(make-origin 'macro expr))
		      env attributes vocab)))))
	    (else
	      (static-error
		"fluid-let" 'kwd:fluid-let
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'fluid-let advanced-vocabulary fluid-let-macro)
  (add-primitivized-micro-form 'fluid-let scheme-vocabulary fluid-let-macro)

  (define parameterize-micro
    (let* ((kwd '())
	   (body (get-expr-pattern #t))
	   (in-pattern `(_ ((param value) ...) ,@body))
	   (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(let ([p-env (pat:match-against m&e expr env)])
	  (if p-env
	      (let* ((params (pat:pexpand '(param ...) p-env kwd))
		     (vals (pat:pexpand '(value ...) p-env kwd))
		     (body (pat:pexpand body p-env kwd))
		     (pzs (map generate-name params))
		     (saves (map generate-name params))
		     (swap (generate-name (structurize-syntax 'swap expr '(-1)))))
		(expand-expr
		 (structurize-syntax
		  (if (null? params)
		      `(let-values () ,@body)
		      `(let ,(append
			      (map list pzs params)
			      (map list saves vals))
			 (let ((,swap (lambda ()
					,@(map 
					   (lambda (save pz)
					     `(let ([x ,save])
						(begin
						  (set! ,save (,pz))
						  (,pz x))))
					   saves pzs))))
			   (#%dynamic-wind
			    ,swap
			    (#%lambda () ,@body)
			    ,swap))))
		  expr '(-1)
		  #f
		  (make-origin 'micro expr))
		 env attributes vocab))
	      (static-error
		"parameterize" 'kwd:parameterize
		expr "malformed expression"))))))

  (add-primitivized-micro-form 'parameterize advanced-vocabulary parameterize-micro)
  (add-primitivized-micro-form 'parameterize scheme-vocabulary parameterize-micro)

  (define (make-with-handlers-macro begin?)
      (let* ((kwd '())
	     (in-pattern-1 `(_ () ,@(get-expr-pattern begin?)))
	     (out-pattern-1 (if (not begin?)
				'expr
				`(let-values () ,@(get-expr-pattern begin?))))
	     (in-pattern-2 `(_ ((pred handler) ...) ,@(get-expr-pattern begin?)))
	     (out-pattern-2
	      `((#%call/ec
		 (lambda (k)
		   (let ((handlers (#%list
				    (#%cons pred handler)
				    ...)))
		     (parameterize
			 ((#%current-exception-handler
			   (lambda (e)
			     (k
			      (lambda ()
				(let loop ((handlers handlers))
				  (cond
				   ((#%null? handlers)
				    (#%raise e))
				   (((#%caar handlers) e)
				    ((#%cdar handlers) e))
				   (else
				    (loop (#%cdr handlers))))))))))
		       (#%call-with-values
			(lambda () ,@(get-expr-pattern begin?))
			(lambda args
			  (lambda () (#%apply #%values args))))))))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error
	      "with-handlers" 'kwd:with-handlers
	      expr "malformed expression")))))

  (add-primitivized-macro-form 'with-handlers
			       advanced-vocabulary
			       (make-with-handlers-macro #f))
  (add-primitivized-macro-form 'with-handlers 
			       scheme-vocabulary
			       (make-with-handlers-macro #t))

  (define (norm-path p) ; normalizes ending slash or not
    (and p
	 (let-values ([(base name dir?) (split-path p)])
	   (build-path base name))))
    (define mzlib-directory (with-handlers ([void void])
			    (norm-path (collection-path "mzlib"))))
  (define (get-on-demand-form name vocab)
    (let ([dir (norm-path (current-load-relative-directory))])
      (and (equal? dir mzlib-directory)
	   (find-on-demand-form name vocab))))

  (add-primitivized-micro-form 'define-macro common-vocabulary
    (let* ((kwd '())
	    (in-pattern `(_ macro-name macro-handler))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((macro-name (pat:pexpand 'macro-name p-env kwd))
		     (macro-handler (pat:pexpand 'macro-handler p-env kwd)))
		(valid-syntactic-id? macro-name)
		(unless (get-top-level-status attributes)
		  (static-error
		    "define-macro" 'kwd:define-macro
		    expr "only supported at top-level"))
		(let* ((real-name (sexp->raw macro-name)))
		  (let ([on-demand (get-on-demand-form real-name vocab)])
		    (if on-demand
			(case (car on-demand)
			  [(micro) (add-primitivized-micro-form real-name vocab (cadr on-demand))]
			  [(macro) (add-primitivized-macro-form real-name vocab (cadr on-demand))])
			(let* ((expanded-handler (as-nested
						  attributes
						  (lambda ()
						    (expand-expr macro-handler
								 env attributes vocab))))
			       (real-handler (m3-elaboration-evaluator
					      expanded-handler
					      parsed->raw
					      'define-macro))
			       (cache-table (make-hash-table)))
			  (unless (procedure? real-handler)
			    (static-error
			      "define-macro" 'kwd:define-macro
			      expr "expander is not a procedure"))
			  (add-user-macro-form 
			   real-name vocab
			   (lambda (m-expr m-env)
			     (structurize-syntax
			      (apply m3-macro-body-evaluator real-handler
				     (cdr (sexp->raw m-expr cache-table)))
			      m-expr '() cache-table
			      (make-origin 'macro expr)))))))
		  (expand-expr (structurize-syntax '(#%void) expr
						   '() #f (make-origin 'micro expr))
		    env attributes vocab)))))
	  (else
	    (static-error
	      "define-macro" 'kwd:define-macro
	      expr "malformed definition"))))))

  (add-primitivized-micro-form 'let-macro common-vocabulary
    (let* ((kwd '())
	    (in-pattern `(_ macro-name macro-handler b0 b1 ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((macro-name (pat:pexpand 'macro-name p-env kwd))
		     (macro-handler (pat:pexpand 'macro-handler p-env kwd))
		     (body (pat:pexpand '(begin b0 b1 ...) p-env kwd)))
		(valid-syntactic-id? macro-name)
		(let* ((real-name (sexp->raw macro-name))
		       (expanded-handler (as-nested
					  attributes
					  (lambda ()
					    (expand-expr macro-handler
							 env attributes vocab))))
		       (real-handler (m3-elaboration-evaluator
					expanded-handler
					parsed->raw
					'let-macro))
			(cache-table (make-hash-table)))
		  (unless (procedure? real-handler)
		    (static-error
		      "let-macro" 'kwd:let-macro
		      expr "expander is not a procedure"))
		  (let ((extended-vocab
			  (create-vocabulary 'user-macro-extended-vocab
			    vocab)))
		    (add-user-macro-form real-name extended-vocab
		      (lambda (m-expr m-env)
			(structurize-syntax
			  (apply m3-macro-body-evaluator real-handler
			    (cdr (sexp->raw m-expr cache-table)))
			  m-expr '() cache-table
			  (make-origin 'macro expr))))
		    (expand-expr
		      (structurize-syntax body expr
					  '() 
					  #f (make-origin 'micro expr))
		      env attributes extended-vocab))))))
	  (else
	    (static-error
	      "let-macro" 'kwd:let-macro
	      expr "malformed expression"))))))

  (let ((b-e/c-t
	  (lambda (kwd-symbol kwd:kwd-symbol kwd-string phase-string on-demand?)
	    (let ([micro (let* ((kwd '())
				(in-pattern '(_ e0 e1 ...))
				(m&e (pat:make-match&env in-pattern kwd)))
			   (lambda (expr env attributes vocab)
			     (cond
			      ((pat:match-against m&e expr env)
			       =>
			       (lambda (p-env)
				 (let ((exprs (pat:pexpand '(begin e0 e1 ...)
							   p-env kwd)))
				   (expand-expr
				    (structurize-syntax
				     (with-handlers
					 ((exn? (lambda (exn)
						  (static-error
						    kwd-string
						    kwd:kwd-symbol
						    expr
						    "exception at ~a time: ~a"
						    phase-string
						    (exn-message exn)))))
				       (m3-elaboration-evaluator
					(let ([top-level? (get-top-level-status attributes)]
					      [internal? (get-internal-define-status attributes)])
					  (dynamic-wind
					   (lambda ()  
					     (set-top-level-status attributes #t) 
					     (set-internal-define-status attributes #f))
					   (lambda ()
					     (expand
					      (structurize-syntax exprs expr)
					      attributes vocab
					      m3-elaboration-evaluator
					      m3-macro-body-evaluator))
					   (lambda () 
					     (set-top-level-status attributes top-level?)
					     (set-internal-define-status attributes internal?))))
					parsed->raw
					kwd-symbol))
				     expr
				     '() #f (make-origin 'micro expr))
				    env attributes vocab))))
			      (else
			       (static-error
				 kwd-string kwd:kwd-symbol
				 expr
				 "malformed expression")))))])
	      (add-micro-form kwd-symbol full-vocabulary micro)
	      (if on-demand?
		  (add-on-demand-form 'micro kwd-symbol scheme-vocabulary micro)
		  (add-micro-form kwd-symbol scheme-vocabulary micro))))))
    (b-e/c-t 'begin-construction-time 'kwd:begin-construction-time
      "begin-construction-time" "construction" #t)
    (b-e/c-t 'begin-elaboration-time 'kwd:begin-elaboration-time
      "begin-elaboration-time" "elaboration" #f))

  (define unquote-micro
    (lambda (expr env)
      (static-error
	"unquote" 'kwd:unquote
	expr "outside quasiquote")))
  (add-primitivized-macro-form 'unquote intermediate-vocabulary unquote-micro)
  (add-primitivized-macro-form 'unquote scheme-vocabulary unquote-micro)

  (define unquote-splicing-micro
    (lambda (expr env)
      (static-error
	"unquote-splicing" 'kwd:unquote-splicing
	expr "outside quasiquote")))
  (add-primitivized-macro-form 'unquote-splicing intermediate-vocabulary unquote-splicing-micro)
  (add-primitivized-macro-form 'unquote-splicing scheme-vocabulary unquote-splicing-micro)

  (include "quasi.ss")

  (define reference-file-macro
    (let* ((kwd '())
	    (in-pattern '(_ filename))
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
			`(#%load/use-compiled ,(quote-form-expr f))
			expr '(-1)
			#f
			(make-origin 'macro expr))
		      env attributes vocab)
		    (static-error
		      "reference-file" 'kwd:reference-file
		      filename "Does not yield a filename"))))))
	  (else
	    (static-error
	      "reference-file" 'kwd:reference-file
	      expr "Malformed reference-file"))))))

  (add-primitivized-micro-form 'reference-file beginner-vocabulary reference-file-macro)
  (add-on-demand-form 'micro 'reference-file common-vocabulary reference-file-macro)

  (define require-library-micro
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
		(let ((f (as-nested attributes (lambda () (expand-expr filename env attributes vocab))))
		      (cs (as-nested
			   attributes
			   (lambda ()
			     (map (lambda (c)
				    (expand-expr c env attributes vocab))
				  collections)))))
		  (unless (and (quote-form? f)
			    (z:string? (quote-form-expr f)))
		    (static-error
		      "require-library" 'kwd:require-library
		      filename "Does not yield a filename"))
		  (for-each
		    (lambda (c collection)
		      (unless (and (quote-form? c)
				(z:string? (quote-form-expr c)))
			(static-error
			  "require-library" 'kwd:require-library
			  collection "Does not yield a string")))
		    cs collections)
		  (let ((raw-f (z:read-object (quote-form-expr f)))
			 (raw-cs (map (lambda (c)
					(z:read-object (quote-form-expr c)))
				   cs)))
		    (unless (relative-path? raw-f)
		      (static-error
			"require-library" 'kwd:require-library
			f
			"Library path ~s must be a relative path"
			raw-f))
		    (expand-expr
		      (structurize-syntax
		       `(#%require-library/proc ,(quote-form-expr f)
			     ,@(map quote-form-expr cs))
		       expr '(-1)
		       #f
		       (make-origin 'micro expr))
		      env attributes vocab))))))
	  (else
	    (static-error
	      "require-library" 'kwd-require-library
	      expr "Malformed require-library"))))))

  (add-primitivized-micro-form 'require-library beginner-vocabulary require-library-micro)
  (add-primitivized-micro-form 'require-library scheme-vocabulary require-library-micro)

  (define require-relative-library-micro
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
		(let ((f (as-nested attributes (lambda () (expand-expr filename env attributes vocab))))
		      (cs (as-nested
			   attributes
			   (lambda ()
			     (map (lambda (c)
				    (expand-expr c env attributes vocab))
				  collections)))))
		  (unless (and (quote-form? f)
			    (z:string? (quote-form-expr f)))
		    (static-error
		      "require-relative-library"
		      'kwd:require-relative-library
		      filename "Does not yield a filename"))
		  (for-each
		    (lambda (c collection)
		      (unless (and (quote-form? c)
				(z:string? (quote-form-expr c)))
			(static-error
			  "require-relative-library"
			  'kwd:require-relative-library
			  collection "Does not yield a string")))
		    cs collections)
		  (let ((raw-f (z:read-object (quote-form-expr f)))
			 (raw-cs (map (lambda (c)
					(z:read-object (quote-form-expr c)))
				   cs)))
		    (unless (relative-path? raw-f)
		      (static-error 
			"require-relative-library"
			'kwd:require-relative-library
			f
			"library path ~s must be a relative path"
			raw-f))
		    (expand-expr
		      (structurize-syntax
		       `(#%require-relative-library/proc ,(quote-form-expr f)
			     ,@(map quote-form-expr cs))
		       expr '(-1)
		       #f
		       (make-origin 'micro expr))
		      env attributes vocab))))))
	  (else
	    (static-error
	      "require-relative-library" 'kwd:require-relative-library
	      expr "malformed expression"))))))

  (add-primitivized-micro-form 'require-relative-library beginner-vocabulary require-relative-library-micro)
  (add-primitivized-micro-form 'require-relative-library scheme-vocabulary require-relative-library-micro)

  (add-on-demand-form 'micro 'define-constructor beginner-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ sym modes ...))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern '(#%void)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error
	    "define-constructor" 'kwd:define-constructor
	    expr "malformed definition")))))

  (add-on-demand-form 'macro 'define-type beginner-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ sym type))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern '(#%void)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error
	    "define-type" 'kwd:define-type
	    expr "malformed definition")))))

  (add-on-demand-form 'macro ': beginner-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ expr type))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern 'expr))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error
	    ":" 'kwd::
	    expr "malformed declaration")))))

  (add-on-demand-form 'macro 'type: beginner-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ type attr ...))
	    (out-pattern '(#%void))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error
	    "type:" 'kwd:type:
	    expr "malformed declaration")))))

  (add-on-demand-form 'macro 'mrspidey:control beginner-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ para val))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern '(#%void)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error
	    "mrspidey:control" 'kwd:mrspidey:control
	    expr "malformed declaration")))))

  (add-on-demand-form 'macro 'polymorphic beginner-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ body))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern 'body))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error
	    "polymorphic" 'kwd:polymorpic
	    expr "malformed declaration")))))

  )
