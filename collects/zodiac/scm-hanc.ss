; $Id: scm-hanc.ss,v 1.65 2000/05/28 03:47:31 shriram Exp $

(define-struct signature-element (source))
(define-struct (name-element struct:signature-element) (name))
(define-struct (unit-element struct:signature-element) (id signature))

(define immediate-signature-name '|<immediate signature>|)

(define cu/s-this-link-attr 'cu/s-this-link-name)

(define-struct signature (name elements exploded))

; Cheap trick: instead of fixing the vocabs to ignore the
;   environment (if possible) , we just drop the environment.
(define (sig-env e)
  (make-empty-environment))

(define check-unique-cu/s-exports
  (lambda (in:exports sign:exports)
    (let loop ((in:all in:exports)
		(sign:all sign:exports)
		(in:names null)
		(sign:names null)
		(in:rest null)
		(sign:rest null))
      (if (or (null? sign:all) (null? in:all))
	(begin
	  (let loop ((in in:rest)
		      (signs (map car sign:rest)))
	    (unless (null? in)
	      (if (memq (car signs) (cdr signs))
		(static-error
		  "unit" 'term:unit-double-export (car in)
		  "name \"~s\" is exported twice" (car signs))
		(loop (cdr in) (cdr signs)))))
	  (let loop ((in in:names)
		      (signs sign:names))
	    (unless (null? in)
	      (if (memq (car signs) (cdr signs))
		(static-error
		  "unit" 'term:unit-double-export (car in)
		  "name \"~s\" is exported twice" (car signs))
		(loop (cdr in) (cdr signs))))))
	(let ((in (car in:all)) (sign (car sign:all)))
	  (if (or (symbol? sign) (z:symbol? sign))
	    (loop (cdr in:all) (cdr sign:all)
	      (cons in in:names)
	      (cons (if (symbol? sign)
		      sign
		      (z:read-object sign))
		sign:names)
	      in:rest sign:rest)
	    (loop (cdr in:all) (cdr sign:all)
	      in:names sign:names
	      (cons in in:rest)
	      (cons sign sign:rest))))))))

; This is based on code lifted from Matthew's implementation (note the
; use of brackets (-:).

(define verify-duplicates-&-sort-signature-elements
  (lambda (elements)
    (let loop ((seen '()) (rest elements))
      (unless (null? rest)
	(let ((first (car rest)))
	  (let ((first-name
		  (cond
		    ((name-element? first)
		      (name-element-name first))
		    ((unit-element? first)
		      (unit-element-id first))
		    (else
		      (internal-error first "Invalid unit element")))))
	    (when (memq first-name seen)
	      (static-error
		"signature" 'term:duplicate-signature
		(signature-element-source first)
		"duplicate entry: ~s" first-name))
	    (loop (cons first-name seen) (cdr rest))))))
    (letrec
      ((split
	 (lambda (l f s)
	   (cond
	     [(null? l) (values f s)]
	     [(null? (cdr l)) (values (cons (car l) f) s)]
	     [else (split (cddr l) (cons (car l) f)
		     (cons (cadr l) s))])))
	(merge
	  (lambda (f s)
	    (cond
	      [(null? f) s]
	      [(null? s) f]
	      [(less-than? (car s) (car f))
		(cons (car s) (merge f (cdr s)))]
	      [else
		(cons (car f) (merge (cdr f) s))])))
	(less-than?
	  (lambda (a b)
	    (if (name-element? a)
	      (if (name-element? b)
		(symbol-less-than? (name-element-name a)
		  (name-element-name b))
		#t)
	      (if (name-element? b)
		#f
		(symbol-less-than? (unit-element-id a)
		  (unit-element-id b))))))
	(symbol-less-than?
	  (lambda (a b)
	    (string<? (symbol->string a) (symbol->string b)))))
      (let loop ([elements elements])
	(cond
	  [(null? elements) null]
	  [(null? (cdr elements)) elements]
	  [else (let-values ([(f s) (split elements null null)])
		  (merge (loop f) (loop s)))])))))

(define explode-signature-elements
  (lambda (elements)
    (map (lambda (elt)
	   (cond
	     ((name-element? elt)
	       (name-element-name elt))
	     ((unit-element? elt)
	       (cons (unit-element-id elt)
		 (signature-exploded (unit-element-signature elt))))
	     (else
	       (internal-error elt "Invalid signature element"))))
      elements)))

(define sig-list->sig-vector
  (lambda (l)
    (list->vector
     (map
      (lambda (e)
	(if (or (z:symbol? e) (symbol? e))
	    e
	    (named-sig-list->named-sig-vector e)))
      l))))

(define named-sig-list->named-sig-vector
  (lambda (l)
    (cons (car l)
	  (sig-list->sig-vector (cdr l)))))

(define create-signature
  (opt-lambda (elements (name immediate-signature-name))
    (let ((sorted-elements
	    (verify-duplicates-&-sort-signature-elements elements)))
      (make-signature name sorted-elements
	(explode-signature-elements sorted-elements)))))

(define add-signature
  (lambda (name attributes elements)
    (let ((sig-space (get-attribute attributes 'sig-space
		       (lambda ()
			 (let ((ss (make-hash-table)))
			   (put-attribute attributes 'sig-space ss)
			   ss)))))
      (hash-table-put! sig-space (z:read-object name)
	(create-signature elements (z:read-object name))))))

(define push-signature
  (lambda (name attributes elements)
    (let ((sig-space (get-attribute attributes 'sig-space
		       (lambda ()
			 (let ((ss (make-hash-table)))
			   (put-attribute attributes 'sig-space ss)
			   ss)))))
      (begin0
	(hash-table-get sig-space (z:read-object name)
	  (lambda () #f))
	(hash-table-put! sig-space (z:read-object name)
	  (create-signature elements (z:read-object name)))))))

(define pop-signature
  (lambda (name attributes old-value)
    (let ((sig-space (get-attribute attributes 'sig-space
		       (lambda ()
			 (let ((ss (make-hash-table)))
			   (put-attribute attributes 'sig-space ss)
			   ss)))))
      (hash-table-remove! sig-space (z:read-object name))
      (when old-value
	(hash-table-put! sig-space (z:read-object name)
	  old-value)))))

(define lookup-signature
  (lambda (name attributes)
    (let ((sig-space (get-attribute attributes 'sig-space)))
      (if sig-space
	(let ((entry
		(hash-table-get sig-space (z:read-object name)
		  (lambda ()
		    (static-error
		      "signature" 'term:unbound-sig-name name
		      "unbound name: ~s" (z:read-object name))))))
	  entry)
	(static-error
	  "signature" 'term:unbound-sig-name name
	  "unbound name: ~s" (z:read-object name))))))

(define extract-sub-unit-signature
  (lambda (signature indices)
    (if (null? indices)
      signature
      (let* ((first (car indices))
	      (raw-first (z:read-object first)))
	(let loop ((elements (signature-elements signature)))
	  (if (null? elements)
	    (static-error
	      "signature" 'term:signature-no-sub-unit first
	      "no such sub-unit")
	    (if (unit-element? (car elements))
	      (if (eq? raw-first (unit-element-id (car elements)))
		(extract-sub-unit-signature
		  (unit-element-signature (car elements))
		  (cdr indices))
		(loop (cdr elements)))
	      (loop (cdr elements)))))))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(define cu/s-attr 'compound-unit/sig-table)

(define-struct tag-table-entry (signature))
(define-struct (tag-table-import-entry struct:tag-table-entry) ())
(define-struct (tag-table-link-entry struct:tag-table-entry) ())

(define extract-cu/s-tag-table
  (lambda (attributes)
    (car 
      (get-attribute attributes cu/s-attr
	(lambda ()
	  (internal-error attributes
	    "Unable to find compound-unit/sig attribute"))))))

(define cu/s-tag-table-put
  (lambda (maker)
    (lambda (table tag sig env attributes)
      (hash-table-put! table (z:read-object tag)
	(maker (expand-expr sig env attributes sig-vocab))))))

(define cu/s-tag-table-put/import
  (cu/s-tag-table-put make-tag-table-import-entry))

(define cu/s-tag-table-put/link
  (cu/s-tag-table-put make-tag-table-link-entry))

(define cu/s-tag-table-lookup
  (opt-lambda (table tag (not-found (lambda () #f)))
    (hash-table-get table (z:read-object tag) not-found)))

(define cu/s-tag-table-lookup/static-error
  (lambda (table tag)
    (cu/s-tag-table-lookup table tag
      (lambda ()
	(static-error
	  "unit linkage" 'term:unit-link-unbound-tag tag
	  "unbound tag")))))

(define cu/s-tag-table-lookup/internal-error
  (lambda (table tag)
    (cu/s-tag-table-lookup table tag
      (lambda ()
	(internal-error tag "Should have been bound")))))

; --------------------------------------------------------------------

(define sig-vocab
  (create-vocabulary 'sig-vocab #f
    "malformed signature expression"
    "malformed signature expression"
    "malformed signature expression"
    "malformed signature expression"))

(add-sym-micro sig-vocab
  (lambda (expr env attributes vocab)
    (lookup-signature expr attributes)))

(add-list-micro sig-vocab
  (lambda (expr env attributes vocab)
    (let ((contents (expose-list expr)))
      (create-signature
	(apply append
	  (map (lambda (e)
		 (expand-expr e env attributes sig-element-vocab))
	    contents))))))

; --------------------------------------------------------------------

(define sig-element-vocab
  (create-vocabulary 'sig-element-vocab #f
    "malformed signature element"
    "malformed signature element"
    "malformed signature element"
    "malformed signature element"))

(add-sym-micro sig-element-vocab
  (lambda (expr env attributes vocab)
    (list (make-name-element expr (z:read-object expr)))))

(add-micro-form 'struct sig-element-vocab
  (let* ((kwd '(struct))
	  (in-pattern '(struct base (field ...) omit ...))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((base (pat:pexpand 'base p-env kwd))
		   (fields (pat:pexpand '(field ...) p-env kwd))
		   (in:omits (pat:pexpand '(omit ...) p-env kwd)))
	      (valid-syntactic-id? base)
	      (valid-syntactic-id/s? fields)
	      (let ((omit-names
		      (map (lambda (o)
			     (expand-expr o env attributes
			       signature-struct-omission-checker-vocab))
			in:omits)))
		(let ((generated-names
			(map z:read-object
			  (generate-struct-names base fields expr
			    (memq '-selectors omit-names)
			    (memq '-setters omit-names)))))
		  (let loop ((omits omit-names))
		    (unless (null? omits)
		      (let ((first (car omits)))
			(when (z:symbol? first)
			  (unless (memq (z:read-object first) generated-names)
			    (static-error
			      "structs in signature"
			      'term:signature-struct-illegal-omit-name
			      first
			      "name not generated; illegal to omit")))
			(loop (cdr omits)))))
		  (let ((real-omits
			  (let loop ((omits omit-names))
			    (if (null? omits) '()
			      (if (symbol? (car omits))
				(loop (cdr omits))
				(cons (z:read-object (car omits))
				  (loop (cdr omits))))))))
		    (let loop ((names generated-names))
		      (if (null? names) '()
			(if (memq (car names) real-omits)
			  (loop (cdr names))
			  (cons (make-name-element expr (car names))
			    (loop (cdr names))))))))))))
	(else
	  (static-error
	    "struct" 'kwd:signature-struct expr
	    "malformed clause"))))))

(define signature-struct-omission-checker-vocab
  (create-vocabulary 'signature-struct-omission-checker-vocab #f
    "malformed signature structure omission declaration"
    "malformed signature structure omission declaration"
    "malformed signature structure omission declaration"
    "malformed signature structure omission declaration"))

(add-sym-micro signature-struct-omission-checker-vocab
  (lambda (expr env attributes vocab)
    (let ((raw-expr (z:read-object expr)))
      (unless (memq raw-expr '(-selectors -setters))
	(static-error
	  "structs in signature" 'term:signature-invalid-struct-omit
	  expr "invalid omission specifier"))
      raw-expr)))

(add-micro-form '- signature-struct-omission-checker-vocab
  (let* ((kwd '(-))
	  (in-pattern '(- var))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((var (pat:pexpand 'var p-env kwd)))
	      (valid-syntactic-id? var)
	      (structurize-syntax (z:read-object var) expr))))
	(else
	  (static-error
	    "structs in signature" 'term:signature-malformed-omit-clause
	    expr "malformed omission specifier"))))))

(add-micro-form 'open sig-element-vocab
  (let* ((kwd '(open))
	  (in-pattern '(open sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? sig)
	      (signature-elements
		(expand-expr sig env attributes sig-vocab)))))
	(else
	  (static-error
	    "structs in signature" 'term:signature-malformed-open-clause
	    expr "malformed open clause"))))))

(add-micro-form 'unit sig-element-vocab
  (let* ((kwd '(unit :))
	  (in-pattern '(unit id : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (list (make-unit-element expr (z:read-object id)
		      (expand-expr sig env attributes sig-vocab))))))
	(else
	  (static-error
	    "structs in signature" 'term:signature-malformed-unit-clause
	    expr "Malformed unit clause"))))))

; --------------------------------------------------------------------

(define u/s-prim-imports-vocab
  (create-vocabulary 'u/s-prim-imports-vocab #f
    "malformed imports declaration"
    "malformed imports declaration"
    "malformed imports declaration"
    "malformed imports declaration"))

(add-sym-micro u/s-prim-imports-vocab
  (lambda (expr env attributes vocab)
    (convert-to-prim-format
      (signature-elements
	(lookup-signature expr attributes)))))

(add-list-micro u/s-prim-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (convert-to-prim-format
		(signature-elements
		  (expand-expr sig env attributes sig-vocab))
		(z:read-object id)))))
	((pat:match-against m&e-2 expr env)
	  (static-error
	    "signature" 'term:signature-ambiguous-:
	    expr "ambiguous : in signature"))
	(else
	  (convert-to-prim-format
	    (signature-elements
	      (expand-expr expr env attributes sig-vocab))))))))

(define convert-to-prim-format
  (opt-lambda (sig-elements (prefix #f))
    (convert-to-prim-format-helper sig-elements
      (cond
	((symbol? prefix)
	  (let ((s (symbol->string prefix)))
	    (if (string=? "" s)
	      s
	      (string-append s ":"))))
	((string? prefix)
	  prefix)
	(else
	  "")))))

(define convert-to-prim-format-helper
  (lambda (sig-elements prefix-string)
    (apply append
      (map (lambda (elt)
	     (cond
	       ((name-element? elt)
		 (list
		   (string->symbol
		     (string-append prefix-string
		       (symbol->string (name-element-name elt))))))
	       ((unit-element? elt)
		 (let ((new-prefix
			 (string-append prefix-string
			   (symbol->string (unit-element-id elt))
			   ":")))
		   (convert-to-prim-format-helper
		     (signature-elements
		       (unit-element-signature elt))
		     new-prefix)))
	       (else
		 (internal-error elt "Illegal signature element"))))
	sig-elements))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(define u/s-sign-imports-vocab
  (create-vocabulary 'u/s-sign-imports-vocab #f
    "malformed signature imports declaration"
    "malformed signature imports declaration"
    "malformed signature imports declaration"
    "malformed signature imports declaration"))

(add-sym-micro u/s-sign-imports-vocab
  (lambda (expr env attributes vocab)
    (cons (z:read-object expr)
      (signature-exploded
	(lookup-signature expr attributes)))))

(add-list-micro u/s-sign-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (cons (z:read-object id)
		(signature-exploded
		  (expand-expr sig env attributes sig-vocab))))))
	((pat:match-against m&e-2 expr env)
	  (static-error
	    "signature" 'term:signature-ambiguous-:
	    expr "ambiguous : in signature"))
	(else
	  (cons immediate-signature-name
	    (explode-signature-elements
	      (signature-elements
		(expand-expr expr env attributes sig-vocab)))))))))

; --------------------------------------------------------------------

(define create-prim-exports
  (lambda (export-sig renames source env attributes)
    (let ((sig-names (signature-elements
		       (expand-expr export-sig env attributes sig-vocab))))
      (let ((table (make-hash-table)))
	(for-each (lambda (z-rename)
		    (let ((rename-couple (expose-list z-rename)))
		      (hash-table-put! table
			(z:read-object (cadr rename-couple))
			(z:read-object (car rename-couple)))))
	  renames)
	(let loop ((sig-names sig-names))
	  (if (null? sig-names)
	    '()
	    (let ((first (car sig-names)))
	      (when (unit-element? first)
		(static-error
		  "unit" 'term:no-unit-exports source
		  "unit exports not allowed"))
	      (let ((name (name-element-name first)))
		(cons
		  (let ((entry (hash-table-get table name (lambda () #f))))
		    (if entry
		      (list entry name)
		      (list name name)))
		  (loop (cdr sig-names)))))))))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(define u/s-sign-exports-vocab
  (create-vocabulary 'u/s-sign-exports-vocab #f
    "malformed signature exports declaration"
    "malformed signature exports declaration"
    "malformed signature exports declaration"
    "malformed signature exports declaration"))

(add-sym-micro u/s-sign-exports-vocab
  (lambda (expr env attributes vocab)
    (signature-exploded
      (lookup-signature expr attributes))))

(add-list-micro u/s-sign-exports-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (signature-exploded
		(expand-expr sig env attributes sig-vocab)))))
	((pat:match-against m&e-2 expr env)
	  (static-error
	    "signature" 'term:signature-ambiguous-: expr
	    "ambiguous : in signature"))
	(else
	  (explode-signature-elements
	    (signature-elements
	      (expand-expr expr env attributes sig-vocab))))))))

; --------------------------------------------------------------------

(define signature->symbols-micro
    (let* ((kwd '())
	    (in-pattern '(_ name))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((name (pat:pexpand 'name p-env kwd)))
		(valid-syntactic-id? name)
		(let ((elements
			(sig-list->sig-vector
			  (signature-exploded
			    (lookup-signature name attributes)))))
		  (expand-expr
		    (structurize-syntax `(,'quote ,elements) expr '(-1))
		    env attributes vocab)))))
	  (else
	    (static-error
	      "signature->symbols" 'kwd:signature->symbols
	      expr "malformed expression"))))))

(add-primitivized-micro-form 'signature->symbols full-vocabulary signature->symbols-micro)
(add-on-demand-form 'micro 'signature->symbols common-vocabulary signature->symbols-micro)

(define define-signature-micro
    (let* ((kwd '())
	    (in-pattern '(_ name sig))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((name (pat:pexpand 'name p-env kwd))
		     (sig (pat:pexpand 'sig p-env kwd)))
		(valid-syntactic-id? name)
		(unless (get-top-level-status attributes)
		  (static-error
		    "define-signature" 'kwd:define-signature
		    expr "only supported at top-level"))
		(let ((elements
			(signature-elements
			  (expand-expr sig env attributes sig-vocab))))
		  (add-signature name attributes elements))
		(expand-expr
		  (structurize-syntax '(#%void) expr '(-1)
				      #f (z:make-origin 'micro expr))
		  env attributes vocab))))
	  (else
	    (static-error
	      "define-signature" 'kwd:define-signature
	      expr "malformed definition"))))))

(add-primitivized-micro-form 'define-signature full-vocabulary define-signature-micro)
(add-primitivized-micro-form 'define-signature scheme-vocabulary define-signature-micro)

(define let-signature-micro
    ;; >> Broken by current embedded define hacks! <<
    ;; e.g., (let ([a 7]) 5 (let-signature a () a))
    (let* ((kwd '())
	    (in-pattern '(_ name sig b0 b1 ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((name (pat:pexpand 'name p-env kwd))
		    (sig (pat:pexpand 'sig p-env kwd))
		    (body (pat:pexpand '(begin b0 b1 ...) p-env kwd)))
		(valid-syntactic-id? name)
		(let* ((elements
			 (signature-elements
			   (expand-expr sig env attributes sig-vocab)))
		       (old-value (push-signature name attributes elements)))
		  (dynamic-wind
		   void
		   (lambda ()
		     ; Yuck - if name is in the environment, we shadow it
		     ;  by retracting the env:
		     (let ([new-env
			    (let loop ([env env])
			      (if (lexically-resolved? name env)
				  (let ([env (copy-env env)]
					[var (let ((name (z:read-object name)) 
						   (marks (z:symbol-marks name)))
					       (resolve-in-env name marks env))])
				    (retract-env (list var) env)
				    (loop env))
				  env))])
		       (let ([r (expand-expr
				 (structurize-syntax body expr)
				 new-env attributes vocab)])
			 r)))
		   (lambda ()
		     (pop-signature name attributes old-value)))))))
	  (else
	   (static-error
	     "let-signature" 'kwd:let-signature
	     expr "malformed expression"))))))

(add-primitivized-micro-form 'let-signature full-vocabulary let-signature-micro)
(add-primitivized-micro-form 'let-signature scheme-vocabulary let-signature-micro)

(define u/s-expand-includes-vocab
  (create-vocabulary 'u/s-expand-includes-vocab))

(add-primitivized-micro-form 'include u/s-expand-includes-vocab
  (let* ((kwd '())
          (in-pattern '(_ filename))
          (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
        ((pat:match-against m&e expr env)
          =>
          (lambda (p-env)
            (let ((filename (pat:pexpand 'filename p-env kwd)))
              (unless (z:string? filename)
                (static-error
		  "include" 'kwd:unit-include
		  filename "file name must be a string"))
              (let ((raw-filename (z:read-object filename)))
                (let-values (((base name dir?) (split-path raw-filename)))
                  (when dir?
                    (static-error
		      "include" 'kwd:unit-include
		      filename "cannot include a directory"))
                  (let* ((original-directory (current-load-relative-directory))
			  (p (with-handlers
			       ((exn:i/o:filesystem?
				  (lambda (exn)
				    (static-error
				      "include" 'kwd:unit-include filename
				      "unable to open file ~s: ~a" raw-filename exn))))
			       (open-input-file
				 (if (and original-directory
				       (not (complete-path? raw-filename)))
				   (path->complete-path raw-filename
				     original-directory)
				   raw-filename)))))
		    (parameterize ([current-load-relative-directory
				     (if (string? base) 
				       (if (complete-path? base)
					 base
					 (path->complete-path base
					   (or original-directory 
					     (current-directory))))
				       (or original-directory
					 (current-directory)))])
		      (dynamic-wind
			void
			(lambda ()
			  (let ([exprs
				 (let ((reader (z:read p
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
				   (let loop ()
				     (let ((input (reader)))
				       (if (z:eof? input)
					   '()
					   (cons input
						 (loop))))))])
			    (expand-expr (structurize-syntax 
					  (cons 'begin exprs) 
					  expr '(-1)
					  #f
					  (z:make-origin 'micro expr))
					 env attributes
					 vocab)))
			(lambda ()
			  (close-input-port p))))))))))
	(else
          (static-error
	    "include" 'kwd:unit-include
	    expr "malformed expression"))))))

(define unit/sig-micro
    (let* ((kwd-1 '(import rename))
	    (in-pattern-1 '(_ signature
			     (import imports ...)
			     (rename renames ...)
			     clauses ...))
	    (m&e-1 (pat:make-match&env in-pattern-1 kwd-1))
	    (kwd-2 '(import))
	    (in-pattern-2 '(_ signature
			     (import imports ...)
			     clauses ...))
	    (out-pattern-2 '(unit/sig signature
			      (import imports ...)
			      (rename)
			      clauses ...))
	    (m&e-2 (pat:make-match&env in-pattern-2 kwd-2)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (let* ((in:signature (pat:pexpand 'signature p-env kwd-1))
		      (in:imports (pat:pexpand '(imports ...) p-env kwd-1))
		      (in:renames (pat:pexpand '(renames ...) p-env kwd-1))
		      (in:clauses (pat:pexpand '(clauses ...) p-env kwd-1))
		      (sigenv (sig-env env)))
		(let* ((prim-unit:imports (apply append
					    (map (lambda (import)
						   (expand-expr import sigenv
								attributes
								u/s-prim-imports-vocab))
					      in:imports)))
			(prim-unit:exports (create-prim-exports in:signature
					     in:renames expr env attributes))
			(prim-unit:clauses in:clauses)
			(sign-unit:imports (map (lambda (import)
						  (expand-expr import sigenv
						    attributes
						    u/s-sign-imports-vocab))
					     in:imports))
			(sign-unit:exports (expand-expr in:signature sigenv
					     attributes u/s-sign-exports-vocab)))
		  (expand-expr
		    ;; We don't use '(-1) as the third argument to
		    ;; structurize-syntax since the
		    ;; prim-unit:{imports,exports} are raw sexp's
		    ;; which get undesirably marked in the process,
		    ;; leading to imports not matching against uses in
		    ;; the body.  This should be remedied by making
		    ;; these values structurized, so that the
		    ;; remainder can also be structurized with
		    ;; impunity and '(-1) can be used.
		    (structurize-syntax
		      `(#%make-unit-with-signature
			 (#%unit
			   (import ,@prim-unit:imports)
			   (export ,@prim-unit:exports)
			   ,@prim-unit:clauses)
			 (quote ,(map named-sig-list->named-sig-vector sign-unit:imports))
			 (quote ,(sig-list->sig-vector sign-unit:exports)))
		      expr '()
		      #f
		      (z:make-origin 'micro expr))
		    env attributes (append-vocabulary vocab
						      u/s-expand-includes-vocab
						      'include-within-unit))))))
	  ((pat:match-against m&e-2 expr env)
	    =>
	    (lambda (p-env)
	      (expand-expr
		(structurize-syntax
		  (pat:pexpand out-pattern-2 p-env kwd-2)
		  expr
		  '()
		  #f
		  (z:make-origin 'micro expr))
		env attributes vocab)))
	  (else
	    (static-error
	      "unit/sig" 'kwd:unit/sig
	      expr "malformed expression"))))))


(add-primitivized-micro-form 'unit/sig full-vocabulary unit/sig-micro)
(add-primitivized-micro-form 'unit/sig scheme-vocabulary unit/sig-micro)

; --------------------------------------------------------------------

(define cu/s-imports-record-tag-sigs-vocab
  (create-vocabulary 'cu/s-imports-record-tag-sigs-vocab #f
    "malformed import clause"
    "malformed import clause"
    "malformed import clause"
    "malformed import clause"))

(add-list-micro cu/s-imports-record-tag-sigs-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (let ((table (extract-cu/s-tag-table attributes)))
		(when (cu/s-tag-table-lookup table tag)
		  (static-error
		    "compound-unit/sig" 'kwd:compound-unit/sig tag
		    "duplicate link tag definition"))
		(cu/s-tag-table-put/import table tag sig env attributes)))))
	(else
	  (static-error
	    "compound unit/sig" 'kwd:compound-unit/sig expr
	    "malformed import clause"))))))

(define cu/s-sign-imports-vocab
  (create-vocabulary 'cu/s-sign-imports-vocab #f
    "malformed import clause"
    "malformed import clause"
    "malformed import clause"
    "malformed import clause"))

(add-list-micro cu/s-sign-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (let ((table (extract-cu/s-tag-table attributes)))
		(cons (z:read-object tag)
		  (signature-exploded
		    (tag-table-entry-signature
		      (cu/s-tag-table-lookup/internal-error table tag))))))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig expr
	    "malformed import clause"))))))

(define cu/s-link-imports-vocab
  (create-vocabulary 'cu/s-link-imports-vocab #f
    "malformed link imports declaration"
    "malformed link imports declaration"
    "malformed link imports declaration"
    "malformed link imports declaration"))

(add-list-micro cu/s-link-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (let ((table (extract-cu/s-tag-table attributes)))
		(convert-to-prim-format
		  (signature-elements
		    (tag-table-entry-signature
		      (cu/s-tag-table-lookup/internal-error table tag)))
		  (z:read-object tag))))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig expr
	    "malformed import clause"))))))

; --------------------------------------------------------------------

(define cu/s-link-record-tag-sigs-vocab
  (create-vocabulary 'cu/s-link-record-tag-sigs-vocab #f
    "malformed link clause"
    "malformed link clause"
    "malformed link clause"
    "malformed link clause"))

(add-list-micro cu/s-link-record-tag-sigs-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig misc))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (let ((table (extract-cu/s-tag-table attributes)))
		(when (cu/s-tag-table-lookup table tag)
		  (static-error
		    "unit linkage" 'term:unit-link-duplicate-tag
		    tag
		    "duplicate link tag name"))
		(cu/s-tag-table-put/link table tag sig env attributes)))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed link clause"))))))

(define cu/s-link-exports-vocab
  (create-vocabulary 'cu/s-link-exports-vocab #f
    "malformed link export declaration"
    "malformed link export declaration"
    "malformed link export declaration"
    "malformed link export declaration"))

(add-list-micro cu/s-link-exports-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig misc))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (let ((table (extract-cu/s-tag-table attributes)))
		(signature-exploded
		  (tag-table-entry-signature
		    (cu/s-tag-table-lookup/internal-error table tag)))))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed link clause"))))))

(define cu/s-link-tags-vocab
  (create-vocabulary 'cu/s-link-tags-vocab #f
    "malformed link tag declaration"
    "malformed link tag declaration"
    "malformed link tag declaration"
    "malformed link tag declaration"))

(add-list-micro cu/s-link-tags-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig misc))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      tag)))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed link clause"))))))

(define cu/s-link-exprs-vocab
  (create-vocabulary 'cu/s-link-exprs-vocab #f
    "malformed link expression"
    "malformed link expression"
    "malformed link expression"
    "malformed link expression"))

(add-list-micro cu/s-link-exprs-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig (expr path ...)))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((expr (pat:pexpand 'expr p-env kwd)))
	      expr)))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compount-unit/sig
	    expr "malformed link clause"))))))

(define cu/s-link-linking-sigs-vocab
  (create-vocabulary 'cu/s-link-linking-sigs-vocab #f
    "malformed link clause"
    "malformed link clause"
    "malformed link clause"
    "malformed link clause"))

(add-list-micro cu/s-link-linking-sigs-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig (expr path ...)))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (path-elts (pat:pexpand '(path ...) p-env kwd)))
	      (map (lambda (p)
		     (put-attribute attributes cu/s-this-link-attr
		       (z:read-object tag))
		     (expand-expr p env attributes
		       cu/s-unit-path-linkage-vocab))
		path-elts))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed link clause"))))))

(define cu/s-check-self-import
  (lambda (tag attributes)
    (when #f ; we allow self-import, now
      (when (eq? (z:read-object tag)
		 (get-attribute attributes cu/s-this-link-attr
				(lambda () (internal-error tag "No this-link attribute"))))
	(static-error
	  "unit linkage" 'term:unit-link-self-import-tag tag
	  "self import of tag ~s" (z:read-object tag))))))

(define cu/s-link-prim-unit-names-vocab
  (create-vocabulary 'cu/s-link-prim-unit-names-vocab #f
    "malformed link clause"
    "malformed link clause"
    "malformed link clause"
    "malformed link clause"))

(add-list-micro cu/s-link-prim-unit-names-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig (expr path ...)))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (path-elts (pat:pexpand '(path ...) p-env kwd)))
	      (apply append
		(map (lambda (p)
		       (expand-expr p env attributes
			 cu/s-unit-path-prim-links-vocab))
		  path-elts)))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed link clause"))))))

; --------------------------------------------------------------------

(define cu/s-unit-path-extract-final-sig-vocab
  (create-vocabulary 'cu/s-unit-path-extract-final-sig-vocab))

(add-sym-micro cu/s-unit-path-extract-final-sig-vocab
  (lambda (expr env attributes vocab)
    (let ((sig
	    (tag-table-entry-signature
	      (cu/s-tag-table-lookup/static-error
		(extract-cu/s-tag-table attributes)
		expr))))
      sig)))

(add-list-micro cu/s-unit-path-extract-final-sig-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids))
		       (small-sig
			 (expand-expr sig env attributes sig-vocab)))
		  (with-handlers
		    ((exn:unit?
		       (lambda (exn)
			 (static-error
			   "signature matching" 'term:signature-not-matching
			   expr (exn-message exn)))))
		    (verify-signature-match 'compound-unit/sig
		      #f
		      (format "signature ~s" (signature-name small-sig))
		      (sig-list->sig-vector (signature-exploded small-sig))
		      (format "signature ~s" (signature-name final-sig))
		      (sig-list->sig-vector (signature-exploded final-sig))))
		  small-sig)))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (let ((big-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag)))
		     (small-sig
		       (expand-expr sig env attributes sig-vocab)))
		(with-handlers
		  ((exn:unit?
		     (lambda (exn)
		       (static-error
			 "signature matching" 'term:signature-not-matching
			 expr (exn-message exn)))))
		  (verify-signature-match 'compound-unit/sig
		    #f
		    (format "signature ~s" (signature-name small-sig))
		    (sig-list->sig-vector (signature-exploded small-sig))
		    (format "signature ~s" (signature-name big-sig))
		    (sig-list->sig-vector (signature-exploded big-sig)))
		  small-sig)))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (valid-syntactic-id? tag)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids)))
		  final-sig)))))
	(else
	  (static-error
	    "unit linkage" 'kwd:unit-link-path-malformed
	    expr "malformed unit path element"))))))

(define cu/s-unit-path-linkage-vocab
  (create-vocabulary 'cu/s-unit-path-linkage-vocab #f
    "malformed linkage"
    "malformed linkage"
    "malformed linkage"
    "malformed linkage"))

(add-sym-micro cu/s-unit-path-linkage-vocab
  (lambda (expr env attributes vocab)
    (cu/s-check-self-import expr attributes)
    (let ((sig
	    (tag-table-entry-signature
	      (cu/s-tag-table-lookup/static-error
		(extract-cu/s-tag-table attributes)
		expr))))
      (cons (z:read-object expr)
	(signature-exploded sig)))))

(add-list-micro cu/s-unit-path-linkage-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (cu/s-check-self-import tag attributes)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids))
		       (small-sig
			 (expand-expr sig env attributes sig-vocab)))
		  (with-handlers
		    ((exn:unit?
		       (lambda (exn)
			 (static-error
			   "signature matching" 'term:signature-not-matching
			   expr (exn-message exn)))))
		    (verify-signature-match 'compound-unit/sig
		      #f
		      (format "signature ~s" (signature-name small-sig))
		      (sig-list->sig-vector (signature-exploded small-sig))
		      (format "signature ~s" (signature-name final-sig))
		      (sig-list->sig-vector (signature-exploded final-sig)))
		    (cons (z:read-object tag)
		      (signature-exploded small-sig))))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (cu/s-check-self-import tag attributes)
	      (let ((big-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag)))
		     (small-sig
		       (expand-expr sig env attributes sig-vocab)))
		(with-handlers
		  ((exn:unit?
		     (lambda (exn)
		       (static-error
			 "signature matching" 'term:signature-not-matching
			 expr (exn-message exn)))))
		  (verify-signature-match 'compound-unit/sig
		    #f
		    (format "signature ~s" (signature-name small-sig))
		    (sig-list->sig-vector (signature-exploded small-sig))
		    (format "signature ~s" (signature-name big-sig))
		    (sig-list->sig-vector (signature-exploded big-sig)))
		  (cons (z:read-object tag)
		    (signature-exploded small-sig)))))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (valid-syntactic-id? tag)
	      (cu/s-check-self-import tag attributes)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids)))
		  (cons (z:read-object tag)
		    (signature-exploded final-sig)))))))
	(else
	  (static-error
	    "unit linkage" 'kwd:unit-link-path-malformed
	    expr "malformed unit path element"))))))

(define cu/s-unit-path-prim-links-vocab
  (create-vocabulary 'cu/s-unit-path-prim-links-vocab #f
    "malformed linkage"
    "malformed linkage"
    "malformed linkage"
    "malformed linkage"))

(add-sym-micro cu/s-unit-path-prim-links-vocab
  (lambda (expr env attributes vocab)
    (let ((tag-table-entry
	    (cu/s-tag-table-lookup/static-error
	      (extract-cu/s-tag-table attributes)
	      expr)))
      (let ((sig (tag-table-entry-signature tag-table-entry)))
	(cond
	  ((tag-table-import-entry? tag-table-entry)
	    (cu/s-build-link-names sig
	      (string-append
		(cu/s-build-link-prefix (list expr))
		":")))
	  ((tag-table-link-entry? tag-table-entry)
	    (list
	      (cons (z:read-object expr)
		(cu/s-build-link-names sig))))
	  (else
	    (internal-error tag-table-entry "Illegal tag-table entry")))))))

(add-list-micro cu/s-unit-path-prim-links-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (let ((small-sig
		      (expand-expr sig env attributes sig-vocab)))
		(let ((tag-table-entry (cu/s-tag-table-lookup/internal-error
					 (extract-cu/s-tag-table attributes)
					 tag)))
		  (cond
		    ((tag-table-import-entry? tag-table-entry)
		      (cu/s-build-link-names small-sig
			(string-append
			  (cu/s-build-link-prefix ids tag)
			  ":")))
		    ((tag-table-link-entry? tag-table-entry)
		      (list
			(cons (z:read-object tag)
			  (cu/s-build-link-names small-sig
			    (cu/s-build-link-prefix ids)))))
		    (else
		      (internal-error tag-table-entry
			"Illegal tag-table entry"))))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (let ((small-sig
		       (expand-expr sig env attributes sig-vocab)))
		(let ((tag-table-entry (cu/s-tag-table-lookup/internal-error
					 (extract-cu/s-tag-table attributes)
					 tag)))
		  (cond
		    ((tag-table-import-entry? tag-table-entry)
		      (cu/s-build-link-names small-sig
			(string-append
			  (cu/s-build-link-prefix (list tag))
			  ":")))
		    ((tag-table-link-entry? tag-table-entry)
		      (list
			(cons (z:read-object tag)
			  (cu/s-build-link-names small-sig))))
		    (else
		      (internal-error tag-table-entry
			"Illegal tag-table entry"))))))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids)))
		  (let ((tag-table-entry (cu/s-tag-table-lookup/internal-error
					   (extract-cu/s-tag-table attributes)
					   tag)))
		    (cond
		      ((tag-table-import-entry? tag-table-entry)
			(cu/s-build-link-names final-sig
			  (string-append
			    (cu/s-build-link-prefix ids tag)
			    ":")))
		      ((tag-table-link-entry? tag-table-entry)
			(list
			  (cons (z:read-object tag)
			    (cu/s-build-link-names final-sig
			      (string-append
				(cu/s-build-link-prefix ids)
				":")))))
		      (else
			(internal-error tag-table-entry
			  "Illegal tag-table entry")))))))))
	(else
	  (static-error
	    "unit linkage" 'kwd:unit-link-path-malformed
	    expr "malformed unit path element"))))))

(define cu/s-unit-path-tag+build-prefix-vocab
  (create-vocabulary 'cu/s-unit-path-tag+build-prefix-vocab))

; Returns a pair of values:
; - Prefix tag of unit-path as Scheme symbol
; - String representing unit-path with ":" interspersed

(add-sym-micro cu/s-unit-path-tag+build-prefix-vocab
  (lambda (expr env attributes vocab)
    (cons (z:read-object expr)
      "")))

(add-list-micro cu/s-unit-path-tag+build-prefix-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (cons (z:read-object tag)
		(apply symbol-append
		  (let loop ((ids ids))
		    (if (null? ids) '("")
		      (if (null? (cdr ids))
			(list (z:read-object (car ids)))
			(cons (z:read-object (car ids))
			  (cons ":"
			    (loop (cdr ids))))))))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (cons (z:read-object tag)
		""))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (cons (z:read-object tag)
		(apply symbol-append
		  (let loop ((ids ids))
		    (if (null? ids) '("")
		      (if (null? (cdr ids))
			(list (z:read-object (car ids)))
			(cons (z:read-object (car ids))
			  (cons ":"
			    (loop (cdr ids))))))))))))
	(else
	  (static-error
	    "unit linkage" 'kwd:unit-link-path-malformed
	    expr "malformed unit path element"))))))

(define cu/s-unit-path-tag-vocab
  (create-vocabulary 'cu/s-unit-path-tag-vocab))

; Returns prefix tag of unit-path as Scheme symbol

(add-sym-micro cu/s-unit-path-tag-vocab
  (lambda (expr env attributes vocab)
    (z:read-object expr)))

(add-list-micro cu/s-unit-path-tag-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (cons (z:read-object tag)
		(apply symbol-append
		  (let loop ((ids ids))
		    (if (null? ids) '("")
		      (if (null? (cdr ids))
			(list (z:read-object (car ids)))
			(cons (z:read-object (car ids))
			  (cons ":"
			    (loop (cdr ids))))))))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (z:read-object tag))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (z:read-object tag))))
	(else
	  (static-error
	    "unit linkage" 'kwd:unit-link-path-malformed
	    expr "malformed unit path element"))))))

(define cu/s-build-link-names
  (opt-lambda (signature (prefix-string ""))
    (convert-to-prim-format-helper (signature-elements signature)
      prefix-string)))

(define cu/s-build-link-prefix
  (opt-lambda (ids (tag #f))
    (if (null? ids)
      ""
      (apply string-append
	(let ((result (let loop ((str-ids (map symbol->string
					    (map z:read-object ids))))
			(if (null? (cdr str-ids))
			  (list (car str-ids))
			  (cons (car str-ids)
			    (cons ":"
			      (loop (cdr str-ids))))))))
	  (if tag
	    (cons (symbol->string (z:read-object tag))
	      (cons ":"
		result))
	    result))))))

; --------------------------------------------------------------------

(define-struct cu/s-export ())
(define-struct (cu/s-var-export struct:cu/s-export) (var external))
(define-struct (cu/s-unit-export struct:cu/s-export) (sig name))
(define-struct (cu/s-open-export struct:cu/s-export) (sig))

(define cu/s-verify-variable-in-path
  (lambda (path variable env attributes)
    (let ((tag-table (extract-cu/s-tag-table attributes)))
      (let ((final-sig (expand-expr path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
	(cu/s-verify-variable-in-sig
	  (signature-exploded final-sig)
	  variable)))))

(define cu/s-verify-variable-in-sig
  (lambda (sig variable)
    (let ((raw-var (z:read-object variable)))
      (let loop ((elements (signature-elements sig)))
	(if (null? elements)
	  (static-error
	    "signature" 'term:signature-no-var variable
	    "no such identifier")
	  (or (and (name-element? (car elements))
		(eq? raw-var (name-element-name (car elements))))
	    (loop (cdr elements))))))))

(define cu/s-prim-export-vocab
  (create-vocabulary 'cu/s-prim-export-vocab #f
    "malformed export declaration"
    "malformed export declaration"
    "malformed export declaration"
    "malformed export declaration"))

; Returns a fully-formed export element of the form
;   (tag (internal-name external-name))
; where each is a symbol or a z:symbol

(define prefix-w/-:
  (lambda (prefix name)
    (cond
      ((symbol? prefix)
	(if (string=? "" (symbol->string prefix))
	  name
	  (symbol-append prefix ":" name)))
      ((string? prefix)
	(if (string=? "" prefix)
	  name
	  (symbol-append prefix ":" name)))
      (else
	(internal-error 'prefix-w/-: "Got ~s as prefix" prefix)))))

(add-micro-form 'var cu/s-prim-export-vocab
  (let* ((kwd '(var))
	  (in-pattern-1 '(var (unit-path variable)))
	  (in-pattern-2 '(var (unit-path variable) external-variable))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd)))
	      (valid-syntactic-id? variable)
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab)))
		(cons (car tag+prefix)
		  (list (list (prefix-w/-: (cdr tag+prefix)
				(z:read-object variable))
			  variable)))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd))
		   (external (pat:pexpand 'external-variable p-env kwd)))
	      (valid-syntactic-id? variable)
	      (valid-syntactic-id? external)
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab)))
		(cons (car tag+prefix)
		  (list (list (prefix-w/-: (cdr tag+prefix)
				(z:read-object variable))
			  external)))))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed var export"))))))

(add-micro-form 'open cu/s-prim-export-vocab
  (let* ((kwd '(open))
	  (in-pattern '(open unit-path))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd)))
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(cons (car tag+prefix)
		  (map list
		    (convert-to-prim-format
		      (signature-elements final-sig)
		      (cdr tag+prefix))
		    (convert-to-prim-format
		      (signature-elements final-sig))))))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed open export"))))))

(add-micro-form 'unit cu/s-prim-export-vocab
  (let* ((kwd '(unit))
	  (in-pattern-1 '(unit unit-path))
	  (in-pattern-2 '(unit unit-path variable))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd)))
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(cons (car tag+prefix)
		  (map list
		    (convert-to-prim-format (signature-elements final-sig)
		      (cdr tag+prefix))
		    (convert-to-prim-format (signature-elements final-sig)
		      (car tag+prefix))))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd)))
	      (valid-syntactic-id? variable)
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(cons (car tag+prefix)
		  (map list
		    (convert-to-prim-format (signature-elements final-sig)
		      (cdr tag+prefix))
		    (convert-to-prim-format (signature-elements final-sig)
		      (z:read-object variable))))))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed unit export"))))))

(define cu/s-export-sign-vocab
  (create-vocabulary 'cu/s-export-sign-vocab))

(add-micro-form 'var cu/s-export-sign-vocab
  (let* ((kwd '(var))
	  (in-pattern-1 '(var (unit-path variable)))
	  (in-pattern-2 '(var (unit-path variable) external-variable))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd)))
	      (list variable))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd))
		   (external (pat:pexpand 'external-variable p-env kwd)))
	      (list external))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed var export"))))))

(add-micro-form 'open cu/s-export-sign-vocab
  (let* ((kwd '(open))
	  (in-pattern '(open unit-path))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd)))
	      (let ((final-sig
		      (expand-expr unit-path env attributes
			cu/s-unit-path-extract-final-sig-vocab)))
		(signature-exploded final-sig)))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed open export"))))))

(add-micro-form 'unit cu/s-export-sign-vocab
  (let* ((kwd '(unit))
	  (in-pattern-1 '(unit unit-path))
	  (in-pattern-2 '(unit unit-path variable))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd)))
	      (let ((tag
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(list (cons tag
			(signature-exploded final-sig)))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd)))
	      (let ((tag
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(list (cons (z:read-object variable)
			(signature-exploded final-sig)))))))
	(else
	  (static-error
	    "compound-unit/sig" 'kwd:compound-unit/sig
	    expr "malformed unit export"))))))

; --------------------------------------------------------------------

(define record-tag-signatures
  (lambda (imports links env attributes)
    (map (lambda (i)
	   (expand-expr i env attributes cu/s-imports-record-tag-sigs-vocab))
      imports)
    (map (lambda (l)
	   (expand-expr l env attributes cu/s-link-record-tag-sigs-vocab))
      links)))

(define compound-unit/sig-micro
    (let* ((kwd '(import link export))
	    (in-pattern '(_
			   (import imports ...)
			   (link links ...)
			   (export exports ...)))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (put-attribute attributes cu/s-attr
		(cons (make-hash-table)
		  (get-attribute attributes cu/s-attr
		    (lambda () '()))))
	      (let* ((in:imports (pat:pexpand '(imports ...) p-env kwd))
		      (in:links (pat:pexpand '(links ...) p-env kwd))
		      (in:exports (pat:pexpand '(exports ...) p-env kwd))
		      (sigenv (sig-env env)))
		(record-tag-signatures in:imports in:links sigenv attributes)
		;; linkage = given to verify-linkage-signature-match
		;; prim = goes into underlying compound-unit
		;; sign = given to make-unit-with-signature
		(let* ((linkage:tags (map (lambda (l)
					    (expand-expr l sigenv attributes
					      cu/s-link-tags-vocab))
				       in:links))
			(linkage:unit-vars linkage:tags)
			(linkage:unit-exprs (map (lambda (l)
						   (expand-expr l sigenv attributes
						     cu/s-link-exprs-vocab))
					      in:links))
			(linkage:link-exports
			  (map (lambda (l)
				 (expand-expr l sigenv attributes
				   cu/s-link-exports-vocab))
			    in:links))
			(linkage:link-imports
			  (map (lambda (l)
				 (expand-expr l sigenv attributes
				   cu/s-link-linking-sigs-vocab))
			    in:links))
			(prim:imports (apply append
					(map (lambda (l)
					       (expand-expr l sigenv attributes
						 cu/s-link-imports-vocab))
					  in:imports)))
			(prim:links (map (lambda (l)
					   (expand-expr l sigenv attributes
					     cu/s-link-prim-unit-names-vocab))
				      in:links))
			(prim:exports (map (lambda (e)
					     (expand-expr e sigenv attributes
					       cu/s-prim-export-vocab))
					in:exports))
			(sign:imports (map (lambda (i)
					     (expand-expr i sigenv attributes
					       cu/s-sign-imports-vocab))
					in:imports))
			(sign:exports (apply append
					(map (lambda (e)
					       (expand-expr e sigenv attributes
						 cu/s-export-sign-vocab))
					  in:exports))))
		  (check-unique-cu/s-exports in:exports sign:exports)
		  (let ((output
			  `(let ,(map list linkage:unit-vars linkage:unit-exprs)
			     (#%verify-linkage-signature-match
			       'compound-unit/sig
			       ',linkage:tags
			       (#%list ,@linkage:unit-vars)
			       ',(map sig-list->sig-vector linkage:link-exports)
			       ',(map (lambda (l)
					(map named-sig-list->named-sig-vector l))
				   linkage:link-imports))
			     (#%make-unit-with-signature
			       (compound-unit
				 (import ,@prim:imports)
				 (link ,@(map (lambda (tag body)
						`(,tag
						   ((#%unit-with-signature-unit
						      ,tag)
						     ,@body)))
					   linkage:tags prim:links))
				 (export ,@prim:exports))
			       ',(map named-sig-list->named-sig-vector sign:imports)
			       ',(sig-list->sig-vector sign:exports)))))
		    (expand-expr
		      (structurize-syntax
			output
			expr '(-1)
			#f
			(z:make-origin 'micro expr))
		      env attributes vocab))))))
	  (else
	    (static-error
	      "compound-unit/sig" 'kwd:compound-unit/sig
	      expr "malformed expression"))))))

(add-primitivized-micro-form 'compound-unit/sig full-vocabulary compound-unit/sig-micro)
(add-primitivized-micro-form 'compound-unit/sig scheme-vocabulary compound-unit/sig-micro)


; --------------------------------------------------------------------

(define iu/s-linkage-vocab
  (create-vocabulary 'iu/s-linkage-vocab #f
    "malformed linkage declaration"
    "malformed linkage declaration"
    "malformed linkage declaration"
    "malformed linkage declaration"))

(add-sym-micro iu/s-linkage-vocab
  (lambda (expr env attributes vocab)
    (cons expr
      (signature-exploded (expand-expr expr env attributes sig-vocab)))))

(add-list-micro iu/s-linkage-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((in:id (pat:pexpand 'id p-env kwd))
		   (in:sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? in:id)
	      (cons (z:read-object in:id)
		(signature-exploded
		  (expand-expr in:sig env attributes sig-vocab))))))
	((pat:match-against m&e-2 expr env)
	  (static-error
	    "signature" 'term:signature-ambiguous-: expr
	    "ambiguous : in signature"))
	(else
	  (cons immediate-signature-name
	    (signature-exploded
	      (expand-expr expr env attributes sig-vocab))))))))

(define iu/s-imports-vocab
  (create-vocabulary 'iu/s-imports-vocab #f
    "malformed import declaration"
    "malformed import declaration"
    "malformed import declaration"
    "malformed import declaration"))

(add-sym-micro iu/s-imports-vocab
  (lambda (expr env attributes vocab)
    (convert-to-prim-format
      (signature-elements (expand-expr expr env attributes sig-vocab)))))

(add-list-micro iu/s-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((in:id (pat:pexpand 'id p-env kwd))
		   (in:sig (pat:pexpand 'sig p-env kwd)))
	      (convert-to-prim-format
		(signature-elements
		  (expand-expr in:sig env attributes sig-vocab))
		(z:read-object in:id)))))
	((pat:match-against m&e-2 expr env)
	  (static-error
	    "signature" 'term:signature-ambiguous-: expr
	    "ambiguous : in signature"))
	(else
	  (convert-to-prim-format
	    (signature-elements
	      (expand-expr expr env attributes sig-vocab))))))))


(define do-invoke-unit/sig-micro
  (lambda (in:expr in:linkage expr env attributes vocab)
    (let* ((sigenv (sig-env env))
	   (proc:linkage (map (lambda (l)
				(expand-expr l sigenv attributes
					     iu/s-linkage-vocab))
			      in:linkage))
	   (proc:imports (apply append
				(map (lambda (l)
				       (expand-expr l sigenv attributes
						    iu/s-imports-vocab))
				     in:linkage))))
      (expand-expr
       (structurize-syntax
	`(let ((unit ,in:expr))
	   (#%verify-linkage-signature-match
	    'invoke-unit/sig
	    '(invoke)
	    (#%list unit)
	    '(#())
	    '(,(map named-sig-list->named-sig-vector proc:linkage)))
	   (#%invoke-unit
	    (#%unit-with-signature-unit unit)
	    ;; Structurize proc:imports without marks to allow capture
	    ,@(map (lambda (x) (structurize-syntax x expr '()))
		   proc:imports)))
	expr '(-1)
	#f
	(z:make-origin 'micro expr))
       env attributes vocab))))

(define invoke-unit/sig-micro
  (let* ((kwd '())
	 (in-pattern '(_ expr linkage ...))
	 (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
       ((pat:match-against m&e expr env)
	=>
	(lambda (p-env)
	  (let ((in:expr (pat:pexpand 'expr p-env kwd))
		(in:linkage (pat:pexpand '(linkage ...) p-env kwd)))
	    (do-invoke-unit/sig-micro
	     in:expr in:linkage
	     expr env attributes vocab))))
	  (else
	    (static-error
	      "invoke-unit/sig" 'kwd:invoke-unit/sig
	      expr "malformed expression"))))))

(add-primitivized-micro-form 'invoke-unit/sig full-vocabulary invoke-unit/sig-micro)
(add-primitivized-micro-form 'invoke-unit/sig scheme-vocabulary invoke-unit/sig-micro)

(define unit->unit/sig-micro
    (let* ((kwd '())
	    (in-pattern '(_ expr (in-sig ...) out-sig))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((in-expr (pat:pexpand 'expr p-env kwd))
		    (in-sigs (pat:pexpand '(in-sig ...) p-env kwd))
		    (out-sig (pat:pexpand 'out-sig p-env kwd))
		    (sigenv (sig-env env)))
		(expand-expr
		  (structurize-syntax
		    `(#%make-unit-with-signature
		       ,in-expr
		       ',(map
			  named-sig-list->named-sig-vector
			  (map (lambda (s)
				(let ((proc:s
					(expand-expr s sigenv attributes
					  sig-vocab)))
				  (cons (signature-name proc:s)
				    (signature-exploded proc:s))))
			   in-sigs))
		       ',(sig-list->sig-vector
			  (let ((proc:s
				 (expand-expr out-sig sigenv attributes sig-vocab)))
			   (signature-exploded proc:s))))
		    expr '(-1)
		    #f
		    (z:make-origin 'micro expr))
		  env attributes vocab))))
	  (else
	    (static-error
	      "unit->unit/sig" 'kwd:unit->unit/sig
	      expr "malformed expression"))))))

(add-primitivized-micro-form 'unit->unit/sig full-vocabulary unit->unit/sig-micro)
(add-primitivized-micro-form 'unit->unit/sig scheme-vocabulary unit->unit/sig-micro)

(define do-define-invoke-micro
  (lambda (global? in:expr in:export in:imports prefix expr env attributes vocab)
    (let* ((sigenv (sig-env env))
	   (proc:linkage (map (lambda (l)
				(expand-expr l sigenv attributes
					     iu/s-linkage-vocab))
			      in:imports))
	   (proc:ex-linkage (expand-expr in:export sigenv attributes
					 u/s-sign-exports-vocab))
	   (proc:imports (apply append
				(map (lambda (l)
				       (expand-expr l sigenv attributes
						    iu/s-imports-vocab))
				     in:imports)))
	   (proc:exports (expand-expr in:export sigenv attributes
				      iu/s-imports-vocab)))
      (expand-expr
       (structurize-syntax
	`(,(if global?
	       'global-define-values/invoke-unit
	       'define-values/invoke-unit)
	   ,(map (lambda (x) (structurize-syntax x expr '()))
		 proc:exports)
	   (let ((unit ,in:expr))
	     (#%verify-linkage-signature-match
	      'invoke-unit/sig
	      '(invoke)
	      (#%list unit)
	      '(,(sig-list->sig-vector proc:ex-linkage))
	      '(,(map named-sig-list->named-sig-vector proc:linkage)))
	     (#%unit/sig->unit unit))
	   ,prefix
	   ;; Structurize proc:imports without marks to allow capture
	   ,@(map (lambda (x) (structurize-syntax x expr '()))
		  proc:imports))
	expr '(-1)
	#f
	(z:make-origin 'micro expr))
       env attributes vocab))))

(define (make-define-values/invoke-unit/sig-micro global?)
  (let* ((kwd '())
	 (in-pattern '(_ export expr))
	 (in-pattern2 '(_ export expr prefix linkage ...))
	 (m&e (pat:make-match&env in-pattern kwd))
	 (m&e2 (pat:make-match&env in-pattern2 kwd))
	 (badsyntax (lambda (expr why)
		      (static-error
			(if global?
			  "global-define-values"
			  "define-values")
			(if global?
			  'kwd:global-define-values
			  'kwd:define-values)
			expr 
			(format "Malformed ~adefine-values/invoke-unit/sig~a"
			  (if global? "global-" "")
			  why)))))
    (lambda (expr env attributes vocab)
      (let ([doit (lambda (p-env prefix?)
		    (let ((in:export (pat:pexpand 'export p-env kwd))
			  (in:expr (pat:pexpand 'expr p-env kwd))
			  (in:prefix (and prefix? (pat:pexpand 'prefix p-env kwd)))
			  (in:linkage (if prefix?
					  (pat:pexpand '(linkage ...) p-env kwd)
					  null)))
		      (unless (or (z:symbol? in:prefix)
				  (and (z:boolean? in:prefix)
				       (not (z:read-object in:prefix)))
				  (not in:prefix))
			(badsyntax expr " (bad prefix)"))
		      (do-define-invoke-micro
		       global?
		       in:expr in:export in:linkage in:prefix
		       expr env attributes vocab)))])
	
	(cond
	 [(pat:match-against m&e expr env)
	  => (lambda (p-env)
	       (doit p-env #f))]
	 [(pat:match-against m&e2 expr env)
	  => (lambda (p-env)
	       (doit p-env #t))]
	 [else
	  (badsyntax expr "")])))))

(add-on-demand-form 'micro 'define-values/invoke-unit/sig common-vocabulary
		    (make-define-values/invoke-unit/sig-micro #f))
(add-on-demand-form 'micro 'global-define-values/invoke-unit/sig common-vocabulary
		    (make-define-values/invoke-unit/sig-micro #t))
