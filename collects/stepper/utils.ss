(unit/sig stepper:cogen-utils^
  (import [z : zodiac:system^]
          [e : zodiac:interface^])
  
  
  ; get-binding-name extracts the S-expression name for a binding. Zodiac
  ; creates a unique, gensym'd symbol for each binding, but the name is
  ; unreadable. Here, we create a new gensym, but the name of the generated
  ; symbol prints in the same way as the original symbol.
  
  (define (get-binding-name binding)
    (let ([name (lookup-new-binding-name binding)])
      (or name
	  (let* ([orig-name (z:binding-orig-name binding)]
		 [name (string->uninterned-symbol (symbol->string orig-name))])
	    (set-new-binding-name! binding name)
	    name))))

  (define-values (lookup-new-binding-name set-new-binding-name!)
    (let-values ([(getter setter) (z:register-client 'new-name (lambda () #f))])
      (values
       (lambda (parsed) (getter (z:parsed-back parsed)))
       (lambda (parsed n) (setter (z:parsed-back parsed) n)))))

  ; check whether the supplied id is a keyword. if the id is a syntax or
  ; macro keyword, issue an error.  If disallow-procedures? is true, then
  ; we issue an error for _any_ use of a keyword. These procedures are used
  ; to prevent the program from redefining keywords.
  
  (define check-for-keyword/both
    (lambda (disallow-procedures?)
      (lambda (id)
	(let ([real-id
	       (cond
		 [(z:binding? id) (z:binding-orig-name id)]
		 [(z:top-level-varref? id) (z:varref-var id)]
		 [(z:bound-varref? id)
		  (z:binding-orig-name (z:bound-varref-binding id))]
		 [(z:symbol? id)
		  (z:read-object id)]
		 [else
		  (e:internal-error id "Given in check-for-keyword")])])
	  (when (and (keyword-name? real-id)
		     (or disallow-procedures?
			 (let ([gdv (global-defined-value real-id)])
			   (or (syntax? gdv)
			       (macro? gdv)))))
	    (e:static-error id "keyword: invalid use of keyword ~s" real-id))))))
  
  (define check-for-keyword (check-for-keyword/both #t))
  (define check-for-syntax-or-macro-keyword (check-for-keyword/both #f))
    
  (define the-undefined-value (letrec ((x x)) x))
  
  (define-struct (undefined struct:exn) (id))
  (define signal-undefined (make-parameter #t))
  (define undefined-error-format
    "Variable ~s referenced before definition or initialization")
  
  (define-struct (not-boolean struct:exn) (val))
  (define signal-not-boolean (make-parameter #f))
  (define not-boolean-error-format "Condition value is neither true nor false: ~e")
  
  ; there is a problem with Zodiac.  The problem is that Zodiac has not been
  ; distinguishing between top-level variables and those bound by unit clauses.
  ; this is an important distinction to make, because the variables bound by 
  ; unit clauses may take on the `undefined' value, whereas those bound as
  ; top-level variables will never require this check.  (If used before defined,
  ; these values are simply considered unbound.  To this end, Matthew has modified
  ; Zodiac to add a bit of information which aries can use to distinguish these 
  ; fields.  Currently, this information is stored in the `unit?' field of a 
  ; `top-level-varref/bind/unit' structure.  There are cleaner solutions, but
  ; this one fits well into the current state of the world.  This may change at
  ; some point in the future.  For the moment, here is the function which 
  ; distinguishes between these two types of binding:
  
  (define (is-unit-bound? varref)
    (and (z:top-level-varref/bind/unit? varref)
	 (z:top-level-varref/bind/unit-unit? varref)))
  
  ; Objects that are passed to eval get quoted by M3.  These objects
  ; do not belong in the `read' structure framework.  Hence, if they
  ; are passed to z:sexp->raw, they will error.  Thus, we first check
  ; before sending things there.
  
  ; jbc additional comments, including elucidation from shriram:
  ; there are three `levels' of parsed stuff:
  ; raw: simple, unannotated scheme values
  ; sexp: simple scheme values with attached zodiac information
  ; parsed: fully parsed into zodiac structures
  
  (define read->raw
    (lambda (read)
      (if (z:zodiac? read)
	  (z:sexp->raw read)
	  read)))
    
  ; divined notes about the structure of an arglist.  Evidently, an arglist can
  ; take one of three forms:
  ; list-arglist : this arglist represents a simple list of arguments
  ; ilist-arglist : this arglist represents a list of arguments which uses 
  ;   `dot-notation' to separate the last element of the list
  ; sym-arglist : this arglist represents the `single argument with no 
  ;   parens' style of argument list.
  
  (define arglist->ilist
    (lambda (arglist)
      (cond
	((z:list-arglist? arglist)
	 (z:arglist-vars arglist))
	((z:ilist-arglist? arglist)
	 (let loop ((vars (z:arglist-vars arglist)))
	   (if (null? (cddr vars))
	       (cons (car vars) (cadr vars))
	       (cons (car vars) (loop (cdr vars))))))
	((z:sym-arglist? arglist)
	 (car (z:arglist-vars arglist)))
	(else
	 (e:internal-error arglist
                           "Given to arglist->ilist")))))
  
  (define make-improper
    (lambda (combine)
      (rec improper ;; `rec' is for the name in error messages
	   (lambda (f list)
	     (let improper-loop ([list list])
	       (cond
		 ((null? list) list)
		 ((pair? list) (combine (f (car list))
					(improper-loop (cdr list))))
		 (else (f list))))))))
  (define improper-map (make-improper cons))
  (define improper-foreach (make-improper (lambda (x y) y))))