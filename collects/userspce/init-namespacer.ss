(unit/sig plt:init-namespace^
  (import plt:basis-import^
	  [init-params : plt:init-params^]
	  mzlib:function^)
  
  (define (exploded->flattened exploded)
    (let ([sig exploded])
      (let loop ([l (vector->list sig)][r null])
	(cond
	 [(null? l) r]
	 [(symbol? (car l)) (loop (cdr l) (cons (car l) r))]
	 [else (let ([sub (loop (vector->list (cadr l)) null)]
		     [prefix (string-append (symbol->string (car l)) ":")])
		 (loop (cdr l)
		       (append
			(map (lambda (s)
			       (string->symbol
				(string-append
				 prefix
				 (symbol->string s))))
			     sub))))]))))

  (define (build-gdvs exploded)
    (let ([flattened (exploded->flattened exploded)])
      (map
       (lambda (x) `(global-defined-value ',x ,x))
       flattened)))

  (define core-flat@ (require-library-unit/sig "coreflatr.ss"))
  
  ;; build-single-teachpack-unit : string -> (union #f (unit () X))
  (define (build-single-teachpack-unit v)
    (with-handlers
     ([(lambda (x) #t)
       (lambda (x)
	 (invalid-teachpack (exn-message x))
	 #f)])
     (let ([new-unit (parameterize ([read-case-sensitive #t])
				   (load/cd v))])
       (if (unit/sig? new-unit)
					; Put the unit into a procedure that invokes it into
					;  the current namespace
	   (let* ([signature 
		   (exploded->flattened (unit-with-signature-exports new-unit))])
	     (eval
	      `(unit/sig ()
		 (import plt:userspace^)
		 (with-handlers ([(lambda (x) #t)
				  (lambda (x)
				    ((error-display-handler)
				     (format
				      "Invalid Teachpack: ~a~n~a"
				      ,v
				      (if (exn? x)
					  (exn-message x)
					  x))))])
		   (global-define-values/invoke-unit/sig
		    ,signature
		    ,new-unit
		    #f
		    plt:userspace^)))))
	   (begin
	     (invalid-teachpack 
	      (format "loading Teachpack file does not result in a unit/sig, got: ~e"
		      new-unit))
	     #f)))))

  ;; build-namespace-thunk : (listof string) -> (union #f (list (union 'mz 'mr) (-> void)))
  ;; accepts a filename and returns a thunk that invokes the corresponding teachpack and
  ;; a symbol indicating if this is a mzscheme teachpack or a mred teachpack.
  (define (build-namespace-thunk v)
    (unless (and (list? v)
		 (andmap string? v))
	    (error 'build-teachpack-thunk "expected a list of strings, got: ~e" v))
    (let* ([tagn 0]
	   [link-clauses
	    (let loop ([units v]
		       [link-clauses null])
	      (cond
	       [(null? units) (reverse link-clauses)]
	       [else
		(let ([unit (build-single-teachpack-unit (car units))])
		  (if unit
		      (begin
			(set! tagn (+ tagn 1))
			(loop (cdr units)
			      (cons
			       `[,(string->symbol (format "teachpack~a" tagn)) : ()
				 (,unit userspace)]
			       link-clauses)))
		      (loop (cdr units)
			    link-clauses)))]))]
	   [cu
	    (eval
	     `(compound-unit/sig
		(import)
		(link
		 ,@(list*
		    `[userspace
		      : plt:userspace^ 
		      (,(if (defined? 'mred@)
			    `(compound-unit/sig
			       (import)
			       (link [core : mzlib:core-flat^ (,core-flat@)]
				     [mred : mred^ (,(global-defined-value 'mred@))]
				     [turtles : turtle^ ((require-library "turtler.ss" "graphics")
							 (core : mzlib:function^))]
				     [posn : ((struct posn (x y)))
					   ((unit/sig ((struct posn (x y)))
					      (import)
					      (define-struct posn (x y))))])
			       (export (open core)
				       (open mred)
				       (open posn)
				       (open turtles)))
			    `(compound-unit/sig 
			       (import)
			       (link [core : mzlib:core-flat^ (,core-flat@)]
				     [posn : ((struct posn (x y)))
					   ((unit/sig ((struct posn (x y)))
					      (import)
					      (define-struct posn (x y))))])
			       (export (open core)
				       (open posn)))))]
		    `[language-specific-additions
		      : ()
		      ((unit/sig ()
			 (import plt:userspace^)

			 (cond
			  [(,init-params:beginner-language? (,init-params:current-setting))
			   ,@(build-gdvs (signature->symbols plt:beginner-extras^))]
			  [(,init-params:intermediate-language? (,init-params:current-setting))
			   ,@(build-gdvs (signature->symbols plt:intermediate-extras^))]
			  [(,init-params:advanced-language? (,init-params:current-setting))
			   ,@(build-gdvs (signature->symbols plt:advanced-extras^))]
			  [(,init-params:full-language? (,init-params:current-setting)) (void)]))
		       userspace)]

		    link-clauses))
		(export)))])
      (lambda ()
	(invoke-unit/sig
	 cu))))

  (define (teachpack-ok? x)
    (if (build-single-teachpack-unit x)
	#t
	#f))

  (define namespace-thunk void)
  (define init-namespace (lambda () (namespace-thunk)))

  (define (teachpack-changed v)
    (set! namespace-thunk (build-namespace-thunk v))))
