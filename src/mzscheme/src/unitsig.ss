
(invoke-unit
 (unit
  (import)
  ; The exports are meaningless, except that they allow shadowing
  (export unit/sig
	  invoke-unit/sig
	  define-signature
	  let-signature
          unit-with-signature
          compound-unit-with-signature
	  invoke-unit-with-signature
	  unit->unit-with-signature)
  
  ; Parse-time structs:
  (define-struct signature (name src global-src elems))
  (define-struct parse-unit (imports renames vars body))
  
  (define d-s 'define-signature)
  (define l-s 'let-signature)
  (define unit/sig 'unit/sig)
  (define u->u/sig 'unit->unit/sig)
  (define cpd-unit/sig 'compound-unit/sig)
  (define invoke-unit/sig 'invoke-unit/sig)

  (define inline-sig-name '<unnamed>)

  (define (result x) x)

  (define syntax-error
    (case-lambda 
     [(who expr msg sub)
      (raise-syntax-error who msg expr sub)]
     [(who expr msg)
      (raise-syntax-error who msg expr)]))

  (define undef-sig-error
    (lambda (who expr what)
      (syntax-error who expr 
		    (format "signature \"~s\" not defined" what))))

  (define not-a-sig-error
    (lambda (who expr what)
      (syntax-error who expr 
		    (format "\"~s\" is not a signature" what))))

  (define rename-signature
    (lambda (sig name)
      (make-signature name 
		      (signature-src sig) 
		      (signature-global-src sig) 
		      (signature-elems sig))))

  (define intern-signature
    (lambda (name desc global-name error)
      (make-signature
       name
       name
       global-name
       (if (vector? desc)
	   (map
	    (lambda (elem)
	      (cond
	       [(symbol? elem) elem]
	       [(and (pair? elem) (symbol? (car elem)))
		(intern-signature (car elem) (cdr elem) #f error)]
	       [else (error)]))
	    (vector->list desc))
	   (error)))))

  (define get-sig
    (lambda (who expr name sigid)
      (if (not (symbol? sigid))
	  (parse-signature who expr 
			   (if name
			       name
			       inline-sig-name)
			   sigid)
	  (let ([v (with-handlers
		    ([void (lambda (x) (undef-sig-error who expr sigid))])
		    (local-expansion-time-value sigid))]
		[gv (with-handlers
		     ([void (lambda (x) #f)])
		     (global-expansion-time-value sigid))])
	    (let ([s (intern-signature sigid v
				       (and (eq? v gv) sigid)
				       (lambda ()
					 (not-a-sig-error who expr sigid)))])
	      (if name
		  (rename-signature s name)
		  s))))))

  (define check-unique
    (lambda (names error-k)
      (cond
       [(null? names) (void)]
       [(memq (car names) (cdr names))
	(error-k (car names))]
       [else (check-unique (cdr names) error-k)])))

  (define build-struct-names
    (lambda (name fields omit-sel? omit-set?)
      (let ([name (symbol->string name)]
	    [fields (map symbol->string fields)]
	    [+ string-append])
	(map string->symbol
	     (append
	      (list 
	       (+ "make-" name)
	       (+ name "?")
	       (+ "struct:" name))
	      (if omit-sel?
		  null
		  (map
		   (lambda (f)
		     (+ name "-" f))
		   fields))
	      (if omit-set?
		  null
		  (map
		   (lambda (f)
		     (+ "set-" name "-" f "!"))
		   fields)))))))

  (define parse-signature
    (lambda (who expr name body)
      (let ([elems
	     (let loop ([body body])
	       (cond
		[(null? body) null]
		[(not (pair? body))
		 (syntax-error who expr "improper signature list form" body)]
		[else
		 (let ([first (car body)]
		       [k (lambda () (loop (cdr body)))])
		   (cond
		    [(and (symbol? first) (not (eq? first ':))) (cons first (k))]
		    [(or (not (pair? first))
			 (not (list? first))) 
		     (syntax-error who expr "improper signature clause form" first)]
		    [(eq? (car first) 'struct)
		     (let ([sdef (cdr first)]
			   [bad
			    (lambda (why)
			      (syntax-error who expr 
					    (format "improper `struct' clause form~a"
						    why)
					    first))])
		       (unless (and (<= 2 (length sdef))
				    (or (symbol? (car sdef))
					(bad ": struct name not an identifier"))
				    (list? (cadr sdef))
				    (andmap (lambda (s)
					      (or (symbol? s)
						  (bad ": field name not an identifier")))
					    (cadr sdef))
				    (or (= (length sdef) 2)
					(andmap
					 (lambda (omit)
					   (or (eq? omit '-selectors)
					       (eq? omit '-setters)
					       (and (pair? omit)
						    (eq? (car omit) '-)
						    (pair? (cdr omit))
						    (symbol? (cadr omit))
						    (null? (cddr omit)))
					       (bad ": bad omission specifier")))
					 (cddr sdef))))
			       (bad ""))
		       (let ([name (car sdef)]
			     [fields (cadr sdef)]
			     [omit-names
			      (let loop ([l (cddr sdef)])
				(cond
				 [(null? l) null]
				 [(pair? (car l)) (cons (cadar l) (loop (cdr l)))]
				 [else (loop (cdr l))]))]
			     [omit-setters? (memq '-setters (cddr sdef))]
			     [omit-selectors? (memq '-selectors (cddr sdef))])
			 (letrec ([names (build-struct-names 
					  name fields 
					  omit-selectors? omit-setters?)]
				  [filter
				   (lambda (names)
				     (cond
				      [(null? names) null]
				      [(memq (car names) omit-names) (filter (cdr names))]
				      [else (cons (car names) (filter (cdr names)))]))])
			   
			   (append (if (null? omit-names) names (filter names))
				   (k)))))]
		    [(eq? (car first) 'unit)
		     (let ([udef (cdr first)]
			   [bad
			    (lambda (why)
			      (syntax-error who expr 
					    (format "improper `unit' clause form~a"
						    why)
					    first))])
		       (unless (and (= 3 (length udef))
				    (or (symbol? (car udef))
					(bad ": unit name not an identifier"))
				    (eq? ': (cadr udef)))
			       (bad ""))
		       (let ([s (get-sig who expr (car udef) (caddr udef))])
			 (cons s (k))))]
		    [(eq? (car first) 'open)
		     (let ([odef (cdr first)])
		       (unless (and (pair? odef)
				    (null? (cdr odef)))
			       (syntax-error who expr 
					     "improper `open' clause form"
					     first))
		       (let ([s (get-sig who expr #f (car odef))])
			 (append (signature-elems s) (k))))]
		    [else
		     (syntax-error who expr "improper signature clause type" first)]))]))])
	(check-unique (map
		       (lambda (elem)
			 (if (symbol? elem)
			     elem
			     (signature-name elem)))
		       elems)
		      (lambda (name)
			(syntax-error who expr
				      (format "duplicate name \"~s\" in signature"
					      name))))
	  (make-signature name name #f (sort-signature-elems elems)))))

  (define explode-sig
    (lambda (sig)
      (list->vector
       (map 
	(lambda (v)
	  (if (symbol? v)
	      v
	      (cons
	       (signature-name v)
	       (explode-sig v))))
	(signature-elems sig)))))

  (define explode-named-sig
    (lambda (s)
      (cons
       (cond
	[(signature-name s)]
	[(signature-src s)]
	[else inline-sig-name])
       (explode-sig s))))

  (define explode-named-sigs
    (lambda (sigs)
      (map explode-named-sig sigs)))

  (define sort-signature-elems
    (lambda (elems)
      (letrec ([split
		(lambda (l f s)
		  (cond
		   [(null? l) (values f s)]
		   [(null? (cdr l)) (values (cons (car l) f) s)]
		   [else (split (cddr l) (cons (car l) f) (cons (cadr l) s))]))]
	       [merge
		(lambda (f s)
		  (cond
		   [(null? f) s]
		   [(null? s) f]
		   [(less-than? (car s) (car f))
		    (cons (car s) (merge f (cdr s)))]
		   [else
		    (cons (car f) (merge (cdr f) s))]))]
	       [less-than?
		(lambda (a b)
		  (if (symbol? (car a))
		      (if (symbol? (car b))
			  (string<? (cdr a) (cdr b))
			  #t)
		      (if (symbol? (car b))
			  #f
			  (string<? (cdr a) (cdr b)))))]
	       [pair
		(lambda (i)
		  (cons i (symbol->string (if (symbol? i) i (signature-name i)))))])
	(map car
	     (let loop ([elems (map pair elems)])
	       (cond
		[(null? elems) null]
		[(null? (cdr elems)) elems]
		[else (let-values ([(f s) (split elems null null)])
				  (merge (loop f) (loop s)))]))))))
		   
  (define flatten-signature
    (lambda (id sig)
      (apply
       append
       (map
	(lambda (elem)
	  (if (symbol? elem)
	      (list
	       (if id
		   (string->symbol (string-append id ":" (symbol->string elem)))
		   elem))
	      (flatten-signature (let* ([n (signature-name elem)]
					[s (if n
					       (symbol->string n)
					       #f)])
				   (if (and id s)
				       (string-append id ":" s)
				       (or id s)))
				 elem)))
	(signature-elems sig)))))

  (define flatten-signatures
    (lambda (sigs)
      (apply append (map (lambda (s) 
			   (let* ([name (signature-name s)]
				  [id (if name
					  (symbol->string name)
					  #f)])
			     (flatten-signature id s)))
			 sigs))))

  (define define-signature
    (lambda body
      (let ([expr (cons d-s body)])
	(unless (and (pair? body)
		     (or (symbol? (car body))
			 (syntax-error d-s expr "signature name is not an identifier"))
		     (pair? (cdr body))
		     (null? (cddr body)))
		(syntax-error d-s expr "improper form"))
	(let* ([name (car body)]
	       [sig (get-sig d-s expr name (cadr body))])
	  (result `(#%define-expansion-time ,name
		      (#%quote ,(explode-sig sig))))))))
  
  (define let-signature
    (lambda body
      (let ([expr (cons d-s body)])
	(unless (and (pair? body)
		     (or (symbol? (car body))
			 (syntax-error l-s expr "signature name is not an identifier"))
		     (pair? (cdr body))
		     (pair? (cddr body)))
		(syntax-error l-s expr "improper form"))
	(let* ([name (car body)]
	       [sig (get-sig l-s expr name (cadr body))])
	  (result `(#%let-expansion-time ,name
		     (#%quote ,(explode-sig sig))
		     ,@(cddr body)))))))
  
  (define signature-parts
    (lambda (q?)
      (lambda (sig)
	(let loop ([elems (signature-elems sig)])
	  (cond
	   [(null? elems) null]
	   [(q? (car elems)) (cons (car elems) (loop (cdr elems)))]
	   [else (loop (cdr elems))])))))
  (define signature-vars (signature-parts symbol?))
  (define signature-subsigs (signature-parts signature?))

  (define do-rename
    (lambda (export-name renames)
      (let loop ([renames renames])
	(cond
	 [(null? renames) export-name]
	 [(eq? (cadar renames) export-name)
	  (caar renames)]
	 [else (loop (cdr renames))]))))
      
  (define check-signature-unit-body
    (lambda (sig a-unit renames who expr)
      (let ([vars (parse-unit-vars a-unit)])
	(for-each
	 (lambda (var)
	   (let ([renamed (do-rename var renames)])
	     (unless (memq renamed vars)
		     (syntax-error who expr
				   (format 
				    "signature \"~s\" requires variable \"~s\"~a"
				    (signature-src sig)
				    var
				    (if (eq? var renamed)
					""
					(format " renamed \"~s\"" renamed)))))))
	 (signature-vars sig))
	(unless (null? (signature-subsigs sig))
		(syntax-error who expr
			      (format 
			       "signature \"~s\" requires sub-units"
			       (signature-src sig)))))))

  (define parse-imports
    (lambda (who untagged-legal? really-import? expr clause)
      (let ([bad
	     (lambda (why . rest)
	       (apply
		syntax-error who expr 
		(format (if really-import?
			    "bad `import' clause~a" 
			    "bad linkage specification~a")
			why)
		rest))])
	(unless (and (list? clause)
		     (andmap
		      (lambda (item)
			(or (and (symbol? item)
				 untagged-legal?)
			    (and
			     (or (list? item)
				 (bad ": bad item" item))
			     (or (and (or (null? item) 
					  (null? (cdr item))
					  (not (eq? ': (cadr item))))
				      untagged-legal?)
				 (and (= (length item) 3)
				      (eq? ': (cadr item)))
				 (bad ": bad item" item)))))
		      clause))
		(bad ""))
	(map
	 (lambda (item)
	   (if (or (symbol? item)
		   (not (and (= 3 (length item))
			     (eq? (cadr item) ':))))
	       (rename-signature (get-sig who expr #f item) #f)
	       (get-sig who expr (car item) (caddr item))))
	 clause))))

  (define parse-unit
    (lambda (expr body sig)
      (unless (and (pair? body)
		   (pair? (car body))
		   (eq? 'import (caar body)))
	      (syntax-error unit/sig expr 
			    "expected `import' clause"))
      (let* ([imports (parse-imports unit/sig #t #t expr (cdar body))]
	     [imported-names (flatten-signatures imports)]
	     [exported-names (flatten-signature #f sig)]
	     [body (cdr body)])
	(let-values ([(renames body)
		      (if (and (pair? body)
			       (pair? (car body))
			       (eq? 'rename (caar body)))
			  (values (cdr (car body)) (cdr body))
			  (values null body))])
	     (let ([bad
		    (lambda (why sub)
		      (syntax-error unit/sig expr 
				    (format "bad `rename' clause~a" why)
				    sub))])
	       (for-each
		(lambda (id)
		  (unless (and (pair? id) 
			       (pair? (cdr id)) 
			       (null? (cddr id))
			       (or (symbol? (car id))
				   (bad ": source is not an identifier" id))
			       (or (symbol? (cadr id))
				   (bad ": destination is not an identifier" id)))
			  (bad "" id)))
		renames))
	     (check-unique (map car renames)
			   (lambda (name)
			     (syntax-error unit/sig expr
					   (format "id \"~s\" renamed twice" name))))
	     (let* ([renamed-internals (map car renames)]
		    [swapped-renames (map (lambda (s) (cons (cadr s) (car s))) renames)]
		    [filtered-exported-names 
		     (if (null? renames)
			 exported-names
			 (let loop ([e exported-names])
			   (if (null? e)
			       e
			       (if (assoc (car e) swapped-renames)
				   (loop (cdr e))
				   (cons (car e) (loop (cdr e)))))))]
		    [local-vars (append renamed-internals filtered-exported-names imported-names)])
	       (let loop ([lines body][ports null][body null][vars null])
		 (cond
		  [(and (null? ports) (null? lines))
		   (make-parse-unit imports renames vars body)]
		  [(and (null? ports) (not (pair? lines)))
		   (syntax-error unit/sig expr "improper body list form")]
		  [else
		   (let-values ([(line line-kind) (local-expand-body-expression
						   (if (null? ports)
						       (car lines)
						       (read (car ports)))
						   local-vars)]
				[(rest-lines) (if (null? ports)
						  (cdr lines)
						  lines)])
		     (cond
		      [(and (eof-object? line) (not (null? ports)))
		       (values lines body vars)]
		      [(eq? line-kind '#%define-values)
		       (unless (and (pair? (cdr line))
				    (list? (cadr line))
				    (pair? (cddr line))
				    (null? (cdddr line))
				    (andmap symbol? (cadr line)))
			       (syntax-error unit/sig expr 
					     "improper `define-values' clause form"
					     line))
		       (let ([names (cadr line)])
			 (loop rest-lines
			       ports
			       (cons line body)
			       (append names vars)))]
		      [(eq? line-kind '#%begin)
		       (unless (list? (cdr line))
			       (syntax-error unit/sig expr 
					     "improper `begin' clause form"
					     line))
		       (loop (append (cdr line) rest-lines)
			     ports
			     body
			     vars)]
		      [(and (pair? line)
			    (eq? (car line) 'include))
		       (let ([line (local-expand-defmacro line)])
			 (unless (and (pair? (cdr line))
				      (string? (cadr line))
				      (null? (cddr line)))
				 (syntax-error unit/sig expr 
					       "improper `include' clause form"
					       line))
			    (let ([file (cadr line)])
			      (let-values ([(base name dir?) (split-path file)])
		                (when dir?
				      (syntax-error unit/sig expr 
						    (format "cannot include a directory ~s"
							    file)))
				(let* ([old-dir (current-load-relative-directory)]
				       [p (open-input-file (if (and old-dir (not (complete-path? file)))
							       (path->complete-path file old-dir)
							       file))])
				  (let-values ([(lines body vars)
						(parameterize ([current-load-relative-directory
								(if (string? base) 
								    (if (complete-path? base)
									base
									(path->complete-path base
											     (or old-dir 
												 (current-directory))))
								    (or old-dir
									(current-directory)))])
						   (dynamic-wind
						    void
						    (lambda ()
						      (loop rest-lines
							    (cons p ports)
							    body
							    vars))
						    (lambda ()
						      (close-input-port p))))])
				       (loop lines ports body vars))))))]
		      [else
		       (loop rest-lines
			     ports
			     (cons line body)
			     vars)]))])))))))
  
  
  (define unit-with-signature
    (lambda body
      (let ([expr (cons unit/sig body)])
	(unless (pair? body)
		(syntax-error unit/sig expr "improper form"))
	(let ([sig (get-sig unit/sig expr #f (car body))]
	      [rest (cdr body)])
	  (let ([a-unit (parse-unit expr rest sig)])
	    (check-signature-unit-body sig a-unit (parse-unit-renames a-unit) unit/sig expr)
	    (result
	     `(#%make-unit-with-signature
	       (#%unit
		(import ,@(flatten-signatures
			   (parse-unit-imports a-unit)))
		(export ,@(map
			   (lambda (name)
			     (list (do-rename name (parse-unit-renames a-unit))
				   name))
			   (signature-vars sig)))
		,@(reverse! (parse-unit-body a-unit)))
	       (#%quote ,(explode-named-sigs (parse-unit-imports a-unit)))
	       (#%quote ,(explode-sig sig)))))))))
  
  (define-struct link (name sig expr links))
  (define-struct sig-explode-pair (sigpart exploded))

  (define parse-compound-unit
    (lambda (expr body)
      (unless (and (pair? body)
		   (pair? (car body))
		   (eq? 'import (caar body)))
	      (syntax-error cpd-unit/sig expr 
			    "expected `import' clause"))
      (let* ([imports (parse-imports cpd-unit/sig #f #t expr (cdar body))]
	     [body (cdr body)])
	(unless (and (pair? body)
		     (pair? (car body))
		     (eq? 'link (caar body)))
		(syntax-error cpd-unit/sig expr 
			      "expectd `link' clause"))
	(unless (list? (car body))
		(syntax-error cpd-unit/sig expr 
			      "improper `link' clause form"))
	(let* ([bad
		(lambda (why sub)
		  (syntax-error cpd-unit/sig expr 
				(format "bad `link' element~a" why)
				sub))]
	       [links
		(map
		 (lambda (line)
		   (unless (and (pair? line)
				(or (symbol? (car line))
				    (bad ": link tag is not an identifier" line))
				(or (and (pair? (cdr line))
					 (eq? (cadr line) ':))
				    (bad ": expected \":\"" line))
				(or (pair? (cddr line))
				    (bad ": expected signature" line))
				(or (pair? (cdddr line))
				    (bad ": expected unit expression and links" line))
				(or (null? (cddddr line))
				    (bad ": extra expressions in sub-clause" line))
				(or (pair? (cadddr line))
				    (bad ": unit expression and links not a sub-clause" line))
				(or (list? (cdaddr (cdr line)))
				    (bad ": unit expression and link sub-clause" line)))
			   (bad "" line))
		   (make-link (car line) 
			      (get-sig cpd-unit/sig expr #f (caddr line))
			      (caaddr (cdr line))
			      (cdaddr (cdr line))))
		 (cdar body))]
	       [body (cdr body)]
	       [vars null]
	       [in-sigs imports]
	       [find-link
		(lambda (name links)
		  (let loop ([links links])
		    (cond
		     [(null? links) #f]
		     [(eq? name (link-name (car links)))
		      (car links)]
		     [else (loop (cdr links))])))]
	       [find-sig
		(lambda (name sigs)
		  (let loop ([sigs sigs])
		    (cond
		     [(null? sigs) #f]
		     [(and (signature? (car sigs))
			   (eq? name (signature-name (car sigs))))
		      (car sigs)]
		     [else (loop (cdr sigs))])))]
	       [flatten-path
		(lambda (clause path var-k unit-k)
		  (letrec ([check-sig
			    (lambda (sig use-sig)
			      (when use-sig
				    (with-handlers
				     ([exn:unit?
				       (lambda (exn)
					 (syntax-error 
					  cpd-unit/sig expr
					  (exn-message exn)))])
				     (verify-signature-match
				      cpd-unit/sig #f
				      (format "signature ~s" (signature-src use-sig))
				      (explode-sig use-sig)
				      (format "signature ~s" (signature-src sig))
				      (explode-sig sig)))))]
			   [flatten-subpath
			    (lambda (base last use-sig name sig p)
			      (cond
			       [(null? p) 
				(check-sig sig use-sig)
				(unit-k base last name (if use-sig
							   use-sig
							   sig))]
			       [(or (not (pair? p))
				    (not (symbol? (car p))))
				(syntax-error cpd-unit/sig expr
					      (format "bad `~a' path" clause)
					      path)]
			       [(memq (car p) (signature-vars sig))
				(if (and (null? (cdr p)) (not use-sig))
				    (let* ([id-nopath (car p)]
					   [id (if name
						   (string->symbol 
						    (string-append name
								   ":"
								   (symbol->string id-nopath)))
						   id-nopath)])
				      (var-k base id id-nopath))
				    (syntax-error cpd-unit/sig expr
						  (format 
						   "bad `~a' path: \"~a\" is a variable" 
						   clause
						   (car p))
						  path))]
			       [(find-sig (car p) (signature-elems sig))
				=>
				(lambda (s)
				  (flatten-subpath base
						   (car p)
						   use-sig
						   (let ([n (symbol->string 
							     (signature-name s))])
						     (if name
							 (string-append name ":" n)
							 n))
						   s
						   (cdr p)))]
			       [else
				(syntax-error cpd-unit/sig expr
					      (format 
					       "bad `~a' path: \"~a\" not found"
					       clause
					       (car p))
					      path)]))])
		    (let-values ([(p use-sig)
				  (cond
				   [(symbol? path)
				    (values (list path) #f)]
				   [(and (pair? path)
					 (symbol? (car path))
					 (pair? (cdr path))
					 (eq? (cadr path) ':)
					 (pair? (cddr path))
					 (null? (cdddr path)))
				    (values (list (car path))
					    (get-sig cpd-unit/sig expr
						     #f
						     (caddr path)))]
				   [(and (pair? path)
					 (list? (car path))
					 (not (null? (car path)))
					 (andmap
					  (lambda (s)
					    (and (symbol? s)
						 (not (eq? s ':))))
					  (car path))
					 (pair? (cdr path))
					 (eq? (cadr path) ':)
					 (pair? (cddr path))
					 (null? (cdddr path)))
				    (values (car path)
					    (get-sig cpd-unit/sig expr
						     #f
						     (caddr path)))]
				   [(and (pair? path)
					 (list? path)
					 (not (null? (car path)))
					 (andmap
					  (lambda (s)
					    (and (symbol? s)
						 (not (eq? s ':))))
					  path))
				    (values path #f)]
				   [else
				    (syntax-error cpd-unit/sig expr
						  (format 
						   "bad `~a' path"
						   clause)
						  path)])])
		      (cond
		       [(and (null? (cdr p))
			     (memq (car p) vars))
			(let ([id (car p)])
			  (var-k #f id id))]
		       [(find-link (car p) links)
			=> (lambda (link) 
			     (flatten-subpath (link-name link)
					      (car p)
					      use-sig
					      #f
					      (link-sig link)
					      (cdr p)))]
		       [(find-sig (car p) in-sigs)
			=> (lambda (sig) 
			     (let ([s (symbol->string (signature-name sig))])
			       (flatten-subpath #f
						(car p)
						use-sig
						s
						sig
						(cdr p))))]
		       [else
			(syntax-error cpd-unit/sig expr
				      (format 
				       "bad `~a' path: \"~a\" not found"
				       clause
				       (car p))
				      path)]))))])
	  (check-unique (map link-name links)
			(lambda (name)
			  (syntax-error cpd-unit/sig expr
					(format "duplicate sub-unit tag \"~s\"" name))))
	  (check-unique (append (map signature-name imports)
				(map link-name links))
			(lambda (name)
			  (syntax-error cpd-unit/sig expr
					(format 
					 "name \"~s\" is both import and sub-unit identifier" 
					 name))))
	  ;; Expand `link' clause using signatures
	  (for-each
	   (lambda (link)
	     (set-link-links! 
	      link
	      (map
	       (lambda (link)
		 (flatten-path 'link link
			       (lambda (base var var-nopath)
				 (make-sig-explode-pair
				  var
				  (list
				   (if base
				       (list base var)
				       var))))
			       (lambda (base last id sig)
				 (make-sig-explode-pair
				  (rename-signature sig last)
				  (if base
				      (list (cons base (flatten-signature id sig)))
				      (flatten-signature id sig))))))
	       (link-links link))))
	   links)
	  (unless (and (pair? body)
		       (pair? (car body))
		       (eq? 'export (caar body)))
		  (syntax-error cpd-unit/sig expr "expected `export' clause"))
	  (unless (list? (car body))
		  (syntax-error cpd-unit/sig expr 
				"bad `export' clause form"))
	  (unless (null? (cdr body))
		  (syntax-error cpd-unit/sig expr 
				"another clause follows `export' clause"))
	  (let* ([upath? (lambda (p)
			   (or (symbol? p)
			       (and (list? p)
				    (andmap symbol? p))))]
		 [spath? (lambda (p)
			   (or (and (list? p)
				    (= 3 (length p))
				    (eq? ': (cadr p))
				    (upath? (car p))
				    (or (symbol? (caddr p))
					(parse-signature cpd-unit/sig expr #f (caddr p))))
			       (upath? p)))]
		 [exports 
		  (map
		   (lambda (export)
		     (cond
		      [(or (not (list? export))
			   (not (<= 2 (length export) 3))
			   (not (or (null? (cddr export))
				    (and (pair? (cddr export))
					 (null? (cdddr export))))))
		       (syntax-error cpd-unit/sig expr "bad `export' sub-clause"
				     export)]
		      [else
		       (cond
			[(eq? (car export) 'open)
			 (let ([odef (cdr export)])
			   (unless (and (pair? odef)
					(spath? (car odef))
					(null? (cdr odef)))
				   (syntax-error cpd-unit/sig expr 
						 "bad `open' sub-clause of `export'"
						 export))
			   (flatten-path 'export
					 (car odef)
					 (lambda (base var var-nopath)
					   (syntax-error 
					    cpd-unit/sig expr 
					    "`open' sub-clause path is a variable"
					    (car export)))
					 (lambda (base last name sig)
					   (if base
					       (make-sig-explode-pair
						(signature-elems sig)
						(cons base 
						      (map
						       list
						       (flatten-signature name sig)
						       (flatten-signature #f sig))))
					       (syntax-error 
						cpd-unit/sig expr 
						"cannot export imported variables"
						export)))))]
			 [(eq? (car export) 'var)
			  (let ([vdef (cdr export)])
			    (unless (and (pair? vdef)
					 (pair? (car vdef))
					 (upath? (caar vdef))
					 (pair? (cdar vdef))
					 (null? (cddar vdef))
					 (symbol? (cadar vdef))
					 (or (null? (cdr vdef))
					     (and (pair? (cdr vdef))
						  (symbol? (cadr vdef))
						  (null? (cddr vdef)))))
				    (syntax-error cpd-unit/sig expr 
						  "bad `var' sub-clause of `export'"
						  export))
			    (flatten-path 'export
					  (let ([upath (caar vdef)]
						[vname (cadar vdef)])
					    (if (symbol? upath)
						(list upath vname)
						(append upath (list vname))))
					  (lambda (base var var-nopath)
					    (if base
						(make-sig-explode-pair
						 (list (if (null? (cdr vdef))
							   var-nopath
							   (cadr vdef)))
						 (list base
						       (if (null? (cdr vdef))
							   (list var var-nopath)
							   (list var (cadr vdef)))))
						(syntax-error 
						 cpd-unit/sig expr 
						 "cannot exported imported variables"
						 (car export))))
					  (lambda (base last name var)
					    (syntax-error 
					     cpd-unit/sig expr 
					     "`var' sub-clause path specifies a unit"
					     export))))]
			 [(eq? (car export) 'unit)
			  (let ([udef (cdr export)])
			    (unless (and (pair? udef)
					 (spath? (car udef))
					 (or (null? (cdr udef))
					     (and (pair? (cdr udef))
						  (symbol? (cadr udef))
						  (null? (cddr udef)))))
				    (syntax-error cpd-unit/sig expr 
						  "bad `unit' sub-clause of `export'"
						  export))
			    (flatten-path 'export
					  (car udef)
					  (lambda (base var var-nopath)
					    (syntax-error 
					     cpd-unit/sig expr 
					     "`unit' sub-clause path is a variable"
					     (car export)))
					  (lambda (base last name sig)
					    (if base
						(make-sig-explode-pair
						 (list (rename-signature
							sig
							(if (null? (cdr udef))
							    last
							    (cadr udef))))
						 (let ([flat (flatten-signature name sig)])
						   (cons base 
							 (map
							  list
							  flat
							  (flatten-signature 
							   (symbol->string (if (null? (cdr udef))
									       last
									       (cadr udef)))
							   sig)))))
						(syntax-error 
						 cpd-unit/sig expr 
						 "cannot exported imported variables"
						 export)))))]
			 [else
			  (syntax-error cpd-unit/sig expr 
					(format 
					 "bad `export' sub-clause")
					export)])]))
		   (cdar body))])
	    (check-unique (map
			   (lambda (s)
			     (if (signature? s)
				 (signature-name s)
				 s))
			   (apply
			    append
			    (map sig-explode-pair-sigpart exports)))
			  (lambda (name)
			    (syntax-error cpd-unit/sig expr
					  (format 
					   "the name \"~s\" is exported twice" 
					   name))))
	    `(#%let ,(map
		      (lambda (link)
			(list (link-name link)
			      (link-expr link)))
		      links)
	       (#%verify-linkage-signature-match
		(#%quote ,cpd-unit/sig)
		(#%quote ,(map link-name links))
		(#%list ,@(map link-name links))
		(#%quote ,(map (lambda (link) (explode-sig (link-sig link))) links))
	        (#%quote ,(map
			   (lambda (link)
			     (map (lambda (sep)
				    (explode-named-sig (sig-explode-pair-sigpart sep)))
				  (link-links link)))
			   links)))
	       ; All checks done. Make the unit:
	       (#%make-unit-with-signature
		(#%compound-unit
		 (import ,@(flatten-signatures
			    imports))
		 (link ,@(map
			  (lambda (link)
			    (list (link-name link)
				  (cons `(#%unit-with-signature-unit
					  ,(link-name link))
					(apply
					 append
					 (map 
					  sig-explode-pair-exploded 
					  (link-links link))))))
			  links))
		 (export ,@(map sig-explode-pair-exploded exports)))
	        (#%quote ,(explode-named-sigs imports))
	        (#%quote ,(explode-sig
			   (make-signature
			    'dummy
			    'dummy
			    #f
			    (apply
			     append
			     (map sig-explode-pair-sigpart exports))))))))))))
			      
  (define compound-unit-with-signature
    (lambda body
      (let ([expr (cons cpd-unit/sig body)])
	(result (parse-compound-unit expr body)))))

  (define parse-invoke-vars
    (lambda (who rest expr)
      (parse-imports who #t #f expr rest)))

  (define build-invoke-unit
    (lambda (who invoke-unit u sigs nsl)
      (result `(let ([u ,u])
		 (#%verify-linkage-signature-match
		  (#%quote ,who)
		  (#%quote (invoke))
		  (#%list u)
		  (#%quote (#()))
		  (#%quote (,(explode-named-sigs sigs))))
		 (,invoke-unit (#%unit-with-signature-unit u)
			       ,@nsl
			       ,@(flatten-signatures
				  sigs))))))
      

  (define invoke-unit-with-signature
    (lambda body
      (let ([expr (cons invoke-unit/sig body)])
	(unless (and (pair? body)
		     (list? (cdr body)))
		(syntax-error invoke-unit/sig expr "improper form"))
	(let ([u (car body)]
	      [sigs (parse-invoke-vars invoke-unit/sig (cdr body) expr)])
	  (build-invoke-unit invoke-unit/sig '#%invoke-unit u sigs null)))))
  
  (define unit->unit-with-signature
    (lambda body
      (let ([expr (cons u->u/sig body)])
	(unless (and (pair? body)
		     (pair? (cdr body))
		     (list? (cadr body))
		     (pair? (cddr body))
		     (null? (cdddr body)))
		(syntax-error u->u/sig expr "improper form"))
	(let ([e (car body)]
	      [im-sigs (map (lambda (sig)
			      (get-sig u->u/sig expr #f sig))
			    (cadr body))]
	      [ex-sig (get-sig u->u/sig expr #f (caddr body))])
	  `(#%make-unit-with-signature
	    ,e
	    (#%quote ,(explode-named-sigs im-sigs))
	    (#%quote ,(explode-sig ex-sig)))))))

  (vector define-signature
	  let-signature
          unit-with-signature
          compound-unit-with-signature
	  invoke-unit-with-signature
	  unit->unit-with-signature)))

> stop unitsig <
