
; For shadowing (during compile):
(let ([define-values/invoke-unit void]
      [define-values/invoke-unit/sig void]
      [global-define-values/invoke-unit void]
      [global-define-values/invoke-unit/sig void])
  (unit
    (import)
    (export)

    (define (extract-signature sig badsyntax)
      ; A cheesy way of expanding the saignature: use the compound-unit/sig
      ;  macro implementation. Construct an expression, expand it, and
      ;  then pull the result back apart.
      (with-handlers ([(lambda (x) (not (exn:misc:user-break? x)))
		       (lambda (x) (badsyntax sig "bad signature"))])
	(let ([expr (local-expand-defmacro `(#%compound-unit/sig
					     (import)
					     (link [A : ,sig (0)]
						   [B : () (0 A)])
					     (export)))]
	      [find-expr (lambda (l v)
			   (let loop ([l l][queue null])
			     (cond
			      [(and (pair? l) (eq? (car l) v))
			       l]
			      [(pair? l)
			       ; breadth first:
			       (loop (cdr l) (cons (car l) queue))]
			      [(pair? queue)
			       (loop (car queue) (cdr queue))]
			      [else #f])))])
	  (let* ([verify (find-expr expr '#%verify-linkage-signature-match)]
		 [exploded (car (cadr (list-ref verify 4)))]
		 [link (find-expr (find-expr expr '#%make-unit-with-signature)
				  'link)]
		 [b-line (caddr link)]
		 [a-import (cadr (cadr b-line))]
		 [flattened (cdr a-import)])
	    (values exploded flattened)))))

    (define (extract-named-signature sig badsyntax)
      (let-values ([(prefix sig) (if (and (list? sig)
					  (= 3 (length sig))
					  (eq? (cadr sig) ':))
				     (values (car sig) (caddr sig))
				     (values #f sig))])
	(let-values ([(exploded flattened) (extract-signature sig badsyntax)])
	  (if prefix
	      (values (cons prefix exploded)
		      (let ([p (string-append (symbol->string prefix) ":")])
			(map (lambda (s)
			       (string->symbol
				(string-append
				 p
				 (symbol->string s))))
			     flattened)))
	      (values (cons 'name exploded) flattened)))))

    (define (do-define-values/invoke-unit global? exports unit prefix imports)
      (let* ([badsyntax (lambda (s why)
			  (raise-syntax-error
			   (if global? 
			       'global-define-values/invoke-unit
			       'define-values/invoke-unit)
			   (format "bad syntax (~a)" why)
			   `(,(if global? 
				  'global-define-values/invoke-unit
				  'define-values/invoke-unit)
			     ,exports
			     ,unit ,prefix ,@imports)
			   s))]
	     [symcheck (lambda (s)
			 (or (symbol? s)
			     (badsyntax s "not an identifier")))])
	(unless (list? exports)
	  (badsyntax exports "not a sequence of identifiers"))
	(for-each symcheck exports)
	(when prefix
	  (unless (symbol? prefix)
	    (badsyntax prefix "prefix is not an identifier")))
	(for-each symcheck imports)
	
	(let* ([tagged-exports (if prefix
				   (let ([prefix (string-append
						  (symbol->string prefix)
						  ":")])
				     (map (lambda (s)
					    (string->symbol
					     (string-append
					      prefix
					      (symbol->string s))))
					  exports))
				   exports)]
	       [extract-unit `(#%unit 
				(import ,@exports)
				(export)
				(#%values ,@exports))]
	       [invoke-unit
		`(#%invoke-unit
		  (#%compound-unit
		   (import ,@imports)
		   (link [unit-to-invoke (,unit ,@imports)]
			 [export-extractor (,extract-unit (unit-to-invoke ,@exports))])
		   (export))
		  ,@imports)])
	  (if global?
	      `(#%let-values ([,tagged-exports ,invoke-unit])
		 ,@(map
		    (lambda (x)
		      `(#%global-defined-value ',x ,x))
		    tagged-exports)
                  (#%void))
	      `(#%define-values ,tagged-exports ,invoke-unit)))))
    
    (define define-values/invoke-unit
      (case-lambda 
       [(exports unit name . imports) (do-define-values/invoke-unit #f exports unit name imports)]
       [(exports unit) (do-define-values/invoke-unit #f exports unit #f null)]))
    
    (define global-define-values/invoke-unit
      (case-lambda 
       [(exports unit name . imports) (do-define-values/invoke-unit #t exports unit name imports)]
       [(exports unit) (do-define-values/invoke-unit #t exports unit #f null)]))
    
    (define (do-define-values/invoke-unit/sig global? signame unit prefix imports)
      (let* ([formname (if global?
			   'global-define-values/invoke-unit/sig
			   'define-values/invoke-unit/sig)]
	     [badsyntax (lambda (s why)
			  (raise-syntax-error
			   formname
			   (format "bad syntax (~a)" why)
			   `(,formname
			     ,signame ,unit ,prefix ,@imports)
			   s))]
	     [unit-var (gensym)])
	(let-values ([(ex-exploded ex-flattened) (extract-signature signame badsyntax)]
		     [(im-explodeds im-flatteneds)
		      (let loop ([l imports][el null][fl null])
			(if (null? l)
			    (values (reverse! el) (reverse! fl))
			    (let-values ([(e f) (extract-named-signature (car l) badsyntax)])
			      (loop (cdr l) (cons e el) (cons f fl)))))])
	  `(,(if global?
		 'global-define-values/invoke-unit 
		 'define-values/invoke-unit)
	    ,ex-flattened
	    (let ([,unit-var ,unit])
	      (#%verify-linkage-signature-match
	       ',formname
	       '(invoke)
	       (#%list ,unit-var)
	       '(,ex-exploded)
	       '(,im-explodeds))
	      (#%unit/sig->unit ,unit-var))
	    ,(if (or (eq? prefix #f)
		     (symbol? prefix))
		 prefix
		 (badsyntax prefix "prefix is not #f or a symbol"))
	    ,@(apply append im-flatteneds)))))
    
    (define define-values/invoke-unit/sig
      (case-lambda 
       [(signame unit prefix . imports)
	(do-define-values/invoke-unit/sig #f signame unit prefix imports)]
       [(signame unit)
	(do-define-values/invoke-unit/sig #f signame unit #f null)]))

    (define global-define-values/invoke-unit/sig
      (case-lambda 
       [(signame unit prefix . imports)
	(do-define-values/invoke-unit/sig #t signame unit prefix imports)]
       [(signame unit)
	(do-define-values/invoke-unit/sig #t signame unit #f null)]))

    (values define-values/invoke-unit
	    define-values/invoke-unit/sig
	    global-define-values/invoke-unit
	    global-define-values/invoke-unit/sig)))
