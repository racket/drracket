
(unit
 (import)
 (export send*
	 local
	 recur
	 rec
	 evcase
	 nor
	 nand
	 signature->symbols)
 
 (define send*
  (lambda (x . rest)
    (let ([g (gensym "send*")])
      `(let ([,g ,x])
	 ,@(map (lambda (x) `(send ,g ,@x))
		rest)))))

 ;; Another let-like form.
 (define local 
  (lambda (defines expr1 . body)
    (unless (list? defines)
	    (raise-syntax-error
		       'local
		       "bad definition sequence"
		       (list* 'local defines expr1 body)
		       defines))
    (let* ([symilist? (lambda (l)
			 (let loop ([l l])
			   (or (null? l)
			       (symbol? l)
			       (and (pair? l)
				    (symbol? (car l))
				    (loop (cdr l))))))]
	   [defs 
	     (map
	      (lambda (def)
		(unless (and (list? def)
			     (pair? def)
			     (case (car def)
			       [(#%define-values define-values)
				(and (= 3 (length def))
				     (list? (cadr def))
				     (andmap symbol? (cadr def))
				     (let-values ([(d kind) (local-expand-body-expression `(,(car def) (,(gensym)) 1))])
                                       (eq? kind '#%define-values)))]
			       [(#%define define)
				(and (or (and (= 3 (length def))
					      (symbol? (cadr def)))
					 (and (<= 3 (length def))
					      (pair? (cadr def))
					      (symilist? (cadr def))))
				     (let-values ([(d kind) (local-expand-body-expression `(,(car def) ,(gensym) 1))])
                                       (eq? kind '#%define-values)))]
			       [(#%define-struct define-struct)
				(and (= 3 (length def))
				     (or (symbol? (cadr def))
					 (and (list? (cadr def))
					      (= 2 (length (cadr def)))
					      (symbol? (caadr def))))
				     (list? (caddr def))
				     (andmap symbol? (caddr def))
				     (let-values ([(d kind) (local-expand-body-expression `(,(car def) ,(gensym) ()))])
                                       (eq? kind '#%define-values)))]
			       [else #f]))
		   (raise-syntax-error
		    'local
		    "bad definition"
		    (list* 'local defines expr1 body)
		    def))
		(case (car def)
		  [(#%define-values define-values) (cadr def)]
		  [(#%define define) (list (if (symbol? (cadr def))
					       (cadr def)
					       (caadr def)))]
		  [else (let ([s `(#%define-struct
				   ,(if (symbol? (cadr def))
					(cadr def)
					(caadr def))
				   ,(caddr def))])
			  (cadr (expand-defmacro s)))]))
	      defines)]
	   [defined-names (apply append defs)])
      ; We wrap everything in an extra `let' to permit the shadowing
      ;  of syntax by local definitions:
      `(let ,(map (lambda (n) `(,n (#%void))) defined-names)
	 ; We have to use #% forms for define, now:
	 ,@(map (lambda (def)
		  (case (car def)
		    [(#%define-values define-values)
		     (cons '#%define-values (cdr def))]
		    [(#%define define)
		     (cons '#%define (cdr def))]
		    [(#%define-struct define-struct)
		     (cons '#%define-struct (cdr def))]))
		defines)
	 ; Another let, in case there were more embedded defines:
	 (let () ,expr1 ,@body)))))
 
 ;; recur is another name for 'let' in a named let
 (define recur 
  (lambda (name args . body) `(let ,name ,args ,@body)))

 ;; define a recursive value
 (define rec
  (lambda (x rest)
    (if (symbol? x)
	`(letrec ([,x ,rest])
	   ,x)
	(raise-syntax-error 'rec "identifier must be a symbol" 
			    (list 'rec x rest) x))))

 (define evcase 
   (lambda (v . tests-in)
     (let ([serror
	    (lambda (msg at)
	      (raise-syntax-error
	       'evcase
	       msg
	       (list* 'evcase v tests-in)
	       at))])
       (let ([gen (gensym "evcase")])
	 `(let ([,gen ,v])
	    (cond
	     ,@(let loop ([tests tests-in])
		 (cond
		  [(null? tests) `()]
		  [(pair? tests)
		   (let ([test-value (car tests)]
			 [rest (cdr tests)])
		     `(,(if (or (not (pair? test-value))
				(not (pair? (cdr test-value))))
			    (serror
			     "bad syntax (clause is not a test-value pair)"
			     test-value)
			    (let ([test (car test-value)]
				  [body (cdr test-value)])
			      (if (and (pair? body) (list? body))
				  #t
				  (serror
				   "bad syntax (improper clause body)"
				   body))
			      (let ([condition 
				     (cond
				      [(and (eq? test 'else)
					    (not (local-expansion-time-bound? 'else)))
				       (if (null? rest)
					   'else
					   (serror
					    "bad syntax (`else' clause must be last)"
					    test-value))]
				      [else `(eqv? ,gen ,test)])])
				`(,condition
				  (begin ,@body)))))
		       .
		       ,(loop rest)))]
		  [else (serror
			 "bad syntax (body must contain a list of pairs)"
			 tests)]))))))))

 (define nor (lambda args `(#%not (#%or ,@args))))
 (define nand (lambda args `(#%not (#%and ,@args))))

 (define signature->symbols
   (lambda (name)
     (unless (symbol? name)
	     (raise-syntax-error 'signature->symbols
				 "not an identifier"
				 (list 'signature->symbols name)
				 name))
     (let ([v (global-expansion-time-value name)])
       (letrec ([sig? (lambda (v)
			(and (vector? v)
			     (andmap
			      (lambda (s)
				(or (and (pair? s)
					 (symbol? (car s))
					 (sig? (cdr s)))
				    (symbol? s)))
			      (vector->list v))))])
	 (unless (sig? v)
		 (raise-syntax-error 'signature->symbols
				     "expansion-time value is not a signature"
				     (list 'signature->symbols name)
				     name))
	 v)))))
