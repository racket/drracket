(#%define-macro #%quasiquote
  (#%lambda (form)
    (#%let-values
      (((normal)
        (#%lambda (x old)
          (#%if (#%eq? x old)
            (#%if (#%null? x) x (#%list (#%quote #%quote) x))
            x))))
      (normal
        (#%letrec*-values
          (((qq)
            (#%lambda (x level)
              (#%let-values
                (((qq-list)
                  (#%lambda (x level)
                    (#%let-values
                      (((old-first) (#%car x)))
                      (#%let-values
                        (((old-second) (#%cdr x)))
                        (#%let-values
                          (((first) (qq old-first level)))
                          (#%let-values
                            (((second) (qq old-second level)))
                            (#%let-values
                              ()
                              (#%if (#%if (#%eq? first old-first)
                                      (#%eq? second old-second)
                                      #f)
                                x
                                (#%list
                                  (#%quote #%cons)
                                  (normal first old-first)
                                  (normal second old-second)))))))))))
                (#%if (#%pair? x)
                  (#%let-values
                    (((first) (#%car x)))
                    (#%if (#%if (#%eq? first (#%quote unquote))
                            (#%list? x)
                            #f)
                      (#%let-values
                        (((rest) (#%cdr x)))
                        (#%if (#%let-values
                                (((g35) (#%not (#%pair? rest))))
                                (#%if g35 g35 (#%not (#%null? (#%cdr rest)))))
                          (#%raise-syntax-error
                            (#%quote unquote)
                            "takes exactly one expression"
                            (#%list (#%quote quasiquote) form)))
                        (#%if (#%zero? level)
                          (#%car rest)
                          (qq-list x (#%sub1 level))))
                      (#%if (#%if (#%eq? first (#%quote quasiquote))
                              (#%list? x)
                              #f)
                        (qq-list x (#%add1 level))
                        (#%if (#%if (#%eq? first (#%quote unquote-splicing))
                                (#%list? x)
                                #f)
                          (#%raise-syntax-error
                            (#%quote unquote-splicing)
                            "invalid context within quasiquote"
                            (#%list (#%quote quasiquote) form))
                          (#%if (#%if (#%pair? first)
                                  (#%if (#%eq? (#%car first)
                                               (#%quote unquote-splicing))
                                    (#%list? first)
                                    #f)
                                  #f)
                            (#%let-values
                              (((rest) (#%cdr first)))
                              (#%if (#%let-values
                                      (((g34) (#%not (#%pair? rest))))
                                      (#%if g34
                                        g34
                                        (#%not (#%null? (#%cdr rest)))))
                                (#%raise-syntax-error
                                  (#%quote unquote-splicing)
                                  "takes exactly one expression"
                                  (#%list (#%quote quasiquote) form)))
                              (#%let-values
                                (((uqsd) (#%car rest))
                                 ((old-l) (#%cdr x))
                                 ((l) (qq (#%cdr x) level)))
                                (#%if (#%zero? level)
                                  (#%let-values
                                    (((l) (normal l old-l)))
                                    (#%let-values
                                      ()
                                      (#%list (#%quote #%append) uqsd l)))
                                  (#%let-values
                                    (((restx) (qq-list rest (#%sub1 level))))
                                    (#%let-values
                                      ()
                                      (#%if (#%if (#%eq? l old-l)
                                              (#%eq? restx rest)
                                              #f)
                                        x
                                        (#%list
                                          (#%quote #%cons)
                                          (#%list
                                            (#%quote #%cons)
                                            (#%list
                                              (#%quote #%quote)
                                              (#%quote unquote-splicing))
                                            (normal restx rest))
                                          (normal l old-l))))))))
                            (qq-list x level))))))
                  (#%if (#%vector? x)
                    (#%let-values
                      (((l) (#%vector->list x)))
                      (#%let-values
                        (((l2) (qq l level)))
                        (#%let-values
                          ()
                          (#%if (#%eq? l l2)
                            x
                            (#%list (#%quote #%list->vector) l2)))))
                    (#%if (#%box? x)
                      (#%let-values
                        (((v) (#%unbox x)))
                        (#%let-values
                          (((qv) (qq v level)))
                          (#%let-values
                            ()
                            (#%if (#%eq? v qv)
                              x
                              (#%list (#%quote #%box) qv)))))
                      x)))))))
          (qq form 0))
        form))))

> qstop quasiquote <

(#%define-macro #%and 
  (#%case-lambda
   [() #t]
   [(expr) expr]
   [(expr . exprs)
    `(#%if ,expr
	 (#%and ,@exprs)
	 #f)]))

> kstop and <

(#%define-macro #%or
  (#%case-lambda 
   [() #f]
   [(expr) expr]
   [(expr . exprs)
    (#%let ([temp (#%gensym)])
      `(#%let ([,temp ,expr])
	 (#%if ,temp
	       ,temp
	       (#%or ,@exprs))))]))

> kstop or <

(#%define-macro #%cond
    (#%lambda form
     (#%let ([serror
	      (#%lambda (msg at)
		(#%raise-syntax-error 'cond msg (#%cons 'cond form) at))])
      (#%let loop ([tests form])
	(#%if (#%null? tests)
	    (#%if (#%compile-allow-cond-fallthrough)
		'(#%void)
		'(#%cond))
	    (#%if (#%not (#%pair? tests))
		(serror
		 "bad syntax (body must contain a list of pairs)"
		 tests)
		(#%let ([line (#%car tests)]
		        [rest (#%cdr tests)])
		  (#%if (#%not (#%pair? line))
		        (serror
			 "bad syntax (clause is not a test-value pair)"
			 line)
		      (#%let* ([test (#%car line)]
			       [value (#%cdr line)]
			       [else? (#%and (#%eq? test 'else) 
					     (#%not (#%local-expansion-time-bound? 'else)))])
			(#%if (#%and else? (#%pair? rest))
			      (serror "bad syntax (`else' clause must be last)" line))
			(#%if (#%and (#%pair? value)
				     (#%eq? '=> (#%car value))
				     (#%not (#%local-expansion-time-bound? '=>)))
			    (#%if (#%and (#%pair? (#%cdr value))
					 (#%null? (#%cddr value)))
				(#%let ([test (#%if else?
						    #t 
						    test)]
					[gen (#%gensym)])
				  `(#%let ([,gen ,test])
				     (#%if ,gen
					 (,(#%cadr value) ,gen)
					 ,(loop rest))))
				(serror
				 "bad syntax (bad clause form with =>)"
				 line))
			    (#%if else?
				(#%cons '#%begin value)
				(#%if (#%null? value)
				    (#%let ([gen (#%gensym)])
				      `(#%let ([,gen ,test])
					 (#%if ,gen ,gen ,(loop rest))))
				    (#%list
				     '#%if test
				     (#%cons '#%begin value)
				     (loop rest))))))))))))))

> cstop cond <

(#%define-macro #%case 
    (#%lambda (v . tests-in)
     (#%let ([serror
	      (#%lambda (msg at)
		(#%raise-syntax-error
		 'case
		 msg
		 (#%list* 'case v tests-in)
		 at))])
      (#%let ([gen (#%gensym)])
	`(#%let ([,gen ,v])
	   (#%cond
	    ,@(#%let loop ([tests tests-in])
		(#%cond
		 [(#%null? tests) `()]
		 [(#%pair? tests)
		  (#%let ([test-value (#%car tests)]
			  [rest (#%cdr tests)])
		    (#%cons
		     (#%if (#%or (#%not (#%pair? test-value))
				 (#%not (#%pair? (#%cdr test-value))))
			 (serror
			  "bad syntax (clause is not a test-value pair)"
			  test-value)
			 (#%let ([test (#%car test-value)]
				 [value (#%cdr test-value)])
			   (#%if (#%and (#%pair? value) (#%list? value))
				 #t
				 (serror
				  "bad syntax (improper clause body)"
				  value))
			   (#%cons
			    (#%cond
			     [(#%and (#%eq? test 'else)
				     (#%not (#%local-expansion-time-bound? 'else)))
			      (#%if (#%null? rest)
				    'else
				    (serror
				     "bad syntax (`else' clause must be last)"
				     test-value))]
			     [else
			      (#%if (#%not (#%list? test))
				    (serror
				     "bad syntax (improper datum list)"
				     test))
			      (#%cons 
			       '#%or
			       (#%let loop ([test test])
				 (#%cond
				  [(#%null? test) ()]
				  [else
				   (#%let ([t (#%list '#%quote (#%car test))])
				     (#%cons `(#%eqv? ,gen ,t)
					     (loop (#%cdr test))))])))])
			    `((#%begin ,@value)))))
		     (loop rest)))]
		 [else (serror
			"bad syntax (body must contain a list of pairs)"
			tests)]))))))))

> kstop case <

(#%define-macro #%do 
    (#%lambda (bindings test-and-result . body)
      (#%let ([code (#%list* 'do bindings test-and-result body)])
	(#%if (#%list? bindings)
	      (#%void)
	      (#%raise-syntax-error 'do "bad syntax (bad binding sequence)" code))
	(#%map (#%lambda (binding)
		 (#%if (#%and (#%list? binding)
			      (#%let ([l (#%length binding)])
			        (#%or (#%= l 2) (#%= l 3))))
		     (#%void)
		     (#%raise-syntax-error 'do "bad syntax (bad binding)" code)))
	       bindings)
	(#%if (#%list? test-and-result) (#%void)
	      (#%raise-syntax-error 'do "bad syntax (bad test and result)" code))
	(#%if (#%list? body) (#%void)
	      (#%raise-syntax-error 'do "bad syntax (improper body)" code))
	(#%let* ([vars (#%map (#%lambda (binding)
				(#%let ([var (#%car binding)])
				  (#%if (#%symbol? var)
				      var
				      (#%raise-syntax-error
				       'do "bad syntax (variable must be an identifier)"
				       code))))
			      bindings)]
		 [inits (#%map #%cadr bindings)]
		 [steps (#%map (#%lambda (binding)
				 (#%cond
				  [(#%pair? (#%cddr binding))
				   (#%caddr binding)]
				  [else (#%car binding)]))
			       bindings)]
		 [test (#%if (#%pair? test-and-result)
			   (#%car test-and-result)
			   (#%raise-syntax-error
			    'do "bad syntax (test/result part)"
			    code))]
	       [results (#%if (#%null? (#%cdr test-and-result))
			    (#%list '(#%void))
			    (#%cdr test-and-result))]
	       [loop (#%gensym)])
	  `(#%let ,loop ,(#%map (#%lambda (var init)
				(#%list var init))
			        vars inits)
		(#%if ,test
		    (#%begin
		      ,@results)
		    (#%begin
		      ,@body
		      (,loop ,@steps))))))))

> kstop do <

(#%define-macro #%define
     (#%lambda body
       (#%let ([code (#%cons 'define body)])
	 (#%if (#%null? body)
	       (#%raise-syntax-error
		'define
		"bad syntax (no definition body)"
		code))
	 (#%let ([first (#%car body)]) 
	  (#%cond
	   [(#%symbol? first)
	    (#%if (#%and (#%pair? (#%cdr body))
			 (#%null? (#%cddr body)))
		  `(#%define-values (,first) ,@(#%cdr body))
		  (#%raise-syntax-error
		   'define
		   "bad syntax (zero or multiple expressions after identifier)"
		   code))]
	   [(#%pair? first)
	    (#%let ([bad-symbol  (#%lambda (s) (#%raise-syntax-error 'define
								     "bad identifier"
								     code
								     s))])
		   (#%let loop ([l first])
			  (#%cond
			   [(#%null? l) #f]
			   [(#%pair? l) 
			    (#%if (#%symbol? (#%car l))
				  (loop (#%cdr l))
				  (bad-symbol (#%car l)))]
			   [(#%symbol? l) #f]
			   [else (bad-symbol l)])))
	    
	    `(#%define-values (,(#%car first)) (#%lambda ,(#%cdr first) ,@(#%cdr body)))]
	   [else
	    (#%raise-syntax-error
	     'define
	     "not an identifier"
	     code
	     first)])))))

> kstop define <

(#%define-macro #%set!-values
  (#%lambda (args expr)
    (#%and (#%or (#%list? args)
		 (#%raise-syntax-error
			       'set!-values
			       "bad syntax (illegal use of `.')"
			       (#%list 'set!-values args expr)
			       args))
	   (#%andmap (#%lambda (arg)
			(#%or (#%symbol? arg)
			      (#%raise-syntax-error
			       'set!-values
			       "bad syntax (not an identifier)"
			       (#%list 'set!-values args expr)
			       arg)))
		     args))
    (#%if (#%and (#%pair? args) (#%null? (#%cdr args)))
	  `(#%set! ,(#%car args) ,expr)
	  (#%let ([gens (#%map (#%lambda (arg) (#%gensym)) args)])
	     `(#%let-values ([,gens ,expr])
		 ,@(#%if (#%null? args)
			 `((#%void))
			 (#%map (#%lambda (arg gen) `(#%set! ,arg ,gen)) args gens)))))))

> kstop set!-values <

(#%define-macro #%delay
  (#%lambda (body)
    `(#%make-promise (#%lambda () ,body))))

> kstop delay <

(#%define-macro #%let/cc 
  (#%lambda (var expr1 . body)
    `(#%call/cc (#%lambda (,var) ,expr1 ,@body))))

> kstop let/cc <

(#%define-macro #%let/ec 
  (#%lambda (var expr1 . body)
    `(#%call/ec (#%lambda (,var) ,expr1 ,@body))))

> kstop let/ec <

(#%define-macro #%when 
  (#%lambda (test expr1 . body)
    `(#%if ,test
	   (#%begin
	    ,expr1
	    ,@body))))

> kstop when <

(#%define-macro #%unless 
  (#%lambda (test expr1 . body)
    `(#%if ,test
	   (#%void)
	   (#%begin
	    ,expr1
	    ,@body))))

> kstop unless <

(#%define-macro #%define-struct
  (#%lambda body
     (#%let ([syntax-error
	      (#%lambda (s . detail)
		(#%apply
		 #%raise-syntax-error
		 'define-struct
		 s
		 (#%cons 'define-struct body)
		 detail))]
	     [build-struct-names
	      (#%lambda (name fields)
		(#%let ([name (#%symbol->string name)]
			[fields (#%map #%symbol->string fields)]
			[+ #%string-append])
		  (#%map #%string->symbol
			 (#%append
			  (#%list 
			   (+ "struct:" name)
			   (+ "make-" name)
			   (+ name "?"))
			  (#%apply
			   #%append
			   (#%map
			    (#%lambda (f) 
			       (#%list 
				(+ name "-" f)
				(+ "set-" name "-" f "!")))
			   fields))))))])
	    (#%or (#%pair? body)
		  (syntax-error "empty declaration"))
	    (#%or (#%= 2 (#%length body))
		  (syntax-error "wrong number of parts"))
	    (#%or (#%symbol? (#%car body))
		  (#%and (#%pair? (#%car body))
			 (#%symbol? (#%caar body))
			 (#%pair? (#%cdar body))
			 (#%null? (#%cddar body)))
		  (syntax-error "first part must be an identifier or identifier-expression pair"))
	    (#%or (#%list? (#%cadr body))
		  (syntax-error "illegal use of `.' in field list"))
	    (#%for-each (#%lambda (x) 
			  (#%or (#%symbol? x)
				(syntax-error "field name not a identifier" x)))
		      (#%cadr body))
	    (#%let ([name (#%if (#%symbol? (#%car body))
			      (#%car body)
			      (#%caar body))]
		  [fields (#%cadr body)])
	      `(#%define-values ,(build-struct-names name fields) (#%struct ,@body))))))

> kstop define-struct <

(#%define-macro #%let-struct
  (#%lambda (base field . body)
    `(#%let () (#%define-struct ,base ,field) ,@body)))

> kstop let-struct <

> literal "#ifndef NO_OBJECT_SYSTEM"

(#%define-macro #%class 
  (#%lambda (super args . rest)
    `(#%class*/names (this super-init) ,super () ,args ,@rest)))

> kstop class <

(#%define-macro #%class* 
  (#%lambda (super interfaces args . rest)
    `(#%class*/names (this super-init) ,super ,interfaces ,args ,@rest)))

> kstop class* <

(#%define-macro #%send 
  (#%lambda (obj msg . params)
    `((#%ivar ,obj ,msg) ,@params)))

> kstop send <

(#%define-macro #%make-generic 
  (#%lambda (c% name)
    `(#%uq-make-generic ,c% (quote ,name))))

> kstop make-generic <

(#%define-macro #%ivar 
    (#%lambda args
      (#%if (#%list? args)
	  (#%let ([l (#%length args)])
	    (#%cond
	     [(#%= l 2) `(#%uq-ivar ,(#%car args) (#%quote ,(#%cadr args)))]
	     [else (#%raise-syntax-error
		    'ivar
		    (#%format "bad syntax (~s parts after the keyword)" l)
		    (#%cons 'ivar args))]))
	  (#%raise-syntax-error
	   'ivar
	   "bad syntax (illegal use of `.')"
	   (#%cons 'ivar args)))))

> kstop ivar <

> literal "#endif"

(#%define-macro #%fluid-let 
    (#%lambda (assignments expr1 . body)
     (#%if (#%null? assignments)
      `(#%begin ,expr1 ,@body)
      (#%let ([mk-expr (lambda () (#%list* 'fluid-let 
					      assignments 
					      expr1
					      body))])
	(#%unless (#%list? assignments)
           (#%raise-syntax-error 'fluid-let
				 "bad syntax" (mk-expr)
				 assignments))
	(#%let ([tempnames (#%map (#%lambda (assignment) 
				   (#%cond
				    [(#%and (#%pair? assignment)
					   (#%symbol? (#%car assignment))
					   (#%pair? (#%cdr assignment))
					   (#%null? (#%cddr assignment)))
				    (#%gensym)]
				    [else
				     (#%raise-syntax-error
				      'fluid-let
				      "bad syntax"
				      (mk-expr)
				      assignment)]))
				  assignments)])
	     `(#%let ,(#%map (#%lambda (tempname assignment)
			     (#%list tempname (#%car assignment)))
			   tempnames assignments)
	   (#%dynamic-wind
	    (#%lambda ()
	      ,@(#%map (#%lambda (assignment)
			 `(#%set! ,(#%car assignment) ,(#%cadr assignment)))
		       assignments))
	    (#%lambda () ,expr1 ,@body)
	    (#%lambda ()
	      ,@(#%map (#%lambda (assignment tempname)
		       `(#%set! ,(#%car assignment) ,tempname))
		     assignments tempnames)))))))))

> kstop fluid-let <

(#%define-macro #%parameterize
    (#%lambda (params . body)
     (#%let ([fail
	      (#%lambda (msg)
	       (#%raise-syntax-error
		'parameterize
		msg
		(#%list* 'parameterize params body)))])
      (#%if (#%null? body) (fail "bad syntax (empty body)"))
      (#%if (#%null? params)
        `(#%begin ,@body)
	(#%if (#%or (#%not (#%pair? params))
		    (#%not (#%pair? (#%car params)))
		    (#%not (#%pair? (#%cdar params)))
		    (#%not (#%null? (#%cddar params))))
	      (fail "bad syntax")
	      (#%let ([param (#%caar params)]
		      [orig (#%gensym)]
		      [pz (#%gensym)])
		 `(#%let* ([,pz (#%in-parameterization (#%current-parameterization) ,param)]
			   [,orig (,pz)])
		     (#%dynamic-wind
		        (#%lambda () (,pz ,(#%cadar params)))
		        (#%lambda () (parameterize ,(cdr params) ,@body))
			(#%lambda () (,pz ,orig))))))))))

> kstop parameterize <

(#%define-macro #%with-handlers
    (#%lambda (clauses . body)
      (#%if (#%and (#%list? clauses)
		   (#%andmap (#%lambda (p)
			       (#%and (#%pair? p)
				      (#%pair? (#%cdr p))
				      (#%null? (#%cddr p))))
			     clauses)
		   (#%pair? body))
	  (#%void)
	  (#%raise-syntax-error 'with-handlers "bad syntax"
				(#%list* 'with-handlers clauses body)))
      (if (#%null? clauses)
	  `(begin ,@body)
        (#%let ([k (#%gensym)]
	        [loop (#%gensym)]
	        [list (#%gensym)])
	  `((#%call/ec 
	     (#%lambda (,k)
	       (#%let ([,list (#%list ,@(#%map (#%lambda (p)
		  			       `(#%cons ,(#%car p)
		      				,(#%cadr p)))
					     clauses))])
	         (#%parameterize ([#%current-exception-handler
				   (#%lambda (e)
				    (,k
				     (#%let ,loop ([,list ,list])
				       (#%cond
				        [(#%null? ,list)
					 (#%lambda () (#%raise e))]
					[((#%caar ,list) e)
					 (#%lambda () ((#%cdar ,list) e))]
					[else
					 (,loop (#%cdr ,list))]))))])
		   (#%call-with-values
		    (#%lambda () ,@body)
		    (#%lambda args (,k (#%lambda () (#%apply #%values args))))))))))))))

> kstop with-handlers <

> literal "#ifdef USE_STRUCT_CASE"

(#%define-macro #%struct-case-lambda
  (#%lambda (x . pairs)
    (#%unless (#%and (#%symbol? x)
                     (#%list? pairs)
                     (#%andmap (#%lambda (p) (#%and (#%list? p) (#%not (#%null? p)))) pairs))
	      (#%raise-syntax-error 'struct-case-lambda "bad syntax"
				    (#%list* 'struct-case-lambda x pairs)))
    (#%let-values ([(predicates functions else)
		    (#%let loop ([l pairs])
		      (#%if (#%null? l)
			    (#%values () () ())
			    (#%let-values ([(p-rest f-rest else) (loop (#%cdr l))]
					   [(p) (#%caar l)]
					   [(f) (#%let ([r (#%cdar l)])
						  `(#%lambda (,x) ,@(#%if (#%null? r)
									  '(x)
									  r)))])
			     (#%cond
			      [(#%and (#%eq? p 'else)
				      (#%not (#%local-expansion-time-bound? 'else)))
			       (#%unless (#%null? p-rest)
					 (#%raise-syntax-error 'struct-case-lambda "else clause must be last"
							       (#%list* 'struct-case-lambda x pairs)))
			       (#%values () () (#%list f))]
			      [else
			       (#%values (#%cons p p-rest)
					 (#%cons f f-rest)
					 else)]))))])
      (#%list* '#%make-struct-case 
	       (#%cons '#%list predicates)
	       (#%cons '#%list functions)
	       else))))

> kstop struct-case-lambda <

> literal "#endif"

(#%define with-parameterization
  (#%lambda (p thunk)
     (#%unless (#%parameterization? p)
	(#%raise-type-error 'with-parameterization "parameterization" p))
     (#%unless (#%and (#%procedure? thunk)
		      (#%procedure-arity-includes? thunk 0))
	(#%raise-type-error 'with-parameterization "procedure (arity 0)" thunk))
     (#%let* ([orig (#%current-parameterization)])
       (#%dynamic-wind
	 (#%lambda () (#%current-parameterization p))
	 (#%lambda () (thunk))
	 (#%lambda () (#%current-parameterization orig))))))

> fstop with-parameterization <

(#%define with-new-parameterization
  (#%lambda (thunk)
     (#%unless (#%and (#%procedure? thunk)
		      (#%procedure-arity-includes? thunk 0))
	(#%raise-type-error 'with-new-parameterization "procedure (arity 0)" thunk))
     (#%let* ([naya (#%make-parameterization)])
       (#%with-parameterization naya thunk))))

> fstop with-new-parameterization <

(#%define load/cd
  (#%let ([make-exn make-exn:i/o:filesystem:directory]
	  [debug debug-info-handler])
    (#%lambda (n)
      (#%unless (#%string? n)
	(#%raise-type-error 'load/cd "string" n))
      (#%let-values ([(base name dir?) (#%split-path n)])
	 (#%if dir?
	     (#%raise
	      (make-exn
	       (#%format "load/cd: cannot open a directory: ~s" n)
	       ((debug))
	       n))
	     (#%if (#%not (#%string? base))
		 (#%load n)
		 (#%begin
		   (#%if (#%not (#%directory-exists? base))
		       (#%raise
			(make-exn
			 (#%format 
			  "load/cd: directory of ~s does not exist (current directory is ~s)" 
			  n (#%current-directory))
			 ((debug))
			 base)))
		   (#%let ([orig (#%current-directory)])
		     (#%dynamic-wind
		      (#%lambda () (#%current-directory base))
		      (#%lambda () (#%load name))
		      (#%lambda () (#%current-directory orig)))))))))))
  
> fstop load/cd <

(#%define (read-eval-print-loop)
  (#%let* ([eeh #f]
	   [jump #f]
	   [rep-error-escape-handler (lambda () (jump))])
    (#%dynamic-wind
      (#%lambda () (#%set! eeh (#%error-escape-handler))
		   (#%error-escape-handler rep-error-escape-handler))
      (#%lambda ()
	(#%let/ec done
          (#%let loop ()
	    (#%let/ec k
		(#%set! jump k)
		(#%let ([v ((#%current-prompt-read))])
		   (#%when (#%eof-object? v) (done))
		   (#%call-with-values
		    (#%lambda () ((#%current-eval) v))
		    (#%lambda results (#%for-each (#%current-print) results)))))
	    (loop))))
      (#%lambda () (#%error-escape-handler eeh)
		   (#%set! jump #f)
                   (#%set! eeh #f)))))

> fstop read-eval-print-loop <

> literal "#ifndef NO_FILE_SYSTEM_UTILS"

(#%define load/use-compiled
  (#%let ([re:suffix (#%regexp "\\..?.?.?$")]
	  [resolve (#%lambda (s)
		     (#%if (#%complete-path? s)
			   s
			   (#%let ([d (#%current-load-relative-directory)])
			      (#%if d (#%path->complete-path s d) s))))]
	  [date>=?
	   (#%lambda (a bm)
	     (#%let ([am (#%file-modify-seconds a)])
		(#%or (#%and (#%not bm) am) (#%and am bm (#%>= am bm)))))])
    (#%lambda (path)
      (#%unless (#%and (#%string? path) (#%or (#%relative-path? path) (#%absolute-path? path)))
	(#%raise-type-error 'load/use-compiled "pathname string" path))
      (#%let*-values ([(path) (resolve path)]
		      [(base file dir?) (#%split-path path)]
		      [(base) (#%if (#%eq? base 'relative) 'same base)])
        (#%let* ([get-so (#%lambda (file)
			    (#%build-path base
					  "compiled"
					  "native"
					  (#%system-library-subpath)
					  (#%regexp-replace 
					   re:suffix file
					   (#%case (#%system-type)
					      [(windows) ".dll"]
					      [else ".so"]))))]
		 [zo (#%build-path base
				   "compiled"
				   (#%regexp-replace re:suffix file ".zo"))]
		 [so (get-so file)]
		 [_loader-so (get-so "_loader.ss")]
		 [path-d (#%file-modify-seconds path)]
		 [with-dir (#%lambda (t) (#%parameterize ([#%current-load-relative-directory 
							   (#%if (#%string? base) base (#%current-directory))]) (t)))])
	    (#%cond
	     [(#%and (date>=? _loader-so path-d)
		     (#%let ([getter (#%load-extension _loader-so)])
			(getter (#%string->symbol (#%regexp-replace re:suffix file "")))))
	      => (#%lambda (loader) (with-dir loader))]
	     [(date>=? so path-d)
	      (with-dir (#%lambda () ((#%current-load-extension) so)))]
	     [(date>=? zo path-d)
	      (with-dir (#%lambda () ((#%current-load) zo)))]
	     [else (#%load path)]))))))

> fstop load/use-compiled <

(#%define-values (load-relative load-relative-extension)
  (#%let ([mk
	   (#%lambda (load name)
             (#%lambda (path)
               (#%unless (#%and (#%string? path) (#%or (#%relative-path? path) (#%absolute-path? path)))
                 (#%raise-type-error name "pathname string" path))
               (#%if (#%complete-path? path)
           	  (load path)
           	  (#%let ([dir (#%current-load-relative-directory)])
           	     (load (#%if dir (#%path->complete-path path dir) path))))))])
    (#%values (mk #%load 'load-relative) (mk #%load-extension 'load-relative-extension))))

> fstop load-relative load-relative-extension <

(#%define-values (require-library require-relative-library collection-path)
  (#%let* ([get-table current-loaded-library-table]
	   [not-found (box 0)]
	   [null-str (#%string #\nul)]
	   [debug debug-info-handler]
	   [make-path-exn make-exn:i/o:filesystem:path]
	   [check (#%lambda (who s)
		    (#%unless (#%string? s)
		      (#%raise-type-error who "string" s))
		    (#%unless (#%relative-path? s)
		      (#%raise (make-path-exn
			       (#%format "~a: invalid relative path: ~s" who s)
			       ((debug)) s))))]
	   [with-null (#%lambda (l) (#%let loop ([l l])
					   (#%if (#%null? l)
						 #%null
						 (#%list* (#%car l) null-str (loop (#%cdr l))))))]
	   [make-coll-exn make-exn:i/o:filesystem:collection]
	   [make-file-exn make-exn:i/o:filesystem:file]
	   [check-collection
	    (#%lambda (who collection collection-path)
		      (check who collection) (#%for-each (lambda (p) (check who p)) collection-path))]
	   [find-col
	    (#%lambda (who collection collection-path)
	      (#%let ([all-paths (#%current-library-collection-paths)])
		     (#%let loop ([paths all-paths])
			    (#%if (#%null? paths)
				 (#%raise
				  (make-coll-exn
				  (#%format "~a: collection not found: ~s in any of: ~s" 
					    who collection all-paths)
				  ((debug))
				  collection))
				 (#%let ([dir (#%build-path (#%car paths) collection)])
				    (#%if (#%directory-exists? dir)
				       (#%let* ([cpath (#%apply #%build-path dir collection-path)])
					  (#%if (#%directory-exists? cpath)
						cpath
						(let loop ([p dir][l collection-path][c collection])
						  (#%let ([np (#%build-path p (#%car l))]
							  [nc (#%build-path c (#%car l))])
							 (#%if (#%directory-exists? np)
							       (loop np (#%cdr l) nc)
							       (#%raise
								(make-coll-exn
								 (#%format "require-library: collection ~s does not have sub-collection: ~s in: ~s"
									   c (#%car l) p)
								 ((debug))
								 nc)))))))
				       (loop (#%cdr paths))))))))])
	  (#%letrec ([collection-path (#%lambda (collection . collection-path) 
						(check-collection 'collection-path collection collection-path)
						(find-col 'collection-path collection collection-path))]
		     [require-relative-library
		      (#%lambda (file . collection-path)
			 (check 'require-relative-library file) 
			 (check-collection 'require-relative-library file collection-path)
			 (#%let ([cp (#%current-require-relative-collection)])
			    (#%if cp
				  (#%apply require-library file (#%append cp collection-path))
				  (#%raise
				   (make-coll-exn
				    (#%format "require-relative-library: there is no current collection for library: ~s~a"
					      file
					      (#%if (#%null? collection-path)
						    ""
						    (#%format " in sub-collection: ~s" collection-path)))
				    ((debug))
				    (#%apply #%build-path file collection-path))))))]
		     [require-library
		      (#%case-lambda
		       [(file) (require-library file "mzlib")]
		       [(file collection . collection-path)
			(check 'require-library file) (check-collection 'require-library collection collection-path)
			(#%let ([table (get-table)]
				[sym (#%string->symbol (#%apply #%string-append 
								(with-null (#%list* file collection collection-path))))])
			       (#%let ([found (#%hash-table-get table sym (#%lambda () not-found))])
				      (#%if (#%eq? found not-found)
					    (#%let* ([c (find-col 'require-library collection collection-path)]
						     [p (#%build-path c file)]
						     [result (#%if (#%file-exists? p)
								   (#%parameterize ([#%current-require-relative-collection
										     (#%cons collection collection-path)])
								     (#%call-with-values
								      (#%lambda ()
										((#%if (#%require-library-use-compiled)
										       #%load/use-compiled 
										       #%load)
										 p))
								      #%list))
								   (#%raise
								    (make-file-exn
								     (#%format "require-library: collection ~s does not have library: ~s in: ~s"
									       (#%apply #%build-path collection collection-path) file c)
								     ((debug))
								     p)))])
						    (#%hash-table-put! table sym result)
						    (#%apply #%values result))
					    (#%apply #%values found))))])])
		    (#%values require-library require-relative-library collection-path))))

> fstop require-library require-relative-library collection-path <

(#%define-macro #%reference-library
  (#%letrec ([rl (#%case-lambda 
		  [(name) (rl name "mzlib")]
		  [(name collection . collection-path)
		   (#%let ([name (#%local-expand-defmacro name)]
			   [collection (#%local-expand-defmacro collection)]
			   [check (#%lambda (s kind)
				     (#%unless (#%string? s)
					 (#%raise-syntax-error 
					  'reference-library
					  (#%format "~s name is not a string" kind)
					  (#%list 'reference-library name collection))))])
			  (check name "library")
			  (check collection "collection")
			  (#%for-each (#%lambda (c) (check c "sub-collection")) collection-path)
			  `(#%require-library ,name ,collection ,@collection-path))])])
       rl))

> kstop reference-library <

(#%define path-list-string->path-list
  (#%let ((r (#%regexp (#%let ((sep (#%case (#%system-type) 
					    ((unix) ":")
					    ((windows macos) ";"))))
			      (#%format "([^~a]*)~a(.*)" sep sep))))
	  (cons-path (#%lambda (default s l) 
			       (#%if (#%string=? s "")
				     (#%append default l)
				     (#%if (#%or (#%relative-path? s) (#%absolute-path? s)) (#%cons s l) l)))))
    (#%lambda (s default)
      (#%unless (#%string? s) (#%raise-type-error 'path-list-string->path-list "string" s))
      (#%unless (#%list? default) (#%raise-type-error 'path-list-string->path-list "list" default))
      (#%let loop ([s s])
	(#%let ([m (#%regexp-match r s)])
          (#%if m
		(cons-path default (#%cadr m) (loop (#%caddr m)))
		(cons-path default s #%null)))))))

> fstop path-list-string->path-list <

(#%define find-executable-path
  (#%lambda (program libpath)
    (#%unless (#%string? program)
      (#%raise-type-error 'find-executable-path "string" program))
    (#%unless (#%string? libpath)
      (#%raise-type-error 'find-executable-path "string" libpath))
    (#%letrec ([found-exec
		(#%lambda (exec-name)
		  (#%let-values ([(base name isdir?) (#%split-path exec-name)])
		     (#%if (#%string? base)
		       (#%let ([lib (#%build-path base libpath)])
			 (#%if (#%or (#%directory-exists? lib) 
				     (#%file-exists? lib))
			     lib
			     (#%let ([resolved (#%resolve-path exec-name)])
			       (#%cond
				[(#%string=? resolved exec-name) #f]
				[(#%relative-path? resolved)
				 (found-exec (#%build-path base resolved))]
			       [else (found-exec resolved)]))))
		       #f)))])
      (#%if (#%relative-path? program)
	  (#%let ([paths-str (#%getenv "PATH")])
	    (#%let loop ([paths (#%if paths-str (#%path-list-string->path-list paths-str #%null) #%null)])
		  (#%if (#%null? paths)
			#f
			(#%let* ([base (#%path->complete-path (#%car paths))]
				 [name (#%build-path base program)])
				(#%if (#%file-exists? name)
				      (found-exec name)
				      (loop (#%cdr paths)))))))
	  (found-exec program)))))

> fstop find-executable-path <

> literal "#endif"

> literal "#ifdef TIME_SYNTAX"

(#%define-macro #%time
  (#%lambda (expr1 . body)
    `(#%let-values ([(s) (#%current-gc-milliseconds)]
                    [(v cpu user) (#%time-apply (#%lambda () ,expr1 ,@body))])
	(#%printf "cpu time: ~s real time: ~s gc time: ~s~n"
		  cpu user (#%- (#%current-gc-milliseconds) s))
	(#%apply #%values v))))

> kstop time <

> literal "#endif"

> literal "#ifndef NO_UNIT_SYSTEM"

(#%define verify-linkage-signature-match
  (#%let ([make-non #%make-exn:unit:signature:non-signed-unit]
	  [make-arity #%make-exn:unit:signature:arity]
	  [p-suffix (#%lambda (pos) (#%case pos [(1) 'st][(2) 'nd][(3) 'rd][else 'th]))])
    (#%lambda (who tags units esigs isigs)
      (#%for-each
       (#%lambda (u tag)
	  (#%unless (#%unit-with-signature? u)
	     (#%raise
	      (make-non
	       (#%format
		"~s: expression for \"~s\" is not a signed unit"
		who tag)
	       ((#%debug-info-handler)) u))))
       units tags)
      (#%for-each
       (#%lambda (u tag esig)
	 (#%verify-signature-match
	  who #f
	  (#%format "specified export signature for ~a" tag)
	  esig
	  (#%format "export signature for actual ~a sub-unit" tag)
	  (#%unit-with-signature-exports u)))
       units tags esigs)
      (#%for-each
       (#%lambda (u tag isig)
	 (#%let ([n (#%length (#%unit-with-signature-imports u))]
		 [c (#%length isig)])
	   (#%unless (#%= c n)
	      (#%raise
	       (make-arity
		(#%format
		 "~s: ~a unit imports ~a units, but ~a units were provided"
		 who tag n c)
		((#%debug-info-handler)) u)))))
       units tags isigs)
      (#%for-each
       (#%lambda (u tag isig)
	 (#%let loop ([isig isig][expecteds (#%unit-with-signature-imports u)][pos 1])
	   (#%unless (#%null? isig)
	     (#%let ([expected (#%car expecteds)]
		     [provided (#%car isig)])
	       (#%verify-signature-match
		who #t
		(#%format "~a unit's ~s~s import (~a signature)" tag
			  pos (p-suffix pos)
			  (#%car expected))
		(#%cdr expected)
		(#%format "~s~s linkage (~a signature) for ~a"
			  pos (p-suffix pos)
			  (#%car provided)
			  tag)
		(#%cdr provided))
	       (loop (#%cdr isig) (#%cdr expecteds) (#%add1 pos))))))
       units tags isigs))))

> fstop verify-linkage-signature-match <

; Extra names:
(#%define-values (unit/sig? unit-with-signature->unit unit/sig->unit)
       (values #%unit-with-signature?
	       #%unit-with-signature-unit
	       #%unit-with-signature-unit))

> fstop unit/sig? unit-with-signature->unit unit/sig->unit <

> literal "#endif"

(#%define rationalize
  (#%letrec ([check (#%lambda (x) 
                      (#%unless (#%real? x) (#%raise-type-error 'rationalize "real" x)))]
	     [find-between 
	      (#%lambda (lo hi)
		(#%if (#%integer? lo)
		    lo
		    (#%let ([lo-int (#%floor lo)]
			    [hi-int (#%floor hi)])
		      (#%if (#%< lo-int hi-int)
			  (#%add1 lo-int)
			  (#%+ lo-int
			     (#%/ (find-between (#%/ (#%- hi lo-int)) (#%/ (#%- lo lo-int)))))))))])
     (#%lambda (x within)
       (check x) (check within)
       (#%let* ([delta (#%abs within)]
		[lo (#%- x delta)]
	        [hi (#%+ x delta)])
	 (#%cond
	  [(#%not (#%= x x)) +nan.0]
	  [(#%<= lo 0 hi) (#%if (#%exact? x) 0 0.0)]
	  [(#%negative? lo) (#%- (find-between (#%- hi) (#%- lo)))]
	  [else (find-between lo hi)])))))

> fstop rationalize <

(#%define (simple-return-primitive? v)
  (#%unless (#%primitive? v) (#%raise-type-error 'simple-return-primitive? "primitive-procedure" v))
  (#%not (#%memq (#%inferred-name v) '(call-with-values 
				       apply 
				       error
				       call-with-current-continuation
				       hash-table-get
				       write-image-to-file))))

> fstop simple-return-primitive? <

(#%define (port? x) (#%or (#%input-port? x) (#%output-port? x)))

> fstop port? <
