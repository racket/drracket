(unit/sig ricedefs^
  (import [params : plt:userspace:params^])

  (define check-second 
    (lambda (prim-name a b)
      (unless (list? b)
	(error prim-name
	       "second argument must be of type <list>, given ~e and ~e"
	       a b))))
  
  (define check-last
    (lambda (prim-name args)
      (let loop ([l args])
	(cond
	 [(null? l) (void)]
	 [(null? (cdr l))
	  (let ([last (car l)])
	    (unless (list? last)
	      (error prim-name
		     "last argument must be of type <list>, given ~e; all args: ~a"
		     last
		     (map (lambda (x) (format "~e" x)) args))))]
	 [else (loop (cdr l))]))))

  (define (check-arity prim len lst)
    (let ([lst-len (length lst)])
      (unless (#%>= lst-len len)
	(error prim
	       "expects at least ~a arguments, given ~a"
	       len
	       (if (#%= 0 lst-len)
		   0
		   (format
		    "~a: ~a"
		    lst-len
		    (apply string-append
			   (cons (format "~e" (car lst))
				 (let loop ([rst (cdr lst)])
				   (cond
				    [(null? rst) null]
				    [else (cons (format " ~e" (car rst))
						(loop (cdr rst)))]))))))))))
		    

  (define =
    (if (params:<=-at-least-two-args)
	(lambda args
	  (check-arity '= 2 args)
	  (apply #%= args))
	#%=))

  (define +
    (if (params:<=-at-least-two-args)
	(lambda args
	  (check-arity '+ 2 args)
	  (apply #%+ args))
	#%+))

  (define /
    (if (params:<=-at-least-two-args)
	(lambda args
	  (check-arity '/ 2 args)
	  (apply #%/ args))
	#%/))

  (define *
    (if (params:<=-at-least-two-args)
	(lambda args
	  (check-arity '* 2 args)
	  (apply #%* args))
	#%*))

  (define >=
    (if (params:<=-at-least-two-args)
	(lambda args
	  (check-arity '>= 2 args)
	  (apply #%>= args))
	#%>=))

  (define <
    (if (params:<=-at-least-two-args)
	(lambda args
	  (check-arity '< 2 args)
	  (apply #%< args))
	#%<))

  (define >
    (if (params:<=-at-least-two-args)
	(lambda args
	  (check-arity '> 2 args)
	  (apply #%> args))
	#%>))

  (define <=
    (if (params:<=-at-least-two-args)
	(lambda args
	  (check-arity '<= 2 args)
	  (apply #%<= args))
	#%<=))

  (define cons (if (params:allow-improper-lists)
		   #%cons
		   (lambda (a b)
		     (check-second 'cons a b)
		     (#%cons a b))))
  
  (define set-cdr! (if (params:allow-improper-lists)
		       #%set-cdr!
		       (lambda (a b)
			 (check-second 'set-cdr! a b)
			 (#%set-cdr! a b))))
  
  (define list* (if (params:allow-improper-lists)
		    #%list*
		    (lambda x
		      (check-last 'list* x)
		      (apply #%list* x))))
  
  (define append (if (params:allow-improper-lists)
		     #%append
		     (lambda x
		       (check-last 'append x)
		       (apply #%append x))))
  
  (define append! (if (params:allow-improper-lists)
		      #%append!
		      (lambda x
			(check-last 'append! x)
			(apply #%append! x)))))
