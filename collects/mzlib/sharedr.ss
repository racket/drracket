
(unit/sig->unit
(compound-unit/sig
 (import)
 (link [FUNCTION : mzlib:function^ ((require-library "functior.ss") )]
       [SHARED : (shared) (

(unit/sig (shared)
 (import mzlib:function^)

 ;; SHARED starts here

 (define shared
  (let ()
    (define-struct twople (left right))
    (define-struct cons-rhs (id car cdr))
    (define-struct vector-rhs (id args))
    (define-struct box-rhs (id arg))
    (define-struct weak-box-rhs (id let arg))
    (define-struct trans (rhs lets set!s))
    (lambda (defns . body)
      (letrec ([bad (lambda (s sexp)
		      (error 'shared (string-append s ": ~a") sexp))]
	       [build-args
		(lambda (args howmany)
		  (cond
		   [(null? args) '()]
		   [(pair? args) (cons (car args) 
				       (build-args (cdr args)
						   (if (number? howmany)
						       (sub1 howmany)
						       howmany)))]
		   [else (bad "args" args)]))]
	       [build-args1
		(lambda (x)
		  (cond
		   [(and (pair? x) (null? (cdr x))) (list (car x))]
		   [else (bad "args" x)]))]
	       [build-args2
		(lambda (x)
		  (if (pair? x)
		      (let ((xcdr (cdr x)))
			(if (pair? xcdr)
			    (let ((xcdrcdr (cdr xcdr)))
			      (if (null? xcdrcdr)
				  (list (car x) (car xcdr))
				  (bad "args" x)))
			    (bad "args" x)))
		      (bad "args" x)))]		     
	       [build-defn
		(lambda (x)
		  (unless (and (pair? x)
			       (symbol? (car x)))
			  (bad "bad binding" x))
		  (if (not (and (pair? (cdr x))
				(pair? (cadr x))
				(symbol? (caadr x))))
		      (make-trans x '() '())
		      (let ([id (car x)]
			    [constructor (caadr x)]
			    [args (cdadr x)])
			(case constructor
			  [(list) (let ([args (build-args args 'whatever)])
				    (if (null? args)
					(make-trans `(,id (list))
						    '()
						    '())
					(make-cons-rhs id (car args) `(list ,@(cdr args)))))]
			  [(vector) (let ([args (build-args args 'whatever)])
				      (make-vector-rhs id args))]
			  [(box) (let ([args (build-args1 args)])
				   (make-box-rhs id (car args)))]
			  ; [(make-weak-box) (let ([args (build-args1 args)])
			  ; (make-weak-box-rhs id (car args)))]
			  [(cons) (let ([args (build-args2 args)])
				    (make-cons-rhs id (car args) (cadr args)))]
			  [else (make-trans x '() '())]))))]
	       [build-defns
		(lambda (x)
		  (cond
		   [(null? x) '()]
		   [(pair? x) (cons (build-defn (car x))
				    (build-defns (cdr x)))]
		   [else (bad "defns list" x)]))]
	       [transform
		(lambda (binding)
		  (cond
		   [(vector-rhs? binding)
		    (let ()
		      (define-struct b&s (bind set!))
		      (let* ([id (vector-rhs-id binding)])
			(let ([elems
			       (twople-left
				(foldl (lambda (x data) 
					 (let ([list (twople-left data)]
					       [i (twople-right data)]
					       [eid (gensym)])
					   (make-twople (cons (make-b&s `(,eid ,x)
									`(vector-set! ,id ,i ,eid))
							      list)
							(+ i 1))))
				       (make-twople '() 0)
				       (vector-rhs-args binding)))])
			  (make-trans `(,id (vector ,@(map (lambda (x) '(void))
							   (vector-rhs-args binding))))
				      (map b&s-bind elems)
				      (map b&s-set! elems)))))]
		   [(box-rhs? binding)
		    (let ([id (box-rhs-id binding)]
			  [eid (gensym)])
		      (make-trans `(,id (box (void)))
				  (list `(,eid ,(box-rhs-arg binding)))
				  (list `(set-box! ,id ,eid))))]
		   [(weak-box-rhs? binding)
		    (let ([id (weak-box-rhs-id binding)]
			  [eid (gensym)])
		      (make-trans `(,id (make-weak-box (void)))
				  (list `(,eid ,(weak-box-rhs-arg binding)))
				  (list `(set-weak-box! ,id ,eid))))]
		   [(cons-rhs? binding) 
		    (let ([id (cons-rhs-id binding)]
			  [car-id (gensym)]
			  [cdr-id (gensym)])
		      (make-trans `(,id (cons (void) (void)))
				  (list `(,car-id ,(cons-rhs-car binding))
					`(,cdr-id ,(cons-rhs-cdr binding)))
				  (list `(set-car! ,id ,car-id)
					`(set-cdr! ,id ,cdr-id))))]
		   [(trans? binding) binding]
		   [else (bad "internal error:" binding)]))]
	       [transformed-defns (map transform (build-defns defns))])
	(list 'letrec
	      (map trans-rhs transformed-defns)
	      (list 'let (apply append (map trans-lets transformed-defns))
		    (cons 'begin
			  (append (apply append (map trans-set!s transformed-defns))
				  body)))))))))


  ;; SHARED ends here

        FUNCTION)])

  (export (var (SHARED shared)))))


