; $Id: pattern.ss,v 1.5 1998/05/08 22:15:22 mflatt Exp $

; Uses of memq are okay, since they look up pattern var in kwd list

; Use of equal? WILL FAIL!

(unit/sig zodiac:pattern^
  (import zodiac:misc^ zodiac:sexp^
    (z : zodiac:reader-structs^) zodiac:scheme-core^)

  (define (syntax-andmap pred l)
    (andmap pred (expose-list l)))

  (define (syntax-ormap pred l)
    (ormap pred (expose-list l)))

  ; ----------------------------------------------------------------------

  (define make-match&env
    (lambda (p k)			; pattern x kwd
      (letrec
	((m&e
	   (lambda (p)
	     (cond
	       ((ellipsis? p) 
		 (let ((p-head (car p)))
		   (let ((nestings (get-ellipsis-nestings p-head k)))
		     (let ((match-head (m&e p-head)))
		       (lambda (e esc env)
			 (if (z:list? e)
			     (list (cons nestings
					 (map (lambda (e) (match-head e esc env))
					      (expose-list e))))
			     (esc #f)))))))
	       ((pair? p) 
		 (let ((match-head (m&e (car p)))
		       (match-tail (m&e (cdr p))))
		   (lambda (e esc env) 
		     (if  (or (and (z:list? e)
				   (not (syntax-null? e)))
			      (z:improper-list? e))
			  (append (match-head (syntax-car e) esc env)
				  (match-tail (syntax-cdr e) esc env))
			  (esc #f)))))
	       ((null? p) 
		(lambda (e esc env)
		  (if (syntax-null? e) '() (esc #f))))
	       ((symbol? p) 
		 (if (memq p k)
		     (lambda (e esc env)
		       (if (z:symbol? e)
			   (if (lexically-resolved? e env)
			       (esc #f)
			       (if (name-eq? p (z:read-object e))
				   '()
				   (esc #f)))
			   (esc #f)))
		     (lambda (e esc env)
		       (list (cons p e)))))
	       (else
		 (lambda (e esc env)
		   (if (equal? p e) '() (esc #f))))))))
	(m&e p))))

  (define match-against
    (lambda (matcher e env)
      (let/ec esc
	(matcher e esc env))))

  (define penv-merge append)

  (define extend-penv
    (lambda (name output env)
      (cons (cons name output) env)))

  ; ----------------------------------------------------------------------

  (define pexpand
    (lambda (p r k)			; pattern x p-env x kwd
      (letrec
	((expander
	   (lambda (p r)
	     (cond
	       ((ellipsis? p)
		 (append
		   (let* ((p-head (car p))
			   (nestings (get-ellipsis-nestings p-head k))
			   (rr (ellipsis-sub-envs nestings r)))
		     (map (lambda (r1)
			    (expander p-head (append r1 r)))
		       rr))
		   (expander (cddr p) r)))
	       ((pair? p)
		 (cons (expander (car p) r)
		   (expander (cdr p) r)))
	       ((symbol? p)
		 (if (memq p k) p
		   (let ((x (assq p r)))
		     (if x (cdr x) p))))
	       (else p)))))
	(expander p r))))

;;; returns a list that nests a pattern variable as deeply as it
;;; is ellipsed
  (define get-ellipsis-nestings
    (lambda (p k)
      (let sub ((p p))
	(cond ((ellipsis? p) (list (sub (car p))))
	  ((pair? p) (append (sub (car p)) (sub (cdr p))))
	  ((symbol? p) (if (memq p k) '() (list p)))
	  (else '())))))

;;; finds the subenvironments in r corresponding to the ellipsed
;;; variables in nestings
  (define ellipsis-sub-envs
    (lambda (nestings r)
      (ormap (lambda (c)
	       (if (contained-in? nestings (car c)) (cdr c) #f))
	r)))

;;; checks if nestings v and y have an intersection
  (define contained-in?
    (lambda (v y)
      (if (or (symbol? v) (symbol? y)) (eq? v y)
	(ormap (lambda (v_i)
		 (ormap (lambda (y_j)
			  (contained-in? v_i y_j))
		   y))
	  v))))

;;; tests if x is an ellipsing pattern, i.e., of the form
;;; (blah ... . blah2)
  (define ellipsis?
    (lambda (x)
      (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '...))))

  ; ----------------------------------------------------------------------

  (define match-and-rewrite
    (case-lambda
      ((expr rewriter out kwd env)
	(let ((p-env (match-against rewriter expr env)))
	  (and p-env
	    (pexpand out p-env kwd))))
      ((expr rewriter out kwd succeed fail env)
	(let ((p-env (match-against rewriter expr env)))
	  (if p-env
	    (succeed (pexpand out p-env kwd))
	    (fail))))))

  )
