; $Id: quasi.ss,v 1.10 1999/06/13 21:41:25 mflatt Exp $

; Fix the null? in qq-normalize.

(define qq-normalize
  (lambda (new old)
    (if (eq? new old)
      (if (and (z:list? new) (zero? (z:sequence-length new)))
	'null
	(list '#%quote new))
      new)))

(define qq-process
  (lambda (e source env attributes vocab)
    (expand-expr
      (structurize-syntax e source)
      env attributes vocab)))

(define quasiquote-micro
  (let* ((kwd '())
	  (in-pattern '(_ template))
	  (m&e (pat:make-match&env in-pattern kwd))
	  (qq-pattern-1 '(unquote body))
	  (qq-pattern-2 '(unquote x ...))
	  (qq-pattern-3 '(quasiquote x ...))
	  (qq-pattern-4 '(unquote-splicing x ...))
	  (qq-pattern-5 '((unquote-splicing body) . rest))
	  (qq-pattern-6 '((unquote-splicing x ...) . y))
	  (qq-m&e-1 (pat:make-match&env qq-pattern-1 '(unquote)))
	  (qq-m&e-2 (pat:make-match&env qq-pattern-2 '(unquote)))
	  (qq-m&e-3 (pat:make-match&env qq-pattern-3 '(quasiquote)))
	  (qq-m&e-4 (pat:make-match&env qq-pattern-4 '(unquote-splicing)))
	  (qq-m&e-5 (pat:make-match&env qq-pattern-5 '(unquote-splicing)))
	  (qq-m&e-6 (pat:make-match&env qq-pattern-6 '(unquote-splicing))))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((template (pat:pexpand 'template p-env kwd)))
	      (qq-process
		(qq-normalize
		  (let qq ((x template)
			    (level 0))
		    (let ((qq-list (lambda (x level)
				     (let* ((old-first (syntax-car x))
					     (old-rest (syntax-cdr x))
					     (first (qq old-first level))
					     (rest (qq old-rest level)))
				       (if (and (eq? first old-first)
					     (eq? rest old-rest))
					 x
					 (list '#%cons
					   (qq-normalize first old-first)
					   (qq-normalize rest old-rest)))))))
		      (cond
			((and (or (z:list? x) (z:improper-list? x))
			   (not (zero? (z:sequence-length x))))
			  (cond
			    ((pat:match-against qq-m&e-1 x env)
			      =>
			      (lambda (p-env)
				(let ((body (pat:pexpand 'body p-env kwd)))
				  (if (zero? level)
				    body
				    (qq-list x (sub1 level))))))
			    ((pat:match-against qq-m&e-2 x env)
			      (static-error
				"unquote" 'kwd:unquote x
				"takes exactly one expression"))
			    ((pat:match-against qq-m&e-3 x env)
			      (qq-list x (add1 level)))
			    ((pat:match-against qq-m&e-4 x env)
			      (static-error
				"unquote-splicing" 'kwd:unquote-splicing x
				"invalid context inside quasiquote"))
			    ((pat:match-against qq-m&e-5 x env)
			      =>
			      (lambda (p-env)
				(let* ((body (pat:pexpand 'body p-env kwd))
					(rest (pat:pexpand 'rest p-env kwd))
					(q-rest (qq rest level)))
				  (if (zero? level)
				    (list '#%append body
				      (qq-normalize q-rest rest))
				    (let ((q-body (qq body (sub1 level))))
				      (if (and (eq? q-rest rest)
					    (eq? q-body body))
					x
					(list '#%cons
					  (list '#%cons
					    (list '#%quote 'unquote-splicing)
					    (list '#%cons
					      (qq-normalize q-body body)
					      '()))
					  (qq-normalize q-rest rest))))))))
			    ((pat:match-against qq-m&e-6 x env)
			      (static-error
				"unquote-splicing" 'kwd:unquote-splicing x
				"takes exactly one expression"))
			    (else
			      (qq-list x level))))
			((z:vector? x)
			  (let* ((v (structurize-syntax (z:read-object x) x))
				  (qv (qq v level)))
			    (if (eq? v qv)
			      x
			      (list '#%list->vector qv))))
			((z:box? x)
			  (let* ((b (z:read-object x))
				  (qb (qq b level)))
			    (if (eq? b qb)
			      x
			      (list '#%box qb))))
			(else
			  x))))
		  template)
		expr env attributes vocab))))
	(else
	  (static-error
	    "quasiquote" 'kwd:quasiquote expr "malformed expression"))))))

(add-primitivized-micro-form 'quasiquote intermediate-vocabulary quasiquote-micro)
(add-primitivized-micro-form 'quasiquote scheme-vocabulary quasiquote-micro)

