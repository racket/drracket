; By Dorai Sitaram
; then Shriram Krishnamurthi
; then Matthew Flatt

(unit
  (import)
  (export define-syntax
	  -:sr:tag
	  -:sr:untag
	  -:sr:flatten
	  -:sr:matches-pattern?
	  -:sr:get-bindings
	  -:sr:expand-pattern)

  (define -:sr:tag 'undefined--:sr:tag)
  (define -:sr:untag 'undefined--:sr:untag)
  (define -:sr:flatten 'undefined--:sr:flatten)

  (letrec ([hyg:rassq
	    (lambda (k al)
	      (let loop ([al al])
		(if (null? al) 
		    #f
		    (let ([c (car al)])
		      (if (eq? (cdr c) k) 
			  c
			  (loop (cdr al)))))))]
	   [hyg:tag
	    (lambda (e kk al)
	      (cond 
	       [(pair? e)
		(let* ((a-te-al (hyg:tag (car e) kk al))
		       (d-te-al (hyg:tag (cdr e) kk (cdr a-te-al))))
		  (cons (cons (car a-te-al) (car d-te-al))
			(cdr d-te-al)))]
	       [(vector? e)
		(let ([v-te-al (hyg:tag (vector->list e) kk al)])
		  (cons (list->vector (car v-te-al))
			(cdr v-te-al)))]
	       [(symbol? e)
		(cond 
		 [(eq? e '...) (cons '... al)]
		 [(memq e kk) (cons e al)]
		 [(hyg:rassq e al) 
		  => (lambda (c)
		       (cons (car c) al))]
		 [else
		  (let ((te (gensym)))
		    (cons te (cons (cons te e) al)))])]
	       [else (cons e al)]))]
	   [hyg:untag
	    (lambda (e al tmps)
	      (if (pair? e)
		  (let ([a (hyg:untag (car e) al tmps)])
		    (if (list? e)
			(case a
			  [(quote) 
			   (hyg:untag-no-tags e al)]
			  [(quasiquote)
			   (list a (hyg:untag-quasiquote (cadr e) al tmps))]
			  [(if begin)
			   `(,a ,@(map (lambda (e1)
					 (hyg:untag e1 al tmps)) (cdr e)))]
			  [(set! define)
			   `(,a ,(hyg:untag-vanilla (cadr e) al tmps)
				,@(map (lambda (e1)
					 (hyg:untag e1 al tmps)) (cddr e)))]
			  [(lambda)
			   (hyg:untag-lambda a (cadr e) (cddr e) al tmps)]
			  [(letrec)
			   (hyg:untag-letrec a (cadr e) (cddr e) al tmps)]
			  [(let)
			   (let ((e2 (cadr e)))
			     (if (symbol? e2)
				 (hyg:untag-named-let a e2 (caddr e) (cdddr e) al tmps)
				 (hyg:untag-let a e2 (cddr e) al tmps)))]
			  [(let*)
			   (hyg:untag-let* (cadr e) (cddr e) al tmps)]
			  [(do) (hyg:untag-do (cadr e) (caddr e) (cdddr e) al tmps)]
			  [(case)
			   `(case ,(hyg:untag-vanilla (cadr e) al tmps)
			      ,@(map
				 (lambda (c)
				   `(,(hyg:untag-vanilla (car c) al tmps)
				     ,@(hyg:untag-list (cdr c) al tmps)))
				 (cddr e)))]
			  [(cond)
			   `(cond ,@(map
				     (lambda (c)
				       (hyg:untag-list c al tmps))
				     (cdr e)))]
			  [else 
			   ; Must be an application:
			   (cons a (hyg:untag-list (cdr e) al tmps))])
			(cons a (hyg:untag-list* (cdr e) al tmps))))
		  (hyg:untag-vanilla e al tmps)))]
	   [hyg:untag-list
	    (lambda (ee al tmps)
	      (map (lambda (e)
		     (hyg:untag e al tmps)) ee))]
	   [hyg:untag-list*
	    (lambda (ee al tmps)
	      (let loop ((ee ee))
		(if (pair? ee)
		    (cons (hyg:untag (car ee) al tmps)
			  (loop (cdr ee)))
		    (hyg:untag ee al tmps))))]
	   [hyg:untag-no-tags
	    (lambda (e al)
	      (cond 
	       [(pair? e)
		(cons (hyg:untag-no-tags (car e) al)
		      (hyg:untag-no-tags (cdr e) al))]
	       [(vector? e)
		(list->vector
		 (hyg:untag-no-tags (vector->list e) al))]
	       [(not (symbol? e)) e]
	       [(assq e al) => cdr]
	       [else e]))]
	   [hyg:untag-quasiquote
	    (lambda (form al tmps)
	      (let qq ([x form][level 0])
		(cond
		 [(pair? x)
		  (let ([first (qq (car x) level)])
		    (cond
		     [(and (eq? first 'unquote) (list? x))
		      (let ([rest (cdr x)])
			(if (or (not (pair? rest))
				(not (null? (cdr rest))))
			    (raise-syntax-error
			     'unquote
			     "takes exactly one expression"
			     (list 'quasiquote (hyg:untag-no-tags form al)))
			    (if (zero? level)
				(list 'unquote (hyg:untag (car rest) al tmps))
				(cons first (qq rest (sub1 level))))))]
		     [(and (eq? first 'quasiquote) (list? x))
		      (cons 'quasiquote (qq (cdr x) (add1 level)))]
		     [(and (eq? first 'unquote-splicing) (list? x))
		      (raise-syntax-error
		       'unquote-splicing
		       "invalid context within quasiquote"
		       (list 'quasiquote (hyg:untag-no-tags form al)))]
		     [(pair? first)
		      (let ([car-first (qq (car first) level)])
			(if (and (eq? car-first 'unquote-splicing)
				 (list? first))
			    (let ([rest (cdr first)])
			      (if (or (not (pair? rest))
				      (not (null? (cdr rest))))
				  (raise-syntax-error
				   'unquote-splicing
				   "takes exactly one expression"
				   (list 'quasiquote (hyg:untag-no-tags form al)))
				  (list (list 'unquote-splicing
					      (if (zero? level)
						  (hyg:untag (cadr rest) al tmps)
						  (qq (cadr rest) (sub1 level)))
					      (qq (cdr x) level)))))
			    (cons (cons car-first
					(qq (cdr first) level))
				  (qq (cdr x) level))))]
		     [else
		      (cons first (qq (cdr x) level))]))]
		 [(vector? x)
		  (list->vector
		   (qq (vector->list x) level))]
		 [(box? x)
		  (box (qq (unbox x) level))]
		 [else (hyg:untag-no-tags x al)])))]
	   [hyg:untag-lambda
	    (lambda (formname bvv body al tmps)
	      (let ((tmps2 (append! (hyg:flatten bvv) tmps)))
		`(,formname ,bvv
		   ,@(hyg:untag-list body al tmps2))))]
	   [hyg:untag-letrec
	    (lambda (formname varvals body al tmps)
	      (let ((tmps (append! (map car varvals) tmps)))
		`(,formname
		     ,(map
		       (lambda (varval)
			 `(,(car varval)
			   ,(hyg:untag (cadr varval) al tmps)))
		       varvals)
		   ,@(hyg:untag-list body al tmps))))]
	   [hyg:untag-let
	    (lambda (formname varvals body al tmps)
	      (let ((tmps2 (append! (map car varvals) tmps)))
		`(,formname
		     ,(map
		       (lambda (varval)
			 `(,(car varval)
			   ,(hyg:untag (cadr varval) al tmps)))
		       varvals)
		   ,@(hyg:untag-list body al tmps2))))]
	   [hyg:untag-named-let
	    (lambda (formname lname varvals body al tmps)
	      (let ((tmps2 (cons lname (append! (map car varvals) tmps))))
		`(,formname ,lname
		   ,(map
		     (lambda (varval)
		       `(,(car varval)
			 ,(hyg:untag (cadr varval) al tmps)))
		     varvals)
		   ,@(hyg:untag-list body al tmps2))))]
	   [hyg:untag-let*
	    (lambda (varvals body al tmps)
	      (let ((tmps2 (append! (reverse! (map car varvals)) tmps)))
		`(let*
		     ,(let loop ((varvals varvals)
				 (i (length varvals)))
			(if (null? varvals) '()
			    (let ((varval (car varvals)))
			      (cons `(,(car varval)
				      ,(hyg:untag (cadr varval)
						  al (list-tail tmps2 i)))
				    (loop (cdr varvals) (- i 1))))))
		   ,@(hyg:untag-list body al tmps2))))]
	   [hyg:untag-do
	    (lambda (varinistps exit-test body al tmps)
	      (let ((tmps2 (append! (map car varinistps) tmps)))
		`(do
		     ,(map
		       (lambda (varinistp)
			 (let ((var (car varinistp)))
			   `(,var ,@(hyg:untag-list (cdr varinistp) al
						    (cons var tmps)))))
		       varinistps)
		     ,(hyg:untag-list exit-test al tmps2)
		   ,@(hyg:untag-list body al tmps2))))]
	   [hyg:untag-vanilla
	    (lambda (e al tmps)
	      (cond 
	       [(pair? e)
		(cons (hyg:untag-vanilla (car e) al tmps)
		      (hyg:untag-vanilla (cdr e) al tmps))]
	       [(vector? e)
		(list->vector
		 (hyg:untag-vanilla (vector->list e) al tmps))]
	       [(not (symbol? e)) e]
	       [(memq e tmps) e]
	       [(assq e al) => cdr]
	       [else e]))]
	   [hyg:flatten
	    (lambda (e)
	      (let loop ((e e) (r '()))
		(cond 
		 [(pair? e) (loop (car e)
				  (loop (cdr e) r))]
		 [(null? e) r]
		 [else (cons e r)])))])
    (set! -:sr:tag hyg:tag)
    (set! -:sr:untag hyg:untag)
    (set! -:sr:flatten hyg:flatten))

  (define -:sr:matches-pattern? 'undefined--:sr:matches-pattern?)
  (define -:sr:get-bindings 'undefined--:sr:get-bindings)
  (define -:sr:expand-pattern 'undefined--:sr:expand-pattern)

  (letrec ([mbe:position
	    (lambda (x l)
	      (let loop ((l l) (i 0))
		(cond ((not (pair? l)) #f)
		      ((equal? (car l) x) i)
		      (else (loop (cdr l) (+ i 1))))))]
           [mbe:append-map
            (lambda (f l)
              (let loop ((l l))
                (if (null? l) '()
                    (append (f (car l)) (loop (cdr l))))))]
	   [mbe:matches-pattern?
	    (lambda (p e k)
	      (cond 
	       [(mbe:ellipsis? p)
		     (and (or (null? e) (pair? e))
			  (let* ((p-head (car p))
				 (p-tail (cddr p))
				 (e-head=e-tail (mbe:split-at-ellipsis e p-tail)))
			    (and e-head=e-tail
				 (let ((e-head (car e-head=e-tail))
				       (e-tail (cdr e-head=e-tail)))
				   (and (andmap
					 (lambda (x) (mbe:matches-pattern? p-head x k))
					 e-head)
					(mbe:matches-pattern? p-tail e-tail k))))))]
	       [(pair? p)
		(and (pair? e)
		     (mbe:matches-pattern? (car p) (car e) k)
		     (mbe:matches-pattern? (cdr p) (cdr e) k))]
	       [(symbol? p) (if (memq p k) (eq? p e) #t)]
	       [else (equal? p e)]))]
	   [mbe:get-bindings
	    (lambda (p e k)
	      (cond 
	       [(mbe:ellipsis? p)
		(let* ((p-head (car p))
		       (p-tail (cddr p))
		       (e-head=e-tail (mbe:split-at-ellipsis e p-tail))
		       (e-head (car e-head=e-tail))
		       (e-tail (cdr e-head=e-tail)))
		  (cons (cons (mbe:get-ellipsis-nestings p-head k)
			      (map (lambda (x) (mbe:get-bindings p-head x k))
				   e-head))
			(mbe:get-bindings p-tail e-tail k)))]
	       [(pair? p)
		(append (mbe:get-bindings (car p) (car e) k)
			(mbe:get-bindings (cdr p) (cdr e) k))]
	       [(symbol? p)
		(if (memq p k) '() (list (cons p e)))]
	       [else '()]))]
	   [mbe:expand-pattern
	    (lambda (p r k)
	      (cond 
	       [(mbe:ellipsis? p)
		(append (let* ((p-head (car p))
			       (nestings (mbe:get-ellipsis-nestings p-head k))
			       (rr (mbe:ellipsis-sub-envs nestings r)))
			  (map (lambda (r1)
				 (mbe:expand-pattern p-head (append r1 r) k))
			       rr))
			(mbe:expand-pattern (cddr p) r k))]
	       [(pair? p)
		(cons (mbe:expand-pattern (car p) r k)
		      (mbe:expand-pattern (cdr p) r k))]
	       [(symbol? p)
		(if (memq p k) p
		    (let ((x (assq p r)))
		      (if x (cdr x) p)))]
	       [else p]))]
	   [mbe:get-ellipsis-nestings
	    (lambda (p k)
	      (let sub ((p p))
		(cond 
		 [(mbe:ellipsis? p) (cons (sub (car p)) (sub (cddr p)))]
		 [(pair? p) (append (sub (car p)) (sub (cdr p)))]
		 [(symbol? p) (if (memq p k) '() (list p))]
		 [else '()])))]
	   [mbe:ellipsis-sub-envs
            (lambda (nestings r)
              (let ((sub-envs-list 
                     (let loop ((r r) (sub-envs-list '()))
                       (if (null? r) (reverse! sub-envs-list)
                           (let ((c (car r)))
                             (loop (cdr r)
                                   (if (mbe:contained-in? nestings (car c))
                                       (cons (cdr c) sub-envs-list)
                                       sub-envs-list)))))))
                (case (length sub-envs-list)
                  ((0) #f)
                  ((1) (car sub-envs-list))
                  (else
                   (let loop ((sub-envs-list sub-envs-list) (final-sub-envs '()))
                     (if (ormap null? sub-envs-list) (reverse! final-sub-envs)
                         (loop (map cdr sub-envs-list)
                               (cons (mbe:append-map car sub-envs-list)
                                     final-sub-envs))))))))]
	   [mbe:contained-in?
	    (lambda (v y)
	      (if (or (symbol? v) (symbol? y)) (eq? v y)
		  (ormap (lambda (v_i)
			   (ormap (lambda (y_j)
				    (mbe:contained-in? v_i y_j))
				  y))
			 v)))]
	   [mbe:split-at-ellipsis
	    (lambda (e p-tail)
	      (if (null? p-tail) (cons e '())
		  (let ((i (mbe:position (car p-tail) e)))
		    (if i (cons (comlist:butlast e (- (length e) i))
				(list-tail e i))
			(error 'mbe:split-at-ellipsis "bad argument in syntax-rules")))))]
	   [mbe:ellipsis?
	    (lambda (x)
	      (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '...)))]
	   [comlist:butlast
	    (lambda (lst n)
	      (letrec ((l (- (length lst) n))
		       (bl (lambda (lst n)
			     (cond ((null? lst) lst)
				   ((positive? n)
				    (cons (car lst) (bl (cdr lst) (+ -1 n))))
				   (else '())))))
		(bl lst (if (negative? n)
			    (error 'butlast "negative argument in syntax-rules: ~s"
				   n)
			    l))))])
    (set! -:sr:matches-pattern? mbe:matches-pattern?)
    (set! -:sr:get-bindings mbe:get-bindings)
    (set! -:sr:expand-pattern mbe:expand-pattern))

  (define make-expander
    (lambda (who macro-name syn-rules)
      (if (or (not (pair? syn-rules))
	      (not (eq? (car syn-rules) 'syntax-rules)))
	  (error who "~s not an R5RS macro: ~s"
		 macro-name syn-rules)
	  (let ((keywords (cons macro-name (cadr syn-rules)))
		(clauses (cddr syn-rules)))
	    `(lambda macro-arg
	       (let ((macro-arg (cons ',macro-name macro-arg))
		     (keywords ',keywords))
		 (cond ,@(map
			  (lambda (clause)
			    (let ([in-pattern (car clause)]
				  [out-pattern (cadr clause)])
			      `((-:sr:matches-pattern? ',in-pattern macro-arg
						       keywords)
				(let ([tagged-out-pattern+alist
				       (-:sr:tag
					',out-pattern
					(append! (-:sr:flatten ',in-pattern)
						 keywords) '())])
				  (-:sr:untag
				   (-:sr:expand-pattern
				    (car tagged-out-pattern+alist)
				    (-:sr:get-bindings ',in-pattern macro-arg
						       keywords)
				    keywords)
				   (cdr tagged-out-pattern+alist)
				   '())))))
			  clauses)
		       (else (error ',macro-name "no matching clause: ~s"
				    ',clauses)))))))))

  (define define-syntax
    (lambda (macro-name syn-rules)
      (let ([expander (make-expander 'define-syntax macro-name syn-rules)])
	`(define-macro ,macro-name ,expander)))))
