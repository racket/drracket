(require-library "macro.ss")
(require-library "match.ss")

(define-macro rmatch 
  (local [(define-struct child (destruct values traversal))
	  (define-struct traversal (children))
	  
	  (define-struct id (id))
	  (define-struct prepeat (pattern))
	  (define-struct trepeat (traversal))
	  
	  (define parse-pattern
	    (lambda (pattern)
	      (cond
		[(pair? pattern)
		 (let ([p1 (car pattern)]
		       [p2 (cdr pattern)])
		   (let*-values ([(left left-values left-names) (parse-pattern p1)]
				 [(right right-values right-names) (parse-pattern p2)])
		     (values 
		      (make-traversal (list (make-child '#%car left-values left) (make-child '#%cdr right-values right)))
		      (+ left-values right-values)
		      (append left-names right-names))))]
		[(box? pattern)
		 (let ([p (unbox pattern)])
		   (let*-values ([(sub-pat sub-values sub-names) (parse-pattern p)])
		     (values 
		      (make-traversal (list (make-child '#%unbox sub-values sub-pat)))
		      sub-values
		      sub-names)))]
		[(vector? pattern)
		 (let ([pats (vector->list pattern)])
		   (let loop ([pats pats]
			      [index 0]
			      [l null]
			      [vals 0]
			      [names null])
		     (cond
		       [(null? pats) (values (make-traversal (reverse l))
					     vals
					     (reverse names))]
		       [else (let-values ([(sub-pat sub-values sub-names) (parse-pattern (car pats))])
			       (loop (cdr pats)
				     (+ index 1)
				     (cons (make-child `(#%lambda (v) (#%vector-ref v ,index)) sub-values sub-pat)
					   l)
				     (+ vals sub-values)
				     (append sub-names names)))])))]
		[(prepeat? pattern)
		 (let-values ([(sub-traversal sub-values sub-names)
			       (parse-pattern (prepeat-pattern pattern))])
		   (values (make-trepeat sub-traversal)
			   sub-values
			   sub-names))]
		[(id? pattern)
		 (values pattern
			 1
			 (list (id-id pattern)))]
		[(or (symbol? pattern)
		     (null? pattern)
		     (number? pattern)
		     (char? pattern)
		     (string? pattern))
		 (values pattern 0 null)]
		[else (error 'parse-pattern "unrecognized pattern: ~a" pattern)])))
	  
	  (define n-names
	    (lambda (n prefix)
	      (cond
		[(zero? n) null]
		[else (cons (gensym prefix) (n-names (sub1 n) prefix))])))
	  
	  (define traverse-traversal
	    (lambda (t above-name)
	      (cond
		[(traversal? t)
		 (let* ([children (traversal-children t)])
		   (let ([child-namess (map (lambda (c) (n-names (child-values c) "child")) children)]
			 [piece-names (map (lambda (c) (gensym "destruct")) children)]
			 [build-piece
			  (lambda (name child)
			    `[,name (,(child-destruct child) ,above-name)])]
			 [recur (lambda (child piece-name child-names)
				  `[,child-names ,(traverse-traversal (child-traversal child) piece-name)])])
		     `(let ,(map build-piece piece-names children)
			(let-values ,(map recur children piece-names child-namess)
			  (values ,@(apply append child-namess))))))]
		[(trepeat? t)
		 (let* ([sub-t (trepeat-traversal t)]
			[l (gensym "...l")]
			[hd (gensym "...hd")]
			[loop (gensym "...loop")]
			[value-count
			 (cond
			   [(traversal? sub-t) (apply + (map child-values (traversal-children sub-t)))]
			   [(id? sub-t) 1]
			   [else 0])]
			[n-list (lambda (n f)
				  (let loop ([n n])
				    (cond
				      [(zero? n) null]
				      [else (cons (f (- n 1)) (loop (- n 1)))])))]
			[sub-names (n-list value-count
					   (lambda (n) (gensym (format "...sub~a" n))))]
			[rec-sub-names (n-list value-count
					       (lambda (n) (gensym (format "...rec~a" n))))])
		   `(let ,loop ([,l ,above-name])
		      (cond
			[(null? ,l) (values ,@(n-list value-count (lambda (i) 'null)))]
			[(pair? ,l) (let ([,hd (car ,l)])
				      (let-values ([,sub-names ,(traverse-traversal sub-t hd)]
						   [,rec-sub-names (,loop (cdr ,l))])
					(values ,@(map (lambda (sub rec) `(cons ,sub ,rec))
						       sub-names
						       rec-sub-names))))]
			[else (error 'rmatch "didn't find a list: found: ~a" ,above-name)])))]
					

		[(id? t) above-name]
		[(number? t) `(if (= ,t ,above-name)
				  (values)
				  (error 'rmatch "didn't find number: ~a, found: ~a" ,t ,above-name))]
		[(symbol? t) `(if (eq? ,above-name ',t)
				  (values)
				  (error 'rmatch "didn't find symbol: ~a, found: ~a" ',t ,above-name))]
		[(null? t) `(if (null? ,above-name)
				(values)
				(error 'rmatch "expected null, found: ~a" ,above-name))]
		[(string? t) `(if (and (string? ,above-name) (string=? ,above-name ,t))
				  (values)
				  (error 'rmatch "expected ~s, found: ~s" ,t ,above-name))]
		[(char? t) `(if  (eq? ,above-name ,t)
				 (values)
				 (error 'rmatch "expected ~s, found: ~s" ,t ,above-name))]
		[else (error 'traverse-traversal "unknown input: ~s~n" t)])))
	  
	  (define generate-code
	    (lambda (traversal names body)
	      (let ([input (gensym "input")])
		`(lambda (,input)
		   (let-values ([,names ,(traverse-traversal traversal input)])
		     ,body)))))
	  
	  ;; takes the text of a pattern
	  ;; and translates it into an example pattern
	  ;; with symbols at the positions 
	  ;; where identifers go
	  ;; eg '(cons x y) becomes (cons 'x 'y)
	  ;; and `(,x ,y) becomse (list 'x 'y)
	  (define translation-namespace (make-namespace 'empty))

	  (define _1
	    (let* ([tmp-namespace (make-namespace)]
		   [add-already-macro
		    (lambda (m-name)
		      (let ([m-value 
			     (parameterize ([current-namespace tmp-namespace])
			       (global-defined-value m-name))])
			(parameterize ([current-namespace translation-namespace])
			  (global-defined-value m-name m-value))))]
		   [add-not-yet-macro
		    (lambda (m-name function)
		      (parameterize ([current-namespace tmp-namespace])
			(eval `(define-macro ,m-name ,function)))
		      (add-already-macro m-name))])

	      (global-defined-value 'translation-namespace translation-namespace)

	      (add-already-macro 'quasiquote)
	      (add-already-macro '#%quasiquote)
	      (add-already-macro 'quote)
	      (add-already-macro '#%quote)
	      (add-not-yet-macro 'list 
				 (lambda in
				   (let loop ([l in])
				     (cond
				       [(null? l) null]
				       [else `(#%cons ,(car l) ,(loop (cdr l)))]))))
	      (add-not-yet-macro '... (lambda (x) `(#%repeat ,x)))
	      (add-not-yet-macro 'repeat (lambda (x) `(#%repeat ,x)))
	      (add-not-yet-macro 'box (lambda (x) `(#%box ,x)))
	      (add-not-yet-macro 'cons (lambda (x y) `(#%cons ,x ,y)))
	      (add-not-yet-macro 'vector (lambda x `(#%vector ,@x)))))
	  
	  (define translate-pattern
	    (lambda (pattern)
	      (let ([expanded (parameterize ([current-namespace translation-namespace])
				(expand-defmacro pattern))]
		    [dups (make-hash-table)])
		(values
		 dups
		 (let loop ([in expanded])
		   (match in
		     [`(#%repeat ,p) (make-prepeat (loop p))]
		     [`(#%cons ,p1 ,p2) (cons (loop p1) (loop p2))]
		     [`(#%box ,p) (box (loop p))]
		     [`(#%vector ,@ps) (apply vector (map loop ps))]
		     [`(#%quote ,p) p]
		     [`null null]
		     [(? symbol? in) 
		      (let/ec k
			(let ([new-id (gensym "rmatch:dup")])
			  (hash-table-put! 
			   dups
			   in
			   (cons
			    new-id
			    (hash-table-get 
			     dups 
			     in (lambda ()
				  (hash-table-put! dups in null)
				  (k (make-id in))))))
			  (make-id new-id)))]
		     [(or (? number? in) (? char? in) (? string? in)) in]
		     [(? null? in) null]
		     [x (error 'translate-pattern "unrecognized pattern: ~s, in ~s"
			       x pattern)]))))))
	  
	  (define main
	    (lambda (expression pattern-body-pairs)
	      (let*-values ([(main) (gensym "main")]
			    [(inner-k) (gensym "inner-k")]
			    [(outer-k) (gensym "outer-k")])
		(unless (and (list? pattern-body-pairs)
			     (andmap (lambda (x) (and (list? x) (<= 2 (length x))))
				     pattern-body-pairs))
		  (error 'rmatch
			 "expected body of lists of length at least two, patterns and results, got: ~a"
			 pattern-body-pairs))
		`(let ([,main ,expression])
		   (let/ec ,outer-k
		     ,@(map (lambda (pattern-body-pair)
			      (let*-values ([(pattern) (car pattern-body-pair)]
					    [(bodies) (cdr pattern-body-pair)]
					    [(dups translation) (translate-pattern pattern)]
					    [(traversal names-count names)
					     (parse-pattern translation)]
					    [(code) (traverse-traversal traversal main)]
					    [(dup-vars) (let loop ([dups (hash-table-map dups cons)])
							  (cond
							    [(null? dups) null]
							    [else (if (= (length (car dups)) 1)
								      (loop (cdr dups))
								      (cons (car dups) (loop (cdr dups))))]))])
				`(let/ec ,inner-k
				   (let-values ([,names 
						 (parameterize ([current-exception-handler
								 (lambda (exn)
								   ;(display (exn-message exn))
								   ;(newline)
								   (,inner-k #f))])
						   ,code)])
				     (unless (and ,@(map (lambda (vars) `(equal? ,@vars))
							 dup-vars))
				       (,inner-k #f))
				     (call-with-values (lambda () (begin ,@bodies))
						       ,outer-k)))))
			    pattern-body-pairs)
		     (error 'rmatch "no patterns matched"))))))]
    (lambda (x y) (main x y))))

(define-macro let+
  (lambda (bindings . bodies)
    (let* ([syn-error
	    (lambda (msg expr)
	      (raise-syntax-error 'let+ msg
				  `(let+ ,bindings ,@bodies)
				  expr))]
	   [expand-pattern
	    (lambda (x)
	      (match x
		[`(values ,(? symbol? x) ...) x]
		[(? symbol? x) `(,x)]
		[x (syn-error "invalid pattern" x)]))]
	   [get-patterns
	    (lambda (x)
	      (match x
		[`(values ,x ...) x]
		[else `(,x)]))]

	   [single-binding
	    (lambda (binding E body)
	      (let* ([patterns (get-patterns binding)]
		     [gensyms (map (lambda (x) (gensym "pattern")) patterns)])
		`(let-values ([,gensyms ,E])
		  ,(let loop ([patterns patterns]
			      [gensyms gensyms])
		     (cond
		       [(null? patterns) body]
		       [else `(rmatch ,(car gensyms)
				      ([,(car patterns)
					,(loop (cdr patterns) (cdr gensyms))]))])))))]
	   [multiple-bindings
            (lambda (binding E body)
              `(let-values ,(map list (map expand-pattern binding) E)
                     ,body))]
           [recursive-single-binding
	    (lambda (binding E body)
	      `(letrec-values ([,(expand-pattern binding) ,E])
			      ,body))]
           [recursive-multiple-bindings
            (lambda (binding E body)
              `(letrec-values ,(map (lambda (x y) `(,x ,y))
				    (map expand-pattern binding)
				    E)
			      ,body))]
	   [translate-binding
	    (lambda (binding body)
	      (match binding
		[`(val ,B ,E) (single-binding B E body)]
		[`(vals (,B ,E) ...) (multiple-bindings B E body)]
		[`(rec ,B ,E) (recursive-single-binding B E body)]
		[`(recs (,B ,E) ...) (recursive-multiple-bindings B E body)]
		[`(_ ,E ...) `(begin ,@E ,body)]
		[x (syn-error "invalid binding" x)]))])
      (unless (and (list? bindings)
		   (andmap (lambda (x) (and (list? x)
					    (<= 2 (length x))
					    (symbol? (car x))))
			   bindings))
	(syn-error "invalid syntax" bindings))
      (let loop ([l bindings])
	(cond
	 [(null? l) `(begin ,@bodies)]
	 [else (translate-binding (car l) (loop (cdr l)))])))))


#|
(define (test)

  (define (test-at-x result name)
    (lambda (pattern value)
      (let ([test-result (with-handlers ([(lambda (x) 'EXCEPTION-RAISED)
					  (lambda (x) 
					    (display
					     (if (exn? x)
						 (exn-message x)
						 x))
					    (newline)
					    #f)])
			   (eval `(rmatch ,value
					  ([,pattern x]))))])
	(unless (equal? result test-result)
	  (error name "failed with pattern: ~s and value ~s, got: ~s" pattern value test-result)))))
  
  (define (test-equal ans expr)
    (let ([got (with-handlers ([(lambda (x) #T)
				     (lambda (x)
				       (display
					(if (exn? x)
					    (exn-message x)
					    x))
				       (newline)
				       'DIFFERENT-FROM-IT-ALL)])
		      (eval expr))])
      (unless (equal? ans got)
	(error 'test-equal "~s:~nexpected ~s~n     got:~s~n" expr ans got))))
				       
  (define 3-at-x (test-at-x 3 '3-at-x))
  (define 33s-at-x (test-at-x (list 3 3 3) '33s-at-x))

  (define (plus-val-xy bindings)
    (unless (equal? (cons 1 3)
		    (with-handlers ([(lambda (x) #t)
				     (lambda (x) 
				       (printf "~a~n"
					       (if (exn? x)
						   (exn-message x)
						   x))
				       #f)])
		      (eval `(let+ ,bindings
				   (cons x y)))))
      (error 'plus-val-xy "failed with bindings: ~s" bindings)))

  (printf "starting test suite~n")

  (plus-val-xy '([val x 1] [val y 3]))
  (plus-val-xy '([val (values x y) (values 1 3)]))
  (plus-val-xy '([val (values (box x) (list y)) (values (box 1) (list 3))]))
  (plus-val-xy '([vals [x 1] [y 3]]))
  (plus-val-xy '([vals [(values x y) (values 1 3)]]))
  ;(plus-val-xy '([vals [(values (box x) (list y)) (values (box 3) (list 3))]]))

  (3-at-x 'x 3)
  (3-at-x '(cons x y) '(cons 3 2))
  (3-at-x '(list x) '(list 3))
  (3-at-x '(list 2 x) '(list 2 3))
  (3-at-x '(vector x) '(vector 3))
  (3-at-x '(vector x 2) '(vector 3 2))
  (3-at-x '(vector 2 x) '(vector 2 3))
  (3-at-x '(box x) '(box 3))
  (3-at-x '(box (box x)) '(box (box 3)))
  (3-at-x '(vector 'x `(3) `(,x)) '(vector 'x (list 3) (list 3)))
  (3-at-x '(vector 'x "abc" #\f `(3) `(,x)) '(vector 'x "abc" #\f (list 3) (list 3)))
  (3-at-x '`(box ,x) ''(box 3))

  (test-equal 3 '(rmatch (list 3 4) ([(list x x) x] [(list x 4) x])))
  (test-equal 3 '(rmatch (list 3 3) ([(list x x) x])))

  (test-equal (list 3) '(rmatch (list 3) ([(repeat x) x])))
  (test-equal (list 'x 'z)
	      '(rmatch (list (list 'x 'y) (list 'z 'w)) ([(repeat `(,x ,y)) x])))
  (test-equal
   `(x 1 y)
   '(rmatch '(letrec ([x 1]) y) ([`(letrec ([,x ,v]) ,b) (list x v b)])))
  
  (test-equal
   `((x) (1) y)
   '(rmatch '(letrec ([x 1]) y) ([`(letrec ,(repeat `[,x ,v]) ,b) (list x v b)])))
  
  (33s-at-x '(... (box x)) '(list (box 3) (box 3) (box 3)))

  (test-equal 3 '(let ([x 3]) (let+ ([vals [x 1] [y x]]) y)))
  (test-equal 3 '(let ([x 3]) (let+ ([val x 3] [vals [x 1] [y x]]) y)))

  (test-equal 3 '(let+ ([rec (values x) (lambda (y) (if y 3 (x #t)))]) (x #f)))
  (test-equal 3 '(let+ ([recs [x (lambda (y) (if y 3 (x #t)))]]) (x #f)))
  (test-equal 3 '(let+ ([recs [(values x) (lambda (y) (if y 3 (x #t)))]]) (x #f)))

  (test-equal 3 '(let+ ([rec a (lambda () a)] [val b (a)]) 3))

  (printf "all tests passed~n")
  )

(test)
|#