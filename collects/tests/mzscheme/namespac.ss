
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'namespaces)

(define flag-map
  (list (list 'constants 
	      'no-constants
	      '(#%define car 5) 
	      exn:misc:constant?
	      #f)
	(list 'keywords 
	      'no-keywords
	      '(#%let ([#%lambda 7]) (void)) 
	      exn:syntax?
	      #f)
	(list 'call/cc=call/ec
	      'call/cc!=call/ec
	      '((call/cc (#%lambda (x) x)) void)
	      exn:misc:continuation?
	      #f)
	(list 'hash-percent-syntax
	      'all-syntax
	      '(if #t (void))
	      exn:variable?
	      #f)))

(define (do-one-by-one more less)
  (let loop ([n (length flag-map)])
    (unless (zero? n)
	    (let ([test-info
		   (let loop ([l flag-map][p 1])
		     (if (null? l)
			 '()
			 (let* ([g (car l)]
				[g++ (cdddr g)])
			   (cons
			    (cond
			     [(= p n) (cons (less g) (less g++))]
			     [else (cons (more g) (more g++))])
			    (loop (cdr l) (add1 p))))))])
	      (let* ([flags (map car test-info)]
		     [namespace (apply make-namespace flags)])
		(printf "trying: ~s~n" flags)
		(let loop ([tests (map caddr flag-map)]
			   [results (map cdr test-info)])
		  (if (null? results)
		      '()
		      (begin
			(if (car results)
			    (error-test 
			     `(with-handlers ([(#%lambda (x) #f) void]) ; outside parameterize re-raises exns after escaping
				(parameterize ([current-namespace ,namespace])
				  (eval ',(car tests))))
			     (car results))
			    (with-handlers ([(lambda (x) #f) void]) 
			      (parameterize ([current-namespace namespace])
			        (test (void) eval (car tests)))))
			(loop (cdr tests) (cdr results)))))))
	    (loop (sub1 n)))))

(unless (defined? 'building-flat-tests)
    (do-one-by-one car cadr)
    (do-one-by-one cadr car))

; Test primitive-name
(let ([gvl (parameterize ([current-namespace (make-namespace)]) (make-global-value-list))]
      [aliases (list (cons "call/cc" "call-with-current-continuation")
		     (cons "call/ec" "call-with-escaping-continuation")
		     (cons "unit/sig?" "unit-with-signature?")
		     (cons "unit/sig->unit" "unit-with-signature-unit")
		     (cons "unit-with-signature->unit" "unit-with-signature-unit"))])
  (test #t 'names
	(andmap
	 (lambda (nv-pair)
	   (let ([name (car nv-pair)]
		 [value (cdr nv-pair)])
	     (or (not (primitive? value))
		 (let* ([s (symbol->string name)]
			[sr (if (char=? #\# (string-ref s 0))
				(substring s 2 (string-length s))
				s)]
			[st (let ([m (assoc sr aliases)])
			      (if m
				  (cdr m)
				  sr))])
		   (equal? st (primitive-name value))))))
	 gvl)))

(define (test-empty . flags)
  (let ([e (apply make-namespace flags)])
    (parameterize ([current-namespace e])
      (test null make-global-value-list)
      (test 'unbound 'empty-namespace
	    (with-handlers ([void (lambda (exn) 'unbound)])
			   (eval 'car)))
      (test 'unbound 'empty-namespace
	    (with-handlers ([void (lambda (exn) 'unbound)])
			   (eval '#%car)))
      (global-defined-value 'hello 5)
      (test 5 'empty-namespace (eval 'hello))
      (test '((hello . 5)) make-global-value-list))))
(test-empty 'empty)
(apply test-empty (append '(empty) (map car flag-map) (map cadr flag-map)))
