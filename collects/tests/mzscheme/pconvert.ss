
(unless (defined? 'SECTION)
  (load-relative "testing.ss"))

(SECTION 'pconvert)

(require-library "pconver.ss")

(constructor-style-printing #t)
(quasi-read-style-printing #f)

(define (xl) 1)
(define (xu) (unit (import) (export)))
(define (xc) (class '() ()))

(begin
  (define-struct test (value constructor-sexp shared-constructor-sexp
			     quasi-sexp shared-quasi-sexp cons-as-list))
  
  (define-struct no-cons-test (value constructor-sexp shared-constructor-sexp
				     quasi-sexp shared-quasi-sexp))
  (define-struct same-test (value sexp))
  (define get-value
    (lambda (test-case)
      (cond
	[(test? test-case)
	 (test-value test-case)]
	[(no-cons-test? test-case)
	 (no-cons-test-value test-case)]
	[(same-test? test-case)
	 (same-test-value test-case)])))
  (define run-test
    (lambda (test-case)
      (let* ([before (get-value test-case)]
	     [cmp
	      (lambda (selector constructor-style? quasi-read? sharing? cons-as-list?)
		(unless (parameterize ([constructor-style-printing constructor-style?]
				       [show-sharing sharing?]
				       [quasi-read-style-printing quasi-read?]
				       [abbreviate-cons-as-list cons-as-list?])
			  (test (selector test-case) print-convert before))
		  (printf ">> (constructor-style-printing ~a) (quasi-read-style-printing ~a) (show-sharing ~a) (abbreviate-cons-as-list ~a)~n"
			  constructor-style? quasi-read? sharing? cons-as-list?)))])
	;(printf "testing: ~s~n" before)
	;(printf ".") (flush-output (current-output-port))
	(cond
	  [(test? test-case)
	   (cmp test-shared-constructor-sexp #t #f #t #t)
	   (cmp test-constructor-sexp #t #f #f #t)
	   (cmp test-shared-quasi-sexp #f #f #t #t)
	   (cmp test-quasi-sexp #f #f #f #t)
	   (cmp test-cons-as-list #t #f #f #f)]
	  [(no-cons-test? test-case)
	   (cmp no-cons-test-shared-constructor-sexp #t #f #t #t)
	   (cmp no-cons-test-constructor-sexp #t #f #f #t)
	   (cmp no-cons-test-shared-quasi-sexp #f #f #t #t)
	   (cmp no-cons-test-quasi-sexp #f #f #f #t)]
	  [(same-test? test-case)
	   (cmp same-test-sexp #t #t #t #t)
	   (cmp same-test-sexp #t #t #t #f)
	   (cmp same-test-sexp #t #t #f #t)
	   (cmp same-test-sexp #t #t #f #f)
	   (cmp same-test-sexp #t #f #t #t)
	   (cmp same-test-sexp #t #f #t #f)
	   (cmp same-test-sexp #t #f #f #t)
	   (cmp same-test-sexp #t #f #f #f)
	   (cmp same-test-sexp #f #t #t #t)
	   (cmp same-test-sexp #f #t #t #f)
	   (cmp same-test-sexp #f #t #f #t)
	   (cmp same-test-sexp #f #t #f #f)
	   (cmp same-test-sexp #f #f #t #t)
	   (cmp same-test-sexp #f #f #t #f)
	   (cmp same-test-sexp #f #f #f #t)
	   (cmp same-test-sexp #f #f #f #f)]))))

  (define 
    tests
    (list
     (make-same-test "abc" "abc")
     (make-same-test 8 8)
     (make-same-test 'a ''a)
     (make-test (list 1) '(list 1) '(list 1) '`(1) '`(1) '(cons 1 null))
     (make-same-test (vector 0 0 0 0 0 0 0 0 0 0) '(vector 0 0 0 0 0 0 0 0 0 0))
     (make-same-test (delay 1) '(delay ...))
     (make-same-test (let-struct a (a) (make-a 3)) '(make-a 3))
     (make-same-test (box 3) '(box 3))
     (make-test null 'null 'null '`() '`() 'null)
     (make-same-test add1 'add1)
     (make-same-test (void) '(void))
     (make-same-test (unit (import) (export)) '(unit ...))
     (make-same-test (make-weak-box 12) '(make-weak-box 12))
     (make-same-test (regexp "1") '(regexp ...))
     (make-same-test (lambda () 0) '(lambda () ...))
     (make-same-test xl 'xl)
     (make-same-test (letrec ([xl (lambda () 1)]) xl) '(lambda () ...))
     (make-same-test (letrec ([xl-ID-BETTER-NOT-BE-DEFINED (lambda () 1)]) 
		       xl-ID-BETTER-NOT-BE-DEFINED)
		     '(lambda () ...))
     (make-same-test xc 'xc)
     (make-same-test (letrec ([xc (class '() ())]) xc) '(class ...))
     (make-same-test (letrec ([xc-ID-BETTER-NOT-BE-DEFINED (class '() ())]) 
		       xc-ID-BETTER-NOT-BE-DEFINED)
		     '(class ...))
     (make-same-test xu 'xu)
     (make-same-test (letrec ([xu (unit (import) (export))]) xu)
		     '(unit ...))
     (make-same-test (letrec ([xu-ID-BETTER-NOT-BE-DEFINED (unit (import) (export))]) 
		       xu-ID-BETTER-NOT-BE-DEFINED)
		     '(unit ...))
     (make-same-test (lambda (x) x) '(lambda (a1) ...))
     (make-same-test (lambda x x) '(lambda args ...))
     (make-same-test (lambda (a b . c) a) '(lambda (a1 a2 . args) ...))
     (make-same-test (case-lambda) '(case-lambda))
     (make-same-test (case-lambda [() a] [(x) a]) '(case-lambda [() ...] [(a1) ...]))
     (make-same-test (case-lambda [() a] [(x y) a])
		     '(case-lambda [() ...] [(a1 a2) ...]))
     (make-same-test (case-lambda [() a] [(x . y) a])
		     '(case-lambda [() ...] [(a1 . args) ...]))
     (make-same-test (case-lambda [() a] [x a])
		     '(case-lambda [() ...] [args ...]))
     (make-same-test (case-lambda [() a] [(x y z) a] [x a])
		     '(case-lambda [() ...] [(a1 a2 a3) ...] [args ...]))
     (make-same-test (let ([ht (make-hash-table)])
		       (hash-table-put! ht 'x 1)
		       ht)
		     '(make-hash-table))
     (make-test (list 'a (box (list ())) (cons 1 '()))
		'(list (quote a) (box (list null)) (list 1))
		'(list (quote a) (box (list null)) (list 1))
		'`(a ,(box `(())) (1))
		'`(a ,(box `(())) (1))
		'(cons 'a 
		       (cons (box (cons null null))
			     (cons (cons 1 null)
				   null))))
     (make-test (let ([x (list 1)]) (set-car! x x) x)
		'(shared ([-0- (list -0-)]) -0-)
		'(shared ([-0- (list -0-)]) -0-)
		'(shared ([-0- `(,-0-)]) -0-)
		'(shared ([-0- `(,-0-)]) -0-)
		'(shared ([-0- (cons -0- null)]) -0-))
     (make-test (let ([x (list 1)]) (set-cdr! x x) x)
		'(shared ([-0- (cons 1 -0-)]) -0-)
		'(shared ([-0- (cons 1 -0-)]) -0-)
		'(shared ([-0- `(1 . ,-0-)]) -0-)
		'(shared ([-0- `(1 . ,-0-)]) -0-)
		'(shared ([-0- (cons 1 -0-)]) -0-))
     (make-test (let* ([a (list 1 2 3)]
		       [b (list 1 a (cdr a))])
		  (set-car! b b)
		  (append b (list (list 2 3))))
		'(shared ([-1- (list -1- (list 1 2 3) (list 2 3))])
		   (list -1- (list 1 2 3) (list 2 3) (list 2 3)))
		'(shared ([-1- (list -1- -3- -4-)]
			  [-3- (cons 1 -4-)]
			  [-4- (list 2 3)])
		   (list -1- -3- -4- (list 2 3)))
		'(shared ([-1- `(,-1- (1 2 3) (2 3))])
		   `(,-1- (1 2 3) (2 3) (2 3)))
		'(shared ([-1- `(,-1- ,-3- ,-4-)]
			  [-3- `(1 . ,-4-)]
			  [-4- `(2 3)])
		   `(,-1- ,-3- ,-4- (2 3)))
		'(shared ([-1- (cons -1- 
				     (cons (cons 1 (cons 2 (cons 3 null)))
					   (cons (cons 2 (cons 3 null))
						 null)))])
		   (cons -1- 
			 (cons (cons 1 (cons 2 (cons 3 null)))
			       (cons (cons 2 (cons 3 null)) 
				     (cons (cons 2 (cons 3 null))
					   null))))))
     (make-no-cons-test (let* ([a (list 1 2 3)]
			       [b (list 1 a (cdr a))])
			  (set-car! b b)
			  (let* ([share-list (append b (list (list 2 3)))]
				 [v (vector 1 share-list (cdr share-list))])
			    (vector-set! v 0 v)
			    v))
			'(shared
			     ((-0- (vector -0-
					   (list -2-
						 (list 1 2 3)
						 (list 2 3)
						 (list 2 3))
					   (list (list 1 2 3)
						 (list 2 3)
						 (list 2 3))))
			      (-2- (list -2- (list 1 2 3) (list 2 3))))
			   -0-)
			'(shared
			     ((-0- (vector -0- (cons -2- -8-) -8-))
			      (-2- (list -2- -4- -5-))
			      (-4- (cons 1 -5-))
			      (-5- (list 2 3))
			      (-8- (list -4- -5- (list 2 3))))
			   -0-)
			'(shared
			     ((-0- (vector -0-
					   `(,-2-
					     (1 2 3)
					     (2 3)
					     (2 3))
					   `((1 2 3)
					     (2 3)
					     (2 3))))
			      (-2- `(,-2- (1 2 3) (2 3))))
			   -0-)
			'(shared
			     ((-0- (vector -0- `(,-2- . ,-8-) -8-))
			      (-2- `(,-2- ,-4- ,-5-))
			      (-4- `(1 . ,-5-))
			      (-5- `(2 3))
			      (-8- `(,-4- ,-5- (2 3))))
			   -0-))))
  (for-each run-test tests))

(begin
  (define make-test-shared
    (lambda (shared?)
      (lambda (object output)
	(parameterize ([constructor-style-printing #t]
		       [show-sharing #t]
		       [quasi-read-style-printing #f]
		       [abbreviate-cons-as-list #t])
	  (test (if shared?
		    `(shared ((-1- ,output))
		       (list -1- -1-))
		    `(list ,output ,output))
		print-convert
		(list object object))))))
  (define test-shared (make-test-shared #t))
  (define test-not-shared (make-test-shared #f))

  (test-not-shared #t #t)
  (test-not-shared #f #f)
  (test-not-shared 1 1)
  (test-not-shared 3276832768 3276832768)
  (test-not-shared (regexp "") '(regexp ...))
  (let ([in (open-input-string "")]) (test-not-shared in in))
  (let ([out (open-output-string)]) (test-not-shared out out))
  (test-not-shared #\a #\a)
  (test-not-shared 'x ''x)
  (test-not-shared (lambda (x) x) '(lambda (a1) ...))
  (test-not-shared (make-promise (lambda () 1)) '(delay ...))
  (test-not-shared (class () ()) '(class ...))
  (test-not-shared (unit (import) (export)) '(unit ...))
  (test-not-shared (make-object (class () ())) '(make-object (class ...)))

  (test-shared "abc" "abc")
  (test-shared (list 1 2 3) '(list 1 2 3))
  (test-shared (vector 1 2 3) '(vector 1 2 3))
  (let-struct a () (test-shared (make-a) '(make-a)))
  (test-shared (box 1) '(box 1))
  (test-shared (make-hash-table) '(make-hash-table)))

(arity-test print-convert 1 2)
(arity-test build-share 1 1)
(arity-test get-shared 1 2)
(arity-test print-convert-expr 3 3)
(report-errs)
