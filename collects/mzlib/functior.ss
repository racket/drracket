(unit/sig
    mzlib:function^
  (import)
  
  (define true #t)
  (define false #f)
  
  (define identity (polymorphic (lambda (x) x)))
  
  (define compose
    (polymorphic
     (case-lambda 
      [(f) (if (procedure? f) f (raise-type-error 'compose "procedure" f))]
      [(f g)
       (let ([f (compose f)]
             [g (compose g)])
         (if (eqv? 1 (arity f)) ; optimize: don't use call-w-values
             (if (eqv? 1 (arity g)) ; optimize: single arity everywhere
                 (lambda (x) (f (g x)))
                 (lambda args (f (apply g args))))
             (if (eqv? 1 (arity g)) ; optimize: single input
                 (lambda (a)
                   (call-with-values
                    (lambda () (g a))
                    f))
                 (lambda args
                   (call-with-values
                    (lambda () (apply g args))
                    f)))))]
      [(f . more)
       (let ([m (apply compose more)])
         (compose f m))])))
  
  (define quicksort
    (polymorphic
     (lambda (l less-than)
       (let* ([v (list->vector l)]
              [count (vector-length v)])
         (let loop ([min 0][max count])
           (if (< min (sub1 max))
               (let ([pval (vector-ref v min)])
                 (let pivot-loop ([pivot min]
                                  [pos (add1 min)])
                   (if (< pos max)
                       (let ([cval (vector-ref v pos)])
                         (if (less-than cval pval)
                             (begin
                               (vector-set! v pos (vector-ref v pivot))
                               (vector-set! v pivot cval)
                               (pivot-loop (add1 pivot) (add1 pos)))
                             (pivot-loop pivot (add1 pos))))
                       (if (= min pivot)
                           (loop (add1 pivot) max)
                           (begin
                             (loop min pivot)
                             (loop pivot max))))))))
         (vector->list v)))))
  
  (define ignore-errors
    (polymorphic
     (lambda (thunk)
       (let/ec escape
         (with-handlers ([void (lambda (x) (escape (void)))])
           (thunk))))))
  
  (define remove
    (polymorphic
     (letrec ([rm (case-lambda 
                   [(item list) (rm item list equal?)]
                   [(item list equal?)
                    (let loop ([list list])
                      (cond
			[(null? list) ()]
			[(equal? item (car list)) (cdr list)]
			[else (cons (car list)
				    (loop (cdr list)))]))])])
       rm)))
  
  (define remq
    (polymorphic
     (lambda (item list)
       (remove item list eq?))))
  
  (define remv
    (polymorphic
     (lambda (item list)
       (remove item list eqv?))))
  
  (define remove* 
    (polymorphic
     (case-lambda
      [(l r equal?)
       (if (null? l)
           r
           (remove* (cdr l) (remove (car l) r equal?) equal?))]
      [(l r) (remove* l r equal?)])))
  
  (define remq*
    (polymorphic
     (lambda (l r)
       (remove* l r eq?))))
  
  (define remv*
    (polymorphic
     (lambda (l r)
       (remove* l r eqv?))))
  
  ;; fold : ((A B -> B) B (listof A) -> B)
  ;; fold : ((A1 ... An B -> B) B (listof A1) ... (listof An) -> B)
  
  ;; foldl builds "B" from the beginning of the list to the end of the
  ;; list and foldr builds the "B" from the end of the list to the
  ;; beginning of the list.
  
  (define mapadd
    (polymorphic
     (lambda (f l last)
       (letrec ((helper
                 (lambda (l)
                   (cond
		     [(null? l) (list last)]
		     [else (cons (f (car l)) (helper (cdr l)))]))))
         (helper l)))))
  
  (define foldl
    (polymorphic
     (letrec ((fold-one
               (lambda (f init l)
                 (letrec ((helper
                           (lambda (init l)
                             (cond
			       [(null? l) init]
			       [else (helper (f (car l) init) (cdr l))]))))
                   (helper init l))))
              (fold-n
               (lambda (f init  l)
                 (cond
		   [(ormap null? l)
		    (if (andmap null? l) 
			init
			(error 'foldl "received non-equal length input lists"))]
		   [else (fold-n
			  f
			  (apply f (mapadd car l init))
			  (map cdr l))]))))
       (case-lambda
        [(f init l) (fold-one f init l)]
        [(f init l . ls) (fold-n f init (cons l ls))]))))
  
  (define foldr
    (polymorphic
     (letrec ((fold-one
               (lambda (f init l)
                 (letrec ((helper
                           (lambda (init l)
                             (cond
			       [(null? l) init]
			       [else (f (car l) (helper init (cdr l)))]))))
                   (helper init l))))
              (fold-n
               (lambda (f init l)
                 (cond
		   [(ormap null? l)
		    (if (andmap null? l)
			init
			(error 'foldr "received non-equal length input lists"))]
		   [else (apply f
				(mapadd car l
					(fold-n f init (map cdr l))))]))))
       (case-lambda
        [(f init l) (fold-one f init l)]
        [(f init l . ls) (fold-n f init (cons l ls))]))))
  
  (define make-find
    (lambda (name whole-list?)
      (polymorphic
       (lambda (f list)
         (unless (and (procedure? f)
                      (procedure-arity-includes? f 1))
           (raise-type-error name "procedure (arity 1)" f))
         (let loop ([l list])
           (cond
	     [(null? l) #f]
	     [(not (pair? l)) 
	      (raise (make-exn:application:mismatch
		      (format "~a: second argument must be a (proper) list; given ~e" name list)
		      (current-continuation-marks)
		      list))]
	     [(f (car l)) (if whole-list? l (car l))]
	     [else (loop (cdr l))]))))))
  
  (define assf
    (make-find 'assf #f))
  
  (define memf
    (make-find 'memf #t))
  
  (define filter
    (polymorphic
     (lambda (f list)
       (unless (and (procedure? f)
                    (procedure-arity-includes? f 1))
         (raise-type-error 'filter "procedure (arity 1)" f))
       (let loop ([l list])
         (cond
	   [(null? l) null]
	   [(pair? l)
	    (let* ([keep? (f (car l))]
		   [frest (loop (cdr l))])
	      (if keep?
		  (cons (car l) frest)
		  frest))]
	   [else (raise (make-exn:application:mismatch
			 (format "filter: second argument must be a (proper) list; given ~e" list)
			 (current-continuation-marks)
			 list))])))))
  
  (define first (polymorphic (lambda (x) 
                               (unless (pair? x)
                                 (raise-type-error 'first "non-empty list" x))
                               (car x))))
  (define second (polymorphic cadr))
  (define third (polymorphic caddr))
  (define fourth (polymorphic cadddr))
  (define fifth (polymorphic (compose fourth cdr)))
  (define sixth (polymorphic (compose fourth cddr)))
  (define seventh (polymorphic (compose fourth cdddr)))
  (define eighth (polymorphic (compose fourth cddddr)))
  
  (define rest (polymorphic (lambda (x) 
                              (unless (pair? x)
                                (raise-type-error 'rest "non-empty list" x))
                              (cdr x))))
  
  (define  build-string
    (lambda  (n  fcn)
      (unless  (and (integer? n) (exact? n) (>= n 0))
        (error  'build-string  "~s must be an exact integer >= 0"  n))
      (unless  (procedure? fcn)
        (error  'build-string  "~s must be a procedure"  fcn))
      (let  ((str  (make-string n)))
        (let  loop  ((i  0))
          (if (= i n)  
              str
              (begin
                (string-set!  str  i  (fcn i))
                (loop  (add1 i))))))))
  
  ;; (build-vector n f) returns a vector 0..n-1 where the ith element is (f i).
  ;; The eval order is guaranteed to be: 0, 1, 2, ..., n-1.
  ;; eg: (build-vector 4 (lambda (i) i)) ==> #4(0 1 2 3)
  
  (define  build-vector
    (polymorphic
     (lambda  (n  fcn)
       (unless  (and (integer? n) (exact? n) (>= n 0))
         (error  'build-vector  "~s must be an exact integer >= 0"  n))
       (unless  (procedure? fcn)
         (error  'build-vector  "~s must be a procedure"  fcn))
       (let  ((vec  (make-vector n)))
         (let  loop  ((i  0))
           (if  (= i n)  vec
                (begin
                  (vector-set!  vec  i  (fcn i))
                  (loop  (add1 i)))))))))
  
  (define  build-list
    (polymorphic
     (lambda  (n  fcn)
       (unless  (and (integer? n) (exact? n) (>= n 0))
         (error  'build-list  "~s must be an exact integer >= 0"  n))
       (unless  (procedure? fcn)
         (error  'build-list  "~s must be a procedure"  fcn))
       (if  (zero? n)  '()
            (let  ([head  (list (fcn 0))])
              (let  loop  ([i 1]  [p head])
                (if  (= i n)  head
                     (begin
                       (set-cdr!  p  (list (fcn i)))
                       (loop  (add1 i)  (cdr p))))))))))
  
  (define loop-until
    (polymorphic
     (lambda (start done? next body)
       (let loop ([i start])
         (unless (done? i)
           (body i)
           (loop (next i)))))))
  
  (define last-pair
    (polymorphic
     (lambda (l)
       (if (pair? l)
           (if (pair? (cdr l))
               (last-pair (cdr l))
               l)
           (raise-type-error 'last-pair "pair" l)))))
  
  (define boolean=?
    (lambda (x y)
      (unless (and (boolean? x)
                   (boolean? y))
        (raise-type-error 'boolean=? 
                          "boolean"
                          (if (boolean? x) y x)))
      (eq? x y)))
  
  (define (symbol=? x y)
    (unless (and (symbol? x)
		 (symbol? y))
      (raise-type-error 'symbol=? "symbol"
			(if (symbol? x) y x)))
    (eq? x y))
  
  (define cons? (lambda (x) (pair? x)))
  (define empty? (lambda (x) (null? x)))
  (define empty '()))
