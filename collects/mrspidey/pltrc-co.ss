;; pltrc-co.ss
;; Stuff that released code needs
;; ----------------------------------------------------------------------

(define-macro defmacro
  (lambda (name args . body)
    `(define-macro ,name (lambda ,args ,@body))))

(define (struct-expander-fn def-str struct:)
  (#%let ([make-exn make-exn:syntax]
	 [debug debug-info-handler])
  (#%lambda body
     (#%let ([syntax-error
	      (#%lambda (s)
		(#%raise
		 (make-exn
		  (#%format "~s: ~a" (cons def-str body) s)
		  ((debug))
		  (#%cons 'define-struct body))))]
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
		  (syntax-error "improper field list"))
	    (#%let* ([name (#%if (#%symbol? (#%car body))
			      (#%car body)
			      (#%caar body))]
		  [fields (#%cadr body)]
                  [fields
                   (map (lambda (arg)
                          (match arg
                            [((or ': '!) field type) field]
                            [(? symbol? field) field]
                            [x (syntax-error (format "field name not a identifier at ~s" x))]))
                        fields)])
	      `(#%define-values ,(build-struct-names name fields)
                                (,struct: ,(car body) ,fields)))))))

(#%define-macro define-const-typed-structure
  (struct-expander-fn ' define-const-typed-structure '#%struct))
(#%define-macro define-typed-structure
  (struct-expander-fn 'define-typed-structure  '#%struct))

;; ----------------------------------------------------------------------

(#%define-macro dynamic-let
  (#%let ([make-exn make-exn:syntax]
	  [debug debug-info-handler])
    (#%lambda (params . body)
     (#%let ([fail
	      (#%lambda (msg)
	       (#%raise (make-exn msg ((debug))
				  (#%list* 'dynamic-let params body))))])
      (#%if (#%null? body) (fail "dynamic-let: bad syntax (empty body)"))
      (#%if (#%null? params)
        `(#%begin ,@body)
	(#%if (#%or (#%not (#%pair? params))
		    (#%not (#%pair? (#%car params)))
		    (#%not (#%pair? (#%cdar params)))
		    (#%not (#%null? (#%cddar params))))
	      (fail "dynamic-let: bad syntax")
	      (#%let ([param (#%caar params)]
		      [orig (#%gensym)]
		      [pz (#%gensym)])
		 `(#%let* ([param ,param]
                           [,pz (if (parameter? param)
                                    (#%in-parameterization 
                                     (#%current-parameterization) ,param #t)
                                    param)]
			   [,orig (,pz)])
		     (#%dynamic-wind
		        (#%lambda () (,pz ,(#%cadar params)))
		        (#%lambda () (dynamic-let ,(cdr params) ,@body))
			(#%lambda () (,pz ,orig)))))))))))

;; ----------------------------------------------------------------------

(define cout 0)
(define wh-cout (box '()))

(defmacro let*-vals args
  (match args
    [(([varss exps] ...) . body)
      (set! cout (add1 cout))
      (printf "let*-vals ~s~n" cout)
      (let* ([varss (map (lambda (vars) 
                           (map 
                             (lambda (x) (if (eq? x '_) (gensym) x))
                             (if (symbol? vars) (list vars) vars)))
                      varss)]
              [binds (map list varss exps)])
        `(begin 
           (set-box! (global-defined-value 'wh-cout)
             (cons ,cout (unbox (global-defined-value 'wh-cout))))
           (let*-values ,binds 
             (begin            
               (set-box! (global-defined-value 'wh-cout)
                 (cdr (unbox (global-defined-value 'wh-cout))))
               . ,body))))]))

(defmacro let*-vals args
  (match args
    [(([varss exps] ...) . body)
      (let* ([varss (map (lambda (vars) 
                           (map 
                             (lambda (x) (if (eq? x '_) (gensym) x))
                             (if (symbol? vars) (list vars) vars)))
                      varss)]
              [binds (map list varss exps)])
        `(let*-values ,binds . ,body))]))

(defmacro for args
  (match args
    [(var base limit . body)
     (let ([loop (gensym)][l (gensym)])
       `(let ([,l ,limit])
          (recur ,loop ([,var ,base])
                 (when (< ,var ,l)
                   ,@body
                   (,loop (add1 ,var))))))]))

(define assert-on (make-parameter #t (lambda (x) x)))

(defmacro assert args
  (match args     
    [(exp . rest)
     (if (assert-on)
         `(unless ,exp 
            ,@(apply append
                     (map (lambda (r) `((display ,r) (newline))) rest))
            (error 'assert "Assertion failed: ~s" ',exp))
         `(void))]))

(defmacro eval-at-compile-time args
  (apply eval args))

;; ----------------------------------------------------------------------

'(unless (defined? '__keep-mrspidey-annotations)

   (defmacro begin-test-case exps '(void))
   ;;(defmacro define-type exps '(void))

   (defmacro define-typed-structure args
     (match args
       [(name.parent fields)
         `(define-struct 
            ,name.parent
            ,(map (match-lambda
                    [((or ': '!) (? symbol? s) type) s]
                    [(? symbol? s) s]
                    [field
                      (error 'define-typed-structure "Bad field ~s" field)])
               fields))]
       [_ (error 'define-typed-structure 
            "Bad syntax ~s" `(define-typed-structure ,@args))]))

   (defmacro define-const-typed-structure args
     `(define-typed-structure ,@args))

   (defmacro : args
     (match args     
       [(exp type) exp]))

   (defmacro cache-exp args
     (match args
       [(exp zafile) exp]))

   (defmacro cache-inv args
     (match args
       [(exp zafile) exp]))

   ;; (load "~cormac/scheme/remove-mrspidey-annotations.ss"))

   )

;;----------------------------------------------------------------------
