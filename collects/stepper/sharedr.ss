(unit/sig stepper:shared^
  (import [z : zodiac:system^]
	  [e : zodiac:interface^]
          stepper:client-procs^)
  
  ; A step-result is either:
  ; (make-before-after-result finished-exprs exp redex reduct)
  ; or (make-before-error-result finished-exprs exp redex err-msg)
  ; or (make-error-result finished-exprs err-msg)
  ; or (make-finished-result finished-exprs)
  (define-struct before-after-result (finished-exprs exp redex post-exp reduct))
  (define-struct before-error-result (finished-exprs exp redex err-msg))
  (define-struct error-result (finished-exprs err-msg))
  (define-struct finished-result (finished-exprs))
  
  (define (read-exprs text)
    (let ([reader (z:read (open-input-string text) 
                          (z:make-location 1 1 0 "stepper-string"))])
      (let read-loop ([new-expr (reader)])
        (if (z:eof? new-expr)
            ()
            (cons new-expr (read-loop (reader)))))))

  ; the closure record is placed in the closure table

  (define-struct closure-record (name mark constructor?))

  ; bogus-binding is used so that we can create legal zodiac bindings for temporary variables
  
  (define (create-bogus-binding name)
    (let* ([gensymed-name (gensym name)]
           [binding (z:make-lexical-binding #f #f #f (z:make-empty-back-box) 
                                            gensymed-name name)])
      (set-new-binding-name! binding gensymed-name)
      binding))

  ; make-binding-source creates a pool of bindings, indexed by arbitrary keys. These bindings
  ; not eq? to any other bindings, but a client can always get the same binding by
  ; invoking the resulting procedure with the same key (numbers work well). make-binding-source
  ; also takes a string which will be part of the printed representation of the binding's
  ; name; this makes debugging easier.
  ; make-gensym-source : (string -> (key -> binding))
  
  (define (make-binding-source id-string)
    (let ([assoc-table (make-hash-table-weak)])
      (lambda (key)
        (let ([maybe-fetch (hash-table-get assoc-table key (lambda () #f))])
          (or maybe-fetch
             (begin
               (let* ([new-binding (create-bogus-binding 
                                    (string-append id-string (format "~a" key) "-"))])
                 (hash-table-put! assoc-table key new-binding)
                 new-binding)))))))
  
  ; get-binding-name extracts the S-expression name for a binding. Zodiac
  ; creates a unique, gensym'd symbol for each binding, but the name is
  ; unreadable. Here, we create a new gensym, but the name of the generated
  ; symbol prints in the same way as the original symbol.
  
  (define (get-binding-name binding)
    (let ([name (lookup-new-binding-name binding)])
      (or name
	  (let* ([orig-name (z:binding-orig-name binding)]
		 [name (string->uninterned-symbol (symbol->string orig-name))])
	    (set-new-binding-name! binding name)
	    name))))

  (define-values (lookup-new-binding-name set-new-binding-name!)
    (let-values ([(getter setter) (z:register-client 'new-name (lambda () #f))])
      (values
       (lambda (parsed) (getter (z:parsed-back parsed)))
       (lambda (parsed n) (setter (z:parsed-back parsed) n)))))

  ; get-arg-binding maintains a list of bindings associated with the non-negative
  ; integers.  These symbols are used in the elaboration of applications; the nth
  ; in the application is evaluated and stored in a variable whose name is the nth
  ; gensym supplied by get-arg-symbol.
  
  (define get-arg-binding
    (make-binding-source "arg"))
  
  ; test cases: (returns #t on success)
  #| (let ([arg3 (get-arg-symbol 3)]
        [arg2 (get-arg-symbol 2)]
        [arg1 (get-arg-symbol 1)]
        [arg2p (get-arg-symbol 2)])
    (and (not (eq? arg3 arg2))
         (not (eq? arg3 arg1))
         (not (eq? arg3 arg2p))
         (not (eq? arg2 arg1))
         (eq? arg2 arg2p)
         (not (eq? arg1 arg2p))))
  |#
  
  ; gensyms needed by many modules:

  ; no-sexp is used to indicate no sexpression for display.
  ; e.g., on an error message, there's no sexp.
  (define no-sexp (gensym "no-sexp-"))

  ; *unevaluated* is the value assigned to temps before they are evaluated.
  (define *unevaluated* (gensym "unevaluated-"))
 
  ; if-temp : uninterned-symbol
  (define if-temp (create-bogus-binding "if-temp-"))

  ; struct-flag : uninterned symbol
  (define struct-flag (gensym "struct-flag-"))
  
  ; highlight-placeholder : uninterned symbol
  (define highlight-placeholder (gensym "highlight-placeholder"))

  ; list-partition takes a list and a number, and returns two lists; the first one contains the
  ; first n elements of the list, and the second contains the remainder.  If n is greater than
  ; the length of the list, the exn:application:mismatch exception is raised.
  
  (define (list-partition lst n)
    (if (= n 0)
        (values null lst)
        (if (null? lst)
            (list-ref lst 0) ; cheap way to generate exception
            (let-values ([(first rest) (list-partition (cdr lst) (- n 1))])
              (values (cons (car lst) first) rest)))))

  ; to perform source correlation, we use the 'register-client' ability of zodiac to
  ; add fields to parsed structures at runtime.
  
  (define expr-read read-getter)
  (define set-expr-read! read-setter)
  
  (define (list-take n a-list)
    (if (= n 0)
        null
        (cons (car a-list) (list-take (- n 1) (cdr a-list)))))
  
  (define (flatten-take n a-list)
    (apply append (list-take n a-list)))
  
  (define-values (closure-table-put! closure-table-lookup)
    (let ([closure-table (make-hash-table-weak)])
      (values
       (lambda (key value)
	 (hash-table-put! closure-table key value))
       (lambda args ; key or key & failure-thunk
         (apply hash-table-get closure-table args)))))) 