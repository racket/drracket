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
  
  ; bogus-varref is used so that we can create legal zodiac varrefs for temporary variables
  
  (define (create-bogus-bound-varref name binding)
    (z:make-bound-varref #f #f #f #f name binding))
  
  (define (create-bogus-top-level-varref name)
    (z:make-top-level-varref #f #f #f #f name))

  ; gensyms needed by many modules:

  ; no-sexp is used to indicate no sexpression for display.
  ; e.g., on an error message, there's no sexp.
  (define no-sexp (gensym "no-sexp-"))

  ; *unevaluated* is the value assigned to temps before they are evaluated.
  (define *unevaluated* (gensym "unevaluated-"))
 
  ; if-temp : uninterned-symbol
  (define if-temp (gensym "if-temp-"))

  ; struct-flag : uninterned symbol
  (define struct-flag (gensym "struct-flag-"))
  
  ; highlight-placeholder : uninterned symbol
  (define highlight-placeholder (gensym "highlight-placeholder"))

  ; make-gensym-source creates a pool of gensyms, indexed by arbitrary keys. These gensyms
  ; not eq? to any other symbols, but a client can always get the same symbol by
  ; invoking the resulting procedure with the same key (numbers work well). make-gensym-source
  ; also takes a string which will be part of the printed representation of the symbol;
  ; this makes debugging easier.
  ; make-gensym-source : (string -> (key -> symbol))
  
  (define (make-gensym-source id-string)
    (let ([assoc-table (make-hash-table-weak)])
      (lambda (key)
        (let ([maybe-fetch (hash-table-get assoc-table key (lambda () #f))])
          (or maybe-fetch
             (begin
               (let ([new-sym (gensym (string-append id-string (format "~a" key) "-"))])
                 (hash-table-put! assoc-table key new-sym)
                 new-sym)))))))
  
  ; get-arg-varref maintains a list of gensyms associated with the non-negative
  ; integers.  These symbols are used in the elaboration of applications; the nth
  ; in the application is evaluated and stored in a variable whose name is the nth
  ; gensym supplied by get-arg-symbol.
  
  (define get-arg-varref
    (let ([gensym-source (make-gensym-source "arg")])
      (lambda (arg-num)
        (create-bogus-bound-varref (gensym-source arg-num) #f))))
  
  ; top-level-exp-gensym-source hands out gensyms for the expressions which are not top-level
  ; defines. these expressions' results are bound to variables named by these gensyms. Note that 
  ; this implementation depends on putting exprs in hash tables and thus on non-copying
  ; garbage collection.
  
  (define top-level-exp-gensym-source
    (make-gensym-source "top-level-exp"))

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