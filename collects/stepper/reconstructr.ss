(unit/sig stepper:reconstruct^
  (import [z : zodiac:system^]
          mzlib:function^
	  [e : stepper:error^]
          [utils : cogen-utils^]
          [b : userspace:basis^]
          [s : stepper:model^]
	  stepper:shared^)

  (define nothing-so-far (gensym "nothing-so-far-"))
  
  (define (mark-source mark)
    (car (mark)))
  
  (define (mark-bindings mark)
    (cddr (mark)))
  
  (define (mark-label mark)
    (cadr (mark)))
  
  (define (mark-binding-value mark-binding)
    ((car mark-binding)))
  
  (define (mark-binding-varref mark-binding)
    (cadr mark-binding))
  
  (define (expose-mark-list mark-list)
    (map (lambda (mark)
           (list (mark-label mark)
                 (mark-source mark)))
         mark-list))
  
  (define (find-var-binding mark-list var)
    (if (null? mark-list)
        ; must be a primitive
        (error 'find-var-binding "variable not found in environment: ~a" var)
	; (error var "no binding found for variable.")
	(let* ([bindings (mark-bindings (car mark-list))]
	       [matches (filter (lambda (mark-var)
				  (eq? var (z:varref-var (mark-binding-varref mark-var))))
                                bindings)])
	  (cond [(null? matches)
		 (find-var-binding (cdr mark-list) var)]
		[(> (length matches) 1)
		 (error 'find-var-binding "more than one variable binding found for var: ~a" var)]
		[else ; (length matches) = 1
		 (car matches)]))))

  (define memoized-read->raw
    (let ([table (make-hash-table-weak)])
      (lambda (read)
        (or (hash-table-get table read (lambda () #f))
            (let ([raw (z:sexp->raw read)])
              (hash-table-put! table read raw)
              raw)))))
  
  (define (make-apply-pred-to-raw pred)
    (lambda (expr)
      (pred (memoized-read->raw (expr-read expr)))))
             
  (define (make-check-raw-first-symbol symbol)
    (make-apply-pred-to-raw
     (lambda (raw)
       (and (pair? raw)
            (eq? (car raw) symbol)))))

  (define comes-from-define?
    (make-check-raw-first-symbol 'define))

  (define comes-from-define-procedure?
    (make-apply-pred-to-raw
     (lambda (raw) (and (pair? raw)
                        (eq? (car raw) 'define)
                        (pair? (cadr raw))))))
  
  (define comes-from-lambda-defined-procedure?
    (make-apply-pred-to-raw
     (lambda (raw) (and (pair? raw)
                        (eq? (car raw) 'define)
                        (pair? (caddr raw))
                        (eq? (caaddr raw) 'lambda)))))
  
  (define comes-from-define-struct?
    (make-check-raw-first-symbol 'define-struct))
  
  (define comes-from-cond?
    (make-check-raw-first-symbol 'cond))
  
  (define comes-from-lambda?
    (make-check-raw-first-symbol 'lambda))
  
  (define comes-from-case-lambda?
    (make-check-raw-first-symbol 'case-lambda))

  (define comes-from-and?
    (make-check-raw-first-symbol 'and))
  
  (define comes-from-or?
    (make-check-raw-first-symbol 'or))

  (define (rectify-value val)
    (let ([closure-record (closure-table-lookup val (lambda () #f))])
      (cond
        [closure-record
         (or (closure-record-name closure-record)
             (let ([mark (closure-record-mark closure-record)])
               (o-form-case-lambda->lambda 
                (rectify-source-expr (mark-source mark) (list mark) null))))]
        [else
         (s:print-convert val)])))
  
  (define (o-form-case-lambda->lambda o-form)
    (cond [(eq? (car o-form) 'lambda)
           o-form]
          [else ; o-form = case-lambda
           (let ([args (caadr o-form)]
                 [body-exps (cdr (cadr o-form))])
             `(lambda ,args ,@body-exps))]))
  
  (define (o-form-lambda->define o-form name)
    (let ([args (cadr o-form)]
          [body-exps (cddr o-form)])
      `(define (,name ,@args) ,@body-exps)))
  
  (define (final-mark-list? mark-list)
    (and (not (null? mark-list)) (eq? (mark-label (car mark-list)) 'final)))
 
  (define continuation? 
    (let ([r (regexp "#<continuation>")])
      (lambda (k)
        (let ([p (open-output-string)])
          (display k p)
          (not (not (regexp-match r (get-output-string p))))))))
  
  (define (skip-result-step? mark-list)
    (in-inserted-else-clause mark-list))
  
  (define (skip-redex-step? mark-list)
    (and (pair? mark-list)
         (let ([expr (mark-source (car mark-list))])
           (or (and (z:varref? expr)
                    (or (z:bound-varref? expr)
                        (let ([var (z:varref-var expr)])
                          (or (and (s:check-pre-defined-var var)
                                   (or (procedure? (s:global-lookup var))
                                       (eq? var 'empty)))
                              (call-with-current-continuation
                               (lambda (k)
                                 (with-handlers ([exn:variable?
                                                  (lambda (exn) (k #f))])
                                   (let ([val (mark-binding-value
                                               (find-var-binding mark-list (z:varref-var expr)))])
                                     (and (procedure? val)
                                          (not (continuation? val))
                                          (eq? var
                                               (closure-record-name 
                                                (closure-table-lookup val (lambda () (k #f))))))))))))))
               (and (z:app? expr)
                    (let ([fun-val (mark-binding-value
                                    (find-var-binding mark-list 
                                                      (z:varref-var (get-arg-symbol 0))))])
                      (and (procedure? fun-val)
                           (procedure-arity-includes? 
                            fun-val
                            (length (z:app-args expr)))
                           (or (and (s:constructor-style-printing?)
                                    (if (s:abbreviate-cons-as-list?)
                                        (eq? fun-val list) ; that needs exporting too.
                                        (and (s:user-cons? fun-val)
                                             (second-arg-is-list? mark-list))))
                               (s:user-vector? fun-val)
                               (and (eq? fun-val void)
                                    (eq? (z:app-args expr) null))
                               (struct-constructor-procedure? fun-val)
                               ; this next clause may be obviated by the previous one.
                               (let ([closure-record (closure-table-lookup fun-val (lambda () #f))])
                                 (and closure-record
                                      (closure-record-constructor? closure-record)))))))
               (in-inserted-else-clause mark-list)))))
  
  (define (second-arg-is-list? mark-list)
    (let ([arg-val (mark-binding-value (find-var-binding mark-list (z:varref-var (get-arg-symbol 2))))])
      (list? arg-val)))  
  
  (define (in-inserted-else-clause mark-list)
    (and (not (null? mark-list))
         (let ([expr (mark-source (car mark-list))])
           (or (and (z:zodiac? expr)
                    (not (z:if-form? expr))
                    (comes-from-cond? expr))
               (in-inserted-else-clause (cdr mark-list))))))
    
  (define (rectify-source-expr expr mark-list lexically-bound-vars)
    (let ([recur (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (cond [(z:varref? expr)
             (cond [(memq (z:varref-var expr) lexically-bound-vars)
                    (z:binding-orig-name (z:bound-varref-binding expr))]
                   [(z:top-level-varref? expr)
                    (z:varref-var expr)]
                   [else
                    (rectify-value (mark-binding-value (find-var-binding mark-list 
                                                                         (z:varref-var expr))))])]
            
            [(z:app? expr)
             (map recur (cons (z:app-fun expr) (z:app-args expr)))]
            
            [(z:struct-form? expr)
             (if (comes-from-define-struct? expr)
                 (e:internal-error expr "this expression should have been skipped during reconstruction")
                 (let ([super-expr (z:struct-form-super expr)]
                       [raw-type (utils:read->raw (z:struct-form-type expr))]
                       [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
                   (if super-expr
                       `(struct (,raw-type ,(recur super-expr))
                                ,raw-fields)
                       `(struct ,raw-type ,raw-fields))))]
            
            [(z:if-form? expr)
             (cond
               [(comes-from-cond? expr)
                `(cond ,@(rectify-cond-clauses (z:zodiac-start expr) expr mark-list lexically-bound-vars))]
               [(comes-from-and? expr)
                `(and ,@(rectify-and-clauses (z:zodiac-start expr) expr mark-list lexically-bound-vars))]
               [(comes-from-or? expr)
                `(or ,@(rectify-or-clauses (z:zodiac-start expr) expr mark-list lexically-bound-vars))]
               [else
                `(if ,(recur (z:if-form-test expr))
                     ,(recur (z:if-form-then expr))
                     ,(recur (z:if-form-else expr)))])]
            
            [(z:quote-form? expr)
             (let ([raw (utils:read->raw (z:quote-form-expr expr))])
               (rectify-value raw)
;               (cond [(or (string? raw)
;                          (number? raw)
;                          (boolean? raw)
;                          (s:image? raw))
;                      raw]
;                     [else
;                      `(quote ,raw)])
               )]

            [(z:case-lambda-form? expr)
             (let* ([arglists (z:case-lambda-form-args expr)]
                    [bodies (z:case-lambda-form-bodies expr)]
                    [o-form-arglists
                     (map (lambda (arglist) 
                            (utils:improper-map z:binding-orig-name
                                              (utils:arglist->ilist arglist)))
                          arglists)]
                    [var-form-arglists
                     (map (lambda (arglist)
                            (map z:binding-var (z:arglist-vars arglist)))
                          arglists)]
                    [o-form-bodies 
                     (map (lambda (body var-form-arglist)
                            (rectify-source-expr body 
                                                 mark-list
                                                 (append var-form-arglist
                                                         lexically-bound-vars)))
                          bodies
                          var-form-arglists)])
               (cond [(or (comes-from-lambda? expr) (comes-from-define? expr))
                      `(lambda ,(car o-form-arglists) ,(car o-form-bodies))]
                     [(comes-from-case-lambda? expr)
                      `(case-lambda ,@(map list o-form-arglists o-form-bodies))]
                     [else
                      (e:dynamic-error expr "unknown source for case-lambda")]))]
            
            ; we won't call rectify-source-expr on define-values expressions
            
            [else
             (print-struct #t)
             (e:dynamic-error
              expr
              (format "stepper:rectify-source: unknown object to rectify, ~a~n" expr))])))
 
  ; these macro unwinders (and, or) are specific to beginner level
  
  (define (rectify-and-clauses and-source expr mark-list lexically-bound-vars)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (if (and (z:if-form? expr) (equal? and-source (z:zodiac-start expr)))
          (cons (rectify-source (z:if-form-test expr))
                (rectify-and-clauses and-source (z:if-form-then expr) mark-list lexically-bound-vars))
          null)))
  
  (define (rectify-or-clauses or-source expr mark-list lexically-bound-vars)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (if (and (z:if-form? expr) (equal? or-source (z:zodiac-start expr)))
          (cons (rectify-source (z:if-form-test expr))
                (rectify-or-clauses or-source (z:if-form-else expr) mark-list lexically-bound-vars))
          null)))
  
  (define (rectify-cond-clauses cond-source expr mark-list lexically-bound-vars)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (if (equal? cond-source (z:zodiac-start expr))
          (if (z:if-form? expr)
              (cons (list (rectify-source (z:if-form-test expr))
                          (rectify-source (z:if-form-then expr)))
                    (rectify-cond-clauses cond-source (z:if-form-else expr) mark-list lexically-bound-vars))
              null)
          `((else ,(rectify-source expr))))))

  ; reconstruct-completed : reconstructs a completed expression or definition.  This now
  ; relies upon the s:global-lookup procedure to find values in the user-namespace.
  ; I'm not yet sure whether or not 'vars' must be supplied or whether they can be derived
  ; from the expression itself.
  
  (define (reconstruct-completed expr)    
      (cond [(z:define-values-form? expr)
             (if (comes-from-define-struct? expr)
                 (utils:read->raw (expr-read expr))
                 (let* ([vars (map z:varref-var (z:define-values-form-vars expr))]
                        [values (map s:global-lookup vars)]
                        [rectified-vars (map rectify-value values)])
                   (cond [(comes-from-define-procedure? expr)
                          (let* ([mark (closure-record-mark  (closure-table-lookup (car values)))]
                                 [rectified (rectify-source-expr (mark-source mark) (list mark) null)])
                            (o-form-lambda->define (o-form-case-lambda->lambda rectified)
                                                   (car vars)))]
                         [(comes-from-lambda-defined-procedure? expr)
                          (let* ([mark (closure-record-mark (closure-table-lookup (car values)))]
                                 [rectified (rectify-source-expr (mark-source mark) (list mark) null)])
                            `(define ,(car vars) ,(o-form-case-lambda->lambda rectified)))]
                         [(comes-from-define? expr)
                          `(define ,(car vars) ,(car rectified-vars))]
                         [else
                          `(define-values ,vars
                             ,(if (= (length values) 1)
                                  (car rectified-vars)
                                  `(values ,@rectified-vars)))])))]
            [(z:begin-form? expr) ; hack for xml stuff
             (utils:read->raw (expr-read expr))]
            [else
             (let ([value (s:global-lookup (top-level-exp-gensym-source expr))])
               (rectify-value value))]))
    
  ; reconstruct-current : takes a parsed expression, a list of marks, the kind of break, and
  ; any values that may have been returned at the break point. It produces a list containing the
  ; reconstructed sexp, and the (contained) sexp which is the redex.  If the redex is a heap value
  ; (and can thus be distinguished from syntactically identical occurrences of that value using
  ; eq?), it is embedded directly in the sexp. Otherwise, its place in the sexp is taken by the 
  ; highlight-placeholder, which is replaced by the highlighted redex in the construction of the 
  ; text%
  
  ; z:parsed (list-of mark) symbol (list-of value) -> 
  ; (list sexp sexp)

  (define (reconstruct-current expr mark-list break-kind returned-value-list)
    
    (local
        ((define (rectify-source-top-marks expr)
           (rectify-source-expr expr mark-list null))
         
         (define (rectify-top-level expr so-far)
           (if (z:define-values-form? expr)
               (let ([vars (z:define-values-form-vars expr)]
                     [val (z:define-values-form-val expr)])
                 (cond [(comes-from-define-struct? expr)
                        (let* ([struct-expr val]
                               [super-expr (z:struct-form-super struct-expr)]
                               [raw-type (utils:read->raw (z:struct-form-type struct-expr))]
                               [raw-fields (map utils:read->raw (z:struct-form-fields struct-expr))])
                          `(define-struct
                            ,(if super-expr
                                 (list raw-type so-far)
                                 raw-type)
                            ,raw-fields))]
                       [(or (comes-from-define-procedure? expr)
                            (and (comes-from-define? expr)
                                 (pair? so-far)
                                 (eq? (car so-far) 'lambda)))
                        (let* ([proc-name (z:varref-var
                                           (car (z:define-values-form-vars expr)))]
                               [o-form-proc so-far])
                          (o-form-lambda->define o-form-proc proc-name))]
                                              
                       [(comes-from-define? expr)
                        `(define 
                           ,(z:varref-var (car vars))
                           ,so-far)]
                       
                       [else
                        `(define-values 
                           ,(map utils:read->raw vars)
                           ,(rectify-source-top-marks val))]))
               so-far))
         
         (define (reconstruct-inner mark-list so-far)
           (let ([rectify-source-current-marks 
                  (lambda (expr)
                    (rectify-source-expr expr mark-list null))])
             (let* ([top-mark (car mark-list)]
                    [expr (mark-source top-mark)])
               (cond 
                 ; variable references
                 [(z:varref? expr)
                  (if (eq? so-far nothing-so-far)
                      (rectify-source-current-marks expr)
                      (e:dynamic-error expr 
                                       "variable reference given as context"))]
                 
                 ; applications
                 
                 [(z:app? expr)
                  (let* ([sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
                         [arg-temps (build-list (length sub-exprs) get-arg-symbol)]
                         [arg-temp-syms (map z:varref-var arg-temps)]
                         [arg-vals (map (lambda (arg-sym) 
                                          (mark-binding-value (find-var-binding mark-list arg-sym)))
                                        arg-temp-syms)])
                    (case (mark-label (car mark-list))
                      ((not-yet-called)
                       ;                         (printf "length of mark-list: ~s~n" (length mark-list))
                       ;                         (printf "mark has binding for third arg: ~s~n" 
                       ;                                 (find-var-binding (list (car mark-list)) (z:varref:var 
                       (letrec
                           ([split-lists
                             (lambda (exprs vals)
                               (if (or (null? vals)
                                       (eq? (car vals) *unevaluated*))
                                   (values null exprs)
                                   (let-values ([(small-vals small-exprs)
                                                 (split-lists (cdr exprs) (cdr vals))])
                                     (values (cons (car vals) small-vals) small-exprs))))])
                         (let-values ([(evaluated unevaluated) (split-lists sub-exprs arg-vals)])
                           (let* ([eval-exprs (list-take (length evaluated) sub-exprs)]
                                  [rectified-evaluated (map rectify-value evaluated)])
                             (if (null? unevaluated)
                                 rectified-evaluated
                                 (append rectified-evaluated
                                         (cons so-far
                                               (map rectify-source-current-marks (cdr unevaluated)))))))))
                      ((called)
                       (if (eq? so-far nothing-so-far)
                           `(...) ; in unannotated code
                           `(... ,so-far ...)))
                      (else
                       (e:static-error expr "bad label in application mark"))))]
                 
                 ; define-struct 
                 
                 [(z:struct-form? expr)
                  (if (comes-from-define-struct? expr)
                      so-far
                      (let ([super-expr (z:struct-form-super expr)]
                            [raw-type (utils:read->raw (z:struct-form-type expr))]
                            [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
                        (if super-expr
                            `(struct (,raw-type ,so-far)
                                     ,raw-fields)
                            `(struct ,raw-type ,raw-fields))))]
                 
                 ; if
                 
                 [(z:if-form? expr)
                  (let ([test-exp (if (eq? so-far nothing-so-far)
                                      (rectify-source-current-marks 
                                       (create-bogus-bound-varref if-temp))
                                      so-far)])
                    (cond [(comes-from-cond? expr)
                           (let* ([clause (list test-exp (rectify-source-current-marks (z:if-form-then expr)))]
                                  [cond-source (z:zodiac-start expr)]
                                  [rest-clauses (rectify-cond-clauses cond-source (z:if-form-else expr) mark-list null)])
                             `(cond ,clause ,@rest-clauses))]
                          [(comes-from-and? expr)
                           `(and ,test-exp ,@(rectify-and-clauses (z:zodiac-start expr)
                                                                  (z:if-form-then expr)
                                                                  mark-list
                                                                  null))]
                          [(comes-from-or? expr)
                           `(or ,test-exp ,@(rectify-or-clauses (z:zodiac-start expr)
                                                                (z:if-form-else expr)
                                                                mark-list
                                                                null))]
                          [else
                           `(if ,test-exp 
                                ,(rectify-source-current-marks (z:if-form-then expr))
                                ,(rectify-source-current-marks (z:if-form-else expr)))]))]
                 
                 ; quote : there is no mark or break on a quote.
                 
                 ; define-values : define's don't get marks, so they can't occur here
                 
                 ; lambda : there is no mark or break on a quote
                 
                 [else
                  (print-struct #t)
                  (e:dynamic-error
                   expr
                   (format "stepper:reconstruct: unknown object to reconstruct, ~a~n" expr))]))))
         
         
         (define redex #f)
         
         (define (current-def-rectifier so-far mark-list first)
           (if (null? mark-list)
               (rectify-top-level expr so-far)
               (let ([reconstructed (reconstruct-inner mark-list so-far)])
                 (current-def-rectifier
                  (if first
                      (begin
                        (set! redex reconstructed)
                        highlight-placeholder)
                      reconstructed)
                  (cdr mark-list)
                  #f))))
         
         
         ;         (define (confusable-value? val)
         ;           (not (or (number? val)
         ;                    (boolean? val)
         ;                    (string? val)
         ;                    (symbol? val))))
         
         (define answer
           (if (eq? break-kind 'result-break)
               (let* ([innermost (if (null? returned-value-list)
                                     (rectify-source-expr (mark-source (car mark-list)) mark-list null)
                                     (rectify-value (car returned-value-list)))]
                      [current-def (current-def-rectifier highlight-placeholder (cdr mark-list) #f)])
                 (list current-def innermost))
               (begin
                 (let ([current-def (current-def-rectifier nothing-so-far mark-list #t)])
                   (list current-def redex)))))

         )
      
      answer)))
