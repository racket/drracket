(unit/sig stepper:annotate^
  (import [z : zodiac:system^]
	  mzlib:function^
	  [e : stepper:error^]
          [utils : cogen-utils^]
          [s : stepper:model^]
	  stepper:shared^
          stepper:zodiac-client-procs^)
  
  ; ANNOTATE SOURCE CODE
  
  ; gensyms for annotation:
  
  ; the mutator-gensym is used in building the mutators that go into certain marks.
  ; (define mutator-gensym (gensym "mutator-"))
  
  ; the `closure-temp' symbol is used for the let which wraps created closures, so
  ; that we can stuff them into the hash table.
  
  ; closure-temp: uninterned-symbol
  
  (define closure-temp (gensym "closure-temp-"))
  
  ; var-set-union takes some lists of varrefs where no element appears twice in one list, and 
  ; forms a new list which is the union of the sets.  when a top-level and a non-top-level
  ; varref have the same name, we must keep the non-top-level one.
  
  (define (varref-remove* a-set b-set)
    (remove* a-set 
             b-set 
             (lambda (a-var b-var) 
               (eq? (z:varref-var a-var)
                    (z:varref-var b-var)))))
    
  (define (varref-set-pair-union a-set b-set)
    (cond [(or (eq? a-set 'all) (eq? b-set 'all)) 'all]
          [else (append a-set (varref-remove* a-set b-set))]))
  
  (define var-set-union
    (lambda args
      (foldl varref-set-pair-union
	     null
	     args)))
  
  (define (var-set-intersect a-set b-set)
    (varref-remove* (varref-remove* a-set b-set) b-set))
      
  (define-values (never-undefined? mark-never-undefined)
    (values
     (lambda (parsed) (never-undefined-getter (z:parsed-back parsed)))
     (lambda (parsed) (never-undefined-setter (z:parsed-back parsed) #t))))
   
  (define (interlace a b)
    (foldr (lambda (a b built)
             (cons a (cons b built)))
           null
           a
           b))
        
  (define (closure-key-maker closure)
    closure)
  
  ; debug-key: this key will be used as a key for the continuation marks.
  
  (define debug-key (gensym "debug-key-"))

  ; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
  ; a source expression (in the parsed zodiac format) and a set of z:varref/value pairs.
  ;((z:parsed (union (list-of z:varref) 'all) (list-of z:varref) (list-of z:varref) symbol) ->
  ; debug-info)
  
  (define (make-debug-info source tail-bound free-vars label)
    (let* ([top-level-varrefs (filter z:top-level-varref? free-vars)]
           [bound-varrefs (filter z:bound-varref? free-vars)]
           [top-level-kept (if (eq? tail-bound 'all)
                               top-level-varrefs 
                               null)]
           [lexical-kept (if (eq? tail-bound 'all)
                             bound-varrefs
                             (var-set-intersect bound-varrefs 
                                                tail-bound))]
           [kept-vars (append top-level-kept lexical-kept)]
           ; the reason I don't need var-set-union here is that these sets are guaranteed
           ; not to overlap.
            [var-clauses (map (lambda (x) 
                               (let ([var (z:varref-var x)])
                                 `(cons (#%lambda () ,var)
                                        (cons ,x
                                              null))))
                             kept-vars)])
      `(#%lambda () (list ,source (#%quote ,label) ,@var-clauses))))
  
  
  ; wrap-struct-form 
  
  (define (wrap-struct-form names annotated)
    (let* ([arg-temps (build-list (length names) get-arg-symbol)]
           [arg-temp-syms (map z:varref-var arg-temps)]
           [struct-proc-names (cdr names)]
           [closure-records (map (lambda (proc-name) `(,make-closure-record
                                                       (#%quote ,proc-name) 
                                                       (#%lambda () #f)
                                                       ,(eq? proc-name (car struct-proc-names))))
                                 struct-proc-names)]
           [proc-arg-temp-syms (cdr arg-temp-syms)]
           [setters (map (lambda (arg-temp-sym closure-record)
                           `(,closure-table-put! ,arg-temp-sym ,closure-record))
                         proc-arg-temp-syms
                         closure-records)]
           [full-body (append setters (list `(values ,@arg-temp-syms)))])
      `(#%let-values ((,arg-temp-syms ,annotated)) ,@full-body)))

  ; update-closure-record-name : adds a name to an existing closure table record,
  ; if there is one for that value.
  
  (define (update-closure-record-name value name)
    (let* ([closure-record (closure-table-lookup value)]
           [old-name (closure-record-name closure-record)])
      (if old-name
          (e:internal-error "closure-record already has a name: ~a" old-name)
          (set-closure-record-name! closure-record name))))
  
  
  (define initial-env-package null)
  
  ; annotate takes a list of expressions to annotate, a list of previously-defined variables
  ; (in the form returned by annotate), and a break routine to be called at breakpoints in the
  ; annotated code and returns an annotated expression, along with a new pair
  ; of environments (to be passed back in etc.)
  
  (define (annotate red-exprs parsed-exprs input-struct-proc-names break)
    (local
	(  
         (define (make-break kind)
           `(#%lambda returned-value-list
             (,break (continuation-mark-set->list
                      (current-continuation-marks) 
                      (#%quote ,debug-key))
                     (#%quote ,kind)
                     returned-value-list)))
  
         ; wrap creates the w-c-m expression.
         
         (define (simple-wcm-wrap debug-info expr)
           `(#%with-continuation-mark (#%quote ,debug-key) ,debug-info ,expr))
         
         (define (wcm-pre-break-wrap debug-info expr)
           (simple-wcm-wrap debug-info `(#%begin (,(make-break 'result-break)) ,expr)))
         
         (define (break-wrap expr)
           `(#%begin (,(make-break 'normal)) ,expr))
         
         (define (simple-wcm-break-wrap debug-info expr)
           (simple-wcm-wrap debug-info (break-wrap expr)))
         
         (define (return-value-wrap expr)
            `(#%let* ([result ,expr])
              (,(make-break 'result-break) result)
              result))

;  For Multiple Values:         
;           `(#%call-with-values
;             (#%lambda ()
;              expr)
;             (#%lambda result-values
;              (,(make-break 'result-break) result-values)
;              (#%apply #%values result-values))))

         (define (find-read-expr expr)
           (let ([offset (z:location-offset (z:zodiac-start expr))])
             (let search-exprs ([exprs red-exprs])
               (let* ([later-exprs (filter 
                                    (lambda (expr) 
                                      (<= offset (z:location-offset (z:zodiac-finish expr))))
                                    exprs)]
                      [expr 
                       (car later-exprs)])
                 (if (= offset (z:location-offset (z:zodiac-start expr)))
                     expr
                     (cond
                       ((z:scalar? expr) (e:static-error "starting offset inside scalar:" offset))
                       ((z:sequence? expr) 
                        (let ([object (z:read-object expr)])
                            (cond
                            ((z:list? expr) (search-exprs object))
                            ((z:vector? expr) 
                             (search-exprs (vector->list object))) ; can source exprs be here?
                            ((z:improper-list? expr)
                             (search-exprs (search-exprs object))) ; can source exprs be here?
                            (else (e:static-error "unknown expression type in sequence" expr)))))
                       (else (e:static-error "unknown read type" expr))))))))
  
         (define (struct-procs-defined expr)
           (if (and (z:define-values-form? expr)
                    (z:struct-form? (z:define-values-form-val expr)))
               (map z:varref-var (z:define-values-form-vars expr))
               null))
         
         (define struct-proc-names (apply append input-struct-proc-names
                                          (map struct-procs-defined parsed-exprs)))
         
         (define (non-annotated-proc? varref)
           (let ([name (z:varref-var varref)])
             (or (s:check-pre-defined-var name)
                 (memq name struct-proc-names))))
         
         ; annotate/inner takes 
         ; a) a zodiac expression to annotate
         ; b) a list of all varrefs s.t. this expression is tail w.r.t. their bindings
         ;    or 'all to indicate that this expression is tail w.r.t. _all_ bindings.
         ; c) a list of all top-level variables which occur in the program
         ; d) a boolean indicating whether this expression will be the r.h.s. of a reduction
         ;    (and therefore should be broken before)
         ;
         ; it returns
         ; a) an annotated s-expression
         ; b) a list of varrefs for the variables which occur free in the expression
         ;
	 ;(z:parsed (union (list-of z:varref) 'all) (list-of z:varref) bool -> 
         ;          sexp (list-of z:varref))
	 
	 (define (annotate/inner expr tail-bound pre-break?)
	   
           ; translate-varref: (bool bool -> sexp (listof varref))
	   
	   (let* ([tail-recur (lambda (expr) (annotate/inner expr tail-bound #t))]
                  [define-values-recur (lambda (expr) (annotate/inner expr tail-bound #f))]
                  [non-tail-recur (lambda (expr) (annotate/inner expr null #f))]
                  [lambda-body-recur (lambda (expr) (annotate/inner expr 'all #t))]
                  [make-debug-info-normal (lambda (free-vars)
                                            (make-debug-info expr tail-bound free-vars 'none))]
                  [make-debug-info-app (lambda (tail-bound free-vars label)
                                         (make-debug-info expr tail-bound free-vars label))]
                  [wcm-wrap (if pre-break?
                                wcm-pre-break-wrap
                                simple-wcm-wrap)]
                  [wcm-break-wrap (lambda (debug-info expr)
                                    (wcm-wrap debug-info (break-wrap expr)))]


                  
                  [translate-varref
                   (lambda (maybe-undef? top-level?)
                     (let* ([v (z:varref-var expr)]
                            [real-v (if (z:top-level-varref? expr)
                                        v
                                        (z:binding-orig-name
                                         (z:bound-varref-binding expr)))]
                            [free-vars (list expr)]
                            [debug-info (make-debug-info-normal free-vars)]
                            [annotated (if (and maybe-undef? (utils:signal-undefined))
                                           `(#%if (#%eq? ,v ,utils:the-undefined-value)
                                             (#%raise (,utils:make-undefined
                                                       ,(format utils:undefined-error-format real-v)
                                                       (#%current-continuation-marks)
                                                       (#%quote ,v)))
                                             ,v)
                                           v)])
                       (values (wcm-break-wrap debug-info (return-value-wrap annotated)) free-vars)))])
	     
             ; find the source expression and associate it with the parsed expression
             
             (set-expr-read! expr (find-read-expr expr))
	     
	     (cond
	       
	       ; the variable forms 
	       
	       [(z:bound-varref? expr)
		(translate-varref 
		 (not (never-undefined? (z:bound-varref-binding expr)))
		 #f)]
	       
               [(z:top-level-varref? expr)
		(if (utils:is-unit-bound? expr)
		    (translate-varref #t #f)
		    (begin
		      (utils:check-for-keyword/proc expr)
		      (translate-varref #f #t)))]
	       
	       [(z:app? expr)
		(let+
		 ([val sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
		  [val arg-temps (build-list (length sub-exprs) get-arg-symbol)]
                  [val arg-temp-syms (map z:varref-var arg-temps)] 
		  [val let-clauses (map (lambda (sym) `(,sym (#%quote ,*unevaluated*))) arg-temp-syms)]
		  [val pile-of-values
		       (map (lambda (expr) 
			      (let-values ([(annotated free) (non-tail-recur expr)])
				(list annotated free)))
			    sub-exprs)]
		  [val annotated-sub-exprs (map car pile-of-values)]
		  [val free-vars (apply var-set-union (map cadr pile-of-values))]
		  [val set!-list (map (lambda (arg-symbol annotated-sub-expr)
					`(#%set! ,arg-symbol ,annotated-sub-expr))
				      arg-temp-syms annotated-sub-exprs)]
                  [val new-tail-bound (var-set-union tail-bound arg-temps)]
		  [val app-debug-info (make-debug-info-app new-tail-bound arg-temps 'called)]
                  [val annotate-app? (let ([fun-exp (z:app-fun expr)])
                                        (and (z:top-level-varref? fun-exp)
                                             (non-annotated-proc? fun-exp)))]
		  [val final-app (break-wrap (simple-wcm-wrap app-debug-info 
                                                              (if annotate-app?
                                                                  (return-value-wrap arg-temp-syms)
                                                                  arg-temp-syms)))]
		  [val debug-info (make-debug-info-app new-tail-bound
                                                       (var-set-union free-vars arg-temps)
                                                       'not-yet-called)]
		  [val let-body (wcm-wrap debug-info `(#%begin ,@set!-list ,final-app))]
                  [val let-exp `(#%let ,let-clauses ,let-body)])
		 (values let-exp free-vars))]
	       
	       [(z:struct-form? expr)
		(let ([super-expr (z:struct-form-super expr)]
		      [raw-type (utils:read->raw (z:struct-form-type expr))]
		      [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
		  (if super-expr
		      (let+ ([val (values annotated-super-expr free-vars-super-expr) 
				  (non-tail-recur super-expr)]
			     [val annotated
				  `(#%struct 
				    ,(list raw-type annotated-super-expr)
				    ,raw-fields)]
                             [val debug-info (make-debug-info-normal free-vars-super-expr)])
			    (values (wcm-wrap debug-info annotated) free-vars-super-expr))
		      (values (wcm-wrap (make-debug-info-normal null)
                                        `(#%struct ,raw-type ,raw-fields)) 
                              null)))]

	       [(z:if-form? expr) 
		(let+
		 ([val (values annotated-test free-vars-test) 
		       (non-tail-recur (z:if-form-test expr))]
		  [val (values annotated-then free-vars-then) 
		       (tail-recur (z:if-form-then expr))]
		  [val (values annotated-else free-vars-else) 
		       (tail-recur (z:if-form-else expr))]
		  [val annotated `(#%begin
                                   (#%set! ,if-temp ,annotated-test)
                                   ,(break-wrap
                                     `(#%if (#%boolean? ,if-temp)
                                       (#%if ,if-temp
                                        ,annotated-then
                                        ,annotated-else)
                                       (#%raise (,utils:make-not-boolean
                                                 (#%format ,utils:not-boolean-error-format
                                                  ,if-temp)
                                                 (#%current-continuation-marks)
                                                 ,if-temp)))))]
                  [val if-temp-varref-list (list (create-bogus-bound-varref if-temp))]
		  [val free-vars (var-set-union if-temp-varref-list
                                                free-vars-test 
                                                free-vars-then 
                                                free-vars-else)]
		  [val debug-info (make-debug-info-app (var-set-union tail-bound if-temp-varref-list)
                                                       free-vars
                                                       'none)]
                  [val wcm-wrapped (wcm-wrap debug-info annotated)]
                  [val outer-annotated `(#%let ((,if-temp (#%quote ,*unevaluated*))) ,wcm-wrapped)])
		 (values outer-annotated free-vars))]
	       
	       [(z:quote-form? expr)
                (values (wcm-wrap (make-debug-info-normal null)
                                  `(#%quote ,(utils:read->raw (z:quote-form-expr expr)))) 
                        null)]
	       
	       ; there is no begin, begin0, or let in beginner. but can they be generated? 
	       ; for instance by macros? Maybe.

	       ; adding begin for xml stuff.  Just as a dummy, mind you.
               
               [(z:begin-form? expr) ; let's see if this works.
                (let ([exps (z:begin-form-bodies expr)])
                  (values
                   `(#%begin 
                      ,@(case (length exps)
                          ((2) (let-values ([(annotated-struct-decl _) (tail-recur (cadr exps))])
                                 (list annotated-struct-decl)))
                          ((3) (let-values ([(annotated-xml-decl _1) (tail-recur (cadr exps))]
                                            [(annotated-struct-decl _2) (tail-recur (caddr exps))])
                                 (list annotated-xml-decl annotated-struct-decl)))
                          [else (e:internal-error 'annotate/inner
                                                  "I don't recognize this use of (xml) begin")]))
                   null))]
                 
	       [(z:define-values-form? expr)
		(let+ ([val vars (z:define-values-form-vars expr)]
		       [val _ (map utils:check-for-keyword vars)]
		       [val var-names (map z:varref-var vars)]
                       
                       ; NB: this next recurrence is NOT really tail, but we cannot
                       ; mark define-values itself, so we mark the sub-expr as
                       ; if it was in tail posn (i.e., we must hold on to 
                       ; bindings).
		       
                       [val val (z:define-values-form-val expr)]
                       [val (values annotated-val free-vars-val)
			    (define-values-recur val)]
		       [val free-vars (varref-remove* vars free-vars-val)])
                      (cond [(z:case-lambda-form? val)
                             (values `(#%define-values ,var-names
                                       (#%let ((,closure-temp ,annotated-val))
                                        (,update-closure-record-name ,closure-temp (#%quote ,(car var-names)))
                                        ,closure-temp))
                                     free-vars)]
                            [(z:struct-form? val)
                             (values `(#%define-values ,var-names
                                       ,(wrap-struct-form var-names annotated-val)) 
                                     free-vars)]
                            [else
                             (values `(#%define-values ,var-names
                                       ,annotated-val) 
                                     free-vars)]))]
	       
	       ; there is no set! in beginner level
	       
	       [(z:case-lambda-form? expr)
		(let* ([annotate-case
			(lambda (arglist body)
			  (let ([var-list (map create-bogus-bound-varref 
                                               (map z:binding-var
                                                    (z:arglist-vars arglist)))])
			    (let-values ([(annotated free-vars)
					  (lambda-body-recur body)])
			      (let* ([new-free-vars (varref-remove* var-list free-vars)]
                                     [args (utils:arglist->ilist arglist)]
                                     [new-annotated (list (utils:improper-map z:binding-var args) annotated)])
                                (utils:improper-foreach utils:check-for-keyword args)
                                (utils:improper-foreach mark-never-undefined args)
				(list new-annotated new-free-vars)))))]
		       [pile-of-results (map annotate-case 
					     (z:case-lambda-form-args expr)
					     (z:case-lambda-form-bodies expr))]
		       [annotated-bodies (map car pile-of-results)]
		       [annotated-case-lambda (cons '#%case-lambda annotated-bodies)] 
		       [new-free-vars (apply var-set-union (map cadr pile-of-results))]
		       [closure-info (make-debug-info-app 'all new-free-vars 'none)]
                       [wrapped-annotated (wcm-wrap (make-debug-info-normal null)
                                                    annotated-case-lambda)]
		       [hash-wrapped `(#%let ([,closure-temp ,wrapped-annotated])
				       (,closure-table-put! (,closure-key-maker ,closure-temp) 
                                        (,make-closure-record 
                                         #f
                                         ,closure-info 
                                         #f))
				       ,closure-temp)])
		  (values hash-wrapped
			  new-free-vars))]
	       
	       ; there's no with-continuation-mark in beginner level.
	       
	       ; there are _definitely_ no units or classes
	       
	       [else
		(print-struct #t)
		(e:internal-error
		 expr
		 (format "stepper:annotate/inner: unknown object to annotate, ~a~n" expr))])))
         
         (define (annotate/top-level expr)
           (let-values ([(annotated dont-care)
                         (annotate/inner expr 'all #f)])
             (cond [(z:define-values-form? expr)
                    annotated]
                   [(z:begin-form? expr) ; cheap hack for xml
                    annotated]
                   [else
                    `(#%define-values ,(list (top-level-exp-gensym-source expr))
                      ,annotated)]))))
         
         ; body of local
         
      (let* ([annotated-exprs (map (lambda (expr)
                                     (annotate/top-level expr))
                                   parsed-exprs)])
           
           (values annotated-exprs
                   struct-proc-names)))))
	 
    
  
    
