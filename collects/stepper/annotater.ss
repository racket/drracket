(unit/sig stepper:annotate^
  (import [z : zodiac:system^]
	  mzlib:function^
	  [e : zodiac:interface^]
          [utils : stepper:cogen-utils^]
          stepper:marks^
          [s : stepper:model^]
	  stepper:shared^
          stepper:client-procs^)
  
  ; ANNOTATE SOURCE CODE
  
  ; gensyms for annotation:
  
  ; the mutator-gensym is used in building the mutators that go into certain marks.
  ; (define mutator-gensym (gensym "mutator-"))
  
  ; the `closure-temp' symbol is used for the let which wraps created closures, so
  ; that we can stuff them into the hash table.
  
  ; closure-temp: uninterned-symbol
  
  (define closure-temp (gensym "closure-temp-"))
  
  ; dual-map : (('a -> (values 'b 'c)) ('a list)) -> (values ('b list) ('c list))
  
  (define (dual-map f . lsts)
    (if (null? (car lsts))
        (values null null)
        (let+ ([val (values a b) (apply f (map car lsts))]
               [val (values a-rest b-rest) (apply dual-map f (map cdr lsts))])
          (values (cons a a-rest) (cons b b-rest)))))
    
  ; var-set-union takes some lists of varrefs where no element appears twice in one list, and 
  ; forms a new list which is the union of the sets.
  
  ; varref-remove* removes the varrefs in a-set from the varrefs in b-set
  
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
      
  (define never-undefined? never-undefined-getter)
  (define (mark-never-undefined parsed) (never-undefined-setter parsed #t))
   
  (define (interlace a b)
    (foldr (lambda (a b built)
             (cons a (cons b built)))
           null
           a
           b))
        
  (define (closure-key-maker closure)
    closure)
  
  ; paroptarglist-> ilist and arglist->ilist are used to recreate
  ; mzscheme sexp syntax from the parsed zodiac form, so that the
  ; resulting expression can be fed to mzscheme.
  
  

  ; debug-key: this key will be used as a key for the continuation marks.
  
  (define debug-key (gensym "debug-key-"))

  ; translate-varref : returns the name the varref will get in the final output
  
  (define (translate-varref expr)
    (if (or (z:top-level-varref? expr) (not (z:parsed-back expr))) ; top level or extra-bogus varrefs
        (z:varref-var expr)
        (utils:get-binding-name (z:bound-varref-binding expr))))
  
  ; bindings->varrefs : turn a list of bindings into a list of bogus varrefs
  
  (define (bindings->varrefs bindings)
    (map create-bogus-bound-varref
         (map z:binding-var bindings)
         bindings))
  
  ; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
  ; a source expression (in the parsed zodiac format) and a set of z:varref/value pairs.
  ;((z:parsed (union (list-of z:varref) 'all) (list-of z:varref) (list-of z:varref) symbol) ->
  ; debug-info)
    
  (define (make-debug-info source tail-bound free-vars label)
    (let* ([kept-vars (if (eq? tail-bound 'all)
                          free-vars
                          (var-set-intersect tail-bound    ; the order of these arguments is important if
                                             ; the tail-bound varrefs don't have bindings
                                             free-vars))]
           [real-kept-vars (filter z:bound-varref? kept-vars)]
           [var-clauses (map (lambda (x) 
                               (let ([var (translate-varref x)])
                                 (list var x)))
                             real-kept-vars)])
      (make-full-mark source label var-clauses)))
  
  ; cheap-wrap for non-debugging annotation
  
  (define cheap-wrap
    (lambda (zodiac body)
      (let ([start (z:zodiac-start zodiac)]
	    [finish (z:zodiac-finish zodiac)])
	`(#%with-continuation-mark (#%quote ,debug-key)
	  ,(make-cheap-mark (z:make-zodiac #f start finish))
	  ,body))))
  
  ; wrap-struct-form 
  
  (define (wrap-struct-form names annotated)
    (let* ([arg-temps (build-list (length names) get-arg-varref)]
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
  
  ; annotate takes 
  ; a) a list of zodiac:read expressions,
  ; b) a list of zodiac:parsed expressions,
  ; c) a list of previously-defined variables, 
  ; d) a break routine to be called at breakpoints, and
  ; e) a boolean which indicates whether the expression is to be annotated "cheaply".
  ;
  ; actually, I'm not sure that annotate works for more than one expression, even though
  ; it's supposed to take a whole list.  I wouldn't count on it. Also, both the red-exprs
  ; and break arguments may be #f, the first during a zodiac:elaboration-evaluator call,
  ; the second during any non-stepper use.
  
  (define (annotate red-exprs parsed-exprs input-struct-proc-names break cheap-wrap?)
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
           (if break
               (simple-wcm-wrap debug-info `(#%begin (,(make-break 'result-break)) ,expr))
               (simple-wcm-wrap debug-info expr)))
         
         (define (break-wrap expr)
           (if break
               `(#%begin (,(make-break 'normal-break)) ,expr)
               expr))
         
         (define (double-break-wrap expr)
           (if break
               `(#%begin (,(make-break 'double-break)) ,expr)))
         
         (define (simple-wcm-break-wrap debug-info expr)
           (simple-wcm-wrap debug-info (break-wrap expr)))
         
         (define (return-value-wrap expr)
           (if break
               `(#%let* ([result ,expr])
                 (,(make-break 'result-break) result)
                 result)
               expr))

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
         ; c) a list of bound-varrefs of 'floating' variables; i.e. lexical bindings  NO: TAKEN OUT
         ;    whose value must be captured in order to reconstruct outer expressions. 
         ;    Necessitated by 'unit', useful for 'letrec*-values'.
         ; d) a boolean indicating whether this expression will be the r.h.s. of a reduction
         ;    (and therefore should be broken before)
         ; e) a boolean indicating whether this expression is top-level (and therefore should
         ;    not be wrapped, if a begin).
         ; f) a boolean indicating whether this expression should receive the "cheap wrap" (aka
         ;    old-style aries annotation) or not.  #t => cheap wrap. NOTE: THIS HAS BEEN 
         ;    (TEMPORARILY?) TAKEN OUT/MOVED TO THE TOP LEVEL.
         ;
         ; it returns
         ; a) an annotated s-expression
         ; b) a list of varrefs for the variables which occur free in the expression
         ;
	 ;(z:parsed (union (list-of z:varref) 'all) (list-of z:varref) bool bool -> 
         ;          sexp (list-of z:varref))
	 
	 (define (annotate/inner expr tail-bound pre-break? top-level?)
	   
	   (let* ([tail-recur (lambda (expr) (annotate/inner expr tail-bound #t #f))]
                  [define-values-recur (lambda (expr) (annotate/inner expr tail-bound #f #f))]
                  [non-tail-recur (lambda (expr) (annotate/inner expr null #f #f))]
                  [lambda-body-recur (lambda (expr) (annotate/inner expr 'all #t #f))]
                  ; note: no pre-break for the body of a let; it's handled by the break for the
                  ; let itself.
                  [let-body-recur (lambda (expr vars) (annotate/inner expr (var-set-union tail-bound vars) #f #f))]
                  [cheap-wrap-recur (lambda (expr) (let-values ([(ann _) (non-tail-recur expr)]) ann))]
                  [make-debug-info-normal (lambda (free-vars)
                                            (make-debug-info expr tail-bound free-vars 'none))]
                  [make-debug-info-app (lambda (tail-bound free-vars label)
                                         (make-debug-info expr tail-bound free-vars label))]
                  [wcm-wrap (if pre-break?
                                wcm-pre-break-wrap
                                simple-wcm-wrap)]
                  [wcm-break-wrap (lambda (debug-info expr)
                                    (wcm-wrap debug-info (break-wrap expr)))]
                  [expr-cheap-wrap (lambda (annotated) (cheap-wrap expr annotated))])
	     
             ; find the source expression and associate it with the parsed expression
             
             (when (and red-exprs (not cheap-wrap?))
               (set-expr-read! expr (find-read-expr expr)))
	     
	     (cond
	       
	       ; the variable forms 
	       
               [(z:varref? expr)
                (let* ([v (translate-varref expr)]
                       [real-v (if (z:top-level-varref? expr)
                                   v
                                   (z:binding-orig-name
                                    (z:bound-varref-binding expr)))]
                       [maybe-undef? (or (and (z:bound-varref? expr) 
                                              (not (never-undefined? (z:bound-varref-binding expr))))
                                         (utils:is-unit-bound? expr))]
                       [truly-top-level? (and (z:top-level-varref? expr) (not (utils:is-unit-bound? expr)))]
                       [_ (when truly-top-level?
                            (utils:check-for-syntax-or-macro-keyword expr))]
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
                  (values (if cheap-wrap?
                              (if (or (and maybe-undef? (utils:signal-undefined)) truly-top-level?)
                                  (expr-cheap-wrap annotated)
                                  annotated)
                              (wcm-break-wrap debug-info (return-value-wrap annotated))) free-vars))]

               [(z:app? expr)
		(let+ ([val sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
                       [val (values annotated-sub-exprs free-vars-sub-exprs)
                            (dual-map non-tail-recur sub-exprs)]
                       [val free-vars (apply var-set-union free-vars-sub-exprs)])
                   (if cheap-wrap?
                       (values (expr-cheap-wrap annotated-sub-exprs) free-vars)
                       (let+ ([val arg-temps (build-list (length sub-exprs) get-arg-varref)]
                              [val arg-temp-syms (map z:varref-var arg-temps)] 
                              [val let-clauses `((,arg-temp-syms 
                                                  (#%values ,@(map (lambda (x) `(#%quote ,*unevaluated*)) arg-temps))))]
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
                              [val let-exp `(#%let-values ,let-clauses ,let-body)])
                         (values let-exp free-vars))))]
	       
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
			    (values (if cheap-wrap?
                                        (expr-cheap-wrap annotated)
                                        (wcm-wrap debug-info annotated)) 
                                    free-vars-super-expr))
                      (let ([annotated `(#%struct ,raw-type ,raw-fields)])
                        (values (if cheap-wrap?
                                    (expr-cheap-wrap annotated)
                                    (wcm-wrap (make-debug-info-normal null) annotated)) 
                              null))))]

	       [(z:if-form? expr) 
		(let+ ([val (values annotated-test free-vars-test) 
                            (non-tail-recur (z:if-form-test expr))]
                       [val (values annotated-then free-vars-then) 
                            (tail-recur (z:if-form-then expr))]
                       [val (values annotated-else free-vars-else) 
                            (tail-recur (z:if-form-else expr))]
                       [val free-vars (var-set-union free-vars-test 
                                                     free-vars-then 
                                                     free-vars-else)]
                       [val inner-annotated `(#%if ,if-temp
                                              ,annotated-then
                                              ,annotated-else)]
                       [val annotated-2 (if (utils:signal-not-boolean)
                                            `(#%if (#%boolean? ,if-temp)
                                              ,inner-annotated
                                              (#%raise (,utils:make-not-boolean
                                                        (#%format ,utils:not-boolean-error-format
                                                         ,if-temp)
                                                        (#%current-continuation-marks)
                                                        ,if-temp)))
                                            inner-annotated)])
                  (if cheap-wrap?
                      (values 
                       (expr-cheap-wrap (if (utils:signal-not-boolean)
                                            `(#%let ((,if-temp ,annotated-test)) ,annotated-2)
                                            `(#%if ,annotated-test ,annotated-then ,annotated-else)))
                       free-vars)
                      (let+ ([val annotated `(#%begin
                                              (#%set! ,if-temp ,annotated-test)
                                              ,(break-wrap
                                                (if (utils:signal-not-boolean)
                                                    `(#%if (#%boolean? ,if-temp)
                                                      ,inner-annotated
                                                      (#%raise (,utils:make-not-boolean
                                                                (#%format ,utils:not-boolean-error-format
                                                                 ,if-temp)
                                                                (#%current-continuation-marks)
                                                                ,if-temp)))
                                                    inner-annotated)))]
                             [val if-temp-varref-list (list (create-bogus-bound-varref if-temp #f))]
                             
                             [val debug-info (make-debug-info-app (var-set-union tail-bound if-temp-varref-list)
                                                                  (var-set-union free-vars if-temp-varref-list)
                                                                  'none)]
                             [val wcm-wrapped (wcm-wrap debug-info annotated)]
                             [val outer-annotated `(#%let ((,if-temp (#%quote ,*unevaluated*))) ,wcm-wrapped)])
                        (values outer-annotated free-vars))))]
	       
	       [(z:quote-form? expr)
                (let ([annotated `(#%quote ,(utils:read->raw (z:quote-form-expr expr)))])
                  (values (if cheap-wrap?
                              annotated
                              (wcm-wrap (make-debug-info-normal null) annotated))
                          null))]
               
               [(z:begin-form? expr)
                (if top-level? 
                    (let+ ([val bodies (z:begin-form-bodies expr)]
                           [val (values annotated-bodies free-vars)
                                (dual-map (lambda (expr)
                                            (annotate/inner expr 'all #f #t)) 
                                          bodies)])
                       (values `(#%begin ,@annotated-bodies)
                               (apply var-set-union free-vars)))
                    (let+ ([val bodies (z:begin-form-bodies expr)]
                           [val (values all-but-last-body last-body-list) 
                                (list-partition bodies (- (length bodies) 1))]
                           [val last-body (car last-body-list)]
                           [val (values annotated-a free-vars-a)
                                (dual-map non-tail-recur all-but-last-body)]
                           [val (values annotated-final free-vars-final)
                                (tail-recur last-body)]
                           [val free-vars (apply var-set-union free-vars-final free-vars-a)]
                           [val debug-info (make-debug-info-normal free-vars)]
                           [val annotated `(#%begin ,@(append annotated-a (list annotated-final)))])
                       (values (if cheap-wrap?
                                   (expr-cheap-wrap annotated)
                                   (wcm-wrap debug-info annotated))
                               free-vars)))]

               [(z:begin0-form? expr)
                (let+ ([val bodies (z:begin0-form-bodies expr)]
                       [val (values annotated-bodies free-vars-lists)
                            (dual-map non-tail-recur bodies)]
                       [val free-vars (apply var-set-union free-vars-lists)]
                       [val debug-info (make-debug-info-normal free-vars)]
                       [val annotated `(#%begin0 ,@annotated-bodies)])
                   (values (if cheap-wrap?
                               (expr-cheap-wrap annotated)
                               (wcm-wrap debug-info annotated))
                           free-vars))]
               
               ; gott in himmel! this transformation is complicated.  Just for the record,
               ; here's a sample transformation:
               ;(let-values ([(a b c) e1] [(d e) e2]) e3)
               ;
               ;turns into
               ;
               ;(let-values ([(dummy1 dummy2 dummy3 dummy4 dummy5)
               ;              (values *unevaluated* *unevaluated* *unevaluated* *unevaluated* *unevaluated*)])
               ;  (with-continuation-mark 
               ;   key huge-value
               ;   (begin
               ;     (set!-values (dummy1 dummy2 dummy3) e1)
               ;     (set!-values (dummy4 dummy5) e2)
               ;     (let-values ([(a b c d e) (values dummy1 dummy2 dummy3 dummy4 dummy5)])
               ;       e3))))
               ;
               ; let me know if you can do it in less.
               
               ; another irritating point: the mark and the break that must go immediately 
               ; around the body.  Irritating because they will be instantly replaced by
               ; the mark and the break produced by the annotated body itself. However, 
               ; they're necessary, because the body may not contain free references to 
               ; all of the variables defined in the let, and thus their values are not 
               ; known otherwise.  
               ; whoops! hold the phone.  I think I can get away with a break before, and
               ; a mark after, so only one of each.  groovy, eh?
                      
               [(z:let-values-form? expr)
                (let+ ([val var-sets (z:let-values-form-vars expr)]
                       [val var-set-list (apply append var-sets)]
                       [val vals (z:let-values-form-vals expr)]
                       [_ (for-each utils:check-for-keyword var-set-list)]
                       [_ (for-each mark-never-undefined var-set-list)]
                       [val (values annotated-vals free-vars-vals)
                            (dual-map non-tail-recur vals)]
                       [val (values annotated-body free-vars-body)
                            (let-body-recur (z:let-values-form-body expr) 
                                            (bindings->varrefs var-set-list))]
                       [val free-vars (apply var-set-union (varref-remove* (bindings->varrefs var-set-list) free-vars-body)
                                             free-vars-vals)])
                  (if cheap-wrap?
                      (let ([bindings
                             (map (lambda (vars val)
                                    `(,(map utils:get-binding-name vars) ,val))
                                  var-sets
                                  annotated-vals)])
                        (values (expr-cheap-wrap `(#%let-values ,bindings ,annotated-body)) free-vars))
                      (let+ ([val dummy-var-sets
                                  (let ([counter 0])
                                    (map (lambda (var-set)
                                           (map (lambda (var) 
                                                  (begin0 
                                                    (get-arg-varref counter)
                                                    (set! counter (+ counter 1))))
                                                var-set))
                                         var-sets))]
                             [val dummy-var-list (apply append dummy-var-sets)]
                             [val outer-dummy-initialization
                                  `([,(map z:varref-var dummy-var-list)
                                     (#%values ,@(build-list (length dummy-var-list) 
                                                             (lambda (_) `(#%quote ,*unevaluated*))))])]
                             [val set!-clauses
                                  (map (lambda (dummy-var-set val)
                                         `(#%set!-values ,(map z:varref-var dummy-var-set) ,val))
                                       dummy-var-sets
                                       annotated-vals)]
                             [val inner-transference
                                  `([,(map utils:get-binding-name var-set-list) 
                                     (values ,@(map z:varref-var dummy-var-list))])]
                             ; time to work from the inside out again
                             [val inner-let-values
                                  `(#%let-values ,inner-transference ,annotated-body)]
                             [val middle-begin
                                  `(#%begin ,@set!-clauses ,(double-break-wrap inner-let-values))]
                             [val wrapped-begin
                                  (wcm-wrap (make-debug-info-app (var-set-union tail-bound dummy-var-list)
                                                                 (var-set-union free-vars dummy-var-list)
                                                                 'let-body)
                                            middle-begin)]
                             [val whole-thing
                                  `(#%let-values ,outer-dummy-initialization ,wrapped-begin)])
                        (values whole-thing free-vars))))]
               
               [(z:letrec-values-form? expr)
                (let+ ([val var-sets (z:letrec-values-form-vars expr)]
                       [val var-set-list (apply append var-sets)]
                       [val var-set-list-varrefs (bindings->varrefs var-set-list)]
                       [val var-set-list-binding-names (map utils:get-binding-name var-set-list)]
                       [val vals (z:letrec-values-form-vals expr)]
                       [_ (when (andmap z:case-lambda-form? vals)
                            (for-each mark-never-undefined var-set-list))] ; we could be more aggressive about this.
                       [_ (for-each utils:check-for-keyword var-set-list)]
                       [val (values annotated-vals free-vars-vals)
                            (dual-map non-tail-recur vals)]
                       [val (values annotated-body free-vars-body)
                            (let-body-recur (z:letrec-values-form-body expr) 
                                            var-set-list-varrefs)]
                       [val free-vars-inner (apply var-set-union free-vars-body free-vars-vals)]
                       [val free-vars-outer (varref-remove* var-set-list-varrefs free-vars-inner)])
                  (if cheap-wrap?
                      (let ([bindings
                             (map (lambda (vars val)
                                    `(,(map utils:get-binding-name vars)
                                      ,val))
                                  var-sets
                                  annotated-vals)])
                        (values (expr-cheap-wrap `(#%letrec-values ,bindings ,annotated-body))
                                free-vars-outer))
                      (let+ ([val outer-initialization
                                  `((,var-set-list-binding-names 
                                     (values ,@var-set-list-binding-names)))]
                             [val set!-clauses
                                  (map (lambda (var-set val)
                                         `(#%set!-values ,(map utils:get-binding-name var-set) ,val))
                                       var-sets
                                       annotated-vals)]
                             [val middle-begin
                                  `(#%begin ,@set!-clauses ,(double-break-wrap annotated-body))]
                             [val wrapped-begin
                                  (wcm-wrap (make-debug-info-app (var-set-union tail-bound var-set-list-varrefs)
                                                                 (var-set-union free-vars-inner var-set-list-varrefs)
                                                                 'let-body)
                                            middle-begin)]
                             [val whole-thing
                                  `(#%letrec-values ,outer-initialization ,wrapped-begin)])
                        (values whole-thing free-vars-outer))))]
               
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
                      (cond [(and (z:case-lambda-form? val) (not cheap-wrap?))
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
	       
	       [(z:set!-form? expr)
                (utils:check-for-keyword (z:set!-form-var expr))
                (let+ ([val v (translate-varref (z:set!-form-var expr))]
                       [val (values annotated-body rhs-free-vars)
                            (non-tail-recur (z:set!-form-val expr))]
                       [val free-vars (var-set-union (list (z:set!-form-var expr)) rhs-free-vars)]
                       [val debug-info (make-debug-info-normal free-vars)]
                       [val annotated `(#%set! ,v ,annotated-body)])
                   (values (if cheap-wrap?
                               (expr-cheap-wrap annotated)
                               (wcm-wrap (make-debug-info-normal free-vars) annotated))
                           free-vars))]
                
               [(z:case-lambda-form? expr)
		(let+ ([val (values annotated-cases free-vars-cases)
                            (dual-map
                             (lambda (arglist body)
                               (let ([var-list (bindings->varrefs (z:arglist-vars arglist))]
                                     [args (utils:arglist->ilist arglist)])
                                 (utils:improper-foreach utils:check-for-keyword args)
                                 (utils:improper-foreach mark-never-undefined args)
                                 (let+ ([val (values annotated free-vars)
                                             (lambda-body-recur body)]
                                        [val new-free-vars (varref-remove* var-list free-vars)]
                                        [val new-annotated (list (utils:improper-map utils:get-binding-name args) 
                                                               annotated)]) 
                                   (values new-annotated new-free-vars))))
                             (z:case-lambda-form-args expr)
                             (z:case-lambda-form-bodies expr))]
		       [val annotated-case-lambda (cons '#%case-lambda annotated-cases)] 
		       [val new-free-vars (apply var-set-union free-vars-cases)]
		       [val closure-info (make-debug-info-app 'all new-free-vars 'none)]
                       [val wrapped-annotated (wcm-wrap (make-debug-info-normal null)
                                                        annotated-case-lambda)]
		       [val hash-wrapped `(#%let ([,closure-temp ,wrapped-annotated])
                                           (,closure-table-put! (,closure-key-maker ,closure-temp) 
                                            (,make-closure-record 
                                             #f
                                             ,closure-info 
                                             #f))
                                           ,closure-temp)])
		  (values (if cheap-wrap?
                              annotated-case-lambda
                              hash-wrapped)
			  new-free-vars))]
	       
               ; the annotation for w-c-m is insufficient for
               ; debugging: there must be an intermediate let & set!s to
               ; allow the user to see the computed values for the key and the
               ; value.
               
               [(z:with-continuation-mark-form? expr)
                (let+ ([val (values annotated-key free-vars-key)
                            (non-tail-recur (z:with-continuation-mark-form-key expr))]
                       [val (values annotated-val free-vars-val)
                            (non-tail-recur (z:with-continuation-mark-form-val expr))]
                       [val (values annotated-body free-vars-body)
                            (non-tail-recur (z:with-continuation-mark-form-body expr))]
                       [val free-vars (var-set-union free-vars-key free-vars-val free-vars-body)]
                       [val debug-info (make-debug-info-normal free-vars)]
                       [val annotated `(#%with-continuation-mark
                                        ,annotated-key
                                        ,annotated-val
                                        ,annotated-body)])
                   (values (if cheap-wrap?
                               (expr-cheap-wrap annotated)
                               (wcm-wrap debug-info annotated))
                           free-vars))]
               
               [(not cheap-wrap?)
                (e:static-error "cannot annotate units or classes except in cheap-wrap mode")]
               
               [(z:unit-form? expr)
                (let+ ([val imports (z:unit-form-imports expr)]
                       [val exports (map (lambda (export)
                                           (list (translate-varref (car export))
                                                 (z:read-object (cdr export))))
                                         (z:unit-form-exports expr))]
                       [val clauses (map annotate/top-level (z:unit-form-clauses expr))])
                  (for-each utils:check-for-keyword imports)
                  (values
                   `(#%unit
                     (import ,@(map utils:get-binding-name imports))
                     (export ,@exports)
                     ,@clauses)
                   null))]
	       
               [(z:compound-unit-form? expr)
                (let ((imports (map utils:get-binding-name
                                    (z:compound-unit-form-imports expr)))
                      (links (z:compound-unit-form-links expr))
                      (exports (z:compound-unit-form-exports expr)))
                  (let
                      ((links
                        (map
                         (lambda (link-clause)
                           (let+ ([val tag (utils:read->raw (car link-clause))]
                                  [val sub-unit (cheap-wrap-recur (cadr link-clause))]
                                  [val imports
                                       (map (lambda (import)
                                              (if (z:lexical-varref? import)
                                                  (translate-varref import)
                                                  `(,(utils:read->raw (car import))
                                                    ,(utils:read->raw (cdr import)))))
                                            (cddr link-clause))])
                             `(,tag (,sub-unit ,@imports))))
                         links))
                       (exports
                        (map
                         (lambda (export-clause)
                           `(,(utils:read->raw (car export-clause))
                             (,(utils:read->raw (cadr export-clause))
                              ,(utils:read->raw (cddr export-clause)))))
                         exports)))
                    (let ((e `(#%compound-unit
                               (import ,@imports)
                               (link ,@links)
                               (export ,@exports))))
                      (values (expr-cheap-wrap e) null))))]
               
               [(z:invoke-unit-form? expr)
                (values
                 (expr-cheap-wrap `(#%invoke-unit ,(cheap-wrap-recur (z:invoke-unit-form-unit expr))
                                    ,@(map translate-varref
                                           (z:invoke-unit-form-variables expr))))
                 null)]
               
               [(z:interface-form? expr)
                (let ((vars (z:interface-form-variables expr)))
                  (for-each utils:check-for-keyword vars)
                  (values
                   (expr-cheap-wrap
                    `(#%interface ,(map cheap-wrap-recur
                                        (z:interface-form-super-exprs expr))
                      ,@(map utils:read->raw vars)))
                   null))]
               
               [(z:class*/names-form? expr)
                (let* ([process-arg
                        (lambda (element)
                          (if (pair? element)
                              (and (utils:check-for-keyword (car element))
                                   (list (utils:get-binding-name (car element))
                                         (cheap-wrap-recur (cdr element))))
                              (and (utils:check-for-keyword element)
                                   (utils:get-binding-name element))))]
                       [paroptarglist->ilist
                        (lambda (paroptarglist)
                          (cond
                            ((z:sym-paroptarglist? paroptarglist)
                             (process-arg (car (z:paroptarglist-vars paroptarglist))))
                            ((z:list-paroptarglist? paroptarglist)
                             (map process-arg (z:paroptarglist-vars paroptarglist)))
                            ((z:ilist-paroptarglist? paroptarglist)
                             (let loop ((vars (map process-arg
                                                   (z:paroptarglist-vars paroptarglist))))
                               (if (null? (cddr vars))
                                   (cons (car vars) (cadr vars))
                                   (cons (car vars) (loop (cdr vars))))))
                            (else
                             (e:internal-error paroptarglist
                                               "Given to paroptarglist->ilist"))))])
                  (values
                   (expr-cheap-wrap
                    `(#%class*/names
                      (,(utils:get-binding-name (z:class*/names-form-this expr))
                       ,(utils:get-binding-name (z:class*/names-form-super-init expr)))
                      ,(cheap-wrap-recur (z:class*/names-form-super-expr expr))
                      ,(map cheap-wrap-recur (z:class*/names-form-interfaces expr))
                      ,(paroptarglist->ilist (z:class*/names-form-init-vars expr))
                      ,@(map
                         (lambda (clause)
                           (cond
                             ((z:public-clause? clause)
                              `(public
                                 ,@(map (lambda (internal export expr)
                                          `((,(utils:get-binding-name internal)
                                             ,(utils:read->raw export))
                                            ,(cheap-wrap-recur expr)))
                                        (z:public-clause-internals clause)
                                        (z:public-clause-exports clause)
                                        (z:public-clause-exprs clause))))
                             ((z:override-clause? clause)
                              `(override
                                 ,@(map (lambda (internal export expr)
                                          `((,(utils:get-binding-name internal)
                                             ,(utils:read->raw export))
                                            ,(cheap-wrap-recur expr)))
                                        (z:override-clause-internals clause)
                                        (z:override-clause-exports clause)
                                        (z:override-clause-exprs clause))))
                             ((z:private-clause? clause)
                              `(private
                                 ,@(map (lambda (internal expr)
                                          `(,(utils:get-binding-name internal)
                                            ,(cheap-wrap-recur expr)))
                                        (z:private-clause-internals clause)
                                        (z:private-clause-exprs clause))))
                             ((z:inherit-clause? clause)
                              `(inherit
                                 ,@(map (lambda (internal inherited)
                                          `(,(utils:get-binding-name internal)
                                            ,(utils:read->raw inherited)))
                                        (z:inherit-clause-internals clause)
                                        (z:inherit-clause-imports clause))))
                             ((z:rename-clause? clause)
                              `(rename
                                ,@(map (lambda (internal import)
                                         `(,(utils:get-binding-name internal)
                                           ,(utils:read->raw import)))
                                       (z:rename-clause-internals clause)
                                       (z:rename-clause-imports clause))))
                             ((z:sequence-clause? clause)
                              `(sequence
                                 ,@(map cheap-wrap-recur
                                        (z:sequence-clause-exprs clause))))))
                         (z:class*/names-form-inst-clauses expr))))
                   null))]          
	       
	       [else
		(print-struct #t)
		(e:internal-error
		 expr
                 "stepper:annotate/inner: unknown object to annotate, ~a~n"
                 expr)])))
         
         (define (annotate/top-level expr)
           (let-values ([(annotated dont-care)
                         (annotate/inner expr 'all #f #t)])
             annotated)))
         
         ; body of local
         
      (let* ([annotated-exprs (map (lambda (expr)
                                     (annotate/top-level expr))
                                   parsed-exprs)])
        (values annotated-exprs
                struct-proc-names)))))
	 
