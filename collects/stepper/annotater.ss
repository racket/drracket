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
  
  ; binding-set-union takes some lists of bindings where no element appears twice in one list, and 
  ; forms a new list which is the union of the sets.
  
  (define (binding-set-pair-union a-set b-set)
    (cond [(or (eq? a-set 'all) (eq? b-set 'all)) 'all]
          [else (append a-set (remq* a-set b-set))]))
  
  (define binding-set-union
    (lambda args
      (foldl binding-set-pair-union
	     null
	     args)))
  
  (define (binding-set-intersect a-set b-set)
    (remq* (remq* a-set b-set) b-set))
      
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
    (if (z:top-level-varref? expr) ; top level varrefs
        (z:varref-var expr)
        (get-binding-name (z:bound-varref-binding expr))))
  
  ; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
  ; a source expression (in the parsed zodiac format) and a set of z:binding/value pairs.
  ;((z:parsed (union (list-of z:binding) 'all) (list-of z:binding) symbol) ->
  ; debug-info)
    
  (define (make-debug-info source tail-bound free-bindings label)
    (let* ([kept-bindings (if (eq? tail-bound 'all)
                              free-bindings
                              (binding-set-intersect tail-bound
                                                     free-bindings))]
           [var-clauses (map (lambda (x) 
                               (let ([var (get-binding-name x)])
                                 (list var x)))
                             kept-bindings)])
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
    (let* ([arg-temps (build-list (length names) get-arg-binding)]
           [arg-temp-syms (map z:binding-var arg-temps)]
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
               `(#%begin (,(make-break 'double-break)) ,expr)
               expr))
         
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
                             (search-exprs (search-exprs object))) ; can source exprs be here? (is this a bug?)
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
         ; b) a list of all findins which this expression is tail w.r.t. 
         ;    or 'all to indicate that this expression is tail w.r.t. _all_ bindings.
         ; c) a list of varrefs of 'floating' variables; i.e. lexical bindings  NO: TAKEN OUT
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
                  [let-body-recur (lambda (expr bindings) (annotate/inner expr (binding-set-union tail-bound bindings) #f #f))]
                  [cheap-wrap-recur (lambda (expr) (let-values ([(ann _) (non-tail-recur expr)]) ann))]
                  [make-debug-info-normal (lambda (free-bindings)
                                            (make-debug-info expr tail-bound free-bindings 'none))]
                  [make-debug-info-app (lambda (tail-bound free-bindings label)
                                         (make-debug-info expr tail-bound free-bindings label))]
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
                       [free-bindings (if (z:bound-varref? expr)
                                          (list (z:bound-varref-binding expr))
                                          null)]
                       [debug-info (make-debug-info-normal free-bindings)]
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
                              (wcm-break-wrap debug-info (return-value-wrap annotated))) free-bindings))]

               [(z:app? expr)
		(let*-values
                    ([(sub-exprs) (cons (z:app-fun expr) (z:app-args expr))]
                     [(annotated-sub-exprs free-bindings-sub-exprs)
                      (dual-map non-tail-recur sub-exprs)]
                     [(free-bindings) (apply binding-set-union free-bindings-sub-exprs)])
                   (if cheap-wrap?
                       (values (expr-cheap-wrap annotated-sub-exprs) free-bindings)
                       (let* ([arg-temps (build-list (length sub-exprs) get-arg-binding)]
                              [arg-temp-syms (map z:binding-var arg-temps)] 
                              [let-clauses `((,arg-temp-syms 
                                              (#%values ,@(map (lambda (x) `(#%quote ,*unevaluated*)) arg-temps))))]
                              [set!-list (map (lambda (arg-symbol annotated-sub-expr)
                                                `(#%set! ,arg-symbol ,annotated-sub-expr))
                                              arg-temp-syms annotated-sub-exprs)]
                              [new-tail-bound (binding-set-union tail-bound arg-temps)]
                              [app-debug-info (make-debug-info-app new-tail-bound arg-temps 'called)]
                              [annotate-app? (let ([fun-exp (z:app-fun expr)])
                                               (and (z:top-level-varref? fun-exp)
                                                    (non-annotated-proc? fun-exp)))]
                              [final-app (break-wrap (simple-wcm-wrap app-debug-info 
                                                                      (if annotate-app?
                                                                          (return-value-wrap arg-temp-syms)
                                                                          arg-temp-syms)))]
                              [debug-info (make-debug-info-app new-tail-bound
                                                               (binding-set-union free-bindings arg-temps)
                                                               'not-yet-called)]
                              [let-body (wcm-wrap debug-info `(#%begin ,@set!-list ,final-app))]
                              [let-exp `(#%let-values ,let-clauses ,let-body)])
                         (values let-exp free-bindings))))]
	       
	       [(z:struct-form? expr)
		(let ([super-expr (z:struct-form-super expr)]
		      [raw-type (utils:read->raw (z:struct-form-type expr))]
		      [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
		  (if super-expr
		      (let*-values
                          ([(values annotated-super-expr free-bindings-super-expr) 
                            (non-tail-recur super-expr)]
                           [(annotated)
                            `(#%struct 
                              ,(list raw-type annotated-super-expr)
                              ,raw-fields)]
                           [(debug-info) (make-debug-info-normal free-bindings-super-expr)])
                        (values (if cheap-wrap?
                                    (expr-cheap-wrap annotated)
                                    (wcm-wrap debug-info annotated)) 
                                free-bindings-super-expr))
                      (let ([annotated `(#%struct ,raw-type ,raw-fields)])
                        (values (if cheap-wrap?
                                    (expr-cheap-wrap annotated)
                                    (wcm-wrap (make-debug-info-normal null) annotated)) 
                              null))))]

	       [(z:if-form? expr) 
		(let*-values
                    ([(annotated-test free-bindings-test) 
                      (non-tail-recur (z:if-form-test expr))]
                     [(annotated-then free-bindings-then) 
                      (tail-recur (z:if-form-then expr))]
                     [(annotated-else free-bindings-else) 
                      (tail-recur (z:if-form-else expr))]
                     [(free-bindings) (binding-set-union free-bindings-test 
                                                         free-bindings-then 
                                                         free-bindings-else)]
                     [(if-temp-sym) (z:binding-var if-temp)]
                     [(inner-annotated) `(#%if ,if-temp-sym
                                          ,annotated-then
                                          ,annotated-else)]
                     [(annotated-2) (if (utils:signal-not-boolean)
                                        `(#%if (#%boolean? ,if-temp-sym)
                                          ,inner-annotated
                                          (#%raise (,utils:make-not-boolean
                                                    (#%format ,utils:not-boolean-error-format
                                                     ,if-temp-sym)
                                                    (#%current-continuation-marks)
                                                    ,if-temp-sym)))
                                        inner-annotated)])
                  (if cheap-wrap?
                      (values 
                       (expr-cheap-wrap (if (utils:signal-not-boolean)
                                            `(#%let ((,if-temp-sym ,annotated-test)) ,annotated-2)
                                            `(#%if ,annotated-test ,annotated-then ,annotated-else)))
                       free-bindings)
                      (let* ([annotated `(#%begin
                                          (#%set! ,if-temp-sym ,annotated-test)
                                          ,(break-wrap annotated-2))]
                             [debug-info (make-debug-info-app (binding-set-union tail-bound (list if-temp))
                                                              (binding-set-union free-bindings (list if-temp))
                                                              'none)]
                             [wcm-wrapped (wcm-wrap debug-info annotated)]
                             [outer-annotated `(#%let ((,if-temp-sym (#%quote ,*unevaluated*))) ,wcm-wrapped)])
                        (values outer-annotated free-bindings))))]
	       
	       [(z:quote-form? expr)
                (let ([annotated `(#%quote ,(utils:read->raw (z:quote-form-expr expr)))])
                  (values (if cheap-wrap?
                              annotated
                              (wcm-wrap (make-debug-info-normal null) annotated))
                          null))]
               
               [(z:begin-form? expr)
                (if top-level? 
                    (let*-values
                     ([(bodies) (z:begin-form-bodies expr)]
                      [(annotated-bodies free-bindings)
                       (dual-map (lambda (expr)
                                   (annotate/inner expr 'all #f #t)) 
                                 bodies)])
                       (values `(#%begin ,@annotated-bodies)
                               (apply binding-set-union free-bindings)))
                    (let*-values 
                        ([(bodies) (z:begin-form-bodies expr)]
                         [(all-but-last-body last-body-list) 
                          (list-partition bodies (- (length bodies) 1))]
                         [(last-body) (car last-body-list)]
                         [(annotated-a free-bindings-a)
                          (dual-map non-tail-recur all-but-last-body)]
                         [(annotated-final free-bindings-final)
                          (tail-recur last-body)]
                         [(free-bindings) (apply binding-set-union free-bindings-final free-bindings-a)]
                         [(debug-info) (make-debug-info-normal free-bindings)]
                         [(annotated) `(#%begin ,@(append annotated-a (list annotated-final)))])
                       (values (if cheap-wrap?
                                   (expr-cheap-wrap annotated)
                                   (wcm-wrap debug-info annotated))
                               free-bindings)))]

               [(z:begin0-form? expr)
                (let*-values
                    ([(bodies) (z:begin0-form-bodies expr)]
                       [(annotated-bodies free-bindings-lists)
                        (dual-map non-tail-recur bodies)]
                       [(free-bindings) (apply binding-set-union free-bindings-lists)]
                       [(debug-info) (make-debug-info-normal free-bindings)]
                       [(annotated) `(#%begin0 ,@annotated-bodies)])
                   (values (if cheap-wrap?
                               (expr-cheap-wrap annotated)
                               (wcm-wrap debug-info annotated))
                           free-bindings))]
               
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
                (let*-values
                    ([(binding-sets) (z:let-values-form-vars expr)]
                     [(binding-set-list) (apply append binding-sets)]
                     [(vals) (z:let-values-form-vals expr)]
                     [(_1) (for-each utils:check-for-keyword binding-set-list)]
                     [(_2) (for-each mark-never-undefined binding-set-list)]
                     [(annotated-vals free-bindings-vals)
                      (dual-map non-tail-recur vals)]
                     [(annotated-body free-bindings-body)
                      (let-body-recur (z:let-values-form-body expr) binding-set-list)]
                     [(free-bindings) (apply binding-set-union (remq* binding-set-list free-bindings-body)
                                             free-bindings-vals)])
                  (if cheap-wrap?
                      (let ([bindings
                             (map (lambda (bindings val)
                                    `(,(map get-binding-name bindings) ,val))
                                  binding-sets
                                  annotated-vals)])
                        (values (expr-cheap-wrap `(#%let-values ,bindings ,annotated-body)) free-bindings))
                      (let* ([dummy-binding-sets
                              (let ([counter 0])
                                (map (lambda (binding-set)
                                       (map (lambda (binding) 
                                              (begin0 
                                                (get-arg-binding counter)
                                                (set! counter (+ counter 1))))
                                            binding-set))
                                     binding-sets))]
                             [dummy-binding-list (apply append dummy-binding-sets)]
                             [outer-dummy-initialization
                              `([,(map z:binding-var dummy-binding-list)
                                 (#%values ,@(build-list (length dummy-binding-list) 
                                                         (lambda (_) `(#%quote ,*unevaluated*))))])]
                             [set!-clauses
                              (map (lambda (dummy-binding-set val)
                                     `(#%set!-values ,(map z:binding-var dummy-binding-set) ,val))
                                   dummy-binding-sets
                                   annotated-vals)]
                             [inner-transference
                              `([,(map get-binding-name binding-set-list) 
                                 (values ,@(map z:binding-var dummy-binding-list))])]
                             ; time to work from the inside out again
                             [inner-let-values
                              `(#%let-values ,inner-transference ,annotated-body)]
                             [middle-begin
                              `(#%begin ,@set!-clauses ,(double-break-wrap inner-let-values))]
                             [wrapped-begin
                              (wcm-wrap (make-debug-info-app (binding-set-union tail-bound dummy-binding-list)
                                                             (binding-set-union free-bindings dummy-binding-list)
                                                             'let-body)
                                        middle-begin)]
                             [whole-thing
                              `(#%let-values ,outer-dummy-initialization ,wrapped-begin)])
                        (values whole-thing free-bindings))))]
               
               [(z:letrec-values-form? expr)
                (let*-values 
                    ([(binding-sets) (z:letrec-values-form-vars expr)]
                     [(binding-list) (apply append binding-sets)]
                     [(binding-names) (map get-binding-name binding-list)]
                     [(vals) (z:letrec-values-form-vals expr)]
                     [(_1) (when (andmap z:case-lambda-form? vals)
                             (for-each mark-never-undefined binding-list))] ; we could be more aggressive about this.
                     [(_2) (for-each utils:check-for-keyword binding-list)]
                     [(annotated-vals free-bindings-vals)
                      (dual-map non-tail-recur vals)]
                     [(annotated-body free-bindings-body)
                      (let-body-recur (z:letrec-values-form-body expr) 
                                      binding-list)]
                     [(free-bindings-inner) (apply binding-set-union free-bindings-body free-bindings-vals)]
                     [(free-bindings-outer) (remq* binding-list free-bindings-inner)])
                  (if cheap-wrap?
                      (let ([bindings
                             (map (lambda (bindings val)
                                    `(,(map get-binding-name bindings)
                                      ,val))
                                  binding-sets
                                  annotated-vals)])
                        (values (expr-cheap-wrap `(#%letrec-values ,bindings ,annotated-body))
                                free-bindings-outer))
                      (let* ([outer-initialization
                              `((,binding-names (values ,@binding-names)))]
                             [set!-clauses
                              (map (lambda (binding-set val)
                                     `(#%set!-values ,(map get-binding-name binding-set) ,val))
                                   binding-sets
                                   annotated-vals)]
                             [middle-begin
                              `(#%begin ,@set!-clauses ,(double-break-wrap annotated-body))]
                             [wrapped-begin
                              (begin
                                (printf "debug-info: ~n~a~n" 
                                        (make-debug-info-app (binding-set-union tail-bound binding-list)
                                                             (binding-set-union free-bindings-inner binding-list)
                                                             'let-body))
                                (wcm-wrap (make-debug-info-app (binding-set-union tail-bound binding-list)
                                                               (binding-set-union free-bindings-inner binding-list)
                                                               'let-body)
                                          middle-begin))]
                             [whole-thing
                              `(#%letrec-values ,outer-initialization ,wrapped-begin)])
                        (values whole-thing free-bindings-outer))))]
               
	       [(z:define-values-form? expr)  
		(let*-values
                    ([(bindings) (z:define-values-form-vars expr)]
                     [(_1) (map utils:check-for-keyword bindings)]
                     [(binding-names) (map z:binding-var bindings)]
                     
                     ; NB: this next recurrence is NOT really tail, but we cannot
                     ; mark define-values itself, so we mark the sub-expr as
                     ; if it was in tail posn (i.e., we must hold on to 
                     ; bindings).
                     
                     [(val) (z:define-values-form-val expr)]
                     [(annotated-val free-bindings-val)
                      (define-values-recur val)]
                     [(free-bindings) (remq* bindings free-bindings-val)])
                      (cond [(and (z:case-lambda-form? val) (not cheap-wrap?))
                             (values `(#%define-values ,binding-names
                                       (#%let ((,closure-temp ,annotated-val))
                                        (,update-closure-record-name ,closure-temp (#%quote ,(car binding-names)))
                                        ,closure-temp))
                                     free-bindings)]
                            [(z:struct-form? val)
                             (values `(#%define-values ,binding-names
                                       ,(wrap-struct-form binding-names annotated-val)) 
                                     free-bindings)]
                            [else
                             (values `(#%define-values ,binding-names
                                       ,annotated-val) 
                                     free-bindings)]))]
	       
	       [(z:set!-form? expr)
                (utils:check-for-keyword (z:set!-form-var expr)) 
                (let*-values 
                    ([(v) (translate-varref (z:set!-form-var expr))]
                     [(annotated-body rhs-free-bindings)
                      (non-tail-recur (z:set!-form-val expr))]
                     [(free-bindings) (binding-set-union (list (z:set!-form-var expr)) rhs-free-bindings)]
                     [(debug-info) (make-debug-info-normal free-bindings)]
                     [(annotated) `(#%set! ,v ,annotated-body)])
                   (values (if cheap-wrap?
                               (expr-cheap-wrap annotated)
                               (wcm-wrap (make-debug-info-normal free-bindings) annotated))
                           free-bindings))]
                
               [(z:case-lambda-form? expr)
		(let*-values 
                    ([(annotated-cases free-bindings-cases)
                      (dual-map
                       (lambda (arglist body)
                         (let ([binding-list (z:arglist-vars arglist)]
                               [args (utils:arglist->ilist arglist)])
                           (utils:improper-foreach utils:check-for-keyword args) 
                           (utils:improper-foreach mark-never-undefined args)
                           (let*-values
                               ([(annotated free-bindings)
                                 (lambda-body-recur body)]
                                [(new-free-bindings) (remq* binding-list free-bindings)]
                                [(new-annotated) (list (utils:improper-map get-binding-name args) 
                                                       annotated)]) 
                             (values new-annotated new-free-bindings))))
                       (z:case-lambda-form-args expr)
                       (z:case-lambda-form-bodies expr))]
                     [(annotated-case-lambda) (cons '#%case-lambda annotated-cases)] 
                     [(new-free-bindings) (apply binding-set-union free-bindings-cases)]
                     [(closure-info) (make-debug-info-app 'all new-free-bindings 'none)]
                     [(wrapped-annotated) (wcm-wrap (make-debug-info-normal null)
                                                    annotated-case-lambda)]
                     [(hash-wrapped) `(#%let ([,closure-temp ,wrapped-annotated])
                                       (,closure-table-put! (,closure-key-maker ,closure-temp) 
                                        (,make-closure-record 
                                         #f
                                         ,closure-info 
                                         #f))
                                       ,closure-temp)])
		  (values (if cheap-wrap?
                              annotated-case-lambda
                              hash-wrapped)
			  new-free-bindings))]
	       
               ; the annotation for w-c-m is insufficient for
               ; debugging: there must be an intermediate let & set!s to
               ; allow the user to see the computed values for the key and the
               ; value.
               
               [(z:with-continuation-mark-form? expr)
                (let*-values
                    ([(annotated-key free-bindings-key)
                      (non-tail-recur (z:with-continuation-mark-form-key expr))]
                     [(annotated-val free-bindings-val)
                      (non-tail-recur (z:with-continuation-mark-form-val expr))]
                     [(annotated-body free-bindings-body)
                      (non-tail-recur (z:with-continuation-mark-form-body expr))]
                     [(free-bindings) (binding-set-union free-bindings-key free-bindings-val free-bindings-body)]
                     [(debug-info) (make-debug-info-normal free-bindings)]
                     [(annotated) `(#%with-continuation-mark
                                    ,annotated-key
                                    ,annotated-val
                                    ,annotated-body)])
                   (values (if cheap-wrap?
                               (expr-cheap-wrap annotated)
                               (wcm-wrap debug-info annotated))
                           free-bindings))]
               
               [(not cheap-wrap?)
                (e:static-error "cannot annotate units or classes except in cheap-wrap mode")]
               
               [(z:unit-form? expr)
                (let* ([imports (z:unit-form-imports expr)]
                       [exports (map (lambda (export)
                                       (list (translate-varref (car export))
                                             (z:read-object (cdr export))))
                                     (z:unit-form-exports expr))]
                       [clauses (map annotate/top-level (z:unit-form-clauses expr))])
                  (for-each utils:check-for-keyword imports) 
                  (values
                   `(#%unit
                     (import ,@(map get-binding-name imports))
                     (export ,@exports)
                     ,@clauses)
                   null))]
	       
               [(z:compound-unit-form? expr)
                (let ((imports (map get-binding-name
                                    (z:compound-unit-form-imports expr)))
                      (links (z:compound-unit-form-links expr))
                      (exports (z:compound-unit-form-exports expr)))
                  (let
                      ((links
                        (map
                         (lambda (link-clause)
                           (let* ([tag (utils:read->raw (car link-clause))]
                                  [sub-unit (cheap-wrap-recur (cadr link-clause))]
                                  [imports
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
                (let ([vars (z:interface-form-variables expr)])
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
                                   (list (get-binding-name (car element))
                                         (cheap-wrap-recur (cdr element))))
                              (and (utils:check-for-keyword element)
                                   (get-binding-name element))))]
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
                      (,(get-binding-name (z:class*/names-form-this expr))
                       ,(get-binding-name (z:class*/names-form-super-init expr)))
                      ,(cheap-wrap-recur (z:class*/names-form-super-expr expr))
                      ,(map cheap-wrap-recur (z:class*/names-form-interfaces expr))
                      ,(paroptarglist->ilist (z:class*/names-form-init-vars expr))
                      ,@(map
                         (lambda (clause)
                           (cond
                             ((z:public-clause? clause)
                              `(public
                                 ,@(map (lambda (internal export expr)
                                          `((,(get-binding-name internal)
                                             ,(utils:read->raw export))
                                            ,(cheap-wrap-recur expr)))
                                        (z:public-clause-internals clause)
                                        (z:public-clause-exports clause)
                                        (z:public-clause-exprs clause))))
                             ((z:override-clause? clause)
                              `(override
                                 ,@(map (lambda (internal export expr)
                                          `((,(get-binding-name internal)
                                             ,(utils:read->raw export))
                                            ,(cheap-wrap-recur expr)))
                                        (z:override-clause-internals clause)
                                        (z:override-clause-exports clause)
                                        (z:override-clause-exprs clause))))
                             ((z:private-clause? clause)
                              `(private
                                 ,@(map (lambda (internal expr)
                                          `(,(get-binding-name internal)
                                            ,(cheap-wrap-recur expr)))
                                        (z:private-clause-internals clause)
                                        (z:private-clause-exprs clause))))
                             ((z:inherit-clause? clause)
                              `(inherit
                                 ,@(map (lambda (internal inherited)
                                          `(,(get-binding-name internal)
                                            ,(utils:read->raw inherited)))
                                        (z:inherit-clause-internals clause)
                                        (z:inherit-clause-imports clause))))
                             ((z:rename-clause? clause)
                              `(rename
                                ,@(map (lambda (internal import)
                                         `(,(get-binding-name internal)
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
                struct-proc-names))))
  
)
	 
