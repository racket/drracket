(module marks mzscheme

  (require (lib "list.ss")
	   (lib "contract.ss")
           "my-macros.ss"
           "shared.ss")

  (define-struct full-mark-struct (source label bindings values))

  ; CONTRACTS
  (define mark? (-> ; no args  
                 full-mark-struct?))
  (define mark-list? (listof procedure?))
  
  (provide/contract 
   ;[make-debug-info (-> any? binding-set? varref-set? any? boolean? syntax?)] ; (location tail-bound free label lifting? -> mark-stx)
   [expose-mark (-> mark? (list/p any? symbol? (listof (list/p identifier? any?))))]
   [make-top-level-mark (syntax? . -> . syntax?)]
   [lookup-binding (case-> (-> mark-list? identifier? any)
                           (-> mark-list? identifier? procedure? any)
                           (-> mark-list? identifier? procedure? procedure? any))]
   [lookup-binding-with-symbol (-> mark-list? symbol? any)]
   [lookup-binding-list-with-symbol (-> mark-list? symbol? any)])
  
  (provide
   make-debug-info
   wcm-wrap
   skipto-mark?
   skipto-mark
   strip-skiptos
   mark-list?
   mark-source
   mark-bindings
   mark-label
   mark-binding-value
   mark-binding-binding
   display-mark
   all-bindings
   lookup-binding-list
   debug-key
   extract-mark-list
   (struct normal-breakpoint-info (mark-list kind))
   (struct error-breakpoint-info (message))
   (struct breakpoint-halt ())
   (struct expression-finished (returned-value-list)))
  
  ; BREAKPOINT STRUCTURES
  
  (define-struct normal-breakpoint-info (mark-list kind))
  (define-struct error-breakpoint-info (message))
  (define-struct breakpoint-halt ())
  (define-struct expression-finished (returned-value-list))
  
  (define identifier-list? (listof identifier?))

  (define-struct skipto-mark-struct ())
  (define skipto-mark? skipto-mark-struct?)
  (define skipto-mark (make-skipto-mark-struct))
  (define (strip-skiptos mark-list)
    (filter (lx (not (skipto-mark? _))) mark-list))
  
    
  ; debug-key: this key will be used as a key for the continuation marks.
  (define-struct debug-key-struct ())
  (define debug-key (make-debug-key-struct))
  
  (define (extract-mark-list mark-set)
    (strip-skiptos (continuation-mark-set->list mark-set debug-key)))
   
  
  ; the 'varargs' creator is used to avoid an extra cons cell in every mark:
  (define (make-make-full-mark-varargs source label bindings)
    (lambda values 
      (make-full-mark-struct source label bindings values)))
  
  ; see module top for type
  (define (make-full-mark location label bindings)
    (datum->syntax-object #'here `(lambda () (,(make-make-full-mark-varargs location label bindings)
                                              ,@(map make-mark-binding-stx bindings)))))
  
  (define (mark-source mark)
    (full-mark-struct-source (mark)))
  
  ;; extract-locations : mark-set -> (listof syntax-object)
  (define (extract-locations mark-set)
    (map mark-source (extract-mark-list mark-set)))
    
  ; : identifier -> identifier
  (define (make-mark-binding-stx id)
    #`(lambda () #,(syntax-property id 'stepper-dont-check-for-function #t)))
  
  (define (mark-bindings mark)
    (map list 
         (full-mark-struct-bindings (mark)) 
         (full-mark-struct-values (mark))))
  
  (define (mark-label mark)
    (full-mark-struct-label (mark)))
  
  (define (mark-binding-value mark-binding)
    ((cadr mark-binding)))
  
  (define (mark-binding-binding mark-binding)
    (car mark-binding))

  (define (expose-mark mark)
    (let ([source (mark-source mark)]
          [label (mark-label mark)]
          [bindings (mark-bindings mark)])
      (list source
            label
            (map (lambda (binding)
                   (list (mark-binding-binding binding)
                         (mark-binding-value binding)))
                 bindings))))
  
  (define (display-mark mark)
    (apply
     string-append
     (format "source: ~a~n" (syntax-object->datum (mark-source mark)))
     (format "label: ~a~n" (mark-label mark))
     (format "bindings:~n")
     (map (lambda (binding)
                 (format " ~a : ~a~n" (syntax-e (mark-binding-binding binding))
                         (mark-binding-value binding)))
               (mark-bindings mark))))
  
  
  ; possible optimization: rig the mark-maker to guarantee statically that a
  ; variable can occur at most once in a mark.  
  
  (define (binding-matches matcher mark binding)
    (let ([matches
           (filter (lambda (b)
                     (matcher (mark-binding-binding b) binding))
                   (mark-bindings mark))])
      (if (and (pair? matches) (pair? (cdr matches))) ; (> (length matches) 1)
          (error 'lookup-binding "multiple bindings found for ~a" binding)
          matches)))
  
  
  (define lookup-binding
    (case-lambda
      ((mark-list binding binding-matcher fail-thunk)
       (if (null? mark-list)
           (fail-thunk)
          (let ([matches (binding-matches binding-matcher (car mark-list) binding)])
            (cond [(null? matches)
                   (lookup-binding (cdr mark-list) binding binding-matcher fail-thunk)]
                  [else
                   (mark-binding-value (car matches))]))))
      ((mark-list binding binding-matcher)
       (lookup-binding mark-list binding binding-matcher 
                       (lambda ()
                         (error 'lookup-binding "variable not found in environment: ~a~n" (if (syntax? binding) 
                                                                                              (syntax-object->datum binding)
                                                                                              binding)))))
      ((mark-list binding)
       (lookup-binding mark-list
                       binding
                       module-identifier=?))))
 
  (define (lookup-binding-with-symbol mark-list binding)
    (lookup-binding mark-list binding (lambda (id sym) (eq? (syntax-e id) sym))))
  
  (define (lookup-binding-list mark-list binding)
    (apply append (map (lambda (x) (binding-matches module-identifier=? x binding)) mark-list)))
  
  (define (lookup-binding-list-with-symbol mark-list sym)
    (apply append (map (lambda (x) (binding-matches (lambda (stx y) (eq? (syntax-e stx) y)) x sym)) mark-list)))
  
  (define (all-bindings mark)
    (map mark-binding-binding (mark-bindings mark)))
  
  (define (wcm-wrap debug-info expr)
    #`(with-continuation-mark #,debug-key #,debug-info #,expr))

  
  ; DEBUG-INFO STRUCTURES
  
  ;;;;;;;;;;
  ;;
  ;; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
  ;; a source expression and a set of binding/value pairs.
  ;; (syntax-object BINDING-SET VARREF-SET any boolean) -> debug-info)
  ;;
  ;;;;;;;;;;
     
  (define (make-debug-info source tail-bound free-vars label lifting?)
       (let*-2vals ([kept-vars (binding-set-varref-set-intersect tail-bound free-vars)])
         (if lifting?
             (let*-2vals ([let-bindings (filter (lambda (var) 
                                                  (case (syntax-property var 'stepper-binding-type)
                                                    ((let-bound macro-bound) #t)
                                                    ((lambda-bound stepper-temp non-lexical) #f)
                                                    (else (error 'make-debug-info 
                                                                 "varref ~a's binding-type info was not recognized: ~a"
                                                                 (syntax-e var)
                                                                 (syntax-property var 'stepper-binding-type)))))
                                                kept-vars)]
                          [lifter-syms (map get-lifted-var let-bindings)])
               (make-full-mark source label (append kept-vars lifter-syms)))
             (make-full-mark source label kept-vars))))
  
  
  (define (make-top-level-mark source-expr)
    (make-full-mark source-expr 'top-level null)))
