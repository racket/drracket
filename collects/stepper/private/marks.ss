(module marks mzscheme

  (require (lib "list.ss")
	   (lib "contracts.ss")
           "my-macros.ss")
  
  (provide
   skipto-mark?
   skipto-mark
   strip-skiptos
   mark-list?
   cheap-mark?
   make-cheap-mark
   cheap-mark-source
   make-full-mark ; : make-full-mark-contract
   stx-protector-stx ; : (protector -> identifier) FOR TESTING ONLY
   mark-source
   mark-bindings
   mark-label
   mark-binding-value
   mark-binding-binding
   expose-mark
   display-mark
   lookup-binding
   lookup-binding-list
   debug-key
   extract-mark-list
   (struct normal-breakpoint-info (mark-list kind returned-value-list))
   (struct error-breakpoint-info (message))
   (struct breakpoint-halt ())
   (struct expression-finished (returned-value-list)))
  
  (define-struct normal-breakpoint-info (mark-list kind returned-value-list))
  (define-struct error-breakpoint-info (message))
  (define-struct breakpoint-halt ())
  (define-struct expression-finished (returned-value-list))
  
  (define identifier-list? (listof identifier?))

  (define-struct skipto-mark-struct ())
  (define skipto-mark? skipto-mark-struct?)
  (define skipto-mark (make-skipto-mark-struct))
  (define (strip-skiptos mark-list)
    (filter (lx (not (skipto-mark? _))) mark-list))
  
  (define mark? procedure?)
  (define mark-list? (listof mark?))

  (define make-full-mark-contract (-> syntax? symbol? identifier-list? syntax?)) ; (location label bindings -> mark-stx)
  
  ; debug-key: this key will be used as a key for the continuation marks.
  (define-struct debug-key-struct ())
  (define debug-key (make-debug-key-struct))
  
  (define (extract-mark-list mark-set)
    (strip-skiptos (continuation-mark-set->list mark-set debug-key)))
  
  ; get-label-num : symbol -> num
  ;  returns a number n. get-label-num is a one-to-one mapping from
  ;  symbols to numbers.
  
  (define label-list null)
  (define (get-label-num sym)
    (let loop ([l-list label-list] [count 0])
      (if (null? l-list)
          (begin
            (set! label-list (append label-list (list sym)))
            count)
          (if (eq? sym (car l-list))
              count
              (loop (cdr l-list) (+ count 1))))))

;  (equal?
;   (list (get-label-num 'foo)
;         (get-label-num 'bar)
;         (get-label-num 'baz)
;         (get-label-num 'bar)
;         (get-label-num 'foo)
;         (get-label-num 'baz)
;         (get-label-num 'quux))
;   '(0 1 2 1 0 2 3))
  
  (define-struct full-mark-struct (source label-num bindings))
  ; the 'varargs' creator is used to avoid an extra cons cell in every mark:
  (define (make-full-mark-varargs source label-num . bindings)
    (make-full-mark-struct source label-num bindings))
  
  (define-struct stx-protector (stx))
  
  ; see module top for type
  (define make-full-mark
    (contract
     make-full-mark-contract 
     (lambda (location label bindings)
       (datum->syntax-object #'here `(lambda () (,make-full-mark-varargs 
                                                 (quote-syntax ,location) 
                                                 ,(get-label-num label)
                                                 ,@(apply append (map make-mark-binding-stx bindings))))))
     'make-full-mark
     'caller))
  
  (define-struct cheap-mark (source))
  
  (define (mark-source mark)
    (if (cheap-mark? mark)
        (cheap-mark-source mark)
        (full-mark-struct-source (mark))))
  
  ;; extract-locations : mark-set -> (listof syntax-object)
  (define (extract-locations mark-set)
    (map mark-source (extract-mark-list mark-set)))
    
  ; : identifier -> (list identifier TST)
  (define (make-mark-binding-stx id)
    (list id (make-stx-protector id)))
  
  (define (mark-bindings mark)
    (letrec ([pair-off
              (lambda (lst)
                (cond [(null? lst) null]
                      [(null? (cdr lst)) (error 'mark-bindings 
                                                           "uneven number of vars and bindings")]
                      [else (cons (list (car lst) (cadr lst)) (pair-off (cddr lst)))]))])
      (pair-off (full-mark-struct-bindings (mark)))))
  
  (define (mark-label mark)
    (list-ref label-list (full-mark-struct-label-num (mark))))
  
  (define (mark-binding-value mark-binding)
    (car mark-binding))
  
  (define (mark-binding-binding mark-binding)
    (stx-protector-stx (cadr mark-binding)))

  (define (expose-mark mark)
    (let ([source (mark-source mark)]
          [label (mark-label mark)]
          [bindings (mark-bindings mark)])
      (list source
            label
            (map (lambda (binding)
                   (list (syntax-e (mark-binding-binding binding))
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
  
  (define (binding-matches mark binding)
    (let ([matches
           (filter (lambda (b)
                     (module-identifier=? (mark-binding-binding b) binding))
                   (mark-bindings mark))])
      (if (> (length matches) 1)
          (error 'lookup-binding "multiple bindings found for ~a" binding)
          matches)))
  
  (define lookup-binding
    (contract 
     (-> mark-list? identifier? any)
     (lambda (mark-list binding)
       (if (null? mark-list)
           (error 'lookup-binding "variable not found in environment: ~a~n" (syntax-e binding))
           (let ([matches (binding-matches (car mark-list) binding)])
             (cond [(null? matches)
                    (lookup-binding (cdr mark-list) binding)]
                   [else
                    (car matches)]))))
     'marks.ss
     'caller))
  
  (define (lookup-binding-list mark-list binding)
    (apply append (map (lambda (x) (binding-matches x binding)) mark-list))))
