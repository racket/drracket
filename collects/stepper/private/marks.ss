(module marks mzscheme

  (require (lib "list.ss"))
  
  (provide
   cheap-mark?
   make-cheap-mark
   cheap-mark-source
   make-full-mark
   mark-source
   mark-bindings
   mark-label
   mark-binding-value
   mark-binding-binding
   expose-mark
   ;display-mark
   lookup-binding
   lookup-binding-list
   debug-key
   extract-mark-list)
  
  ; debug-key: this key will be used as a key for the continuation marks.
  (define-struct debug-key-struct ())
  (define debug-key (make-debug-key-struct))
  
  (define (extract-mark-list mark-set)
    (continuation-mark-set->list mark-set debug-key))
  
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
  
  (define (make-full-mark location label bindings)
    (datum->syntax-object #f `(lambda () (,make-full-mark-varargs ,location ,(get-label-num label) ,@(apply append bindings)))))
  
  (define-struct cheap-mark (source))
  
  (define (mark-source mark)
    (if (cheap-mark? mark)
        (cheap-mark-source mark)
        (full-mark-struct-source (mark))))
  
  ;; extract-locations : mark-set -> (listof syntax-object)
  (define (extract-locations mark-set)
    (map mark-source (extract-mark-list mark-set)))
    
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
    (cadr mark-binding))

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
  
  '(define (display-mark mark)
    (let ([exposed (expose-mark mark)])
      (printf "source: ~a~n" (let ([read (cp:read-getter (car exposed))])
                               (and read
                                    (z:sexp->raw read))))
      (printf "label: ~a~n" (cadr exposed))
      (printf "bindings:~n")
      (for-each (lambda (binding-pair)
                  (printf " ~a : ~a~n" (car binding-pair) (cadr binding-pair)))
                (caddr exposed))))
  
  (define (binding-matches mark binding)
    (let ([matches
           (filter (lambda (b)
                     (eq? binding (mark-binding-binding b)))
                   (mark-bindings mark))])
      (if (> (length matches) 1)
          (error 'lookup-binding "multiple bindings found for ~a" binding)
          matches)))
  
  (define (lookup-binding mark-list binding)
    (if (null? mark-list)
        (error 'lookup-binding "variable not found in environment: ~a" binding)
        (let ([matches (binding-matches (car mark-list) binding)])
          (cond [(null? matches)
                 (lookup-binding (cdr mark-list) binding)]
                [else
                 (car matches)]))))
  
  (define (lookup-binding-list mark-list binding)
    (apply append (map (lambda (x) (binding-matches x binding)) mark-list))))
