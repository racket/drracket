(unit/sig stepper:marks^
  (import [z : zodiac:system^]
          [cp : stepper:client-procs^]
          mzlib:function^)
  
  (define (make-full-mark location label bindings)
    `(#%lambda () (#%list ,location (#%quote ,label) ,@(apply append bindings))))
  
  (define (make-cheap-mark location)
    location)
  
  (define (cheap-mark? mark)
    (z:zodiac? mark))
  
  (define (cheap-mark-source mark)
    mark)
  
  (define (mark-source mark)
    (if (cheap-mark? mark)
        (cheap-mark-source mark)
        (car (mark))))
  
  (define (mark-bindings mark)
    (letrec ([pair-off
              (lambda (lst)
                (cond [(null? lst) null]
                      [(null? (cdr lst)) (error 'mark-bindings "uneven number of vars and bindings")]
                      [else (cons (list (car lst) (cadr lst)) (pair-off (cddr lst)))]))])
      (pair-off (cddr (mark)))))
  
  (define (mark-label mark)
    (cadr (mark)))
  
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
                   (list (z:binding-orig-name (mark-binding-binding binding))
                         (mark-binding-value binding)))
                 bindings))))
  
  (define (display-mark mark)
    (let ([exposed (expose-mark mark)])
      (printf "source: ~a~n" (let ([read (cp:read-getter (car exposed))])
                               (and read
                                    (z:sexp->raw read))))
      (printf "label: ~a~n" (cadr exposed))
      (printf "bindings:~n")
      (for-each (lambda (binding-pair)
                  (printf " ~a : ~a~n" (car binding-pair) (cadr binding-pair)))
                (caddr exposed))))
  
  (define (lookup-binding mark-list binding)
    (if (null? mark-list)
        (error 'lookup-binding "variable not found in environment: ~a" binding)
	(let* ([bindings (mark-bindings (car mark-list))]
	       [matches (filter (lambda (b)
				  (eq? binding (mark-binding-binding b)))
                                bindings)])
	  (cond [(null? matches)
		 (lookup-binding (cdr mark-list) binding)]
		[(> (length matches) 1)
		 (error 'lookup-binding "more than one variable binding found for binding: ~a" binding)]
		[else ; (length matches) = 1
		 (car matches)])))))
