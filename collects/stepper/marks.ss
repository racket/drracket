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
  
  (define (mark-binding-varref mark-binding)
    (cadr mark-binding))

  (define (original-name varref)
    (if (z:top-level-varref? varref)
        (z:varref-var varref)
        (let ([binding (z:bound-varref-binding varref)])
          (if binding
              (z:binding-orig-name binding)
              (z:varref-var varref))))) ; this happens for application temps
  
  (define (expose-mark mark)
    (let ([source (mark-source mark)]
          [label (mark-label mark)]
          [bindings (mark-bindings mark)])
      (list source
            label
            (map (lambda (binding)
                   (list (original-name (mark-binding-varref binding))
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
  
  (define (lookup-var-binding mark-list var)
    (printf "entering lookup-var-binding~n")
    (if (null? mark-list)
        ; must be a primitive
        (begin
          (printf "going into error~n")
          (error 'lookup-var-binding "variable not found in environment: ~a" var))
	; (error var "no binding found for variable.")
	(let* ([bindings (mark-bindings (car mark-list))]
               [_ (printf "bindings: ~a~n" bindings)]
	       [matches (filter (lambda (mark-var)
				  (eq? var (z:varref-var (mark-binding-varref mark-var))))
                                bindings)])
          (printf "matches length: ~a~n" (length matches))
	  (cond [(null? matches)
		 (lookup-var-binding (cdr mark-list) var)]
		[(> (length matches) 1)
		 (error 'lookup-var-binding "more than one variable binding found for var: ~a" var)]
		[else ; (length matches) = 1
		 (car matches)])))))
