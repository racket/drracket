;; env.ss
;; ----------------------------------------------------------------------
;; Environments

    (define empty-env '())

    (define lookup-or-fail
      (lambda (env x fail-thunk)
	(match (assq x env)
	  [#f (fail-thunk)]
	  [(_ . b) b])))

    (define lookup-or-#f
      (lambda (env x)
	(match (assq x env)
	  [#f #f]
	  [(_ . b) b])))

    (define lookup
      (lambda (env x)
	(match (assq x env)
	  [#f (mrspidey:internal-error 'lookup "no binding for ~a" x)]
	  [(_ . b) b])))

    (define bound-in-env?
      (lambda (env x)
	(match (assq x env)
	  [#f #f]
	  [_ #t])))

    (define extend-env
      (lambda (env x v)
	(cons (cons x v) env)))

    (define extend-env*
      (lambda (env xs vs)
	(append (map cons xs vs) env)))

    (define join-env
      (lambda (env newenv)
	(append newenv env)))

    (define bang-env!
      (lambda (env x nu-v)
        (let ([binding (assq x env)])
          (if binding
              (set-cdr! binding nu-v)
              (mrspidey:internal-error 'lookup "no binding for ~a" x)))))	
	
(define (env:change-binding env x f err)
  (recur loop ([env env])
    (if (null? env)
      (err)
      (if (eq? x (caar env))
        (cons (cons x (f (cdar env))) (cdr env))
        (cons (car env) (loop (cdr env)))))))

(define (env:remove env x)
  (let* ([bind #f]
         [env
           (recur loop ([env env])
             (if (null? env)
               '()
               (if (eq? x (caar env))
                 (begin
                   (set! bind (cdar env))
                   (cdr env))
                 (cons (car env) (loop (cdr env))))))])
    (values bind env)))
