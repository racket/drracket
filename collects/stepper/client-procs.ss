(unit/sig stepper:client-procs^
  (import [z : zodiac:system^])
  
  (define (make-client-pair name)
    (let-values ([(getter setter) (z:register-client name (lambda () #f))])
      (values
       (lambda (parsed) (getter (z:parsed-back parsed)))
       (lambda (parsed n) (setter (z:parsed-back parsed) n)))))
          
  (define-values (never-undefined-getter never-undefined-setter)
    (make-client-pair 'maybe-undefined))
  
  (define-values (read-getter read-setter)
    (make-client-pair 'read)))