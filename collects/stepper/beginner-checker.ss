; beginner-checker.ss

(unit/sig stepper:beginner-checker^
  (import [z : zodiac:system^]
          mzlib:function^
          [e : stepper:error^])
  
  (define (check-variable-duplication exp-list global-names)
    (let top-level-loop ([exp-list exp-list] [defined-names null])
      (cond [(null? exp-list) 'okay]
            [(z:define-values-form? (car exp-list))
             (let ([defined-vars (map z:varref-var (z:define-values-form-vars (car exp-list)))])
               (let loop ([vars defined-vars])
                 (cond [(null? vars)
                        (top-level-loop (cdr exp-list)
                                        (append defined-vars defined-names))]
                       [(memq (car vars) global-names)
                        (e:static-error 'beginner-checker "the primitive ~s is redefined in this program." (car vars))]
                       [(memq (car vars) defined-names)
                        (e:static-error 'beginner-checker "the variable ~s is defined twice in this program." (car vars))]
                       [else
                        (loop (cdr vars))])))]
            [else
             (top-level-loop (cdr exp-list) defined-names)]))))
                     
                      
