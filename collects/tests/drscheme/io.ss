(define (output-err-port-checking)
  (define (check-output expression expected)
    (begin
      (clear-definitions drs-frame)
      (type-in-definitions drs-frame expression)
      (do-execute drs-frame)
      (let ([got (fetch-output drs-frame)])
        (unless (equal? expected got)
          (error 'io.ss "expected ~s, got ~s for ~s" expected got expression)))))
  
  (check-output "(display 1)" "{embedded \"1\"}")
  (check-output "(display 1 (current-output-port))" "{embedded \"1\"}")
  (check-output "(display 1 (current-error-port))" "1")
  (check-output "(display 1) (display 1 (current-error-port))" (format "{embedded \"1\"}~n1"))
  (check-output "(display 1 (current-error-port)) (display 1)" (format "1~n{embedded \"1\"}"))
  (check-output "(display 1) (display 1 (current-error-port)) (display 1)" (format "{embedded \"1\"}~n1~n{embedded \"1\"}"))
  (check-output "(display 1 (current-error-port)) (display 1) (display 1 (current-error-port))" (format "1~n{embedded \"1\"}~n1"))
  (check-output "(let ([s (make-semaphore)]) (thread (lambda () (display 1) (semaphore-post s))) (semaphore-wait s))" "{embedded \"1\"}")
  (check-output "(let ([s (make-semaphore)]) (thread (lambda () (display 1 (current-output-port)) (semaphore-post s))) (semaphore-wait s))" "{embedded \"1\"}")
  (check-output "(let ([s (make-semaphore)]) (thread (lambda () (display 1 (current-error-port)) (semaphore-post s))) (semaphore-wait s))" "1"))

(define (long-io/execute-test)
  (clear-definitions drs-frame)
  (type-in-definitions
   drs-frame
   "(let f ([n 7] [p null]) (if (= n 0) p (list (f (- n 1) (cons 'l p)) (f (- n 1)  (cons 'r p)))))")
  (do-execute drs-frame)
  (clear-definitions drs-frame)
  (do-execute drs-frame)
  (unless (equal? "" (fetch-output drs-frame))
    (error 'io.ss "failed long io / execute test")))

(define (read-char-test)
  (clear-definitions drs-frame)
  (do-execute drs-frame)
  
  (type-in-interactions drs-frame "(begin (read-char) (sleep 1) (read-char))")
  (type-in-interactions drs-frame (string #\newline))
  (type-string (format "ab~ncd~n"))
  (wait-for-computation)
  ;; error!
  )

(define drs-frame (wait-for-drscheme-frame))
(set-language-level! "Textual (MzScheme)")

(read-char-test)
(output-err-port-checking)
(long-io/execute-test)