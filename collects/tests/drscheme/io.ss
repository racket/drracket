(set-language-level! "Textual (MzScheme)")

(define frame (wait-for-drscheme-frame))

(define (check-output expression expected)
  (begin
     (clear-definitions frame)
     (type-in-definitions frame expression)
     (do-execute frame)
     (let ([got (fetch-output frame)])
       (unless (equal? expected got)
	 (error 'io.ss "expected ~s, got ~s for ~s" expected got expression)))))

(check-output "(display 1)" "[1]")
(check-output "(display 1 (current-output-port))" "[1]")
(check-output "(display 1 (current-error-port))" "1")
(check-output "(display 1) (display 1 (current-error-port))" (format "[1]~n1"))
(check-output "(display 1 (current-error-port)) (display 1)" (format "1~n[1]"))
(check-output "(display 1) (display 1 (current-error-port)) (display 1)" (format "[1]~n1~n[1]"))
(check-output "(display 1 (current-error-port)) (display 1) (display 1 (current-error-port))" (format "1~n[1]~n1"))
(check-output "(let ([s (make-semaphore)]) (thread (lambda () (display 1) (semaphore-post s))) (semaphore-wait s))" "[1]")
(check-output "(let ([s (make-semaphore)]) (thread (lambda () (display 1 (current-output-port)) (semaphore-post s))) (semaphore-wait s))" "[1]")
(check-output "(let ([s (make-semaphore)]) (thread (lambda () (display 1 (current-error-port)) (semaphore-post s))) (semaphore-wait s))" "1")


;; long io / execute test
(clear-definitions frame)
(type-in-definitions
 frame
 "(let f ([n 7] [p null]) (if (= n 0) p (list (f (- n 1) (cons 'l p)) (f (- n 1)  (cons 'r p)))))")
(do-execute frame)
(clear-definitions frame)
(do-execute frame)
(unless (equal? "" (fetch-output frame))
  (error 'io.ss "failed long io / execute test"))
