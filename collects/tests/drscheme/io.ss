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
  (check-output "(display 1 (current-error-port)) (display 1) (display 1 (current-error-port))"
                (format "1~n{embedded \"1\"}~n1"))
  (check-output "(let ([s (make-semaphore)]) (thread (lambda () (display 1) (semaphore-post s))) (semaphore-wait s))"
                "{embedded \"1\"}")
  (check-output
   "(let ([s (make-semaphore)]) (thread (lambda () (display 1 (current-output-port)) (semaphore-post s))) (semaphore-wait s))" 
   "{embedded \"1\"}")
  (check-output
   "(let ([s (make-semaphore)]) (thread (lambda () (display 1 (current-error-port)) (semaphore-post s))) (semaphore-wait s))"
   "1"))

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
  (define (do-input-test program input expected)
    (do-execute drs-frame)
    (type-in-interactions drs-frame program)
    (let ([before-newline-pos (send interactions-text last-position)])
      (type-in-interactions drs-frame (string #\newline))
      (wait (lambda ()
              (= (send interactions-text last-position)
                 (+ before-newline-pos 3)))
            "input box didn't appear")
    ;; output-start-pos skips over the newline and then the input box
      (let ([output-start-pos (+ before-newline-pos 3)])
        (type-string (format input))
        (wait-for-computation drs-frame)
        (let ([got (fetch-output drs-frame 
                                 output-start-pos
                                 (send interactions-text paragraph-end-position
                                       (- (send interactions-text last-paragraph) 1)))])
          (unless (equal? got expected)
            (printf "FAILED: expected: ~s~n             got: ~s~n         program: ~s~n           input: ~s~n"
                    expected got program input))))))
  
  (clear-definitions drs-frame)
  (do-input-test "(read-char)" "a~n" "#\\a")
  (do-input-test "(read-line)" "abcdef~n" "\"abcdef\"")  
  (do-input-test "(list (read-char) (read-line))" "abcdef~n" "(#\\a \"bcdef\")")
  
  (do-input-test "(read)" "a~n" "a")
  (do-input-test "(list (read) (read))" "a a~n" "(a a)")
  (do-input-test "(list (read-char) (read))" "aa~n" "(#\\a a)")
  
  (do-input-test "(begin (read-char) (sleep 1) (read-char))" "ab~ncd~n" "#\\b")
  (do-input-test "(list (read) (sleep 1) (read) (read))" "a b~nc d~n" "(a #<void> b c)"))

(define drs-frame (wait-for-drscheme-frame))
(define interactions-text (ivar drs-frame interactions-text))
(set-language-level! "Textual (MzScheme)")

(output-err-port-checking)
(long-io/execute-test)
(read-char-test)
