(module io mzscheme
  (require "drscheme-test-util.ss"
           (lib "gui.ss" "tests" "utils")
           (lib "class.ss")
           (lib "list.ss")
           (lib "pretty.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix fw: (lib "framework.ss" "framework")))
  
  (provide run-test)
  
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
    (let ([string-port (open-output-string)])
      (pretty-print
       (let f ([n 7] [p null]) (if (= n 0) p (list (f (- n 1) (cons 'l p)) (f (- n 1)  (cons 'r p)))))
       string-port)
      (clear-definitions drs-frame)
      (type-in-definitions
       drs-frame
       "(let f ([n 7] [p null]) (if (= n 0) p (list (f (- n 1) (cons 'l p)) (f (- n 1)  (cons 'r p)))))")
      (do-execute drs-frame)
      (let ([got-output (fetch-output drs-frame)])
        (clear-definitions drs-frame)
        (do-execute drs-frame)
        (unless (equal? "" (fetch-output drs-frame))
          (error 'io.ss "failed long io / execute test (extra io)"))
        (unless (whitespace-string=?
                 (get-output-string string-port)
                 got-output)
          (error 'io.ss "failed long io / execute test (output doesn't match)")))))
  
  
  (define (reading-test)
    (define (do-input-test program input expected-value expected-io)
      (do-execute drs-frame)
      (type-in-interactions drs-frame program)
      (let ([before-newline-pos (send interactions-text last-position)])
        (type-in-interactions drs-frame (string #\newline))
        (wait (lambda ()
                (= (send interactions-text last-position)
                   (+ before-newline-pos 4)))
              "input box didn't appear")
	;; output-start-pos skips over the newline and then the input box
        (let ([output-start-pos (+ before-newline-pos 3)])
          (type-string (format input))
          (wait-for-computation drs-frame)
          (let ([got-io
                 (fetch-output drs-frame 
                               (+ before-newline-pos 1)
                               (+ before-newline-pos 2))]
                [got-value
                 (fetch-output drs-frame 
                               output-start-pos
                               (send interactions-text paragraph-end-position
                                     (- (send interactions-text last-paragraph) 1)))])
            (when expected-io
              (unless (equal? got-io expected-io)
                (printf "FAILED IO: expected: ~s~n                got: ~s~n            program: ~s~n              input: ~s~n"
                        expected-io got-io program input)))
            (unless (equal? got-value expected-value)
              (printf "FAILED: expected: ~s~n             got: ~s~n         program: ~s~n           input: ~s~n"
                      expected-value got-value program input))))))
    
    (clear-definitions drs-frame)
    (do-input-test "(read-char)" "a~n" "#\\a" #f)
    (do-input-test "(read-line)" "abcdef~n" "\"abcdef\"" #f)  
    (do-input-test "(list (read-char) (read-line))" "abcdef~n" "(#\\a \"bcdef\")" #f)
    
    (do-input-test "(read)" "a~n" "a" #f)
    (do-input-test "(list (read) (read))" "a a~n" "(a a)" #f)
    (do-input-test "(list (read-char) (read))" "aa~n" "(#\\a a)" #f)
    
    (do-input-test "(begin (read-char) (sleep 1) (read-char))" "ab~ncd~n" "#\\b" #f)
    (do-input-test "(list (read) (sleep 1) (read) (read))" "a b~nc d~n" "(a #<void> b c)" #f)
    
    (do-input-test "(begin (display 1) (read))" "2~n" "2" #f)
    
    (do-input-test "(read-line)" "~n" "\"\"" #f)
    (do-input-test "(read-char)" "~n" "#\\newline" #f)
    
    (do-input-test "(list (read) (printf \"1~n\") (read) (printf \"3~n\"))"
                   "0 2\n"
                   "(0 #<void> 2 #<void>)"
                   "{embedded \"0 2\n1\n3\"}")
    
    (do-input-test "(write (read))"
                   "()\n"
                   ""
                   "{embedded \"()\n()\"}")
    
    (do-input-test "(begin (write (read)) (write (read)))"
                   "(1)\n(2)\n"
                   ""
                   "{embedded \"(1)\n(1)(2)\n(2)\"}"))
  
  (define drs-frame (wait-for-drscheme-frame))
  (define interactions-text (send drs-frame get-interactions-text))
  (set-language-level! (list "PLT" (regexp "Textual")))
  
  (define (run-test)
    (long-io/execute-test)
    (output-err-port-checking)
    (reading-test)
    ))
