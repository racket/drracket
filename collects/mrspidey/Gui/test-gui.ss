;; test-gui.ss
;; ----------------------------------------------------------------------

(define test-gui-shake-reps 25)

(define (shake-test-files) test-files)

;; ----------------------------------------------------------------------

(define (test-gui-file f)
  (printf "FILE: ~s~n" f)
  (let* ([ftd (list f (lambda () (open-input-file f)) #t)]
         [ftd* (list ftd)])
    (send spidey do-and-show-analysis ftd*)
    (for i 0  test-gui-shake-reps
         (printf "Iteration ~s/~s~n" i  test-gui-shake-reps)
         (send spidey shake-it))
    (send spidey close-all-frames)))

(define (test-gui)
  (for-each test-gui-file (shake-test-files)))


