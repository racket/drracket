;;; pr-99.ss

(define x 5)

(thread-wait (thread (lambda () (set! x (current-parameterization)))))

(if (eq? x (current-parameterization))
    (printf "Test is *not* successful~n")
    (printf "Test is successful~n"))

                      