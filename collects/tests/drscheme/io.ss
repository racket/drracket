

(define (f n) (if (= n 0) null (list (f (- n 1)) (f (- n 1)))))
