(define frame (wait-for-drscheme-frame))

(clear-definitions frame)
(type-in-definitions frame "(display 1)")
(do-execute frame)
(message-box "output" (fetch-output frame))
(define (f n) (if (= n 0) null (list (f (- n 1)) (f (- n 1)))))
