(define semaphore (make-semaphore 0))
(define loop-size 3000)
(define events/loop 10)

(define frame (make-object frame% "frame" #f 100 100))
(define counter 0)
(define canvas
  (make-object 
   (class canvas% ()
     (inherit refresh)
     (override
      [on-paint
       (lambda ()
	 (cond
	   [(equal? 0 counter)
	    (void)]
	   [else
	    (set! counter (- counter 1))
	    (refresh)]))])
     (sequence (super-init frame)))))
		
(send frame show #t)
;(event-dispatch-handler (let ([orig (event-dispatch-handler)]) (lambda (eventspace) (orig eventspace))))

(define (test name body-expression after-expression)
  (let ([start-time (current-milliseconds)])
    (eval
     `(let loop ([n loop-size])
	(unless (zero? n)
	  ,body-expression
	  (loop (- n 1)))))
    (let* ([end-time (current-milliseconds)]
	   [total-time (- end-time start-time)])

      (eval after-expression)

      (printf "~a: time per event ~a msec~n~a: total time ~a msec~n"
	      name
	      (exact->inexact 
	       (/ (floor (* (/ total-time loop-size events/loop) 1000)) 1000))
	      name
	      total-time))))


(test
 "canvas"
 `(begin
    ,@(let loop ([n events/loop])
	 (cond
	   [(zero? n) `()]
	   [else `((queue-callback void)
		   .
		   ,(loop (- n 1)))])))
 '(void))


(test
 "queue"
 '(begin (set! counter events/loop)
	 (send canvas refresh))
 '(begin (queue-callback (lambda () (semaphore-post semaphore)))
	 (yield semaphore)))

