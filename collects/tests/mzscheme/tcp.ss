
(define (tread)
  (let ([l (tcp-listen 40000)])
    (let-values ([(r w) (tcp-accept l)])
	(read-line)
	(printf "Hit return to start reading~n")
	(read-line)
	(let loop ([last 'none-read])
	  (let ([v (read r)])
	    (if (eof-object? v)
		last
		(loop v)))))))

(define (twrite)
  (let-values ([(r w) (tcp-connect "localhost" 40000)])
    (let loop ([n 0])
      (if (tcp-port-send-waiting? w)
	  (begin
	    (printf "write-full at ~s~n" n)
	    (let loop ([m 0])
	      (if (= m 5)
		  (begin
		    (printf "done: ~s~n" (+ m n -1))
		    (close-output-port w)
		    (close-input-port r))
		  (begin
		    (fprintf w "~s~n" (+ m n))
		    (loop (add1 m))))))
	  (begin
	    (fprintf w "~s~n" n)
	    (loop (add1 n)))))))

		