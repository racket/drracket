
; run a thunk using a censor that removes dangerous chars from a 
; string for printing to a terminal
(lambda (thunk)
  (let ([censor (lambda (s)
		  (list->string
		   (let loop ([s (string->list s)])
		     (if (null? s)
			 null
			 (let ([c (car s)])
			   (cond
			    [(and (not (char-whitespace? c)) (or (char<=? c #\space) (char>=? c #\200)))
			     (append (cons #\{ (string->list 
						(number->string 
						 (char->integer c))))
				     (cons #\} (loop (cdr s))))]
			    [else
			     (cons c (loop (cdr s)))]))))))])
    (let* ([oldp (current-output-port)]
	   [cp (make-output-port
		(lambda (s)
		  (display (censor s) oldp))
		void)])
      (dynamic-wind
       (lambda () (current-output-port cp))
       thunk
       (lambda ()
	 (current-output-port oldp))))))


