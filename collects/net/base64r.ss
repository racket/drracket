
(unit/sig mzlib:base64^
  (import)

  (define (base64-encode src)
    ; Always includes a terminator
    (let* ([len (string-length src)]
	   [new-len (let ([l (add1 (ceiling (* len 8/6)))])
		      ; Break l into 72-character lines.
		      ; Insert CR/LF between each line.
		      (+ l (* (quotient l 72) 2)))]
	   [dest (make-string new-len #\0)]
	   [char-map (list->vector
		      (let ([each-char (lambda (s e)
					 (let loop ([l null][i (char->integer e)])
					   (if (= i (char->integer s))
					       (cons s l)
					       (loop (cons (integer->char i)
							   l)
						     (sub1 i)))))])
			(append
			 (each-char #\A #\Z)
			 (each-char #\a #\z)
			 (each-char #\0 #\9)
			 (list #\+ #\/))))])
      (let loop ([bits 0][v 0][col 0][srcp 0][destp 0])
	(cond
	 [(= col 72)
	  ; Insert CRLF
	  (string-set! dest destp #\return)
	  (string-set! dest (add1 destp) #\linefeed)
	  (loop bits
		v
		0
		srcp
		(+ destp 2))]
	 [(and (= srcp len)
	       (<= bits 6))
	  ; That's all, folks.
	  ; Write the last few bits.
	  (begin
	    (string-set! dest destp (vector-ref char-map (arithmetic-shift v (- 6 bits))))
	    (add1 destp))
	  (if (= col 71)
	      ; Have to write CRLF before terminator
	      (begin
		(string-set! dest (+ destp 1) #\return)
		(string-set! dest (+ destp 2) #\linefeed)
		(string-set! dest (+ destp 3) #\=))
	      (string-set! dest (add1 destp) #\=))
	  dest]
	 [(< bits 6)
	  ; Need more bits.
	  (loop (+ bits 8)
		(bitwise-ior (arithmetic-shift v 8)
			     (char->integer (string-ref src srcp)))
		col
		(add1 srcp)
		destp)]
	 [else
	  ; Write a char.
	  (string-set! dest destp (vector-ref char-map (arithmetic-shift v (- 6 bits))))
	  (loop (- bits 6)
		(bitwise-and v (sub1 (arithmetic-shift 1 (- bits 6))))
		(add1 col)
		srcp
		(add1 destp))])))))

