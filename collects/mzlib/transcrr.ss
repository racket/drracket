
(unit/sig mzlib:transcript^
  (import)

  (define-values (transcript-on transcript-off)
    (let ([in #f]
	  [out #f]
	  [err #f]
	  [tee-out (lambda (p p2)
		     (make-output-port
		      (lambda (s)
			(display s p)
			(display s p2)
			(flush-output p)
			(flush-output p2))
		      void))]
	  [tee-in (lambda (in out)
		    (let ([s null])
		      (make-input-port
		       (lambda () 
			 (let loop ()
			   (if (null? s)
			       (begin
				 (let loop ()
				   (set! s (cons (read-char in) s))
				   (when (char-ready? in)
				     (loop)))
				 (set! s (reverse! s))
				 (for-each 
				  (lambda (c) (unless (eof-object? c) (write-char c out))) 
				  s)
				 (flush-output out)
				 (loop))
			       (begin0
				(car s)
				(set! s (cdr s))))))
		       (lambda () (char-ready? in))
		       void
		       (lambda () (peek-char in)))))])
      (values
       (lambda (file)
	 (when in
	   (error 'transcript-on "transcript is already on"))
	 (let ([p (open-output-file file)])
	   (set! in (current-input-port))
	   (set! out (current-output-port))
	   (set! err (current-error-port))
	   (current-output-port (tee-out out p))
	   (current-error-port (tee-out err p))
	   (current-input-port (tee-in in p))))
       (lambda ()
	 (unless in
	   (error 'transcript-on "transcript is not on"))
	 (current-input-port in)
	 (current-output-port out)
	 (current-error-port err)
	 (set! in #f)
	 (set! out #f)
	 (set! err #f))))))

