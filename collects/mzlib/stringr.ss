  (unit/sig
   mzlib:string^
   (import)

   (define make-string-do!
     (lambda (translate)
       (lambda (s)
	 (let loop ([n (sub1 (string-length s))])
	   (unless (negative? n)
		   (string-set! s n
				(translate (string-ref s n)))
		   (loop (sub1 n)))))))
   (define string-lowercase! (make-string-do! char-downcase))
   (define string-uppercase! (make-string-do! char-upcase))

   (define eval-string
     (let ([do-eval
	    (lambda (str)
	      (let ([p (open-input-string str)])
		(apply
		 values
		 (let loop ()
		   (let ([e (read p)])
		     (if (eof-object? e)
			 '()
			 (call-with-values
			  (lambda () (eval e))
			  (case-lambda
			   [() (loop)]
			   [(only) (cons only (loop))]
			   [multi 
			    (append multi (loop))]))))))))])
       (case-lambda
	[(str) (eval-string str #f #f)]
	[(str error-display) (eval-string str error-display #f)]
	[(str error-display error-result)
	 (if (or error-display error-result)
	     (with-handlers ([void
			      (lambda (exn)
				((or error-display (error-display-handler))
				 (exn-message exn))
				(if error-result
				    (error-result)
				    #f))])
		 (do-eval str))
	     (do-eval str))])))

   (define read-from-string-one-or-all
     (case-lambda
      [(k all? str) (read-from-string-one-or-all k all? str #f #f)]
      [(k all? str error-display) (read-from-string-one-or-all k all? str error-display #f)]
      [(k all? str error-display error-result)
       (let* ([p (open-input-string str)]
	      [go (lambda ()
		    (let loop ()
		      (let ([v (read p)])
			(if (eof-object? v)
			    '()
			    (cons v
				  (if all?
				      (loop)
				      '()))))))])
	 (if error-display
	     (with-handlers ([void
			      (lambda (exn)
				((or error-display (error-display-handler))
				 (exn-message exn))
				(k (if error-result
				       (error-result)
				       #f)))])
		 (go))
	     (go)))]))

   (define read-from-string
     (lambda args
       (let/ec k
         (let ([l (apply read-from-string-one-or-all k #f args)])
	   (if (null? l)
	       eof
	       (car l))))))
     
   (define read-from-string-all
     (lambda args
       (let/ec k
         (apply read-from-string-one-or-all k #t args))))
   
   (define expr->string
     (lambda (v)
       (let* ([s ""]
	      [write-to-s
	       (lambda (str)
		 (set! s (string-append s str)))]
	      [port (make-output-port write-to-s (lambda () #f))])
	 (write v port)
	 s)))
   
   (define newline-string (string #\newline))
   
   (define regexp-match-exact?
     (lambda (p s)
       (let ([m (regexp-match p s)])
	 (and m
	      (string=? (car m) s)))))
   )
