
(define to-zo? (equal? argv #("zo")))

(define DIGS-PER-LINE 20)

(let loop ([s ""])
  (let ([expr (read)])
    (unless (eof-object? expr)
	    (if (eq? expr '>)
		(let ([tag (read)])
		  (if (eq? tag 'literal)
		      (begin
			(unless to-zo?
				(display (read))
				(newline))
			(loop s))
		      (let ([args (let loop ()
				    (let ([v (read)])
				      (if (or (eof-object? v) (eq? v '<))
					  null
					  (cons v (loop)))))]
			    [prefix
			     (case tag
			       [(stop) 'JUST_DEFINED]
			       [(qstop) 'JUST_DEFINED_QQ]
			       [(fstop) 'JUST_DEFINED_FUNC]
			       [(kstop) 'JUST_DEFINED_KEY]
			       [(cstop) 'JUST_DEFINED_COND])])
			(if to-zo?
			    (begin
			      (display s) 
			      (newline))
			    (begin
			      (printf "  {~n    static MZCOMPILED_STRING_FAR unsigned char expr[] = {")
			      (let loop ([chars (string->list s)][pos 0])
				(unless (null? chars)
					(let ([char (car chars)])
					  (printf "~a, " (char->integer char)))
					(loop (cdr chars)
					      (if (= pos DIGS-PER-LINE)
						  (begin
						    (newline)
						    0)
						  (add1 pos)))))
			      (printf "0};~n    EVAL_ONE_SIZED_STR((char *)expr, ~a);~n" (string-length s))
			      (for-each
			       (lambda (id)
				 (printf "    ~a(~a);~n" prefix id))
			       args)
			      (printf "  }~n")))
			(loop ""))))
		(let ([c (compile expr)]
		      [p (open-output-string)])
		  (write c p)
		  (loop (string-append s (get-output-string p))))))))



		   
