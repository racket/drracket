
(unit/sig 
 ()
 (import)
 (let ([load (current-load)]
       [load-extension (current-load-extension)]
       [ep (current-error-port)]
       [tab ""])
   (let ([mk-chain
	  (lambda (load)
	    (lambda (filename)
	      (fprintf ep
		       "~aloading ~a at ~a~n" 
		       tab filename (current-process-milliseconds))
	      (begin0
	       (let ([s tab])
		 (dynamic-wind
		  (lambda () (set! tab (string-append " " tab)))
		  (lambda () 
		    (if (regexp-match "_loader" filename)
			(let ([f (load filename)])
			  (lambda (sym)
			    (fprintf ep
				     "~atrying ~a's ~a~n" tab filename sym)
			    (let ([loader (f sym)])
			      (and loader
				   (lambda ()
				     (fprintf ep
					      "~astarting ~a's ~a at ~a~n" 
					      tab filename sym
					      (current-process-milliseconds))
				     (let ([s tab])
				       (begin0
					(dynamic-wind
					 (lambda () (set! tab (string-append " " tab)))
					 (lambda () (loader))
					 (lambda () (set! tab s)))
					(fprintf ep
						 "~adone ~a's ~a at ~a~n"
						 tab filename sym
						 (current-process-milliseconds)))))))))
			(load filename)))
		  (lambda () (set! tab s))))
	       (fprintf ep
			"~adone ~a at ~a~n"
			tab filename (current-process-milliseconds)))))])
     (current-load (mk-chain load))
     (current-load-extension (mk-chain load-extension)))))

