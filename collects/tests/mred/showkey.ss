(let ([c%
       (class-asi mred:canvas%
	 (public
	   [on-char
	    (lambda (ev)
	      (printf "code: ~a  meta: ~a alt: ~a  shift: ~a~n" 
		      (send ev get-key-code)
		      (send ev get-control-down)
		      (send ev get-alt-down)
		      (send ev get-shift-down)))]))])
  (define f (make-object mred:frame% null "tests" 0 0 100 100))
  (define p (make-object mred:vertical-panel% f))
  (define c (make-object c% p))
  (send f show #t))

