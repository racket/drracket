
; Tests macro expansion by setting the eval handler and
;  running all tests

(let ([orig (current-eval)])
  (dynamic-wind
   (lambda ()
     (current-eval
      (lambda (x)
	(orig
	 (expand-defmacro
	  (expand-defmacro
	   (expand-defmacro-once
	    (expand-defmacro-once x))))))))
   (lambda ()
     (load-relative "all.ss"))
   (lambda ()
     (current-eval orig))))
