
; Tests macro expansion by setting the eval handler and
;  running all tests

(unless (defined? 'expand-load)
  (global-defined-value 'expand-load "all.ss"))

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(let ([orig (current-eval)])
  (dynamic-wind
   (lambda ()
     (current-eval
      (lambda (x)
	(set! mz-test-syntax-errors-allowed? #t)
	(let ([x (expand-defmacro
		  (expand-defmacro
		   (expand-defmacro-once
		    (expand-defmacro-once x))))])
	  (set! mz-test-syntax-errors-allowed? #f)
	  (orig x)))))
   (lambda ()
     (load-relative expand-load))
   (lambda ()
     (current-eval orig))))
