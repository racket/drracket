
(unless (defined? 'flat-load)
   (define flat-load "all.ss"))
(unless (defined? 'lines-per-file)
   (define lines-per-file +inf.0))

(require-library "pretty.ss")


(define line-count 0)
(define file-count 0)

(define flatp (open-output-file "flat.ss" 'replace))
(define old-eval (current-eval))
(define old-namespace (current-namespace))

(pretty-print '(define error-test void) flatp)
(pretty-print '(define building-flat-tests #t) flatp)
(pretty-print '(define section #f) flatp)

(define (flat-pp v)
  (pretty-print v flatp)
  (set! line-count (add1 line-count))
  (when (>= line-count lines-per-file)
	(set! line-count 0)
	(set! file-count (add1 file-count))
	(close-output-port flatp)
	(set! flatp
	      (open-output-file
	       (format "flat~a.ss" file-count)
	       'replace))))

(define error-test
  (case-lambda
   [(expr) (error-test expr #f)]
   [(expr exn?)
    (unless (eq? exn? exn:syntax?)
	    (flat-pp `(thunk-error-test (lambda () ,expr)
					(quote ,expr)
					,@(if exn?
					      (list (string->symbol
						     (primitive-name
						      exn?)))
					      null))))]))

(define building-flat-tests #t)

(dynamic-wind
 (lambda () 
   (current-eval
    (lambda (e)
      (unless (or (and (pair? e)
		       (memq (car e) '(load load-relative error-test)))
		  (not (eq? (current-namespace) old-namespace)))
	      (flat-pp e))
      (old-eval e))))
 (lambda ()
   (load flat-load))
 (lambda ()
   (current-eval old-eval)))
