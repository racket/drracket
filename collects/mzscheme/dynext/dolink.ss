
(require-library "link.ss" "mzscheme" "dynext")

; Parse arguments:
(define a (vector->list argv))

(define quiet? #f)

(when (and (pair? a) (string=? (car a) "-q"))
      (set! quiet? #t)
      (set! a (cdr a)))

(define inputs null)
(define output #f)

(let loop ([a a])
  (unless (null? a)
	  (if (string=? (car a) "-o")
	      (if (null? (cdr a))
		  (error 'dynlink "expected a filename after -o")
		  (if output
		      (error 'dynlink "multiple output files provided")
		      (begin 
			(set! output (cadr a))
			(loop (cddr a)))))
	      (begin
		(set! inputs (append inputs (list (car a))))
		(loop (cdr a))))))

(when (not output)
      (error 'dynlink "no output file specified"))
(when (null? inputs)
      (error 'dynlink "no input files specified"))

(link-extension quiet? inputs output)
