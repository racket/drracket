
(require-library "pretty.ss")

(define quicksort
  (lambda (l less-than)
    (let* ([v (list->vector l)]
	   [count (vector-length v)])
      (let loop ([min 0][max count])
	(if (< min (sub1 max))
	    (let ([pval (vector-ref v min)])
	      (let pivot-loop ([pivot min]
			       [pos (add1 min)])
		(if (< pos max)
		    (let ([cval (vector-ref v pos)])
		      (if (less-than cval pval)
			  (begin
			    (vector-set! v pos (vector-ref v pivot))
			    (vector-set! v pivot cval)
			    (pivot-loop (add1 pivot) (add1 pos)))
			  (pivot-loop pivot (add1 pos))))
		    (if (= min pivot)
			(loop (add1 pivot) max)
			(begin
			  (loop min pivot)
			  (loop pivot max))))))))
      (vector->list v))))

(define (sym<? a b)
  (string<? (symbol->string a) (symbol->string b)))
(define (exspec<? a b)
  (sym<? (cadr a) (cadr b)))

(define (get-one f)
  (with-input-from-file f read))

(define (get-all f)
  (with-input-from-file f
    (lambda () 
      (let loop ()
	(let ([r (read)])
	  (if (eof-object? r)
	      null
	      (cons r (loop))))))))

(define (prefix-wx: s)
  (string->symbol (format "wx:~a" s)))

(define only-imports (get-one "import.ss"))
(define mred-exports (get-one "export.ss"))
(define propagate (get-one "propgate.ss"))

(define imports (quicksort (append only-imports propagate) sym<?))

(define macros (get-all "macros.ss"))
(define (wrap-macros expr)
  (let loop ([l macros][expr expr])
    (if (null? l)
	expr
	(let ([m (car l)])
	`(let-macro ,(cadr m) ,(caddr m)
		    ,(loop (cdr l) expr))))))

(define code (get-all "mred.ss"))

(define sig (cons 'mred@ (append propagate mred-exports)))

(define debug? #f)

(with-output-to-file "wrap.ss"
  (lambda ()
    (when debug?
      (pretty-print '(load "/home/scheme/plt/collects/errortrace/errortrace.ss"))
      (display "> kstop errortrace <")
      (newline))
    (pretty-print
     `(define-signature mred^ ,sig))
    (display "> kstop sig <")
    (newline)
    (let ([c
	   `(lambda (wx@)
	      (letrec ([ex (invoke-unit
			    (compound-unit 
			     (import)
			     (link [wx (wx@)]
				   [mred (,(wrap-macros
					    `(unit (import ,@(map prefix-wx: imports))
						   (export)
						   ,@code
						   (vector ,@mred-exports
							   ,@(map prefix-wx: propagate))))
					  (wx ,@imports))])
			     (export)))])
		(letrec ([mred@
			  (unit->unit/sig
			   (compound-unit
			    (import)
			    (link [wx (wx@)]
				  [mred ((unit (import)
					       (export ,@mred-exports [-mred@ mred@])
					       ,@(let loop ([l mred-exports][n 0])
						   (if (null? l)
						       null
						       (cons `(define ,(car l) (vector-ref ex ,n))
							     (loop (cdr l) (add1 n)))))
					       (define -mred@ mred@)))])
			    (export (wx ,@propagate) (mred mred@ ,@mred-exports)))
			   () ,sig)])
		  (lambda ()
		    (global-defined-value 'mred@ mred@)
		    ,@(let loop ([l (append mred-exports propagate)][n 0])
			(if (null? l)
			    null
			    (cons `(global-defined-value ',(car l) (vector-ref ex ,n))
				  (loop (cdr l) (add1 n)))))))))])
      (if debug?
	  (pretty-print `(eval ',c))
	  (pretty-print c)))
    (display "> fstop unit <")
    (newline))
  'replace)

(when (directory-exists? "../../../collects/mred")
  (with-output-to-file "../../../collects/mred/sig.ss"
    (lambda () (pretty-print `(define-signature mred^ ,sig)))
    'truncate))
