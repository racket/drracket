;;; `test.scm' Test correctness of MzScheme implementations.
;;; Copyright (C) 1991, 1992, 1993, 1994 Aubrey Jaffer.
;;; Modified for MzScheme by Matthew

;;; MODIFIED for MzScheme - Matthew 8/95
;;;  Added a few more tests, like append!, reverse!, etc.
;;;  Added testing for inexact numbers
;;;  Added a lot of error testing
;;; modified for rational and complex numbers - Matthew 12/95
;;; modified to test exceptions and more of MzScheme - Matthew 4/96
;;; split into multiple files - Matthew 4/96
;;; extended, extended, extended

;;; This includes examples from
;;; William Clinger and Jonathan Rees, editors.
;;; Revised^4 Report on the Algorithmic Language Scheme
;;; and the IEEE specification.

; The format of the next line is important: file.ss relies on it
(define cur-section '())(define errs '())

(define teval eval)

(define SECTION (lambda args
		  (display "SECTION") (write args) (newline)
		  (set! cur-section args) #t))
(define record-error (lambda (e) (set! errs (cons (list cur-section e) errs))))

(print-struct #t)

(define number-of-tests 0)
(define number-of-error-tests 0)
(define number-of-exn-tests 0)

(define test
  (lambda (expect fun . args)
    (set! number-of-tests (add1 number-of-tests))
    (write (cons fun args))
    (display "  ==> ")
    (flush-output)
    ((lambda (res)
      (write res)
      (newline)
      (cond ((not (equal? expect res))
	     (record-error (list res expect (cons fun args)))
	     (display " BUT EXPECTED ")
	     (write expect)
	     (newline)
	     #f)
	    (else #t)))
     (if (procedure? fun) (apply fun args) (car args)))))

(define exn-table
  (list (cons exn? (cons exn-message string?))
	(cons exn:variable? (cons exn:variable-id symbol?))
	(cons exn:application:non-procedure? (cons exn:application-value 
						   (lambda (x) (not (procedure? x)))))
	(cons exn:application:arity? (cons exn:application-value integer?))
	(cons exn:application:arity? (cons exn:application:arity-expected
					   (lambda (a)
					     (or (integer? a)
						 (and (arity-at-least? a)
						      (integer? (arity-at-least-value a)))
						 (and (list? a)
						      (andmap
						       (lambda (a)
							 (or (integer? a)
							     (and (arity-at-least? a)
								  (integer? 
								   (arity-at-least-value a)))))
						       a))))))
	(cons exn:application:type? (cons exn:application:type-expected symbol?))
	(cons exn:application:range? (cons exn:application-value integer?))
	(cons exn:application:range:bounds? (cons exn:application:range:bounds-min integer?))
	(cons exn:application:range:bounds? (cons exn:application:range:bounds-max integer?))
	(cons exn:application:math:zero? (cons exn:application-value zero?))
	(cons exn:application:math:radix? (cons exn:application-value integer?))
	(cons exn:application:list-sizes? (cons exn:application-value list?))
	(cons exn:application:map-arity? (cons exn:application-value procedure?))
	(cons exn:application:map-arity? (cons exn:application:map-arity-provided 
					       (lambda (x) (and (integer? x) (positive? x)))))
	(cons exn:application:mode-conflict? 
	      (cons exn:application-value symbol?))
	(cons exn:application:mode-conflict? 
	      (cons exn:application:mode-conflict-filename
		    string?))
	(cons exn:application:file-position?
	      (cons exn:application-value (lambda (x) (or (input-port? x)
							  (output-port? x)))))
	(cons exn:application:fprintf:extra-arguments?
	      (cons exn:application-value string?))
	(cons exn:application:fprintf:extra-arguments?
	      (cons exn:application:fprintf:extra-arguments-extras list?))
	(cons exn:application:fprintf:no-argument?
	      (cons exn:application-value string?))
	(cons exn:application:fprintf:argument-type?
	      (cons exn:application:fprintf:argument-type-expected symbol?))

	(cons exn:struct:struct-type? 
	      (cons exn:struct:struct-type-value (lambda (x) (not (struct-type? x)))))

	(cons exn:read? (cons exn:read-port input-port?))
	(cons exn:read:number? (cons exn:read:number-input string?))
	(cons exn:read:char? (cons exn:read:char-input string?))
	(cons exn:read:eof? (cons exn:read:eof-expected string?))
	(cons exn:read:unsupported? (cons exn:read:unsupported-input string?))
	(cons exn:read:vector-length? (cons exn:read:vector-length-input string?))

	(cons exn:object:class-type? (cons exn:object:class-type-value
					   (lambda (x) (not (class? x)))))
	(cons exn:object:interface-type? (cons exn:object:interface-type-value
					       (lambda (x) (not (interface? x)))))
	(cons exn:object:generic? (cons exn:object:generic-object object?))
	(cons exn:object:inherit? (cons exn:object:inherit-ivar symbol?))
	(cons exn:object:implement? (cons exn:object:implement-ivar symbol?))
	(cons exn:object:class-ivar? (cons exn:object:class-ivar-class class?))
	(cons exn:object:class-ivar? (cons exn:object:class-ivar-ivar symbol?))
	(cons exn:object:interface-ivar? (cons exn:object:interface-ivar-interface interface?))
	(cons exn:object:interface-ivar? (cons exn:object:interface-ivar-ivar symbol?))
	(cons exn:object:ivar? (cons exn:object:ivar-object object?))
	(cons exn:object:ivar? (cons exn:object:ivar-ivar symbol?))
	(cons exn:object:private-class? (cons exn:object:private-class-class class?))
	(cons exn:object:init? (cons exn:object:init-object object?))
	(cons exn:object:init? (cons exn:object:init-class class?))

	(cons exn:unit:non-unit? (cons exn:unit:non-unit-value (lambda (x) (not (unit? x)))))
	(cons exn:unit:arity? (cons exn:unit:arity-unit unit?))
	(cons exn:unit:import? (cons exn:unit:import-in-unit unit?))
	(cons exn:unit:import? (cons exn:unit:import-out-unit unit?))
	(cons exn:unit:import? (cons exn:unit:import-in-tag symbol?))
	(cons exn:unit:import? (cons exn:unit:import-out-tag symbol?))
	(cons exn:unit:import? (cons exn:unit:import-name symbol?))
	(cons exn:unit:export? (cons exn:unit:export-unit unit?))
	(cons exn:unit:export? (cons exn:unit:export-tag symbol?))
	(cons exn:unit:export? (cons exn:unit:export-name symbol?))
	(cons exn:unit:invoke:variable? (cons exn:unit:invoke:variable-name symbol?))
	(cons exn:unit:signature:non-signed-unit? 
	      (cons exn:unit:signature:non-signed-unit-value (lambda (x) (not (unit/sig? x)))))
	(cons exn:unit:signature:arity?
	      (cons exn:unit:signature:arity-unit unit/sig?))
	(cons exn:unit:signature:match? (cons exn:unit:signature:match-dest-context string?))
	(cons exn:unit:signature:match? (cons exn:unit:signature:match-src-context string?))
	(cons exn:unit:signature:match? (cons exn:unit:signature:match-variable string?))

	(cons exn:i/o:read? (cons exn:i/o:read-port input-port?))
	(cons exn:i/o:write? (cons exn:i/o:write-port output-port?))
	(cons exn:i/o:filesystem? (cons exn:i/o:filesystem-pathname string?))
	(cons exn:i/o:port-closed? (cons exn:i/o:port-closed-port 
				    (lambda (x) (or (input-port? x) (output-port? x)))))
	(cons exn:i/o:user-port? (cons exn:i/o:user-port-port input-port?))

	(cons exn:i/o:tcp:connect? (cons exn:i/o:tcp:connect-address string?))
	(cons exn:i/o:tcp:connect? (cons exn:i/o:tcp:connect-port-id integer?))
	(cons exn:i/o:tcp:listen? (cons exn:i/o:tcp:listen-port-id integer?))
	(cons exn:i/o:tcp:accept? (cons exn:i/o:tcp:accept-listener tcp-listener?))
	(cons exn:i/o:tcp:listener-closed? (cons exn:i/o:tcp:listener-closed-listener tcp-listener?))
	
	(cons exn:misc:constant? (cons exn:misc:constant-id symbol?))))

(define thunk-error-test
  (case-lambda 
   [(th expr) (thunk-error-test th expr exn:application:type?)]
   [(th expr exn?)
    (set! number-of-error-tests (add1 number-of-error-tests))
    (write expr)
    (display "  =e=> ")
    (call/ec (lambda (escape)
	       (let* ([old-esc-handler (error-escape-handler)]
		      [old-handler (current-exception-handler)]
		      [orig-err-port (current-error-port)]
		      [test-handler
		       (lambda ()
			 (escape #t))]
		      [test-exn-handler
		       (lambda (e)
			 (when (and exn? (not (exn? e)))
			       (printf " WRONG EXN TYPE: ~s " e)
			       (record-error (list e 'exn-type expr)))

			 (for-each
			  (lambda (row)
			    (let ([pred? (car row)])
			      (when (pred? e)
				    (set! number-of-exn-tests 
					  (add1 number-of-exn-tests))
				    (let ([sel (cadr row)]
					  [pred? (cddr row)])
				      (unless (pred? (sel e))
					      (printf " WRONG EXN ELEM: ~s " e)
					      (record-error (list e 'exn-elem expr)))))))
			  exn-table)
						 
			 (old-handler e))])
		 (dynamic-wind
		  (lambda () 
		    (current-error-port (current-output-port))
		    (current-exception-handler test-exn-handler)
		    (error-escape-handler test-handler))
		  (lambda ()
		    (let ([v (th)])
		      (write v)
		      (display " BUT EXPECTED ERROR")
		      (record-error (list v 'Error expr))
		      (newline)
		      #f))
		  (lambda () 
		    (current-error-port orig-err-port)
		    (current-exception-handler old-handler)
		    (error-escape-handler old-esc-handler))))))]))

(if (not (defined? 'error-test))
    (define error-test
      (case-lambda 
       [(expr) (error-test expr exn:application:type?)]
       [(expr exn?)
	(thunk-error-test (lambda () (eval expr)) expr exn?)])))

(define (syntax-test expr)
  (error-test expr exn:syntax?)
  (error-test `(if #f ,expr) exn:syntax?))

(define (arity-test f min max)
  (letrec ([aok?
	    (lambda (a)
	      (cond
	       [(integer? a) (= a min max)]
	       [(arity-at-least? a) (and (negative? max)
					 (= (arity-at-least-value a) min))]
	       [(and (list? a) (andmap integer? a))
		(and (= min (car a)) (= max
					(let loop ([l a])
					  (if (null? (cdr l))
					      (car l)
					      (loop (cdr l))))))]
	       [(list? a)
		; Just check that all are consistent for now.
		; This should be improved.
		(andmap
		 (lambda (a)
		   (if (number? a)
		       (<= min a (if (negative? max) a max))
		       (>= (arity-at-least-value a) min)))
		 a)]
	       [else #f]))]
	   [make-ok?
	    (lambda (v)
	      (lambda (e)
		(and (exn:application:arity? e)
		     (= (exn:application-value e) v)
		     (aok? (exn:application:arity-expected e)))))]
	   [do-test
	    (lambda (f args check?)
	      (set! number-of-error-tests (add1 number-of-error-tests))
	      (printf "(apply ~s '~s)  =e=> " f args)
	      (let/ec done
		      (let ([v (with-handlers ([void
						(lambda (exn)
						  (if (check? exn)
						      (printf " ~a~n" (exn-message exn))
						      (let ([ok-type? (exn:application:arity? exn)])
							(printf " WRONG EXN ~a: ~s~n" 
								(if ok-type?
								    "FIELD"
								    "TYPE")
								exn)
							(record-error (list exn 
									    (if ok-type?
										'exn-field
										'exn-type)
									    (cons f args)))))
						  (done (void)))])
					      (apply f args))])
			(printf "~s~n BUT EXPECTED ERROR~n" v)
			(record-error (list v 'Error (cons f args))))))])
    (let loop ([n 0][l '()])
      (unless (>= n min)
	      (do-test f l (make-ok? n))
	      (loop (add1 n) (cons 1 l))))
    (let loop ([n min])
      (test #t procedure-arity-includes? f n)
      (unless (>= n max)
	  (loop (add1 n))))
    (if (>= max 0)
	(do-test f (let loop ([n 0][l '(1)])
		     (if (= n max)
			 l
			 (loop (add1 n) (cons 1 l))))
		 (make-ok? (add1 max)))
	(test #t procedure-arity-includes? f (arithmetic-shift 1 100)))))

(define (test-values l thunk)
  (test l call-with-values thunk list))

(define (report-errs)
  (printf "~nPerformed ~a expression tests (~a good expressions, ~a bad expressions)~n"
	  (+ number-of-tests number-of-error-tests)
	  number-of-tests 
	  number-of-error-tests)
  (printf "and ~a exception field tests.~n~n" 
	  number-of-exn-tests)
  (if (null? errs) 
      (display "Passed all tests.")
      (begin
	(display "Errors were:")
	(newline)
	(display "(SECTION (got expected (call)))")
	(newline)
	(for-each (lambda (l) (write l) (newline))
		  errs)))
  (newline)
  (display "(Other messages report successful tests of error-handling behavior.)")
  (newline))

(define type? exn:application:type?)
(define arity? exn:application:arity?)
(define syntaxe? exn:syntax?)

(define non-z void)
