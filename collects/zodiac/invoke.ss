; $Id: invoke.ss,v 1.42 2000/05/28 03:47:30 shriram Exp $

(begin-elaboration-time
 (require-library "cores.ss"))

(require-library "coreu.ss")

(require-library "load.ss" "zodiac")

(define zodiac:default-interface@
  (unit/sig zodiac:interface^
    (import)
    (define internal-error
	(lambda (where fmt-spec . args)
	  (printf "Error at: ~s~n" where)
	  (apply error 'internal-error fmt-spec args)))
    (define (static-error link-text link-tag where fmt-spec . args)
      (printf "Error tag: ~s~n" link-tag)
      (printf "Error at: ~s~n" where)
      (apply error 'static-error
	(string-append link-text ": " fmt-spec)
	args))))

(define zodiac:system@
  (require-library-unit/sig "link2.ss" "zodiac"))

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig ((open zodiac:system^)
				  (open zodiac:interface^))
  (compound-unit/sig
   (import)
   (link
    (INTERFACE : zodiac:interface^
	       (zodiac:default-interface@))
    (SYSTEM : zodiac:system^
	    (zodiac:system@ INTERFACE
			    (MZLIB-CORE pretty-print)
			    (MZLIB-CORE file)))
    (MZLIB-CORE : mzlib:core^
		(mzlib:core@)))
   (export (open SYSTEM) (open INTERFACE)))
  zodiac)

(define (zodiac:make-see expander)
  (opt-lambda ((show-raw? #t))
    (parameterize ([current-prompt-read
		    (lambda ()
		      (newline)
		      (display "e> ")
		      (flush-output)
		      (let ([read ((zodiac:read))])
			(newline)
			(flush-output)
			(if (zodiac:eof? read)
			    eof
			    read)))]
		   [current-eval
		    (lambda (in)
		      (let ((e (car (expander in))))
			(if show-raw?
			    (zodiac:parsed->raw e)
			    e)))])
      (read-eval-print-loop))))

(define zodiac:see
  (zodiac:make-see 
    (lambda (in)
      (zodiac:scheme-expand-program (list in)))))

(define zodiac:see-parsed
  (lambda ()
    ((zodiac:make-see 
       (lambda (in)
	 (zodiac:scheme-expand-program (list in))))
      #f)))

(define zodiac:see
  (opt-lambda ((print-as-sexp? #t) (vocab zodiac:scheme-vocabulary))
    ((zodiac:make-see 
       (lambda (in)
	 (zodiac:scheme-expand-program
	   (list in)
	   (zodiac:make-attributes)
	   vocab)))
      print-as-sexp?)))

(define zodiac:spidey-see (zodiac:make-see 
			   (lambda (in)
			     (zodiac:scheme-expand-program
			      (list in)
			      (zodiac:make-attributes)
			      zodiac:mrspidey-vocabulary))))
