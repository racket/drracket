; $Id: scm-ou.ss,v 1.17 1999/02/02 19:33:15 mflatt Exp $

(unit/sig zodiac:scheme-objects+units^
  (import zodiac:misc^ (z : zodiac:structures^) (z : zodiac:reader-structs^)
    zodiac:sexp^ (pat : zodiac:pattern^) 
    zodiac:expander^ zodiac:interface^
    zodiac:scheme-core^ zodiac:scheme-main^
    zodiac:scheme-objects^ zodiac:scheme-units^)

  (let ((handler
	  (let ((top-level-resolution (make-top-level-resolution 'dummy #f)))
	    (lambda (expr env attributes vocab)
	      (let loop ((r (resolve expr env vocab)))
		(cond
		  ((lexical-binding? r)
		    (create-lexical-varref r expr))
		  ((top-level-resolution? r)
		   (check-for-signature-name expr attributes)
		   (process-unit-top-level-resolution expr attributes))
		  ((public-binding? r)
		    (create-public-varref r expr))
		  ((override-binding? r)
		    (create-override-varref r expr))
		  ((private-binding? r)
		    (create-private-varref r expr))
		  ((inherit-binding? r)
		    (create-inherit-varref r expr))
		  ((rename-binding? r)
		    (create-rename-varref r expr))
		  ((supervar-binding? r)
		    (create-supervar-varref r expr))
		  ((superinit-binding? r)
		    (create-superinit-varref r expr))
		  ((or (macro-resolution? r) (micro-resolution? r))
		    (if (and (inside-unit? attributes)
			     (check-export expr attributes))
			(loop top-level-resolution)
			(static-error 
			 expr
			 "Invalid use of keyword ~a" (z:symbol-orig-name expr))))
		  (else
		    (internal-error expr "Invalid resolution in ou: ~s" r))))))))

    (add-sym-micro full-vocabulary handler)
    (add-sym-micro scheme-vocabulary handler)
    (add-sym-micro unit-clauses-vocab-delta handler))

  )
