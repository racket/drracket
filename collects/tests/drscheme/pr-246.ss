;;; pr-246.ss

;;; make sure (cons 1 2) is an error in beginner-level Scheme

;;; pr-58.ss

;;; tests check-syntax when given bogus improper list
;;; tested at each language level

;;; Author: Paul Steckler

(load-relative "drscheme-test-util.ss")

(let* ([drs-frame (wait-for-drscheme-frame)] 
       [interactions-edit (ivar drs-frame interactions-edit)]
       [execute-button (ivar drs-frame execute-button)]
       [get-int-pos (lambda () (get-text-pos interactions-edit))]
       [check-execute ; type in term, call execute
	(lambda (str expected)
	  (clear-definitions drs-frame)
	  (push-button-and-wait execute-button) ; clears out any text in interactions-edit 
	  (type-in-definitions drs-frame str)
	  (let ([answer-begin (get-int-pos)])
	    (push-button-and-wait execute-button)
	    (let* ([answer-end (- (get-int-pos) 1)]
		   [actual (send interactions-edit get-text
				 answer-begin answer-end)])
	      (unless (string=? actual expected)
		      (printf "Expected: ~a~n Actual: ~a~n~n"
			      expected actual))
	      (let ([frame (mred:test:get-active-frame)])
		(unless (eq? frame drs-frame)
			(error 'check-syntax "Unexpected window ~a" frame))))))])

  (printf "Starting test~n") 

  (set-language-level! "Beginner" drs-frame)

  (check-execute "(cons 1 2)" 
		 "cons: second argument must be of type <list>, given 1 and 2")

  ; end pr-246

  (printf "Finished test~n"))
       


