;;; pr-58.ss

;;; tests check-syntax when given bogus improper list
;;; tested at each language level

;;; Author: Paul Steckler

(load-relative "drscheme-test-util.ss")

(let* ([drs-frame (wait-for-drscheme-frame)] 
       [interactions-edit (ivar drs-frame interactions-edit)]
       [execute-button (ivar drs-frame execute-button)]
       [get-int-pos (lambda () (get-text-pos interactions-edit))]
       [check-check-syntax ; type in term, call check-syntax
	(lambda (str expected)
	  (clear-definitions drs-frame)
	  (type-in-definitions drs-frame str)
	  (push-button-and-wait execute-button)
	  (let ([answer-begin (+ (get-int-pos) 3)])
	    (mred:test:button-push (ivar drs-frame check-syntax-button))
	    (let* ([answer-end (- (get-int-pos) 1)]
		   [actual (send interactions-edit get-text
				 answer-begin answer-end)])
	      (unless (string=? actual expected)
		      (printf "Expected: ~a~n Actual: ~a~n~n"
			      expected actual))
	      (let ([frame (mred:test:get-active-frame)])
		(unless (eq? frame drs-frame)
			(error 'check-syntax "Unexpected window ~a" frame))))))])

  (printf "Starting tests~n") 

  (set-language-level! "Beginner" drs-frame)

  (check-check-syntax "'(a . b)" "improper lists are not allowed")

  ; from pr-246 
  ; execute says "cons: second argument must be of type <list>, given 1 and 2")

  (check-check-syntax "(cons 1 2)" "")

  ; end pr-246

  (set-language-level! "Intermediate" drs-frame)
  (check-check-syntax "'(a . b)" "improper lists are not allowed")

  (set-language-level! "Advanced" drs-frame)
  (check-check-syntax "'(a . b)" "improper lists are not allowed")

  (set-language-level! "R4RS+" drs-frame)
  (check-check-syntax "'(a . b)" "")

  (printf "Finished tests~n"))
       
