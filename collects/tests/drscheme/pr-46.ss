;;; pr-46.ss

;;; tests register-will in the interactions window

(load-relative "drscheme-test-util.ss")

(let* ([drs-frame (wait-for-drscheme-frame)] 
       [interactions-edit (ivar drs-frame interactions-edit)]
       [execute-button (ivar drs-frame execute-button)]
       [get-int-pos (lambda () (get-text-pos interactions-edit))] 
       [check-execute ; type in term, hit execute
	(lambda (str expected)
	  (clear-definitions drs-frame)
	  (type-in-definitions drs-frame str)
	  (let ([answer-begin (+ (get-int-pos) 3)])
	    (push-button-and-wait execute-button)
	    (let ([answer-end (- (get-int-pos) 1)])
	      (let ([actual (send interactions-edit get-text
				  answer-begin answer-end)])
		(unless (string=? actual expected)
			(printf "Expected: ~a~n Actual: ~a~n~n"
				expected actual)))
	      (let ([frame (mred:test:get-active-frame)])
		(unless (eq? frame drs-frame)
			(error 'check-syntax "Unexpected window ~a" frame))))))]
       [terms-and-msgs
	'(("(register-will (list 1 2 3) display)" "")
	  ("(collect-garbage)" ""))])

  (for-each 
   (lambda (p) (check-execute (car p) (cadr p)))
   terms-and-msgs))


  