;;; check-syntax.ss

;;; Author: Paul Steckler, modifying code by Robby Findler

(load-relative "drscheme-test-util.ss")

(let* ([drs-frame (wait-for-drscheme-frame)] 
       [interactions-edit (ivar drs-frame interactions-edit)]
       [get-int-pos (lambda () (get-text-pos interactions-edit))]
       [check-check-syntax ; type in term, call check-syntax
	(lambda (str expected)
	  (clear-definitions drs-frame)
	  (type-in-definitions drs-frame str)
	  (let ([answer-begin (get-int-pos)])
	    (mred:test:button-push (ivar drs-frame check-syntax-button))
	    (let ([answer-end (- (get-int-pos) 1)])
	      (let ([actual (send interactions-edit get-text
				  answer-begin answer-end)])
		(unless (string=? actual expected)
			(printf "Expected: ~a~n Actual: ~a~n~n"
				expected actual)))
	      (let ([frame (mred:test:get-active-frame)])
		(unless (eq? frame drs-frame)
			(error 'check-syntax "Unexpected window ~a" frame))))))]

      ; question: should we test for errors at different syntax levels?

      [terms-and-msgs ; terms and expected error message, if any

       ; why are some of these messages init-capped, others not?

       '(("x" "")
         ("." "can't use `.' outside list")
	 ("(" "missing close paren")
	 ("begin" "Invalid use of keyword begin")
         ("(begin)" "Malformed begin")
	 ("1" "")
	 ("add1" "")
	 ("(lambda (x) x)" ""))])

  (set-language-level! "R4RS+" drs-frame)

  (printf "Starting check-syntax tests~n") 

  (for-each 
   (lambda (p) (check-check-syntax (car p) (cadr p)))
   terms-and-msgs))

  (printf "Finished check-syntax tests~n") 


       