
(begin-elaboration-time 
 (require-library "invoke.ss"))

(begin-elaboration-time
 (define-values/invoke-unit (define-syntax
			      -:sr:tag
			      -:sr:untag
			      -:sr:flatten
			      -:sr:matches-pattern?
			      -:sr:get-bindings
			      -:sr:expand-pattern)
   (require-library "synruler.ss")))

(define-macro define-syntax define-syntax)

(keyword-name '-:sr:tag)
(keyword-name '-:sr:untag)
(keyword-name '-:sr:flatten)
(keyword-name '-:sr:matches-pattern?)
(keyword-name '-:sr:get-bindings)
(keyword-name '-:sr:expand-pattern)
