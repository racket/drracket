;;; repl-test.ss

(define (open-language-dialog)
  (let ([f (get-top-level-focus-window)])
    (fw:test:menu-select "Language" "Configure Language...")
    (wait-for-new-frame f)
    (with-handlers ([exn:user? (lambda (x) (void))])
      (fw:test:button-push "Show Details"))))

(define (set-language language close-dialog?)
  (open-language-dialog)
  (fw:test:set-choice! "Language" language)
  (when close-dialog?
    (let ([f (get-top-level-focus-window)])
      (fw:test:button-push "OK")
      (wait-for-new-frame f))))

(define (test-setting setting-name value expression result)
  (fw:test:set-check-box! setting-name value)
  (let ([f (get-top-level-focus-window)])
    (fw:test:button-push "OK")
    (wait-for-new-frame f))
  (let* ([drs (get-top-level-focus-window)]
	 [interactions (ivar drs interactions-edit)])
    (clear-definitions drs)
    (type-in-definitions drs expression)
    (do-execute drs)
    (let* ([got (fetch-output drs)])
      (unless (string=? result got)
	(printf "FAILED: ~a test~n expected: ~a~n     got: ~a~n" expression result got)))))

(define (mzscheme)
  (generic-settings "MzScheme")
  (set-language "MzScheme" #f)
  (test-setting "Unmatched cond/case is an error" #t "(cond [#f 1])" "cond or case: no matching clause"))

(define (zodiac language)
  (generic-settings language)
  (test-setting "Unmatched cond/case is an error" #t "(cond [#f 1])" "cond or case: no matching clause"))

(define (generic-settings language)
  (set-language language #f)
  (test-setting "Case sensitive" #t "(eq? 'a 'A)" "#f")
  (set-language language #f)
  (test-setting "Case sensitive" #f "(eq? 'a 'A)" "#t")
  (set-language language #f)
  (test-setting "Unmatched cond/case is an error" #f "(cond [#f 1])" "")
  (generic-output language))

(define (generic-output language)
  (let* ([drs (wait-for-drscheme-frame)]
	 [expression "(let ([x (box 3/2)]) (list x x))"]
	 [set-output-choice
	  (lambda (option show-sharing rationals)    
	    (set-language language #f)
	    (fw:test:set-choice! "Output Style" option)
	    (when show-sharing
	      (fw:test:set-check-box!
	       "Show sharing in values"
	       (if (eq? show-sharing 'on) #t #f)))
	    (when rationals
	      (fw:test:set-check-box!
	       "Print rationals in whole/part notation"
	       (if (eq? rationals 'on) #t #f)))
	    (let ([f (get-top-level-focus-window)])
	      (fw:test:button-push "OK")
	      (wait-for-new-frame f)))]
	 [test
	  (lambda (option show-sharing rationals answer)
	    (set-output-choice option show-sharing rationals)
	    (do-execute drs)
	    (let ([got (fetch-output drs)])
	      (unless (string=? answer got)
		(printf "FAILED ~a, sharing ~a, rationals ~a, got ~s expected ~s~n"
			option show-sharing rationals got answer))))])

    (clear-definitions drs)
    (type-in-definitions drs expression)

    (test "R4RS" 'off #f "(#&3/2 #&3/2)")    
    (test "R4RS" 'on #f "(#0=#&3/2 #0#)")
    (test "Quasiquote" 'off 'off "`(,(box 3/2) ,(box 3/2))")
    (test "Quasiquote" 'off 'on "`(,(box (+ 1 1/2)) ,(box (+ 1 1/2)))")
    (test "Quasiquote" 'on 'off "(shared ((-1- (box 3/2))) `(,-1- ,-1-))")
    (test "Quasiquote" 'on 'on "(shared ((-1- (box (+ 1 1/2)))) `(,-1- ,-1-))")
    (test "Constructor" 'off 'off "(list (box 3/2) (box 3/2))")
    (test "Constructor" 'off 'on "(list (box (+ 1 1/2)) (box (+ 1 1/2)))")
    (test "Constructor" 'on 'off "(shared ((-1- (box 3/2))) (list -1- -1-))")
    (test "Constructor" 'on 'on "(shared ((-1- (box (+ 1 1/2)))) (list -1- -1-))")))

(define (zodiac-generic language)
  (set-language language #f)
  (test-setting "Allow improper lists" #f "'(1 . 2)" "Improper lists not allowed")
  (open-language-dialog)
  (test-setting "Allow improper lists" #t "'(1 . 2)" "(cons 1 2)")
  (open-language-dialog)
  (test-setting "Allow improper lists" #f "(cons 1 2)" "Improper lists not allowed")
  (open-language-dialog)
  (test-setting "Allow improper lists" #t "(cons 1 2)" "(cons 1 2)")
  
  (open-language-dialog)
  (test-setting "Signal undefined variables when first referenced" #t "(local [(define x x)] 1)" "error")
  (open-language-dialog)
  (test-setting "Signal undefined variables when first referenced" #f "(local [(define x x)] 1)" "1")

  (open-language-dialog)
  (test-setting "Conditionals must evaluate to either #t or #f" #t "(not 1)" "error")
  (open-language-dialog)
  (test-setting "Conditionals must evaluate to either #t or #f" #f "(not 1)" "#t")
  (open-language-dialog)
  (test-setting "Conditionals must evaluate to either #t or #f" #t "(if 1 2 3)" "error")
  (open-language-dialog)
  (test-setting "Conditionals must evaluate to either #t or #f" #f "(if 1 2 3)" "#t")
  
  (open-language-dialog)
  (test-setting "Eq? only compares symbols" #f "(eq? 1 1)" "#t")
  (open-language-dialog)
  (test-setting "Eq? only compares symbols" #t "(eq? 1 1)" "eq?: expected symbols as arguments, received 1 and 1")
  (open-language-dialog)
  (test-setting "Unmatched cond/case is an error" #t "(cond [#f #f])" "no matching cond clause")
  (open-language-dialog)
  (test-setting "Unmatched cond/case is an error" #f "(cond [#f #f])" ""))

(define (zodiac-beginner)
  (zodiac-generic "Beginner"))

(define (zodiac-intermediate)
  (zodiac-generic "Intermediate"))
  
(define (zodiac-advanced)
  (zodiac-generic "Advanced")
  (open-language-dialog)
  (test-setting "Allow set! on undefined identifiers" #t "(set! x 123) x" "123")
  (open-language-dialog)
  (test-setting "Allow set! on undefined identifiers" #f "(set! x 123) x" "set!: cannot set undefined identifier: x"))

(zodiac "Beginner")
;(zodiac-beginner)
;(zodiac-intermediate)
;(zodiac-advanced)
;(mzscheme)


