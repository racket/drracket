(define language (make-parameter "<<not a language>>"))

(define (set-language close-dialog?)
  (set-language-level! (language) close-dialog?)
  (unless close-dialog?
    (with-handlers ([exn:user? (lambda (x) (void))])
      (fw:test:button-push "Show Details"))))

(define (test-setting setting-name value expression result)
  (fw:test:set-check-box! setting-name value)
  (let ([f (get-top-level-focus-window)])
    (fw:test:button-push "OK")
    (wait-for-new-frame f))
  (let* ([drs (get-top-level-focus-window)]
	 [interactions (ivar drs interactions-text)])
    (clear-definitions drs)
    (type-in-definitions drs expression)
    (do-execute drs)
    (let* ([got (fetch-output drs)])
      (unless (string=? result got)
	(printf "FAILED: ~a ~a test~n expected: ~a~n     got: ~a~n" (language) expression result got)))
    '(dump-memory-stats)))

(define (mred)
  (parameterize ([language "Graphical without Debugging (MrEd)"])
    (generic-settings #f)
    (generic-output #t #t)
    (set-language #f)
    (test-setting "Unmatched cond/case is an error" #t "(cond [#f 1])" "cond or case: no matching clause")
    
    (let ([drs (wait-for-drscheme-frame)])
      (clear-definitions drs)
      (set-language #t)
      (do-execute drs))
    
    (test-expression "true" "reference to undefined identifier: true")
    (test-expression "mred^" "compile: illegal use of an expansion-time value name in: mred^")
    (test-expression "(eq? 'a 'A)" "#t")
    (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
    (test-expression "(cond [(= 1 2) 3])" "")
    (test-expression "(cons 1 2)" "(1 . 2)")
    (test-expression "'(1)" "(1)")
    (test-expression "(define shrd (box 1)) (list shrd shrd)"
		     "(#&1 #&1)")
    (test-expression "(local ((define x x)) 1)" "define-values: illegal use (not at top-level) in: (#%define-values (x) x)")
    (test-expression "(if 1 1 1)" "1")
    (test-expression "(+ 1)" "1")
    (test-expression "1.0" "1.0")
    (test-expression "#i1.0" "1.0")
    (test-expression "3/2" "3/2")
    (test-expression "(list 1)" "(1)")
    (test-expression "argv" "#0()")))

(define (mzscheme)
  (parameterize ([language "Textual without Debugging (MzScheme)"])
    (generic-settings #f)
    (generic-output #t #t)
    (set-language #f)
    (test-setting "Unmatched cond/case is an error" #t "(cond [#f 1])" "cond or case: no matching clause")
    
    (let ([drs (wait-for-drscheme-frame)])
      (clear-definitions drs)
      (set-language #t)
      (do-execute drs))
    
    (test-expression "true" "reference to undefined identifier: true")
    (test-expression "mred^" "reference to undefined identifier: mred^")
    (test-expression "(eq? 'a 'A)" "#t")
    (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
    (test-expression "(cond [(= 1 2) 3])" "")
    (test-expression "(cons 1 2)" "(1 . 2)")
    (test-expression "'(1)" "(1)")
    (test-expression "(define shrd (box 1)) (list shrd shrd)"
		     "(#&1 #&1)")
    (test-expression "(local ((define x x)) 1)" "define-values: illegal use (not at top-level) in: (#%define-values (x) x)")
    (test-expression "(if 1 1 1)" "1")
    (test-expression "(+ 1)" "1")
    (test-expression "1.0" "1.0")
    (test-expression "#i1.0" "1.0")
    (test-expression "3/2" "3/2")
    (test-expression "(list 1)" "(1)")
    (test-expression "argv" "#0()")))

(define (mred-debug)
  (parameterize ([language "Graphical (MrEd)"])
    (generic-settings #f)
    (generic-output #t #t)
    (set-language #f)
    (test-setting "Unmatched cond/case is an error" #t "(cond [#f 1])" "no matching cond clause")
    (set-language #f)
    (test-setting "Signal undefined variables when first referenced" #t "(letrec ([x x]) 1)"
		  "Variable x referenced before definition or initialization")
    (set-language #f)
    (test-setting "Signal undefined variables when first referenced" #f "(letrec ([x x]) 1)" "1")
    
    (let ([drs (wait-for-drscheme-frame)])
      (clear-definitions drs)
      (set-language #t)
      (do-execute drs))
    
    (test-expression "true" "reference to undefined identifier: true")
    (test-expression "mred^" "Invalid use of signature name mred^")
    (test-expression "(eq? 'a 'A)" "#t")
    (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
    (test-expression "(cond [(= 1 2) 3])" "")
    (test-expression "(cons 1 2)" "(1 . 2)")
    (test-expression "'(1)" "(1)")
    (test-expression "(define shrd (box 1)) (list shrd shrd)"
		     "(#&1 #&1)")
    (test-expression "(local ((define x x)) 1)" "Invalid position for internal definition")
    (test-expression "(letrec ([x x]) 1)" "1")
    (test-expression "(if 1 1 1)" "1")
    (test-expression "(+ 1)" "1")
    (test-expression "1.0" "1.0")
    (test-expression "#i1.0" "1.0")
    (test-expression "3/2" "3/2")
    (test-expression "(list 1)" "(1)")
    (test-expression "argv" "#0()")))

(define (mzscheme-debug)
  (parameterize ([language "Textual (MzScheme)"])
    (generic-settings #f)
    (generic-output #t #t)
    (set-language #f)
    (test-setting "Unmatched cond/case is an error" #t "(cond [#f 1])" "no matching cond clause")
    (set-language #f)
    (test-setting "Signal undefined variables when first referenced" #t "(letrec ([x x]) 1)"
		  "Variable x referenced before definition or initialization")
    (set-language #f)
    (test-setting "Signal undefined variables when first referenced" #f "(letrec ([x x]) 1)" "1")
    
    (let ([drs (wait-for-drscheme-frame)])
      (clear-definitions drs)
      (set-language #t)
      (do-execute drs))
    
    (test-expression "true" "reference to undefined identifier: true")
    (test-expression "mred^" "reference to undefined identifier: mred^")
    (test-expression "(eq? 'a 'A)" "#t")
    (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
    (test-expression "(cond [(= 1 2) 3])" "")
    (test-expression "(cons 1 2)" "(1 . 2)")
    (test-expression "'(1)" "(1)")
    (test-expression "(define shrd (box 1)) (list shrd shrd)"
		     "(#&1 #&1)")
    (test-expression "(local ((define x x)) 1)" "Invalid position for internal definition")
    (test-expression "(letrec ([x x]) 1)" "1")
    (test-expression "(if 1 1 1)" "1")
    (test-expression "(+ 1)" "1")
    (test-expression "1.0" "1.0")
    (test-expression "#i1.0" "1.0")
    (test-expression "3/2" "3/2")
    (test-expression "(list 1)" "(1)")
    (test-expression "argv" "#0()")))

(define (zodiac-beginner)
  (parameterize ([language "Beginning Student"])
    (zodiac)
    (generic-output #f #f)
    
    (let ([drs (wait-for-drscheme-frame)])
      (clear-definitions drs)
      (set-language #t)
      (do-execute drs))
    
    (test-expression "true" "true")
    (test-expression "mred^" "reference to undefined identifier: mred^")
    (test-expression "(eq? 'a 'A)" "false")
    (test-expression "(set! x 1)" "reference to undefined identifier: set!")
    (test-expression "(cond [(= 1 2) 3])" "no matching cond clause")
    (test-expression "(cons 1 2)" "cons: second argument must be of type <list>, given 1 and 2")
    (test-expression "'(1)" "Misuse of quote: '(1) is not a symbol")
    (test-expression "(define shrd (box 1)) (list shrd shrd)"
		     "(cons (box 1) (cons (box 1) empty))")
    (test-expression "(local ((define x x)) 1)" "Invalid definition: must be at the top level")
    (test-expression "(letrec ([x x]) 1)" "First term after parenthesis is illegal in an application")
    (test-expression "(if 1 1 1)" "Condition value is neither true nor false: 1")
    (test-expression "(+ 1)" "+: expects at least 2 arguments, given 1: 1")
    (test-expression "1.0" "#i1.0")
    (test-expression "#i1.0" "#i1.0")
    (test-expression "3/2" "3/2")
    (test-expression "(list 1)" "(cons 1 empty)")
    (test-expression "argv" "reference to undefined identifier: argv")))

(define (zodiac-intermediate)
  (parameterize ([language "Intermediate Student"])
    (zodiac)
    (generic-output #t #f)
    (set-language #f)
    (test-setting "Signal undefined variables when first referenced" #t "(local ((define x x)) 1)"
		  "Variable x referenced before definition or initialization")
    (set-language #f)
    (test-setting "Signal undefined variables when first referenced" #f "(local ((define x x)) 1)" "1")
    
    (let ([drs (wait-for-drscheme-frame)])
      (clear-definitions drs)
      (set-language #t)
      (do-execute drs))
    
    (test-expression "true" "true")
    (test-expression "mred^" "reference to undefined identifier: mred^")
    (test-expression "(eq? 'a 'A)" "false")
    (test-expression "(set! x 1)" "reference to undefined identifier: set!")
    (test-expression "(cond [(= 1 2) 3])" "no matching cond clause")
    (test-expression "(cons 1 2)" "cons: second argument must be of type <list>, given 1 and 2")
    (test-expression "'(1)" "(list 1)")
    (test-expression "(define shrd (box 1)) (list shrd shrd)"
		     "(list (box 1) (box 1))")
    (test-expression "(local ((define x x)) 1)" "Variable x referenced before definition or initialization")
    (test-expression "(letrec ([x x]) 1)" "Variable x referenced before definition or initialization")
    (test-expression "(if 1 1 1)" "Condition value is neither true nor false: 1")
    (test-expression "(+ 1)" "+: expects at least 2 arguments, given 1: 1")
    (test-expression "1.0" "#i1.0")
    (test-expression "#i1.0" "#i1.0")
    (test-expression "3/2" "3/2")
    (test-expression "(list 1)" "(list 1)")
    (test-expression "argv" "reference to undefined identifier: argv")))

(define (zodiac-advanced)
  (parameterize ([language "Advanced Student"])
    (zodiac)
    (generic-output #t #t)
    (set-language #f)
    (test-setting "Signal undefined variables when first referenced" #t "(local ((define x x)) 1)" 
		  "Variable x referenced before definition or initialization")
    (set-language #f)
    (test-setting "Signal undefined variables when first referenced" #f "(local ((define x x)) 1)" "1")
    
    (let ([drs (wait-for-drscheme-frame)])
      (clear-definitions drs)
      (set-language #t)
      (do-execute drs))
    
    (test-expression "true" "true")
    (test-expression "mred^" "reference to undefined identifier: mred^")
    (test-expression "(eq? 'a 'A)" "false")
    (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
    (test-expression "(cond [(= 1 2) 3])" "no matching cond clause")
    (test-expression "(cons 1 2)" "cons: second argument must be of type <list>, given 1 and 2")
    (test-expression "'(1)" "(list 1)")
    (test-expression "(define shrd (box 1)) (list shrd shrd)"
		     "(shared ((-1- (box 1))) (list -1- -1-))")
    (test-expression "(local ((define x x)) 1)" "Variable x referenced before definition or initialization")
    (test-expression "(letrec ([x x]) 1)" "Variable x referenced before definition or initialization")
    (test-expression "(if 1 1 1)" "1")
    (test-expression "(+ 1)" "+: expects at least 2 arguments, given 1: 1")
    (test-expression "1.0" "#i1.0")
    (test-expression "#i1.0" "#i1.0")
    (test-expression "3/2" "3/2")
    (test-expression "(list 1)" "(list 1)")
    (test-expression "argv" "reference to undefined identifier: argv")))

(define (zodiac)
  (generic-settings #t)
  
  (set-language #f)
  (test-setting "Print booleans as true and false" #t "#t #f" (format "true~nfalse"))
  (set-language #f)
  (test-setting "Print booleans as true and false" #f "#t #f" (format "#t~n#f"))
  
  (set-language #f)
  (test-setting "Unmatched cond/case is an error" #t "(cond [false 1])" "no matching cond clause"))

(define (generic-settings false/true?)
  (set-language #f)
  (test-setting "Case sensitive" #t "(eq? 'a 'A)" (if false/true? "false" "#f"))
  (set-language #f)
  (test-setting "Case sensitive" #f "(eq? 'a 'A)" (if false/true? "true" "#t"))
  (set-language #f)
  (test-setting "Unmatched cond/case is an error" #f
		(format "(cond [~a 1])" (if false/true? "false" "#f"))
		""))

(define (generic-output list? quasi-quote?)
  (let* ([drs (wait-for-drscheme-frame)]
	 [expression (format "(define x (box 3/2))~n(list x x)")]
	 [set-output-choice
	  (lambda (option show-sharing rationals)    
	    (set-language #f)
	    (fw:test:set-radio-box! "Output Style" option)
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
	      (unless (whitespace-string=? answer got)
		(printf "FAILED ~a ~a, sharing ~a, rationals ~a, got ~s expected ~s~n"
			(language) option show-sharing rationals got answer))))])
    
    (clear-definitions drs)
    (type-in-definitions drs expression)
    
    (test "write" 'off #f "(#&3/2 #&3/2)")    
    (test "write" 'on #f "(#0=#&3/2 #0#)")
    (when quasi-quote?
      (test "Quasiquote" 'off 'off "`(,(box 3/2) ,(box 3/2))")
      (test "Quasiquote" 'off 'on "`(,(box (+ 1 1/2)) ,(box (+ 1 1/2)))")
      (test "Quasiquote" 'on 'off "(shared ((-1- (box 3/2))) `(,-1- ,-1-))")
      (test "Quasiquote" 'on 'on "(shared ((-1- (box (+ 1 1/2)))) `(,-1- ,-1-))"))
    (test "Constructor" 'off 'off (if list?
				      "(list (box 3/2) (box 3/2))"
				      "(cons (box 3/2) (cons (box 3/2) empty))"))
    (test "Constructor" 'off 'on (if list?
				     "(list (box (+ 1 1/2)) (box (+ 1 1/2)))"
				     "(cons (box (+ 1 1/2)) (cons (box (+ 1 1/2)) empty))"))
    (test "Constructor" 'on 'off (if list? 
				     "(shared ((-1- (box 3/2))) (list -1- -1-))"
				     (format "(shared ((-1- (box 3/2))) (cons -1- (cons -1- empty)))")))
    (test "Constructor" 'on 'on (if list?
				    "(shared ((-1- (box (+ 1 1/2)))) (list -1- -1-))"
				    (format "(shared ((-1- (box (+ 1 1/2)))) (cons -1- (cons -1- empty)))")))))

(define (whitespace-string=? string1 string2)
  (let loop ([i 0]
	     [j 0]
	     [in-whitespace? #t])
    (cond
      [(= i (string-length string1)) (only-whitespace? string2 j)]
      [(= j (string-length string2)) (only-whitespace? string1 i)]
      [else (let ([c1 (string-ref string1 i)]
		  [c2 (string-ref string2 j)])
	      (cond
		[in-whitespace?
		 (cond
		   [(whitespace? c1)
		    (loop (+ i 1)
			  j
			  #t)]
		   [(whitespace? c2)
		    (loop i
			  (+ j 1)
			  #t)]
		   [else (loop i j #f)])]
		[(and (whitespace? c1)
		      (whitespace? c2))
		 (loop (+ i 1)
		       (+ j 1)
		       #t)]
		[(char=? c1 c2)
		 (loop (+ i 1)
		       (+ j 1)
		       #f)]
		[else #f]))])))

(define (whitespace? c)
  (or (char=? c #\newline)
      (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\return)))

(define (only-whitespace? str i)
  (let loop ([n i])
    (cond
      [(= n (string-length str))
       #t]
      [(whitespace? (string-ref str n))
       (loop (+ n 1))]
      [else #f])))

;; whitespace-string=? tests
'(map (lambda (x) (apply equal? x))
     (list (list #t (whitespace-string=? "a" "a"))
	   (list #f (whitespace-string=? "a" "A"))
	   (list #f (whitespace-string=? "a" " "))
	   (list #f (whitespace-string=? " " "A"))
	   (list #t (whitespace-string=? " " " "))
	   (list #t (whitespace-string=? " " "  "))
	   (list #t (whitespace-string=? "  " "  "))
	   (list #t (whitespace-string=? "  " " "))
	   (list #t (whitespace-string=? "a a" "a a"))
	   (list #t (whitespace-string=? "a a" "a  a"))
	   (list #t (whitespace-string=? "a  a" "a a"))
	   (list #t (whitespace-string=? " a" "a"))
	   (list #t (whitespace-string=? "a" " a"))
	   (list #t (whitespace-string=? "a " "a"))
	   (list #t (whitespace-string=? "a" "a "))))

(define (test-expression expression expected)
  (let* ([drs (wait-for-drscheme-frame)]
	 [interactions-text (ivar drs interactions-text)]
	 [last-para (send interactions-text last-paragraph)])
    (send interactions-text set-position
	  (send interactions-text last-position)
	  (send interactions-text last-position))
    (type-in-interactions drs expression)
    (type-in-interactions drs (string #\newline))
    (wait-for-computation drs)
    (let ([got
	   (fetch-output
	    drs
	    (send interactions-text paragraph-start-position (+ last-para 1))
	    (send interactions-text paragraph-end-position
		  (- (send interactions-text last-paragraph) 1)))])
      (unless (whitespace-string=? got expected)
	(printf "FAILED: ~a expected ~s to produce ~s, got ~s instead~n"
		(language) expression expected got)))))

(zodiac-beginner)
(zodiac-intermediate)
(zodiac-advanced)
(mzscheme-debug)
(mred-debug)
(mzscheme)
(mred)
