
(module language-test mzscheme
  (require "drscheme-test-util.ss"
           (lib "gui.ss" "tests" "utils")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix fw: (lib "framework.ss" "framework")))
  
  (provide run-test)

  (define language (make-parameter "<<not a language>>"))
  
  ;; set-language : boolean -> void
  (define (set-language close-dialog?)
    (set-language-level! (language) close-dialog?))
  
  (define (raw-mred)
    (parameterize ([language (list "Full" "Graphical without debugging (MrEd)")])

      (check-top-of-repl)

      (generic-settings #f)
      (generic-output #t #t #t)
      
;      (set-language #f)
;      (test-setting "Unmatched cond/case is an error"
;                    #t
;                    "(cond [#f 1])" "cond or case: no matching clause")
      
      (test-hash-bang)
      
      (let ([drs (wait-for-drscheme-frame)])
        (clear-definitions drs)
        (set-language #t)
        (do-execute drs))
      
      (test-expression "(sqrt -1)" "0+1i")

      (test-expression "class" "{image #f} class: bad syntax in #<struct:object:derived-from-rep-text%>:105: class")
      (test-expression "shared" "reference to undefined identifier: shared")
      
      (test-expression "(define (. x y) (* x y)) ." (regexp "read: illegal use of \"\\.\" in USERPORT:[0-9]*"))
      
      (test-expression "(define (f define) 1)" "")
      (test-expression "(define (f car) 1)" "")
      (test-expression "(define (f empty) 1)" "")
      
      (test-expression "call/cc" "#<primitive:call-with-current-continuation>")
      
      (test-expression "(error 'a \"~a\" 1)" "a: 1")
      (test-expression "(error \"a\" \"a\")" "a \"a\"")
      
      (test-expression "(time 1)" (format "{embedded \"cpu time: 0 real time: 0 gc time: 0\"}~n1"))
      
      (test-expression "(list make-posn posn-x posn-y posn?)"
                       "reference to undefined identifier: make-posn")
      (test-expression "set-posn-x!" "reference to undefined identifier: set-posn-x!")
      (test-expression "set-posn-y!" "reference to undefined identifier: set-posn-y!")
      
      (test-expression "true" "reference to undefined identifier: true")
      (test-expression "mred^" "reference to undefined identifier: mred^")
      (test-expression "(eq? 'a 'A)" "#t")
      (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
      (test-expression "(cond [(= 1 2) 3])" "")
      (test-expression "(cons 1 2)" "(1 . 2)")
      (test-expression "'(1)" "(1)")
      (test-expression "(define shrd (box 1)) (list shrd shrd)"
                       "(#&1 #&1)")
      (test-expression
       "(local ((define x x)) 1)"
       (regexp "{image #f} define-values: illegal use \\(not at top-level\\) in #<struct:object:derived-from-rep-text%>:[0-9]*: \\(define-values \\(x\\) x\\)"))
      (test-expression "(if 1 1 1)" "1")
      (test-expression "(+ 1)" "1")
      
      (test-expression "1.0" "1.0")
      (test-expression "#i1.0" "1.0")
      (test-expression "4/3" "{number 4/3 \"1 1/3\"}")
      (test-expression "1/3" "{number 1/3 \"1/3\"}")
      (test-expression "-4/3" "{number -4/3 \"-1 1/3\"}")
      (test-expression "-1/3" "{number -1/3 \"-1/3\"}")
      (test-expression "3/2" "{number 3/2 \"1 1/2\"}")
      (test-expression "1/2" "{number 1/2 \"1/2\"}")
      (test-expression "-1/2" "{number -1/2 \"-1/2\"}")
      (test-expression "-3/2" "{number -3/2 \"-1 1/2\"}")
      (test-expression "+1/3i" "0+1/3i")
      (test-expression "+1/2i" "0+1/2i")
      (test-expression "(exact? 1.5)" "#f")
      
      (test-expression "(list 1)" "(1)")
      (test-expression "argv" "#0()")))
  
  (define (raw-mzscheme)
    (parameterize ([language (list "Full" "Textual without debugging (MzScheme)")])

      (check-top-of-repl)

      (generic-settings #f)
      (generic-output #t #t #t)
;      (set-language #f)
;      (test-setting "Unmatched cond/case is an error" #t "(cond [#f 1])" "cond or case: no matching clause")
      
      (test-hash-bang)
      
      (let ([drs (wait-for-drscheme-frame)])
        (clear-definitions drs)
        (set-language #t)
        (do-execute drs))
      
      (test-expression "(sqrt -1)" "0+1i")

      (test-expression "class" "reference to undefined identifier: class")
      (test-expression "shared" "reference to undefined identifier: shared")
      
      (test-expression "(define (. x y) (* x y)) ." (regexp "read: illegal use of \"\\.\" in USERPORT:[0-9]*"))
      
      (test-expression "(define (f define) 1)" "")
      (test-expression "(define (f car) 1)" "")
      (test-expression "(define (f empty) 1)" "")
      
      (test-expression "call/cc" "#<primitive:call-with-current-continuation>")
      
      (test-expression "(error 'a \"~a\" 1)" "a: 1")
      (test-expression "(error \"a\" \"a\")" "a \"a\"")
      
      (test-expression "(time 1)" (format "{embedded \"cpu time: 0 real time: 0 gc time: 0\"}~n1"))
      
      (test-expression "(list make-posn posn-x posn-y posn?)" "reference to undefined identifier: make-posn")
      (test-expression "set-posn-x!" "reference to undefined identifier: set-posn-x!")
      (test-expression "set-posn-y!" "reference to undefined identifier: set-posn-y!")
      
      (test-expression "true" "reference to undefined identifier: true")
      (test-expression "mred^" "reference to undefined identifier: mred^")
      (test-expression "(eq? 'a 'A)" "#t")
      (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
      (test-expression "(cond [(= 1 2) 3])" "")
      (test-expression "(cons 1 2)" "(1 . 2)")
      (test-expression "'(1)" "(1)")
      (test-expression "(define shrd (box 1)) (list shrd shrd)"
                       "(#&1 #&1)")
      (test-expression
       "(local ((define x x)) 1)"
       (regexp "{image #f} define-values: illegal use \\(not at top-level\\) in #<struct:object:derived-from-rep-text%>:[0-9]*: \\(define-values \\(x\\) x\\)"))
      (test-expression "(if 1 1 1)" "1")
      (test-expression "(+ 1)" "1")
      
      (test-expression "1.0" "1.0")
      (test-expression "#i1.0" "1.0")
      (test-expression "4/3" "{number 4/3 \"1 1/3\"}")
      (test-expression "1/3" "{number 1/3 \"1/3\"}")
      (test-expression "-4/3" "{number -4/3 \"-1 1/3\"}")
      (test-expression "-1/3" "{number -1/3 \"-1/3\"}")
      (test-expression "3/2" "{number 3/2 \"1 1/2\"}")
      (test-expression "1/2" "{number 1/2 \"1/2\"}")
      (test-expression "-1/2" "{number -1/2 \"-1/2\"}")
      (test-expression "-3/2" "{number -3/2 \"-1 1/2\"}")
      (test-expression "+1/3i" "0+1/3i")
      (test-expression "+1/2i" "0+1/2i")
      (test-expression "(exact? 1.5)" "#f")
      
      (test-expression "(list 1)" "(1)")
      (test-expression "argv" "#0()")))
  
  (define (mred)
    (parameterize ([language (list "Full" "Graphical (MrEd)")])
      (check-top-of-repl)

      (generic-settings #f)
      (generic-output #t #t #t)
;      (set-language #f)
;      (test-setting "Unmatched cond/case is an error" #t
;                    "(cond [#f 1])"
;                    "{image #f} cond: all question results were false")
;      (set-language #f)
;      (test-setting "Signal undefined variables when first referenced" #t "(letrec ([x x]) 1)"
;                    "{image #f} local variable used before its definition: x")
;      (set-language #f)
;      (test-setting "Signal undefined variables when first referenced" #f "(letrec ([x x]) 1)" "1")
      
      (test-hash-bang)
      
      (let ([drs (wait-for-drscheme-frame)])
        (clear-definitions drs)
        (set-language #t)
        (do-execute drs))
      
      (test-expression "(sqrt -1)" "0+1i")

      (test-expression "class" "{image #f} class: bad syntax in #<struct:object:derived-from-rep-text%>:87: class")
      (test-expression "shared" "{image #f} reference to undefined identifier: shared")
      
      (test-expression "(define (. x y) (* x y)) ." (regexp "read: illegal use of \"\\.\" in USERPORT:[0-9]*"))
      
      (test-expression "(define (f define) 1)" "")
      (test-expression "(define (f car) 1)" "")
      (test-expression "(define (f empty) 1)" "")
      
      (test-expression "call/cc" "#<primitive:call-with-current-continuation>")
      
      (test-expression "(error 'a \"~a\" 1)" "{image #f} a: 1")
      (test-expression "(error \"a\" \"a\")" "{image #f} a \"a\"")
      
      (test-expression "(time 1)" (format "{embedded \"cpu time: 0 real time: 0 gc time: 0\"}~n1"))
      
      (test-expression "(list make-posn posn-x posn-y posn?)"
                       "{image #f} reference to undefined identifier: make-posn")
      (test-expression "set-posn-x!" "{image #f} reference to undefined identifier: set-posn-x!")
      (test-expression "set-posn-y!" "{image #f} reference to undefined identifier: set-posn-y!")
      
      (test-expression "true" "{image #f} reference to undefined identifier: true")
      (test-expression "mred^" "{image #f} reference to undefined identifier: mred^")
      (test-expression "(eq? 'a 'A)" "#t")
      (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
      (test-expression "(cond [(= 1 2) 3])" "")
      (test-expression "(cons 1 2)" "(1 . 2)")
      (test-expression "'(1)" "(1)")
      (test-expression "(define shrd (box 1)) (list shrd shrd)"
                       "(#&1 #&1)")
      (test-expression 
       "(local ((define x x)) 1)"
       (regexp "{image #f} define-values: illegal use \\(not at top-level\\) in #<struct:object:derived-from-rep-text%>:[0-9]*: \\(define-values \\(x\\) x\\)"))
      (test-expression "(letrec ([x x]) 1)" "1")
      (test-expression "(if 1 1 1)" "1")
      (test-expression "(+ 1)" "1")
      
      (test-expression "1.0" "1.0")
      (test-expression "#i1.0" "1.0")
      (test-expression "4/3" "{number 4/3 \"1 1/3\"}")
      (test-expression "1/3" "{number 1/3 \"1/3\"}")
      (test-expression "-4/3" "{number -4/3 \"-1 1/3\"}")
      (test-expression "-1/3" "{number -1/3 \"-1/3\"}")
      (test-expression "3/2" "{number 3/2 \"1 1/2\"}")
      (test-expression "1/2" "{number 1/2 \"1/2\"}")
      (test-expression "-1/2" "{number -1/2 \"-1/2\"}")
      (test-expression "-3/2" "{number -3/2 \"-1 1/2\"}")
      (test-expression "+1/3i" "0+1/3i")
      (test-expression "+1/2i" "0+1/2i")
      (test-expression "(exact? 1.5)" "#f")
      
      (test-expression "(list 1)" "(1)")
      (test-expression "argv" "#0()")))
  
  (define (mzscheme)
    (parameterize ([language (list "Full" "Textual (MzScheme)")])

      (check-top-of-repl)

      (generic-settings #f)
      (generic-output #t #t #t)
      
      (test-hash-bang)
      
      (let ([drs (wait-for-drscheme-frame)])
        (clear-definitions drs)
        (set-language #t)
        (do-execute drs))
      
      (test-expression "(sqrt -1)" "0+1i")

      (test-expression "class" "{image #f} reference to undefined identifier: class")
      (test-expression "shared" "{image #f} reference to undefined identifier: shared")
      
      (test-expression "(define (. x y) (* x y)) ." (regexp "read: illegal use of \"\\.\" in USERPORT:[0-9]*"))
      
      (test-expression "(define (f define) 1)" "")
      (test-expression "(define (f car) 1)" "")
      (test-expression "(define (f empty) 1)" "")
      
      (test-expression "call/cc" "#<primitive:call-with-current-continuation>")
      
      (test-expression "(error 'a \"~a\" 1)" "{image #f} a: 1")
      (test-expression "(error \"a\" \"a\")" "{image #f} a \"a\"")
      
      (test-expression "(time 1)" (format "{embedded \"cpu time: 0 real time: 0 gc time: 0\"}~n1"))
      
      (test-expression "(list make-posn posn-x posn-y posn?)"
                       "{image #f} reference to undefined identifier: make-posn")
      (test-expression "set-posn-x!" "{image #f} reference to undefined identifier: set-posn-x!")
      (test-expression "set-posn-y!" "{image #f} reference to undefined identifier: set-posn-y!")
      
      (test-expression "true" "{image #f} reference to undefined identifier: true")
      (test-expression "mred^" "{image #f} reference to undefined identifier: mred^")
      (test-expression "(eq? 'a 'A)" "#t")
      (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
      (test-expression "(cond [(= 1 2) 3])" "")
      (test-expression "(cons 1 2)" "(1 . 2)")
      (test-expression "'(1)" "(1)")
      (test-expression "(define shrd (box 1)) (list shrd shrd)"
                       "(#&1 #&1)")
      (test-expression 
       "(local ((define x x)) 1)"
       (regexp "{image #f} define-values: illegal use \\(not at top-level\\) in #<struct:object:derived-from-rep-text%>:[0-9]*: \\(define-values \\(x\\) x\\)"))
      (test-expression "(letrec ([x x]) 1)" "1")
      (test-expression "(if 1 1 1)" "1")
      (test-expression "(+ 1)" "1")

      (test-expression "1.0" "1.0")
      (test-expression "#i1.0" "1.0")
      (test-expression "4/3" "{number 4/3 \"1 1/3\"}")
      (test-expression "1/3" "{number 1/3 \"1/3\"}")
      (test-expression "-4/3" "{number -4/3 \"-1 1/3\"}")
      (test-expression "-1/3" "{number -1/3 \"-1/3\"}")
      (test-expression "3/2" "{number 3/2 \"1 1/2\"}")
      (test-expression "1/2" "{number 1/2 \"1/2\"}")
      (test-expression "-1/2" "{number -1/2 \"-1/2\"}")
      (test-expression "-3/2" "{number -3/2 \"-1 1/2\"}")
      (test-expression "+1/3i" "0+1/3i")
      (test-expression "+1/2i" "0+1/2i")
      (test-expression "(exact? 1.5)" "#f")

      (test-expression "(list 1)" "(1)")
      (test-expression "argv" "#0()")))
  
  (define (beginner)
    (parameterize ([language (list "How to Design Programs" "Beginning Student")])
      (check-top-of-repl)
  
      (generic-settings #t)
      (generic-output #f #f #f)
      
      (test-hash-bang)
      
      (let ([drs (wait-for-drscheme-frame)])
        (clear-definitions drs)
        (set-language #t)
        (do-execute drs))
      
      (test-expression "(sqrt -1)" "0+1i")

      (test-expression "class" "reference to undefined identifier: class")
      (test-expression "shared" "reference to undefined identifier: shared")

      (test-expression "(define (. x y) (* x y)) ." ".")
      
      (test-expression "call/cc" "reference to undefined identifier: call/cc")
      
      (test-expression "(error 'a \"~a\" 1)"
                       "procedure error: expects 2 arguments, given 3: 'a \"~a\" 1")
      (test-expression "(error \"a\" \"a\")"
                       "error: expected a symbol and a string, got \"a\" and \"a\"")
      
      (test-expression "(time 1)" "reference to undefined identifier: time")
      
      (test-expression "(list make-posn posn-x posn-y posn?)"
                       "(cons make-posn (cons posn-x (cons posn-y (cons posn? empty))))")
      (test-expression "set-posn-x!" "reference to undefined identifier: set-posn-x!")
      (test-expression "set-posn-y!" "reference to undefined identifier: set-posn-y!")
      
      (test-expression "true" "true")
      (test-expression "mred^" "reference to undefined identifier: mred^")
      (test-expression "(eq? 'a 'A)" "false")
      (test-expression "(set! x 1)" "reference to undefined identifier: set!")
      (test-expression "(cond [(= 1 2) 3])" "cond: all question results were false")
      (test-expression "(cons 1 2)" "cons: second argument must be of type <list>, given 1 and 2")
      (test-expression "'(1)" "quote: expected a name after a ', found something else")
      (test-expression "(define shrd (list 1)) (list shrd shrd)"
                       "(cons (cons 1 empty) (cons (cons 1 empty) empty))")
      (test-expression "(local ((define x x)) 1)"
                       "function call: expected a defined name or a primitive operation name after an open parenthesis, but found something else")
      (test-expression "(letrec ([x x]) 1)"
                       "function call: expected a defined name or a primitive operation name after an open parenthesis, but found something else")
      (test-expression "(if 1 1 1)" "if: question result is not true or false: 1")
      (test-expression "(+ 1)" "procedure +: expects at least 2 arguments, given 1: 1")
      
      (test-expression "1.0" "1")
      (test-expression "#i1.0" "#i1.0")
      (test-expression "4/3" "{number 4/3 \"1 1/3\"}")
      (test-expression "1/3" "{number 1/3 \"1/3\"}")
      (test-expression "-4/3" "{number -4/3 \"-1 1/3\"}")
      (test-expression "-1/3" "{number -1/3 \"-1/3\"}")
      (test-expression "3/2" "1.5")
      (test-expression "1/2" "0.5")
      (test-expression "-1/2" "-0.5")
      (test-expression "-3/2" "-1.5")
      (test-expression "+1/3i" "0+1/3i")
      (test-expression "+1/2i" "0+0.5i")
      (test-expression "(exact? 1.5)" "true")
      
      (test-expression "(list 1)" "(cons 1 empty)")
      (test-expression "argv" "reference to undefined identifier: argv")))
  
  (define (intermediate)
    (parameterize ([language (list "How to Design Programs" "Intermediate Student")])
      (check-top-of-repl)

      (generic-settings #t)
      (generic-output #t #f #f)
;      (set-language #f)
;      (test-setting "Signal undefined variables when first referenced" #t "(local ((define x x)) 1)"
;                    "local variable used before its definition: x")
;      (set-language #f)
;      (test-setting "Signal undefined variables when first referenced" #f "(local ((define x x)) 1)" "1")
      
      (test-hash-bang)
      
      (let ([drs (wait-for-drscheme-frame)])
        (clear-definitions drs)
        (set-language #t)
        (do-execute drs))
      
      (test-expression "(sqrt -1)" "0+1i")

      (test-expression "class" "reference to undefined identifier: class")
      (test-expression "shared" "reference to undefined identifier: shared")
      
      (test-expression "(define (. x y) (* x y)) ." ".")
      
      (test-expression "call/cc" "reference to undefined identifier: call/cc")
      
      (test-expression "(error 'a \"~a\" 1)"
                       "procedure error: expects 2 arguments, given 3: 'a \"~a\" 1")
      (test-expression "(error \"a\" \"a\")"
                       "error: expected a symbol and a string, got \"a\" and \"a\"")
      
      (test-expression "(time 1)" (format "{embedded \"cpu time: 0 real time: 0 gc time: 0\"}~n1"))
      
      (test-expression "(list make-posn posn-x posn-y posn?)" "(list make-posn posn-x posn-y posn?)")
      (test-expression "set-posn-x!" "reference to undefined identifier: set-posn-x!")
      (test-expression "set-posn-y!" "reference to undefined identifier: set-posn-y!")
      
      (test-expression "true" "true")
      (test-expression "mred^" "reference to undefined identifier: mred^")
      (test-expression "(eq? 'a 'A)" "false")
      (test-expression "(set! x 1)" "reference to undefined identifier: set!")
      (test-expression "(cond [(= 1 2) 3])" "cond: all question results were false")
      (test-expression "(cons 1 2)" "cons: second argument must be of type <list>, given 1 and 2")
      (test-expression "'(1)" "(list 1)")
      (test-expression "(define shrd (list 1)) (list shrd shrd)"
                       "(list (list 1) (list 1))")
      (test-expression "(local ((define x x)) 1)" "local variable used before its definition: x")
      (test-expression "(letrec ([x x]) 1)" "local variable used before its definition: x")
      (test-expression "(if 1 1 1)" "if: question result is not true or false: 1")
      (test-expression "(+ 1)" "procedure +: expects at least 2 arguments, given 1: 1")
      
      (test-expression "1.0" "1")
      (test-expression "#i1.0" "#i1.0")
      (test-expression "4/3" "{number 4/3 \"1 1/3\"}")
      (test-expression "1/3" "{number 1/3 \"1/3\"}")
      (test-expression "-4/3" "{number -4/3 \"-1 1/3\"}")
      (test-expression "-1/3" "{number -1/3 \"-1/3\"}")
      (test-expression "3/2" "1.5")
      (test-expression "1/2" "0.5")
      (test-expression "-1/2" "-0.5")
      (test-expression "-3/2" "-1.5")
      (test-expression "+1/3i" "0+1/3i")
      (test-expression "+1/2i" "0+0.5i")
      
      (test-expression "(exact? 1.5)" "true")
      (test-expression "(list 1)" "(list 1)")
      (test-expression "argv" "reference to undefined identifier: argv")))
  
  (define (advanced)
    (parameterize ([language (list "How to Design Programs" "Advanced Student")])
      (check-top-of-repl)

      (generic-settings #t)
      (generic-output #t #t #t)
;      (set-language #f)
;      (test-setting "Signal undefined variables when first referenced" #t "(local ((define x x)) 1)" 
;                    "local variable used before its definition: x")
;      (set-language #f)
;      (test-setting "Signal undefined variables when first referenced" #f "(local ((define x x)) 1)" "1")
      
      (test-hash-bang)
      
      (let ([drs (wait-for-drscheme-frame)])
        (clear-definitions drs)
        (set-language #t)
        (do-execute drs))
      
      (test-expression "(sqrt -1)" "0+1i")

      (test-expression "class" "reference to undefined identifier: class")
      (test-expression "shared" "reference to undefined identifier: shared")
      
      (test-expression "(define (. x y) (* x y)) ." ".")
      
      (test-expression "call/cc" "call-with-current-continuation")
      
      (test-expression "(error 'a \"~a\" 1)"
                       "procedure error: expects 2 arguments, given 3: 'a \"~a\" 1")
      (test-expression "(error \"a\" \"a\")"
                       "error: expected a symbol and a string, got \"a\" and \"a\"")
      
      (test-expression "(time 1)" (format "{embedded \"cpu time: 0 real time: 0 gc time: 0\"}~n1"))
      
      (test-expression "(list make-posn posn-x posn-y posn?)" "(list make-posn posn-x posn-y posn?)")
      (test-expression "set-posn-x!" "set-posn-x!")
      (test-expression "set-posn-y!" "set-posn-y!")
      
      (test-expression "true" "true")
      (test-expression "mred^" "reference to undefined identifier: mred^")
      (test-expression "(eq? 'a 'A)" "false")
      (test-expression "(set! x 1)" "set!: cannot set undefined identifier: x")
      (test-expression "(cond [(= 1 2) 3])" "cond: all question results were false")
      (test-expression "(cons 1 2)" "cons: second argument must be of type <list or cyclic list>, given 1 and 2")
      (test-expression "'(1)" "(list 1)")
      (test-expression "(define shrd (list 1)) (list shrd shrd)"
                       "(shared ((-1- (list 1))) (list -1- -1-))")
      (test-expression "(local ((define x x)) 1)" "local variable used before its definition: x")
      (test-expression "(letrec ([x x]) 1)" "local variable used before its definition: x")
      (test-expression "(if 1 1 1)" "if: question result is not true or false: 1")
      (test-expression "(+ 1)" "procedure +: expects at least 2 arguments, given 1: 1")

      (test-expression "1.0" "1")
      (test-expression "#i1.0" "#i1.0")
      (test-expression "4/3" "{number 4/3 \"1 1/3\"}")
      (test-expression "1/3" "{number 1/3 \"1/3\"}")
      (test-expression "-4/3" "{number -4/3 \"-1 1/3\"}")
      (test-expression "-1/3" "{number -1/3 \"-1/3\"}")
      (test-expression "3/2" "1.5")
      (test-expression "1/2" "0.5")
      (test-expression "-1/2" "-0.5")
      (test-expression "-3/2" "-1.5")
      (test-expression "+1/3i" "0+1/3i")
      (test-expression "+1/2i" "0+0.5i")
      (test-expression "(exact? 1.5)" "true")
      
      (test-expression "(list 1)" "(list 1)")
      (test-expression "argv" "reference to undefined identifier: argv")))
  
  (define (test-setting setting-name value expression result)
    (fw:test:set-check-box! setting-name value)
    (let ([f (get-top-level-focus-window)])
      (fw:test:button-push "OK")
      (wait-for-new-frame f))
    (let* ([drs (get-top-level-focus-window)]
           [interactions (send drs get-interactions-text)])
      (clear-definitions drs)
      (type-in-definitions drs expression)
      (do-execute drs)
      (let* ([got (fetch-output drs)])
        (unless (string=? result got)
          (printf "FAILED: ~s ~s ~s ~s test~n expected: ~a~n      got: ~a~n"
                  (language) setting-name value expression result got)))))
  
  (define (test-hash-bang)
    (let* ([expression (format "#!~n1")]
           [result "1"]
           [drs (get-top-level-focus-window)]
           [interactions (send drs get-interactions-text)])
      (clear-definitions drs)
      (type-in-definitions drs expression)
      (do-execute drs)
      (let* ([got (fetch-output drs)])
        (unless (string=? "1" got)
          (printf "FAILED: ~s ~a test~n expected: ~a~n     got: ~a~n"
                  (language) expression result got)))))
  
  (define (check-top-of-repl)
    (let ([drs (wait-for-drscheme-frame)])
      (set-language #t)
      (do-execute drs)
      (let* ([interactions (send drs get-interactions-text)]
             [get-line (lambda (n) (send interactions get-text 
                                         (send interactions paragraph-start-position n)
                                         (send interactions paragraph-end-position n)))]
             [line0-expect (format "Welcome to DrScheme, version ~a." (version:version))]
             [line1-expect (format "Language: ~a." (car (last-pair (language))))]
             [line0-got (get-line 0)]
             [line1-got (get-line 1)])
        (unless (and (string=? line0-expect line0-got)
                     (string=? line1-expect line1-got))
          (printf "expected lines: ~n  ~a~n  ~a~ngot lines:~n  ~a~n  ~a~n" 
                  line0-expect line1-expect
                  line0-got line1-got)
          (error 'language-test.ss "failed get top of repl test")))))

  
  (define (generic-settings false/true?)
    (set-language #f)
    (test-setting "Case sensitive" #t "(eq? 'g 'G)" (if false/true? "false" "#f"))
    (set-language #f)
    (test-setting "Case sensitive" #f "(eq? 'g 'G)" (if false/true? "true" "#t"))
;    (set-language #f)
;    (test-setting "Unmatched cond/case is an error" #f
;                  (format "(cond [~a 1])" (if false/true? "false" "#f"))
;                  "")
    )
  
  (define (generic-output list? quasi-quote? has-sharing?)
    (let* ([drs (wait-for-drscheme-frame)]
           [expression (format "(define x (list 4/3))~n(list x x)")]
           [set-output-choice
            (lambda (option show-sharing pretty?)
              (set-language #f)
              (fw:test:set-radio-box! "Output Style" option)
              (when (and has-sharing? show-sharing)
                (fw:test:set-check-box!
                 "Show sharing in values"
                 (if (eq? show-sharing 'on) #t #f)))
              (fw:test:set-check-box!
               "Insert newlines in printed values"
               pretty?)
              (let ([f (get-top-level-focus-window)])
                (fw:test:button-push "OK")
                (wait-for-new-frame f)))]
           [test
	    ;; answer must either be a string, or a procedure that accepts both zero and 1
	    ;; argument. When the procedure accepts 1 arg, the argument is `got' and
	    ;; the result must be a boolean indicating if the result was satisfactory.
	    ;; if the procedure receives no arguments, it must return a descriptive string
	    ;; for the error message
            (lambda (option show-sharing pretty? answer)
              (set-output-choice option show-sharing pretty?)
              (do-execute drs)
              (let ([got (fetch-output drs)])
                (unless (if (procedure? answer)
                            (answer got)
                            (whitespace-string=? answer got))
                  (printf "FAILED ~s ~a, sharing ~a pretty? ~a~n            got ~s~n       expected ~s~n"
                          (language) option show-sharing pretty?
                          got
                          (if (procedure? answer) (answer) answer)))))])
      
      (clear-definitions drs)
      (type-in-definitions drs expression)
      
      (test "write" 'off #t "(({number 4/3 \"1 1/3\"}) ({number 4/3 \"1 1/3\"}))")
      (when has-sharing?
        (test "write" 'on #t "(#0=({number 4/3 \"1 1/3\"}) #0#)"))
      (when quasi-quote?
        (test "Quasiquote" 'off #t "`(({number 4/3 \"1 1/3\"}) ({number 4/3 \"1 1/3\"}))")
        (when has-sharing?
          (test "Quasiquote" 'on #t "(shared ((-1- `({number 4/3 \"1 1/3\"}))) `(,-1- ,-1-))")))
      
      (test "Constructor" 'off #t
            (if list?
                "(list (list {number 4/3 \"1 1/3\"}) (list {number 4/3 \"1 1/3\"}))"
                "(cons (cons {number 4/3 \"1 1/3\"} empty) (cons (cons {number 4/3 \"1 1/3\"} empty) empty))"))
      (when has-sharing?
        (test "Constructor" 'on #t
              (if list?
                  "(shared ((-1- (list {number 4/3 \"1 1/3\"}))) (list -1- -1-))"
                  "(shared ((-1- (cons {number 4/3 \"1 1/3\"} empty))) (cons -1- (cons -1- empty)))")))
      
      ;; setup comment box
      (clear-definitions drs)
      (fw:test:menu-select "Edit" "Insert Text Box")
      (fw:test:keystroke #\a)
      (fw:test:keystroke #\b)
      (fw:test:keystroke #\c)
      
      ;; test comment box in print-convert and print-convert-less settings
      (test "Constructor" #f #t "{embedded \"abc\"}")
      (test "write" #f #t "{embedded \"abc\"}")
      
      ;; setup write / pretty-print difference
      (clear-definitions drs)
      (for-each fw:test:keystroke
                (string->list
                 "(define (f n)\n(cond [(zero? n) null]\n[else (cons n (f (- n 1)))]))\n(f 200)"))
      (test "Constructor" #f #f
            (case-lambda
             [(x) (not (member #\newline (string->list x)))]
             [() "no newlines in result"]))
      (test "Constructor" #f #t
            (case-lambda
             [(x) (member #\newline (string->list x))]
             [() "newlines in result (may need to make the window smaller)"]))
      (test "write" #f #f
            (case-lambda
             [(x) (not (member #\newline (string->list x)))]
             [() "no newlines in result"]))
      (test "write" #f #t
            (case-lambda
             [(x) (member #\newline (string->list x))]
             [() "newlines in result (may need to make the window smaller)"]))))
  
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
  
  (define re:out-of-sync
    (regexp
     "WARNING: Interactions window is out of sync with the definitions window\\."))
  
  ;; test-expression : string (union string regexp (string -> boolean)) -> void
  ;; types an expression in the REPL and tests the output from the REPL.
  (define (test-expression expression expected)
    (let* ([drs (wait-for-drscheme-frame)]
           [interactions-text (send drs get-interactions-text)]
           [last-para (send interactions-text last-paragraph)]
           [check-expectation
            (lambda (got)
              (cond
                [(string? expected)
                 (whitespace-string=? expected got)]
                [(regexp? expected)
                 (regexp-match expected got)]
                [(procedure? expected)
                 (expected got)]))]
           [err-msg
            (cond
              [(string? expected)
               "FAILED: ~s expected ~s to produce ~s, got ~s instead~n"]
              [(regexp? expected)
               "FAILED: ~s expected ~s to match ~s, got ~s instead~n"]
              [(procedure? expected)
               "FAILED: ~s expected ~s to pass predicate ~s, got ~s~n"])])
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
        (when (regexp-match re:out-of-sync got)
          (error 'text-expression "got out of sync message"))
        (unless (check-expectation got)
          (printf err-msg (language) expression expected got)))))
  
  
  (define (run-test)
    ;; clear teachpack
    (let ([drs (wait-for-drscheme-frame)])
      (fw:test:menu-select "Language" "Clear All Teachpacks"))
    
    (raw-mred)
    (raw-mzscheme)
    (mred)
    (mzscheme)
    (beginner)
    (intermediate)
    (advanced)))
