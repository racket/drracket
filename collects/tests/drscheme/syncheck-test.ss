
(module syncheck-test mzscheme
  
  (require "drscheme-test-util.ss"
           (lib "gui.ss" "tests" "utils")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "text-string-style-desc.ss" "mrlib"))
  
  (provide run-test)
  
  ;; type str/ann = (list (union symbol string) symbol)
  ;; type test = (make-test string (listof str/ann))
  (define-struct test (input expected))
  
  ;; tests : (listof test)
  (define tests
    (list 
     (make-test "12345"
                '(("12345" constant)))
     (make-test "'abcdef"
                '(("'" imported-syntax)
                  ("abcdef" constant)))
     (make-test "(define f 1)"
                '(("("      #f)
                  ("define" imported-syntax)
                  (" "      #f)
                  ("f"      lexically-bound-variable)
                  (" "      #f)
                  ("1"      constant)
                  (")"      #f)))
     (make-test "(lambda (x) x)"
                '(("("      #f)
                  ("lambda" imported-syntax)
                  (" ("     #f)
                  ("x"      lexically-bound-variable)
                  (") "     #f)
                  ("x"      lexically-bound-variable)
                  (")"      #f)))
     (make-test "(lambda x x)"
                '(("("      #f)
                  ("lambda" imported-syntax)
                  (" "      #f)
                  ("x"      lexically-bound-variable)
                  (" "      #f)
                  ("x"      lexically-bound-variable)
                  (")"      #f)))
     (make-test "(lambda (x . y) x y)"
                '(("("      #f)
                  ("lambda" imported-syntax)
                  (" ("     #f)
                  ("x"      lexically-bound-variable)
                  (" . "    #f)
                  ("y"      lexically-bound-variable)
                  (") "     #f)
                  ("x"      lexically-bound-variable)
                  (" "      #f)
                  ("y"      lexically-bound-variable)
                  (")"      #f)))
     
     (make-test "(case-lambda [(x) x])"
                '(("("           #f)
                  ("case-lambda" imported-syntax)
                  (" [("         #f)
                  ("x"           lexically-bound-variable)
                  (") "          #f)
                  ("x"           lexically-bound-variable)
                  ("])"          #f)))
     (make-test "(if 1 2 3)"
                '(("("  #f)
                  ("if" imported-syntax)
                  (" "  #f)
                  ("1"  constant)
                  (" "  #f)
                  ("2"  constant)
                  (" "  #f)
                  ("3"  constant)
                  (")"  #f)))
     (make-test "(if 1 2)"
                '(("("  #f)
                  ("if" imported-syntax)
                  (" "  #f)
                  ("1"  constant)
                  (" "  #f)
                  ("2"  constant)
                  (")"  #f)))
     
     (make-test "(begin 1 2)"
                '(("("     #f)
                  ("begin" imported-syntax)
                  (" "     #f)
                  ("1"     constant)
                  (" "     #f)
                  ("2"     constant)
                  (")"     #f)))
     (make-test "(begin0 1 2)"
                '(("("      #f)
                  ("begin0" imported-syntax)
                  (" "      #f)
                  ("1"      constant)
                  (" "      #f)
                  ("2"      constant)
                  (")"      #f)))
     (make-test "(let ([x x]) x)"
                '(("("   #f)
                  ("let" imported-syntax)
                  (" ([" #f)
                  ("x"   lexically-bound-variable)
                  (" "   #f)
                  ("x"   error)
                  ("]) " #f)
                  ("x"   lexically-bound-variable)
                  (")"   #f)))
     (make-test "(letrec ([x x]) x)"
                '(("("      #f)
                  ("letrec" imported-syntax)
                  (" (["    #f)
                  ("x"      lexically-bound-variable)
                  (" "      #f)
                  ("x"      lexically-bound-variable)
                  ("]) "    #f)
                  ("x"      lexically-bound-variable)
                  (")"      #f)))
     (make-test "(set! x 1)"
                '(("("    #f)
                  ("set!" imported-syntax)
                  (" "    #f)
                  ("x"    error)
                  (" "    #f)
                  ("1"    constant)
                  (")"    #f)))
     (make-test "(let ([x 1]) (set! x 2))"
                '(("("    #f)
                  ("let"   imported-syntax)
                  (" (["   #f)
                  ("x"     lexically-bound-variable)
                  (" "     #f)
                  ("1"     constant)
                  ("]) ("  #f)
                  ("set!"  imported-syntax)
                  (" "     #f)
                  ("x"     lexically-bound-variable)
                  (" "     #f)
                  ("2"     constant)
                  ("))"    #f)))
     (make-test "object%"
                '(("object%" lexically-bound-variable)))
     (make-test "unbound-id"
                '(("unbound-id" error)))
     (make-test "(define bd 1) bd"
                '(("("       #f)
                  ("define"  imported-syntax)
                  (" "       #f)
                  ("bd"      lexically-bound-variable)
                  (" "       #f)
                  ("1"       constant)
                  (") "      #f)
                  ("bd"      lexically-bound-variable)))
     (make-test "#'abc"
                '(("#'"  imported-syntax)
                  ("abc" constant)))
     (make-test "(with-continuation-mark 1 2 3)"
                '(("("                      #f)
                  ("with-continuation-mark" imported-syntax)
                  (" "                      #f)
                  ("1"                      constant)
                  (" "                      #f)
                  ("2"                      constant)
                  (" "                      #f)
                  ("3"                      constant)
                  (")"                      #f)))
     (make-test "(f x)"
                '(("(" #f)
                  ("f" error)
                  (" " #f)
                  ("x" error)
                  (")" #f)))
     (make-test "(define-syntax (f stx) (syntax 1))"
                '(("("             #f)
                  ("define-syntax" imported-syntax)
                  (" ("            #f)
                  ("f"             lexically-bound-syntax)
                  (" "             #f)
                  ("stx"           lexically-bound-variable)
                  (") ("           #f)
                  ("syntax"        imported-syntax)
                  (" "             #f)
                  ("1"             constant)
                  ("))"            #f)))
     (make-test "(module m mzscheme)"
                '(("("        #f)
                  ("module"   imported-syntax)
                  (" m "      #f)
                  ("mzscheme" error)
                  (")"        #f)))
     (make-test "(require-for-syntax mzscheme)"
                '(("("                  #f)
                  ("require-for-syntax" imported-syntax)
                  (" "          #f)
                  ("mzscheme"   error)
                  (")"          #f)))
     (make-test "(require (lib \"list.ss\"))"
                '(("("                   #f)
                  ("require"             imported-syntax)
                  (" "                   #f)
                  ("(lib \"list.ss\")"   error)
                  (")"                   #f)))
     (make-test "(module m mzscheme (provide x) (define x 1))"
                '(("("             #f)
                  ("module"        imported-syntax)
                  (" m mzscheme (" #f)
                  ("provide"       imported-syntax)
                  (" "             #f)
                  ("x"             lexically-bound-variable)
                  (") ("           #f)
                  ("define"        imported-syntax)
                  (" "             #f)
                  ("x"             lexically-bound-variable)
                  (" "             #f)
                  ("1"             constant)
                  ("))"            #f)))
     
     (make-test "(module m mzscheme (+ 1 2))"
                '(("("             #f)
                  ("module"        imported-syntax)
                  (" m mzscheme (" #f)
                  ("+"             imported-variable)
		  (" "             #f)
		  ("1"             constant)
		  (" "             #f)
		  ("2"             constant)
                  ("))"            #f)))
     
     (make-test "(module m mzscheme (require (lib \"list.ss\")))"
                '(("("                 #f)
                  ("module"            imported-syntax)
                  (" m mzscheme ("     #f)
                  ("require"           imported-syntax)
                  (" "                 #f)
                  ("(lib \"list.ss\")" error)
                  ("))"                #f)))
     
     (make-test "(module m mzscheme (require-for-syntax (lib \"list.ss\")) (define-syntax s foldl))"
                '(("("                     #f)
                  ("module"                imported-syntax)
                  (" m mzscheme ("         #f)
                  ("require-for-syntax"    imported-syntax)
                  (" (lib \"list.ss\")) (" #f)
                  ("define-syntax"         imported-syntax)
                  (" "                     #f)
                  ("s"                     lexically-bound-syntax)
                  (" "                     #f)
                  ("foldl"                 imported-variable)
                  ("))"                    #f)))
     
     (make-test "(module m mzscheme (require-for-syntax (lib \"etc.ss\")) (define-syntax s (rec f 1)))"
                '(("("                     #f)
                  ("module"                imported-syntax)
                  (" m mzscheme ("         #f)
                  ("require-for-syntax"    imported-syntax)
                  (" (lib \"etc.ss\")) ("  #f)
                  ("define-syntax"         imported-syntax)
                  (" "                     #f)
                  ("s"                     lexically-bound-syntax)
                  (" ("                    #f)
                  ("rec"                   imported-syntax)
                  (" "                     #f)
                  ("f"                     lexically-bound-variable)
                  (" "                     #f)
                  ("1"                     constant)
                  (")))"                   #f)))

     (make-test "(define-syntax s (lambda (stx) (syntax-case stx () (_ 123))))"
                '(("("             #f)
                  ("define-syntax" imported-syntax)
                  (" "             #f)
                  ("s"             lexically-bound-syntax)
                  (" ("            #f)
                  ("lambda"        imported-syntax)
                  (" ("            #f)
                  ("stx"           lexically-bound-variable)
                  (") ("           #f)
                  ("syntax-case"   imported-syntax)
                  (" "             #f)
                  ("stx"           lexically-bound-variable)
                  (" () (_ "       #f)
                  ("123"           constant)
                  ("))))"          #f)))

     (make-test "(require (lib \"list.ss\")) first"
                '(("("                    #f)
                  ("require"              imported-syntax)
                  (" (lib \"list.ss\")) " #f)
                  ("first"                imported-variable)))
     
     (make-test "(require (lib \"etc.ss\")) (rec f 1)"
                '(("("                    #f)
                  ("require"              imported-syntax)
                  (" (lib \"etc.ss\")) (" #f)
                  ("rec"                  imported-syntax)
                  (" "                    #f)
                  ("f"                    lexically-bound-variable)
                  (" "                    #f)
                  ("1"                    constant)
                  (")"                    #f)))
     
     (make-test "(define-struct s ())"
                '(("("             #f)
                  ("define-struct" imported-syntax)
                  (" "             #f)
                  ("s"             lexically-bound-syntax)
                  (" ())"          #f)))
     
     
     ;; sure would be nice if `s' in the second instance came
     ;; out as lexically bound syntax and were bound to the 
     ;; first s...
     (make-test "(define-struct s ()) (define-struct (t s) ())"
                '(("("             #f)
                  ("define-struct" imported-syntax)
                  (" "             #f)
                  ("s"             lexically-bound-syntax)
                  (" ()) ("        #f)
                  ("define-struct" imported-syntax)
                  (" ("            #f)
                  ("t"             lexically-bound-syntax)
                  (" s) ())"       #f))) 
     
     (make-test "`(1 ,x 2)"
                '(("`"        imported-syntax)
                  ("("        #f)
                  ("1"        constant)
                  (" ,"       #f)
                  ("x"        error)
                  (" "        #f)
                  ("2"        constant)
                  (")"        #f)))

     (make-test "`(a ,2 b c d)"
                `(("`"  imported-syntax)
                  ("("  #f)
                  ("a"  constant)
                  (" ," #f)
                  ("2"  constant)
                  (" "  #f)
                  ("b"  constant)
                  (" "  #f)
                  ("c"  constant)
                  (" "  #f)
                  ("d"  constant)
                  (")"  #f)))
     
     (make-test "#!"
                '(("#!" #f)))
     
     (make-test "#!\n"
                '(("#!\n" #f)))
     
     (make-test "#!\n1"
                '(("#!\n" #f)
                  ("1"    constant)))
     
     (make-test "#!\n1\n1"
                '(("#!\n" #f)
                  ("1"    constant)
                  ("\n"   #f)
                  ("1"    constant)))
     
     (make-test "(module m mzscheme (lambda (x) x) (provide))"
                '(("("             #f)
                  ("module"        imported-syntax)
                  (" m mzscheme (" #f)
                  ("lambda"        imported-syntax)
                  (" ("            #f)
                  ("x"             lexically-bound-variable)
                  (") "            #f)
                  ("x"             lexically-bound-variable)
                  (") ("           #f)
                  ("provide"       imported-syntax)
                  ("))"            #f)))
     
     (make-test "(define tordu3 '(a . #0=(b c d . #0#)))"
                '(("("        #f)
                  ("define"   imported-syntax)
                  (" "        #f)
                  ("tordu3"   lexically-bound-variable)
                  (" "        #f)
                  ("'"        imported-syntax)
                  ("(a . #0=(b c d . #0#))" constant)
                  (")"        #f)))


                                                                             
                                          ;;             ;;;                 
                                           ;               ;                 
                                           ;               ;                 
 ;;;;   ; ;;;  ; ;;;   ;;;  ;;; ;;;        ;;;;    ;;;     ;     ;;;  ;;; ;;;
     ;   ;      ;     ;   ;  ;   ;         ;   ;  ;   ;    ;    ;   ;  ;   ; 
  ;;;;   ;      ;     ;   ;  ; ; ;         ;   ;  ;;;;;    ;    ;   ;  ; ; ; 
 ;   ;   ;      ;     ;   ;  ; ; ;         ;   ;  ;        ;    ;   ;  ; ; ; 
 ;   ;   ;      ;     ;   ;   ; ;          ;   ;  ;   ;    ;    ;   ;   ; ;  
  ;;; ; ;;;;   ;;;;    ;;;    ; ;         ; ;;;    ;;;   ;;;;;;  ;;;    ; ;  
                                                                             
                                                                             
                                                                             
     ;; the tests below should also be part of arrow-based test suite.


     (make-test "(let l () l l)"
                '(("("    #f)
                  ("let"  imported-syntax)
                  (" "    #f)
                  ("l"    lexically-bound-variable)
                  (" () " #f)
                  ("l"    lexically-bound-variable)
                  (" "    #f)
                  ("l"    lexically-bound-variable)
                  (")"    #f)))
     (make-test "(class object% this)"
                '(("("       #f)
                  ("class"   imported-syntax)
                  (" "       #f)
                  ("object%" lexically-bound-variable)
                  (" "       #f)
                  ("this"    lexically-bound-syntax)
                  (")"       #f)))
     
     (make-test "(module m mzscheme (require (lib \"list.ss\")) foldl)"
                '(("("                    #f)
                  ("module"               imported-syntax)
                  (" m mzscheme ("        #f)
                  ("require"              imported-syntax)
                  (" (lib \"list.ss\")) " #f)
                  ("foldl"                imported-variable)
                  (")"                    #f)))
     (make-test "(module m (lib \"htdp-beginner.ss\" \"lang\") empty)"
                '(("("                                         #f)
                  ("module"                                    imported-syntax)
                  (" m (lib \"htdp-beginner.ss\" \"lang\") "   #f)
                  ("empty"                                     imported-variable)
                  (")"                                         #f)))
     (make-test "(module m mzscheme (require (prefix x: (lib \"list.ss\"))) x:foldl)"
                '(("("                                #f)
                  ("module"                           imported-syntax)
                  (" m mzscheme ("                    #f)
                  ("require"                          imported-syntax)
                  (" (prefix x: (lib \"list.ss\"))) " #f)
                  ("x:foldl"                          imported-variable)
                  (")"                                #f)))
     (make-test "(module m mzscheme (require (lib \"etc.ss\")) (rec f 1))"
                '(("("                     #f)
                  ("module"                imported-syntax)
                  (" m mzscheme ("         #f)
                  ("require"               imported-syntax)
                  (" (lib \"etc.ss\")) ("  #f)
                  ("rec"                   imported-syntax)
                  (" "                     #f)
                  ("f"                     lexically-bound-variable)
                  (" "                     #f)
                  ("1"                     constant)
                  ("))"                    #f)))
     
     (make-test "(module m (lib \"htdp-intermediate.ss\" \"lang\") (local ((define x x)) x))"
                '(("("                                            #f)
                  ("module"                                       imported-syntax)
                  (" m (lib \"htdp-intermediate.ss\" \"lang\") (" #f)
                  ("local"                                        imported-syntax)
                  (" ((define "                                   #f)
                  ("x"                                            lexically-bound-variable)
                  (" "                                            #f)
                  ("x"                                            lexically-bound-variable)
                  (")) "                                          #f)
                  ("x"                                            lexically-bound-variable)
                  ("))"                                           #f)))
     ))
  
  (define (run-test)
    (set-language-level! (list "PLT" (regexp "Graphical")))
    (let* ([drs (wait-for-drscheme-frame)]
           [defs (send drs get-definitions-text)])
      (send defs force-stop-colorer #t))
    (for-each run-one-test tests))
  
  (define (run-one-test test)
    (let ([drs (wait-for-drscheme-frame)]
          [input (test-input test)]
          [expected (test-expected test)])
      (clear-definitions drs)
      (type-in-definitions drs input)
      (test:button-push (send drs syncheck:get-button))
      (wait-for-computation drs)
      (let ([got (get-annotated-output drs)])
        (compare-output expected got input))))
  
  (define remappings
    '((constant #f)))
  
  (define (collapse-and-rename expected)
    (let ([renamed
           (map (lambda (ent)
                  (let* ([str (car ent)]
                         [id (cadr ent)]
                         [matches (assoc id remappings)])
                    (if matches
                        (list str (cadr matches))
                        ent)))
                expected)])
      (let loop ([ids renamed])
        (cond
          [(null? ids) null]
          [(null? (cdr ids)) ids]
          [else (let ([fst (car ids)]
                      [snd (cadr ids)])
                  (if (eq? (cadr fst) (cadr snd))
                      (loop (cons (list (string-append (car fst) (car snd)) (cadr fst))
                                  (cddr ids)))
                      (cons fst (loop (cdr ids)))))]))))
                 
  
  ;; compare-output 
  ;; should show first difference.
  (define (compare-output raw-expected got input)
    (let ([expected (collapse-and-rename raw-expected)])
      (unless (equal? got expected)
        (printf "FAILED: ~s\n      expected: ~s\n           got: ~s\n"
                input expected got))))
  
  ;; get-annotate-output : drscheme-frame -> (listof str/ann)
  (define (get-annotated-output drs)
    (get-string/style-desc (send drs get-definitions-text))))

