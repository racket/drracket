
(module syncheck-test mzscheme
  
  (require "drscheme-test-util.ss"
           (lib "gui.ss" "tests" "utils")
           (lib "etc.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "text-string-style-desc.ss" "mrlib"))
  
  (provide run-test)
  
  ;; type str/ann = (list (union symbol string) symbol)
  ;; type test = (make-test string
  ;;                        (listof str/ann)
  ;;                        (listof (cons (list number number) (listof (list number number)))))
  (define-struct test (input expected arrows))
  (define-struct (dir-test test) ())
  
  (define build-test
    (opt-lambda (input expected [arrow-table '()])
      (make-test input expected arrow-table)))
  
  ;; tests : (listof test)
  (define tests
    (list 
     
     (build-test "12345"
                '(("12345" constant)))
     (build-test "'abcdef"
                '(("'" imported-syntax)
                  ("abcdef" constant)))
     (build-test "(define f 1)"
                '(("("      #f)
                  ("define" imported-syntax)
                  (" "      #f)
                  ("f"      lexically-bound-variable)
                  (" "      #f)
                  ("1"      constant)
                  (")"      #f)))
     (build-test "(lambda (x) x)"
                '(("("      #f)
                  ("lambda" imported-syntax)
                  (" ("     #f)
                  ("x"      lexically-bound-variable)
                  (") "     #f)
                  ("x"      lexically-bound-variable)
                  (")"      #f))
                (list '((9 10) (12 13))))
     (build-test "(lambda x x)"
                '(("("      #f)
                  ("lambda" imported-syntax)
                  (" "      #f)
                  ("x"      lexically-bound-variable)
                  (" "      #f)
                  ("x"      lexically-bound-variable)
                  (")"      #f))
                (list '((8 9) (10 11))))
     (build-test "(lambda (x . y) x y)"
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
                  (")"      #f))
                (list '((9 10) (16 17))
                      '((13 14) (18 19))))
     
     (build-test "(case-lambda [(x) x])"
                 '(("("           #f)
                   ("case-lambda" imported-syntax)
                   (" [("         #f)
                   ("x"           lexically-bound-variable)
                   (") "          #f)
                   ("x"           lexically-bound-variable)
                   ("])"          #f))
                 (list '((15 16) (18 19))))
     
     (build-test "(if 1 2 3)"
                '(("("  #f)
                  ("if" imported-syntax)
                  (" "  #f)
                  ("1"  constant)
                  (" "  #f)
                  ("2"  constant)
                  (" "  #f)
                  ("3"  constant)
                  (")"  #f)))
     (build-test "(if 1 2)"
                '(("("  #f)
                  ("if" imported-syntax)
                  (" "  #f)
                  ("1"  constant)
                  (" "  #f)
                  ("2"  constant)
                  (")"  #f)))
     
     (build-test "(begin 1 2)"
                '(("("     #f)
                  ("begin" imported-syntax)
                  (" "     #f)
                  ("1"     constant)
                  (" "     #f)
                  ("2"     constant)
                  (")"     #f)))
     (build-test "(begin0 1 2)"
                '(("("      #f)
                  ("begin0" imported-syntax)
                  (" "      #f)
                  ("1"      constant)
                  (" "      #f)
                  ("2"      constant)
                  (")"      #f)))
     (build-test "(let ([x x]) x)"
                '(("("   #f)
                  ("let" imported-syntax)
                  (" ([" #f)
                  ("x"   lexically-bound-variable)
                  (" "   #f)
                  ("x"   error)
                  ("]) " #f)
                  ("x"   lexically-bound-variable)
                  (")"   #f))
                (list '((7 8) (13 14))))
     (build-test "(letrec ([x x]) x)"
                '(("("      #f)
                  ("letrec" imported-syntax)
                  (" (["    #f)
                  ("x"      lexically-bound-variable)
                  (" "      #f)
                  ("x"      lexically-bound-variable)
                  ("]) "    #f)
                  ("x"      lexically-bound-variable)
                  (")"      #f))
                (list '((10 11) (12 13) (16 17))))
     (build-test "(#%top . x)"
                 '(("("     #f) 
                   ("#%top" imported-syntax)
                   (" . "   #f)
                   ("x"     error)
                   (")"    #f)))
     (build-test "(set! x 1)"
                '(("("    #f)
                  ("set!" imported-syntax)
                  (" "    #f)
                  ("x"    error)
                  (" "    #f)
                  ("1"    constant)
                  (")"    #f)))
     (build-test "(set! x 1) (define x 2)"
                 '(("("      #f)
                   ("set!"   imported-syntax)
                   (" "      #f)
                   ("x"      lexically-bound-variable)
                   (" "      #f)
                   ("1"      constant)
                   (") ("    #f)
                   ("define" imported-syntax)
                   (" "      #f)
                   ("x"      lexically-bound-variable)
                   (" 2)"    #f))
                 (list '((19 20) (6 7))))
     (build-test "(let ([x 1]) (set! x 2))"
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
                  ("))"    #f))
                (list '((7 8) (19 20))))
     (build-test "object%"
                '(("object%" lexically-bound-variable)))
     (build-test "unbound-id"
                '(("unbound-id" error)))
     (build-test "(define bd 1) bd"
                '(("("       #f)
                  ("define"  imported-syntax)
                  (" "       #f)
                  ("bd"      lexically-bound-variable)
                  (" "       #f)
                  ("1"       constant)
                  (") "      #f)
                  ("bd"      lexically-bound-variable))
                (list '((8 10) (14 16))))
     (build-test "#'abc"
                '(("#'"  imported-syntax)
                  ("abc" constant)))
     (build-test "(with-continuation-mark 1 2 3)"
                '(("("                      #f)
                  ("with-continuation-mark" imported-syntax)
                  (" "                      #f)
                  ("1"                      constant)
                  (" "                      #f)
                  ("2"                      constant)
                  (" "                      #f)
                  ("3"                      constant)
                  (")"                      #f)))
     (build-test "(f x)"
                '(("(" #f)
                  ("f" error)
                  (" " #f)
                  ("x" error)
                  (")" #f)))
     (build-test "(define-syntax (f stx) (syntax 1))"
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
     (build-test "(module m mzscheme)"
                '(("("        #f)
                  ("module"   imported-syntax)
                  (" m "      #f)
                  ("mzscheme" error)
                  (")"        #f)))
     (build-test "(require-for-syntax mzscheme)"
                '(("("                  #f)
                  ("require-for-syntax" imported-syntax)
                  (" "          #f)
                  ("mzscheme"   error)
                  (")"          #f)))
     (build-test "(require (lib \"list.ss\"))"
                '(("("                   #f)
                  ("require"             imported-syntax)
                  (" "                   #f)
                  ("(lib \"list.ss\")"   error)
                  (")"                   #f)))
     (build-test "(module m mzscheme (provide x) (define x 1))"
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
                  ("))"            #f))
                (list '((10 18) (20 27) (32 38))
                      '((39 40) (28 29))))
     
     (build-test "(module m mzscheme (+ 1 2))"
                '(("("             #f)
                  ("module"        imported-syntax)
                  (" m mzscheme (" #f)
                  ("+"             imported-variable)
		  (" "             #f)
		  ("1"             constant)
		  (" "             #f)
		  ("2"             constant)
                  ("))"            #f))
                (list '((10 18) (20 21))))
     
     (build-test "(module m mzscheme (require (lib \"list.ss\")))"
                '(("("                 #f)
                  ("module"            imported-syntax)
                  (" m mzscheme ("     #f)
                  ("require"           imported-syntax)
                  (" "                 #f)
                  ("(lib \"list.ss\")" error)
                  ("))"                #f))
                (list '((10 18) (20 27))))
     
     (build-test "(module m mzscheme (require-for-syntax (lib \"list.ss\")) (define-syntax s foldl))"
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
                  ("))"                    #f))
                (list '((10 18) (20 38) (57 70))
                      '((39 54) (73 78))))
     
     (build-test "(module m mzscheme (require-for-syntax (lib \"etc.ss\")) (define-syntax s (rec f 1)))"
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
                  (")))"                   #f))
                (list '((10 18) (20 38) (56 69))
                      '((39 53) (73 76))))

     (build-test "(define-syntax s (lambda (stx) (syntax-case stx () (_ 123))))"
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
                  (" () ("         #f)
                  ("_"             lexically-bound-syntax)
                  (" "             #f)
                  ("123"           constant)
                  ("))))"          #f))
                (list '((26 29) (44 47))))

     (build-test "(require (lib \"list.ss\")) first"
                '(("("                    #f)
                  ("require"              imported-syntax)
                  (" (lib \"list.ss\")) " #f)
                  ("first"                imported-variable))
                (list '((9 24) (26 31))))
     
     (build-test "(require (lib \"etc.ss\")) (rec f 1)"
                '(("("                    #f)
                  ("require"              imported-syntax)
                  (" (lib \"etc.ss\")) (" #f)
                  ("rec"                  imported-syntax)
                  (" "                    #f)
                  ("f"                    lexically-bound-variable)
                  (" "                    #f)
                  ("1"                    constant)
                  (")"                    #f))
                (list '((9 23) (26 29))))
     
     (build-test "(define-struct s ())"
                '(("("             #f)
                  ("define-struct" imported-syntax)
                  (" "             #f)
                  ("s"             lexically-bound-syntax)
                  (" ())"          #f)))
     
     (build-test "(define-struct s ()) (define-struct (t s) ())"
                '(("("             #f)
                  ("define-struct" imported-syntax)
                  (" "             #f)
                  ("s"             lexically-bound-syntax)
                  (" ()) ("        #f)
                  ("define-struct" imported-syntax)
                  (" ("            #f)
                  ("t"             lexically-bound-syntax)
                  (" "             #f)
                  ("s"             lexically-bound-syntax)
                  (") ())"         #f))
                (list '((15 16) (39 40))))
     
     (build-test "(let () (define-struct s (x)) 1)"
                 '(("("             #f)
                   ("let"           imported-syntax)
                   (" () ("         #f)
                   ("define-struct" imported-syntax)
                   (" "             #f)
                   ("s"             lexically-bound-syntax)
                   (" (x)) "        #f)
                   ("1"             constant)
                   (")"             #f)))
     
     (build-test "(let ([x 12]) (define-struct s (x)) x)"
                 '(("("             #f)
                   ("let"           imported-syntax)
                   (" (["           #f)
                   ("x"             lexically-bound-variable)
                   (" "             #f)
                   ("12"            constant)
                   ("]) ("          #f)
                   ("define-struct" imported-syntax)
                   (" "             #f)
                   ("s"             lexically-bound-syntax)
                   (" (x)) "        #f)
                   ("x"             lexically-bound-variable)
                   (")"             #f))
                 (list '((7 8) (36 37))))
     
     (build-test "`(1 ,x 2)"
                '(("`"        imported-syntax)
                  ("("        #f)
                  ("1"        constant)
                  (" ,"       #f)
                  ("x"        error)
                  (" "        #f)
                  ("2"        constant)
                  (")"        #f)))

     (build-test "`(a ,2 b c d)"
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
     
     (build-test "#!"
                '(("#!" #f)))
     
     (build-test "#!\n"
                '(("#!\n" #f)))
     
     (build-test "#!\n1"
                '(("#!\n" #f)
                  ("1"    constant)))
     
     (build-test "#!\n1\n1"
                '(("#!\n" #f)
                  ("1"    constant)
                  ("\n"   #f)
                  ("1"    constant)))
     
     (build-test "(module m mzscheme (lambda (x) x) (provide))"
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
                  ("))"            #f))
                (list '((10 18) (20 26) (35 42))
                      '((28 29) (31 32))))
     
     (build-test "(module m mzscheme (define-struct s (a)) s-a make-s s? set-s-a!)"
                '(("("             #f)
                  ("module"        imported-syntax)
                  (" m mzscheme (" #f)
                  ("define-struct" imported-syntax)
                  (" "             #f)
                  ("s"             lexically-bound-syntax)
                  (" (a)) "        #f)
                  ("s-a"           lexically-bound-variable)
                  (" "             #f)
                  ("make-s"        lexically-bound-variable)
                  (" "             #f)
                  ("s?"            lexically-bound-variable)
                  (" "             #f)
                  ("set-s-a!"      lexically-bound-variable)
                  (")"             #f))
                (list '((10 18) (20 33))))
     
     (build-test "(define tordu3 '(a . #0=(b c d . #0#)))"
                '(("("        #f)
                  ("define"   imported-syntax)
                  (" "        #f)
                  ("tordu3"   lexically-bound-variable)
                  (" "        #f)
                  ("'"        imported-syntax)
                  ("(a . #0=(b c d . #0#))" constant)
                  (")"        #f)))

     (build-test "(let l () l l)"
                '(("("    #f)
                  ("let"  imported-syntax)
                  (" "    #f)
                  ("l"    lexically-bound-variable)
                  (" () " #f)
                  ("l"    lexically-bound-variable)
                  (" "    #f)
                  ("l"    lexically-bound-variable)
                  (")"    #f))
                (list '((5 6) (10 11) (12 13))))
     (build-test "(class object% this)"
                '(("("       #f)
                  ("class"   imported-syntax)
                  (" "       #f)
                  ("object%" lexically-bound-variable)
                  (" "       #f)
                  ("this"    lexically-bound-syntax)
                  (")"       #f)))
     (build-test "(module m mzscheme (require (lib \"list.ss\")) foldl)"
                '(("("                    #f)
                  ("module"               imported-syntax)
                  (" m mzscheme ("        #f)
                  ("require"              imported-syntax)
                  (" (lib \"list.ss\")) " #f)
                  ("foldl"                imported-variable)
                  (")"                    #f))
                (list '((10 18) (20 27))
                      '((28 43) (45 50))))
     (build-test "(module m (lib \"htdp-beginner.ss\" \"lang\") empty)"
                '(("("                                         #f)
                  ("module"                                    imported-syntax)
                  (" m (lib \"htdp-beginner.ss\" \"lang\") "   #f)
                  ("empty"                                     imported-variable)
                  (")"                                         #f))
                (list '((10 41) (42 47))))
     (build-test "(module m mzscheme (require (prefix x: (lib \"list.ss\"))) x:foldl)"
                '(("("                                #f)
                  ("module"                           imported-syntax)
                  (" m mzscheme ("                    #f)
                  ("require"                          imported-syntax)
                  (" (prefix x: (lib \"list.ss\"))) " #f)
                  ("x:foldl"                          imported-variable)
                  (")"                                #f))
                (list '((10 18) (20 27))
                      '((28 55) (57 64))))
     
     (build-test "(module m mzscheme (require (prefix x: (lib \"list.ss\")) (lib \"list.ss\")) x:foldl foldl)"
                '(("("                                                  #f)
                  ("module"                                             imported-syntax)
                  (" m mzscheme ("                                      #f)
                  ("require"                                            imported-syntax)
                  (" (prefix x: (lib \"list.ss\")) (lib \"list.ss\")) " #f)
                  ("x:foldl"                                            imported-variable)
                  (" "                                                  #f)
                  ("foldl"                                              imported-variable)
                  (")"                                                  #f))
                (list '((10 18) (20 27))
                      '((28 55) (73 80) (81 86))
                      '((56 71) (73 80) (81 86))))
                      
     (build-test "(module m mzscheme (require (lib \"etc.ss\")) (rec f 1))"
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
                  ("))"                    #f))
                (list '((10 18) (20 27))
                      '((28 42) (45 48))))
     
     (build-test "(module m (lib \"htdp-intermediate.ss\" \"lang\") (local ((define x x)) x))"
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
                  ("))"                                           #f))
                (list '((10 45) (47 52))
                      '((62 63) (64 65) (68 69))))
     
     (make-dir-test "(module m mzscheme (require \"~a/list.ss\") foldl foldl)"
                    '(("("             #f)
                      ("module"        imported-syntax)
                      (" m mzscheme (" #f)
                      ("require"       imported-syntax)
                      (" \""           #f)    
                      (relative-path   #f)
                      ("/list.ss\") "  #f)
                      ("foldl"         imported-variable)
                      (" "             #f)
                      ("foldl"         imported-variable)
                      (")"             #f))
                    #f)))
  
  (define (run-test)
    (check-language-level #rx"Graphical")
    (let* ([drs (wait-for-drscheme-frame)]
           [defs (send drs get-definitions-text)]
           [filename (make-temporary-file "syncheck-test~a")])
      (let-values ([(dir _1 _2) (split-path filename)])
        (send defs save-file filename)
        (preferences:set 'framework:coloring-active #f)
        (for-each (run-one-test (normalize-path dir)) tests)
        (preferences:set 'framework:coloring-active #t)
        (send defs save-file) ;; clear out autosave
        (send defs set-filename #f)
        (delete-file filename))))
  
  (define ((run-one-test save-dir) test)
    (let* ([drs (wait-for-drscheme-frame)]
           [defs (send drs get-definitions-text)]
           [input (test-input test)]
           [expected (test-expected test)]
           [arrows (test-arrows test)]
           [relative (find-relative-path save-dir (collection-path "mzlib"))])
      (clear-definitions drs)
      (cond
        [(dir-test? test)
         (type-in-definitions drs (format input relative))]
        [else (type-in-definitions drs input)])
      (test:button-push (send drs syncheck:get-button))
      (wait-for-computation drs)
      
      ;; this isn't right -- seems like there is a race condition because
      ;; wait-for-computation isn't waiting long enough?
      '(when (send defs in-edit-sequence?)
         (error 'syncheck-test.ss "still in edit sequence for ~s" input))
      
      ;; need to check for syntax error here
      (let ([got (get-annotated-output drs)])
        (compare-output (cond
                          [(dir-test? test)
                           (map (lambda (x)
                                  (list (if (eq? (car x) 'relative-path) relative (car x))
                                        (cadr x)))
                                expected)]
                          [else
                           expected])
                        got
                        arrows 
                        (send defs syncheck:get-bindings-table)
                        input))))
  
  (define remappings
    '((constant #f)
      (imported-syntax imported-identifier)
      (imported-variable imported-identifier)
      (lexically-bound-syntax lexically-bound-identifier)
      (lexically-bound-variable lexically-bound-identifier)))
  
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
    
  ;; compare-arrows : (listof (cons (list number number) (listof (list number number))))
  ;;                  hash-table[(list text number number) -o> (listof (list text number number))]
  ;;               -> void
  (define (compare-arrows test-exp expected raw-actual)
    (when expected
      (let ()
        (define already-checked (make-hash-table 'equal))
        
        (define actual-ht (make-hash-table 'equal))
        (define stupid-internal-define-syntax1
          (hash-table-for-each raw-actual
                               (lambda (k v)
                                 (hash-table-put! actual-ht
                                                  (cdr k) 
                                                  (quicksort
                                                   (map cdr v)
                                                   (lambda (x y) (< (car x) (car y))))))))
        (define expected-ht (make-hash-table 'equal))
        (define stupid-internal-define-syntax2
          (for-each (lambda (binding) (hash-table-put! expected-ht (car binding) (cdr binding)))
                    expected))
        ;; binding-in-ht? : hash-table (list number number) (listof (list number number)) -> boolean
        (define (test-binding expected? ht)
          (lambda (pr)
            (let ([frm (car pr)]
                  [to (cdr pr)])
              (hash-table-get
               already-checked
               frm
               (lambda ()
                 (hash-table-put! already-checked frm #t)
                 (let ([ht-ent (hash-table-get ht frm (lambda () 'nothing-there))])
                   (unless (equal? ht-ent to)
                     (printf (if expected? 
                                 "FAILED arrow test ~s from ~s\n  expected ~s\n    actual ~s\n"
                                 "FAILED arrow test ~s from ~s\n    actual ~s\n  expected ~s\n")
                             test-exp
                             frm
                             ht-ent
                             to))))))))
        
        (for-each (test-binding #t expected-ht) (hash-table-map actual-ht cons))
        (for-each (test-binding #f actual-ht) (hash-table-map expected-ht cons)))))
  
  (define (compare-output raw-expected got arrows arrows-got input)
    (let ([expected (collapse-and-rename raw-expected)])
      (cond
        [(equal? got expected)
         (compare-arrows input arrows arrows-got)]
        [else
         (printf "FAILED: ~s\n      expected: ~s\n           got: ~s\n"
                 input expected got)])))
  
  ;; get-annotate-output : drscheme-frame -> (listof str/ann)
  (define (get-annotated-output drs)
    (get-string/style-desc (send drs get-definitions-text))))

