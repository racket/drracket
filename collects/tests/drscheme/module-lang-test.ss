(module module-lang-test mzscheme
  (require "drscheme-test-util.ss"
           (lib "class.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix fw: (lib "framework.ss" "framework")))
  
  (provide run-test)
  
  (define-struct test (definitions   ;; string
                       interactions    ;; (union #f string)
                       result))       ;; string
  
  (define tests
    (list (make-test "" 
                     #f
                     (regexp "module-language: the definitions window must contain a module"))
	  (make-test "1" 
                     #f
                     (regexp "module-language: only module expressions are allowed"))
          (make-test "(module m mzscheme) 1" 
                     #f 
                     (regexp "module-language: there can only be one expression in the definitions window"))
          (make-test "(module m mzscheme (provide x) (define x 1))" "x" "1")
          (make-test "(module m mzscheme (define x 1))" "x" "1")
          (make-test "(module m mzscheme (define x 1) (define y 1) (provide y))" "x" "1")
          (make-test "(module m mzscheme (define x 1) (define y 2) (provide y))" "y" "2")
          (make-test "(module m mzscheme (require (lib \"list.ss\")))" 
                     "foldl" 
                     (regexp "foldl"))
          
          (make-test "(module m mzscheme (require (rename (lib \"list.ss\") local-foldl foldl)))" 
                     "local-foldl"
                     (regexp "foldl>"))
          
          (make-test "(module m mzscheme (require (all-except (lib \"list.ss\") foldl)))" 
                     "first"
                     (regexp "first>"))
          (make-test "(module m mzscheme (require (all-except (lib \"list.ss\") foldl)))" 
                     "foldl"
                     ". reference to undefined identifier: foldl")
          
          (make-test "(module m mzscheme (require (prefix x: (lib \"list.ss\")) (lib \"list.ss\")))" 
                     "foldl"
                     (regexp "foldl>"))
          (make-test "(module m mzscheme (require (prefix x: (lib \"list.ss\")) (lib \"list.ss\")))" 
                     "x:foldl"
                     (regexp "foldl>"))))
  
  (define drs (wait-for-drscheme-frame))
  (define interactions-text (send drs get-interactions-text))
  
  (define (single-test test)
    (let/ec k
      (clear-definitions drs)
      (type-in-definitions drs (test-definitions test))
      (do-execute drs)
        
      (let ([ints (test-interactions test)])
        
        (when ints
          (let ([after-execute-output
                 (send interactions-text
                       get-text
                       (send interactions-text paragraph-start-position 2)
                       (send interactions-text paragraph-end-position 2))])
            (unless (string=? "> " after-execute-output)
              (printf "FAILED: ~a\n        ~a\n        expected no output after execution, got: ~s\n"
                      (test-definitions test)
                      (or (test-interactions test) 'no-interactions)
                      after-execute-output)
              (k (void)))
            (type-in-interactions drs ints)
            (fw:test:keystroke #\return)
            (wait-for-computation drs)))
        
        (let* ([para-to-check (- (send interactions-text position-paragraph
                                       (send interactions-text last-position))
                                 1)]
               [after-int-start
                (send interactions-text paragraph-start-position para-to-check)]
               [after-int-end
                (send interactions-text paragraph-end-position para-to-check)]
               [after-int-output (send interactions-text
                                       get-text
                                       after-int-start
                                       after-int-end)]
               [passed?
                (cond
                  [(string? (test-result test))
                   (string=? after-int-output (test-result test))]
                  [(regexp? (test-result test))
                   (regexp-match (test-result test) after-int-output)])])
          (unless passed?
            (printf "FAILED: ~a\n        ~a\n  expected: ~a\n       got: ~a\n"
                    (test-definitions test)
                    (or (test-interactions test) 'no-interactions)
                    (test-result test)
                    after-int-output))))))
  
  (define (run-test)
    (set-language-level! '("module") #t)
    (for-each single-test tests)))
