;;; drscheme-test.ss

;;; files for testing of DrScheme

;;; Author:  Paul Steckler

(load-relative "drscheme-test-util.ss")

(define test-files 
  (list 
   "menu-test.ss"          ; opens some dialogs and closes them
   "repl-test.ss"          ; executes and loads some terms in the REPL
   "check-syntax-test.ss"  ; calls syntax checker on some terms
   ))

(define pr-files 
  (list
   "pr-17.ss"
   "pr-39.ss"
   "pr-39.ss"
   "pr-46.ss"
   "pr-48.ss"
   "pr-51.ss"
   "pr-58.ss"
   "pr-80.ss"
   "pr-99.ss"
   "pr-144.ss"
   "pr-246.ss"
   ))

(define (run-it s)
  (clear-definitions (wait-for-drscheme-frame))
  (printf "Running tests in file ~a...~n" s)
  (load-relative s)
  (printf "Done with file ~a.~n" s))

(printf "Running DrScheme tests...~n") 

(for-each run-it test-files)

(printf "Done with DrScheme tests.~n") 

(printf "Running tests designed from GNATS pr's...~n") 

(for-each run-it pr-files)

(printf "Done with GNATS pr tests.~n") 
