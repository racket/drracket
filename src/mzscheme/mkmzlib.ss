
; Utilities for distributing pre-compiled MzLib

(current-library-collection-paths (list (build-path (current-directory) 'up "collects")))

(require-library "compile.ss")

(define flags '(ignore-require-library
		preserve-elaborations))
(define suffix ".zo")

; Compiles large mzlib files to .zo files
(define (make-zo)
  (define (go f)
    (printf "Compiling ~a.ss to ~a~a~n" f f suffix)
    (compile-file (build-path 'up "collects" "mzlib" (string-append f ".ss"))
		  (build-path 'up "collects" "mzlib" "compiled" (string-append f suffix))
		  flags))
  
  (define (go-unit suffix)
    (lambda (f)
      (require-library (string-append f "s.ss"))
      (go (string-append f suffix))))
  
  (define go-r (go-unit "r"))
  (define go-u (go-unit "u"))
  
  (go "matchr")
  
  (go "letplus")
  (go "macro")
  (go "synrule")
  (go "shared")
  
  (require-library "macro.ss")
  
  (go-r "compile")
  (go-r "pretty")
  (go-r "functio")
  (go-r "compat")
  (go "compatm")
  (go-r "zmath")
  (go-r "string")
  (go-r "file")
  (go-r "date")
  (go-r "inflate")
  (go-r "pconver"))

; Expand letplsrc.ss to letplus.ss
(define (make-let+)
  (require-library "match.ss")
  
  (begin
    (define e (with-input-from-file "../collects/mzlib/letplsrc.ss" read))
    (with-output-to-file "../collects/mzlib/letplus.ss" 
      (lambda () (write (expand-defmacro e)))
      'replace)))

  
