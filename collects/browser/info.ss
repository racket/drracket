(lambda (request failure)
  (case request
    [(name) "Browser"]
    [(compile-prefix) `(begin
			 (require-library "sig.ss" "mred")
			 (require-library "sig.ss" "browser"))]
    [(compile-omit-files) (list "sig.ss")]
    [(compile-elaboration-zos) (list "sig.ss")]
    [else (failure)]))

