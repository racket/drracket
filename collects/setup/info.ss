
(lambda (request failure)
  (case request
    [(name) "Setup PLT"]
    [(compile-prefix) `(begin
			 (require-library "refer.ss")
			 (require-library "setupsig.ss" "setup"))]
    [(compile-omit-files) (list "setup.ss" "setupsig.ss")]
    [(compile-elaboration-zos) (list "setupsig.ss")]
    [(mzscheme-launcher-libraries) (list "setup.ss")]
    [(mzscheme-launcher-names) (list "Setup PLT")]
    [else (failure)]))
