(lambda (request failure)
  (case request
    [(name) "stepper"]
    [(compile-prefix) '(begin (require-library "sig.ss" "stepper")
                              (require-library "drsig.ss" "drscheme"))]
    [(compile-omit-files) '("test.ss" "testr.ss" "sig.ss")]
    [else (failure)]))