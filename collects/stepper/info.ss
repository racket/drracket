(lambda (request failure)
  (case request
    [(name) "The Foot"]
    [(compile-prefix) '(begin (require-library "sig.ss" "stepper")
                              (require-library "drsig.ss" "drscheme"))]
    [(compile-omit-files) '("beginner-checker.ss" 
                            "gui.ss"
                            "stepper-test.ss"
                            "stepper-testr.ss"
                            "sig.ss"
                            "big-annotater.ss")]
    [else (failure)]))