(let ([sigfiles (list "sig.ss" "btrees.ss" "bullets.ss")])
  (lambda (request failure)
    (case request
      [(name) "Browser"]
      [(compile-prefix) `(begin
			   (require-library "sig.ss" "mred")
			   (require-library "sig.ss" "browser"))]
      [(compile-omit-files) sigfiles]
      [(compile-elaboration-zos) sigfiles]
      [else (failure)])))


