(lambda (request response)
  (case request
    [(name) "Framework"]
    [(install-collection)
     (lambda (_)
       (require-library "launcher.ss" "launcher")
       (make-mred-launcher
	(list "-qe-"
	      "(require-library \"framework-test-engine.ss\" \"tests\" \"framework\")")
	(mred-program-launcher-path "Framework Test Engine"))
       (make-mzscheme-launcher
	(list "-mqve-" "(require-library \"main.ss\" \"tests\" \"framework\")")
	(mred-program-launcher-path
	 "Framework Test")))]))