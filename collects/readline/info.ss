(lambda (request failure-thunk)
  (case request
    [(name) "readline"]
    [(install-collection)
     (lambda (path)
       (parameterize ([current-namespace (make-namespace)]
		      [current-directory (build-path path "collects" "readline")])
	 (global-defined-value 'argv #())
	 (load "mzmake.ss")))]
    [else (failure-thunk)]))
