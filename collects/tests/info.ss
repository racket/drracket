(let ([sub-collections (list "framework")])
  (lambda (request result)
    (case request
      [(name) "Test Suites"]
      [(install-collection)
       (lambda (arg)
	(error-print-width 500)
	 (for-each (lambda (sub-collection)
		     (let ([sub-info (build-path (collection-path "tests" sub-collection) "info.ss")])
		       (when (file-exists? sub-info)
			 (((load-relative sub-info) 'install-collection void) arg))))
		   sub-collections))]
      [else (result)])))
