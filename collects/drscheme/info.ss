(letrec ([drscheme-info
	  (lambda (request failure)
	    (case request
	      [(name) "DrScheme"]
	      [(clean) '("compiled")]
	      [(mred-launcher-libraries) (list "drscheme.ss")]
	      [(mred-launcher-names) (list "DrScheme")]
	      [(compile-prefix) 
	       '(begin
		  (require-library "refer.ss")
		  (require-library "drsig.ss" "drscheme"))]
              [(doc-sub-collections)
               (list "tools")]
	      [(compile-omit-files)
	       (append
		(drscheme-info 'compile-elaboration-zos failure)
		(list "rep-new.ss" "phooks.ss" "toy.ss" ; should these files be deleted?
		      "rload.ss" "rrequire.ss"
		      "getcoll.ss" "tmp.ss" ;; these files are tmp files in robby's directory, not in cvs
		      "launcher-bootstrap.ss"
                      "rrequire.ss"))]
	      [(compile-subcollections) 
               (map (lambda (x) (list "drscheme" "tools" x))
                    (filter
                     (lambda (x) 
                       (and (not (string-ci=? "CVS" x))
                            (file-exists? 
                             (build-path 
                              (collection-path "drscheme" "tools" x)
                              "info.ss"))))
                     (directory-list (collection-path "drscheme" "tools"))))]
	      [(compile-elaboration-zos-prefix)
	       '(begin
		  (require-library "refer.ss"))]
	      [(compile-elaboration-zos)
	       (list "load-handlers.ss" "drsig.ss")]
	      [else (failure)]))])
  drscheme-info)
