(letrec ([drscheme-info
	  (lambda (request failure)
	    (case request
	      [(name) "DrScheme"]
	      [(mred-launcher-libraries) (list "drscheme.ss")]
	      [(mred-launcher-names) (list "DrScheme")]
	      [(compile-prefix) 
	       '(begin
		  (require-library "refer.ss")
		  (require-library "drsig.ss" "drscheme"))]
	      [(compile-omit-files)
	       (append
		(drscheme-info 'compile-elaboration-zos failure)
		(list "rep-new.ss" "phooks.ss" "toy.ss" ; should these files be deleted?
		      "rload.ss" "rrequire.ss"
		      "getcoll.ss" "tmp.ss" ;; these files are tmp files in robby's directory, not in cvs
		      "drscheme-in-drscheme.ss" "tool.ss" "rrequire.ss"))]
	      [(compile-subcollections) (list (list "drscheme" "tools" "syncheck"))]
	      [(compile-elaboration-zos-prefix)
	       '(begin
		  (require-library "refer.ss"))]
	      [(compile-elaboration-zos)
	       (list "drsig.ss")]
	      [else (failure)]))])
  drscheme-info)
