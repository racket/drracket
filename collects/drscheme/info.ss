(letrec ([drscheme-info
	  (lambda (request failure)
	    (case request
	      [(name) "DrScheme"]
	      [(app-unit-library) "link.ss"]
	      [(app-sig-library) "drsig.ss"]
	      [(splash-image-path) (with-handlers ([void (lambda (x) "mred.gif")]) 
				     (build-path (collection-path "icons") "plt.gif"))]
	      [(splash-max) 138]
	      [(splash-depth) 6]
	      [(compile-prefix) 
	       '(begin
		  (read-case-sensitive #t)
		  (require-library "refer.ss")
		  (require-library "wxs.ss" "system")
		  (require-library "invsig.ss" "system")
		  (require-library "sig.ss" "mred")
		  (require-library "debug.ss" "system")
		  (require-library "drsig.ss" "drscheme")
		  (require-library "hierlists.ss" "hierlist"))]
	      [(compile-omit-files)
	       (append
		(drscheme-info 'compile-elaboration-zos failure)
		(list "rep-new.ss" "phooks.ss" "toy.ss" ; should these files be deleted?
		      "tool.ss"))]
	      [(compile-subcollections) (list (list "tools" "syncheck")
					      (list "tools" "analysis"))]
	      [(compile-elaboration-zos-prefix)
	       '(begin
		  (read-case-sensitive #t)
		  (require-library "refer.ss")
		  (require-library "wxs.ss" "system")
		  (require-library "invsig.ss" "system")
		  (require-library "sig.ss" "mred")
		  (require-library "debug.ss" "system"))]
	      [(compile-elaboration-zos)
	       (list "drsig.ss")]
	      [else (failure)]))])
  drscheme-info)
