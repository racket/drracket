(let ([userspace-info
       (lambda (what failure)
	 (case what
	   [(name) "Userspace"]
	   [(compile-prefix) 
	    '(begin
	       (require-library "refer.ss")
	       (require-library "coreflats.ss")
	       (when (with-handlers ([void (lambda (x) #f)])
		       (collection-path "mred"))
		 (require-library "turtles.ss" "graphics")
		 (require-library "sig.ss" "mred"))
	       (require-library "errors.ss" "userspce")
	       (require-library "params.ss" "userspce")
	       (require-library "sig.ss" "userspce"))]
	   [(compile-omit-files) (list "userspcs.ss" "sig.ss" "errors.ss" "params.ss" "ricedefs.ss")]
	   [(compile-elaboration-zos) (list)]
	   [else (failure)]))])
  userspace-info)
