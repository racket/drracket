
(unit/sig dynext:compile^ (import)

  (define include-dir (collection-path "mzscheme" "include"))

  (define (get-unix-compile)
    (or (find-executable-path "gcc" "gcc")
	(find-executable-path "cc" "cc")))

  (define (get-windows-compile)
    (or (find-executable-path "cl.exe" "cl.exe")))

  (define current-extension-compiler 
    (make-parameter 
     #f 
     (lambda (v)
       (when v 
	     (if (and (string? v) (or (relative-path? v) (absolute-path? v)))
		 (unless (and (file-exists? v)
			      (memq 'execute (file-or-directory-permissions v)))
			 (error 'current-extension-compiler 
				"compiler not found or not executable: ~s" v))
		 (raise-type-error 'current-extension-compiler "pathname string or #f" v)))
       v)))

  (define current-extension-compiler-flags
    (make-parameter
     (case (system-type)
       [(unix) '("-c" "-O2")]
       [(windows) '("/c" "/O2")]
       [(macos) '()])
     (lambda (l)
       (unless (and (list? l) (andmap string? l))
	       (raise-type-error 'current-extension-compiler-flags "list of strings" l))
       l)))

  (define current-make-compile-include-strings
    (make-parameter
     (case (system-type)
       [(unix) (lambda (s) (list (string-append "-I" s)))]
       [(windows) (lambda (s) (list (string-append "/I" s)))]
       [(macos) (lambda (s) (list (string-append "-I" s)))])
     (lambda (p)
       (unless (procedure-arity-includes? p 1)
	       (raise-type-error 'current-make-compile-include-strings "procedure of arity 1" p))
       p)))

  (define current-make-compile-input-strings
    (make-parameter
     (lambda (s) (list s))
     (lambda (p)
       (unless (procedure-arity-includes? p 1)
	       (raise-type-error 'current-make-compile-input-strings "procedure of arity 1" p))
       p)))

  (define current-make-compile-output-strings
    (make-parameter
     (case (system-type)
       [(unix) (lambda (s) (list "-o" s))]
       [(windows) (lambda (s) (list (string-append "/Fo" s)))]
       [(macos) (lambda (s) (list "-o" s))])
     (lambda (p)
       (unless (procedure-arity-includes? p 1)
	       (raise-type-error 'current-make-compile-output-strings "procedure of arity 1" p))
       p)))

  (define-values (my-process* stdio-compile)
   (let-values ([(p* do-stdio) (require-library "stdio.ss" "mzscheme" "dynext")])
     (values
      p*
      (lambda (start-process quiet?)
	(do-stdio start-process quiet? (lambda (s) (error 'compile-extension "~a" s)))))))

  (define unix/windows-compile
    (lambda (quiet? in out includes)
      (let ([c (or (current-extension-compiler) 
		   (if (eq? (system-type) 'unix) 
		       (get-unix-compile) 
		       (get-windows-compile)))])
	(if c
	    (stdio-compile (lambda (quiet?) 
			     (let ([command (append 
					     (list c)
					     (current-extension-compiler-flags)
					     (apply append 
						    (map 
						     (lambda (s) 
						       ((current-make-compile-include-strings) s)) 
						     includes))
					     ((current-make-compile-include-strings) include-dir)
					     ((current-make-compile-input-strings) in)
					     ((current-make-compile-output-strings) out))])
			       (unless quiet? 
				       (printf "compile-extension: ~a~n" command))
			       (apply my-process* command)))
			   quiet?)
	    (error 'compile-extension "can't find compiler")))))

  (define (macos-compile quiet? input-file output-file)
    (error 'compile-extension "Not yet supported for MacOS"))
  
  (define compile-extension
    (case (system-type)
      [(unix windows) unix/windows-compile]
      [(macos) macos-compile])))
