
(parameterize ([use-compiled-file-kinds 'none])
  (require-library "compile.ss" "compiler"))

(parameterize ([use-compiled-file-kinds 'none])
  (require-library "cmdline.ss")
  (require-relative-library "setupsig.ss")
  (require-library "invoke.ss"))

(define-values/invoke-unit/sig setup-option^
  (parameterize ([use-compiled-file-kinds 'none])
    (require-relative-library "setup-optionr.ss")))

(define-values (x-specific-collections x-archives)
  (command-line
   "setup-plt"
   argv
   (once-each
    [("-c" "--clean") "Delete existing compiled files"
		      (clean #t)]
    [("-n" "--no-zo") "Do not produce .zo files"
		      (make-zo #f)]
    [("-x" "--no-launcher") "Do not produce launcher programs"
			    (make-launchers #f)]
    [("-i" "--no-install") "Do not call collection-specific installers"
			   (call-install #f)]
    [("-e" "--extension") "Produce native code extensions"
			  (make-so #t)]
    [("-v" "--verbose") "See names of compiled files and info printfs"
			(verbose #t)]
    [("-m" "--make-verbose") "See make and compiler usual messages"
			     (make-verbose #t)]
    [("-r" "--compile-verbose") "See make and compiler verbose messages"
				(make-verbose #t)
				(compiler-verbose #t)]
    [("-p" "--pause") "Pause at the end if there are any errors"
		      (pause-on-errors #t)]
    [("-l") =>
	    (lambda (flag . collections)
	      (map list collections))
	    '("Setup specific <collection>s only" "collection")])
   (=>
    (lambda (collections . archives)
      (values (if (null? collections)
		  null
		  (car collections))
	      archives))
    '("archive")
    (lambda (s)
      (display s)
      (printf "If no <archive> or -l <collection> is specified, all collections are setup~n")
      (exit 0)))))

(specific-collections x-specific-collections)
(archives x-archives)

(parameterize ([use-compiled-file-kinds (if (clean) 'none (use-compiled-file-kinds))])
  (require-library "sig.ss" "compiler"))

(parameterize ([use-compiled-file-kinds (if (clean) 'none (use-compiled-file-kinds))])
  (invoke-unit/sig
   (compound-unit/sig
    (import (SOPTION : setup-option^))
    (link [STRING : mzlib:string^ ((require-library "stringr.ss"))]
	  [FILE : mzlib:file^ ((require-library "filer.ss") STRING FUNCTION)]
	  [FUNCTION : mzlib:function^ ((require-library "functior.ss"))]
	  [COMPILE : mzlib:compile^ ((require-library "compiler.ss"))]
	  [PRETTY-PRINT : mzlib:pretty-print^ ((require-library "prettyr.ss"))]
	  [LAUNCHER : launcher-maker^ ((require-library "launcherr.ss" "launcher") FILE)]
	  [DCOMPILE : dynext:compile^ ((require-library "compiler.ss" "dynext"))]
	  [DLINK : dynext:link^ ((require-library "linkr.ss" "dynext"))]
	  [DFILE : dynext:file^ ((require-library "filer.ss" "dynext"))]
	  [OPTION : compiler:option^ ((require-library "optionr.ss" "compiler"))]
	  [COMPILER : compiler^ ((require-library "compiler.ss" "compiler")
				 OPTION
				 FUNCTION
				 PRETTY-PRINT
				 FILE
				 STRING
				 COMPILE
				 DCOMPILE
				 DLINK
				 DFILE)]
	  [SETUP : () ((require-relative-library "setupr.ss")
		       SOPTION
		       FILE
		       COMPILER
		       OPTION
		       LAUNCHER)])
    (export))
   setup-option^))
