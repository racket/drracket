(require-library "function.ss")
(require-library "file.ss")
(require-library "guis.ss" "tests" "utils")

(define-signature drscheme:test-util^
  (do-execute
   test-util-error
   poll-until
   wait-for-computation
   wait-for-drscheme-frame
   wait-for-new-frame
   clear-definitions
   type-in-definitions
   type-in-interactions
   wait
   wait-pending
   get-sub-panel
   get-text-pos
   wait-for-button
   push-button-and-wait
   set-language-level!
   repl-in-edit-sequence?
   fetch-output))

(invoke-unit/sig
 (compound-unit/sig 
   (import [fw : framework^]
	   [function : mzlib:function^]
	   [file : mzlib:file^]
	   [mred : mred^])
   (link
    [utils : test-utils:gui^ ((require-library "guir.ss" "tests" "utils") mred)]

    [drs-utils : drscheme:test-util^
	   ((require-library "drscheme-test-util.ss" "tests" "drscheme") mred fw utils)]
    [main : ()
	  ((unit/sig ()
	     (import [fw : framework^]
		     mzlib:function^
		     mzlib:file^
		     [drs-utils : drscheme:test-util^]
		     [utils : test-utils:gui^]
		     [mred : mred^])
	     
	     (define all-tests (map symbol->string (require-library "README" "tests" "drscheme")))
	     
	     (define (run-test test)
	       (unless (member test all-tests)
		 (error 'run-test "unknown test: ~a~n" test))
	       
	       (invoke-unit/sig
		(eval
		 `(unit/sig ()
		    (import [fw : framework^]
			    mzlib:function^
			    mzlib:file^
			    drscheme:test-util^
			    test-utils:gui^
			    mred^)
		    
		    (include ,(build-path (collection-path "tests" "drscheme") test))))
		(fw : framework^)
		mzlib:function^
		mzlib:file^
		(drs-utils : drscheme:test-util^)
		(utils : test-utils:gui^)
		(mred : mred^)))
	     
	     (lambda x
	       (for-each run-test (if (null? x) all-tests x))))
	   fw function file drs-utils utils mred)])
   (export))
 framework^
 mzlib:function^
 mzlib:file^
 mred^)

