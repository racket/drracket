(let ([pred (lambda (x) (void? x))]
      [old-load-framework-automatically? (load-framework-automatically)])

  (load-framework-automatically #f)

  (test
   'macro.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "macro.ss" "framework")
      (global-defined-value 'mixin)
      (void)))
  (test
   'tests.ss
   (lambda (x) x)
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "tests.ss" "framework")
      (unit/sig? (require-library "keys.ss" "framework"))))
  (test
   'testr.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "tests.ss" "framework")
      (eval
       '(define-values/invoke-unit/sig
         ((unit test : framework:test^))
	 (compound-unit/sig
	   (import)
	   (link [mred : mred^ (mred@)]
		 [keys : framework:keys^ ((require-library "keys.ss" "framework"))]
		 [test : framework:test^ ((require-library "testr.ss" "framework") mred keys)])
	   (export (unit test)))))
      (global-defined-value 'test:run-one)
      (global-defined-value 'test:button-push)
      (void)))
  (test
   'test.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "test.ss" "framework")
      (global-defined-value 'test:run-one)
      (global-defined-value 'test:button-push)
      (void)))
  (test
   'frameworkr.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "frameworks.ss" "framework")
      (eval
       '(define-values/invoke-unit/sig
         framework^
	 (compound-unit/sig
	   (import)
	   (link [mred : mred^ (mred@)]
		 [core : mzlib:core^ ((require-library "corer.ss"))]
		 [framework : framework^ ((require-library "frameworkr.ss" "framework") core mred)])
	   (export (open framework)))))
      (global-defined-value 'test:run-one)
      (global-defined-value 'test:button-push)
      (global-defined-value 'frame:basic-mixin)
      (global-defined-value 'editor:basic-mixin)
      (global-defined-value 'exit:exit)
      (void)))
  (test
   'framework.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "framework.ss" "framework")
      (global-defined-value 'test:run-one)
      (global-defined-value 'test:button-push)
      (global-defined-value 'frame:basic-mixin)
      (global-defined-value 'editor:basic-mixin)
      (global-defined-value 'exit:exit)
      (void)))
  (test
   'framework.ss/gen
   (lambda (x) x)
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "pretty.ss")
      (let* ([op ((global-defined-value 'pretty-print-print-line))]
	     [np  (lambda x (apply op x))])
	((global-defined-value 'pretty-print-print-line) np)
	(require-library "framework.ss" "framework")
	(eq? np ((global-defined-value 'pretty-print-print-line))))))

  (load-framework-automatically old-load-framework-automatically?))

