
; Test MzLib
; See also pptest.ss and ztest.ss

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(load-relative "function.ss")

(load-relative "date.ss")

(load-relative "cmdline.ss")

(load-relative "pretty.ss")

(load-relative "pconvert.ss")

; Last - so macros are not present by accident
(load-relative "macrolib.ss")

(require-library "core.ss")
(test #t 'invoke-core-in-#%-space
      (begin
	(let ([l (require-library "corer.ss")])
	  (parameterize ([current-namespace (make-namespace 'hash-percent-syntax)])
			(invoke-unit/sig l)))
	#t))


(report-errs)
