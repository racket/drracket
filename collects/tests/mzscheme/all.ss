
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(load-relative "basic.ss")
(load-relative "read.ss")
(unless (defined? 'building-flat-tests)
  (load-relative "macro.ss"))
(load-relative "syntax.ss")
(load-relative "number.ss")
(load-relative "object.ss")
(load-relative "struct.ss")
(load-relative "unit.ss")
(load-relative "unitsig.ss")
(load-relative "thread.ss")
(unless (or (defined? 'read/zodiac)
	    (defined? 'in-drscheme?))
	(load-relative "namespac.ss"))
(unless (or (defined? 'building-flat-tests)
	    (defined? 'read/zodiac)
	    (defined? 'in-drscheme?))
   (load-relative "param.ss"))
(load-relative "file.ss")
(load-relative "path.ss")
(unless (defined? 'building-flat-tests)
   (load-relative "hashper.ss"))
(unless (or (defined? 'building-flat-tests)
	    (defined? 'read/zodiac)
	    (defined? 'in-drscheme?))
   (load-relative "optimize.ss"))
(unless (defined? 'building-flat-tests)
   (load-relative "name.ss"))

;; Ok, so this isn't really all of them. Here are more:
; thrport.ss
; deep.ss

; See also README
