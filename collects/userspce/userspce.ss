; require this file within MrEd to install into the top-level
;  the bindings normally available to a DrScheme library

(require-library "userspcs.ss" "userspce")
(require-library "params.ss" "userspce")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig ((open plt:userspace:params^)
				(open plt:userspace^))
  (compound-unit/sig 
   (import)
   (link [p : plt:userspace:params^ ((require-relative-library "paramr.ss"))]
	 [u : plt:userspace^ ((require-relative-library "userspcr.ss") p)])
   (export (open p)
	   (open u))))
