;; srpersist.ss

(unless (defined? 'odbc-version)
	(error "odbc-version not defined: should be inexact number greater than or equal to 1.0"))

(require-library "macro.ss")
(require-library "cores.ss")
(require-library "srpersistu.ss" "srpersist")

(cond

 [(>= odbc-version 3.5)
  (require-library "invoke-3.5.ss" "srpersist")]

 [(>= odbc-version 3.0)
  (require-library "invoke-3.0.ss" "srpersist")]

 [(>= odbc-version 2.0)
  (require-library "invoke-2.0.ss" "srpersist")]

 [(>= odbc-version 1.0)
  (require-library "invoke-1.0.ss" "srpersist")])
