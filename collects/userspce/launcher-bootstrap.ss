;; set things up so that the load-handler opens files into
;; a text when the file begins with WXME so that mred saved
;; files still load properly.

(require-library "core.ss" "drscheme-jr")

((make-go
  (if (defined? 'mred@)
      (load-relative "launcher-bootstrap-mred.ss")
      (load-relative "launcher-bootstrap-mzscheme.ss"))))
