(lambda (request)
  (case request
    [(name) "DrScheme"]
    [(app-unit-library) "link.ss"]
    [(app-sig-library) "drsig.ss"]
    [(splash-image-path) (with-handlers ([void (lambda (x) "mred.gif")]) (build-path (collection-path "icons") "plt.gif"))]
    [(splash-max) 138]
    [(splash-depth) 6]
    [(compile-prefix) '(begin
			 (read-case-sensitive #t)
			 (require-library "wxs.ss" "system")
			 (require-library "sig.ss" "mred")
			 (require-library "debug.ss" "system")
			 (require-library "drsig.ss" "drscheme"))]
    [(compile-omit-files)
     (list "drsig.ss"
	   "phooks.ss")]
    [else (error 'drscheme-info "Unknown request: ~s" request)]))
