(lambda (request)
  (case request
    [(name) "DrScheme"]
    [(app-unit-library) "link.ss"]
    [(app-sig-library) "drsig.ss"]
    [(splash-image-path) (with-handlers ([void (lambda (x) "mred.gif")]) (build-path (collection-path "icons") "plt.gif"))]
    [(splash-max) 149]
    [else (error 'drscheme-info "Unknown request: ~s" request)]))
