(lambda (request)
  (case request
    [(name) "Startup Test"]
    [(app-unit-library) "tappinfo.ss"]
    [(app-sig-library) "tsig.ss"]
    [(splash-image-path) #f]
    [else (error 'test-A "Unknown request: ~s" request)]))
