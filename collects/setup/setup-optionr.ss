
(unit/sig setup-option^
  (import)

  (define verbose (make-parameter #f))
  (define make-verbose (make-parameter #f))
  (define compiler-verbose (make-parameter #f))
  (define clean (make-parameter #f))
  (define make-zo (make-parameter #t))
  (define make-so (make-parameter #f))
  (define make-launchers (make-parameter #t))
  (define call-install (make-parameter #t))
  (define pause-on-errors (make-parameter #f))

  (define specific-collections (make-parameter null))
  (define archives (make-parameter null)))



