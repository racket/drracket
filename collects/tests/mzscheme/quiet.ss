
(unless (defined? 'quiet-load)
   (define quiet-load "all.ss"))

(let ([p (make-output-port void void)])
  (parameterize ([current-output-port p])
      (load-relative quiet-load))
  (report-errs))

