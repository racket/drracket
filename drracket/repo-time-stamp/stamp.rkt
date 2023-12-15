#lang racket/base
(require setup/dirs)
(provide stamp)
(define stamp
  (let ([stamp (get-build-stamp)])
    (and (not (equal? "" stamp)) stamp)))
