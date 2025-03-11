#lang racket/base

;; Assume that the first directory in `use-compiled-file-paths` on
;; startup is the right place to put subdirectories for other modes.
;; It might be "compiled", but it might be something like
;; "compiled/ta6osx" depending on the Racket VM and build mode.

(provide compiled-dir)

(define compiled-dir
  (let ([l (use-compiled-file-paths)])
    (if (null? l)
        "compiled"
        (car l))))
