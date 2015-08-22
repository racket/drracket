#lang racket/base
(require racket/list
         compiler/module-suffix)

(provide all-racket-suffixes)

;; all-racket-suffixes : (bytes -> string) string
;;  String-appends all Racket suffixes, including ".rtkd" and "rktl"
;;  into a string wit a given separator
(define (all-racket-suffixes handle-one sep)
  (apply
   string-append
   (add-between
    (for/list ([suffix (in-list (append
                                 (get-module-suffixes)
                                 '(#"rktd" #"rktl")))])
      (handle-one suffix))
    sep)))
