#lang racket
(require mred/mred-sig
         net/tcp-sig
         net/url
         net/url-sig
         racket/gui
         racket/unit
         setup/plt-installer
         setup/plt-installer-sig
         "browser-sig.rkt"
         "browser-unit.rkt")

(provide-signature-elements browser^)

(define-values/invoke-unit/infer browser@)
