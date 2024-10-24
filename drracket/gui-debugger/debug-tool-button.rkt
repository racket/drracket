#lang racket/base
(require racket/class 
         string-constants/string-constant
         images/compile-time
         images/icons/tool
         (for-syntax racket/base images/icons/tool images/icons/style))
(provide debug-drracket-button
         debug-bitmap
         small-debug-bitmap
         debug-callback)

(define-local-member-name debug-callback)

(define debug-bitmap (compiled-bitmap (debugger-icon)))
(define small-debug-bitmap (compiled-bitmap (small-debugger-icon)))

(define debug-drracket-button
  (list 
   (string-constant debug-tool-button-name)
   debug-bitmap
   (λ (drs-frame) (send drs-frame debug-callback))))

