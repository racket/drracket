;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gann Bierner                                -=)
;; gbierner@owlnet.rice.edu                     (=-             \ O
;;                                                _O             \_-)---
;; File: pconveru.ss                           (=-_/             /\
;;                                                 /\
;;
;; This file contains code which formats an expression to show all sharing
;; within it. Call `print-convert' to convert a value.
;; It takes 2 optional arguments.  The first is a boolean value, just-circular.
;; If true, sharing will only be shown for circularity.  The default is #f
;; where all sharing is shown.  The first argument to share:print-all is, of
;; course, the expression to convert.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library "pconvers.ss")

(begin-elaboration-time
 (require-library "refer.ss"))

(define mzlib:print-convert@ (require-library-unit/sig "pconverr.ss"))
