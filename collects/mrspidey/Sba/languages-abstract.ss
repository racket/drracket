;; ======================================================================

(define (init-R4RS!)
  ;; Also extends it with void

  (apply add-default-constructor! (type: sym) (type: (listof bool)))
  (add-default-primitive! (type: (list sym sexp))))

;; ======================================================================

(define (init-Chez-on-R4RS!) (void))
(define (init-Rice-on-Chez!) (void))
(define (init-MzScheme-on-R4RS!) (void))
(define (init-DrScheme-on-MzScheme!) (void))
(define (init-smart-numops!) (void))
