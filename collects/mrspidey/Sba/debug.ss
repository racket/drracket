;; debugging.ss

(define debugging #f)
(define debugging-front #f)
(define debugging-traverse #f)
(define debugging-object #f)
(define debugging-unit #f)
(define debugging-check #f)
(define debugging-atenv #f)
(define debugging-atype #f)
(define debugging-sdl #f)
(define debugging-sdl2 #f)
(define debugging-gui #t)

(define debugging-min #f)
(define debugging-min2 #f)
(define debugging-few #f)
(define debugging-gram #f)
(define debugging-dfa-min #f)
(define debugging-min-table #f)

(define timing-min #f)

(define pretty-print-debug pretty-print)

(define (set-debug-flag which val)
  (case which
    [(general)  (set! debugging val)]
    [(min)      (set! debugging-min val)]
    [(front)    (set! debugging-front val)]
    [(traverse) (set! debugging-traverse val)]))

;(set! debugging #t)
;(set! debugging-min #t)
;(set! debugging-gram #t)
;(set! debugging-min-table #t)
;(set! timing-min #t)
