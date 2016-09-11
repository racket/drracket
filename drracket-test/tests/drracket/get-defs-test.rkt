#lang racket

(require drracket/private/get-defs
         racket/gui
         string-constants)

(define (get-definitions/string
         string
         #:define-prefix
         [define-prefix
           (list (define-popup-info "(define" "(define ...)" "δ"))])
  (define text (new text%))
  (send text insert string)
  (get-definitions define-prefix #f text))

(define-syntax (test-definitions stx)
  (syntax-case stx ()
    [(_ string stuff ... ((name start end) ...))
     #`(let ([actual (map (match-lambda [(defn _ n s e)
                                         (list n s e)])
                          (get-definitions/string string stuff ...))]
             [expected (list (list name start end) ...)])
         (unless (equal? actual expected)
           (eprintf "Test failure at ~a\nActual:   ~s\nExpected: ~s\n"
                    (format "~a:~a"
                            '#,(syntax-source stx)
                            #,(syntax-line stx))
                    actual
                    expected)))]))

(test-definitions 
 #<<END
(define x 1)
(define (f x)
  (define y x)
  y)
(define y 2)
(define
END
 (("x" 0 12)
  ; The end positions for f and the inner y look wrong to me.
  ; If they look wrong to you too (but you know what you're doing),
  ; please change the tests.
  ("f" 13 28) 
  ("y" 29 46)
  ("y" 47 59)
  ((string-constant end-of-buffer-define) 60 67)))

(test-definitions 
 #<<END
(define-metafunction L
  f : p -> q
  [(f p) t])
(define-metafunction L
  [(g p) t])
(define-metafunction/extension f L
  h : p -> q
  [(h p) t])
(define-metafunction/extension f L
  [(i p) t])
(define-metafunction
END
 (("f" 0 48)
  ("g" 49 84)
  ("h" 85 145)
  ("i" 146 193)
  ((string-constant end-of-buffer-define) 194 214)))

(test-definitions
 #<<END
(define-metafunction L
  f : p -> q
  [(f p) t])
(define-metafunction L
  [(g p) t])
(define-metafunction/extension f L
  h : p -> q
  [(h p) t])
(define-metafunction/extension f L
  [(i p) t])
(define-metafunction
END
 #:define-prefix
 (list (define-popup-info "(define" "(define ...)" "δ")
       (define-popup-info "(module" "(module ...)" "M"))
 (("f" 0 48)
  ("g" 49 84)
  ("h" 85 145)
  ("i" 146 193)
  ((string-constant end-of-buffer-define) 194 214)))

(test-definitions
 #<<END
(module m racket/base 1)
(module+ test 2)
(define a 1)
(define b 1)
END
 #:define-prefix
 (list (define-popup-info "(define" "(define ...)" "δ")
       (define-popup-info "(module" "(module ...)" "M"))
 (("m" 0 24) ("test" 25 41) ("a" 42 54) ("b" 55 67)))

(test-definitions
 #<<END
(module a racket/base 1)
(define b 1)
(module+ c 2)
(define d 1)
(module e 1)
(module f 2)
(define g 1)
END
 #:define-prefix
 (list (define-popup-info "(define" "(define ...)" "δ")
       (define-popup-info "(module" "(module ...)" "M"))
 (("a" 0 24) ("b" 25 37) ("c" 38 51) ("d" 52 64) ("e" 65 77) ("f" 78 90) ("g" 91 103)))


(test-definitions
 #<<END
(define a 1)
;;(define b 2)
END
 #:define-prefix
 (list (define-popup-info "(define" "(define ...)" "δ"))
 (("a" 0 27)))

(test-definitions
 #<<END
(define a 1)
;;(define b 2)
(define c 3)
END
 #:define-prefix
 (list (define-popup-info "(define" "(define ...)" "δ"))
 (("a" 0 27) ("c" 28 40)))

(test-definitions
 #<<END
(module a racket/base 1)
(define b 1)
;; (module+ c 2)
;; (define d 1)
;; (module e 1)
(module f 2)
(define g 1)
END
 #:define-prefix
 (list (define-popup-info "(define" "(define ...)" "δ")
       (define-popup-info "(module" "(module ...)" "M"))
 (("a" 0 24) ("b" 25 86) ("f" 87 99) ("g" 100 112)))

(test-definitions
 #<<END
(define a 1)
;;(define b 2)
(define c 3)
END
 #:define-prefix
 (list)
 ())
