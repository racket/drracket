(define-signature quasiquote:graphical-interface^
  (display-image-stream))

(define-signature quasiquote:quotester^
  (get-chart
    get-quote
    (struct entity (name))
    (struct stock ())
    (struct fund ())
    stock
    fund))
