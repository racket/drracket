; Time-stamp: <98/05/08 22:29:05 shriram>

; * Need to make write-holdings-to-file set permissions appropriately.
; * add-{stock,fund} should check if the entry already exists.
; * Allow update of holdings.
; * Print numbers in columns.
; * Improve output quality and media.
; * Enable queries on individual holdings.

;; Format of RC file:
;;   current-seconds (when file was last written)
;;   ((entity quantity price) ...)
;;   <eof>
;; where entity = (stock "...") or (fund "...")

(require-library "match.ss")
(require-library "date.ss")

(require-library "qq.ss" "quasiquote")

(define rc-file "~/.qqrc")

;; entity : entity
;; quantity : num
;; price : num

(define-struct holding (entity quantity price))

;; raw-holding->holding :
;; raw-holding -> holding

(define raw-holding->holding
  (lambda (rh)
    (match rh
      ((('stock name) quantity price)
	(make-holding (stock name) quantity price))
      ((('fund name) quantity price)
	(make-holding (fund name) quantity price))
      (else (error 'qq-client "~s is an invalid entry in the database" rh)))))

;; holding->raw-holding :
;; holding -> raw-holding

(define holding->raw-holding
  (lambda (h)
    (list
      (let ((entity (holding-entity h)))
	(cond
	  ((stock? entity) `(stock ,(entity-name entity)))
	  ((fund? entity) `(fund ,(entity-name entity)))
	  (else
	    (error 'qq-client "~s is not a valid entity" entity))))
      (holding-quantity h)
      (holding-price h)))) 

;; write-holdings-to-file :
;; list (holding) -> ()

(define write-holdings-to-file
  (lambda (holdings)
    (let ((p (open-output-file rc-file 'replace)))
      (display "; -*- Scheme -*-" p)
      (newline p) (newline p)
      (display "; Do not edit directly: please use QuasiQuote clients!" p)
      (newline p) (newline p)
      (write (current-seconds) p)
      (newline p) (newline p)
      (write (map holding->raw-holding holdings) p)
      (newline p)
      (close-output-port p))))

;; read-holdings-from-file :
;; () -> (seconds + #f) x list (holding)

(define read-holdings-from-file
  (lambda ()
    (with-handlers ((exn:i/o:filesystem? (lambda (exn)
					   (values #f null))))
      (let ((p (open-input-file rc-file)))
	(values (read p)
	  (map raw-holding->holding 
	    (read p)))))))

;; update-holdings :
;; list (holding) -> list (holding)

(define update-holdings
  (lambda (holdings)
    (map (lambda (h)
	   (let ((entity (holding-entity h)))
	     (let ((new-value (get-quote entity)))
	       (make-holding entity (holding-quantity h) new-value))))
      holdings)))

;; changed-positions :
;; list (holding) x list (holding) ->
;; list (holding . num) x list (holding . num) x list (holding)

(define changed-positions
  (lambda (old-in new-in)
    (let loop ((old old-in) (new new-in)
		(increases null) (decreases null) (stays null))
      (if (and (null? old) (null? new))
	(values increases decreases stays)
	(if (or (null? old) (null? new))
	  (error 'qq-client "~s and ~s cannot be compared for changes"
	    old-in new-in)
	  (let ((first-old (car old)) (first-new (car new)))
	    (if (string=? (entity-name (holding-entity first-old))
		  (entity-name (holding-entity first-new)))
	      (let* ((price-old (holding-price first-old))
		      (price-new (holding-price first-new))
		      (difference (- price-new price-old)))
		(cond
		  ((= price-old price-new)
		    (loop (cdr old) (cdr new)
		      increases
		      decreases
		      (cons first-new stays)))
		  ((< price-old price-new)
		    (loop (cdr old) (cdr new)
		      (cons (cons first-new difference) increases)
		      decreases
		      stays))
		  (else
		    (loop (cdr old) (cdr new)
		      increases
		      (cons (cons first-new difference) decreases)
		      stays))))
	      (error 'qq-client "~s and ~s are in the same position"
		first-old first-new))))))))

;; total-value :
;; list (holding) -> num

(define total-value
  (lambda (holdings)
    (apply +
      (map (lambda (h)
	     (* (holding-quantity h) (holding-price h)))
	holdings))))

;; print-position-changes :
;; list (holding . num) x list (holding . num) x list (holding) -> ()

(define print-position-changes
  (lambda (increases decreases stays)
    (define print-entry/change
      (lambda (holding change)
	(printf "~a    ~a    ~a~a~n"
	  (entity-name (holding-entity holding))
	  (holding-price holding)
	  (if (> change 0) "+" "-")
	  (abs change))))
    (define print-change
      (lambda (banner changes)
	(unless (null? changes)
	  (printf "~a:~n" banner))
	(for-each (lambda (h+delta)
		    (print-entry/change (car h+delta) (cdr h+delta)))
	  changes)
	(newline)))
    (print-change "Increases" increases)
    (print-change "Decreases" decreases)))

;; print-statement :
;; () -> ()

(define print-statement
  (lambda ()
    (let-values (((old-time old-holdings)
		   (read-holdings-from-file)))
      (let ((new-holdings (update-holdings old-holdings)))
	(when old-time
	  (printf "Changes are since ~a~n~n"
	    (date->string (seconds->date old-time))))
	(let-values (((increases decreases stays)
		       (changed-positions old-holdings new-holdings)))
	  (print-position-changes increases decreases stays))
	(let ((old-total (total-value old-holdings))
	       (new-total (total-value new-holdings)))
	  (printf "Total change: ~a~nTotal value: ~a~n"
	    (- new-total old-total) new-total))
	(write-holdings-to-file new-holdings)))))

;; create-holding :
;; (str -> entity) -> str x num -> holding

(define create-holding
  (lambda (maker)
    (lambda (name quantity)
      (let ((entity (maker name)))
	(let ((price (get-quote entity)))
	  (make-holding entity quantity price))))))

;; create-holding/stock :
;; str x num -> holding

(define create-holding/stock
  (create-holding stock))

;; create-holding/fund :
;; str x num -> holding

(define create-holding/fund
  (create-holding fund))

;; add-holding :
;; (str x num -> holding) -> x str x num -> ()

(define add-holding
  (lambda (maker)
    (lambda (name quantity)
      (let-values (((old-time old-holdings)
		     (read-holdings-from-file)))
	(write-holdings-to-file
	  (cons (maker name quantity)
	    old-holdings))))))

;; add-stock :
;; str x num -> ()

(define add-stock
  (add-holding create-holding/stock))

;; add-fund :
;; str x num -> ()

(define add-fund
  (add-holding create-holding/fund))
