(define balanced-tests
  `(("()" 0 2)
    ("(a)" 0 3)
    ("(a a)" 0 5)
    ("(())" 0 4)
    ("(())" 1 3)
    ("([])" 1 3)
    ("([])" 0 4)
    ("{[]}" 1 3)
    ("{[]}" 0 4)
    ("abc" 0 3)
    ("(abc (abc))" 0 11)
    ("(abc (abc))" 5 10)))

(define unbalanced-tests
  `(("()" #t (1) (1))
    ("(()" #f (0 2) (1 2))
    ("(a()" #f (0) (1 3))
    (")" #f (0 1) (0 1))
    ("())" #f (1 2) (1 3))
    ("() a)" #f (1 4) (1 5))))

(define (run-unbalanced-test test-data)
  (let ([expression (first test-data)]
	[balanced? (second test-data)]
	[forward-starts (third test-data)]
	[backward-starts (fourth test-data)])
    (test
     (string->symbol (format "unbalanced-paren-~a" expression))
     (lambda (x) (not (ormap (lambda (x) x) x)))
     `(let ([t (make-object text%)])
	(send t insert ,expression)
	(append
	 (list (not (eq? ,balanced? (scheme-paren:balanced? t 0 (send t last-position)))))
	 (map (lambda (n) (scheme-paren:forward-match t n (send t last-position))) ',forward-starts)
	 (map (lambda (n) (scheme-paren:backward-match t n 0)) ',backward-starts))))))

(define (run-balanced-test test-data)
  (let ([expression (first test-data)]
	[start (second test-data)]
	[end (third test-data)])
    (test
     (string->symbol (format "balanced-paren-~a/~a/~a" expression start end))
     (lambda (x) (equal? x (list start end #t)))
     `(let ([t (make-object text%)])
	(send t insert ,expression)
	(list (scheme-paren:backward-match t ,end 0)
	      (scheme-paren:forward-match t ,start (send t last-position))
	      (scheme-paren:balanced? t 0 (send t last-position)))))))

(define (run-scheme-unbalanced-test test-data)
  (let ([expression (first test-data)]
	[balanced? (second test-data)]
	[forward-starts (third test-data)]
	[backward-starts (fourth test-data)])
    (test
     (string->symbol (format "scheme-unbalanced-paren-~a" expression))
     (lambda (x) (not (ormap (lambda (x) x) x)))
     `(let* ([t (make-object scheme:text%)]
	     [setup-text
	      (lambda ()
		(send t erase)
		(send t insert ,(string-append " " expression)))]
	     [insert-first
	      (lambda ()
		(send t insert " " 0 0))]
	     [delete-first
	      (lambda ()
		(send t delete 0 1))])
	(append
	 (map
	  (lambda (n)
	    (setup-text)
	    (send t get-backward-sexp (+ n 1))
	    (delete-first)
	    (send t get-backward-sexp n))
	  ',backward-starts)
	 (map
	  (lambda (n)
	    (setup-text)
	    (send t get-backward-sexp (+ n 1))
	    (insert-first)
	    (send t get-backward-sexp (+ n 2)))
	  ',backward-starts)
	 (map
	  (lambda (n)
	    (setup-text)
	    (send t get-forward-sexp (+ n 1))
	    (delete-first)
	    (send t get-forward-sexp n))
	  ',forward-starts)
	 (map
	  (lambda (n)
	    (setup-text)
	    (send t get-forward-sexp (+ n 1))
	    (insert-first)
	    (send t get-forward-sexp (+ n 2)))
	  ',forward-starts))))))

(define (run-scheme-balanced-test test-data)
  (let* ([expression (first test-data)]
	 [start (second test-data)]
	 [end (third test-data)]
	 [answers (list start (+ start 2) end (+ end 2))])
    (test
     (string->symbol (format "balanced-paren-~a/~a" expression answers))
     (lambda (x) (equal? x answers))
     `(let* ([t (make-object scheme:text%)]
	     [setup-text
	      (lambda ()
		(send t erase)
		(send t insert ,(string-append " " expression)))]
	     [insert-first
	      (lambda ()
		(send t insert " " 0 0))]
	     [delete-first
	      (lambda ()
		(send t delete 0 1))])
	(list (begin (setup-text)
		     (send t get-backward-sexp ,(+ end 1))
		     (delete-first)
		     (send t get-backward-sexp ,end))
	      (begin (setup-text)
		     (send t get-backward-sexp ,(+ end 1))
		     (insert-first)
		     (send t get-backward-sexp ,(+ end 2)))
	      (begin (setup-text)
		     (send t get-forward-sexp ,(+ start 1))
		     (delete-first)
		     (send t get-forward-sexp ,start))
	      (begin (setup-text)
		     (send t get-forward-sexp ,(+ start 1))
		     (insert-first)
		     (send t get-forward-sexp ,(+ start 2))))))))

(for-each run-unbalanced-test unbalanced-tests)
(for-each run-scheme-unbalanced-test unbalanced-tests)
(for-each run-balanced-test balanced-tests)
(for-each run-scheme-balanced-test balanced-tests)
