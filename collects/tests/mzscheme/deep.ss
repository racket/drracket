
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'deep)

; Test deep stacks

(define proc-depth 100000)

(test (- proc-depth) 'deep-recursion (let loop ([n proc-depth])
				       (if (zero? n)
					   0
					   (sub1 (loop (sub1 n))))))

(define paren-port
  (let* ([depth 50000]
	 [closing? #f]
	 [count depth])
    (make-input-port
     (lambda ()
       (cond
	[closing?
	 (if (= count depth)
	     eof
	     (begin
	       (set! count (add1 count))
	       #\) ))]
	[else
	 (set! count (sub1 count))
	 (when (zero? count)
	       (set! closing? #t))
	 #\(]))
     (lambda () #t)
     void)))

(define deep-list (read paren-port))

(test #t 'read-deep (pair? deep-list))

(define s (open-output-string))
(display deep-list s)

(test #t 'equal? (equal? deep-list (read (open-input-string (get-output-string s)))))

(report-errs)

