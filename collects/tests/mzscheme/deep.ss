
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'deep)

; Test deep stacks

(define (nontail-loop n esc)
  (let loop ([n n])
    (if (zero? n)
	(esc 0)
	(sub1 (loop (sub1 n))))))

(define (time-it t)
  (let ([s (current-process-milliseconds)])
    (t)
    (- (current-process-milliseconds) s)))

(define (find-depth go)
  ; Find depth that triggers a stack overflow by looking
  ;  for an incongruous change in the running time.
  (let find-loop ([d 100][t (time-it (lambda () (go 100)))])
    (if (zero? t)
	(find-loop (* 2 d) (time-it (lambda () (go (* 2 d)))))
	(begin
	  ; (printf "~a in ~a~n" d t)
	  (let* ([d2 (* 2 d)]
		 [t2 (time-it (lambda () (go d2)))])
	    (if (> (/ t2 d2) (* 2.2 (/ t d)))
		d2
		(find-loop d2 t2)))))))

(define proc-depth (find-depth (lambda (n) (nontail-loop n (lambda (x) x)))))
(printf "non-tail loop overflows at ~a~n" proc-depth)

(test (- proc-depth) 'deep-recursion (nontail-loop proc-depth (lambda (x) x)))

(test 0 'deep-recursion-escape/ec
      (let/ec k
	(nontail-loop proc-depth k)))

(test 0 'deep-recursion-escape/cc
      (let/cc k
	(nontail-loop proc-depth k)))

(define (read-deep depth)
  (define paren-port
    (let* ([depth depth]
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
  (read paren-port))

(define read-depth (find-depth read-deep))
(printf "nested paren read overflows at ~a~n" read-depth)

(define deep-list (read-deep read-depth))

(test #t 'read-deep (pair? deep-list))

(define s (open-output-string))
(display deep-list s)
(test 'ok 'display 'ok)

(test #t 'equal? (equal? deep-list (read (open-input-string (get-output-string s)))))

(define going? #t)
(define (equal?-forever l1 l2)
  (let ([t (thread (lambda () 
		     (equal? l1 l2) ; runs forever; could run out of memory
		     (set! going? #f)))])
    (sleep 1)
    (kill-thread t)
    going?))


(define l1 (cons 0 #f))
(set-cdr! l1 l1)
(define l2 (cons 0 #f))
(set-cdr! l2 l2)
(test #t 'equal?-forever (equal?-forever l1 l2))

(define l1 (cons 0 #f))
(set-car! l1 l1)
(define l2 (cons 0 #f))
(set-car! l2 l2)
(test #t 'equal?-forever/memory (equal?-forever l1 l2))

(define l1 (vector 0))
(vector-set! l1 0 l1)
(define l2 (vector 0))
(vector-set! l2 0 l2)
(test #t 'equal?-forever/vector (equal?-forever l1 l2))

(define-struct a (b c))
(define l1 (make-a 0 #f))
(set-a-b! l1 l1)
(define l2 (make-a 0 #f))
(set-a-b! l2 l2)
(test #t 'equal?-forever/struct (equal?-forever l1 l2))

(define l1 (box 0))
(set-box! l1 l1)
(define l2 (box 0))
(set-box! l2 l2)
(test #t 'equal?-forever/struct (equal?-forever l1 l2))

(test #t 'equal?-forever/struct (call-in-nested-thread (lambda () (equal?-forever l1 l2))))

(report-errs)
