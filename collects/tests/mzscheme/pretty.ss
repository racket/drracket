
; Test pretty-print; relies on personal inspection of the results

(require-library "pretty.ss")

(define-struct s (a b c))

(define (make k?)
  (let ([make (if k? make (lambda (x) '(end)))])
    (list
     1
     'a
     "a"
     (list 'long-name-numero-uno-one-the-first-supreme-item
	   'long-name-number-two-di-ar-ge-second-line)
     (map (lambda (v v2)
	    (make-s v 2 v2))
	  (make #f)
	  (reverse (make #f)))
     '(1)
     '(1 2 3)
     '(1 . 2)
     #(1 2 3 4 5)
     '(#0=() . #0#)
     '#1=(1 . #1#)
     (map box (make #f))
     (make #f))))

(define vs (make #t))

(define print-line-no
  (lambda (line port offset width) 
    (if line         
	(begin
	  (when (positive? line) (write-char #\newline port))
	  (fprintf port "~s~a~a~a " line
		   (if (< line 10) " " "")
		   (if (< line 100) " " "")
		   (if (< line 1000) " " ""))
	  5)
	(fprintf port "!~n"))))

(define modes
  (list
   (list "DEPTH=2" pretty-print-depth 2)
   (list "GRAPH-ON" print-graph #t)
   (list "STRUCT-ON" print-struct #t)
   (list "LINE-NO-ON" pretty-print-print-line print-line-no)))

(define num-combinations (arithmetic-shift 1 (length modes)))

(let loop ([n 0])
  (when (< n num-combinations)
     (let loop ([modes modes][n n])
       (cond
	[(null? modes) (printf ":~n") (map pretty-print vs)]
	[(positive? (bitwise-and n 1))
	 (let ([mode (car modes)])
	   (printf "~s " (car mode))
	   (parameterize ([(cadr mode) (caddr mode)])
	      (loop (cdr modes) (arithmetic-shift n -1))))]
	[else
	 (loop (cdr modes) (arithmetic-shift n -1))]))
     (loop (add1 n))))
