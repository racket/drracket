
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'READING)
(define readstr
  (lambda (s)
    (let* ([o (open-input-string s)]
	   [read (if (defined? 'read/zodiac)
		     (let ([r (read/zodiac (open-input-string s))])
		       (lambda ()
			 (let ([orig (error-escape-handler )])
			   (dynamic-wind
			    (lambda () (error-escape-handler 
					(lambda ()
					  (error-escape-handler orig)
					  (error 'read/zodiac))))
			    r
			    (lambda () (error-escape-handler orig))))))
		     (lambda () (read o)))])
      (let loop ([last eof])
	(let ([v (read)])
	  (if (eof-object? v)
	      last
	      (loop v)))))))

(define readerrtype
  (if (defined? 'read/zodiac)
      (lambda (x) (lambda (y) #t))
      (lambda (x) x)))

; Make sure {whitespace} == {delimiter}
(let ([with-censor (load-relative "censor.ss")])
  (with-censor
   (lambda ()
     (let loop ([n 0])
       (unless (= n 256)
	       (let* ([c0 (integer->char n)]
		      [c (if (read-case-sensitive)
			     c0
			     (char-downcase c0))])
		 (cond
		  [(char-whitespace? c)
		   (test 'b readstr (string #\a c #\b))]
		  [(char=? #\\ c) (test 'ab readstr (string #\a c #\b))]
		  [(char=? #\; c) (test 'a readstr (string #\a c #\b))]
		  [(char=? #\' c) (test ''b readstr (string #\a c #\b))]
		  [(char=? #\` c) (test '`b readstr (string #\a c #\b))]
		  [(char=? #\, c) (test ',b readstr (string #\a c #\b))]
		  [else
		   (test (string->symbol (string #\a (char-downcase c) #\b))
			 'readstr
			 (with-handlers ([void 
					  (lambda (x) 
					    (string->symbol (string #\a (char-downcase c) #\b)))])
			      (readstr (string #\a c #\b))))]))
	       (loop (add1 n)))))))

(error-test '(readstr ")") (readerrtype exn:read?))
(error-test '(readstr "[)") (readerrtype exn:read?))
(error-test '(readstr "[}") (readerrtype exn:read?))
(error-test '(readstr "8 )") (readerrtype exn:read?))
(error-test '(readstr "(8 . )") (readerrtype exn:read?))

(load-relative "numstrs.ss")
(let loop ([l number-table])
  (unless (null? l)
	  (let* ([pair (car l)]
		 [v (car pair)]
		 [s (cadr pair)])
	    (cond
	     [(eq? v 'X) (error-test `(readstr ,s) (readerrtype exn:read?))]
	     [v (test v readstr s)]
	     [else (test (string->symbol s) readstr s)]))
	  (loop (cdr l))))

(error-test '(readstr "#\\silly") (readerrtype exn:read?))
(error-test '(readstr "#\\nully") (readerrtype exn:read?))
(error-test '(readstr "#\\nu") (readerrtype exn:read?))
(error-test '(readstr "#\\733") (readerrtype exn:read?))
(error-test '(readstr "#\\433") (readerrtype exn:read?))

(error-test '(readstr "(hi") (readerrtype exn:read:eof?))
(error-test '(readstr "\"hi") (readerrtype exn:read:eof?))
(error-test '(readstr "#(hi") (readerrtype exn:read:eof?))
(error-test '(readstr "#4(hi") (readerrtype exn:read:eof?))
(error-test '(readstr "|hi") (readerrtype exn:read:eof?))
(error-test '(readstr "#\\") (readerrtype exn:read:eof?))
(error-test '(readstr "#| hi") (readerrtype exn:read:eof?))

(error-test '(readstr ".") (readerrtype exn:read?))
(error-test '(readstr "a .") (readerrtype exn:read?))
(error-test '(readstr "a . b") (readerrtype exn:read?))
(error-test '(readstr "( . )") (readerrtype exn:read?))
(error-test '(readstr "( . 8)") (readerrtype exn:read?))
(error-test '(readstr "(0 . 8 9)") (readerrtype exn:read?))
(error-test '(readstr "( . 8 9)") (readerrtype exn:read?))
(error-test '(readstr "#(8 . )") (readerrtype exn:read?))
(error-test '(readstr "#( . )") (readerrtype exn:read?))
(error-test '(readstr "#( . 8)") (readerrtype exn:read?))
(error-test '(readstr "#(0 . 8 9)") (readerrtype exn:read?))
(error-test '(readstr "#( . 8 9)") (readerrtype exn:read?))
(error-test '(readstr "#( 8 . 9)") (readerrtype exn:read?))
(error-test '(readstr "#( 8 . (9))") (readerrtype exn:read?))

(error-test '(readstr "#Q") (readerrtype exn:read?))
(error-test '(readstr "##") (readerrtype exn:read?))
(error-test '(readstr "#?") (readerrtype exn:read?))
(error-test '(readstr "#-1()") (readerrtype exn:read?))
(error-test '(readstr "#<a>") (readerrtype exn:read?))

(test 2 vector-length (readstr "#2()"))
(test 0 vector-ref (readstr "#2()") 1)
(test 2 vector-length (readstr "#000000000000000000000000000000002()"))

(error-test '(readstr "#2(1 2 3)") (readerrtype exn:read?))
(error-test '(readstr "#200000000000(1 2 3)") (readerrtype exn:misc:out-of-memory?))

(unless (defined? 'read/zodiac)
  (test #t (lambda (x) (eq? (car x) (cdr x))) (readstr "(#0=(1 2) . #0#)"))
  (test #t (lambda (x) (eq? (car x) (cdr x))) (readstr "(#1=(1 2) . #0001#)")))

(error-test '(readstr "#0#") (readerrtype exn:read?))
(error-test '(readstr "#0=#0#") (readerrtype exn:read?))
(error-test '(readstr "(#0# #0=7)") (readerrtype exn:read?))
(error-test '(readstr "(#0=7 #1#)") (readerrtype exn:read?))
(error-test '(readstr "#012345678=7") (readerrtype exn:read?))
(error-test '(readstr "(#12345678=7 #012345678#)") (readerrtype exn:read?))

(test 3 string-length (readstr (string #\" #\a #\nul #\b #\")))
(test (string->symbol (string #\a #\nul #\b)) 'sym (readstr (string #\a #\nul #\b)))
(test (string->symbol (string #\1 #\nul #\b)) 'sym (readstr (string #\1 #\nul #\b)))

; Test read/write invariance on symbols and use of pipe quotes
(define (test-write-sym with-bar without-bar s)
  (let ([sym (string->symbol s)])
    (parameterize ([read-case-sensitive #t])
		  (let ([p (open-output-string)])
		    (write sym p)
		    (test with-bar 'write-sym-with-bar (get-output-string p))
		    (test sym read (open-input-string (get-output-string p))))
		  (let ([p (open-output-string)])
		    (parameterize ([read-accept-bar-quote #f])
				  (write sym p)
				  (test without-bar 'write-sym-no-bar (get-output-string p))
				  (test sym read (open-input-string (get-output-string p)))))
		  (let ([p (open-output-string)])
		    (display sym p)
		    (test s 'display-sym (get-output-string p))))))

(test-write-sym "a->b" "a->b" "a->b")
(test-write-sym "|a,b|" "a\\,b" "a,b")
(test-write-sym "a\\|b" "a|b" "a|b")
(test-write-sym "|a\\b|" "a\\\\b" "a\\b")

(load-relative "numstrs.ss")
(let loop ([l number-table])
  (cond
   [(null? l) 'done]
   [(or (number? (caar l)) (eq? (caar l) 'X))
    (test-write-sym (string-append "|" (cadar l) "|") 
		    (string-append "\\" (cadar l)) 
		    (cadar l))
    (loop (cdr l))]
   [else 
    (test-write-sym (cadar l) (cadar l) (cadar l))
    (loop (cdr l))]))

(report-errs)
