
(unit/sig mzlib:dns^
  (import)

  (define types
    '((a 1)
      (ns 2)
      (md 3)
      (mf 4)
      (cname 5)
      (soa 6)
      (mb 7)
      (mg 8)
      (mr 9)
      (null 10)
      (wks 11)
      (ptr 12)
      (hinfo 13)
      (minfo 14)
      (mx 15)
      (txt 16)))

  (define classes
    '((in 1)
      (cs 2)
      (ch 3)
      (hs 4)))

  (define (cossa i l)
    (cond
     [(null? l) #f]
     [(equal? (cadar l) i)
      (car l)]
     [else (cossa i (cdr l))]))
  

  (define (number->octet-pair n)
    (list (integer->char (arithmetic-shift n -8))
	  (integer->char (modulo n 256))))

  (define (octet-pair->number a b)
    (+ (arithmetic-shift (char->integer a) 8)
       (char->integer b)))

  (define (octet-quad->number a b c d)
    (+ (arithmetic-shift (char->integer a) 24)
       (arithmetic-shift (char->integer b) 16)
       (arithmetic-shift (char->integer c) 8)
       (char->integer d)))

  (define (name->octets s)
    (let ([do-one (lambda (s)
		    (cons
		     (integer->char (string-length s))
		     (string->list s)))])
      (let loop ([s s])
	(let ([m (regexp-match "^([^.]*)[.](.*)" s)])
	  (if m
	      (append
	       (do-one (cadr m))
	       (loop (caddr m)))
	      (append
	       (do-one s)
	       (list #\nul)))))))

  (define (make-std-query-header id question-count)
    (append
     (number->octet-pair id)
     (list #\001 #\nul) ; Opcode & flags (recusive flag set)
     (number->octet-pair question-count)
     (number->octet-pair 0)
     (number->octet-pair 0)
     (number->octet-pair 0)))

  (define (make-query id name type class)
    (append
     (make-std-query-header id 1)
     (name->octets name)
     (number->octet-pair (cadr (assoc type types)))
     (number->octet-pair (cadr (assoc class classes)))))

  (define (add-size-tag m)
    (append (number->octet-pair (length m)) m))

  (define (rr-data rr)
    (cadddr (cdr rr)))

  (define (rr-type rr)
    (cadr rr))

  (define (rr-name rr)
    (car rr))

  (define (parse-name start reply)
    (let ([v (char->integer (car start))])
      (cond
       [(zero? v)
	; End of name
	(values #f (cdr start))]
       [(zero? (bitwise-and #xc0 v))
	; Normal label
	(let loop ([len v][start (cdr start)][accum null])
	  (cond
	   [(zero? len) 
	    (let-values ([(s start) (parse-name start reply)])
	      (let ([s0 (list->string (reverse! accum))])
		(values (if s
			    (string-append s0 "." s)
			    s0)
			start)))]
	   [else (loop (sub1 len) (cdr start) (cons (car start) accum))]))]
       [else
	; Compression offset
	(let ([offset (+ (arithmetic-shift (bitwise-and #x3f v) 8)
			 (char->integer (cadr start)))])
	  (let-values ([(s ignore-start) (parse-name (list-tail reply offset) reply)])
	    (values s (cddr start))))])))

  (define (parse-rr start reply)
    (let-values ([(name start) (parse-name start reply)])
      (let ([type (car (cossa (octet-pair->number (car start) (cadr start)) types))]
	    [start (cddr start)])
	(let ([class (car (cossa (octet-pair->number (car start) (cadr start)) classes))]
	      [start (cddr start)])
	  (let ([ttl (octet-quad->number (car start) (cadr start)
					 (caddr start) (cadddr start))]
		[start (cddddr start)])
	    (let ([len (octet-pair->number (car start) (cadr start))]
		  [start (cddr start)])
	      ; Extract next len bytes for data:
	      (let loop ([len len][start start][accum null])
		(if (zero? len)
		    (values (list name type class ttl (reverse! accum))
			    start)
		    (loop (sub1 len) (cdr start) (cons (car start) accum))))))))))

  (define (parse-ques start reply)
    (let-values ([(name start) (parse-name start reply)])
      (let ([type (car (cossa (octet-pair->number (car start) (cadr start)) types))]
	    [start (cddr start)])
	(let ([class (car (cossa (octet-pair->number (car start) (cadr start)) classes))]
	      [start (cddr start)])
	  (values (list name type class) start)))))

  (define (parse-n parse start reply n)
    (let loop ([n n][start start][accum null])
      (if (zero? n)
	  (values (reverse! accum) start)
	  (let-values ([(rr start) (parse start reply)])
	    (loop (sub1 n) start (cons rr accum))))))

  (define (dns-query nameserver addr type class)
    (unless (assoc type types)
      (raise-type-error 'dns-query "DNS query type" type))
    (unless (assoc class classes)
      (raise-type-error 'dns-query "DNS query class" class))

    (let* ([query (make-query (random 256) addr type class)]
	   [reply
	    (let-values ([(r w) (tcp-connect nameserver 53)])
	      (dynamic-wind
	       void
	       
	       (lambda ()
		 (display (list->string (add-size-tag query)) w)
		 (flush-output w)
		 
		 (let ([a (read-char r)]
		       [b (read-char r)])
		   (let ([len (octet-pair->number a b)])
		     (let ([s (read-string len r)])
		       (unless (= len (string-length s))
			 (error 'dns-query "unexpected EOF from server"))
		       (string->list s)))))
	       
	       (lambda ()
		 (close-input-port r)
		 (close-output-port w))))])

      ; First two bytes must match sent message id:
      (unless (and (char=? (car reply) (car query))
		   (char=? (cadr reply) (cadr query)))
	(error 'dns-query "bad reply id from server"))

      (let ([v0 (caddr reply)]
	    [v1 (cadddr reply)])
	; Check for error code:
	(let ([rcode (bitwise-and #xf (char->integer v1))])
	  (unless (zero? rcode)
	    (error 'dns-query "error from server: ~a"
		   (case rcode
		     [(1) "format error"]
		     [(2) "server failure"]
		     [(3) "name error"]
		     [(4) "not implemented"]
		     [(5) "refused"]))))
	
	(let ([qd-count (octet-pair->number (list-ref reply 4) (list-ref reply 5))]
	      [an-count (octet-pair->number (list-ref reply 6) (list-ref reply 7))]
	      [ns-count (octet-pair->number (list-ref reply 8) (list-ref reply 9))]
	      [ar-count (octet-pair->number (list-ref reply 10) (list-ref reply 11))])
	  
	  (let ([start (list-tail reply 12)])
	    (let*-values ([(qds start) (parse-n parse-ques start reply qd-count)]
			  [(ans start) (parse-n parse-rr start reply an-count)]
			  [(nss start) (parse-n parse-rr start reply ns-count)]
			  [(ars start) (parse-n parse-rr start reply ar-count)])
	      (unless (null? start)
		(error 'dns-query "error parsing server reply"))
	      (values (positive? (bitwise-and #x4 (char->integer v0)))
		      qds ans nss ars reply)))))))

  (define cache (make-hash-table))
  (define (dns-query/cache nameserver addr type class)
    (let ([key (string->symbol (format "~a;~a;~a;~a" nameserver addr type class))])
      (let ([v (hash-table-get cache key (lambda () #f))])
	(if v
	    (apply values v)
	    (let-values ([(auth? qds ans nss ars reply) (dns-query nameserver addr type class)])
	      (hash-table-put! cache key (list auth? qds ans nss ars reply))
	      (values auth? qds ans nss ars reply))))))

  (define (ip->string s)
    (format "~a.~a.~a.~a" 
	    (char->integer (list-ref s 0))
	    (char->integer (list-ref s 1))
	    (char->integer (list-ref s 2))
	    (char->integer (list-ref s 3))))

  (define (try-forwarding k nameserver)
    (let loop ([nameserver nameserver][tried (list nameserver)])
      ; Normally the recusion is done for us, but it's technically optional
      (let-values ([(v ars auth?) (k nameserver)])
	(or v
	    (and (not auth?)
		 (let* ([ns (ormap
			     (lambda (ar)
			       (and (eq? (rr-type ar) 'a)
				    (ip->string (rr-data ar))))
			     ars)])
		   (and ns
			(not (member ns tried))
			(loop ns (cons ns tried)))))))))

  (define (dns-get-address nameserver addr)
    (or (try-forwarding
	 (lambda (nameserver)
	   (let-values ([(auth? qds ans nss ars reply) (dns-query/cache nameserver addr 'a 'in)])
	     (values (and (positive? (length ans))
			  (let ([s (rr-data (car ans))])
			    (ip->string s)))
		     ars auth?)))
	 nameserver)
	(error 'dns-get-address "bad address")))

  (define (dns-get-mail-exchanger nameserver addr)
    (or (try-forwarding
	 (lambda (nameserver)
	   (let-values ([(auth? qds ans nss ars reply) (dns-query/cache nameserver addr 'mx 'in)])
	     (values (let loop ([ans ans][best-pref +inf.0][exchanger #f])
		       (cond
			[(null? ans) (or exchanger
					 ;; Does 'soa mean that the input address is fine?
					 (and (ormap
					       (lambda (ns) (eq? (rr-type ns) 'soa))
					       nss)
					      addr))]
			[else
			 (let ([d (rr-data (car ans))])
			   (let ([pref (octet-pair->number (car d) (cadr d))])
			     (if (< pref best-pref)
				 (let-values ([(name start) (parse-name (cddr d) reply)])
				   (loop (cdr ans) pref name))
				 (loop (cdr ans) best-pref exchanger))))]))
		     ars auth?)))
	 nameserver)
	(error 'dns-get-mail-exchanger "bad address")))

  (define (dns-find-nameserver)
    (case (system-type)
      [(unix) (with-handlers ([void (lambda (x) #f)])
		(with-input-from-file "/etc/resolv.conf"
		  (lambda ()
		    (let loop ()
		      (let ([l (read-line)])
			(or (and (string? l)
				 (let ([m (regexp-match 
					   (format "nameserver[ ~a]+([0-9]+[.][0-9]+[.][0-9]+[.][0-9]+)" #\tab)
					   l)])
				   (and m (cadr m))))
			    (and (not (eof-object? l))
				 (loop))))))))]
      [else #f])))
