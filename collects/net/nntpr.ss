; Time-stamp: <98/07/14 14:41:20 shriram>
; Time-stamp: <97/03/05 15:34:09 shriram>

(unit/sig mzlib:nntp^
  (import)

  ; sender : oport
  ; receiver : iport
  ; server : string
  ; port : number

  (define-struct communicator (sender receiver server port))

  ; code : number
  ; text : string
  ; line : string
  ; communicator : communicator
  ; group : string
  ; article : number

  (define-struct (nntp struct:exn) ())
  (define-struct (unexpected-response struct:nntp) (code text))
  (define-struct (bad-status-line struct:nntp) (line))
  (define-struct (premature-close struct:nntp) (communicator))
  (define-struct (bad-newsgroup-line struct:nntp) (line))
  (define-struct (non-existent-group struct:nntp) (group))
  (define-struct (article-not-in-group struct:nntp) (article))
  (define-struct (no-group-selected struct:nntp) ())
  (define-struct (article-not-found struct:nntp) (article))

  ; signal-error :
  ; (exn-args ... -> exn) x format-string x values ... ->
  ;   exn-args -> ()

  ; - throws an exception

  (define signal-error
    (lambda (constructor format-string . args)
      (lambda exn-args
	(raise (apply constructor
		 (apply format format-string args)
		 (current-continuation-marks)
		 exn-args)))))

  ; default-nntpd-port-number :
  ; number

  (define default-nntpd-port-number 119)

  ; connect-to-server :
  ; string [x number] -> commnicator

  (define connect-to-server
    (opt-lambda (server-name (port-number default-nntpd-port-number))
      (let-values (((receiver sender)
		     (tcp-connect server-name port-number)))
	(let ((communicator
		(make-communicator sender receiver server-name port-number)))
	  (let-values (((code response)
			 (get-single-line-response communicator)))
	    (case code
	      ((200)
		communicator)
	      (else
		((signal-error make-unexpected-response
		   "unexpected connection response: ~s ~s"
		   code response)
		  code response))))))))

  ; close-communicator :
  ; communicator -> ()

  (define close-communicator
    (lambda (communicator)
      (close-input-port (communicator-receiver communicator))
      (close-output-port (communicator-sender communicator))))

  ; disconnect-from-server :
  ; communicator -> ()

  (define disconnect-from-server
    (lambda (communicator)
      (send-to-server communicator "QUIT")
      (let-values (((code response)
		     (get-single-line-response communicator)))
	(case code
	  ((205)
	    (close-communicator communicator))
	  (else
	    ((signal-error make-unexpected-response
	       "unexpected dis-connect response: ~s ~s"
	       code response)
	      code response))))))

  ; send-to-server :
  ; communicator x format-string x list (values) -> ()

  (define send-to-server
    (lambda (communicator message-template . rest)
      (apply fprintf (communicator-sender communicator)
	(string-append message-template "~n")
	rest)))

  ; parse-status-line :
  ; string -> number x string

  (define parse-status-line
    (let ((pattern (regexp "([0-9]+) (.*)")))
      (lambda (line)
	(let ((match (cdr (or (regexp-match pattern line)
			    ((signal-error make-bad-status-line
			       "malformed status line: ~s" line)
			      line)))))
	  (values (string->number (car match))
	    (cadr match))))))

  ; get-one-line-from-server :
  ; iport -> string

  (define get-one-line-from-server
    (lambda (server->client-port)
      (read-line server->client-port 'return-linefeed)))

  ; get-single-line-response :
  ; communicator -> number x string

  (define get-single-line-response
    (lambda (communicator)
      (let ((receiver (communicator-receiver communicator)))
	(let ((status-line (get-one-line-from-server receiver)))
	  (parse-status-line status-line)))))

  ; get-rest-of-multi-line-response :
  ; communicator -> list (string)

  (define get-rest-of-multi-line-response
    (lambda (communicator)
      (let ((receiver (communicator-receiver communicator)))
	(let loop ()
	  (let ((l (get-one-line-from-server receiver)))
	    (cond
	      ((eof-object? l)
		((signal-error make-premature-close
		   "port prematurely closed during multi-line response")
		  communicator))
	      ((string=? l ".")
		'())
	      ((string=? l "..")
		(cons "." (loop)))
	      (else
		(cons l (loop)))))))))

  ; get-multi-line-response :
  ; communicator -> number x string x list (string)

  ; -- The returned values are the status code, the rest of the status
  ; response line, and the remaining lines.

  (define get-multi-line-response
    (lambda (communicator)
      (let ((receiver (communicator-receiver communicator)))
	(let ((status-line (get-one-line-from-server receiver)))
	  (let-values (((code rest-of-line)
			 (parse-status-line status-line)))
	    (values code rest-of-line (get-rest-of-multi-line-response)))))))

  ; open-news-group :
  ; communicator x string -> number x number x number

  ; -- The returned values are the number of articles, the first
  ; article number, and the last article number for that group.

  (define open-news-group
    (let ((pattern (regexp "([0-9]+) ([0-9]+) ([0-9]+)")))
      (lambda (communicator group-name)
	(send-to-server communicator "GROUP ~a" group-name)
	(let-values (((code rest-of-line)
		       (get-single-line-response communicator)))
	  (case code
	    ((211)
	      (let ((match (map string->number
			     (cdr
			       (or (regexp-match pattern rest-of-line)
				 ((signal-error make-bad-newsgroup-line
				    "malformed newsgroup open response: ~s"
				    rest-of-line)
				   rest-of-line))))))
		(let ((number-of-articles (car match))
		       (first-article-number (cadr match))
		       (last-article-number (caddr match)))
		  (values number-of-articles
		    first-article-number
		    last-article-number))))
	    ((411)
	      ((signal-error make-non-existent-group
		 "group ~s does not exist on server ~s"
		 group-name (communicator-server communicator))
		group-name))
	    (else
	      ((signal-error make-unexpected-response
		 "unexpected group opening response: ~s" code)
		code rest-of-line)))))))

  ; head/body-of-message :
  ; string x number -> communicator x number -> list (string)

  (define head/body-of-message
    (lambda (command ok-code)
      (lambda (communicator message-number)
	(send-to-server communicator (string-append command " ~a")
	  (number->string message-number))
	(let-values (((code response)
		       (get-single-line-response communicator)))
	  (if (= code ok-code)
	    (get-rest-of-multi-line-response communicator)
	    (case code
	      ((423)
		((signal-error make-article-not-in-group
		   "article number ~s not in group" message-number)
		  message-number))
	      ((412)
		((signal-error make-no-group-selected
		   "no group selected")))
	      ((430)
		((signal-error make-article-not-found
		   "no article number ~s found" message-number)
		  message-number))
	      (else
		((signal-error make-unexpected-response
		   "unexpected message access response: ~s" code)
		  code response))))))))

  ; head-of-message :
  ; communicator x number -> list (string)

  (define head-of-message
    (head/body-of-message "HEAD" 221))

  ; body-of-message :
  ; communicator x number -> list (string)

  (define body-of-message
    (head/body-of-message "BODY" 222))

  ; make-desired-header :
  ; string -> desired

  (define make-desired-header
    (lambda (raw-header)
      (regexp
	(string-append
	  "^"
	  (list->string
	    (apply append
	      (map (lambda (c)
		     (cond
		       ((char-lower-case? c)
			 (list #\[ (char-upcase c) c #\]))
		       ((char-upper-case? c)
			 (list #\[ c (char-downcase c) #\]))
		       (else
			 (list c))))
		(string->list raw-header))))
	  ":"))))

  ; extract-desired-headers :
  ; list (string) x list (desired) -> list (string)

  (define extract-desired-headers
    (lambda (headers desireds)
      (let loop ((headers headers))
	(if (null? headers) null
	  (let ((first (car headers))
		 (rest (cdr headers)))
	    (if (ormap (lambda (matcher)
			 (regexp-match matcher first))
		  desireds)
	      (cons first (loop rest))
	      (loop rest)))))))

  )
