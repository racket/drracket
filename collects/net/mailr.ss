(unit/sig mzlib:sendmail^
  (import)

  (define-struct (no-mail-recipients struct:exn) ())

  (define sendmail-search-path
    '("/usr/lib" "/usr/sbin"))

  (define sendmail-program-file
    (if (eq? (system-type) 'unix)
      (let loop ((paths sendmail-search-path))
	(if (null? paths)
	  (raise (make-exn:misc:unsupported
		   "unable to find sendmail on this Unix variant"
		   (current-continuation-marks)))
	  (let ((p (build-path (car paths) "sendmail")))
	    (if (and (file-exists? p)
		  (memq 'execute (file-or-directory-permissions p)))
	      p
	      (loop (cdr paths))))))
      (raise (make-exn:misc:unsupported
	       "sendmail only available under Unix"
	       (current-continuation-marks)))))

  ;; send-mail-message/port :
  ;; string x string x list (string) x list (string) x list (string) 
  ;;   [x list (string)] -> oport

  ;; -- sender can be anything, though spoofing is not recommended.
  ;; The recipients must all be pure email addresses.  Note that
  ;; everything is expected to follow RFC conventions.  If any other
  ;; headers are specified, they are expected to be completely
  ;; formatted already.  Clients are urged to use close-output-port on
  ;; the port returned by this procedure as soon as the necessary text
  ;; has been written, so that the sendmail process can complete.

  (define send-mail-message/port 
    (lambda (sender subject to-recipients cc-recipients bcc-recipients
	      . other-headers)
      (when (and (null? to-recipients) (null? cc-recipients)
	      (null? bcc-recipients))
	(raise (make-no-mail-recipients
		 "no mail recipients were specified"
		 (current-continuation-marks))))
      (let ((return (apply process* sendmail-program-file "-i"
		      (append to-recipients cc-recipients bcc-recipients))))
	(let ((reader (car return))
	       (writer (cadr return))
	       (pid (caddr return))
	       (error-reader (cadddr return)))
	  (close-input-port reader)
	  (close-input-port error-reader)
	  (fprintf writer "From: ~a~n" sender)
	  (letrec ((write-recipient-header
		     (lambda (header-string recipients)
		       (let ((header-space
			       (+ (string-length header-string) 2)))
			 (fprintf writer "~a: " header-string)
			 (let loop ((to recipients) (indent header-space))
			   (if (null? to)
			     (newline writer)
			     (let ((first (car to)))
			       (let ((len (string-length first)))
				 (if (>= (+ len indent) 80)
				   (begin
				     (fprintf writer "~n    ~a, " first)
				     (loop (cdr to) (+ len header-space 2)))
				   (begin
				     (fprintf writer "~a, " first)
				     (loop (cdr to)
				       (+ len indent 2))))))))))))
	    (write-recipient-header "To" to-recipients)
	    (write-recipient-header "CC" cc-recipients))
	  (fprintf writer "Subject: ~a~n" subject)
	  (fprintf writer "X-Mailer: MzScheme: see www.cs.rice.edu/CS/PLT/~n")
	  (for-each (lambda (s)
		      (display s writer)
		      (newline writer))
	    other-headers)
	  (newline writer)
	  writer))))

  ;; send-mail-message :
  ;; string x string x list (string) x list (string) x list (string) x
  ;;   list (string) [x list (string)] -> ()

  ;; -- sender can be anything, though spoofing is not recommended.  The
  ;; recipients must all be pure email addresses.  The text is expected
  ;; to be pre-formatted.  Note that everything is expected to follow
  ;; RFC conventions.  If any other headers are specified, they are
  ;; expected to be completely formatted already.

  (define send-mail-message
    (lambda (sender subject to-recipients cc-recipients bcc-recipients text
	      . other-headers)
      (let ((writer (apply send-mail-message/port sender subject
		      to-recipients cc-recipients bcc-recipients
		      other-headers)))
	(for-each (lambda (s)
		    (display s writer)	; We use -i, so "." is not a problem
		    (newline writer))
	  text)
	(close-output-port writer))))

  )
