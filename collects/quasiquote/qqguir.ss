(unit/sig quasiquote:graphical-interface^
  (import)

  (define display-image-stream
    (lambda (input-port stock-name)
      (let ((tmp-file-name
	      (build-path (current-directory)
		(string-append stock-name "."
		  (number->string (current-seconds))
		  ".gif"))))
	(let ((p (open-output-file tmp-file-name)))
	  (let loop ()
	    (let ((c (read-char input-port)))
	      (unless (eof-object? c)
		(display c p)
		(loop))))
	  (close-output-port p)
	  (close-input-port input-port)
	  (process (string-append "xv " tmp-file-name))))))

  )
