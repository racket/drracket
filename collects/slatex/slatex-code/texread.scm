;texread.scm
;SLaTeX v. 2.3
;Various token-readers used on TeX files by SLaTeX
;(c) Dorai Sitaram, Rice U., 1991, 1994

(eval-within slatex 

  (define slatex::eat-till-newline
    (lambda (in)
      ;skip all characters from port in till newline inclusive or eof
      (let loop ()
	(let ((c (read-char in)))
	  (cond ((eof-object? c) 'done)
		((char=? c #\newline) 'done)
		(else (loop)))))))

  (define slatex::read-ctrl-seq
    (lambda (in)
      ;assuming we've just read a backslash, read the remaining
      ;part of a latex control sequence from port in
      (let ((c (read-char in)))
	(if (eof-object? c)
	    (error "read-ctrl-exp: \\ followed by eof."))
	(if (char-alphabetic? c)
	    (list->string
	     (reverse!
	      (let loop ((s (list c)))
		(let ((c (peek-char in)))
		  (cond ((eof-object? c) s)
			((char-alphabetic? c) (read-char in)
					      (loop (cons c s)))
			((char=? c #\%) (eat-till-newline in)
					(loop s))
			(else s))))))
	    (string c)))))

  (define slatex::eat-tabspace
    (lambda (in)
      ;skip to the next non-space and non-tab character from port in
      (let loop ()
	(let ((c (peek-char in)))
	  (cond ((eof-object? c) 'done)
		((or (char=? c #\space) (char=? c *tab*))
		 (read-char in) (loop))
		(else 'done))))))

  (define slatex::eat-whitespace
    (lambda (in)
      ;skip to the next whitespace character from port in
      (let loop ()
	(let ((c (peek-char in)))
	  (cond ((eof-object? c) 'done)
		((char-whitespace? c)
		 (read-char in) (loop))
		(else 'done))))))

  (define slatex::eat-tex-whitespace
    (lambda (in)
      ;skip to the next whitespace character from port in;
      ;skips past latex comments too
      (let loop ()
	(let ((c (peek-char in)))
	  (cond ((eof-object? c) 'done)
		((char-whitespace? c) (read-char in) (loop))
		((char=? c #\%) (eat-till-newline in))
		(else 'done))))))

  (define slatex::chop-off-whitespace
    (lambda (l)
      ;removes leading whitespace from character-list l
      (ormapcdr (lambda (d) (if (char-whitespace? (car d)) #f d)) l)))

  (define slatex::read-grouped-latexexp
    (lambda (in)
      ;reads a latex grouped expression from port in
      ;(removes the groups)
      (eat-tex-whitespace in)
      (let ((c (read-char in)))
	(if (eof-object? c) (error "read-grouped-latexexp: ~
Expected { but found eof."))
	(if (not (char=? c #\{))
	    (error "read-grouped-latexexp: ~
Expected { but found ~a." c))
	(eat-tex-whitespace in)
	(list->string
	 (reverse!
	  (chop-off-whitespace
	   (let loop ((s '()) (nesting 0) (escape? #f))
	     (let ((c (read-char in)))
	       (if (eof-object? c) (error "read-groupted-latexexp: ~
Found eof inside {...}."))
	       (cond (escape? (loop (cons c s) nesting #f))
		     ((char=? c #\\)
		      (loop (cons c s) nesting #t))
		     ((char=? c #\%) (eat-till-newline in)
				     (loop s nesting #f))
		     ((char=? c #\{)
		      (loop (cons c s) (+ nesting 1) #f))
		     ((char=? c #\})
		      (if (= nesting 0) s
			  (loop (cons c s) (- nesting 1) #f)))
		     (else
		      (loop (cons c s) nesting #f)))))))))))

  (define slatex::read-filename
    (let ((filename-delims (list #\{ #\} #\[ #\] #\( #\) #\# #\% #\\ #\,
				 #\space *return* #\newline *tab* #\\)))
      (lambda (in)
	;reads a filename as allowed in latex syntax from port in
	(eat-tex-whitespace in)
	(let ((c (peek-char in)))
	  (if (eof-object? c) (error "read-filename: ~
Expected filename but found eof."))
	  (if (char=? c #\{) (read-grouped-latexexp in)
	      (list->string
	       (reverse!
		(let loop ((s '()) (escape? #f))
		  (let ((c (peek-char in)))
		    (cond ((eof-object? c)
			   (if escape? (error "read-filename: ~
\\ followed by eof.")
			       s))
			  (escape? (read-char in)
				   (loop (cons c s) #f))
			  ((char=? c #\\) (read-char in)
					  (loop (cons c s) #t))
			  ((memv c filename-delims) s)
			  (else (read-char in)
				(loop (cons c s) #f))))))))))))

  (define slatex::read-schemeid
    (let ((schemeid-delims (list #\{ #\} #\[ #\] #\( #\)
				 #\space *return* #\newline *tab*)))
      (lambda (in)
	;reads a scheme identifier from port in
	(eat-whitespace in)
	(list->string
	 (reverse!
	  (let loop ((s '()) (escape? #f))
	    (let ((c (peek-char in)))
	      (cond ((eof-object? c) s)
		    (escape? (read-char in) (loop (cons c s) #f))
		    ((char=? c #\\) (read-char in)
				    (loop (cons c s) #t))
		    ((memv c schemeid-delims) s)
		    (else (read-char in) (loop (cons c s) #f))))))))))

  (define slatex::read-delimed-commaed-filenames
    (lambda (in lft-delim rt-delim)
      ;reads a filename from port in, assuming it's delimited by
      ;lft- and rt-delims
      (eat-tex-whitespace in)
      (let ((c (read-char in)))
	(if (eof-object? c) (error "read-delimed-commaed-filenames: ~
Expected filename(s) but found eof."))
	(if (not (char=? c lft-delim))
	    (error "read-delimed-commaed-filenames: ~
Left delimiter ~a not found." lft-delim))
	(let loop ((s '()))
	  (eat-tex-whitespace in)
	  (let ((c (peek-char in)))
	    (if (eof-object? c) (error "read-delimed-commaed-filenames: ~
Found eof inside filename(s)."))
	    (if (char=? c rt-delim)
		(begin (read-char in) (reverse! s))
		(let ((s (cons (read-filename in) s)))
		  (eat-tex-whitespace in)
		  (let ((c (peek-char in)))
		    (if (eof-object? c)
			(error "read-delimed-commaed-filenames: ~
Found eof inside filename(s)."))
		    (cond
		     ((char=? c #\,) (read-char in))
		     ((char=? c rt-delim) (void))
		     (else (error "read-delimed-commaed-filenames: ~
Bad filename(s) syntax.")))
		    (loop s)))))))))

  (define slatex::read-grouped-commaed-filenames
    (lambda (in)
      ;read a filename from port in, assuming it's grouped
      (read-delimed-commaed-filenames in #\{ #\})))

  (define slatex::read-bktd-commaed-filenames
    (lambda (in)
      ;read a filename from port in, assuming it's bracketed
      (read-delimed-commaed-filenames in #\[ #\])))

  (define slatex::read-grouped-schemeids
    (lambda (in)
      ;read a list of scheme identifiers from port in,
      ;assuming they're all grouped
      (eat-tex-whitespace in)
      (let ((c (read-char in)))
	(if (eof-object? c) (error "read-grouped-schemeids: ~
Expected Scheme identifiers but found eof."))
	(if (not (char=? c #\{)) (error "read-grouped-schemeids: ~
Expected { but found ~a." c))
	(let loop ((s '()))
	  (eat-whitespace in)
	  (let ((c (peek-char in)))
	    (if (eof-object? c) (error "read-grouped-schemeids:
Found eof inside Scheme identifiers."))
	    (if (char=? c #\})
		(begin (read-char in) (reverse! s))
		(loop (cons (read-schemeid in) s))))))))

  (define slatex::eat-delimed-text
    (lambda (in lft-delim rt-delim)
      (eat-tex-whitespace in)
      (let ((c (peek-char in)))
	(if (eof-object? c) 'exit
	    (if (char=? c lft-delim)
		(let loop ()
		  (let ((c (read-char in)))
		    (if (eof-object? c) 'exit
			(if (char=? c rt-delim) 'exit
			    (loop))))))))))

  (define slatex::eat-bktd-text
    (lambda (in)
      (eat-delimed-text in #\[ #\])))

  (define slatex::eat-grouped-text
    (lambda (in)
      (eat-delimed-text in #\{ #\})))

  ;(trace read-filename)
  )