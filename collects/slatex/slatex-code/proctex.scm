;proctex.scm
;SLaTeX v. 2.4
;Implements SLaTeX's piggyback to LaTeX
;(c) Dorai Sitaram, Rice U., 1991, 1999

(eval-if (cl)
  (eval-within slatex
    (defun ignore2 (i ii)
      (declare (ignore i ii))
      (values))))

(eval-unless (cl)
  (eval-within slatex
    (define slatex::ignore2
      (lambda (i ii)
	;ignores its two arguments
	'void))))

(eval-within slatex
	
  (defvar slatex::version-number "2.4w")

  (define slatex::disable-slatex-temply
    (lambda (in)
      ;tell slatex that it should not process slatex commands till
      ;the enabling control sequence is called
      (set! *slatex-enabled?* #f)
      (set! *slatex-reenabler* (read-grouped-latexexp in))))

  (define slatex::enable-slatex-again
    (lambda ()
      ;tell slatex to resume processing slatex commands
      (set! *slatex-enabled?* #t)
      (set! *slatex-reenabler* "UNDEFINED")))

  (define slatex::add-to-slatex-db
    (lambda (in categ)
      ;some scheme identifiers to be added to the token category categ
      (if (memq categ '(keyword constant variable))
	  (slatex::add-to-slatex-db-basic in categ)
	  (slatex::add-to-slatex-db-special in categ))))

  (define slatex::add-to-slatex-db-basic
    (lambda (in categ)
      ;read the following scheme identifiers and add them to the
      ;token category categ
      (let ((setter (cond ((eq? categ 'keyword) (function set-keyword))
			  ((eq? categ 'constant) (function set-constant))
			  ((eq? categ 'variable) (function set-variable))
			  (else (error "add-to-slatex-db-basic: ~
Unknown category ~s." categ))))
	    (ids (read-grouped-schemeids in)))
	(for-each setter ids))))

  (define slatex::add-to-slatex-db-special
    (lambda (in what)
      ;read the following scheme identifier(s) and either
      ;enable/disable its special-symbol status
      (let ((ids (read-grouped-schemeids in)))
	(cond ((eq? what 'unsetspecialsymbol)
	       (for-each (function unset-special-symbol) ids))
	      ((eq? what 'setspecialsymbol)
	       (if (not (= (length ids) 1))
		   (error "add-to-slatex-db-special: ~
\\setspecialsymbol takes one arg exactly."))
	       (let ((transl (read-grouped-latexexp in)))
		 (set-special-symbol (car ids) transl)))
	      (else (error "add-to-slatex-db-special: ~
Unknown command ~s." what))))))

  (define slatex::process-slatex-alias
    (lambda (in what which)
      ;add/remove a slatex control sequence name
      (let ((triggerer (read-grouped-latexexp in)))
	(case which
	  ((intext)
	   (set! *intext-triggerers*
		 (funcall what triggerer *intext-triggerers*
			  (function string=?))))
	  ((resultintext)
	   (set! *resultintext-triggerers*
		 (funcall what triggerer *resultintext-triggerers*
			  (function string=?))))
	  ((display)
	   (set! *display-triggerers*
		 (funcall what triggerer *display-triggerers*
			  (function string=?))))
	  ((response)
	   (set! *response-triggerers*
		 (funcall what triggerer *response-triggerers*
			  (function string=?))))
	  ((respbox)
	   (set! *respbox-triggerers*
		 (funcall what triggerer *respbox-triggerers*
			  (function string=?))))
	  ((box)
	   (set! *box-triggerers*
		 (funcall what triggerer *box-triggerers*
			  (function string=?))))
	  ((input)
	   (set! *input-triggerers*
		 (funcall what triggerer *input-triggerers*
			  (function string=?))))
	  ((region)
	   (set! *region-triggerers*
		 (funcall what triggerer *region-triggerers*
			  (function string=?))))
	  ((mathescape)
	   (if (not (= (string-length triggerer) 1))
	       (error "process-slatex-alias: ~
Math escape should be character."))
	   (set! *math-triggerers*
		 (funcall what (string-ref triggerer 0)
			  *math-triggerers* (function char=?))))
	  (else (error "process-slatex-alias:
Unknown command ~s." which))))))

  (define slatex::decide-latex-or-tex
    (lambda (latex?)
      ;create a junk file if the file is in plain tex rather
      ;than latex; this is used afterward to call the right
      ;command, i.e., latex or tex
      (set! *latex?* latex?)
      (let ((pltexchk.jnk "pltexchk.jnk"))
	(if (file-exists? pltexchk.jnk) (delete-file pltexchk.jnk))
	(if (not *latex?*)
	    (call-with-output-file pltexchk.jnk
	      (lambda (outp)
		(display 'junk outp)
		(newline outp)))))))

  (define slatex::process-include-only
    (lambda (in)
      ;remember the files mentioned by \includeonly
      (set! *include-onlys* '())
      (for-each
       (lambda (filename)
	 (let ((filename (full-texfile-name filename)))
	   (if filename
	       (set! *include-onlys*
		     (adjoin filename *include-onlys*
			     (function string=?))))))
       (read-grouped-commaed-filenames in))))

  (define slatex::process-documentstyle
    (lambda (in)
      ;process the .sty files corresponding to the documentstyle options
      (eat-tex-whitespace in)
      (if (char=? (peek-char in) #\[)
	  (for-each
	   (lambda (filename)
	     (fluid-let ((*slatex-in-protected-region?* #f))
	       (slatex::process-tex-file
		(string-append filename ".sty"))))
	   (read-bktd-commaed-filenames in)))))

  (define slatex::process-documentclass
    (lambda (in)
      (eat-bktd-text in)
      (eat-grouped-text in)))

  (define slatex::process-case-info
    (lambda (in)
      ;find out and tell slatex if the scheme tokens that differ
      ;only by case should be treated identical or not
      (let ((bool (read-grouped-latexexp in)))
	(set! *slatex-case-sensitive?*
	      (cond ((string-ci=? bool "true") #t)
		    ((string-ci=? bool "false") #f)
		    (else (error "process-case-info: ~
\\schemecasesensitive's arg should be true or false.")))))))

  (defvar slatex::seen-first-command? #f)

  (define slatex::process-main-tex-file
    (lambda (filename)
      ;kick off slatex on the main .tex file filename
      (display "SLaTeX v. ")
      (display version-number)
      (newline)
      (set! primary-aux-file-count -1)
      (set! *slatex-separate-includes?* #f)
      (if (or (not *texinputs-list*) (null? *texinputs-list*))
          (set! *texinputs-list*
            (if *texinputs* (path-to-list *texinputs*)
                '(""))))
      (let ((file-hide-file "xZfilhid.tex"))
	(if (file-exists? file-hide-file) (delete-file file-hide-file))
	(if (memq *op-sys* '(dos os2fat))
	    (call-with-output-file file-hide-file
	      (lambda (out)
		(display "\\def\\filehider{x}" out)
		(newline out))
          'text)))
      (display "typesetting code")
      (set! *tex-calling-directory* (directory-namestring filename))
      (set! subjobname (basename filename))
      (set! seen-first-command? #f)
      (process-tex-file filename)
      (display "done")
      (newline)))

  (define slatex::dump-intext
    (lambda (in out)
      (let* ((write-char (if out (function write-char) (function ignore2)))
	     (delim-char (begin (eat-whitespace in) (read-char in)))
	     (delim-char
	      (cond ((char=? delim-char #\{) #\})
		    (else delim-char))))
	(if (eof-object? delim-char)
	    (error "dump-intext: Expected delimiting character ~
but found eof."))
	(let loop ()
	  (let ((c (read-char in)))
	    (if (eof-object? c)
		(error "dump-intext: Found eof inside Scheme code."))
	    (if (char=? c delim-char) 'done
		(begin (funcall write-char c out) (loop))))))))

  (define slatex::dump-display
    (lambda (in out ender)
      (eat-tabspace in)
      (let ((write-char (if out (function write-char) (function ignore2)))
	    (ender-lh (string-length ender)) (c (peek-char in)))
	(if (eof-object? c)
	    (error "dump-display: Found eof inside displayed code."))
	(if (char=? c #\newline) (read-char in))
	(let loop ((i 0))
	  (if (= i ender-lh) 'done
	      (let ((c (read-char in)))
		(if (eof-object? c)
		    (error "dump-display: Found eof inside displayed code."))
		(if (char=? c (string-ref ender i))
		    (loop (+ i 1))
		    (let loop2 ((j 0))
		      (if (< j i)
			  (begin
			    (funcall write-char (string-ref ender j) out)
			    (loop2 (+ j 1)))
			  (begin
			    (funcall write-char c out)
			    (loop 0)))))))))))

  ;continued on proctex2.scm
  )
