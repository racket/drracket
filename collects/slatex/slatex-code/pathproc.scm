;pathproc.scm
;SLaTeX Version 1.99
;File-manipulation routines used by SLaTeX
;(c) Dorai Sitaram, Rice U., 1991, 1994

(eval-unless (cl)
  (eval-within slatex
    (define slatex::directory-namestring
      (lambda (f)
	(let ((p (string-position-right slatex::*directory-mark* f)))
	  (if p
	      (substring f 0 (+ p 1)) ""))))

    (define slatex::basename
      (lambda (f)
	(let ((p (string-position-right *directory-mark* f)))
	  (if p
	      (set! f (substring f (+ p 1) (string-length f))))
	  (let ((p (string-position-right #\. f)))
	    (if p
		(substring f 0 p)
		f)))))

    ))

(eval-if (cl)
  (eval-within slatex
    (defun basename (f)
	(let ((f (file-namestring (merge-pathnames
				   (make-pathname :type "x") f))))
	  (subseq f 0 (- (length f) 2))))))

(eval-within slatex

  (defvar slatex::*texinputs* "")

  (defvar slatex::*texinputs-list* #f)

  (defvar slatex::*path-separator*
    (cond ((eq? *op-sys* 'unix) #\:)
          ((eq? *op-sys* 'mac-os) (integer->char 0))
	  ((memq *op-sys* '(windows os2 dos os2fat)) #\;)
	  (else (error "Couldn't determine path separator character."))))

  (defvar slatex::*directory-mark*
    (cond ((eq? *op-sys* 'unix) #\/)
          ((eq? *op-sys* 'mac-os) #\:)
	  ((memq *op-sys* '(windows os2 dos os2fat)) #\\)
	  (else (error "Couldn't determine directory mark."))))

  (defvar slatex::*directory-mark-string*
    (list->string (list *directory-mark*)))

  (defvar slatex::*file-hider*
    (cond ((memq *op-sys* '(windows os2 unix mac-os)) ".")
	  ((memq *op-sys* '(dos os2fat)) "x") ;no such luck for dos & os2fat
	  (else "."))) ;use any old character

  (define slatex::path-to-list
    (lambda (p)
      ;convert a unix or dos representation of a path to a list of
      ;directory names (strings)
      (let loop ((p (string->list p)) (r (list "")))
	(let ((separator-pos (position-char *path-separator* p)))
	  (if separator-pos
	      (loop (list-tail p (+ separator-pos 1))
		    (cons (list->string (sublist p 0 separator-pos))
			  r))
	      (reverse! (cons (list->string p) r)))))))

  (define slatex::find-some-file
    (lambda (path . files)
      ;look through each directory in path till one of files is found
      (let loop ((path path))
	(if (null? path) #f
	    (let ((dir (car path)))
	      (let loop1 ((files
			   (if (or (string=? dir "") (string=? dir "."))
			       files
			       (map (lambda (file)
				      (string-append dir
						     *directory-mark-string*
						     file)) files))))
		(if (null? files) (loop (cdr path))
		    (let ((file (car files)))
		      (if (file-exists? file) file
			  (loop1 (cdr files)))))))))))

  (define slatex::file-extension
    (lambda (filename)
      ;find extension of filename
      (let ((i (string-position-right #\. filename)))
	(if i (substring filename i (string-length filename))
	    #f))))

  (define slatex::full-texfile-name
    (lambda (filename)
      ;find the full pathname of the .tex/.sty file filename
      (let ((extn (file-extension filename)))
	(if (and extn (or (string=? extn ".sty") (string=? extn ".tex")))
	    (find-some-file *texinputs-list* filename)
	    (find-some-file *texinputs-list*
			    (string-append filename ".tex") filename)))))

  (define slatex::full-styfile-name
    (lambda (filename)
      ;find the full pathname of the .sty file filename
      (find-some-file *texinputs-list*
		      (string-append filename ".sty"))))

  (define slatex::full-clsfile-name
    (lambda (filename)
      ;find the full pathname of the .cls file filename
      (find-some-file *texinputs-list*
		      (string-append filename ".cls"))))

  (define slatex::full-scmfile-name
    (lambda (filename)
      ;find the full pathname of the scheme file filename;
      ;acceptable extensions are .scm .ss .s
      (apply (function find-some-file) *texinputs-list*
	     filename
	     (map (lambda (extn) (string-append filename extn))
		  '(".scm" ".ss" ".s")))))

  (defvar slatex::subjobname 'fwd)

  (defvar slatex::primary-aux-file-count -1)

  (define slatex::new-primary-aux-file
    (lambda e
      ;used by new-aux-file unless in protected region;
      ;this is the default
      (set! primary-aux-file-count
	(+ primary-aux-file-count 1))
      (apply (function string-append) *tex-calling-directory*
	     *file-hider* "Z"
	     (number->string primary-aux-file-count)
	     subjobname e)))

  (define slatex::new-secondary-aux-file
    (let ((n -1))
      (lambda e
	;used by new-aux-file when in protected region
	(set! n (+ n 1))
	(apply (function string-append) *tex-calling-directory*
	       *file-hider*
	       "ZZ" (number->string n) subjobname e))))

  (define slatex::new-aux-file
    (lambda e
      ;create a new auxiliary file with provided extension if any
      (apply (if *slatex-in-protected-region?*
		 (function new-secondary-aux-file)
		 (function new-primary-aux-file))
	     e)))

  )