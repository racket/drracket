;helpers.scm
;SLaTeX v. 2.4
;Helpers for SLaTeX
;(c) Dorai Sitaram, Rice U., 1991, 1994

(eval-unless (cl)
  (eval-within slatex
    (define slatex::prim-data-token?
      (lambda (token)
	;token cannot be empty string!
	(or (char=? (string-ref token 0) #\#)
	    (string->number token))))))

(eval-if (cl)
  (eval-within slatex
    (defun prim-data-token? (token)
      (declare (global-string token))
      (let ((c (char token 0)))
	(or (char= c #\#)
	    (char= c #\:)
	    (numberp (read-from-string token)))))))

(eval-within slatex

  (define slatex::set-keyword
    (lambda (x)
      ;add token x to the keyword database
      (if (not (lmember x keyword-tokens (function token=?)))
	  (begin
            (set! constant-tokens
	      (delete x constant-tokens (function token=?)))
	    (set! variable-tokens
	      (delete x variable-tokens (function token=?)))
	    (set! data-tokens (delete x data-tokens (function token=?)))
	    (set! keyword-tokens (cons x keyword-tokens))))))

  (define slatex::set-constant
    (lambda (x)
      ;add token x to the constant database
      (if (not (lmember x constant-tokens (function token=?)))
	  (begin
            (set! keyword-tokens
	      (delete x keyword-tokens (function token=?)))
	    (set! variable-tokens
	      (delete x variable-tokens (function token=?)))
	    (set! data-tokens (delete x data-tokens (function token=?)))
	    (set! constant-tokens (cons x constant-tokens))))))

  (define slatex::set-variable
    (lambda (x)
      ;add token x to the variable database
      (if (not (lmember x variable-tokens (function token=?)))
	  (begin
            (set! keyword-tokens (delete x keyword-tokens (function token=?)))
	    (set! constant-tokens
	      (delete x constant-tokens (function token=?)))
	    (set! data-tokens (delete x data-tokens (function token=?)))
	    (set! variable-tokens (cons x variable-tokens))))))

  (define slatex::set-data
    (lambda (x)
      ;add token x to the "data" database
      (if (not (lmember x data-tokens (function token=?)))
	  (begin
            (set! keyword-tokens
	      (delete x keyword-tokens (function token=?)))
	    (set! constant-tokens
	      (delete x constant-tokens (function token=?)))
	    (set! variable-tokens
	      (delete x variable-tokens (function token=?)))
	    (set! data-tokens (cons x data-tokens))))))

  (define slatex::set-special-symbol
    (lambda (x transl)
      ;add token x to the special-symbol database with
      ;the translation transl
      (let ((c (lassoc x special-symbols (function token=?))))
	(if c (set-cdr! c transl)
	    (set! special-symbols
	      (cons (cons x transl) special-symbols))))))

  (define slatex::unset-special-symbol
    (lambda (x)
      ;disable token x's special-symbol-hood
      (set! special-symbols
	(delete-if
	    (lambda (c)
	      (token=? (car c) x)) special-symbols))))

  (define slatex::texify
    (lambda (s)
      ;create a tex-suitable string out of token s
      (list->string (slatex::texify-aux s))))

  (define slatex::texify-data
    (lambda (s)
      ;create a tex-suitable string out of the data token s
      (let loop ((l (texify-aux s)) (r '()))
	(if (null? l) (list->string (reverse! r))
	    (let ((c (car l)))
	      (loop (cdr l)
		    (if (char=? c #\-) (append! (list #\$ c #\$) r)
			(cons c r))))))))

  (define slatex::texify-aux
    (let* ((arrow (string->list "-$>$"))
	   (em-dash (string->list "---"))
	   (en-dash (string->list "--"))
	   (arrow2 (string->list "$\\to$"))
	   (em-dash-2 (string->list "${-}{-}{-}$"))
	   (en-dash-2 (string->list "${-}{-}$")))
      (lambda (s)
	;return the list of tex characters corresponding to token s.
	;perhaps some extra context-sensitive prettifying
	;could go in the making of texified-sl below
	(let ((texified-sl (mapcan
			    (lambda (c) (string->list (tex-analog c)))
			    (string->list s))))
	  (let loop ((d texified-sl))
	    ;cdr down texified-sl
	    ;to transform any character combinations
	    ;as desired
	    (cond ((null? d) #f)
		  ((list-prefix? arrow d) ; $->$
		   (let ((d2 (list-tail d 4)))
		     (set-car! d (car arrow2))
		     (set-cdr! d (append (cdr arrow2) d2))
		     (loop d2)))
		  ((list-prefix? em-dash d) ; ---
		   (let ((d2 (list-tail d 3)))
		     (set-car! d (car em-dash-2))
		     (set-cdr! d (append (cdr em-dash-2) d2))
		     (loop d2)))
		  ((list-prefix? en-dash d) ; --
		   (let ((d2 (list-tail d 2)))
		     (set-car! d (car en-dash-2))
		     (set-cdr! d (append (cdr en-dash-2) d2))
		     (loop d2)))
		  (else (loop (cdr d)))))
	  texified-sl))))

  (define slatex::display-begin-sequence
    (lambda (out)
      (if (or *intext?* (not *latex?*))
	  (begin
            (display "\\" out)
	    (display *code-env-spec* out)
	    (newline out))
	  (begin
            (display "\\begin{" out)
	    (display *code-env-spec* out)
	    (display "}%" out)
	    (newline out)))))

  (define slatex::display-end-sequence
    (lambda (out)
      (cond (*intext?* ;(or *intext?* (not *latex?*))
	     (display "\\end" out)
	     (display *code-env-spec* out)
	     ;(display "{}" out)
	     (newline out))
	    (*latex?*
	     (display "\\end{" out)
	     (display *code-env-spec* out)
	     (display "}" out)
	     (newline out))
	    (else
	     (display "\\end" out)
	     (display *code-env-spec* out)
	     (newline out)))))

  (define slatex::display-tex-char
    (lambda (c p)
      (display (if (char? c) (tex-analog c) c) p)))

  (define slatex::display-token
    (lambda (s typ p)
      (cond ((eq? typ 'syntax)
	     (display "\\sy{" p)
	     (display (texify s) p)
	     (display "}" p))
	    ((eq? typ 'variable)
	     (display "\\va{" p)
	     (display (texify s) p)
	     (display "}" p))
	    ((eq? typ 'constant)
	     (display "\\cn{" p)
	     (display (texify s) p)
	     (display "}" p))
	    ((eq? typ 'data)
	     (display "\\dt{" p)
	     (display (texify-data s) p)
	     (display "}" p))
	    (else (error "display-token: ~
Unknown token type ~s." typ)))))

  )