;structs.scm
;SLaTeX v. 2.3
;Structures used by SLaTeX
;(c) Dorai Sitaram, Rice U., 1991, 1994

(eval-within slatex 

  (defvar slatex::*max-line-length* 200)

  (defenum
    ;possible values of =space
    slatex::&void-space
    slatex::&plain-space
    slatex::&init-space
    slatex::&init-plain-space
    slatex::&paren-space
    slatex::&bracket-space
    slatex::&quote-space
    slatex::&inner-space)

  (defenum
    ;possible values of =tab
    slatex::&void-tab
    slatex::&set-tab
    slatex::&move-tab
    slatex::&tabbed-crg-ret
    slatex::&plain-crg-ret)

  (defenum
    ;possible values of =notab
    slatex::&void-notab
    slatex::&begin-comment
    slatex::&mid-comment
    slatex::&begin-string
    slatex::&mid-string
    slatex::&end-string
    slatex::&begin-math
    slatex::&mid-math
    slatex::&end-math)

  (defrecord slatex::make-raw-line
    slatex::=rtedge
    slatex::=char
    slatex::=space
    slatex::=tab
    slatex::=notab)

  (define slatex::make-line
    (lambda ()
      ;makes a "line" record
      (let ((l (make-raw-line)))
	(setf (of l =rtedge) 0)
	(setf (of l =char) (make-string *max-line-length* #\space))
	(setf (of l =space) (make-string *max-line-length* &void-space))
	(setf (of l =tab) (make-string *max-line-length* &void-tab))
	(setf (of l =notab) (make-string *max-line-length* &void-notab))
	l)))

  (defvar slatex::*line1* (make-line))
  (defvar slatex::*line2* (make-line))

  (defrecord slatex::make-case-frame
    slatex::=in-ctag-tkn
    slatex::=in-bktd-ctag-exp
    slatex::=in-case-exp)

  (defrecord slatex::make-bq-frame
    slatex::=in-comma slatex::=in-bq-tkn slatex::=in-bktd-bq-exp)

  (defvar slatex::*latex-paragraph-mode?* 'fwd1)

  (defvar slatex::*intext?* 'fwd2)
  (defvar slatex::*code-env-spec* "UNDEFINED")

  (defvar slatex::*in* 'fwd3)
  (defvar slatex::*out* 'fwd4)

  (defvar slatex::*in-qtd-tkn* 'fwd5)
  (defvar slatex::*in-bktd-qtd-exp* 'fwd6)

  (defvar slatex::*in-mac-tkn* 'fwd7)
  (defvar slatex::*in-bktd-mac-exp* 'fwd8)

  (defvar slatex::*case-stack* 'fwd9)

  (defvar slatex::*bq-stack* 'fwd10)

  (define slatex::display-space
    (lambda (s p)
      (cond ((eq? s &plain-space) (display #\space p))
	    ((eq? s &init-plain-space) (display #\space p))
	    ((eq? s &init-space) (display "\\HL " p))
	    ((eq? s &paren-space) (display "\\PRN " p))
	    ((eq? s &bracket-space) (display "\\BKT " p))
	    ((eq? s &quote-space) (display "\\QUO " p))
	    ((eq? s &inner-space) (display "\\ " p)))))

  (define slatex::display-tab
    (lambda (tab p)
      (cond ((eq? tab &set-tab) (display "\\=" p))
	    ((eq? tab &move-tab) (display "\\>" p)))))

  (define slatex::display-notab
    (lambda (notab p)
      (cond ((eq? notab &begin-string) (display "\\dt{" p))
	    ((eq? notab &end-string) (display "}" p)))))
  )
