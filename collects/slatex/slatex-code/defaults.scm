;defaults.scm
;SLaTeX v. 2.3
;Default database for SLaTeX
;(c) Dorai Sitaram, Rice U., 1991, 1994

(eval-if (cl)
  (eval-within slatex
    (defvar slatex::*slatex-case-sensitive?* #f)))

(eval-unless (cl)
  (eval-within slatex
    (defvar slatex::*slatex-case-sensitive?* #t)))

(eval-within slatex

  (defvar slatex::keyword-tokens
    '(
      ;RnRS (plus some additional Scheme) keywords
      "=>"
      "%"
      "abort"
      "and"
      "begin"
      "begin0"
      "case"
      "case-lambda"
      "cond"
      "define"
      "define!"
      "define-macro!"
      "define-syntax"
      "defmacro"
      "defrec!"
      "delay"
      "do"
      "else"
      "extend-syntax"
      "fluid-let"
      "if"
      "lambda"
      "let"
      "let*"
      "letrec"
      "let-syntax"
      "letrec-syntax"
      "or"
      "quasiquote"
      "quote"
      "rec"
      "record-case"
      "record-evcase"
      "recur"
      "set!"
      "sigma"
      "struct"
      "syntax"
      "syntax-rules"
      "trace"
      "trace-lambda"
      "trace-let"
      "trace-recur"
      "unless"
      "unquote"
      "unquote-splicing"
      "untrace"
      "when"
      "with"
      ))

  (defvar slatex::variable-tokens '())

  (defvar slatex::constant-tokens '())

  (defvar slatex::data-tokens '())

  (defvar slatex::special-symbols
    '(
      ("." . ".")
      ("..." . "{\\dots}")
      ("-" . "$-$")
      ("1-" . "\\va{1$-$}")
      ("-1+" . "\\va{$-$1$+$}")
      ))

  (defvar slatex::macro-definers
    '("define-syntax" "syntax-rules" "defmacro"
       "extend-syntax" "define-macro!"))

  (defvar slatex::case-and-ilk
    '("case" "record-case"))

  (define slatex::tex-analog
    (lambda (c)
      ;find a TeX string that corresponds to the character c
      (case c
        ((#\$ #\& #\% #\# #\_) (string #\\ c))
        ;((#\#) "{\\sf\\#}")
        ;((#\\) "{\\ttbackslash}")
        ((#\{ #\}) (string #\$ #\\ c #\$))
        ((#\\) "$\\backslash$")
        ((#\+) "$+$")
        ((#\*) "$\\ast$")
        ((#\=) "$=$")
        ((#\<) "$\\lt$")
        ((#\>) "$\\gt$")
        ((#\^) "\\^{}")
        ((#\|) "$\\vert$")
        ;((#\~) "\\verb-~-")
        ((#\~) "\\~{}")
        ((#\@) "{\\atsign}")
        ((#\") "{\\tt\\dq}")
        (else (string c)))))

  (define slatex::token=?
    (lambda (t1 t2)
      ;tests if t1 and t2 are identical tokens
      (funcall (if *slatex-case-sensitive?* (function string=?)
		   (function string-ci=?))
	       t1 t2)))

  (defvar slatex::*slatex-enabled?* #t)
  (defvar slatex::*slatex-reenabler* "UNDEFINED")
  (defvar slatex::*intext-triggerers* (list "scheme"))
  (defvar slatex::*resultintext-triggerers* (list "schemeresult"))
  (defvar slatex::*display-triggerers* (list "schemedisplay"))
  (defvar slatex::*response-triggerers* (list "schemeresponse"))
  (defvar slatex::*respbox-triggerers* (list "schemeresponsebox"))
  (defvar slatex::*box-triggerers* (list "schemebox"))
  (defvar slatex::*top-box-triggerers* (list "schemetopbox"))
  (defvar slatex::*input-triggerers* (list "schemeinput"))
  (defvar slatex::*region-triggerers* (list "schemeregion"))
  (defvar slatex::*math-triggerers* '())
  (defvar slatex::*slatex-in-protected-region?* #f)
  (defvar slatex::*protected-files* '())
  (defvar slatex::*include-onlys* 'all)
  (defvar slatex::*latex?* #t)
  (defvar slatex::*slatex-separate-includes?* #f)
  (defvar slatex::*tex-calling-directory* "")
  )
