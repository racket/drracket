;proctex2.scm
;SLaTeX v. 2.4
;Implements SLaTeX's piggyback to LaTeX
;...continued from proctex.scm
;(c) Dorai Sitaram, Rice U., 1991, 1994

(eval-within slatex

  (defvar slatex::debug? #f)

  (define slatex::process-tex-file
    (lambda (raw-filename)
      ;call slatex on the .tex file raw-filename
      (if debug?
	  (begin (display "begin ")
		 (display raw-filename)
		 (newline)))
      (let ((filename (full-texfile-name raw-filename)))
	(if (not filename) ;didn't find it
	    (begin (display "[")
		   (display raw-filename)
		   (display "]") (force-output))
	    (call-with-input-file filename
	      (lambda (in)
		(let ((done? #f))
		  (let loop ()
		    (if done? 'exit-loop
			(begin
                          (let ((c (read-char in)))
                            (cond
                             ((eof-object? c) (set! done? #t))
                             ((char=? c #\%) (eat-till-newline in))
                             ((char=? c #\\)
                              (let ((cs (read-ctrl-seq in)))
                                (if (not seen-first-command?)
                                    (begin
				      (set! seen-first-command? #t)
                                      (decide-latex-or-tex
                                       (or
					(string=? cs "documentstyle")
					(string=? cs "documentclass")
					(string=? cs "NeedsTeXFormat")
					))))
                                (cond
                                 ((not *slatex-enabled?*)
                                  (if (string=? cs *slatex-reenabler*)
                                      (enable-slatex-again)))
                                 ((string=? cs "slatexignorecurrentfile")
                                  (set! done? #t))
                                 ((string=? cs "slatexseparateincludes")
                                  (if *latex?*
                                      (set! *slatex-separate-includes?* #t)))
                                 ((string=? cs "slatexdisable")
                                  (disable-slatex-temply in))
                                 ((string=? cs "begin")
                                  (eat-tex-whitespace in)
                                  (if (eqv? (peek-char in) #\{)
                                      (let ((cs (read-grouped-latexexp in)))
                                        (cond
                                         ((member cs *display-triggerers*)
                                          (slatex::trigger-scheme2tex
                                           'envdisplay in cs))
                                         ((member cs *response-triggerers*)
                                          (trigger-scheme2tex 'envresponse
                                                              in cs))
                                         ((member cs *respbox-triggerers*)
                                          (trigger-scheme2tex 'envrespbox
                                                              in cs))
                                         ((member cs *box-triggerers*)
                                          (trigger-scheme2tex 'envbox
                                                              in cs))
                                         ((member cs *region-triggerers*)
                                          (slatex::trigger-region
                                           'envregion in cs))))))
                                 ((member cs *intext-triggerers*)
                                  (trigger-scheme2tex 'intext in #f))
                                 ((member cs *resultintext-triggerers*)
                                  (trigger-scheme2tex 'resultintext in #f))
                                 ((member cs *display-triggerers*)
                                  (trigger-scheme2tex 'plaindisplay
                                                      in cs))
                                 ((member cs *response-triggerers*)
                                  (trigger-scheme2tex 'plainresponse
                                                      in cs))
                                 ((member cs *respbox-triggerers*)
                                  (trigger-scheme2tex 'plainrespbox
                                                      in cs))
                                 ((member cs *box-triggerers*)
                                  (trigger-scheme2tex 'plainbox
                                                      in cs))
                                 ((member cs *region-triggerers*)
                                  (trigger-region 'plainregion
                                                  in cs))
                                 ((member cs *input-triggerers*)
                                  (slatex::process-scheme-file
                                   (read-filename in)))
                                 ((string=? cs "input")
                                  (let ((f (read-filename in)))
                                    (if (not (string=? f ""))
                                        (fluid-let
					    ((*slatex-in-protected-region?*
					      #f))
                                          (process-tex-file f)))))
                                 ((string=? cs "usepackage")
                                  (fluid-let ((*slatex-in-protected-region?*
                                               #f))
                                    (process-tex-file
                                     (string-append (read-filename in)
                                                    ".sty"))))
                                 ((string=? cs "include")
                                  (if *latex?*
                                      (let ((f (full-texfile-name
                                                (read-filename in))))
                                        (if (and f
						 (or (eq? *include-onlys* 'all)
						     (member f
							     *include-onlys*)))
                                            (fluid-let
                                                ((*slatex-in-protected-region?*
                                                  #f))
                                              (if *slatex-separate-includes?*
                                                  (fluid-let
                                                      ((subjobname
                                                        (basename f))
                                                       (primary-aux-file-count
                                                        -1))
                                                    (process-tex-file f))
                                                  (process-tex-file f)))))))
                                 ((string=? cs "includeonly")
                                  (if *latex?* (process-include-only in)))
                                 ((string=? cs "documentstyle")
                                  (if *latex?* (process-documentstyle in)))
                                 ((string=? cs "documentclass")
                                  (if *latex?* (process-documentclass in)))
                                 ((string=? cs "schemecasesensitive")
                                  (process-case-info in))
                                 ((string=? cs "defschemetoken")
                                  (process-slatex-alias
				   in (function adjoin)
				   'intext))
                                 ((string=? cs "undefschemetoken")
                                  (process-slatex-alias
				   in (function delete)
				   'intext))
                                 ((string=? cs "defschemeresulttoken")
                                  (process-slatex-alias
				   in (function adjoin)
				   'resultintext))
                                 ((string=? cs "undefschemeresulttoken")
                                  (process-slatex-alias
				   in (function delete)
				   'resultintext))
                                 ((string=? cs "defschemeresponsetoken")
                                  (process-slatex-alias
				   in (function adjoin)
				   'response))
                                 ((string=? cs "undefschemeresponsetoken")
                                  (process-slatex-alias
				   in (function delete)
				   'response))
                                 ((string=? cs "defschemeresponseboxtoken")
                                  (process-slatex-alias
				   in (function adjoin)
				   'respbox))
                                 ((string=? cs "undefschemeresponseboxtoken")
                                  (process-slatex-alias
				   in (function delete)
				   'respbox))
                                 ((string=? cs "defschemedisplaytoken")
                                  (process-slatex-alias
				   in (function adjoin)
				   'display))
                                 ((string=? cs "undefschemedisplaytoken")
                                  (process-slatex-alias
				   in (function delete)
				   'display))
                                 ((string=? cs "defschemeboxtoken")
                                  (process-slatex-alias
				   in (function adjoin)
				   'box))
                                 ((string=? cs "undefschemeboxtoken")
                                  (process-slatex-alias
				   in (function delete)
				   'box))
                                 ((string=? cs "defschemeinputtoken")
                                  (process-slatex-alias
				   in (function adjoin)
				   'input))
                                 ((string=? cs "undefschemeinputtoken")
                                  (process-slatex-alias
				   in (function delete)
				   'input))
                                 ((string=? cs "defschemeregiontoken")
                                  (process-slatex-alias
				   in (function adjoin)
				   'region))
                                 ((string=? cs "undefschemeregiontoken")
                                  (process-slatex-alias in
							(function delete)
                                                        'region))
                                 ((string=? cs "defschememathescape")
                                  (process-slatex-alias in
							(function adjoin)
                                                        'mathescape))
                                 ((string=? cs "undefschememathescape")
                                  (process-slatex-alias in
							(function delete)
                                                        'mathescape))
                                 ((string=? cs "setkeyword")
                                  (add-to-slatex-db in 'keyword))
                                 ((string=? cs "setconstant")
                                  (add-to-slatex-db in 'constant))
                                 ((string=? cs "setvariable")
                                  (add-to-slatex-db in 'variable))
                                 ((string=? cs "setspecialsymbol")
                                  (add-to-slatex-db in 'setspecialsymbol))
                                 ((string=? cs "unsetspecialsymbol")
                                  (add-to-slatex-db in 'unsetspecialsymbol))
                                 )))))
			  (loop))))))
	      'text)))
      (if debug?
	  (begin (display "end ")
		 (display raw-filename)
		 (newline)))
      ))

  (define slatex::process-scheme-file
    (lambda (raw-filename)
      ;typeset the scheme file raw-filename so that it can
      ;be input as a .tex file
      (let ((filename (full-scmfile-name raw-filename)))
	(if (not filename)
	    (begin (display "process-scheme-file: ")
		   (display raw-filename)
		   (display " doesn't exist")
		   (newline))
	    (let ((aux.tex (new-aux-file ".tex")))
	      (display ".") (force-output)
	      (if (file-exists? aux.tex) (delete-file aux.tex))
	      (call-with-input-file filename
		(lambda (in)
		  (call-with-output-file aux.tex
		    (lambda (out)
		      (fluid-let ((*intext?* #f)
				  (*code-env-spec* "ZZZZschemedisplay"))
			(scheme2tex in out)))
		    'text))
		'text)
	      (if *slatex-in-protected-region?*
		  (set! *protected-files* (cons aux.tex *protected-files*)))
	      (process-tex-file filename))))))

  (define slatex::trigger-scheme2tex
    (lambda (typ in env)
      ;process the slatex command identified by typ;
      ;env is the name of the environment
      (let* ((aux (new-aux-file)) (aux.scm (string-append aux ".scm"))
				  (aux.tex (string-append aux ".tex")))
	(if (file-exists? aux.scm) (delete-file aux.scm))
	(if (file-exists? aux.tex) (delete-file aux.tex))
	(display ".") (force-output)
	(call-with-output-file aux.scm
	  (lambda (out)
	    (cond ((memq typ '(intext resultintext)) (dump-intext in out))
		  ((memq typ '(envdisplay envresponse envrespbox envbox))
		   (dump-display in out (string-append "\\end{" env "}")))
		  ((memq typ '(plaindisplay plainresponse
					    plainrespbox plainbox))
		   (dump-display in out (string-append "\\end" env)))
		  (else (error "trigger-scheme2tex: ~
Unknown triggerer ~s." typ))))
	  'text)
	(call-with-input-file aux.scm
	  (lambda (in)
	    (call-with-output-file aux.tex
	      (lambda (out)
		(fluid-let
		    ((*intext?* (memq typ '(intext resultintext)))
		     (*code-env-spec*
		      (cond ((eq? typ 'intext) "ZZZZschemecodeintext")
			    ((eq? typ 'resultintext)
			     "ZZZZschemeresultintext")
			    ((memq typ '(envdisplay plaindisplay))
			     "ZZZZschemedisplay")
			    ((memq typ '(envresponse plainresponse))
			     "ZZZZschemeresponse")
			    ((memq typ '(envrespbox plainrespbox))
			     "ZZZZschemeresponsebox")
			    ((memq typ '(envbox plainbox))
			     "ZZZZschemebox")
			    (else (error "trigger-scheme2tex: ~
Unknown triggerer ~s." typ)))))
		  (scheme2tex in out)))
	      'text))
	  'text)
	(if *slatex-in-protected-region?*
	    (set! *protected-files* (cons aux.tex *protected-files*)))
	(if (memq typ '(envdisplay plaindisplay envbox plainbox))
	    (process-tex-file aux.tex))
	(delete-file aux.scm)
	)))

  (define slatex::trigger-region
    (lambda (typ in env)
      ;process a scheme region to create a in-lined file with
      ;slatex output
      (let ((aux.tex (new-primary-aux-file ".tex"))
	    (aux2.tex (new-secondary-aux-file ".tex")))
	(if (file-exists? aux2.tex) (delete-file aux2.tex))
	(if (file-exists? aux.tex) (delete-file aux.tex))
	(display ".") (force-output)
	(fluid-let ((*slatex-in-protected-region?* #t)
		    (*protected-files* '()))
	  (call-with-output-file aux2.tex
	    (lambda (out)
	      (cond ((eq? typ 'envregion)
		     (dump-display in out (string-append "\\end{" env "}")))
		    ((eq? typ 'plainregion)
		     (dump-display in out (string-append "\\end" env)))
		    (else (error "trigger-region: ~
Unknown triggerer ~s." typ))))
	    'text)
	  (process-tex-file aux2.tex)
	  (set! *protected-files* (reverse! *protected-files*))
	  (call-with-input-file aux2.tex
	    (lambda (in)
	      (call-with-output-file aux.tex
		(lambda (out)
		  (slatex::inline-protected-files in out))
		'text))
	    'text)
	  (delete-file aux2.tex)
	  ))))

  (define slatex::inline-protected-files
    (lambda (in out)
      ;inline all the protected files in port in into port out
      (let ((done? #f))
	(let loop ()
	  (if done? 'exit-loop
	      (begin
                (let ((c (read-char in)))
                  (cond ((eof-object? c)
                         ;(display "{}" out)
                         (set! done? #t))
                        ((or (char=? c *return*) (char=? c #\newline))
                         (let ((c2 (peek-char in)))
                           (if (not (eof-object? c2))
                               (write-char c out))))
                        ((char=? c #\%)
                          (write-char c out) (newline out)
                          (eat-till-newline in))
                        ((char=? c #\\)
                         (let ((cs (read-ctrl-seq in)))
                           (cond
                            ((string=? cs "begin")
                             (let ((cs (read-grouped-latexexp in)))
                               (cond ((member cs *display-triggerers*)
                                      (slatex::inline-protected
                                       'envdisplay in out cs))
                                     ((member cs *response-triggerers*)
                                      (inline-protected
                                       'envresponse in out cs))
                                     ((member cs *respbox-triggerers*)
                                      (inline-protected
                                       'envrespbox in out cs))
                                     ((member cs *box-triggerers*)
                                      (inline-protected 'envbox in out cs))
                                     ((member cs *region-triggerers*)
                                      (inline-protected
                                       'envregion in out cs))
                                     (else
                                      (display "\\begin{" out)
                                      (display cs out)
                                      (display "}" out)))))
                            ((member cs *intext-triggerers*)
                             (inline-protected 'intext in out #f))
                            ((member cs *resultintext-triggerers*)
                             (inline-protected 'resultintext in out #f))
                            ((member cs *display-triggerers*)
                             (inline-protected 'plaindisplay in out cs))
                            ((member cs *response-triggerers*)
                             (inline-protected 'plainresponse in out cs))
                            ((member cs *respbox-triggerers*)
                             (inline-protected 'plainrespbox in out cs))
                            ((member cs *box-triggerers*)
                             (inline-protected 'plainbox in out cs))
                            ((member cs *region-triggerers*)
                             (inline-protected 'plainregion in out cs))
                            ((member cs *input-triggerers*)
                             (inline-protected 'input in out cs))
                            (else
                             (display "\\" out)
                             (display cs out)))))
			(else (write-char c out))))
		(loop)))))))

  (define slatex::inline-protected
    (lambda (typ in out env)
      (cond ((eq? typ 'envregion)
	     (display "\\begin{" out)
	     (display env out)
	     (display "}" out)
	     (dump-display in out (string-append "\\end{" env "}"))
	     (display "\\end{" out)
	     (display env out)
	     (display "}" out))
	    ((eq? typ 'plainregion)
	     (display "\\" out)
	     (display env out)
	     (dump-display in out (string-append "\\end" env))
	     (display "\\end" out)
	     (display env out))
	    (else (let ((f (car *protected-files*)))
		    (set! *protected-files* (cdr *protected-files*))
		    (call-with-input-file f
		      (lambda (in)
			(inline-protected-files in out))
		      'text)
		    (delete-file f)
		    )
		  (cond ((memq typ '(intext resultintext))
			 (display "{}" out)
			 (dump-intext in #f))
			((memq typ '(envrespbox envbox))
			 (if (not *latex?*)
			     (display "{}" out))
			 (dump-display in #f
				       (string-append "\\end{" env "}")))
			((memq typ '(plainrespbox plainbox))
			 (display "{}" out)
			 (dump-display in #f
				       (string-append "\\end" env)))
			((memq typ '(envdisplay envresponse))
			 (dump-display in #f
				       (string-append "\\end{" env "}")))
			((memq typ '(plaindisplay plainresponse))
			 (dump-display in #f (string-append "\\end" env)))
			((eq? typ 'input)
			 (read-filename in)) ;and throw it away
			(else (error "inline-protected: ~
Unknown triggerer ~s." typ)))))))
  )