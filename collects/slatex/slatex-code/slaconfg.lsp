;slaconfg.lsp
;Configures SLaTeX for Common Lisp on your system
;(c) Dorai Sitaram, Rice U., 1991, 1994

(set-dispatch-macro-character #\# #\T
  #'(lambda (p ig ig2)
      (declare (ignore ig ig2))
      t))

(set-dispatch-macro-character #\# #\F
  #'(lambda (p ig ig2)
      (declare (ignore ig ig2))
      nil))			

(defvar *slatex-directory* (directory-namestring *load-pathname*))

(defvar dialect 'cl)
(defvar *op-sys*)

(with-open-file (inp (concatenate 'string
                                  *slatex-directory*
                                  "config.dat")
                     :direction :input)
  (read inp) ;ignore dialect info
  (setq *op-sys* (read inp)))

(if (not (member *op-sys* '(windows os2 unix dos os2fat mac-os)))
    (setq *op-sys* 'other))

(load (merge-pathnames "preproc.lsp" *slatex-directory*))

(defvar list-of-slatex-files
  (mapcar
   #'(lambda (f)
       (concatenate 'string *slatex-directory* f))
   (list
    "s4.scm"
    "seqprocs.scm"
    "fileproc.scm"
    "lerror.scm"
    "defaults.scm"
    "structs.scm"
    "helpers.scm"
    "peephole.scm"
    "codeset.scm"
    "pathproc.scm"
    "texread.scm"
    "proctex.scm"
    "proctex2.scm")))

(format t "~&Beginning configuring SLaTeX for Common Lisp on ~a -- ~
            wait..." *op-sys*)

(defvar outfile (concatenate 'string *slatex-directory*
			     #+(or mcl clisp) "slatexsrc.scm"
			     #-(or mcl clisp) "slatex.scm"))

(if (probe-file outfile) (delete-file outfile))

(with-open-file (o outfile :direction :output)
  (format o
    ";slatex.scm file generated for Common Lisp, ~a~%~
           ;(c) Dorai Sitaram, Rice U., 1991, 1994~%"
    *op-sys*)

  #-gcl
  (print `(defpackage slatex (:use cl)) o)
  (print `(in-package :slatex) o)
  (print `(defvar *op-sys* ',*op-sys*) o)

  (dolist (f list-of-slatex-files)

    (format t "~&~a...~%" f)

    (format o "~%~%;~a~%" f)
    (with-open-file (i f :direction :input)
      (loop
        (let ((x (read i nil :eof)))
	  (if (eq x :eof) (return))
	  (let ((xm (expand-macrocalls x)))
	    (cond ((not xm) nil)
              ((and (consp xm) (eq (car xm) 'progn))
                (dolist (y (cdr xm))
                  (if y (pprint y o))))
              (t (pprint xm o)))))))))

#+(or mcl clisp)
(progn
  (format t "~&Getting compiled version...~%")
  (compile-file outfile :output-file
		(concatenate 'string *slatex-directory*
			     "slatex.scm"))
  (format t "~&Finished compilation~%"))

(format t
  "~&Finished configuring SLaTeX for your machine.

Read install for details on

1. which paths to place the SLaTeX files in;

2. how to modify the given batch file or shell script
that invokes SLaTeX.~%~%")
