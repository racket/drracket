;lerror.scm
;SLaTeX v. 2.3
;Display and error routines
;(c) Dorai Sitaram, Rice U., 1991, 1994

;#\newline and #\space are r5rs
;#\return and #\tab aren't

(eval-unless (cl scm)
    (eval-within slatex
    (defvar slatex::*return* (integer->char 13))
    (defvar slatex::*tab* (integer->char 9))))

(eval-if (guile scm)
  (eval-within slatex
    (define slatex::error
      (lambda vv
	(let ((ep (current-error-port)))
	  (display "Error: " ep)
	  (for-each
	   (lambda (v)
	     (display v ep)
	     (newline ep))
	   vv)
	  (abort))))))

(eval-if (chez elk schemetoc)
  (eval-within slatex
    (define slatex::error
      (lambda vv
	(display "Error: ")
	(for-each
	 (lambda (v)
	   (display v) (newline))
	 vv)
	(global-error #f "")))))

(eval-if (stk)
  (eval-within slatex
    (define slatex::error
      (lambda vv
        (display "Error: ")
        (for-each
         (lambda (v) (display v) (newline))
         vv)
        (global-error "Error")))))

(eval-if (bigloo)
  (eval-within slatex
    (define slatex::error
      (lambda vv
	(display "Error: ")
	(for-each
	 (lambda (v)
	   (display v) (newline))
	 vv)
	(global-error 'SLaTeX "error" #f)))))

(eval-unless (bigloo chez cl elk guile schemetoc scm)
  (eval-within slatex
    (define slatex::error
      (lambda vv
	(display "Error: ")
	(for-each
	 (lambda (v)
	   (display v) (newline))
	 vv)
	(global-error "")))))

(eval-if (vscm)
    (eval-within slatex
    (define void
      ;(void) is a no-op expression that's useful in some places
      ;where use of a dummy value would make VSCM "warn" about
      ;unused values
      (let ((x 0))
	(lambda ()
	  (set! x 0))))))

(eval-unless (vscm cl chez gambit mzscheme)
  (eval-within slatex
    (define slatex::void
      (lambda ()
        (if #f #f)))))

(eval-if (cl)
  (eval-within slatex
    (defun slatex::function-available (s)
      (let ((x (find-symbol s
                 (if (member 'gcl *features*) :lisp :cl))))
        (if (and x (fboundp x)) x nil)))

    (defun slatex::exit-scheme ()
      (let ((quitter
             (or (function-available "BYE")
                 (function-available "EXIT")
                 (function-available "QUIT"))))
        (if quitter (funcall quitter)
            (progn
             (format t "You may exit CL now!~%")
             (funcall 'barf)))))))

(eval-if (chez elk mzscheme pcsge schemetoc stk umbscheme vscm)
  (eval-within slatex
    (define slatex::exit-scheme
      (lambda () ;in case it's a macro
        (exit)))))

(eval-if (cscheme)
  (eval-within slatex
    (define slatex::exit-scheme
      (lambda ()
        (%exit)))))

(eval-if (guile scm)
  (eval-within slatex
    (define slatex::exit-scheme quit)))

(eval-if (bigloo)
  (eval-within slatex
    (define slatex::exit-scheme
      (lambda () (exit 0)))))

(eval-unless (bigloo chez cl cscheme elk guile mzscheme pcsge
               schemetoc scm umbscheme vscm)
  (eval-within slatex
    (define slatex::exit-scheme
      (lambda ()
	(display "Exit Scheme!")
	(newline)
	(barf)))))
