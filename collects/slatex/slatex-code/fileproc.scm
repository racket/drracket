;fileproc.scm
;SLaTeX Version 2.3
;File-manipulation routines used by SLaTeX
;(c) Dorai Sitaram, Rice U., 1991, 1994

;file-exists?

(eval-if (vscm)
  (eval-within slatex
    (define slatex::file-exists?
      (if (eq? *op-sys* 'unix)
	  (lambda (f)
	    (system (string-append "test -f " f)))
	  (lambda (f) 'assume-file-exists)))))

(eval-unless (bigloo chez cl cscheme elk gambit guile mzscheme pcsge scm stk
                     vscm)
  (eval-within slatex
    (define slatex::file-exists?
      (lambda (f) #t))));assume file exists

;delete-file

(eval-if (schemetoc stk umbscheme)
  (eval-within slatex
    (define slatex::delete-file
      (lambda (f)
	(call-with-output-file f
	  (lambda (p) 'file-deleted))))))

(eval-unless (bigloo chez cl cscheme guile mzscheme pcsge
               schemetoc scm stk umbscheme vscm)
  (eval-within slatex
    (define slatex::delete-file
      (lambda (f) 'assume-file-deleted))))

;force-output

;the DOS version of C Scheme has flush-output, the Unix version doesn't

(eval-if (cscheme)
  (eval-within slatex
    (define slatex::force-output
      (if (environment-bound? user-initial-environment 'flush-output)
	  flush-output
	  (lambda z 'assume-output-forced)))))

(eval-if (bigloo)
  (eval-within slatex
    (define slatex::force-output
      (lambda z
        (if (null? z)
          (flush-output-port (current-output-port))
          (flush-output-port (car z)))))))

(eval-unless (bigloo chez cl cscheme elk guile mzscheme scm vscm)
  (eval-within slatex
    (define slatex::force-output
      (lambda z 'assume-output-forced))))
