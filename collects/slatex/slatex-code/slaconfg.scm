;slaconfg.scm
;Configures SLaTeX for your Scheme
;(c) Dorai Sitaram, Rice U., 1991, 1994

(define dialect 'forward)
(define *op-sys* 'forward)

(call-with-input-file "config.dat"
  (lambda (p)
    (set! dialect (read p))
    (set! *op-sys* (read p))))

(if (not (memq dialect
           '(bigloo chez cscheme elk guile mzscheme pcsge schemetoc scm
                    stk umbscheme vscm other)))
    (set! dialect 'other))

(if (not (memq *op-sys* '(windows os2 unix dos os2fat mac-os)))
    (set! *op-sys* 'other))

(load "preproc.scm")

(define list-of-slatex-files
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
   "proctex2.scm"))

(display "Beginning configuring SLaTeX for ")
(display dialect)
(display " on ")
(display *op-sys*)
(display " -- wait...")
(newline)

(define outfile
  (if (memq dialect '(bigloo chez mzscheme)) "slatexsrc.scm" "slatex.scm"))

(cond ((memq dialect '(bigloo chez cscheme guile mzscheme pcsge scm))
       (if (file-exists? outfile)
           (delete-file outfile)))
      (else
       (newline)
       (display "If configuring fails following this sentence, ")
       (newline)
       (display "you most likely already have a slatex.scm in the ")
       (display "current directory.")
       (newline)
       (display "Delete it and retry.")
       (newline)))

(define prettyp
;pretty-printer -- not really needed, so use write for dialects
;that don't have it
  (case dialect
    ((bigloo) pp)
    ((chez) pretty-print)
;    ((scm) (if (defined? pretty-print) pretty-print write))
    (else write)))

(call-with-output-file outfile
  (lambda (o)
    ;;begin banner
    (display ";slatex.scm file generated for " o)
    (display dialect o)
    (display ", " o)
    (display *op-sys* o)
    (newline o)
    (display ";(c) Dorai Sitaram, Rice U., 1991, 1994" o)
    (newline o) (newline o)
    ;;end banner

    ;(if (eq? dialect 'bigloo)
    ;(write `(module slatex (main slatex::process-main-tex-file)) o))

    (write `(define slatex::*op-sys* ',*op-sys*) o)
    (newline o)

    (for-each
     (lambda (f)

       (newline)
       (display f) (display "...")

       (newline o)
       (display ";" o)
       (display f o)
       (newline o)
       (newline o)	
       (call-with-input-file f
         (lambda (i)
           (let loop ()
             (let ((x (read i)))
               (if (not (eof-object? x))
                   (let ((xm (expand-macrocalls x)))
                     (cond ((not xm))
                           ((and (pair? xm) (eq? (car xm) 'begin))
                            (for-each
                             (lambda (y)
                               (if y (begin (prettyp y o)
                                            (newline o))))
                             (cdr xm)))
                           (else (prettyp xm o) (newline o)))
                     (loop))))))))
     list-of-slatex-files)))

(if (eq? dialect 'mzscheme)
    (require-library "compile.ss"))

(case dialect
  ((bigloo)
   (newline)
   ;can't get bigloo to compile
   ;(display "Getting compiled version for Bigloo...")
   (display "Couldn't get Bigloo to compile SLaTeX.  Using source for now.")
   (system "cp -p slatexsrc.scm slatex.scm")
   (newline)
   ;(system "bigloo -O -v -o SLaTeX slatex.scm")
   ;(system "rm slatex.o")
   ;(display "Finished compilation (executable is named SLaTeX)")
   ;(newline)
   )
  ((chez mzscheme)
   (newline)
   (display "Getting compiled version...")
   (newline)
   (compile-file "slatexsrc.scm" "slatex.scm")
   ;;(delete-file "slatexsrc.scm")
   (display "Finished compilation")))

(newline)
(newline)
(display "Finished configuring the SLaTeX Scheme file for your machine")
(newline)
(display "Read \"install\" for details on")
(newline)
(newline)
(display "1. which paths to place the SLaTeX files in")
(newline)
(newline)
(display "2. how to use the batch file, shell script, or Scheme script")
(newline)
(display "that invokes SLaTeX")
(newline)
(newline)
