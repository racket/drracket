(make-slatex-alias
 '(
   global-adjoin adjoin
   global-assoc assoc
   global-delete delete
   global-error error
   global-make-string make-string
   global-member member
   global-peek-char peek-char
   global-read read
   global-read-char read-char
   global-string string
   ))

(case dialect
  ((bigloo) 'skip
   )
  ((chez)
   (make-slatex-alias
     '(
       force-output flush-output
       some ormap
       )))
  ((cl)
   (make-slatex-alias
     `(
       adjoin slatex::%adjoin
       append! nconc
       assoc slatex::%assoc
       begin progn
       char? characterp
       char=? char=
       char-alphabetic? alpha-char-p
       delete slatex::%delete
       display princ
       else t
       eq? eq
       equal? equal
       eqv? eql
       file-exists? probe-file
       fluid-let let
       for-each mapc
       integer->char code-char
       lambda slatex::%lambda
       let slatex::%let
       list-tail subseq
       make-string slatex::%make-string
       map mapcar
       member slatex::%member
       memq member
       memv member
       newline terpri
       null? null
       pair? consp
       peek-char slatex::%peek-char
       position-char position
       read slatex::%read
       read-char slatex::%read-char
       *return* ,(read-from-string "#\\return")
       reverse! nreverse
       set! setq
       set-car! rplaca
       set-cdr! rplacd
       string slatex::%string
       string=? string=
       string-ci=? string-equal
       string-length length
       string-ref char
       sublist subseq
       substring subseq
       *tab* ,(read-from-string "#\\tab")
       void values
       )))
  ((cscheme)
   (make-slatex-alias
     `(
       mapcan append-map!
       *return* ,(with-input-from-string "#\\return" read)
       *tab* ,(with-input-from-string "#\\tab" read)
       )))
  ((elk)
   (make-slatex-alias
     '(
       force-output flush-output-port
       )))
  ((gambit)
   (make-slatex-alias
     '(
       force-output flush-output
       )))
  ((guile)
   (make-slatex-alias
     `(
       *return* ,(call-with-input-string "#\\return" read)
       *tab* ,(call-with-input-string "#\\tab" read)
       )))
  ((mzscheme)
   (make-slatex-alias
     `(
       force-output flush-output
       some ormap
       *return* ,(let ((i (open-input-string "#\\return")))
                   (begin0 (read i) (close-input-port i)))
       *tab* ,(let ((i (open-input-string "#\\tab")))
                (begin0 (read i) (close-input-port i)))
       )))
  ((pcsge) 'skip
   )
  ((scm)
   (make-slatex-alias
     `(
       *return* ,(call-with-input-string "#\\return" read)
       *tab* ,(call-with-input-string "#\\tab" read)
       )))
  ((stk)
   (make-slatex-alias
     `(
       force-output flush
       )))
  ((vscm)
   (make-slatex-alias
     '(
       delete-file remove-file
       force-output flush
       ))))
