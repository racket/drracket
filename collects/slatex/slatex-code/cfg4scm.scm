(load "slaconfg.scm")
(load "batconfg.scm")

(case dialect
  ((scm) (quit))
  ((cscheme) (%exit))
  ((bigloo) (exit 0))
  (else (exit)
        (display "You may exit Scheme now!")
        (newline)))