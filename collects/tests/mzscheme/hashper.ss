; Test suite to ensure #% objects are handled properly by MzRice.
; Report problems to Shriram Krishnamurthi <shriram@cs.rice.edu>.

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(define bad#%?
  (if (defined? 'read/zodiac)
    exn?
    syntaxe?))

; Masking names shouldn't hurt the #% versions..

(define car 3)
(test 3 #%car (cons 3 2))
(define car #%car)

(let ((lambda 2))
  (test #t equal? 2 ((#%lambda (x) x) lambda)))

; You can't mask the #% versions.

(error-test '(define #%lambda 2) bad#%?)
(error-test '(set! #%lambda 2) bad#%?)

; We allow random #% things to be set!'ed and define'd.

(test #t equal? (void) (eval '(define #%foo 3)))
(test #t equal? 4 (begin (set! #%foo 4) #%foo))

; But you can't bind #% things either.

(error-test '(let ((#%car 3)) 3) syntaxe?)
(error-test '(let ((#%lambda 3)) 3) syntaxe?)

; Let's try out all #% syntax to make sure it's immune.  (We'll skip
; the macro stuff.)

(map (lambda (s)
       (error-test `(define ,s 3) bad#%?)
       (error-test `(set! ,s 3) bad#%?))
  '(#%lambda #%let-values #%letrec*-values #%define-values #%quote
     #%if #%begin #%set! #%begin0 #%case-lambda #%struct))

; And a few primitives, for good measure.

(map (lambda (s)
       (error-test `(define ,s 3) bad#%?)
       (error-test `(set! ,s 3) bad#%?))
  '(#%car #%cdr #%cons))

(newline)
(newline)

; (printf "Done with #% test suite!~n~n")

(report-errs)
