
;; MrEd automatic testing basic functions and macros

(define errs null)
(define test-count 0)

(define (test expect name got)
  (set! test-count (add1 test-count))
  (unless (equal? expect got)
    (let ([s (format "~a: expected ~a; got ~a" name expect got)])
      (printf "ERROR: ~a~n" s)
      (set! errs (cons s errs)))))


(define-macro st
  (lambda (val obj method . args)
    `(test ,val ',method (send ,obj ,method ,@args))))

(define-macro stv
  (lambda args
    `(st (void) ,@args)))

(define-macro stvals
  (lambda (vals obj method . args)
    `(test ,vals ',method (call-with-values (lambda () (send ,obj ,method ,@args)) list))))


(define (report-errs)
  (newline)
  (if (null? errs)
      (printf "Passed all ~a tests~n" test-count)
      (begin
	(printf "~a Error(s) in ~a tests~n" (length errs) test-count)
	(for-each
	 (lambda (s)
	   (printf "~a~n" s))
	 (reverse errs)))))

