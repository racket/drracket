
(when (not (defined? 'test))
  (load-relative "testing.ss"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Parameterization Tests                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Killing an eventspace
(define c (make-custodian))
(define e (parameterize ([current-custodian c]) (make-eventspace)))
(parameterize ([current-eventspace e]) (send (make-object frame% "x" #f 50 50) show #t))
(test #f 'shutdown? (eventspace-shutdown? e))
(custodian-shutdown-all c)
(test #t 'shutdown? (eventspace-shutdown? e))
(define (try-use-es t)
  (test
   'error
   'shutdown-eventspace
   (with-handlers ([(lambda (x)
		      (and (exn:misc? x)
			   (regexp-match "shutdown" (exn-message x))))
		    (lambda (x)
		      (printf "got expected error: ~a~n" (exn-message x))
		      'error)])
     (parameterize ([current-eventspace e]) 
       (t)))))
(try-use-es (lambda () (make-object frame% "x" #f 50 50)))
(try-use-es (lambda () (make-object dialog% "x" #f 50 50)))
(try-use-es (lambda () (make-object timer%)))
(try-use-es (lambda () (queue-callback void)))

(report-errs)
