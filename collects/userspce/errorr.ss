(unit/sig userspace:error^
  (import)
  
  ;; check-arg : sym bool str str TST -> void
  (define (check-arg pname condition expected arg-posn given)
    (unless condition
      (error pname "expected <~a> as ~a argument, given: ~e" expected arg-posn given)))
  
  ;; check-arity : sym num (list-of TST) -> void
  (define (check-arity name arg# args)
    (if (>= (length args) arg#)
	(void)
	(error name "expects at least ~a arguments, given ~e" arg# (length args))))
  
  ;; check-proc : sym (... *->* ...) num (union sym str) (union sym str) -> void
  (define (check-proc proc f exp-arity arg# arg-err)
    (unless (procedure? f)
      (error proc "procedure expected as ~s argument; given ~e" arg# f))
    (unless (and (number? (arity f)) (= (arity f) exp-arity))
      (error proc
	     "procedure of ~a expected as ~s argument; given procedure of ~s args" 
	     arg-err arg# (arity f)))))