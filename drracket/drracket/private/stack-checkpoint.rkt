#lang racket/base
(provide cut-stack-at-checkpoint with-stack-checkpoint)

;; run a thunk, and if an exception is raised, make it possible to cut the
;; stack so that the surrounding context is hidden
(define checkpoints (make-weak-hasheq))
(define (call-with-stack-checkpoint thunk)
  (define checkpoint #f)
  (call-with-exception-handler
   (λ (exn)
     (when checkpoint ; just in case there's an exception before it's set
       (define key (if (exn? exn) (exn-continuation-marks exn) exn))
       (unless (hash-has-key? checkpoints key)
         (hash-set! checkpoints key checkpoint)))
     exn)
   (lambda ()
     (set! checkpoint (current-continuation-marks))
     (thunk))))
;; returns the stack of the input exception, cutting off any tail that was
;; registered as a checkpoint
(define (cut-stack-at-checkpoint cont-marks)
  (define stack (continuation-mark-set->context cont-marks))
  (define checkpoint
    (cond [(hash-ref checkpoints cont-marks #f) => continuation-mark-set->context]
          [else #f]))
  (define stack-with-gaps-and-extra-info
    (if checkpoint
        (let loop ([st stack]
                   [sl (length stack)]
                   [cp checkpoint]
                   [cl (length checkpoint)])
          (cond [(sl . > . cl) (cons (car st) (loop (cdr st) (sub1 sl) cp cl))]
                [(sl . < . cl) (loop st sl (cdr cp) (sub1 cl))]
                [(equal? st cp) '()]
                [else (loop st sl (cdr cp) (sub1 cl))]))
        stack))
  (map cdr (filter cdr stack-with-gaps-and-extra-info)))

(define-syntax-rule (with-stack-checkpoint expr)
  (call-with-stack-checkpoint (lambda () expr)))

