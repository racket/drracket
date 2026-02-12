#lang racket/base
  (provide break-threads drracket-splash-load-handler-step set-drracket-splash-load-handler-step!)
  (define super-cust (current-custodian))
  (define first-child (make-custodian))
  (current-custodian first-child)
  
  
  (define (break-threads)
    (parameterize ([current-custodian super-cust])
      (thread
       (λ ()
         (let loop ([super-cust super-cust]
                    [current-cust first-child])
           (for ([man (in-list (custodian-managed-list current-cust super-cust))])
             (when (thread? man)
               (break-thread man))
             (when (custodian? man)
               (loop current-cust man))))))))

(define drracket-splash-load-handler-step void)
(define (set-drracket-splash-load-handler-step! f) (set! drracket-splash-load-handler-step f))
