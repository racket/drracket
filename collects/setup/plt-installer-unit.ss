(module plt-installer-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "mred-sig.ss" "mred"))
  (provide plt-installer@)
  
  (define plt-installer@
    (unit/sig setup:plt-installer^
      (import mred^)
      
      (define on-installer-run
        (make-parameter void))
      
      (define (run-installer file)
        (letrec ([f (make-object (class dialog% ()
                                   (override
                                     [can-close? (lambda () (send done is-enabled?))])
                                   (sequence
                                     (super-init "Install Progress"
                                                 #f 600 300 #f #f '(resize-border)))))]
                 [c (make-object editor-canvas% f)]
                 [e (make-object text%)]
                 [s (make-semaphore)]
                 [done (make-object button% "Ok" f (lambda (b e) 
                                                     (semaphore-post s)))]
                 [output (make-output-port
                          (lambda (s)
                            (send e lock #f)
                            (send e insert s (send e last-position) 'same 
                                  ; Scroll on newlines only:
                                  (regexp-match (string #\newline) s))
                            (send e lock #t))
                          void)]
                 [cust (make-custodian)])
          (send done enable #f)
          (send e lock #t)
          (send c set-editor e)
          (let ([t (parameterize ([current-custodian cust])
                     (thread
                      (lambda ()
                        (current-output-port output)
                        (current-error-port output)
                        (parameterize ([current-namespace (make-namespace 'mred)]
                                       [exit-handler (lambda (v) (custodian-shutdown-all cust))])
                          (printf "Loading installer...~n")
                          (global-defined-value 'argv (vector file))
                          (require-library "setup.ss" "setup")))))])
            (thread (lambda () (send f show #t) (semaphore-post s)))
            (thread (lambda () 
                      (thread-wait t) 
                      (semaphore-post s)))
            (yield s)
            (custodian-shutdown-all cust)
            (end-busy-cursor)
            (send done enable #t)
            (fprintf output "(Click Ok to close this progress window.)~n")
            (send e lock #f)
            (send e change-style
                  (make-object style-delta% 'change-bold)
                  (send e line-start-position (sub1 (send e last-line)))
                  (send e last-position))
            (yield s)
            (begin-busy-cursor)
            (send f show #f)
            (yield s)
            ((on-installer-run))))))))

