
(module init mzscheme
  (require (lib "unitsig.ss")
           "drsig.ss"
	   (lib "mred.ss" "mred"))

  (provide init@)
  
  (define init@
    (unit/sig drscheme:init^
      (import)
      
      (define original-output-port (current-output-port))
      (define original-error-port (current-error-port))
      
      (define primitive-eval (current-eval))
      (define primitive-load (current-load))
      
      (print-struct #t)
      (break-enabled #f)
      
      (define system-custodian (current-custodian))
      (define system-eventspace (current-eventspace))
      (define system-thread (current-thread))
      (define first-dir (current-directory))
      
      (error-display-handler
       (lambda (msg)
         (display msg)
         (newline)
         (if (eq? (current-eventspace) system-eventspace)
             (message-box "DrScheme Internal Error" msg)
             (parameterize ([current-eventspace system-eventspace]
                            [current-custodian system-custodian])
               (queue-callback
                (lambda ()
                  (message-box "DrScheme Internal Error" msg))))))))))
