
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
      
      (define original-error-display-handler (error-display-handler))
            
      ;; override error-display-handler to duplicate the error
      ;; message in both the standard place (as defined by the
      ;; current error-display-handler) and in a message box
      ;; identifying the error as a drscheme internal error.
      (error-display-handler
       (lambda (msg exn)
         (original-error-display-handler msg exn)
         (let ([text (let ([p (open-output-string)])
                       (parameterize ([current-error-port p]
                                      [current-output-port p])
                         (original-error-display-handler msg exn))
                       (get-output-string p))])
           (if (eq? (current-eventspace) system-eventspace)
               (message-box "DrScheme Internal Error" text)
               (parameterize ([current-eventspace system-eventspace]
                              [current-custodian system-custodian])
                 (queue-callback
                  (lambda ()
                    (message-box "DrScheme Internal Error" text)))))))))))
