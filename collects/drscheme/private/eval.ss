(module eval mzscheme
  (require (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "pconvert.ss")
	   (lib "moddep.ss" "syntax")
           (lib "toplevel.ss" "syntax")
           "drsig.ss")
  
  (provide eval@)
  (define eval@
    (unit/sig drscheme:eval^
      (import [drscheme:language-configuration : drscheme:language-configuration/internal^]
              [drscheme:rep : drscheme:rep^]
              [drscheme:init : drscheme:init^]
              [drscheme:language : drscheme:language^])
      
      (define (expand-program input
                              language-settings
                              eval-compile-time-part? 
                              init
                              kill-termination
                              iter)
        (let-values ([(eventspace custodian) (build-user-eventspace/custodian
                                              language-settings
                                              init
                                              kill-termination)])
          (let ([language (drscheme:language-configuration:language-settings-language
                           language-settings)]
                [settings (drscheme:language-configuration:language-settings-settings
                           language-settings)])          
            (parameterize ([current-eventspace eventspace])
              (queue-callback
               (lambda ()
                 (let ([read-thnk (send language front-end input settings)])
                   (let loop ()
                     (let ([in (let ([rd (read-thnk)])
                                 (cond
                                   [(eof-object? rd) rd]
                                   [eval-compile-time-part? 
                                    (expand-top-level-with-compile-time-evals rd)]
                                   [else (expand rd)]))])
                       (cond
                         [(eof-object? in)
                          (iter in (lambda () (void)))]
                         [else
                          (iter in (lambda () (loop)))]))))))))))
      
      (define (build-user-eventspace/custodian language-settings init kill-termination)
        (let* ([eventspace (make-eventspace)]
               [language (drscheme:language-configuration:language-settings-language
                          language-settings)]
               [settings (drscheme:language-configuration:language-settings-settings
                          language-settings)]
               [user-custodian (make-custodian)]
               [eventspace-main-thread #f]
               [run-in-eventspace
                (lambda (thnk)
                  (parameterize ([current-eventspace eventspace])
                    (let ([sema (make-semaphore 0)]
                          [ans #f])
                      (queue-callback
                       (lambda ()
                         (let/ec k
                           (parameterize ([error-escape-handler
                                           (let ([drscheme-expand-program-error-escape-handler
                                                  (lambda () (k (void)))])
                                             drscheme-expand-program-error-escape-handler)])
                             (set! ans (thnk))))
                         (semaphore-post sema)))
                      (semaphore-wait sema)
                      ans)))]
               [drs-snip-classes (get-snip-classes)])
          (run-in-eventspace
           (lambda ()
             (current-custodian user-custodian)
             (set-basic-parameters drs-snip-classes)
             (drscheme:rep:current-language-settings language-settings)))
          (send language on-execute settings run-in-eventspace)
          (run-in-eventspace
           (lambda ()
             (set! eventspace-main-thread (current-thread))
             (init)
             (break-enabled #t)))
          (thread
           (lambda ()
             (thread-wait eventspace-main-thread)
             (kill-termination)))
          (values eventspace user-custodian)))
      
      ;; get-snip-classes : -> (listof snipclass)
      ;; returns a list of the snip classes in the current eventspace
      (define (get-snip-classes)
        (let loop ([n (send (get-the-snip-class-list) number)])
          (if (zero? n)
              null
              (cons (send (get-the-snip-class-list) nth (- n 1))
                    (loop (- n 1))))))
      
      ;; set-basic-parameters : (listof (is-a/c? snipclass%)) -> void
      ;; sets the parameters that are shared between the repl's initialization
      ;; and expand-program
      (define (set-basic-parameters snip-classes)
        (for-each (lambda (snip-class) (send (get-the-snip-class-list) add snip-class))
                  snip-classes)
        (read-curly-brace-as-paren #t)
        (read-square-bracket-as-paren #t)
        (error-print-width 250)
        (current-load drscheme-load-handler)
        (current-ps-setup (make-object ps-setup%))

        (let ([user-custodian (current-custodian)])
          (exit-handler (lambda (arg) ; =User=
                          (custodian-shutdown-all user-custodian))))
        (current-namespace (make-namespace 'empty))
        (for-each (lambda (x) (namespace-attach-module drscheme:init:system-namespace x))
                  to-be-copied-module-names))
      
      ;; these module specs are copied over to each new user's namespace 
      (define to-be-copied-module-specs
        (list 'mzscheme
              '(lib "mred.ss" "mred")))
      ;; just double check that they are all here.
      (for-each (lambda (x) (dynamic-require x #f)) to-be-copied-module-specs)
      ;; get the names of those modules.
      (define to-be-copied-module-names
        (let ([get-name
               (lambda (spec)
                 (if (symbol? spec)
                     spec
                     ((current-module-name-resolver) spec #f #f)))])
          (map get-name to-be-copied-module-specs)))
      
      ;; drscheme-load-handler : string ??? ->* TST
      ;; =User=
      ;; the default load handler for programs running in DrScheme
      (define (drscheme-load-handler filename expected-module)
        (unless (string? filename)
	  (raise-type-error 'drscheme-load-handler "string" filename))
        (let* ([input (build-input filename)]
               [ls (drscheme:rep:current-language-settings)]
               [language (drscheme:language-configuration:language-settings-language ls)]
               [settings (drscheme:language-configuration:language-settings-settings ls)]
               [thnk (send language front-end input settings)])
          (parameterize ([read-accept-compiled #t])
	    (if expected-module
		(with-module-reading-parameterization 
		 (lambda ()
		   (let* ([first (thnk)]
			  [module-ized-exp (check-module-form first expected-module filename)]
			  [second (thnk)])
		     (unless (eof-object? second)
		       (raise-syntax-error
			'drscheme-load-handler
			(format "expected only a `module' declaration for `~s', but found an extra expression"
				expected-module)
			second))
		     (eval module-ized-exp))))
		(let loop ([last-time-values (list (void))])
		  (let ([exp (thnk)])
		    (if (eof-object? exp)
			(apply values last-time-values)
			(call-with-values
			 (lambda () (eval exp))
			 (lambda x (loop x))))))))))
      
      ;; build-input : string[file-exists?] -> input
      ;; returns an input to be used with a language's `front-end' method
      (define (build-input filename)
        (let* ([p (open-input-file filename)]
               [chars (list (read-char p)
                            (read-char p)
                            (read-char p)
                            (read-char p))])
          (close-input-port p)
          (cond
            [(equal? chars (string->list "WXME"))
             (let ([text (make-object text%)])
               (send text load-file filename)
               (drscheme:language:make-text/pos text 0 (send text last-position)))]
            [else filename])))
      
      )))
