(module plt-installer-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "mred-sig.ss" "mred")
           (lib "class.ss")
           (lib "class100.ss")

           "plt-installer-sig.ss"

	   ;; All the rest are to get the imports for setup@:
	   "option-sig.ss"
	   "setup-unit.ss"
	   "option-unit.ss"
	   (lib "launcher-sig.ss" "launcher")
	   (lib "launcher-unit.ss" "launcher")
	   (lib "dynext-sig.ss" "dynext")
	   (lib "dynext-unit.ss" "dynext")
	   (lib "sig.ss" "compiler")
	   (lib "option-unit.ss" "compiler")
	   (lib "compiler-unit.ss" "compiler"))
  
  (provide plt-installer@)
  
  (define plt-installer@
    (unit/sig setup:plt-installer^
      (import mred^)
      
      (define on-installer-run
        (make-parameter void))
      
      (define (run-installer file)
        (letrec ([f (make-object (class100 dialog% ()
                                   (override
                                     [can-close? (lambda () (send done is-enabled?))]
				     [on-close (lambda () (semaphore-post s))])
                                   (sequence
                                     (super-init "Install Progress"
                                                 #f 600 300 #f #f '(resize-border)))))]
                 [c (make-object editor-canvas% f)]
                 [e (make-object text%)]
                 [s (make-semaphore)]
                 [done (make-object button% "Close" f (lambda (b e) 
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
	  ((current-text-keymap-initializer) (send e get-keymap))
          (send e lock #t)
          (send c set-editor e)
          (let ([t (parameterize ([current-custodian cust])
                     (thread
                      (lambda ()
                        (current-output-port output)
                        (current-error-port output)
                        (parameterize ([exit-handler (lambda (v) (custodian-shutdown-all cust))])
			  (invoke-unit/sig
			   (compound-unit/sig
			    (import)
			    (link [launcher : launcher^ (launcher@ dcompile dlink)]
				  [dcompile : dynext:compile^ (dynext:compile@)]
				  [dlink : dynext:link^ (dynext:link@)]
				  [dfile : dynext:file^ (dynext:file@)]
				  [option : compiler:option^ (compiler:option@)]
				  [compiler : compiler^ (compiler@
							 option
							 dcompile
							 dlink
							 dfile)]
				  [soption : setup-option^ (setup:option@)]
				  [set-options : () ((unit/sig ()
						       (import setup-option^)
						       ;; >>>>>>>>>>>>>> <<<<<<<<<<<<<<<
						       ;; Here's where we tell setup the archive file!
						       (archives (list file))
						       ;; Here's where we make get a directory:
						       (current-target-directory-getter
							(lambda ()
							  (sleep 0.2) ; kludge to allow f to appear first
							  (end-busy-cursor)
							  (let ([d (get-directory "Select the destination for unpacking" f)])
							    (unless d
							      (printf ">>> Cancelled <<<~n"))
							    (begin-busy-cursor)
							    d))))
						     soption)]
				  [setup : () (setup@
					       SOPTION
					       compiler
					       option
					       launcher)])
			    (export)))))))])
            (thread (lambda () (send f show #t) (semaphore-post s)))
            (thread (lambda () 
                      (thread-wait t) 
                      (semaphore-post s)))
            (yield s)
            (custodian-shutdown-all cust)
            (end-busy-cursor)
            (send done enable #t)
            (yield s)
            (begin-busy-cursor)
            (send f show #f)
            (yield s)
            ((on-installer-run))))))))

