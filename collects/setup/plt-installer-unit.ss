(module plt-installer-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "mred-sig.ss" "mred")
           (lib "class.ss")
           
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
      
      (define re:newline (regexp "\n"))
      
      ;; with-installer-window : ((union (instanceof dialog%) (instanceof frame%)) -> void) -> void
      ;; creates a frame and sets up the current error and output ports
      ;; before calling `do-install'. When it returns, closes down the
      ;; frame.
      (define (with-installer-window do-install)
        (letrec ([frame (make-object (class dialog% ()
                                       (define/override can-close? (lambda () (send done is-enabled?)))
                                       (define/override on-close (lambda () (semaphore-post s)))
                                       (super-make-object
                                        "Install Progress"
                                        #f 600 300 #f #f '(resize-border))))]
                 [c (make-object editor-canvas% frame)]
                 [e (make-object text%)]
                 [s (make-semaphore)]
                 [done (make-object button% "Close" frame (lambda (b e) (semaphore-post s)))]
                 [output (make-output-port
                          (lambda (s)
                            (send e lock #f)
                            (send e insert s (send e last-position) 'same 
                                  ; Scroll on newlines only:
                                  (regexp-match re:newline s))
                            (send e lock #t))
                          void)]
                 [cust (make-custodian)])
          (send done enable #f)
	  ((current-text-keymap-initializer) (send e get-keymap))
          (send e lock #t)
          (send c set-editor e)
          
          (begin-busy-cursor)
          (thread (lambda () (send frame show #t)))
          
          (parameterize ([current-output-port output]
                         [current-error-port output])
            (do-install frame))
          
          (end-busy-cursor)
          (send done enable #t)
          (yield s)
          (send frame show #f)
          ((on-installer-run))))
      
      ;; run-single-installer : string (union (instanceof frame%) (instanceof dialog%)) -> void
      ;; creates a separate thread, runs the installer in that thread,
      ;; returns when the thread completes
      (define (run-single-installer file parent)
        (let ([cust (make-custodian)])
          (parameterize ([current-custodian cust])
            (let ([thd
                   (thread
                    (lambda ()
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
                                                         (let ([d (get-directory 
                                                                   "Select the destination for unpacking"
                                                                   parent)])
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
                           (export))))))])
              (thread-wait thd)))))
      
      (define (run-installer file)
        (with-installer-window 
         (lambda (frame)
           (run-single-installer file frame))))
      
      '(define (run-installer file)
         (letrec ([f (make-object (class dialog% ()
                                    (define/override (can-close?) (send done is-enabled?))
                                    (define/override (on-close) (semaphore-post s))
                                    (super-make-object
                                     "Install Progress"
                                     #f 600 300 #f #f '(resize-border))))]
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
                                   (regexp-match re:newline s))
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

