
(module plt-installer-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "mred-sig.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss")
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
	   (lib "compiler-unit.ss" "compiler")
           (lib "string-constant.ss" "string-constants"))
  
  (provide plt-installer@)
  
  (define plt-installer@
    (unit/sig setup:plt-installer^
      (import mred^)
      
      (define on-installer-run
        (make-parameter void))
      
      (define re:newline (regexp "\n"))
      
      ;; with-installer-window : ((union (instanceof dialog%) (instanceof frame%)) -> void) (-> void) -> void
      ;; creates a frame and sets up the current error and output ports
      ;; before calling `do-install'. 
      ;; runs the installer in a separate thread and returns immediately,
      ;; before the installation is complete. The cleanup thunk is called when installation completes
      (define (with-installer-window do-install cleanup-thunk)
        (let ([orig-eventspace (current-eventspace)]
              [orig-custodian (current-custodian)]
              [inst-eventspace (make-eventspace)])
          (parameterize ([current-eventspace inst-eventspace])
            (letrec ([dlg (make-object (class dialog% ()
                                         (define/override can-close? (lambda () (send done is-enabled?)))
                                         (define/override on-close (lambda () (done-callback)))
                                         (super-make-object
                                          (string-constant plt-installer-progress-window-title)
                                          #f 600 300 #f #f '(resize-border))))]
                     [text (make-object text%)]
                     [canvas (make-object editor-canvas% dlg text)]
                     [button-panel (instantiate horizontal-panel% ()
                                     (parent dlg)
                                     (stretchable-height #f)
                                     (alignment '(center center)))]
                     [kill-button (make-object button% "Kill" button-panel (lambda (b e) (kill)))]
                     [done (make-object button% "Close" button-panel (lambda (b e) (done-callback)))]
                     [output (make-custom-output-port
                              #f
                              (lambda (s start end flush?)
                                (parameterize ([current-eventspace inst-eventspace])
                                  (queue-callback
                                   (lambda ()
                                     (let ([s (substring s start end)])
                                       (send text lock #f)
                                       (send text insert s (send text last-position) 'same 
                                             ; Scroll on newlines only:
                                             (regexp-match re:newline s))
                                       (send text lock #t)))))
                                (- end start))
                              void
                              void)]
                     [kill
                      (lambda ()
                        (custodian-shutdown-all installer-cust)
                        (fprintf output "\nKilled.\n")
                        (send done enable #t))]
                     [done-callback
                      (lambda ()
                        (send dlg show #f)
                        (custodian-shutdown-all installer-cust))]
                     [installer-cust (make-custodian)])
              (send done enable #f)
              ((current-text-keymap-initializer) (send text get-keymap))
              (send text lock #t)
              
              ;; still do this even tho we aren't in the eventspace main thread
              (thread (lambda () (send dlg show #t)))
              
              (parameterize ([current-custodian installer-cust])
                (parameterize ([current-eventspace (make-eventspace)])
                  (queue-callback
                   (lambda ()
                     
                     (let ([installer-thread (current-thread)])
                       (parameterize ([current-custodian orig-custodian])
                         (thread
                          (lambda ()
                            (thread-wait installer-thread)
                            (send kill-button enable #f)))))
                    
                     (parameterize ([current-output-port output]
                                    [current-error-port output])
                       (do-install dlg))
                     (parameterize ([current-eventspace orig-eventspace])
                       (queue-callback
                        (lambda ()
                          ((on-installer-run))
                          (cleanup-thunk))))
                     
                     (send done enable #t)))))))))
      
      ;; run-single-installer : string (union (instanceof frame%) (instanceof dialog%)) -> void
      ;; creates a separate thread, runs the installer in that thread,
      ;; returns when the thread completes
      (define (run-single-installer file parent)
        (let ([cust (make-custodian)])
          (parameterize ([current-custodian (make-custodian)]
                         [exit-handler (lambda (v) (custodian-shutdown-all cust))])
            (let ([thd
                   (thread
                    (lambda ()
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
                         (export)))))])
              (thread-wait thd)))))
      
      (define run-installer 
        (opt-lambda (file [cleanup-thunk void])
          (with-installer-window 
           (lambda (frame)
             (run-single-installer file frame))
           cleanup-thunk))))))