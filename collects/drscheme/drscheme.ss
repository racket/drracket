(module drscheme mzscheme

  ;; except for the mred and class libraries
  ;; (which should already be in this namespace anyway)
  ;; this module only has dynamic-requires to
  ;; get the timing right of the debug display, etc.
  
  (require (lib "mred.ss" "mred")
	   (lib "class.ss"))
  
  (define (show-profiling-results)
    (define f (make-object frame% "Profile Results" #f 400 600))
    (define t (make-object text%))
    (define ec (make-object editor-canvas% f t))
    (define p (open-output-string))

    (parameterize ([current-output-port p])
      ((dynamic-require '(lib "errortrace.ss" "errortrace") 'output-profile-results) #f #t)
      (newline)
      (newline)
      ((dynamic-require '(lib "errortrace.ss" "errortrace") 'output-profile-results) #f #f))
    (send t insert (get-output-string p))
    (send t auto-wrap #t)
    (send t change-style (make-object style-delta% 'change-family 'modern) 0 (send t last-position))
    (send t lock #t)
    (send f show #t))

  (define profiling? (equal? (getenv "PLTDRDEBUG") "profile"))

  (when (getenv "PLTDRDEBUG")
    (printf "PLTDRDEBUG: installing errortrace\n")
    (let ([main-eventspace-thread (current-thread)]
          [main-eventspace (current-eventspace)])
      (let-values ([(sw sh) (get-display-size)])
        (let* ([stack-frame (parameterize ([current-eventspace (make-eventspace)])
                              (make-object frame% "Debugging Window" #f 500))]
               [button-panel (make-object horizontal-panel% stack-frame)]
               [messages-panel (make-object vertical-panel% stack-frame)])
          
          (letrec ([mem (make-object message% "000.000.000" button-panel)]
                   [c (make-object canvas% button-panel)]
                   [quit-button (make-object button% "Quit" button-panel (lambda (x y) (exit)))]
                   [break-button (make-object button% "Break" button-panel 
                                   (lambda (x y) (break-thread main-eventspace-thread)))]
                   [profile-button (make-object button% "Profile Results" button-panel
                                     (lambda (x y)
                                       (show-profiling-results)))]
                   [toggle-profile-button
                    (make-object button% "Disable Profiling" button-panel
                      (lambda (x y)
                        (toggle-profiling-enabled)))]
                   
                   
                   (profiling-enabled? #f)
                   (toggle-profiling-enabled 
                    (lambda ()
                      (set! profiling-enabled? (not profiling-enabled?))
                      (sync-profiling-state)))
                   (sync-profiling-state
                    (lambda ()
                      (send toggle-profile-button set-label "Toggling...")
                      (send toggle-profile-button enable #f)
                      (parameterize ([current-eventspace main-eventspace])
                        (queue-callback
                         (lambda ()
                           (sync-profiling-state/main-eventspace)
                           (send toggle-profile-button enable #t))))))
                   (sync-profiling-state/main-eventspace
                    (lambda ()
                      ((dynamic-require '(lib "errortrace.ss" "errortrace")  
                                        'profiling-record-enabled)
                       profiling-enabled?)
                      (send toggle-profile-button set-label (if profiling-enabled? 
                                                                "Disable Profiling"
                                                                "Enable Profiling"))))
                   
                   
                   [onb (make-object bitmap% (build-path (collection-path "icons")
                                                         "recycle.gif"))]
                   [offb
                    (let ([bdc (make-object bitmap-dc%)]
                          [bitmap (make-object bitmap%
                                    (send onb get-width)
                                    (send onb get-height))])
                      (send bdc set-bitmap bitmap)
                      (send bdc clear)
                      (send bdc set-bitmap #f)
                      bitmap)])
            
            (thread
             (lambda ()
               (let loop ()
                 (sleep 1)
                 (let* ([mem-usage (current-memory-use)]
                        [spacer 1000]
                        [pad (lambda (x)
                               (cond
                                 [(x . < . 10) (format "00~a" x)]
                                 [(x . < . 100) (format "0~a" x)]
                                 [else (number->string x)]))]
                        [f1 (pad (modulo mem-usage spacer))]
                        [f2 (pad (modulo (quotient mem-usage spacer) spacer))]
                        [f3 (pad (quotient (quotient mem-usage spacer) spacer))])
                   (send mem set-label (string-append f3 "." f2 "." f1)))
                 (loop))))
            
            (register-collecting-blit c 0 0 (send onb get-width) (send onb get-height) onb offb)
            (send c min-width (send onb get-width))
            (send c min-height (send onb get-height))
            (send c stretchable-width #f)
            (send c stretchable-height #f)
            (send button-panel set-alignment 'center 'center)
            (unless profiling?
              (send profile-button enable #f)
              (send toggle-profile-button enable #f))
            (send profile-button stretchable-height #t)
            (send toggle-profile-button stretchable-height #t)
            (send quit-button stretchable-height #t)
            (send break-button stretchable-height #t)
          
            (let* ([new-message
                    (lambda ()
                      (let ([m (make-object message% "" messages-panel)])
                        (send m stretchable-width #t)
                        m))]
                   [messages (list (new-message)
                                   (new-message)
                                   (new-message)
                                   (new-message)
                                   (new-message)
                                   (new-message)
                                   (new-message)
                                   (new-message))]
                   [stack null]
                   [ol (current-load)]
                   [update-messages
                    (lambda ()
                      (let ([new-messages-needed (- (length stack) (length messages))])
                        (when (new-messages-needed . > . 0)
                          (let loop ([n new-messages-needed])
                            (unless (zero? n)
                              (let ([m (new-message)])
                                (set! messages (append messages (list m)))
                                (loop (- n 1)))))))
                      (let loop ([stack (reverse stack)]
                                 [messages messages])
                        (cond
                          [(null? messages) (void)]
                          [(null? stack) 
                           (for-each (lambda (m)
                                       (unless (equal? "" (send m get-label))
                                         (send m set-label "")))
                                     messages)]
                          [else
                           (let ([msg (car messages)]
                                 [fn (car stack)])
                             (unless (string=? (send msg get-label) fn)
                               (send msg set-label fn)))
                           (loop (cdr stack) (cdr messages))])))])
              (send stack-frame reflow-container)
              (send stack-frame move 
                    (- sw (send stack-frame get-width))
                    0)

              (dynamic-require '(lib "errortrace.ss" "errortrace") #f)
              (when profiling?
                (printf "PLTDRDEBUG: turning on profiling\n")
                ((dynamic-require '(lib "errortrace.ss" "errortrace") 'profiling-enabled) #t)
                (let ([enable-initially?
                       (message-box
                        "DrScheme"
                        "Turn on profiling during startup?"
                        #f
                        '(yes-no))])
                  (set! profiling-enabled? (eq? enable-initially? 'yes))
                  (sync-profiling-state/main-eventspace)))

              (send stack-frame show #t)
              (current-load
               (lambda (fn module)
                 (set! stack (cons fn stack))
                 (update-messages)
                 (begin0 (ol fn module)
                         (set! stack (cdr stack))
                         (update-messages))))

              (error-print-width 600)))))))

  (define texas-independence-day?
    (let ([date (seconds->date (current-seconds))])
      (and (= 3 (date-month date))
           (= 2 (date-day date)))))
  
  ((dynamic-require '(lib "splash.ss" "framework") 'start-splash)
   (build-path (collection-path "icons") 
               (cond
                 [texas-independence-day?
                  "texas-plt-bw.gif"]
                 [(= (get-display-depth) 1)
                  "pltbw.gif"]
                 [((get-display-depth) . <= . 8)
                  "plt-flat.gif"]
                 [else
                  "plt.jpg"]))
   "DrScheme"
   99)

  (when (getenv "PLTDRCM")
    (printf "PLTDRCM: installing compilation manager\n")
    (current-load/use-compiled
     ((dynamic-require '(lib "cm.ss") 'make-compilation-manager-load/use-compiled-handler))))
  
  (dynamic-require '(lib "start.ss" "drscheme" "private") #f))
