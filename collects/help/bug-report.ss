(module bug-report mzscheme
  (require (lib "head.ss" "net")
           (lib "smtp.ss" "net")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss"))
  
  (provide help-desk:report-bug)
  
  ;; hopefully these are defined by DrScheme...
  (define get-language-level
    (with-handlers ([(lambda (x) (not (exn:break? x)))
                     (lambda (x) (lambda () 'unknown))])
      (namespace-variable-binding 'get-language-level)))
  (define get-teachpack-names
    (with-handlers ([(lambda (x) (not (exn:break? x)))
                     (lambda (x) (lambda () 'unknown))])
      (namespace-variable-binding 'get-teachpack-names)))
  
  (preferences:set-default 'drscheme:email "" string?)
  (preferences:set-default 'drscheme:full-name "" string?)

  (define bug-frame%
    (class (frame:standard-menus-mixin frame:basic%)
      (init title)
      (override file-menu:create-new?
                file-menu:create-open?
                file-menu:between-save-as-and-print
                file-menu:between-print-and-close
                
                edit-menu:create-preferences?
                edit-menu:between-find-and-preferences
                edit-menu:between-select-all-and-find)
      
      (define (file-menu:create-new?) #f)
      (define (file-menu:create-open?) #f)
      (define (file-menu:between-save-as-and-print menu) (void))
      (define (file-menu:between-print-and-close menu) (void))
      (define (edit-menu:create-preferences?) #f)
      (define (edit-menu:between-find-and-preferences menu) (void))
      (define (edit-menu:between-select-all-and-find menu) (void))
      
      (override can-close?)
      (field (ok-to-close? #f))
      (public set-ok-to-close)
      (define (set-ok-to-close ok?) (set! ok-to-close? #t))
      (define (can-close?)
        (or ok-to-close?
            (eq? 'yes
                 (message-box "Cancel Bug Report?"
                              "Are you sure that you want to cancel sending this bug report?"
                              this
                              '(yes-no)))))
      
      (super-make-object title)))
  
  (define (help-desk:report-bug)
    (define bug-frame (instantiate bug-frame% () (title "Bug Report Form")))
    (define top-panel (make-object vertical-panel% (send bug-frame get-area-container)))
    
    (define lps null)
    
    ; build/label : ((union string (list-of string)) (area-container<%> -> item<%>) boolean area-container<%> -> item<%>)
    ; constructs and arranges the gui objects for the bug report form
    ; effect: updates lps with the new label panel, for future alignment
    (define build/label
      (case-lambda
       [(text make-item top?)
        (build/label text make-item top? #f)]
       [(text make-item top? stretch?)
        (build/label text make-item top? stretch? top-panel)]
       [(text make-item top? stretch? top-panel)
        (let*-values ([(hp) (make-object horizontal-panel% top-panel)]
                      [(lp) (make-object vertical-panel% hp)]
                      [(ip) (make-object vertical-panel% hp)]
                      [(label/s) (if (string? text)
                                     (make-object message% text lp)
                                     (map (lambda (s)
                                            (make-object message% s lp))
                                          text))]
                      [(item) (make-item ip)])
          (set! lps (cons lp lps))
          (unless stretch? 
            (send hp stretchable-height #f)
            (send lp stretchable-height #f)
            (send ip stretchable-height #f))
          (send lp stretchable-width #f)
          (send lp set-alignment 'right (if top? 'top 'center))
          (send ip set-alignment 'left 'top)
          item)]))
    
    (define (align-labels)
      (let ([width (apply max (map (lambda (x) (send (car (send x get-children)) min-width))
                                   lps))])
        (for-each (lambda (x) (send x min-width width)) lps))) 
    
    (define name
      (build/label 
       "Name"
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel
              (lambda (text event)
                (preferences:set 'drscheme:full-name (send text get-value)))
              (preferences:get 'drscheme:full-name)))))
       #f))
    
    (define email  
      (build/label
       "Email"
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel
              (lambda (text event)
                (preferences:set 'drscheme:email (send text get-value)))
              (preferences:get 'drscheme:email)))))
       #f))
    
    (define summary
      (build/label
       "Summary" 
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void))))
       #f))
    
    
    (define severity
      (build/label 
       "Severity" 
       (lambda (panel)
         (make-object choice% 
           #f
           (list "critical" "serious" "non-critical")
           panel
           void))
       #f))
    
    (define bug-classes '(("software bug" "sw-bug")
                          ("documentation bug" "doc-bug")
                          ("change request" "change-request")
                          ("support" "support")))
    
    (define bug-class
      (build/label
       "Class" 
       (lambda (panel)
         (make-object choice%
           #f
           (map car bug-classes)
           panel
           void))
       #f))
    
    (define (translate-class class)
      (cadr (assoc class bug-classes)))
    
    (define priority
      (build/label
       "Priority" 
       (lambda (panel)
         (make-object choice%
           #f
           (list "high" "medium" "low")
           panel
           void))
       #f))
    
    (define modern-style-list (make-object style-list%))
    
    (define (make-big-text label)
      (let ([canvas 
             (build/label 
              label 
              (lambda (panel)
                (let* ([text (make-object (editor:keymap-mixin text:basic%))]
                       [canvas (make-object editor-canvas% panel text)])
                  (send text set-style-list modern-style-list)
                  canvas))
              #t
              #t)])
        (send canvas min-width 500)
        (send canvas min-height 130)
        (send canvas get-editor)
        (send canvas allow-tab-exit #t)
        canvas))
    
    (define description (make-big-text "Description"))
    (define reproduce (make-big-text '("Steps to" "Reproduce")))
    
    (define synthesized-outer-panel (make-object vertical-panel% top-panel))
    (define synthesized-panel (make-object vertical-panel% synthesized-outer-panel))
    (define synthesized-info-shown? #t)
    (define (toggle-synthesized-info)
      (cond
        [synthesized-info-shown?
         (set! synthesized-info-shown? #f)
         (send synthesized-button set-label "Show Synthesized Info")
         (send synthesized-outer-panel change-children (lambda (l) null))]
        [else
         (set! synthesized-info-shown? #t)
         (send synthesized-button set-label "Hide Synthesized Info")
         (send synthesized-outer-panel change-children (lambda (l) (list synthesized-panel)))]))
    
    (define version
      (build/label
       "Version"
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    (define environment
      (build/label
       "Environment"
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    
    (define tools
      (build/label
       "Tools"
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    (define docs-installed
      (build/label 
       "Docs Installed"
       (lambda (panel)            
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    
    (define language-level
      (build/label
       "Language"
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    
    (define teachpacks
      (build/label
       "Teachpacks"
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    
    (define collections
      (build/label 
       "Collections"
       (lambda (panel)            
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    
    (define button-panel (make-object horizontal-panel% (send bug-frame get-area-container)))
    (define synthesized-button (make-object button% "Show Synthesized Info" button-panel (lambda x (toggle-synthesized-info))))
    (define ok-button (make-object button% "Submit" button-panel (lambda x (ok))))
    (define cancel-button (make-object button% "Cancel" button-panel (lambda x (cancel))))
    (define grow-box-spacer-pane (make-object grow-box-spacer-pane% button-panel))
    
    (define (smtp-send-bug-report)
      (smtp-send-message
       "cs.rice.edu"
       (preferences:get 'drscheme:email)
       (list "plt-gnats")
       (insert-field
        "X-Mailer"
        (format "Help Desk ~a (bug report form)" (version:version))
        (insert-field     
         "Subject" 
         (send summary get-value)
         (insert-field
          "To"
          "plt-gnats@cs.rice.edu"
          (insert-field
           "From"
           (preferences:get 'drscheme:email)
           empty-header))))
       (append
        (list
         ">Category:       all"
         (format ">Synopsis:       ~a" (send summary get-value))
         ">Confidential:   no"
         (format ">Severity:       ~a" (send severity get-string-selection))
         (format ">Priority:       ~a" (send priority get-string-selection))
         (format ">Class:          ~a" (translate-class (send bug-class get-string-selection)))
         ">Submitter-Id:   unknown"
         (format ">Originator:     ~a" (preferences:get 'drscheme:full-name))
         ">Organization:"
         "titan"
         (format ">Release:        ~a" (send version get-value))
         ">Environment:"
         (format "~a" (send environment get-value))
         (format "Tools: ~a" (send tools get-value))
         "Docs Installed:" (format "~a" (send docs-installed get-value))
         "Collections:"
         (format "~a" (send collections get-value))
         ">Fix: ")
        (cons
         ">Description:"
         (get-strings description))
        (cons
         ">How-To-Repeat:"
         (get-strings reproduce)))))
    
    ; send-bug-report : (-> boolean)
    ; returns true if uncancelled
    (define (send-bug-report)
      (letrec ([f (make-object dialog% "Sending Bug Report" bug-frame)]
               [sema (make-semaphore 0)]
               [msg (make-object message% "Sending Bug Report" f)]
               [button (make-object button% "Cancel" f
                         (lambda (x y)
                           (break-thread smtp-thread)
                           (send f show #f)))]
               [smtp-thread
                (thread
                 (lambda ()
                   (semaphore-wait sema)
                   (send button enable #t)
                   (parameterize ([smtp-sending-end-of-message
                                   (lambda ()
                                     (send button enable #f))])
                     (smtp-send-bug-report)
                     (set! sucess? #t)
                     (send f show #f))))]
               [sucess? #f])
        (send f center)
        (send button enable #f)
        (queue-callback (lambda () (semaphore-post sema)))
        (send f show #t)
        (when sucess?
          (message-box 
           "Bug Report Sent"
           "Thanks for the report. You should receive a confirmation email in the next 30 minutes. If you do not, send email to scheme@cs.rice.edu."
           bug-frame))
        sucess?))
    
    (define (get-strings canvas)
      (let ([t (send canvas get-editor)])
        (let loop ([n 0])
          (cond
            [(> n (send t last-paragraph)) null]
            [else (cons (send t get-text
                              (send t paragraph-start-position n)
                              (send t paragraph-end-position n))
                        (loop (+ n 1)))]))))
    
    
    
    (define (sanity-checking)
      (let ([no-value?
             (lambda (f)
               (cond
                 [(is-a? f editor-canvas%)
                  (= 0 (send (send f get-editor) last-position))]
                 [else (string=? "" (send f get-value))]))])
        (let/ec done-checking
          (for-each
           (lambda (field field-name)
             (when (no-value? field)
               (message-box "Illegal Bug Report"
                            (format "Please fill in the \"~a\" field" field-name))
               (done-checking #f)))
           (list name summary description reproduce)
           (list "Name" "Summary" "Description" "Steps to Reproduce"))
          
          
          (unless (member #\@ (string->list (or (preferences:get 'drscheme:email) "")))
            (message-box "Illegal Bug Report" "Malformed email address")
            (done-checking #f))
          (done-checking #t))))
    
    (define (ok)
      (when (sanity-checking)
        (let ([submitted? (send-bug-report)])
          (when submitted?
            (send bug-frame set-ok-to-close #t)
            (cleanup-frame)))))
    
    (define (cancel)
      (cleanup-frame))
    
    (define (cleanup-frame)
      (send bug-frame close))
    
    (let ([delta (make-object style-delta% 'change-normal)]
          [style (send modern-style-list find-named-style "Standard")])
      (send delta set-delta 'change-family 'modern)
      (if style
          (send style set-delta delta)
          (send modern-style-list new-named-style "Standard"
                (send modern-style-list find-or-create-style
                      (send modern-style-list find-named-style "Basic")
                      delta))))
    
    (send severity set-selection 1)
    (send priority set-selection 1)
    (send version set-value   
          (format "~a"
                  (version:version)))
    
    (send environment set-value   
          (format "~a ~s (~a) (get-display-depth) = ~a"
                  (system-type)
                  (system-type #t)
                  (system-library-subpath)
                  (get-display-depth)))
    (send tools set-value 
          (format "~s"
		  (with-handlers ([not-break-exn?
				   (lambda (x)
				     (exn-message x))])
		    (directory-list (collection-path "drscheme" "tools")))))
    
    (send collections set-value       
          (format "~s"
                  (map (lambda (x) 
                         (list x 
                               (if (directory-exists? x)
                                   (directory-list x)
                                   "non-existant path")))
                       (current-library-collection-paths))))
    
    (align-labels)
    (send button-panel set-alignment 'right 'center)
    (send button-panel stretchable-height #f)
    (send (if (string=? "" (preferences:get 'drscheme:full-name))
              name
              summary)
          focus)
    
    (send docs-installed set-value       
          (format "~s"
                  (with-handlers ([(lambda (x) #t)
                                   (lambda (x) "none")])
                    (directory-list (collection-path "doc")))))
    (send teachpacks set-value (format "~s" (get-teachpack-names)))
    (send language-level set-value (format "~s" (get-language-level)))
    
    (send synthesized-panel stretchable-height #f)
    (send synthesized-outer-panel stretchable-height #f)
    
    (toggle-synthesized-info)
    
    (send bug-frame show #t)))
