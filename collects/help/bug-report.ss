
(module bug-report mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "head.ss" "net")
           (lib "smtp.ss" "net")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "etc.ss"))
  
  (provide help-desk:report-bug)
  
  (define bug-email-server "bugs.plt-scheme.org")
  (define bug-email-server-port 1025)
  (define bug-report-email-address "bugs@plt-scheme.org")
  
  ;; hopefully these are defined by DrScheme...
  (define get-language-level
    (with-handlers ([(lambda (x) (not (exn:break? x)))
                     (lambda (x) (lambda () 'unknown))])
      (namespace-variable-binding 'get-language-level)))
  (define get-teachpack-filenames
    (with-handlers ([(lambda (x) (not (exn:break? x)))
                     (lambda (x) (lambda () 'unknown))])
      (namespace-variable-binding 'get-teachpack-filenames)))
  
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
            (ask-yes-or-no (string-constant cancel-bug-report?)
                           (string-constant are-you-sure-cancel-bug-report?)
                           this)))
      
      (super-make-object title)))
  
  (define (help-desk:report-bug)
    (define bug-frame (instantiate bug-frame% () (title (string-constant bug-report-form))))
    (define outermost-panel (make-object horizontal-panel% (send bug-frame get-area-container)))
    (define top-panel (make-object vertical-panel% outermost-panel))
    
    (define lps null)
    
    ; build/label : ((union string (list-of string)) (area-container<%> -> item<%>) boolean area-container<%> -> item<%>)
    ; constructs and arranges the gui objects for the bug report form
    ; effect: updates lps with the new label panel, for future alignment
    (define build/label
      (opt-lambda (text make-item top? [stretch? #f] [top-panel top-panel] [vertical? #f])
        (let*-values ([(hp) (make-object (if vertical?
                                             vertical-panel%
                                             horizontal-panel%)
                              top-panel)]
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
          (send lp stretchable-height #f)
          (send lp set-alignment (if vertical? 'left 'right) (if top? 'top 'center))
          (send ip set-alignment 'left 'top)
          item)))
    
    (define (align-labels)
      (let ([width (apply max (map (lambda (x) (send (car (send x get-children)) min-width))
                                   lps))])
        (for-each (lambda (x) (send x min-width width)) lps))) 
    
    (define name
      (build/label 
       (string-constant bug-report-field-name)
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
       (string-constant bug-report-field-email)
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
       (string-constant bug-report-field-summary) 
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void))))
       #f))
    
    (define severity
      (build/label 
       (string-constant bug-report-field-severity) 
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
       (string-constant bug-report-field-class)
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
       (string-constant bug-report-field-priority)
       (lambda (panel)
         (make-object choice%
           #f
           (list "high" "medium" "low")
           panel
           void))
       #f))
    
    (define (make-big-text label . args)
      (let ([canvas 
             (apply
              build/label 
              label 
              (lambda (panel)
                (let* ([text (make-object (editor:keymap-mixin text:basic%))]
                       [canvas (make-object canvas:basic% panel text)])
                  (send text set-style-list (scheme:get-style-list))
                  (send text set-styles-fixed #t)
                  canvas))
              #t
              args)])
        (send canvas min-width 500)
        (send canvas min-height 130)
        (send canvas get-editor)
        (send canvas allow-tab-exit #t)
        canvas))
    
    (define description (make-big-text (string-constant bug-report-field-description) #t))
    (define reproduce (make-big-text (list (string-constant bug-report-field-reproduce1)
                                           (string-constant bug-report-field-reproduce2))
                                     #t))
    
    (define synthesized-dialog (make-object dialog% (string-constant bug-report-synthesized-information)))
    (define synthesized-panel (make-object vertical-panel% synthesized-dialog))
    (define synthesized-button-panel (make-object horizontal-panel% synthesized-dialog))
    (define ok-button (make-object button% (string-constant ok) synthesized-button-panel
                        (lambda (x y)
                          (send synthesized-dialog show #f))))
    (define synthesized-info-shown? #t)
    (define (show-synthesized-info)
      (send synthesized-dialog show #t))
    
    (define version
      (build/label
       (string-constant bug-report-field-version)
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel
       #f))
    (define environment
      (build/label
       (string-constant bug-report-field-environment)
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel
       #f))
    
    (define human-language
      (build/label 
       (string-constant bug-report-field-human-language)
       (lambda (panel)            
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    
    (define language-level
      (build/label
       (string-constant bug-report-field-language)
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    
    (define teachpacks
      (build/label
       (string-constant bug-report-field-teachpacks)
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))

    (define docs-installed
      (make-big-text
       (string-constant bug-report-field-docs-installed)
       #t
       synthesized-panel))
    
    (define collections
      (make-big-text 
       (string-constant bug-report-field-collections)
       #t
       synthesized-panel))

    (define button-panel (make-object horizontal-panel% (send bug-frame get-area-container)))
    (define synthesized-button (make-object button%
                                 (string-constant bug-report-show-synthesized-info)
                                 button-panel (lambda x (show-synthesized-info))))
    (define ok-button (make-object button% (string-constant bug-report-submit) button-panel (lambda x (ok))))
    (define cancel-button (make-object button% (string-constant cancel) button-panel (lambda x (cancel))))
    (define grow-box-spacer-pane (make-object grow-box-spacer-pane% button-panel))
    
    ;; smtp-send-bug-report : -> void
    (define (smtp-send-bug-report)
      (smtp-send-message
       bug-email-server
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
          bug-report-email-address
          (insert-field
           "From"
           (format "~a <~a>" 
                   (preferences:get 'drscheme:full-name)
                   (preferences:get 'drscheme:email))
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
         "Docs Installed:" (format "~a" (send (send docs-installed get-editor) get-text))
         "Collections: "
         (format "~a" (send (send collections get-editor) get-text))
         (format "Human Language: ~a" (send human-language get-value))
         ">Fix: ")
        (cons
         ">Description:"
         (get-strings description))
        (cons
         ">How-To-Repeat:"
         (get-strings reproduce)))
       bug-email-server-port))
    
    ; send-bug-report : (-> boolean)
    ; returns true if uncancelled
    (define (send-bug-report)
      (letrec ([f (make-object dialog% (string-constant sending-bug-report) bug-frame)]
               [sema (make-semaphore 0)]
               [msg (make-object message% (string-constant sending-bug-report) f)]
               [button (make-object button% (string-constant cancel) f
                         (lambda (x y)
                           (break-thread smtp-thread)
                           (send f show #f)))]
               [smtp-thread
                (thread
                 (lambda ()
                   (semaphore-wait sema)
                   (send button enable #t)
                   (with-handlers ([(lambda (x) (exn:break? x))
                                    (lambda (x) 
                                      (void))]
                                   [(lambda (x) (not (exn:break? x)))
                                    (lambda (x)
                                      (queue-callback
                                       (lambda ()
                                         (message-box 
                                          (string-constant error-sending-bug-report)
                                          (format (string-constant error-sending-bug-report-expln)
                                                  (if (exn? x)
                                                      (exn-message x)
                                                      (format "~s" x))))))
                                      (send f show #f))])
                     (parameterize ([smtp-sending-end-of-message
                                     (lambda ()
                                       (send button enable #f))])
                       (smtp-send-bug-report)
                       (set! sucess? #t)
                       (send f show #f)))))]
               [sucess? #f])
        (send f center)
        (send button enable #f)
        (queue-callback (lambda () (semaphore-post sema)))
        (send f show #t)
        (when sucess?
          (message-box 
           (string-constant bug-report-sent)
           (string-constant bug-report-sent-detail)
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
               (message-box (string-constant illegal-bug-report)
                            (format (string-constant pls-fill-in-field) field-name))
               (done-checking #f)))
           (list name summary description reproduce)
           (list (string-constant bug-report-field-name)
                 (string-constant bug-report-field-summary)
                 (string-constant bug-report-field-description)
                 (string-append (string-constant bug-report-field-reproduce1) 
                                " "
                                (string-constant bug-report-field-reproduce2))))
          
          (unless (member #\@ (string->list (or (preferences:get 'drscheme:email) "")))
            (message-box (string-constant illegal-bug-report)
                         (string-constant malformed-email-address))
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
    (send (send collections get-editor)
          insert       
          (format "~s"
                  (map (lambda (x)
                         (list x 
                               (if (directory-exists? x)
                                   (directory-list x)
                                   "non-existant path")))
                       (current-library-collection-paths))))
    
    (send human-language set-value (this-language))
    
    (send (send collections get-editor) auto-wrap #t)
    (send (send docs-installed get-editor) auto-wrap #t)
    (send synthesized-button-panel set-alignment 'right 'center)
    
    (align-labels)
    (send button-panel set-alignment 'right 'center)
    (send button-panel stretchable-height #f)
    (send (if (string=? "" (preferences:get 'drscheme:full-name))
              name
              summary)
          focus)
    
    (send (send docs-installed get-editor) insert
          (format "~s"
                  (with-handlers ([(lambda (x) #t)
                                   (lambda (x) "none")])
                    (directory-list (collection-path "doc")))))
    (send teachpacks set-value (format "~s" (get-teachpack-filenames)))
    (send language-level set-value (format "~s" (get-language-level)))
    
    (send bug-frame show #t))
  
  (define (ask-yes-or-no title msg parent)
    (gui-utils:get-choice msg 
                          (string-constant yes)
                          (string-constant no)
                          title
                          #f
                          parent)))
