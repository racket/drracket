
(module app mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   (lib "mred.ss" "mred")
           "drsig.ss"
           (lib "framework.ss" "framework")
           (lib "file.ss"))
  
  (provide app@)
  (define app@
    (unit/sig drscheme:app^
      (import [drscheme:unit : drscheme:unit^]
              [drscheme:frame : drscheme:frame^]
              [help-desk : drscheme:help-desk^]
              [drscheme:tools : drscheme:tools^])

      (define about-frame%
        (class100 (drscheme:frame:basics-mixin (frame:standard-menus-mixin frame:basic%)) (_main-text)
          (private-field [main-text _main-text])
          (private
            [edit-menu:do 
             (lambda (const)
	       (send main-text do-edit-operation const))])
          (override
            [file-menu:create-revert? (lambda () #f)]
            [file-menu:create-save? (lambda () #f)]
            [file-menu:create-save-as? (lambda () #f)]
            [file-menu:between-close-and-quit (lambda (x) (void))]
            [file-menu:between-print-and-close (lambda (x) (void))]
            [edit-menu:between-redo-and-cut (lambda (x) (void))]
            [edit-menu:between-select-all-and-find (lambda (x) (void))]
            [edit-menu:copy-callback (lambda (menu evt) (edit-menu:do 'copy))]
            [edit-menu:select-all-callback (lambda (menu evt) (edit-menu:do 'select-all))]
            [edit-menu:create-find? (lambda () #f)])
          (sequence
            (super-init (string-constant about-drscheme-frame-title)))))
      
      (define (check-new-version)
        (let ([this-version (version:version)]
              [last-version (preferences:get 'drscheme:last-version)]
              [last-language (preferences:get 'drscheme:last-language)])
          (when (or (not last-version)
                    (not last-language)
                    (not (equal? last-version this-version))
                    (not (equal? last-language (this-language))))
            (invite-tour))))
      
      (define (same-widths items)
        (let ([max-width (apply max (map (lambda (x) (send x get-width)) items))])
          (for-each (lambda (x) (send x min-width max-width)) items)))
      
      (define (same-heights items)
        (let ([max-height (apply max (map (lambda (x) (send x get-height)) items))])
          (for-each (lambda (x) (send x min-height max-height)) items)))
      
      (define names
        (string-append
         "PLT is "
         "John Clements, Matthias Felleisen, Robby Findler, "
         "Cormac Flanagan, Matthew Flatt, "
         "Shriram Krishnamurthi, "
         "and "
         "Paul Steckler."))
      
      (define wrap-edit% 
        (class100-asi text%
          (inherit begin-edit-sequence end-edit-sequence
                   get-max-width find-snip position-location)
          (rename [super-after-set-size-constraint after-set-size-constraint])
          (override
            [on-set-size-constraint
             (lambda ()
               (begin-edit-sequence)
               (let ([snip (find-snip 1 'after-or-none)])
                 (when (is-a? snip editor-snip%)
                   (send (send snip get-editor) begin-edit-sequence))))]
            [after-set-size-constraint
             (lambda ()
               (super-after-set-size-constraint)
               (let ([width (get-max-width)]
                     [snip (find-snip 1 'after-or-none)])
                 (when (is-a? snip editor-snip%)
                   (let ([b (box 0)])
                     (position-location 1 b #f #f #t)
                     (let ([new-width (- width 4 (unbox b))])
                       (when (> new-width 0)
                         (send snip resize new-width
                               17) ; smallest random number
                         (send snip set-max-height 'none))))
                   (send (send snip get-editor) end-edit-sequence)))
               (end-edit-sequence))])))
      
      (define (get-plt-bitmap)
        (make-object bitmap%
          (build-path (collection-path "icons")
                      (if (< (get-display-depth) 8)
                          "pltbw.gif"
                          "plt.gif"))))
      
      (define (make-tour-button button-panel)
        (make-object button% (string-constant take-a-tour) button-panel
          (lambda x 
            (help-desk:open-url
             (string-append
              "file:"
              (build-path (collection-path "doc" "help" "tour")
                          "index.html"))))
          '(border)))
      
      
      (define (make-release-notes-button button-panel)
        (make-object button% (string-constant release-notes) button-panel
          (lambda x 
            (help-desk:open-url 
             (string-append
              "file:"
              (build-path (collection-path "doc" "help" "release")
                          "notes.html"))))))
      
      (define tour-frame%
        (class (drscheme:frame:basics-mixin (frame:standard-menus-mixin frame:basic%))
	  (init-rest args)

          (override edit-menu:create-undo?
		    edit-menu:create-redo?
		    edit-menu:create-cut?
		    edit-menu:create-copy?
		    edit-menu:create-paste?
		    edit-menu:create-clear?
		    edit-menu:create-select-all?
		    edit-menu:between-select-all-and-find
		    edit-menu:between-find-and-preferences
		    edit-menu:between-redo-and-cut
		    file-menu:between-print-and-close)
          (define (edit-menu:create-undo?) #f)
          (define (edit-menu:create-redo?) #f)
          (define (edit-menu:create-cut?) #f)
          (define (edit-menu:create-copy?) #f)
          (define (edit-menu:create-paste?) #f)
          (define (edit-menu:create-clear?) #f)
          (define (edit-menu:create-select-all?) #f)
          (define (edit-menu:between-select-all-and-find x) (void))
          (define (edit-menu:between-find-and-preferences x) (void))
          (define (edit-menu:between-redo-and-cut x) (void))
          (define (file-menu:between-print-and-close x) (void))
          
          (apply super-make-object args)))
      
      (define (invite-tour)
        (let* ([f (make-object tour-frame% (format (string-constant welcome-to-something) 
                                                   (string-constant drscheme)))]
               [panel (send f get-area-container)]
               [top-hp (make-object horizontal-panel% panel)]
               [bottom-vp (make-object vertical-panel% panel)]
               [left-vp (make-object vertical-panel% top-hp)]
               [plt-bitmap (get-plt-bitmap)]
               [plt-icon (make-object message% (if (send plt-bitmap ok?)
                                                   plt-bitmap
                                                   "[plt]")
                           left-vp)]
               [outer-button-panel (make-object vertical-panel% top-hp)]
               [top-button-panel (make-object vertical-panel% outer-button-panel)]
               [bottom-button-panel (make-object vertical-panel% outer-button-panel)]
               [tour-button (make-tour-button top-button-panel)]
               [release-notes-button (make-release-notes-button top-button-panel)]
               [close-button (make-object button% (string-constant close) bottom-button-panel
                               (lambda x
                                 (send f close)))]
               [messages-panel (make-object vertical-panel% left-vp)]
               
               [this-version (version:version)]
               [last-version (preferences:get 'drscheme:last-version)]
               [last-language (preferences:get 'drscheme:last-language)]
               [welcome-to-drs-msg (make-object message% (string-constant welcome-to-drscheme) messages-panel)]
               [this-version-message (make-object message%
                                       (format (string-constant version/language)
                                               this-version 
                                               (this-language))
                                       messages-panel)]
               [last-version-message
                (let ([msg (cond
                             [(and last-version
                                   last-language
                                   (not (equal? this-version last-version))
                                   (not (equal? (this-language) last-language)))
                              (format (string-constant parenthetical-last-version/language)
                                      last-version last-language)]
                             [(and last-language
                                   (not (equal? (this-language) last-language)))
                              (format (string-constant parenthetical-last-language)
                                      last-language)]
                             [(and last-version
                                   (not (equal? this-version last-version)))
                              (format (string-constant parenthetical-last-version)
                                      last-version)]
                             [else #f])])
                  (and msg (make-object message% msg messages-panel)))])
          (for-each (lambda (native-lang-string language)
                      (unless (equal? (this-language) language)
                        (instantiate button% ()
                          (label native-lang-string)
                          (parent bottom-vp)
                          (stretchable-width #t)
                          (callback (lambda (x1 x2) (switch-language-to language))))))
                    (string-constants is-this-your-native-language)
                    (all-languages))
          (send bottom-vp stretchable-height #f)
          (send messages-panel stretchable-height #f)
          (send bottom-button-panel stretchable-height #f)
          (send top-button-panel set-alignment 'center 'center)
          (send bottom-button-panel set-alignment 'center 'center)
          (send messages-panel set-alignment 'center 'center)
          
          (send f reflow-container)
          (same-heights (list bottom-button-panel messages-panel))
          (same-widths (list tour-button release-notes-button close-button))
          
          (send tour-button focus)
          (preferences:set 'drscheme:last-version this-version)
          (preferences:set 'drscheme:last-language (this-language))
          (send f show #t)))

      (define (switch-language-to language)
        (when (gui-utils:get-choice
               (string-constant are-you-sure-you-want-to-switch-languages)
               (case (system-type)
                 [(windows) (string-constant exit-cap)]
                 [else (string-constant quit-cap)])
               (string-constant cancel)
               (string-constant drscheme)
               #f)
          (let ([set-language? #t])
            (exit:insert-on-callback 
             (lambda ()
               (when set-language?
                 (write-resource "mred" "gui_language" language))))
            (exit:exit #t)
            (set! set-language? #f))))
      
      (define (about-drscheme)
        (let* ([e (make-object wrap-edit%)]
               [main-text (make-object wrap-edit%)]
               [plt-bitmap (get-plt-bitmap)]
               [plt-icon (if (send plt-bitmap ok?)
                             (make-object image-snip% plt-bitmap)
                             (let ([i (make-object string-snip%)]
                                   [label "[lambda]"])
                               (send i insert label (string-length label) 0)
                               i))]
               [editor-snip (make-object editor-snip% e #f)]
               [f (make-object about-frame% main-text)]
               [main-panel (send f get-area-container)]
               [editor-canvas (make-object editor-canvas% main-panel)]
               [button-panel (make-object horizontal-panel% main-panel)]
               [top (make-object style-delta% 'change-alignment 'top)]
               [d-usual (make-object style-delta% 'change-family 'decorative)]
               [d-dr (make-object style-delta%)]
               [d-http (make-object style-delta%)]
               
               [this-version (version:version)]
               
               [insert-url
                (lambda (str url)
                  (send e change-style d-http)
                  (let* ([before (send e get-start-position)]
                         [_ (send e insert str)]
                         [after (send e get-start-position)])
                    (send e set-clickback before after 
                          (lambda args (help-desk:open-url url))
                          d-http))
                  (send e change-style d-usual))])
          
          
          
          (send* d-http 
            (copy d-usual)
            (set-delta-foreground "BLUE")
            (set-delta 'change-underline #t))
          (send* d-usual 
            (set-delta-foreground "BLACK")
            (set-delta 'change-underline #f))
          
          (send* d-dr (copy d-usual) (set-delta 'change-bold))
          (send d-usual set-weight-on 'normal)
          (send* editor-canvas
            (set-editor main-text)
            (stretchable-width #t)
            (stretchable-height #t))
          
          ;; 50 is close enough to the space
          (if (send plt-bitmap ok?)
              (send* editor-canvas
                (min-width (+ (* 2 (send plt-bitmap get-width)) 50))
                (min-height (+ (send plt-bitmap get-height) 50)))
              (send* editor-canvas
                (min-width 500)
                (min-height 400)))
          
          (send* e 
            (change-style d-dr)
            (insert (format (string-constant welcome-to-drscheme-version/language) 
                            this-version
                            (this-language)))
            (change-style d-usual))
          
          (send e insert " by ")
          
          (insert-url "PLT"
                      "http://www.cs.rice.edu/CS/PLT/")
          
          (send* e
            (insert ".")
            (insert #\newline)
            (insert names)
            (insert #\newline)
            (insert "For licensing information see "))
          
          (let ([copying.lib
                 (normalize-path
                  (build-path (collection-path "mzlib")
                              'up
                              'up
                              "notes"
                              "COPYING.LIB"))])
            (insert-url "COPYING.LIB" (string-append "file:" copying.lib)))
          
          (send* e
            (insert ".")
            (insert #\newline)
            (insert #\newline)
            (insert "Based on:")
            (insert #\newline)
            (insert "  ")
            (insert (banner))
            (insert "  McMicMac (c) 1995-1998 PLT, Rice University (Shriram Krishnamurthi)")
            (insert #\newline))
          
          (when (eq? (system-type) 'macos)
            (send* e
              (insert "  The A List (c) 1997-2000 Kyle Hammond")
              (insert #\newline)))
          
          (let ([tools (drscheme:tools:get-successful-tools)])
            (unless (null? tools)
              (send* e
                (insert #\newline)
                (insert "Installed tools:")
                (insert #\newline))
              (for-each
               (lambda (successful-tool)
                 (let ([name (or (drscheme:tools:successful-tool-name successful-tool)
                                 (format "~s" (drscheme:tools:successful-tool-spec successful-tool)))]
                       [bm (drscheme:tools:successful-tool-bitmap successful-tool)])
                   (send e insert "  ")
                   (when bm
                     (send* e
                       (insert (make-object image-snip% bm))
                       (insert #\space)))
                   (send* e 
                     (insert name)
                     (insert #\newline))))
               tools)))
          
          (send* e
            (auto-wrap #t)
            (set-autowrap-bitmap #f)
            (lock #t))
          (send* main-text 
            (set-autowrap-bitmap #f)
            (auto-wrap #t)
            (insert plt-icon)
            (insert editor-snip)
            (change-style top 0 2)
            (set-position 1)
            (hide-caret #t)
            (scroll-to-position 0)
            (lock #t))
          
          (let* ([tour-button (make-tour-button button-panel)]
                 [release-notes-button (make-release-notes-button button-panel)])
            (same-widths (list tour-button release-notes-button))
            (send tour-button focus))
          (send button-panel stretchable-height #f)
          (send button-panel set-alignment 'center 'center)
          (send f show #t)
          f))
      
      (define (add-language-items-to-help-menu help-menu)
        (let ([added-any? #f])
          (for-each (lambda (native-lang-string language)
                      (unless (equal? (this-language) language)
                        (unless added-any?
                          (make-object separator-menu-item% help-menu)
                          (set! added-any? #t))
                        (instantiate menu-item% ()
                          (label native-lang-string)
                          (parent help-menu)
                          (callback (lambda (x1 x2) (switch-language-to language))))))
                    (string-constants is-this-your-native-language)
                    (all-languages)))))))
