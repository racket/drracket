
(module frame mzscheme
  (require (lib "name-message.ss" "mrlib")
           (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
           (lib "class.ss")
           "drsig.ss"
           (lib "check-gui.ss" "version")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "url.ss" "net")
           (lib "head.ss" "net")
           (lib "plt-installer.ss" "setup")
           (prefix mzlib:file: (lib "file.ss")) (lib "file.ss")
           (prefix mzlib:list: (lib "list.ss")))
  
  (provide frame@)
  (define frame@
    (unit/sig drscheme:frame^
      (import [drscheme:unit : drscheme:unit^]
              [drscheme:app : drscheme:app^]
              [help : drscheme:help-desk^]
              [drscheme:multi-file-search : drscheme:multi-file-search^]
              [drscheme:init : drscheme:init^])
      
      (rename [-mixin mixin])
      
      (define basics<%> (interface (frame:standard-menus<%>)))
      
      (define basics-mixin
        (mixin (frame:standard-menus<%>) (basics<%>)
          (inherit get-edit-target-window get-edit-target-object get-menu-bar)
          [define get-menu-bindings
            (lambda ()
              (let ([name-ht (make-hash-table)]
                    [fun-ht (make-hash-table)])
                (let loop ([menu-container (get-menu-bar)])
                  (for-each
                   (lambda (item)
                     (when (is-a? item selectable-menu-item<%>)
                       (let ([short-cut (send item get-shortcut)])
                         (when short-cut
                           (let ([keyname
                                  (keymap:canonicalize-keybinding-string
                                   (string-append
                                    (case (system-type)
                                      [(windows) "c:"]
                                      [(macosx macos) "d:"]
                                      [(unix)
                                       (case (send item get-x-shortcut-prefix)
                                         [(meta) "m:"]
                                         [(alt) "a:"]
                                         [(ctl) "c:"]
                                         [(ctl-m) "c:m;"])]
                                      [else ""])
                                    (string short-cut)))])
                             (hash-table-put! name-ht keyname (send item get-plain-label))
                             (hash-table-put! fun-ht keyname
                                              (lambda ()
                                                (let ([evt (make-object control-event% 'menu)])
                                                  (send evt set-time-stamp (current-milliseconds))
                                                  (send item command evt))))))))
                     (when (is-a? item menu-item-container<%>)
                       (loop item)))
                   (send menu-container get-items)))
                (values name-ht fun-ht)))]
          
          [define copy-hash-table
            (lambda (ht)
              (let ([res (make-hash-table)])
                (hash-table-for-each
                 ht
                 (lambda (x y) (hash-table-put! res x y)))
                res))]
          [define can-show-keybindings?
            (lambda ()
              (let ([edit-object (get-edit-target-object)])
                (and edit-object
                     (is-a? edit-object editor<%>)
                     (let ([keymap (send edit-object get-keymap)])
                       (is-a? keymap keymap:aug-keymap<%>)))))]
          
          [define show-keybindings
            (lambda ()
              (if (can-show-keybindings?)
                  (let ([edit-object (get-edit-target-object)])
                    (let ([keymap (send edit-object get-keymap)])
                      (let*-values ([(menu-names menu-funs) (get-menu-bindings)])
                        (let* ([table (send keymap get-map-function-table/ht
                                            (copy-hash-table menu-names))]
                               [structured-list
                                (mzlib:list:quicksort
                                 (hash-table-map table list)
                                 (lambda (x y) (string-ci<=? (cadr x) (cadr y))))])
                          (show-keybindings-to-user structured-list this)))))
                  (bell)))]
          
          (override file-menu:open-callback file-menu:open-string file-menu:new-string
                    help-menu:about-callback help-menu:about-string help-menu:create-about?
                    help-menu:before-about)
          [define help-menu:before-about
            (lambda (help-menu)
              (make-help-desk-menu-item help-menu)
              '(make-object menu-item%
                (format (string-constant welcome-to-something)
                        (string-constant drscheme))
                help-menu
                (lambda (item evt)
                  (drscheme:app:invite-tour))))]
          
          [define help-menu:about-callback (lambda (item evt) (drscheme:app:about-drscheme))]
          [define help-menu:about-string (lambda () (string-constant about-drscheme))]
          [define help-menu:create-about? (lambda () #t)]
          
          (define/public (get-additional-important-urls) '())
          (define/override (help-menu:after-about menu)
            (instantiate menu-item% ()
              (label (string-constant help-menu-check-for-updates))
              (parent menu)
              (callback (lambda (item evt) (check-version this))))
            (drscheme:app:add-important-urls-to-help-menu
             menu 
             (get-additional-important-urls))
            (drscheme:app:add-language-items-to-help-menu menu))
          
          [define (file-menu:open-callback item evt) (handler:open-file)]
          (define (file-menu:new-string) (string-constant new-menu-item))
          (define (file-menu:open-string) (string-constant open-menu-item))

          (rename [super-file-menu:between-open-and-revert file-menu:between-open-and-revert])
          [define/override file-menu:between-open-and-revert
            (lambda (file-menu) 
              (make-object menu-item% 
                (string-constant install-plt-file-menu-item...)
                file-menu
                (lambda (item evt)
                  (install-plt-file this)))
              (super-file-menu:between-open-and-revert file-menu))]
          
          (rename [super-file-menu:between-print-and-close file-menu:between-print-and-close])
          (define/override (file-menu:between-print-and-close menu)
            (super-file-menu:between-print-and-close menu)
            (instantiate menu-item% ()
              (label (string-constant mfs-multi-file-search-menu-item))
              (parent menu)
              (callback
               (lambda (_1 _2)
                 (drscheme:multi-file-search:multi-file-search))))
            (new separator-menu-item% (parent menu)))
          
          [define/override edit-menu:between-find-and-preferences
            (lambda (menu)
              (make-object separator-menu-item% menu)
              (let ([keybindings-on-demand
                     (lambda (menu-item)
                       (let ([last-edit-object (get-edit-target-window)])
                         (send menu-item enable (can-show-keybindings?))))])
                (instantiate menu-item% ()
                  (label (string-constant keybindings-menu-item))
                  (parent menu)
                  (callback (lambda x (show-keybindings)))
                  (help-string (string-constant keybindings-info))
                  (demand-callback keybindings-on-demand)))
              (unless (current-eventspace-has-standard-menus?)
                (make-object separator-menu-item% menu)))]
          
          (super-instantiate ())))

      ;; install-plt-file : (union #f dialog% frame%) -> void
      ;; asks the user for a .plt file, either from the web or from
      ;; a file on the disk and installs it.
      (define (install-plt-file parent)
        (define dialog
          (instantiate dialog% ()
            (parent parent)
            (alignment '(left center))
            (label (string-constant install-plt-file-dialog-title))))
        (define tab-panel
          (instantiate tab-panel% ()
            (parent dialog)
            (callback (lambda (x y) (update-panels)))
            (choices (list (string-constant install-plt-web-tab)
                           (string-constant install-plt-file-tab)))))
        (define outer-swapping-panel (instantiate horizontal-panel% ()
                                       (parent tab-panel)
                                       (stretchable-height #f)))
        (define spacing-panel (instantiate horizontal-panel% ()
                                (stretchable-width #f)
                                (parent outer-swapping-panel)
                                (min-width 20)))
        (define swapping-panel (instantiate panel:single% ()
                                 (parent outer-swapping-panel)
                                 (alignment '(left center))
                                 (stretchable-width #t)
                                 (stretchable-height #f)))
        (define file-panel (instantiate horizontal-panel% ()
                             (parent swapping-panel)
                             (stretchable-width #t)
                             (stretchable-height #f)))
        (define url-panel (instantiate horizontal-panel% ()
                            (parent swapping-panel)
                            (stretchable-height #f)))
        (define button-panel (instantiate horizontal-panel% ()
                               (parent dialog)
                               (stretchable-height #f)
                               (alignment '(right center))))
        (define file-text-field (instantiate text-field% ()
                                  (parent file-panel)
                                  (callback void)
                                  (min-width 300)
                                  (stretchable-width #t)
                                  (label (string-constant install-plt-filename))))
        (define file-button (instantiate button% ()
                              (parent file-panel)
                              (label (string-constant browse...))
                              (callback (lambda (x y) (browse)))))
        (define url-text-field (instantiate text-field% ()
                                 (parent url-panel)
                                 (label (string-constant install-plt-url))
                                 (min-width 300)
                                 (stretchable-width #t)
                                 (callback void)))
        
        (define-values (ok-button cancel-button)
          (gui-utils:ok/cancel-buttons
           button-panel
           (lambda (x y)
             (set! cancel? #f)
             (send dialog show #f))
           (lambda (x y)
             (send dialog show #f))))
        
        ;; browse : -> void
        ;; gets the name of a file from the user and
        ;; updates file-text-field
        (define (browse)
          (let ([filename (get-file #f dialog)])
            (when filename
              (send file-text-field set-value filename))))
        
        ;; from-web? : -> boolean
        ;; returns #t if the user has selected a web address
        (define (from-web?)
          (zero? (send tab-panel get-selection)))
        
        (define cancel? #t)
        
        (define (update-panels)
          (send swapping-panel active-child
                (if (from-web?)
                    url-panel
                    file-panel)))
        
        (update-panels)
        (send dialog show #t)
        
        (cond
          [cancel? (void)]
          [(from-web?)
           (install-plt-from-url (send url-text-field get-value) parent)]
          [else 
           (parameterize ([error-display-handler drscheme:init:original-error-display-handler])
             (run-installer (send file-text-field get-value)))]))

      ;; install-plt-from-url : string (union #f dialog%) -> void
      ;; downloads and installs a .plt file from the given url
      (define (install-plt-from-url s-url parent)
        (with-handlers ([(lambda (x) #f)
                         (lambda (exn)
                           (message-box (string-constant drscheme)
                                        (if (exn? exn)
                                            (format "~a" (exn-message exn))
                                            (format "~s" exn))))])
          (let* ([url (string->url s-url)]
                 [tmp-filename (make-temporary-file "tmp~a.plt")]
                 [port (get-impure-port url)]
                 [header (purify-port port)]
                 [size (let* ([content-header (extract-field "content-length" header)]
                              [m (and content-header
                                      (regexp-match "[0-9]+" content-header))])
                         (and m (string->number (car m))))]
                 [d (make-object dialog% (string-constant downloading) parent)] 
                 [message (make-object message% (string-constant downloading-file...) d)] 
                 [gauge (if size 
                            (make-object gauge% #f 100 d) 
                            #f)] 
                 [exn #f] 
                 ; Semaphores to avoid race conditions: 
                 [wait-to-start (make-semaphore 0)] 
                 [wait-to-break (make-semaphore 0)] 
                 ; Thread to perform the download: 
                 [t (thread 
                     (lambda () 
                       (semaphore-wait wait-to-start) 
                       (with-handlers ([void 
                                        (lambda (x) 
                                          (when (not-break-exn? x) 
                                            (set! exn x)))] 
                                       [void ; throw away break exceptions 
                                        void]) 
                         (semaphore-post wait-to-break) 
                         (with-output-to-file tmp-filename 
                           (lambda () 
                             (let loop ([total 0]) 
                               (when gauge 
                                 (send gauge set-value  
                                       (inexact->exact 
                                        (floor (* 100 (/ total size)))))) 
                               (let ([s (read-string 1024 port)]) 
                                 (unless (eof-object? s) 
                                   (unless (eof-object? s) 
                                     (display s) 
                                     (loop (+ total (string-length s))))))))
                           'binary 'truncate))
                       (send d show #f)))]) 
            (send d center) 
            (make-object button% (string-constant &stop)
			 d
			 (lambda (b e) 
			   (semaphore-wait wait-to-break) 
			   (set! tmp-filename #f) 
			   (send d show #f) 
			   (break-thread t))) 
            ; Let thread run only after the dialog is shown 
            (queue-callback (lambda () (semaphore-post wait-to-start))) 
            (send d show #t) 
            (when exn (raise exn))
            (parameterize ([error-display-handler drscheme:init:original-error-display-handler])
              (run-installer tmp-filename
                             (lambda ()
                               (delete-file tmp-filename)))))))
      
      
      (define keybindings-dialog%
        (class dialog%
          (rename [super-on-size on-size])
          (override on-size)
          [define on-size
            (lambda (w h)
              (preferences:set 'drscheme:keybindings-window-size (cons w h))
              (super-on-size w h))]
          (super-instantiate ())))
      
      (define (show-keybindings-to-user bindings frame)
        (letrec ([f (instantiate keybindings-dialog% ()
                      (label (string-constant keybindings-frame-title))
                      (parent frame)
                      (width (car (preferences:get 'drscheme:keybindings-window-size)))
                      (height (cdr (preferences:get 'drscheme:keybindings-window-size)))
                      (style '(resize-border)))]
                 [bp (make-object horizontal-panel% f)]
                 [b-name (make-object button% (string-constant keybindings-sort-by-name)
                           bp (lambda x (update-bindings #f)))]
                 [b-key (make-object button% (string-constant keybindings-sort-by-key)
                          bp (lambda x (update-bindings #t)))]
                 [lb
                  (make-object list-box% #f null f void)]
                 [bp2 (make-object horizontal-panel% f)]
                 [cancel (make-object button% (string-constant close)
                           bp2 (lambda x (send f show #f)))]
                 [space (make-object grow-box-spacer-pane% bp2)]
                 [update-bindings
                  (lambda (by-key?)
                    (let ([format-binding/name
                           (lambda (b) (format "~a (~a)" (cadr b) (car b)))]
                          [format-binding/key
                           (lambda (b) (format "~a (~a)" (car b) (cadr b)))]
                          [predicate/key
                           (lambda (a b) (string-ci<=? (format "~a" (car a))
                                                       (format "~a" (car b))))]
                          [predicate/name
                           (lambda (a b) (string-ci<=? (cadr a) (cadr b)))])
                      (send lb set
                            (if by-key?
                                (map format-binding/key (mzlib:list:quicksort bindings predicate/key))
                                (map format-binding/name (mzlib:list:quicksort bindings predicate/name))))))])
          (send bp stretchable-height #f)
          (send bp set-alignment 'center 'center)
          (send bp2 stretchable-height #f)
          (send bp2 set-alignment 'right 'center)
          (update-bindings #f)
          (send f show #t)))
      
      (define <%>
        (interface (frame:editor<%> basics<%> frame:text-info<%>)
          running
          not-running
	  get-currently-running?
          get-show-menu
          update-shown
          add-show-menu-items))
      
      (define -mixin
        (mixin (frame:editor<%> frame:text-info<%> basics<%>) (<%>)
          (inherit get-editor)
          (rename [super-file-menu:print-callback file-menu:print-callback])
          (inherit get-info-panel)
          (field [show-menu #f])
          (public get-show-menu update-shown)
          [define get-show-menu (lambda () show-menu)]
          [define update-shown (lambda () (void))]
          (define/public (add-show-menu-items show-menu) (void))
          
          [define get-bitmap/string
            (lambda (icon string)
              (let ([p (build-path (collection-path "icons") icon)])
                (if (file-exists? p)
                    (make-object bitmap% p 'gif)
                    string)))]
          (field
           [currently-running? #f]
           [sleepy-bitmap (get-bitmap/string "snoopy-sleepy.gif" (string-constant not-running))]
           [active-bitmap (get-bitmap/string "snoopy-active.gif" (string-constant running))])
          (public running not-running)
          [define running
            (lambda ()
              (unless currently-running?
                (set! currently-running? #t)
                (send running-message set-label active-bitmap)))]
          [define not-running
            (lambda ()
              (when currently-running?
                (set! currently-running? #f)
                (send running-message set-label sleepy-bitmap)))]
          
          (define/public (get-currently-running?) currently-running?)
          
          (inherit get-menu% get-menu-bar)
          (super-instantiate ())
          (set! show-menu (make-object (get-menu%) (string-constant view-menu-label)
                            (get-menu-bar)))

          (add-show-menu-items show-menu)
          
          (field
           [running-message
            (make-object message% sleepy-bitmap (get-info-panel))])))
      
      
      (define (create-root-menubar)
        (let* ([mb (new menu-bar% (parent 'root))]
               [file-menu (new menu% 
                               (label (string-constant file-menu))
                               (parent mb))]
               [help-menu (new menu% 
                               (label (string-constant help-menu))
                               (parent mb))])
          (new menu-item%
               (label (string-constant new-menu-item))
               (parent file-menu)
               (shortcut #\n)
               (callback
                (lambda (x y)
                  (handler:edit-file #f)
                  #t)))
	  (new menu-item%
               (label (string-constant open-menu-item))
               (parent file-menu)
               (shortcut #\o)
               (callback
                (lambda (x y)
                  (handler:open-file)
                  #t)))
          (new menu%
               (label (string-constant open-recent-menu-item))
               (parent file-menu)
               (demand-callback
                (lambda (menu)
                  (handler:install-recent-items menu))))
	  (instantiate menu-item% ()
            (label (string-constant mfs-multi-file-search-menu-item))
            (parent file-menu)
            (callback
             (lambda (_1 _2)
               (drscheme:multi-file-search:multi-file-search))))
          (unless (current-eventspace-has-standard-menus?)
	    (new separator-menu-item% (parent file-menu))
	    (new menu-item%
               (label (string-constant quit-menu-item-others))
               (parent file-menu)
               (shortcut #\q)
               (callback
                (lambda (x y)
                  (exit:exit)
                  #t))))
          (make-help-desk-menu-item help-menu)))
      
      (define (make-help-desk-menu-item help-menu)
        (make-object menu-item%
          (string-constant help-desk)
          help-menu
          (lambda (item evt)
            (help:help-desk)
            #t)))
      
      
      )))
