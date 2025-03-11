#lang racket/base
(require racket/unit
         racket/gui
         setup/plt-installer
         drracket/tool
         framework
         framework/private/srcloc-panel
         net/url
         net/head
         setup/unpack
         string-constants)
(provide tool@)

(define tool@ 
  (unit 
    (import drracket:tool^)
    (export drracket:tool-exports^)
    

    ;; install-plt-file : (union #f dialog% frame%) -> void
    ;; asks the user for a .plt file, either from the web or from
    ;; a file on the disk and installs it.
    (define (install-plt-file parent)
      (define pref (preferences:get 'drracket:install-plt-dialog))
      (define dialog
        (new dialog% [parent parent]
             [label (string-constant install-plt-file-dialog-title)]
             [alignment '(left center)]))
      (define tab-panel
        (new tab-panel% [parent dialog]
             [callback (λ (x y) (update-panels))]
             [choices (list (string-constant install-plt-web-tab)
                            (string-constant install-plt-file-tab))]))
      (define outer-swapping-panel
        (new-horizontal-panel% [parent tab-panel]
                               [stretchable-height #f]))
      (define spacing-panel
        (new-horizontal-panel% [parent outer-swapping-panel]
                               [stretchable-width #f]
                               [min-width 20]))
      (define swapping-panel
        (new panel:single% [parent outer-swapping-panel]
             [alignment '(left center)]
             [stretchable-width #t] [stretchable-height #f]))
      (define file-panel
        (new-horizontal-panel% [parent swapping-panel]
                               [stretchable-width #t] [stretchable-height #f]))
      (define url-panel
        (new-horizontal-panel% [parent swapping-panel]
                               [stretchable-height #f]))
      (define button-panel
        (new-horizontal-panel% [parent dialog]
                               [stretchable-height #f] [alignment '(right center)]))
      (define file-text-field
        (keymap:call/text-keymap-initializer
         (λ ()
           (new text-field% [parent file-panel]
                [callback void] [min-width 300] [stretchable-width #t]
                [init-value (caddr pref)]
                [label (string-constant install-plt-filename)]))))
      (define file-button
        (new button% [parent file-panel]
             [callback (λ (x y) (browse))]
             [label (string-constant browse...)]))
      (define url-text-field
        (keymap:call/text-keymap-initializer
         (λ ()
           (new text-field% [parent url-panel]
                [min-width 300] [stretchable-width #t] [callback void]
                [init-value (cadr pref)]
                [label (string-constant install-plt-url)]))))
      (define-values (ok-button cancel-button)
        (gui-utils:ok/cancel-buttons
         button-panel
         (λ (x y) (set! cancel? #f) (send dialog show #f))
         (λ (x y) (send dialog show #f))))
      ;; browse : -> void
      ;; gets the name of a file from the user and updates file-text-field
      (define (browse)
        (let ([filename (parameterize ([finder:default-extension "plt"]
                                       [finder:default-filters
                                        (if (eq? (system-type) 'macosx)
                                            (finder:default-filters)
                                            '(("PLT Files" "*.plt")
                                              ("Any" "*.*")))])
                          (finder:get-file #f "" #f "" dialog))])
          (when filename
            (send file-text-field set-value (path->string filename)))))
      ;; from-web? : -> boolean
      ;; returns #t if the user has selected a web address
      (define (from-web?)
        (zero? (send tab-panel get-selection)))
      (define cancel? #t)
      (define (update-panels)
        (define w? (from-web?))
        (define t  (if w? url-text-field file-text-field))
        (send swapping-panel active-child (if w? url-panel file-panel))
        (send t focus)
        (send (send t get-editor) set-position
              0 (string-length (send t get-value))))
      ;; initialize
      (send tab-panel set-selection (if (car pref) 0 1))
      (update-panels)
      (send dialog show #t)
      (preferences:set 'drracket:install-plt-dialog
                       (list (from-web?)
                             (send url-text-field get-value)
                             (send file-text-field get-value)))
      (cond
        [cancel? (void)]
        [(from-web?)
         (install-plt-from-url
          (let* ([url (send url-text-field get-value)]
                 ;; trim whitespaces
                 [url (regexp-replace #rx"^ +" url "")]
                 [url (regexp-replace #rx" +$" url "")])
            (if (regexp-match? #rx"^(?:[^/:]*://|$)" url)
                url
                (string-append "http://" url)))
          parent)]
        [else (parameterize ([error-display-handler
                              drracket:init:original-error-display-handler])
                (run-installer
                 (string->path (send file-text-field get-value))))]))

    ;; install-plt-from-url : string (union #f dialog%) -> void
    ;; downloads and installs a .plt file from the given url
    (define (install-plt-from-url s-url parent)
      (define url (string->url s-url))
      (define-values (port header) 
        (with-handlers ([exn:fail? (λ (x) (values #f x))])
          (get-pure-port/headers url #:redirections 5)))
      (cond
        [port
         (define size
           (let* ([content-header (extract-field "content-length" header)]
                  [m (and content-header
                          (regexp-match "[0-9]+" content-header))])
             (and m (string->number (car m)))))
         (define tmp-filename (make-temporary-file "tmp~a.plt"))
         (define d (make-object dialog% (string-constant downloading) parent))
         (define message (make-object message% (string-constant downloading-file...) d))
         (define gauge (and size 
                            (make-object gauge% #f 100 d)))
         (define exn #f)
         ; Semaphores to avoid race conditions: 
         (define wait-to-start (make-semaphore 0))
         (define wait-to-break (make-semaphore 0))
         ; Thread to perform the download: 
         (define t
           (thread 
            (λ () 
              (semaphore-wait wait-to-start) 
              (with-handlers ([exn:fail?
                               (λ (x) 
                                 (set! exn x))] 
                              [exn:break? ; throw away break exceptions 
                               void])
                (semaphore-post wait-to-break) 
                (with-output-to-file tmp-filename 
                  (λ () 
                    (let loop ([total 0])
                      (when gauge 
                        (send gauge set-value  
                              (inexact->exact 
                               (floor (* 100 (/ total size))))))
                      (define s (read-string 1024 port))
                      (unless (eof-object? s) 
                        (display s)
                        (loop (+ total (string-length s))))))
                  #:mode 'binary #:exists 'truncate))
              (send d show #f))))
         (send d center) 
         (make-object button% (string-constant &stop)
           d
           (λ (b e) 
             (semaphore-wait wait-to-break) 
             (set! tmp-filename #f) 
             (send d show #f) 
             (break-thread t))) 
         ; Let thread run only after the dialog is shown 
         (queue-callback (λ () (semaphore-post wait-to-start))) 
         (send d show #t) 
         (when exn (raise exn))
         (define unpack-err (open-output-string))
         (cond
           [(with-handlers ((exn:fail? values))
              (parameterize ([error-display-handler drracket:init:original-error-display-handler]
                             [current-error-port unpack-err])
                (fold-plt-archive tmp-filename void void void void void))
              #f)
            =>
            (λ (exn)
              (delete-file tmp-filename)
              (message-box (string-constant drscheme)
                           (string-append 
                            (string-constant install-plt-error-header)
                            "\n\n"
                            (exn-message exn)
                            "\n\n"
                            (get-output-string unpack-err))
                           #:dialog-mixin frame:focus-table-mixin))]
           [else
            (parameterize ([error-display-handler drracket:init:original-error-display-handler])
              (run-installer tmp-filename
                             (λ ()
                               (delete-file tmp-filename))))])]
        [else
         (define exn header)
         (define sp (open-output-string))
         (parameterize ([current-error-port sp])
           (drracket:init:original-error-display-handler (exn-message exn) exn))
         (message-box (string-constant drracket)
                      (string-append
                       (string-constant install-plt-error-downloading)
                       (get-output-string sp)))]))

    (define plt-installer-mixin
      (mixin (frame:standard-menus<%>) ()
        (super-new)
        (define/override (file-menu:between-open-and-revert file-menu)
          (super file-menu:between-open-and-revert file-menu)
          (new menu-item%
               [label (string-constant install-plt-file-menu-item...)]
               [parent file-menu]
               [callback (λ (item evt)
                           (install-plt-file this))]))))
    
    (define (phase1) 
      (drracket:get/extend:extend-unit-frame plt-installer-mixin)
      
      ;; add a handler to open .plt files.
      (handler:insert-format-handler
       "PLT Files"
       (λ (filename)
         (and (regexp-match? #rx"^(?i:plt)$"
                             (or (filename-extension filename) #""))
              (gui-utils:get-choice
               (format (string-constant install-plt-file) filename)
               (string-constant install-plt-file/yes)
               (string-constant install-plt-file/no))))
       (λ (filename)
         (run-installer filename)
         #f)))
    (define (phase2) (void))))
