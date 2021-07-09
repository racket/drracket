#lang racket/base
(provide module-language-tools@)
(require mrlib/switchable-button 
         mrlib/bitmap-label
         mrlib/close-icon
         racket/contract
         racket/place
         framework
         framework/private/srcloc-panel
         racket/unit
         racket/class
         racket/gui/base
         drracket/private/drsig
         "local-member-names.rkt"
         "insulated-read-language.rkt"
         "eval-helpers-and-pref-init.rkt"
         framework/private/logging-timer
         string-constants
         racket/format)

(define op (current-output-port))
(define (oprintf . args) (apply fprintf op args))

(define-unit module-language-tools@
  (import [prefix drracket:unit: drracket:unit^]
          [prefix drracket:module-language: drracket:module-language/int^]
          [prefix drracket:language: drracket:language^]
          [prefix drracket:language-configuration: drracket:language-configuration^]
          [prefix drracket:init: drracket:init/int^]
          [prefix drracket: drracket:interface^])
  (export drracket:module-language-tools/int^)

  (define-struct opt-in/out-toolbar-button (make-button id number) #:transparent)
  (define opt-out-toolbar-buttons '())
  (define opt-in-toolbar-buttons  '())
  
  (define (add-opt-out-toolbar-button make-button id #:number [number #f])
    (set! opt-out-toolbar-buttons
          (cons (make-opt-in/out-toolbar-button make-button id number)
                opt-out-toolbar-buttons)))
  (define (add-opt-in-toolbar-button  make-button id #:number [number #f])
    (set! opt-in-toolbar-buttons
          (cons (make-opt-in/out-toolbar-button make-button id number)
                opt-in-toolbar-buttons)))

  (define-local-member-name
    set-lang-toolbar-buttons
    get-lang-toolbar-buttons
    remove-toolbar-button
    get-toolbar-button-panel
    get-online-expansion-monitor-pcs
    with-language-specific-default-extensions-and-filters

    get-hash-lang-error-state
    set-hash-lang-error-state
    update-hash-lang-error-gui-state)
    
  (define tab-mixin
    (mixin (drracket:unit:tab<%>) (drracket:module-language-tools:tab<%>)
      (inherit get-frame)
      (define toolbar-buttons '())
      (define/public (get-lang-toolbar-buttons) toolbar-buttons)
      (define/public (set-lang-toolbar-buttons bs ns)
        (for-each
         (λ (old-button) (send (get-frame) remove-toolbar-button old-button))
         toolbar-buttons)
        (set! toolbar-buttons bs)
        (send (get-frame) register-toolbar-buttons toolbar-buttons #:numbers ns)
        (send (get-frame) when-initialized
              (λ ()
                (send (send (get-frame) get-toolbar-button-panel) change-children
                      (λ (l) toolbar-buttons))))
        (send (get-frame) sort-toolbar-buttons-panel))


      (define the-hash-lang-error-state #f)
      (define/public (get-hash-lang-error-state) the-hash-lang-error-state)
      (define/public (set-hash-lang-error-state s)
        (set! the-hash-lang-error-state s)
        (when (equal? (send (get-frame) get-current-tab) this)
          (send (get-frame) update-hash-lang-error-gui-state the-hash-lang-error-state)))
      (define/public (show-#lang-error)
        (message-box (string-constant drracket)
                     (hash-lang-error-state-more-info-msg the-hash-lang-error-state)))
      (super-new)))
  
  (define frame-mixin
    (mixin (drracket:unit:frame<%>) (drracket:module-language-tools:frame<%>)
      (inherit unregister-toolbar-button 
               get-definitions-text
               sort-toolbar-buttons-panel
               get-current-tab)
      
      (define toolbar-button-panel #f)
      (define/public (when-initialized thunk)
        (cond
          [toolbar-button-panel
           (thunk)]
          [else
           (set! after-initialized 
                 (let ([old-after-initialized after-initialized])
                   (λ () 
                     (old-after-initialized) 
                     (thunk))))]))
      (define after-initialized void)
      (define/public (get-toolbar-button-panel) toolbar-button-panel)
      (define/public (remove-toolbar-button button)
        (send toolbar-button-panel change-children (λ (l) (remq button l)))
        (unregister-toolbar-button button)
        (sort-toolbar-buttons-panel))

      (define/augment (on-tab-change old-tab new-tab)
        (inner (void) on-tab-change old-tab new-tab)
        (when toolbar-button-panel
          (send toolbar-button-panel change-children
                (λ (l) (send new-tab get-lang-toolbar-buttons)))
          (sort-toolbar-buttons-panel))

        (when hash-lang-error-parent-panel
          (unless (equal? (send new-tab get-hash-lang-error-state)
                          (send old-tab get-hash-lang-error-state))
            (update-hash-lang-error-gui-state (send new-tab get-hash-lang-error-state)))))
      
      (define/override (file-menu:open-callback menu evt)
        (send (get-definitions-text) with-language-specific-default-extensions-and-filters
              (λ ()
                (super file-menu:open-callback menu evt))))

      (define hash-lang-error-parent-panel #f)
      (define hash-lang-error-panel #f)
      (define hash-lang-error-message #f)

      ;; state : (or/c hash-lang-error-state? #f)
      ;;  -- string is the error message
      (define/public (update-hash-lang-error-gui-state state)
        (cond
          [(hash-lang-error-state? state)
           (send hash-lang-error-message set-msgs
                 (list (hash-lang-error-state-display-msg state))
                 #t
                 (hash-lang-error-state-more-info-msg state)
                 '())
           (send hash-lang-error-parent-panel change-children
                 (λ (x) (append (remq hash-lang-error-panel x) (list hash-lang-error-panel))))]
          [else
           (send hash-lang-error-parent-panel change-children
                 (λ (x) (remq hash-lang-error-panel x)))]))

      (define/override (make-root-area-container cls parent)
        (set! hash-lang-error-parent-panel
              (super make-root-area-container vertical-pane% parent))
        (define root (make-object cls hash-lang-error-parent-panel))
        (set! hash-lang-error-panel
              (new-horizontal-panel%
                   [stretchable-height #f]
                   [parent hash-lang-error-parent-panel]))

        (new message% [label "#lang "] [parent hash-lang-error-panel])
        (set! hash-lang-error-message (new drracket:module-language:error-message%
                                           [parent hash-lang-error-panel]
                                           [stretchable-width #t]
                                           [msgs '("hi")]
                                           [err? #f]))
        (new button%
             [parent hash-lang-error-panel]
             [label (string-append (string-constant module-language-#lang-flush-cache)
                                   (case (car (get-default-shortcut-prefix))
                                     [(cmd)
                                      (if (equal? (system-type) 'macosx)
                                          " (⌘⇧D)"
                                          "")]
                                     [(control) " (Ctrl+Shift-D)"]
                                     [else ""]))]
             [font small-control-font]
             [callback (λ (b evt)
                         (send (send (get-current-tab) get-defs) move-to-new-language #t))])
        (new button%
             [parent hash-lang-error-panel]
             [label (string-constant module-language-#lang-error-more-information)]
             [font small-control-font]
             [callback (λ (b evt) (send (get-current-tab) show-#lang-error))])
        (new close-icon%
             [parent hash-lang-error-panel]
             [callback (λ () (send (get-current-tab) set-hash-lang-error-state #f))])

        (send hash-lang-error-parent-panel change-children (λ (l) (remq hash-lang-error-panel l)))
        root)
      
      (super-new)
      (inherit get-button-panel)
      (set! toolbar-button-panel (new panel:horizontal-discrete-sizes% 
                                      [parent (get-button-panel)]
                                      [alignment '(right center)]
                                      [stretchable-width #t]))
      (after-initialized)
      (set! after-initialized void)
      
      (define/public (initialize-module-language)
        (let ([defs (get-definitions-text)])
          (when (send defs get-in-module-language?)
            (send defs move-to-new-language))))))
  
  (struct hash-lang-error-state (display-msg more-info-msg) #:transparent)
  
  (define definitions-text-mixin
    (mixin (text:basic<%> 
            racket:text<%>
            drracket:unit:definitions-text<%>
            drracket:module-language:big-defs/ints-label<%>)
           (drracket:module-language-tools:definitions-text<%>)
      (inherit get-next-settings
               get-filename
               set-lang-wants-big-defs/ints-labels?
               get-tab
               last-position
               get-start-position
               get-end-position
               insert
               delete
               begin-edit-sequence
               end-edit-sequence
               position-paragraph
               paragraph-start-position
               get-surrogate
               start-colorer
               set-surrogate
               get-keymap)
      (define in-module-language? #f)      ;; true when we are in the module language
      (define hash-lang-language #f)       ;; non-false is the string that was parsed for the language

      ;; non-false means that an edit before this location
      ;; means that the language might have changed
      ;; (this will be the end of the #lang line if there is one,
      ;; or it will be some conservative place if there isn't)
      (define hash-lang-last-location #f)

      (define/public (irl-get-read-language-port-start+end)
        (get-read-language-port-start+end the-irl))
      (define/public (irl-get-read-language-name) (get-read-language-name the-irl))
      
      (define/private (irl-blew-up exn)
        (define sp (open-output-string))
        (parameterize ([current-error-port sp])
          (drracket:init:original-error-display-handler (exn-message exn) exn))
        (send (get-tab) set-hash-lang-error-state
              (hash-lang-error-state (regexp-replace* #rx"\n" (exn-message exn) " ")
                                     (get-output-string sp))))
      
      (define/public (get-in-module-language?) in-module-language?)
      (define/augment (after-insert start len)
        (inner (void) after-insert start len)
        (modification-at start))
      (define/augment (after-delete start len)
        (inner (void) after-delete start len)
        (modification-at start))
      
      (define/augment (after-save-file success?)
        (inner (void) after-save-file success?)
        (when success? (filename-maybe-changed)))

      (define/augment (after-load-file success?)
        (inner (void) after-load-file success?)
        (when success? (filename-maybe-changed)))

      (define last-filename #f)
      (define/private (filename-maybe-changed)
        (define this-filename (get-filename))
        (unless (equal? last-filename this-filename)
          (set! last-filename this-filename)
          (modification-at #f)))
      
      (define timer #f)
      
      ;; modification-at : (or/c #f number) -> void
      ;; checks to see if the lang line has changed when start
      ;; is in the region of the lang line, or when start is #f, or
      ;; when there is no #lang line known.
      (define/private (modification-at start)
        (send (send (get-tab) get-frame) when-initialized
              (λ ()
                (when in-module-language?
                  (when (or (not start)
                            (not hash-lang-last-location)
                            (<= start hash-lang-last-location))
                    
                    (unless timer
                      (set! timer (new logging-timer%
                                       [notify-callback
                                        (λ ()
                                          (when in-module-language?
                                            (move-to-new-language)))]
                                       [just-once? #t])))
                    (send timer stop)
                    (send timer start 200 #t))))))

      (define/private (update-in-module-language? new-one)
        (unless (equal? new-one in-module-language?)
          (set! in-module-language? new-one)
          (cond
            [in-module-language? 
             (move-to-new-language)]
            [else
             (set! hash-lang-language #f)
             (set! hash-lang-last-location #f)
             (clear-things-out)])))

      (define/public (move-to-new-language [flush-irl-cache? #f])
        (when timer (send timer stop))
        (send (get-tab) set-hash-lang-error-state #f)
        (define port (open-input-text-editor this))
        (reset-irl! the-irl port (get-irl-directory) flush-irl-cache?)
        (define-values (lang-name-start lang-name-end)
          (get-read-language-port-start+end the-irl))
        (set! hash-lang-language (and lang-name-end (get-text lang-name-start lang-name-end)))
        (when hash-lang-language
          (preferences:set 'drracket:most-recent-lang-line (string-append hash-lang-language
                                                                          "\n")))
        (set! hash-lang-last-location (get-read-language-last-position the-irl))
        
        (clear-things-out)

        (define mode (or (get-definitions-text-surrogate the-irl)
                         (new racket:text-mode%)))
        (send mode set-get-token (get-insulated-module-lexer the-irl))
        (set-surrogate mode)
        
        (define lang-wants-big-defs/ints-labels?
          (and (call-read-language the-irl 'drracket:show-big-defs/ints-labels #f)
               #t))
        (set-lang-wants-big-defs/ints-labels? lang-wants-big-defs/ints-labels?)
        (send (send (get-tab) get-ints) set-lang-wants-big-defs/ints-labels?
              lang-wants-big-defs/ints-labels?)
        
        (set! extra-default-filters
              (or (call-read-language the-irl 'drracket:default-filters #f)
                  '()))
          
        (set! default-extension
              (or (call-read-language the-irl 'drracket:default-extension #f)
                  ""))
        
        (set! indentation-function
              (or (call-read-language the-irl 'drracket:indentation #f)
                  (λ (x y) #f)))
        (set! range-indentation-function
              (or (call-read-language the-irl 'drracket:range-indentation #f)
                  (λ (x y z) #f)))
        (set! grouping-position
              (or (call-read-language the-irl 'drracket:grouping-position #f)
                  default-grouping-position))

        (set! lang-keymap (new keymap:aug-keymap%))
        (for ([key+proc (in-list (call-read-language the-irl 'drracket:keystrokes '()))])
          (define key (list-ref key+proc 0))
          (define proc (list-ref key+proc 1))
          (define name
            (let loop ([counter #f])
              (define name (if counter
                               (~a (object-name proc) counter)
                               (~a (object-name proc))))
              (cond
                [(send lang-keymap is-function-added? name)
                 (loop (+ (or counter 0) 1))]
                [else name])))
          (send lang-keymap add-function name proc)
          (send lang-keymap map-function key name))
        (send (get-keymap) chain-to-keymap lang-keymap #t)

        (register-new-buttons
         (or (call-read-language the-irl 'drracket:toolbar-buttons #f)
             (call-read-language the-irl 'drscheme:toolbar-buttons #f))
         
         (let ([drracket-opt-out (call-read-language the-irl 'drracket:opt-out-toolbar-buttons '())]
               [drscheme-opt-out (call-read-language the-irl 'drscheme:opt-out-toolbar-buttons '())])
           (and drracket-opt-out drscheme-opt-out
                (append drracket-opt-out drscheme-opt-out)))

         (call-read-language the-irl 'drracket:opt-in-toolbar-buttons '())))

      ;; removes language-specific customizations
      (define/private (clear-things-out)
        (define tab (get-tab))
        (define ints (send tab get-ints))
        (set-surrogate (new racket:text-mode%))
        (set-lang-wants-big-defs/ints-labels? #f)
        (send ints set-lang-wants-big-defs/ints-labels? #f)
        (set! extra-default-filters '())
        (set! default-extension "")
        (set! indentation-function (λ (x y) #f))
        (set! range-indentation-function (λ (x y z) #f))
        (set! grouping-position default-grouping-position)
        (when lang-keymap
          (send (get-keymap) remove-chained-keymap lang-keymap)
          (set! lang-keymap #f))
        (send tab set-lang-toolbar-buttons '() '()))

      (define/private (register-new-buttons buttons opt-out-ids opt-in-ids)
        ;; cleaned-up-buttons : (listof (list/c string?
        ;;                                      (is-a?/c bitmap%) 
        ;;                                      (-> (is-a?/c drracket:unit:frame<%>) any) 
        ;;                                      (or/c real? #f)))
        (define cleaned-up-buttons
          (cond
            [(not buttons) '()]
            [else
             (for/list ([button (in-list buttons)])
               (if (= 3 (length button))
                   (append button (list #f))
                   button))]))
        (define tab (get-tab))
        (define frame (send tab get-frame))
        (send frame when-initialized
              (λ ()
                (send frame begin-container-sequence)
                
                ;; avoid any time with both sets of buttons in the
                ;; panel so the window doesn't get too wide
                (send (send frame get-toolbar-button-panel) change-children (λ (prev) '()))
                
                (define directly-specified-buttons
                  (map (λ (button-spec)
                         (new switchable-button%
                              [label (list-ref button-spec 0)]
                              [bitmap (list-ref button-spec 1)]
                              [parent (send frame get-toolbar-button-panel)]
                              [callback
                               (lambda (button)
                                 ((list-ref button-spec 2) frame))]))
                       cleaned-up-buttons))
                (define directly-specified-button-numbers 
                  (map (λ (button-spec) (list-ref button-spec 3)) 
                       cleaned-up-buttons))
                (define (get-opt-in/out-buttons+numbers opt-in?)
                  (define (flip? b) (if opt-in? (not b) b))
                  (define opt-in/out-ids (if opt-in? opt-in-ids opt-out-ids))
                  (define opt-in/out-toolbar-buttons
                    (if opt-in? opt-in-toolbar-buttons opt-out-toolbar-buttons))
                  (cond
                    [(eq? opt-in/out-ids #f) '()]
                    [else
                     (for/list ([opt-in/out-toolbar-button (in-list opt-in/out-toolbar-buttons)]
                                #:unless (flip? (member
                                                 (opt-in/out-toolbar-button-id opt-in/out-toolbar-button)
                                                 opt-in/out-ids)))
                       (list ((opt-in/out-toolbar-button-make-button opt-in/out-toolbar-button)
                              frame
                              (send frame get-toolbar-button-panel))
                             (opt-in/out-toolbar-button-number opt-in/out-toolbar-button)))]))
                (define opt-out-buttons+numbers (get-opt-in/out-buttons+numbers #f))
                (define opt-in-buttons+numbers  (get-opt-in/out-buttons+numbers #t))
                (send tab set-lang-toolbar-buttons
                      (append directly-specified-buttons
                              (map (λ (x) (list-ref x 0)) opt-out-buttons+numbers)
                              (map (λ (x) (list-ref x 0)) opt-in-buttons+numbers))
                      (append directly-specified-button-numbers
                              (map (λ (x) (list-ref x 1)) opt-out-buttons+numbers)
                              (map (λ (x) (list-ref x 1)) opt-in-buttons+numbers)))
                (send frame end-container-sequence))))
      
      (inherit get-text)
      (define/private (get-lang-name pos)
        (cond
          [(zero? pos) '<<unknown>>]
          [else
           (let ([str (get-text 0 pos)])
             (if (char-whitespace? (string-ref str (- (string-length str) 1)))
                 (substring str 0 (- (string-length str) 1))
                 str))]))
      
      
      ;; online-expansion-monitor-table : hash[(cons mod-path id) -o> (cons/c local-pc remote-pc)]
      (define online-expansion-monitor-table (make-hash))
      (define/public (get-online-expansion-monitor-pcs an-online-expansion-handler)
        (define key (cons (online-expansion-handler-mod-path an-online-expansion-handler)
                          (online-expansion-handler-id an-online-expansion-handler)))
        (define old (hash-ref online-expansion-monitor-table key #f))
        (cond
          [old
           (values (car old) (cdr old))]
          [else
           (define-values (local-pc remote-pc) (place-channel))
           (hash-set! online-expansion-monitor-table key (cons local-pc remote-pc))
           (values local-pc remote-pc)]))
      
      (define/augment (after-set-next-settings settings)
        (update-in-module-language?
         (is-a? (drracket:language-configuration:language-settings-language settings)
                drracket:module-language:module-language<%>))
        (inner (void) after-set-next-settings settings))
      
      (define/override (put-file dir default-name)
        (with-language-specific-default-extensions-and-filters
         (λ ()
           (super put-file dir default-name))))
      (define/override (get-file dir)
        (with-language-specific-default-extensions-and-filters
         (λ ()
           (super get-file dir))))
      
      (define extra-default-filters '())
      (define default-extension "")
      (define indentation-function (λ (x y) #f))
      (define range-indentation-function (λ (x y z) #f))
      (define grouping-position default-grouping-position)
      (define lang-keymap #f)
      (define/public (with-language-specific-default-extensions-and-filters t)
        (parameterize ([finder:default-extension default-extension]
                       [finder:default-filters 
                        (append extra-default-filters (finder:default-filters))])
          (t)))

      (inherit compute-racket-amount-to-indent)
      (define/augment (compute-amount-to-indent pos)
        (cond
          [in-module-language?
           (or (indentation-function this pos)
               (compute-racket-amount-to-indent pos))]
          [else
           (compute-racket-amount-to-indent pos)]))

      (define/private (range-indent start end)
        (define substs (range-indentation-function this start end))
        (and substs
             (let ()
               (define start-line (position-paragraph start #f))
               (define end-line (position-paragraph end #t))
               (begin-edit-sequence)
               (let loop ([substs substs] [line start-line])
                 (unless (or (null? substs)
                             (line . > . end-line))
                   (define pos (paragraph-start-position line))
                   (define del-amt (caar substs))
                   (define insert-str (cadar substs))
                   (unless (zero? del-amt)
                     (delete pos (+ pos del-amt)))
                   (unless (equal? insert-str "")
                     (insert insert-str pos))
                   (loop (cdr substs) (add1 line))))
               (end-edit-sequence))))

      (define/override (tabify-selection [start (get-start-position)]
                                         [end (get-end-position)])
        (unless (and in-module-language?
                     (range-indent start end))
          (super tabify-selection start end)))

      (define/override (tabify-all)
        (unless (and in-module-language?
                     (range-indent 0 (last-position)))
          (super tabify-all)))

      (define/override (find-up-sexp start-pos)
        (*-sexp start-pos 'up (λ () (super find-up-sexp start-pos))))
      (define/override (find-down-sexp start-pos)
        (*-sexp start-pos 'down (λ () (super find-down-sexp start-pos))))
      (define/override (get-backward-sexp start-pos)
        (*-sexp start-pos 'backward (λ () (super get-backward-sexp start-pos))))
      (define/override (get-forward-sexp start-pos)
        (*-sexp start-pos 'forward (λ () (super get-forward-sexp start-pos))))

      (inherit get-limit)
      (define/private (*-sexp start-pos direction do-super)
        (define irl-answer (grouping-position start-pos (get-limit start-pos) direction))
        (cond
          [(equal? irl-answer #t) (do-super)]
          [else irl-answer]))

      (define/private (get-irl-directory)
        (define tmp-b (box #f))
        (define fn (get-filename tmp-b))
        (when (unbox tmp-b) (set! fn #f))
        (define the-dir (get-init-dir fn))
        the-dir)
      
      (super-new)

      (define the-irl (make-irl (get-irl-directory) (λ (exn) (irl-blew-up exn))))
      (define/public (get-irl) the-irl)
      (set! in-module-language? 
            (is-a? (drracket:language-configuration:language-settings-language (get-next-settings))
                   drracket:module-language:module-language<%>))))

  (define default-grouping-position (λ (start limit dir) #t))
  
  (define no-more-online-expansion-handlers? #f)
  (define (no-more-online-expansion-handlers) (set! no-more-online-expansion-handlers? #t))
  (define-values (done done?)
    (let ()
      (struct done ())
      (values (done) done?)))
  (define-values (start start?)
    (let ()
      (struct start ())
      (values (start) start?)))
  ;; mod-path : module-path?
  ;; id : symbol?
  ;; local-handler : ... -> ...
  (struct online-expansion-handler (mod-path id local-handler monitor?))
  (define online-expansion-handlers '())
  (define (get-online-expansion-handlers)
    (cond
      [no-more-online-expansion-handlers?
       online-expansion-handlers]
      [else
       (error 'get-online-expansion-handlers 
              "online-expansion-handlers can still be registered")]))
  (define (add-online-expansion-handler mod-path id local-handler)
    (check-bad-registration 'add-online-expansion-handler mod-path id local-handler)
    (set! online-expansion-handlers
          (cons (online-expansion-handler mod-path id local-handler #f)
                online-expansion-handlers)))
  
  (define (add-online-expansion-monitor mod-path id local-handler)
    (check-bad-registration 'add-online-expansion-monitor mod-path id local-handler)
    (set! online-expansion-handlers
          (cons (online-expansion-handler mod-path id local-handler #t)
                online-expansion-handlers)))
      
  (define (check-bad-registration who mod-path id local-handler)
    (when no-more-online-expansion-handlers?
      (error who 
             "no more online-expansion-handlers can be registered; got ~e ~e ~e"
             mod-path id local-handler))
    (for ([handler (in-list online-expansion-handlers)])
      (when (and (equal? (online-expansion-handler-mod-path handler) mod-path)
                 (equal? (online-expansion-handler-id handler) id))
        (error who
               (string-append
                "already registered a handler with the same mod-path and id\n"
                " mod-path: ~e\n"
                " id: ~e")
               mod-path
               id))))
  
  (define online-expansion-pref-funcs '())
  (define (get-online-expansion-pref-funcs) online-expansion-pref-funcs)
    (define online-expansion-prefs '())
  (define (register-online-expansion-pref func)
    (set! online-expansion-pref-funcs (cons func online-expansion-pref-funcs))))
