
(module frame mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
           (lib "class.ss")
           "drsig.ss"
           (lib "check-gui.ss" "version")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix mzlib:file: (lib "file.ss"))
           (prefix mzlib:list: (lib "list.ss"))
           (prefix zodiac: (lib "zodiac.ss" "syntax")))
  
  (provide frame@)
  (define frame@
    (unit/sig drscheme:frame^
      (import (drscheme:unit : drscheme:unit^)
              (drscheme:app : drscheme:app^)
              (help : drscheme:help-desk^)
              [drscheme:multi-file-search : drscheme:multi-file-search^])
      
      (rename [-mixin mixin])
      
      (define button-label-font
        (send the-font-list find-or-create-font
              (case (system-type)
                [(windows) 8]
                [(macosx) 13]
                [else 10])
              'decorative 'normal 'normal #f))
      
      (define button-label-inset 1)
      (define drop-shadow-size 2)
      
      (define black-color (make-object color% "BLACK"))
      
      (define (calc-button-min-sizes dc label)
        (send dc set-font button-label-font)
        (let-values ([(w h a d) (send dc get-text-extent label button-label-font)])
          (let ([ans-w
                 (+ button-label-inset
                    button-label-inset
                    drop-shadow-size
                    1 ;; for the outer drop shadow
                    1 ;; becuase "(define ...)" has the wrong size under windows
                    (max 0 (inexact->exact (ceiling w))))]
                [ans-h
                 (+ button-label-inset button-label-inset
                    drop-shadow-size
                    1 ;; for the outer drop shadow
                    (max 0 (inexact->exact (ceiling h))))])
            (values ans-w ans-h))))
      
      (define (offset-color color offset-one)
        (make-object color%
          (offset-one (send color red))
          (offset-one (send color green))
          (offset-one (send color blue))))
      
      (define light-button-color (offset-color (get-panel-background)
                                               (lambda (v) (floor (+ v (/ (- 255 v) 2))))))
      (define dark-button-color (offset-color (get-panel-background)
                                              (lambda (v) (floor (- v (/ v 2))))))
      
      (define (draw-button-label dc label w h inverted?)
        (send dc set-text-foreground black-color)
        (send dc set-text-background (get-panel-background))
        (send dc set-pen (send the-pen-list find-or-create-pen
                               (get-panel-background) 1 'solid))
        (send dc set-brush (send the-brush-list find-or-create-brush
                                 (get-panel-background) 'solid))
        
        (send dc draw-rectangle 0 0 w h)
        
        (send dc set-pen (send the-pen-list find-or-create-pen
                               "BLACK" 1 'solid))
        (send dc set-brush
              (send the-brush-list find-or-create-brush
                    (if inverted? dark-button-color light-button-color) 'solid))
        
        (let ([border
               (lambda (d)
                 (send dc draw-rectangle
                       d d
                       (- w drop-shadow-size)
                       (- h drop-shadow-size)))])
          (if inverted?
              (let loop ([n 0])
                (cond
                  [(= n drop-shadow-size) (void)]
                  [else
                   (border n)
                   (loop (+ n 1))]))
              (let loop ([n drop-shadow-size])
                (cond
                  [(zero? n) (void)]
                  [else
                   (border (- n 1))
                   (loop (- n 1))]))))
        
        (when label
          (send dc set-font button-label-font)
          (let-values ([(tw th _2 _3) (send dc get-text-extent label)])
            
            ;; 1 is for the outer drop shadow box
            (send dc draw-text label
                  (+ button-label-inset
                     (if inverted? drop-shadow-size 1))
                  (+ button-label-inset
                     (if inverted? drop-shadow-size 1))))))
      
      (define name-message%
        (class canvas% 
          (init-field parent)
          (inherit popup-menu get-dc get-size get-client-size min-width min-height
                   stretchable-width stretchable-height
                   get-top-level-window)
          (public set-message) ;; set-message : boolean (union #f string) -> void
          (override on-event on-paint)
          
          (define paths #f)
          (define label (string-constant untitled))
          (define (set-message file-name? path-name)
            (set! paths (if (and file-name? (file-exists? path-name))
                            (mzlib:file:explode-path (mzlib:file:normalize-path path-name))
                            #f))
            (let ([new-label (if (and paths (not (null? paths)))
                                 (car (mzlib:list:last-pair paths))
                                 path-name)])
              (unless (equal? label new-label)
                (set! label new-label)
                (update-min-sizes)
                (on-paint))))
          
          (define full-name-window #f)
          
          (define mouse-grabbed? #f)
          (define (on-event evt)
            (cond
              [(and paths (not (null? paths))) 
               (cond
                 [(send evt button-down?)
                  (let-values ([(width height) (get-client-size)])
                    
                    (set! inverted? #t)
                    (on-paint)
                    (let ([menu (make-object popup-menu% #f
                                  (lambda x
                                    (set! inverted? #f)
                                    (on-paint)))])
                      (let loop ([paths (cdr (reverse paths))])
                        (cond
                          [(null? paths) (void)]
                          [else 
                           (make-object menu-item% (car paths) menu
                             (lambda (evt item)
                               (parameterize ([finder:dialog-parent-parameter
                                               (get-top-level-window)])
                                 (let ([file (finder:get-file
                                              (apply build-path (reverse paths)))])
                                   (when file
                                     (handler:edit-file file))))))
                           (loop (cdr paths))]))
                      (popup-menu menu
                                  0
                                  height)))]
                 [else (void)])]
              [else
               (cond
                 [(send evt moving?)
                  (when mouse-grabbed?
                    (let-values ([(max-x max-y) (get-size)])
                      (let ([inside? (and (<= 0 (send evt get-x) max-x)
                                          (<= 0 (send evt get-y) max-y))])
                        (unless (eq? inside? inverted?)
                          (set! inverted? inside?)
                          (on-paint)))))]
                 [(send evt button-up? 'left)
                  (set! mouse-grabbed? #f)
                  (cond
                    [inverted?
                     (set! inverted? #f)
                     (on-paint)
                     (message-box 
                      (string-constant drscheme)
                      (string-constant no-full-name-since-not-saved)
                      (get-top-level-window))]
                    [else
                     (void)])]
                 [(send evt button-down? 'left)
                  (set! mouse-grabbed? #t)
                  (set! inverted? #t)
                  (on-paint)]
                 [else (void)])]))
          
          (define (update-min-sizes)
            (let-values ([(w h) (calc-button-min-sizes (get-dc) label)])
              (min-width w)
              (min-height h)
              (send parent reflow-container)))
          
          (define inverted? #f)
          
          (define (on-paint)
            (let ([dc (get-dc)])
              (let-values ([(w h) (get-client-size)])
                (when (and (> w 5) (> h 5))
                  (draw-button-label dc label w h inverted?)))))
          
          (super-make-object parent)
          (update-min-sizes)
          (stretchable-width #f)
          (stretchable-height #f)))
      
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
          
          (override file-menu:open-callback file-menu:open-string
                    file-menu:new-callback  file-menu:new-string
                    help-menu:about-callback help-menu:about-string help-menu:create-about?
                    help-menu:before-about
                    file-menu:between-open-and-revert
                    edit-menu:between-find-and-preferences)
          [define help-menu:before-about
            (lambda (help-menu)
              (make-object menu-item%
                (string-constant help-desk)
                help-menu
                (lambda (item evt)
                  (help:help-desk)))
              (make-object menu-item%
                (format (string-constant welcome-to-something)
                        (string-constant drscheme))
                help-menu
                (lambda (item evt)
                  (drscheme:app:invite-tour))))]
          
          [define help-menu:about-callback (lambda (item evt) (drscheme:app:about-drscheme))]
          [define help-menu:about-string (lambda () (string-constant about-drscheme))]
          [define help-menu:create-about? (lambda () #t)]
          
          (define/override (help-menu:after-about menu)
            (instantiate menu-item% ()
              (label (string-constant help-menu-check-for-updates))
              (parent menu)
              (callback
               (lambda (item evt)
		 (when 
		  (eq? 'yes 
		       (message-box 
			(string-constant vc-update-check)
		        (string-constant vc-check-prompt)
			this
			'(yes-no)))
                 (check-version this)))))
            (drscheme:app:add-language-items-to-help-menu menu))
          
          [define (file-menu:open-callback item evt) (handler:open-file)]
          (define (file-menu:new-string) (string-constant new-menu-item))
          (define (file-menu:open-string) (string-constant open-menu-item))
          [define file-menu:new-callback
            (lambda (item evt)
              (drscheme:unit:open-drscheme-window))]
          
          (rename [super-file-menu:between-open-and-revert file-menu:between-open-and-revert])
          [define file-menu:between-open-and-revert
            (lambda (file-menu) 
              (make-object menu-item% 
                (string-constant open-url...)
                file-menu
                (lambda (item evt)
                  (help:open-users-url this)))
              (super-file-menu:between-open-and-revert file-menu))]
          
          [define edit-menu:between-find-and-preferences
            (lambda (menu)
              (instantiate menu-item% ()
                (label (string-constant mfs-multi-file-search-menu-item))
                (parent menu)
                (callback
                 (lambda (_1 _2)
                   (drscheme:multi-file-search:multi-file-search))))
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
              (make-object separator-menu-item% menu))]
          
          (super-instantiate ())))
      
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
          get-show-menu
          update-shown))
      
      (define -mixin
        (mixin (frame:editor<%> frame:text-info<%> basics<%>) (<%>)
          (inherit get-editor)
          (rename [super-file-menu:print-callback file-menu:print-callback])
          (inherit get-info-panel)
          (field
           [show-menu #f])
          (public get-show-menu update-shown)
          [define get-show-menu (lambda () show-menu)]
          [define update-shown (lambda () (void))]
          
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
          
          (inherit get-menu% get-menu-bar)
          (super-instantiate ())
          (set! show-menu (make-object (get-menu%) (string-constant show-menu-label)
                            (get-menu-bar)))
          
          (field
           [running-message
            (make-object message% sleepy-bitmap (get-info-panel))]))))))
