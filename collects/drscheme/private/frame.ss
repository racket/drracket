
(module frame mzscheme
  (require (lib "unitsig.ss")
           "drsig.ss"
           "mred-wrap.ss"
           "framework-wrap.ss"
           (prefix mzlib:file: (lib "file.ss"))
           (prefix mzlib:list: (lib "list.ss"))
           (prefix zodiac: (lib "zodiac.ss" "syntax")))
  
  (provide frame@)
  (define frame@
    (unit/sig drscheme:frame^
      (import (drscheme:unit : drscheme:unit^)
              (drscheme:app : drscheme:app^)
              (help : drscheme:help-interface^))
      
      (define button-label-font
        (send mred:the-font-list find-or-create-font
              (case (system-type)
                [(windows) 8]
                [else 10])
              'decorative 'normal 'normal #f))
      
      (define button-label-inset 1)
      (define drop-shadow-size 2)
      
      (define black-color (make-object mred:color% "BLACK"))
      
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
        (make-object mred:color%
          (offset-one (send color red))
          (offset-one (send color green))
          (offset-one (send color blue))))
      
      (define light-button-color (offset-color (mred:get-panel-background)
                                               (lambda (v) (floor (+ v (/ (- 255 v) 2))))))
      (define dark-button-color (offset-color (mred:get-panel-background)
                                              (lambda (v) (floor (- v (/ v 2))))))
      
      (define (draw-button-label dc label w h inverted?)
        (send dc set-text-foreground black-color)
        (send dc set-text-background (mred:get-panel-background))
        (send dc set-pen (send mred:the-pen-list find-or-create-pen
                               (mred:get-panel-background) 1 'solid))
        (send dc set-brush (send mred:the-brush-list find-or-create-brush
                                 (mred:get-panel-background) 'solid))
        
        (send dc draw-rectangle 0 0 w h)
        
        (send dc set-pen (send mred:the-pen-list find-or-create-pen
                               "BLACK" 1 'solid))
        (send dc set-brush
              (send mred:the-brush-list find-or-create-brush
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
        (class/d mred:canvas% (parent)
          ((inherit popup-menu get-dc get-size get-client-size min-width min-height
                    stretchable-width stretchable-height
                    get-top-level-window)
           (public set-message) ;; set-message : boolean (union #f string) -> void
           (override on-event on-paint))
          
          (define paths #f)
          (define label "Untitled")
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
                    (let ([menu (make-object mred:popup-menu% #f
                                  (lambda x
                                    (set! inverted? #f)
                                    (on-paint)))])
                      (let loop ([paths (cdr (reverse paths))])
                        (cond
                          [(null? paths) (void)]
                          [else 
                           (make-object mred:menu-item% (car paths) menu
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
                     (mred:message-box "DrScheme" "The file does not have a full name because it has not yet been saved.")]
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
          
          (super-init parent)
          (update-min-sizes)
          (stretchable-width #f)
          (stretchable-height #f)))
      
      (define basics<%> (interface (frame:standard-menus<%>)))
      
      (define keybindings-dialog%
        (class mred:dialog% args
          (rename [super-on-size on-size])
          (override
            [on-size
             (lambda (w h)
               (preferences:set 'drscheme:keybindings-window-size (cons w h))
               (super-on-size w h))])
          (sequence (apply super-init args))))
      
      (define (show-keybindings-to-user bindings frame)
        (letrec ([f (make-object keybindings-dialog% "Keybindings" frame 
                      (car (preferences:get 'drscheme:keybindings-window-size))
                      (cdr (preferences:get 'drscheme:keybindings-window-size))
                      #f #f '(resize-border))]
                 [bp (make-object mred:horizontal-panel% f)]
                 [b-name (make-object mred:button% "Sort by Name" bp (lambda x (update-bindings #f)))]
                 [b-key (make-object mred:button% "Sort by Key" bp (lambda x (update-bindings #t)))]
                 [lb
                  (make-object mred:list-box% #f null f void)]
                 [bp2 (make-object mred:horizontal-panel% f)]
                 [cancel (make-object mred:button% "Close" bp2 (lambda x (send f show #f)))]
                 [space (make-object mred:grow-box-spacer-pane% bp2)]
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
      
      (define basics-mixin
        (mixin (frame:standard-menus<%>) (basics<%>) args
          (inherit get-edit-target-window get-edit-target-object get-menu-bar)
          (private
            [get-menu-bindings
             (lambda ()
               (let ([name-ht (make-hash-table)]
                     [fun-ht (make-hash-table)])
                 (let loop ([menu-container (get-menu-bar)])
                   (for-each
                    (lambda (item)
                      (when (is-a? item mred:selectable-menu-item<%>)
                        (let ([short-cut (send item get-shortcut)])
                          (when short-cut
                            (let ([keyname
                                   (keymap:canonicalize-keybinding-string
                                    (string-append
                                     (case (system-type)
                                       [(windows) "c:"]
                                       [(macos) "d:"]
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
                                                 (let ([evt (make-object mred:control-event% 'menu)])
                                                   (send evt set-time-stamp (current-milliseconds))
                                                   (send item command evt))))))))
                      (when (is-a? item mred:menu-item-container<%>)
                        (loop item)))
                    (send menu-container get-items)))
                 (values name-ht fun-ht)))]
            
            [copy-hash-table
             (lambda (ht)
               (let ([res (make-hash-table)])
                 (hash-table-for-each
                  ht
                  (lambda (x y) (hash-table-put! res x y)))
                 res))]
            [can-show-keybindings?
             (lambda ()
               (let ([edit-object (get-edit-target-object)])
                 (and edit-object
                      (is-a? edit-object mred:editor<%>)
                      (let ([keymap (send edit-object get-keymap)])
                        (is-a? keymap keymap:aug-keymap<%>)))))]
            
            [show-keybindings
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
                   (mred:bell)))])
          
          (override
            [help-menu:before-about
             (lambda (help-menu)
               (make-object mred:menu-item%
                 "Help Desk"
                 help-menu
                 (lambda (item evt)
                   (help:help-desk)))
               (make-object mred:menu-item%
                 "Welcome to DrScheme"
                 help-menu
                 (lambda (item evt)
                   (drscheme:app:invite-tour))))]
            
            [help-menu:about (lambda (item evt) (drscheme:app:about-drscheme))]
            [help-menu:about-string (lambda () "DrScheme")]
            
            
            [file-menu:new-string (lambda () "")]
            [file-menu:new
             (lambda (item evt)
               (drscheme:unit:open-drscheme-window))]
            [file-menu:open (lambda (item evt) (handler:open-file) #t)]
            [file-menu:open-string (lambda () "")]
            [file-menu:between-open-and-revert
             (lambda (file-menu) 
               (make-object mred:menu-item% 
                 "Open URL..."
                 file-menu
                 (lambda (item evt)
                   (help:open-users-url this))))]
            
            [edit-menu:between-find-and-preferences
             (lambda (menu)
               (make-object mred:separator-menu-item% menu)
               (let ([keybindings-menu-item%
                      (class mred:menu-item% args
                        (inherit enable)
                        (override
                          [on-demand
                           (lambda ()
                             (let ([last-edit-object
                                    (get-edit-target-window)])
                               (enable (can-show-keybindings?))))])
                        (sequence (apply super-init args)))])
                 (make-object keybindings-menu-item% "Keybindings" menu
                   (lambda x (show-keybindings))
                   #f
                   "Show the currently active keybindings"))
               (make-object mred:separator-menu-item% menu))])
          
          (sequence 
            (apply super-init args))))
      
      (define <%> (interface (frame:editor<%> basics<%> frame:text-info<%>)))
      
      (define -mixin
        (mixin (frame:editor<%> frame:text-info<%> basics<%>) (<%>) (name . args)
          
          
          (inherit get-editor)
          (rename [super-file-menu:print file-menu:print])
          (override
            [file-menu:print
             (lambda (item control)
               (let ([ps-setup (make-object mred:ps-setup%)])
                 (send ps-setup copy-from (mred:current-ps-setup))
                 (parameterize ([mred:current-ps-setup ps-setup])
                   (send (get-editor) print))))])
          
          (rename [super-make-root-area-container make-root-area-container])
          (inherit get-info-panel)
          (public
            [root-panel #f])
          (override
            [make-root-area-container
             (lambda (% parent)
               (let* ([s-root (super-make-root-area-container mred:vertical-panel% parent)]
                      [root (make-object % s-root)])
                 (set! root-panel s-root)
                 root))])
          
          (public
            [show-menu #f]
            
            [update-shown (lambda () (void))])
          
          (private
            [get-bitmap/string
             (lambda (icon string)
               (let ([p (build-path (collection-path "icons") icon)])
                 (if (file-exists? p)
                     (make-object mred:bitmap% p 'gif)
                     string)))]
            [currently-running? #f]
            [sleepy-bitmap (get-bitmap/string "snoopy-sleepy.gif" "not running")]
            [active-bitmap (get-bitmap/string "snoopy-active.gif" "running")])
          (public
            [running
             (lambda ()
               (unless currently-running?
                 (set! currently-running? #t)
                 (send running-message set-label active-bitmap)))]
            [not-running
             (lambda ()
               (when currently-running?
                 (set! currently-running? #f)
                 (send running-message set-label sleepy-bitmap)))])
          
          (inherit get-menu% get-menu-bar)
          (sequence 
            (apply super-init name args)
            (set! show-menu (make-object (get-menu%) "&Show" (get-menu-bar))))
          
          (private
            [running-message
             (make-object mred:message% sleepy-bitmap (get-info-panel))]))))))