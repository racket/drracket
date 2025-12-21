#lang racket/base

(module install-pkg racket/base
  (require racket/class
           pkg/gui)
  (provide install-pkg
           pkg-manager)

  (define pkg-gui #f)
  
  (define (pkg-manager wrap-terminal-action)
    (if pkg-gui
        (send pkg-gui show #t)
        (set! pkg-gui (make-pkg-gui #:wrap-terminal-action wrap-terminal-action))))
  
  (define (install-pkg parent wrap-terminal-action 
                       #:package-to-offer [package-to-offer #f])
    (make-pkg-installer #:parent parent 
                        #:wrap-terminal-action wrap-terminal-action
                        #:package-to-offer package-to-offer)))

(module add-racket-to-path racket/base
  (require string-constants 
           racket/system
           racket/gui/base
           racket/class
           racket/list
           setup/dirs)
  
  (provide add-menu-path-item)
  
  (define authopen "/usr/libexec/authopen")
  (define paths.d "/etc/paths.d") 
  
  (define (add-menu-path-item menu)
    (case (system-type)
      [(macosx) (add-menu-macosx-path-item menu)]
      [(windows) (add-menu-windows-path-item menu)]
      [(unix) (add-menu-unix-path-item menu)]))

  (define (add-menu-unix-path-item menu)
    (define console-bin-dir (find-console-bin-dir))
    (when console-bin-dir
      (new menu-item%
           [label (string-constant add-racket/bin-to-path)]
           [parent menu]
           [callback (λ (x y)
                       (message-box
                        (string-constant drracket)
                        (format (string-constant didnt-add-racket/bin-to-path/unix)
                                console-bin-dir)))])))

  (define (add-menu-macosx-path-item menu)
    (define bin-dir (find-console-bin-dir))
      (when bin-dir
        (when (file-exists? authopen)
          (cond
            [(directory-exists? paths.d)
             (define paths.d/racket (build-path paths.d "racket"))
             (define (add-racket/bin-to-path)
               (define sp (open-output-string))
               (define succeeded?
                 (parameterize ([current-input-port
                                 (open-input-string
                                  (format "~a\n" bin-dir))]
                                [current-output-port sp]
                                [current-error-port sp])
                   (system* authopen "-c" "-w" (path->string paths.d/racket))))
               (define output (get-output-string sp))
               (define basic-success-message
                 (format (string-constant added-racket/bin-to-path)
                         paths.d/racket
                         bin-dir
                         paths.d/racket))
               (cond
                 [(and (equal? output "") succeeded?)
                  (message-box (string-constant drracket)
                               basic-success-message)]
                 [succeeded?
                  (message-box (string-constant drracket)
                               (string-append
                                basic-success-message
                                "\n\n" output))]
                 [else
                  (message-box (string-constant drracket)
                               (format (string-constant adding-racket/bin-to-path-failed)
                                       (if (equal? output "")
                                           ""
                                           (string-append "\n\n" output "\n\n"))
                                       paths.d/racket
                                       bin-dir))])
               (void))
             (new menu-item%
                  [label (string-constant add-racket/bin-to-path)]
                  [parent menu]
                  [callback (λ (x y) (add-racket/bin-to-path))])]
            [else
             (new menu-item%
                  [label (string-constant add-racket/bin-to-path)]
                  [parent menu]
                  [callback (λ (x y)
                              (message-box (string-constant drracket)
                                           (string-constant adding-racket/bin-no-paths.d)
                                           #f
                                           '(stop ok)))])]))))

  (define (add-menu-windows-path-item menu)
    (new menu-item%
         [label (string-constant add-racket/bin-to-path)]
         [parent menu]
         [callback (λ (x y) (add-path-if-not-present/windows))]))

  (define error-name 'add-to-windows-path)

  (define (add-path-if-not-present/windows)
    (with-handlers ([exn:fail?
                     (λ (x)
                       (message-box (string-constant drracket)
                                    (exn-message x)))])
      (add-path-if-not-present/windows/raise-error)))

  (define (add-path-if-not-present/windows/raise-error)
    (define HKEY_CURRENT_USER "HKEY_CURRENT_USER")
    (define Environment\\Path "Environment\\Path")
    (define resource-bytes
      (get-resource HKEY_CURRENT_USER Environment\\Path
                    #:type 'bytes))
    (unless (bytes? resource-bytes)
      (raise-user-error error-name "could not read the resource ~a\\~a"
                        HKEY_CURRENT_USER Environment\\Path))
    (define converted-bytes (reecode resource-bytes "WTF-16" "WTF-8"))
    (define no-null-bytes (regexp-replace #rx#"\0$" converted-bytes #""))
    (define already-there (path-list-string->path-list no-null-bytes '()))
    (define bin-dir (find-console-bin-dir))
    (define user-console-dir (find-user-console-bin-dir))
    (unless bin-dir
      (raise-user-error error-name
                        "could not find the path to add"))
    (define has-bin-dir? (member bin-dir already-there))
    (define has-user-console-dir? (member user-console-dir already-there))
    (when (and has-bin-dir? has-user-console-dir?)
      (raise-user-error error-name
                        "directories already present\n  dir: ~a\n  dir: ~a"
                        bin-dir
                        (find-user-console-bin-dir)))
    (define extended-path-bytes no-null-bytes)
    (unless has-bin-dir?
      (set! extended-path-bytes (bytes-append (path->bytes bin-dir) #";" extended-path-bytes)))
    (unless has-user-console-dir?
      (set! extended-path-bytes
            (bytes-append (path->bytes user-console-dir)
                          #";"
                          (remove-addon-dir-extensions extended-path-bytes))))
    (define to-be-written-bytes (reecode (bytes-append extended-path-bytes #"\0") "WTF-8" "WTF-16"))
    (define result
      (write-resource HKEY_CURRENT_USER
                      Environment\\Path
                      to-be-written-bytes
                      #:type 'bytes/expand-string))
    (unless result
      (raise-user-error error-name
                        "could not write the resource\n  section: ~s\n  entry: ~s\n  bytes: ~.s"
                        HKEY_CURRENT_USER
                        Environment\\Path
                        to-be-written-bytes))
    (message-box
     (string-constant drracket)
     (format (string-constant added-racket/bin-to-path/windows)
             bin-dir
             user-console-dir)))

  (define (reecode resource-bytes from to)
    (define converter (bytes-open-converter from to))
    (define-values (converted-bytes amt termination)
      (bytes-convert converter resource-bytes))
    (bytes-close-converter converter)
    (unless (equal? termination 'complete)
      (raise-user-error error-name
                        "could not convert from ~a to ~a\n  bytes: ~s"
                        from to
                        resource-bytes))
    converted-bytes)

  (define (remove-addon-dir-extensions bytes)
    (define dirs (regexp-split #rx#";" bytes))
    (define exploded-addon (explode-path (find-system-path 'addon-dir)))
    (define (is-below-addon? dir)
      (let loop ([addon exploded-addon]
                 [candidate (explode-path (bytes->path dir))])
        (cond
          [(null? addon) #t]
          [(null? candidate) #f]
          [else (and (equal? (car addon) (car candidate))
                     (loop (cdr addon) (cdr candidate)))])))
    (define w/out-addon (for/list ([dir (in-list dirs)]
                                   #:when (or (equal? dir #"")
                                              (not (is-below-addon? dir))))
                          dir))
    (apply bytes-append (add-between w/out-addon #";")))
  )

(module key-bindings racket/base
  
  (require racket/class
           racket/gui/base
           racket/contract
           framework)
  
  (provide
   (contract-out
    [get-sorted-keybindings
     (-> (or/c #f (is-a?/c text%))
         (or/c #f (is-a?/c frame%))
         (listof (list/c symbol? string?)))]))
  
  (define (get-sorted-keybindings edit-object frame)
    (define keymap (and edit-object (send edit-object get-keymap)))
    (define menu-names (if frame (get-menu-bindings frame) '()))
    (define bindings (if (is-a? keymap keymap:aug-keymap<%>)
                         (hash-map (send keymap get-map-function-table) list) 
                         '()))
    (define w/menus 
      (append (hash-map menu-names list)
              (filter (λ (binding) (not (bound-by-menu? binding menu-names)))
                      bindings)))
    (sort
     (sort
      w/menus
      symbol<?
      #:key car)
     string-ci<?
     #:key cadr))

  (define (bound-by-menu? binding menu-table)
    (ormap (λ (constituent)
             (hash-ref menu-table (string->symbol constituent) #f))
           (regexp-split #rx";" (symbol->string (car binding)))))
  
  (define (get-menu-bindings frame)
    (define name-ht (make-hasheq))
    (define mb (send frame get-menu-bar))
    (when mb
      (let loop ([menu-container mb])
        (for ([item (in-list (send menu-container get-items))])
          (when (is-a? item selectable-menu-item<%>)
            (define short-cut (send item get-shortcut))
            (when short-cut
              (define keyname
                (string->symbol
                 (keymap:canonicalize-keybinding-string
                  (string-append
                   (menu-item->prefix-string item)
                   (case short-cut
                     [(#\;) "semicolon"]
                     [(#\:) "colon"]
                     [(#\space) "space"]
                     [else 
                      (cond
                        [(symbol? short-cut) (symbol->string short-cut)]
                        [(char? short-cut) (string short-cut)])])))))
              (hash-set! name-ht keyname (send item get-plain-label))))
          (when (is-a? item menu-item-container<%>)
            (loop item))))
      (when (member (system-type) '(unix windows))
        (for ([top-level-menu (in-list (send mb get-items))])
          (when (is-a? top-level-menu menu%)
            (define amp-key
              (let loop ([str (send top-level-menu get-label)])
                (cond
                  [(regexp-match #rx"[^&]*[&](.)(.*)" str)
                   =>
                   (λ (m)
                     (define this-amp (list-ref m 1))
                     (define rest (list-ref m 2))
                     (cond
                       [(equal? this-amp "&")
                        (loop rest)]
                       [else 
                        (string-downcase this-amp)]))]
                  [else #f])))
            (when amp-key
              (hash-set! name-ht 
                         (string->symbol (format "m:~a" amp-key))
                         (format "~a menu" (send top-level-menu get-plain-label)))
              (when (equal? (system-type) 'windows)
                (hash-set! name-ht 
                           (string->symbol (format "m:s:~a" amp-key))
                           (format "~a menu" (send top-level-menu get-plain-label)))))))))
    name-ht)
  
  (define (menu-item->prefix-string item)
    (apply
     string-append
     (for/list ([prefix (in-list (send item get-shortcut-prefix))])
       (case prefix
         [(alt) (if (eq? (system-type) 'windows) "m:" "a:")]
         [(cmd) "d:"]
         [(meta) "m:"]
         [(ctl) "c:"]
         [(shift) "s:"]
         [(opt option) "a:"]
         [else (error 'menu-item->prefix-string "unknown prefix ~s\n" prefix)])))))

(require string-constants
         racket/match
         racket/class
         racket/string
         racket/file
         racket/math
         racket/unit
         drracket/private/drsig
         racket/gui/base
         framework
         framework/private/srcloc-panel
         net/url
         net/head
         setup/unpack
         mrlib/terminal
         browser/external
         (submod "." install-pkg)
         drracket/get-module-path)
(provide frame@)
(define-unit frame@  
  (import [prefix drracket:unit: drracket:unit/int^]
          [prefix drracket:app: drracket:app^]
          [prefix help: drracket:help-desk^]
          [prefix drracket:multi-file-search: drracket:multi-file-search^]
          [prefix drracket:init: drracket:init/int^]
          [prefix drracket: drracket:interface^])
  (export (rename drracket:frame/int^
                  [-mixin mixin]))
  
  (define basics-mixin
    (mixin (frame:standard-menus<%>) (drracket:frame:basics<%>)
      
      (define/override (on-subwindow-focus win on?)
        (when the-keybindings-frame
          (when on? 
            (send the-keybindings-frame set-bindings
                  (if (can-show-keybindings?)
                      (get-keybindings-to-show)
                      '())))))
      
      (define/override (on-subwindow-char receiver event)
        (define user-key? (send (keymap:get-user)
                                handle-key-event
                                (if (is-a? receiver editor-canvas%)
                                    (send receiver get-editor)
                                    receiver)
                                event))
        ;; (printf "user-key? ~s\n" user-key?)
        (or user-key?
            (super on-subwindow-char receiver event)))
      
      (inherit get-edit-target-window get-edit-target-object get-menu-bar)
      
            
      (define/private (can-show-keybindings?)
        (define edit-object (get-edit-target-object))
        (and edit-object
             (is-a? edit-object editor<%>)
             (let ([keymap (send edit-object get-keymap)])
               (is-a? keymap keymap:aug-keymap<%>))))
      
      ;; pre: (can-show-keybindings?) = #t
      (define/private (get-keybindings-to-show)
        (get-sorted-keybindings (get-edit-target-object) this))
      
      (define/private (show-keybindings)
        (if (can-show-keybindings?)
            (show-keybindings-to-user (get-keybindings-to-show) this)
            (bell)))
      
      (define/override (help-menu:before-about help-menu)
        (make-help-desk-menu-item help-menu))
      
      (define/override (help-menu:about-callback item evt) (drracket:app:about-drscheme))
      (define/override (help-menu:about-string) (string-constant about-drscheme))
      (define/override (help-menu:create-about?) #t)
      
      (define/public (get-additional-important-urls) '())
      (define/override (help-menu:after-about menu)
        (drracket-help-menu:after-about menu this))
      
      (define/override (file-menu:new-string) (string-constant new-menu-item))
      (define/override (file-menu:create-new?) #t)
      (define/override (file-menu:open-string) (string-constant open-menu-item))
      (define/override (file-menu:create-open?) #t)
      (define/override (file-menu:create-open-recent?) #t)
      
      (define/override (file-menu:between-open-and-revert file-menu)
        (new menu-item% 
             [label (string-constant install-pkg-menu-item...)]
             [parent file-menu]
             [callback
              (λ (item evt) 
                 (install-pkg this
                              (lambda (thunk)
                                (parameterize ([error-display-handler 
                                                drracket:init:original-error-display-handler])
                                  (thunk)))))])
        (new separator-menu-item% [parent file-menu])
        (new menu-item% 
             [label (string-constant pkg-manager-menu-item)]
             [parent file-menu]
             [callback
              (λ (item evt) 
                (pkg-manager (lambda (thunk)
                               (parameterize ([error-display-handler 
                                               drracket:init:original-error-display-handler])
                                 (thunk)))))])
        (super file-menu:between-open-and-revert file-menu))
      
      (define/override (file-menu:between-print-and-close menu)
        (super file-menu:between-print-and-close menu)
        (instantiate menu-item% ()
          (label (string-constant mfs-multi-file-search-menu-item))
          (parent menu)
          (callback
           (λ (_1 _2)
             (drracket:multi-file-search:multi-file-search))))
        (new separator-menu-item% (parent menu)))
      
      (define/override (edit-menu:between-find-and-preferences menu)
        (super edit-menu:between-find-and-preferences menu)
        (when (current-eventspace-has-standard-menus?)
          (new separator-menu-item% [parent menu]))
        (define (keybindings-on-demand menu-item)
          (define last-edit-object (get-edit-target-window))
          (send menu-item enable (can-show-keybindings?)))
        (new menu%
             (label (string-constant keybindings-menu-item))
             (parent menu)
             (demand-callback
              (λ (keybindings-menu)
                (for-each (λ (old) (send old delete)) 
                          (send keybindings-menu get-items))
                (new menu-item%
                     (parent keybindings-menu)
                     (label (string-constant keybindings-show-active))
                     (callback (λ (x y) (show-keybindings)))
                     (help-string (string-constant keybindings-info))
                     (demand-callback keybindings-on-demand))
                (new menu-item%
                     (parent keybindings-menu)
                     (label (string-constant keybindings-add-user-defined-keybindings))
                     (callback
                      (λ (x y)
                        (with-handlers ([exn? (λ (x)
                                                (printf "~a\n" (exn-message x)))])
                          (define filename
                            (finder:get-file
                             #f
                             (string-constant keybindings-choose-user-defined-file)
                             #f
                             ""
                             this))
                          (when filename
                            (add-keybindings-item/update-prefs filename))))))
                (define ud (preferences:get 'drracket:user-defined-keybindings))
                (unless (null? ud)
                  (new separator-menu-item% (parent keybindings-menu))
                  (for ([item (in-list ud)])
                    (new menu-item%
                         (label (format (string-constant keybindings-menu-remove)
                                        (if (path? item)
                                            (path->string item)
                                            (format "~s" item))))
                         (parent keybindings-menu)
                         (callback
                          (λ (x y) (remove-keybindings-item item)))))))))
        (unless (current-eventspace-has-standard-menus?)
          (make-object separator-menu-item% menu)))
      
      (super-new)))
  
  (define (add-keybindings-item/update-prefs item)
    (when (add-keybindings-item item)
      (preferences:set 'drracket:user-defined-keybindings
                       (cons item
                             (preferences:get 'drracket:user-defined-keybindings)))))
  
  (define (planet-string-spec? p)
    (define sexp
      (with-handlers ([exn:fail:read? (λ (x) #f)])
        (read (open-input-string p))))
    (and sexp
         (planet-spec? sexp)
         sexp))
  
  (define (planet-spec? p)
    (match p
      [`(planet ,(? string?) (,(? string?) ,(? string?) ,(? number?))) #t]
      [`(planet ,(? string?) (,(? string?) ,(? string?) ,(? number?) ,(? number?))) #t]
      [else #f]))
  
  ;; add-keybindings-item : keybindings-item[path or planet spec] -> boolean
  ;; boolean indicates if the addition happened sucessfully
  (define (add-keybindings-item item)
    (with-handlers ([exn:fail?
                     (λ (x)
                       (message-box (string-constant drscheme)
                                    (format (string-constant keybindings-error-installing-file)
                                            (if (path? item)
                                                (path->string item)
                                                (format "~s" item))
                                            (exn-message x))
                                    #:dialog-mixin frame:focus-table-mixin)
                       #f)])
      (keymap:add-user-keybindings-file item)
      #t))
  
  (define (remove-keybindings-item item)
    (keymap:remove-user-keybindings-file item)
    (preferences:set
     'drracket:user-defined-keybindings
     (remove item
             (preferences:get 'drracket:user-defined-keybindings))))
 
  (define keybindings-frame%
    (class frame%
      (init-field bindings)
      
      (define/override (on-size w h)
        (preferences:set 'drracket:keybindings-window-size (cons w h))
        (super on-size w h))

      (super-new)
  
      (define/public (set-bindings _bindings)
        (set! bindings _bindings)
        (update-bindings))
      
      (define bp (make-object horizontal-panel% this))
      (define search-field 
        (keymap:call/text-keymap-initializer
         (λ ()
           (new text-field% 
                [parent this]
                [label (string-constant mfs-search-string)]
                [callback (λ (a b) (update-bindings))]))))
      (define b-name (new button%
                          [label (string-constant keybindings-sort-by-name)]
                          [parent bp]
                          [callback
                           (λ x 
                             (set! by-key? #f)
                             (update-bindings))]))
      (define b-key (new button%
                         [label (string-constant keybindings-sort-by-key)]
                         [parent bp]
                         [callback (λ x 
                                     (set! by-key? #t)
                                     (update-bindings))]))
      (define lb (new list-box% [parent this] [label #f] [choices '()]))
      (define by-key? #f)
      (define bp2 (make-object horizontal-panel% this))
      (define cancel (make-object button% (string-constant close)
                       bp2 (λ x (send this show #f))))

      (define/private (update-bindings)
        (define (format-binding/name b)
          (format "~a (~a)" (cadr b) (car b)))
        (define (format-binding/key b)
          (format "~a (~a)" (car b) (cadr b)))
        (define (predicate/key a b)
          (string-ci<=? (format "~a" (car a))
                        (format "~a" (car b))))
        (define (predicate/name a b)
          (string-ci<=? (cadr a) (cadr b)))
        (send lb set
              (if by-key?
                  (map format-binding/key (sort (filter-search bindings) predicate/key))
                  (map format-binding/name (sort (filter-search bindings) predicate/name)))))
      
      (define/private (filter-search bindings)
        (define str (send search-field get-value))
        (cond
          [(equal? str "") bindings]
          [else
           (define reg (regexp (regexp-quote str #f)))
           (filter (λ (x) (or (regexp-match reg (cadr x))
                              (regexp-match reg (format "~a" (car x)))))
                   bindings)]))
      (send search-field focus)
      (send bp stretchable-height #f)
      (send bp set-alignment 'center 'center)
      (send bp2 stretchable-height #f)
      (send bp2 set-alignment 'right 'center)
      (update-bindings)))

  (define the-keybindings-frame #f)
  
  (define (show-keybindings-to-user bindings frame)
    (unless the-keybindings-frame
      (set! the-keybindings-frame
            (new keybindings-frame% 
                 [label (string-constant keybindings-frame-title)]
                 [width (car (preferences:get 'drracket:keybindings-window-size))]
                 [height (cdr (preferences:get 'drracket:keybindings-window-size))]
                 [bindings bindings])))
    (send the-keybindings-frame show #t))
  
  (define -mixin
    (mixin (frame:editor<%> frame:text-info<%> drracket:frame:basics<%>) (drracket:frame:<%>)
      (inherit get-editor get-menu% get-menu-bar)
      (define show-menu #f)
      (define/public get-show-menu (λ () show-menu))
      (define/public update-shown (λ () (void)))
      (define/public (add-show-menu-items show-menu) (void))
      (define sort-menu-sort-keys (make-hasheq))
      (define/public (set-show-menu-sort-key item val)
        (cond
          [sort-menu-sort-keys 
           (for ([(k v) (in-hash sort-menu-sort-keys)])
             (when (eq? k item)
               (error 'set-show-menu-sort-key
                      "set menu item ~s twice, to ~s and ~s"
                      (send item get-label)
                      v val))
             (when (= v val)
               (error 'set-show-menu-sort-key
                      "two menu items have the same val: ~s and ~s"
                      (send k get-label)
                      (send item get-label))))
           (hash-set! sort-menu-sort-keys item val)]
          [else
           (error 'set-show-menu-sort-key 
                  "the sort menu has already been created and its order has been set")]))
      (super-new)
      (set! show-menu (make-object (get-menu%) (string-constant view-menu-label)
                        (get-menu-bar)))
      (add-show-menu-items show-menu)
      (sort-show-menu-items show-menu sort-menu-sort-keys)
      (set! sort-menu-sort-keys #f)))
  
  (define (sort-show-menu-items show-menu show-menu-sort-keys)
    (define items (send show-menu get-items))
    (for ([itm (in-list items)])
      (send itm delete))
    (define (get-key item)
      (hash-ref show-menu-sort-keys item 
                (λ () 
                  (define lab
                    (cond
                      [(is-a? item labelled-menu-item<%>)
                       (send item get-label)]
                      [else ""]))
                  (cond 
                    [(regexp-match #rx"^Show (.*)$" lab)
                     => (λ (x) (list-ref x 1))]
                    [(regexp-match #rx"^Hide (.*)$" lab)
                     => (λ (x) (list-ref x 1))]
                    [else lab]))))
    (define (cmp item-x item-y)
      (define x (get-key item-x))
      (define y (get-key item-y))
      (cond
        [(and (number? x) (number? y)) (< x y)]
        [(and (string? x) (string? y)) (string<=? x y)]
        [(and (number? x) (string? y)) #t]
        [(and (string? x) (number? y)) #f]))
    (define sorted-items (sort items cmp))
    
    (define (different-slots? item-key next-item-key)
      (or (not (= (quotient item-key 100)
                  (quotient next-item-key 100)))
          (not (= (sgn item-key)
                  (sgn next-item-key)))))
    
    (for ([item (in-list sorted-items)]
          [next-item (in-list (append (cdr sorted-items) (list #f)))])
      (define item-key (get-key item))
      (define next-item-key (and next-item (get-key next-item)))
      (define add-sep?
        (cond
          [(and (number? item-key) (number? next-item-key))
           (different-slots? item-key next-item-key)]
          [(or (and (string? item-key) (string? next-item-key))
               (not next-item-key))
           #f]
          [else #t]))
      (send item restore)  
      (when add-sep?
        (new separator-menu-item% [parent show-menu]))))
  
  
  (define (create-root-menubar)
    (define mb (new menu-bar% (parent 'root)))
    (define file-menu (new menu% 
                           (label (string-constant file-menu))
                           (parent mb)))
    (define help-menu (new menu% 
                           (label (string-constant help-menu))
                           (parent mb)))
    (new menu-item%
         (label (string-constant new-menu-item))
         (parent file-menu)
         (shortcut #\n)
         (callback
          (λ (x y)
            (handler:edit-file #f)
            #t)))
    (new menu-item%
         (label (string-constant open-menu-item))
         (parent file-menu)
         (shortcut #\o)
         (callback
          (λ (x y)
            (handler:open-file)
            #t)))
    (new menu-item%
         [label (string-constant open-require-path)]
         [shortcut #\o]
         [shortcut-prefix (cons 'shift (get-default-shortcut-prefix))]
         [parent file-menu]
         [callback
          (λ (x y)
            (define pth
              (get-module-path-from-user
               #:init (preferences:get 'drracket:open-module-path-last-used)
               #:pref 'drracket:open-module-path-last-used))
            (when pth (handler:edit-file pth)))])
    (new menu%
         (label (string-constant open-recent-menu-item))
         (parent file-menu)
         (demand-callback
          (λ (menu)
            (handler:install-recent-items menu))))
    (new menu:can-restore-menu-item%
         (label (string-constant reopen-closed-tab))
         (shortcut #\t)
         (shortcut-prefix (cons 'shift (get-default-shortcut-prefix)))
         (parent file-menu)
         [demand-callback (λ (item) (send item enable (pair? (preferences:get 'drracket:recently-closed-tabs))))]
         (callback
          (λ (x y)
            (define fr (handler:edit-file #f))
            (send fr reopen-closed-tab))))
    (new menu-item%
         [label (string-constant mfs-multi-file-search-menu-item)]
         [parent file-menu]
         [callback
          (λ (_1 _2)
            (drracket:multi-file-search:multi-file-search))])
    (unless (current-eventspace-has-standard-menus?)
      (new separator-menu-item% (parent file-menu))
      (new menu-item%
           (label (string-constant quit-menu-item-others))
           (parent file-menu)
           (shortcut #\q)
           (callback
            (λ (x y)
              (when (exit:user-oks-exit)
                (exit:exit))
              #t))))
    (make-help-desk-menu-item help-menu)
    (drracket-help-menu:after-about help-menu #f))
  
  (define (make-help-desk-menu-item help-menu)
    (define (docs-menu-item label)
      (new menu-item%
           [label label]
           [parent help-menu]
           [callback (λ (item evt) (help:help-desk) #t)]))
    (docs-menu-item (string-constant racket-documentation))
    (new separator-menu-item% [parent help-menu])
    (docs-menu-item (string-constant help-desk)))

  (define (drracket-help-menu:after-about menu dlg-parent)
    (drracket:app:add-important-urls-to-help-menu menu '())
    (new menu-item%
         [label (string-constant have-an-issue?)]
         [parent menu]
         [callback
          (λ (x y)
            (define rslt
              (message-box/custom (string-constant drracket)
                                  (string-constant use-github-or-the-mailing-list-for-issues)
                                  (string-constant visit-mailing-lists)
                                  (string-constant visit-github)
                                  #f
                                  dlg-parent))
            (case rslt
              [(1) (send-url "https://lists.racket-lang.org/")]
              [(2) (send-url "https://github.com/racket/racket/issues/new/choose")]))])
    (add-menu-path-item menu)
    (drracket:app:add-language-items-to-help-menu menu)))


(require (submod "." add-racket-to-path)
         (submod "." key-bindings))


(module test racket/base
  (require rackunit
           racket/class
           racket/gui/base
           framework
           (submod ".." key-bindings))

  (check-equal? (get-sorted-keybindings #f (new frame% [label ""]))
                '())
  (check-equal? (get-sorted-keybindings (new text%) (new frame% [label ""]))
                '())
  
  (let ()
    (define k (new keymap%))
    (define t (new text%))
    (send t set-keymap k)
    
    (check-equal? 
     (get-sorted-keybindings t (new frame% [label ""]))
     '()))
  
  (let ()
    (define k (new keymap:aug-keymap%))
    (send k add-function "x" void)
    (send k map-function "c:x" "x")
    (define t (new text%))
    (send t set-keymap k)
  
    (check-equal? 
     (get-sorted-keybindings t (new frame% [label ""]))
     '((c:x "x"))))
  
  (let ()
    (define k (new keymap:aug-keymap%))
    (send k add-function "x" void)
    (send k map-function "c:x" "x")
    (define t (new text%))
    (send t set-keymap k)
    
    (define f (new frame% [label ""]))
    (define mb (new menu-bar% [parent f]))
    (define m (new menu% [label "Edit"] [parent mb]))
    (define mi (new menu-item% [label "Cut"] [shortcut #\x] [parent m] [callback void]))
    
    (check-equal? 
     (get-sorted-keybindings t f)
     (case (system-type)
       [(macosx) 
        '((d:x "Cut") (c:x "x"))]
       [(windows unix)
        '((c:x "Cut"))])))
  
  (let ()
    (define k (new keymap:aug-keymap%))
    (send k add-function "x" void)
    (send k map-function "c:x" "x")
    (define t (new text%))
    (send t set-keymap k)
    
    (define f (new frame% [label ""]))
    (define mb (new menu-bar% [parent f]))
    (define m (new menu% [label "&Edit"] [parent mb]))
    (define mi (new menu-item% [label "Cu&t"] [shortcut #\x] [parent m] [callback void]))
    
    (check-equal? 
     (get-sorted-keybindings t f)
     (case (system-type)
       [(macosx)
        '((d:x "Cut")
          (c:x "x"))]
       [(windows)
        '((c:x "Cut")
          (m:e "Edit menu")
          (m:s:e "Edit menu"))]
       [(unix)
        '((c:x "Cut")
          (m:e "Edit menu"))])))
  
  (let ()
    (define k (new keymap:aug-keymap%))
    (send k add-function "x" void)
    (send k map-function "m:e" "x")
    (define t (new text%))
    (send t set-keymap k)
    
    (define f (new frame% [label ""]))
    (define mb (new menu-bar% [parent f]))
    (define m (new menu% [label "&Edit"] [parent mb]))
    (define mi (new menu-item% [label "Cu&t"] [parent m] [callback void]))
    
    (check-equal? 
     (get-sorted-keybindings t f)
     (case (system-type)
       [(macosx)
        '()]
       [(windows)
        '((m:e "Edit menu")
          (m:s:e "Edit menu"))]
       [(unix)
        '((m:e "Edit menu"))])))
  
  
  (let ()
    (define k (new keymap:aug-keymap%))
    (send k add-function "x" void)
    (send k add-function "y" void)
    (send k map-function "c:x" "x")
    (send k map-function "m:x" "y")
    (define t (new text%))
    (send t set-keymap k)
    
    (define f (new frame% [label ""]))
    (define mb (new menu-bar% [parent f]))
    (define m (new menu% [label "Edit"] [parent mb]))
    (define mi (new menu-item% [label "Cu&t"] [parent m] [callback void]))
    
    (check-equal? 
     (get-sorted-keybindings t f)
     (case (system-type)
       [(macosx) '((c:x "x"))]
       [(unix windows)
        '((c:x "x")
          (m:x "y"))])))
  
  (let ()
    (define k (new keymap:aug-keymap%))
    (send k add-function "y" void)
    (send k map-function "~c:m:x" "y")
    (define t (new text%))
    (send t set-keymap k)
  
    (check-equal? 
     (get-sorted-keybindings t (new frame% [label ""]))
     (if (equal? (system-type) 'macosx)
         '()
         '((~c:m:x "y"))))))
