
(module unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "string-constant.ss" "string-constants")
	   (lib "framework.ss" "framework")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "name-message.ss" "mrlib")
           
           "drsig.ss"
	   
           (lib "mred.ss" "mred")
           (prefix mred: (lib "mred.ss" "mred"))

           (prefix mzlib:file: (lib "file.ss"))
           (prefix mzlib:date: (lib "date.ss")))
  
  (provide unit@)
  
  (define module-browser-progress-constant "Module Browser: ~a")
  (define status-compiling-definitions "Module Browser: compiling definitions")
  (define show-lib-paths "Follow lib requires")
  (define refresh "Refresh")
  
  (define unit@
    (unit/sig drscheme:unit^
      (import [help-desk : drscheme:help-desk^]
              [drscheme:app : drscheme:app^]
              [drscheme:frame : drscheme:frame^]
              [drscheme:text : drscheme:text^]
              [drscheme:rep : drscheme:rep^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^]
              [drscheme:language : drscheme:language^]
              [drscheme:get/extend : drscheme:get/extend^]
              [drscheme:number-snip : drscheme:number-snip^]
              [drscheme:teachpack : drscheme:teachpack^]
              [drscheme:module-overview : drscheme:module-overview^]
              [drscheme:tools : drscheme:tools^]
              [drscheme:eval : drscheme:eval^]
              [drscheme:init : drscheme:init^]
              [drscheme:module-language : drscheme:module-language^])
      
      (rename [-frame% frame%]
              [-frame<%> frame<%>])

      (keymap:add-to-right-button-menu
       (let ([old (keymap:add-to-right-button-menu)])
         (lambda (menu text event)
           (old menu text event)
           (when (and (is-a? text text%)
                      (or (is-a? text (get-definitions-text%))
                          (is-a? text drscheme:rep:text%))
                      (is-a? event mouse-event%))
             (let* ([end (send text get-end-position)]
                    [start (send text get-start-position)])
               (unless (= 0 (send text last-position))
                 (let ([str (if (= end start)
                                (find-symbol
                                 text
                                 (call-with-values
                                  (lambda ()
                                    (send text dc-location-to-editor-location
                                          (send event get-x)
                                          (send event get-y)))
                                  (lambda (x y)
                                    (send text find-position x y))))
                                (send text get-text start end))])
                   (unless (string=? str "")
                     (make-object separator-menu-item% menu)
                     (make-object menu-item%
                       (format (string-constant search-help-desk-for) 
                               (shorten-str 
                                str 
                                (- 200 (string-length (string-constant search-help-desk-for)))))
                       menu
                       (lambda x (help-desk:help-desk str #f 'keyword+index 'contains)))
                     (make-object menu-item%
                       (format (string-constant exact-lucky-search-help-desk-for) 
                               (shorten-str 
                                str 
                                (- 200 (string-length (string-constant exact-lucky-search-help-desk-for)))))
                       menu
                       (lambda x (help-desk:help-desk str #t 'keyword+index 'exact)))
		     (void)))))))))
      
      ;; find-symbol : number -> string
      ;; finds the symbol around the position `pos' (approx)
      (define (find-symbol text pos)
        (let* ([before
                (let loop ([i (- pos 1)]
                           [chars null])
                  (if (< i 0)
                      chars
                      (let ([char (send text get-character i)])
                        (if (non-letter? char)
                            chars
                            (loop (- i 1)
                                  (cons char chars))))))]
               [after
                (let loop ([i pos])
                  (if (< i (send text last-position))
                      (let ([char (send text get-character i)])
                        (if (non-letter? char)
                            null
                            (cons char (loop (+ i 1)))))
                      null))])
          (apply string (append before after))))
      
      ;; non-letter? : char -> boolean
      ;; returns #t if the character belongs in a symbol (approx) and #f it is
      ;; a divider between symbols (approx)
      (define (non-letter? x)
        (or (char-whitespace? x)
            (memq x '(#\` #\' #\, #\; #\"
                       #\{ #\( #\[ #\] #\) #\}))))      
      (define (shorten-str str len)
        (if ((string-length str) . <= . len)
            str
            (substring str 0 len)))
      
      
      
       ;;;                                ;                            ;;    ;           ;;;                 
      ;                           ;                                     ;                  ;                 
      ;                           ;                                     ;                  ;                 
     ;;;;;  ; ;;;  ;;;;    ;;;   ;;;;;  ;;;     ;;;  ; ;;;           ;;;;  ;;;    ;;;;     ;     ;;;    ;;; ;
      ;      ;         ;  ;   ;   ;       ;    ;   ;  ;;  ;         ;   ;    ;        ;    ;    ;   ;  ;   ; 
      ;      ;      ;;;;  ;       ;       ;    ;   ;  ;   ;         ;   ;    ;     ;;;;    ;    ;   ;  ;   ; 
      ;      ;     ;   ;  ;       ;       ;    ;   ;  ;   ;         ;   ;    ;    ;   ;    ;    ;   ;  ;   ; 
      ;      ;     ;   ;  ;   ;   ;   ;   ;    ;   ;  ;   ;         ;   ;    ;    ;   ;    ;    ;   ;  ;   ; 
     ;;;;   ;;;;    ;;; ;  ;;;     ;;;  ;;;;;   ;;;  ;;;  ;;         ;;; ; ;;;;;   ;;; ; ;;;;;;  ;;;    ;;;; 
                                                                                                           ; 
                                                                                                           ; 
                                                                                                         ;;;  
      
      
      (define (get-fraction-from-user parent)
        (let* ([dlg (make-object dialog% (string-constant enter-fraction))]
               [hp (make-object horizontal-panel% dlg)]
               [_1 (make-object message% (string-constant whole-part) hp)]
               [whole (make-object text-field% #f hp void)]
               [vp (make-object vertical-panel% hp)]
               [hp2 (make-object horizontal-panel% vp)]
               [num (make-object text-field% #f hp2 void)]
               [num-m (make-object message% (string-constant numerator) hp2)]
               [hp3 (make-object horizontal-panel% vp)]
               [den (make-object text-field% #f hp3 void)]
               [den-m (make-object message% (string-constant denominator) hp3)]
               [bp (make-object horizontal-panel% dlg)]
               [ok? #f]
               [validate-number
                (lambda ()
                  (let ([num-s (string->number (send num get-value))]
                        [den-s (string->number (send den get-value))]
                        [whole-s (if (string=? (send whole get-value) "")
                                     0
                                     (string->number (send whole get-value)))])
                    (if (and num-s den-s whole-s)
                        (let ([ans (+ whole-s (/ num-s den-s))])
                          (if (and (exact? ans)
                                   (real? ans)
                                   (not (integer? ans)))
                              ans
                              #f))
                        #f)))]
               [ok-callback
                (lambda () 
                  (cond
                    [(validate-number)
                     (set! ok? #t)
                     (send dlg show #f)]
                    [else 
                     (message-box
                      (string-constant drscheme)
                      (string-constant invalid-number)
                      dlg)]))]
               [cancel-callback 
                (lambda () (send dlg show #f))])
          (let-values ([(ok cancel) 
                        (gui-utils:ok/cancel-buttons
                         bp
                         (lambda (x y) (ok-callback))
                         (lambda (x y) (cancel-callback)))])
            (let ([mw (max (send den-m get-width) (send num-m get-width))])
              (send den-m min-width mw)
              (send num-m min-width mw))
            (send bp set-alignment 'right 'center)
            (send dlg show #t)
            (and ok? (validate-number)))))
      
      (define (basename fn)
        (if fn
            (let* ([file-name (mzlib:file:file-name-from-path fn)]
                   [ext (mzlib:file:filename-extension file-name)])
              (if ext
                  (substring file-name 0 (- (string-length file-name)
                                            (string-length ext)
                                            1))
                  file-name))
            #f))
      
      ;; create-executable : (instanceof drscheme:unit:frame<%>) -> void
      (define (create-executable frame)
        (let* ([definitions-text (send frame get-definitions-text)]
               [program-filename (send definitions-text get-filename)])
          (cond
            [(not program-filename)
             (message-box (string-constant create-executable-title)
                          (string-constant must-save-before-executable)
                          frame)]
            [else
             (when (or (not (send definitions-text is-modified?))
                       (gui-utils:get-choice
                        (string-constant definitions-not-saved)
                        (string-constant yes)
                        (string-constant no)
                        (string-constant drscheme)
                        #f
                        frame))
               (let ([settings (send (send frame get-definitions-text) get-next-settings)])
                 (send (drscheme:language-configuration:language-settings-language settings)
                       create-executable
                       (drscheme:language-configuration:language-settings-settings settings)
                       frame
                       program-filename)))])))
      
      (define make-bitmap 
        (case-lambda 
          [(button-name) (make-bitmap 
                          (let ([capd (string-copy button-name)])
                            (string-set! capd 0 (char-upcase (string-ref capd 0)))
                            capd)
                          (build-path
                           (collection-path "icons")
                           (string-append button-name ".bmp")))]
          [(text filename)
           (lambda (area-container-window)
             (let*-values ([(outside-margin) 2]
                           [(middle-margin) 3]
                           [(font) (send area-container-window get-control-font)]
                           [(img-bitmap-dc img-width img-height)
                            (let ([mdc (make-object bitmap-dc%)]
                                  [q (make-object bitmap% filename)])
                              (if (send q ok?)
                                  (begin (send mdc set-bitmap q)
                                         (values mdc
                                                 (send q get-width)
                                                 (send q get-height)))
                                  (let ([b (make-object bitmap% 1 1)])
                                    (send mdc set-bitmap b)
                                    (send mdc clear)
                                    (values mdc 0 0))))]
                           [(width height descent leading)
                            (begin (send img-bitmap-dc set-scale 1 1)
                                   (send img-bitmap-dc get-text-extent text font))]
                           [(new-width) (inexact->exact
                                         (floor
                                          (+ outside-margin
                                             img-width
                                             middle-margin
                                             width
                                             outside-margin)))]
                           [(new-height) (inexact->exact
                                          (floor (+ outside-margin
                                                    (max img-height height)
                                                    outside-margin)))]
                           [(bitmap-dc) (make-object bitmap-dc%)]
                           [(new-bitmap) (make-object bitmap% new-width new-height)])
               (cond
                 [(or (= img-width 0)
                      (= img-height 0))
                  text]
                 [else
                  (send* bitmap-dc
                    (set-bitmap new-bitmap)
                    (set-scale 1 1)
                    (set-font font)
                    (clear)
                    (draw-text text (+ outside-margin img-width middle-margin)
                               (- (/ new-height 2) (/ height 2))))
                  (let ([bm (send img-bitmap-dc get-bitmap)])
                    (send img-bitmap-dc set-bitmap #f)
                    (send bitmap-dc draw-bitmap
                          bm
                          outside-margin
                          (- (/ new-height 2) (/ img-height 2)))
                    (send bitmap-dc set-bitmap #f)
                    new-bitmap)])))]))
      
      (define make-execute-bitmap 
        (make-bitmap (string-constant execute-button-label) 
                     (build-path (collection-path "icons") "execute.bmp")))
      (define make-save-bitmap 
        (make-bitmap (string-constant save-button-label) 
                     (build-path (collection-path "icons") "save.bmp")))
      (define make-break-bitmap 
        (make-bitmap (string-constant break-button-label) 
                     (build-path (collection-path "icons") "break.bmp")))
      
      (define-values (get-program-editor-mixin add-to-program-editor-mixin)
        (let* ([program-editor-mixin
                (mixin (editor:basic<%> (class->interface text%)) () 
                  (init-rest args) 
                  (override after-insert after-delete) 
                  (inherit get-top-level-window) 
                  (rename [super-after-insert after-insert] 
                          [super-after-delete after-delete]) 
                  
                  (define (reset-highlighting) 
                    (let ([f (get-top-level-window)]) 
                      (when (and f 
                                 (is-a? f -frame%)) 
                        (let ([interactions-text (send f get-interactions-text)]) 
                          (when (object? interactions-text) 
                            (send interactions-text reset-highlighting)))))) 
                  
                  (define (after-insert x y) 
                    (reset-highlighting) 
                    (super-after-insert x y)) 
                  
                  (define (after-delete x y) 
                    (reset-highlighting) 
                    (super-after-delete x y)) 
                  
                  (apply super-make-object args))]
               [get-program-editor-mixin
                (lambda ()
                  (drscheme:tools:only-in-phase 'drscheme:unit:get-program-editor-mixin 'phase2 'init-complete)
                  program-editor-mixin)]
               [add-to-program-editor-mixin
                (lambda (mixin)
                  (drscheme:tools:only-in-phase 'drscheme:unit:add-to-program-editor-mixin 'phase1)
                  (set! program-editor-mixin (compose mixin program-editor-mixin)))])
          (values get-program-editor-mixin
                  add-to-program-editor-mixin)))
      
      
      ;; this sends a message to it's frame when it gets the focus
      (define make-searchable-canvas%
        (lambda (%)
          (class %
            (inherit get-top-level-window)
            (rename [super-on-focus on-focus])
            (define/override (on-focus on?)
              (when on?
                (send (get-top-level-window) make-searchable this))
              (super-on-focus on?))
            (super-instantiate ()))))
      
      (define interactions-canvas% (make-searchable-canvas%
                                    (canvas:info-mixin
                                     canvas:wide-snip%)))
      
      (define definitions-canvas%
        (class (make-searchable-canvas% (canvas:delegate-mixin canvas:info%))
          (super-instantiate ())))
      
      (define definitions-text<%> (interface ()))
      
      (define get-definitions-text%
        (let ([definitions-text% #f])
          (lambda ()
            (drscheme:tools:only-in-phase 'phase2 'init-complete)
            (unless definitions-text%
              (set! definitions-text% (make-definitions-text%)))
            definitions-text%)))
      
      (define (make-definitions-text%)
        (let ([definitions-super%
               ((get-program-editor-mixin)
                (drscheme:module-language:module-language-put-file-mixin
                 (scheme:text-mixin
                  (drscheme:rep:drs-bindings-keymap-mixin
                   (text:delegate-mixin
                    text:info%)))))])
          (class* definitions-super% (definitions-text<%>)
            (inherit get-top-level-window)
            
            (rename [super-after-save-file after-save-file])
            (define/override (after-save-file success?)
              (when success?
                (let ([filename (get-filename)])
                  (when filename
                    ;; if a filesystem error happens, just give up
                    ;; on setting the file creator and type.
                    (with-handlers ([exn:i/o:filesystem?
                                     void])
                      (let-values ([(creator type) (file-creator-and-type filename)])
                        (file-creator-and-type filename "DrSc" type))))))
              (super-after-save-file success?))
              
            (rename [super-set-modified set-modified]
                    [super-set-filename set-filename])
            (inherit is-modified? run-after-edit-sequence)
            (define/override (set-modified mod?)
              (super-set-modified mod?)
              (run-after-edit-sequence
               (lambda ()
                 (let ([f (get-top-level-window)])
                   (when f
                     (send f update-save-button (is-modified?)))))))
            (define/override set-filename
              (case-lambda
                [(fn) (set-filename fn #f)]
                [(fn tmp?)
                 (super-set-filename fn tmp?)
                 (let ([f (get-top-level-window)])
                   (when f
                     (send f update-save-message fn)))]))
            
            (rename [super-after-insert after-insert]
                    [super-after-delete after-delete])
            (field
             [needs-execution-state #f]
             [already-warned-state #f]
             [execute-settings (preferences:get drscheme:language-configuration:settings-preferences-symbol)]
             [next-settings execute-settings])
            
            (define/public (get-next-settings) next-settings)
            (define/public (set-next-settings _next-settings) (set! next-settings _next-settings))
            
            (define/public (needs-execution?)
              (or needs-execution-state
                  (not (equal? execute-settings next-settings))))
            
            (define/public (teachpack-changed)
              (set! needs-execution-state #t))
            (define/public (just-executed)
              (set! execute-settings next-settings)
              (set! needs-execution-state #f)
              (set! already-warned-state #f))
            (define/public (already-warned?)
              already-warned-state)
            (define/public (already-warned)
              (set! already-warned-state #t))
            (define/override (after-insert x y)
              (set! needs-execution-state #t)
              (super-after-insert x y))
            (define/override (after-delete x y)
              (set! needs-execution-state #t)
              (super-after-delete x y))
            
            (inherit get-filename)
            (field
             [tmp-date-string #f])
            
            (define (get-date-string)
              (string-append
               (mzlib:date:date->string (seconds->date (current-seconds)))
               " "
               (let ([fn (get-filename)])
                 (if (string? fn)
                     fn
                     (string-constant untitled)))))
            
            (rename [super-on-paint on-paint])
            (define/override (on-paint before dc left top right bottom dx dy draw-caret)
              (when (and before
                         (or (is-a? dc post-script-dc%)
                             (is-a? dc printer-dc%)))
                (set! tmp-date-string (get-date-string))
                (let-values ([(w h d s) (send dc get-text-extent tmp-date-string)])
                  (send (current-ps-setup) set-editor-margin 0 (inexact->exact (ceiling h)))))
              (super-on-paint before dc left top right bottom dx dy draw-caret)
              (when (and (not before)
                         (or (is-a? dc post-script-dc%)
                             (is-a? dc printer-dc%)))
                (send dc draw-text (get-date-string) 0 0)
                (void)))
            (super-instantiate ()))))
      
      
                                                       
   ;      ;           ;; ;                             
  ;       ;          ;                                 
  ;       ;          ;                                 
 ;    ;;; ;   ;;;   ;;;  ;   ; ;;;    ;;;              
 ;   ;   ;;  ;   ;   ;   ;   ;;   ;  ;   ;             
 ;   ;    ;  ;   ;   ;   ;   ;    ;  ;   ;             
 ;   ;    ;  ;;;;;   ;   ;   ;    ;  ;;;;;             
 ;   ;    ;  ;       ;   ;   ;    ;  ;                 
 ;   ;   ;;  ;       ;   ;   ;    ;  ;                 
  ;   ;;; ;   ;;;;   ;   ;   ;    ;   ;;;;  ;   ;   ;  
  ;                                                    
   ;                                                   
                                                       

      
      ;; get-pos : text mouse-event% -> (union #f number)
      (define (get-pos text event)
        (let*-values ([(event-x event-y)
                       (values (send event get-x)
                               (send event get-y))]
                      [(x y) (send text dc-location-to-editor-location
                                   event-x 
                                   event-y)])
          (let* ([on-it? (box #f)]
                 [pos (send text find-position x y #f on-it?)])
            (and (unbox on-it?)
                 pos))))                                                 
      
      (let ([old (keymap:add-to-right-button-menu)])
        (keymap:add-to-right-button-menu
         (lambda (menu editor event)
           (when (is-a? editor text%)
             (let* ([current-pos (get-pos editor event)]
                    [current-word (and current-pos (get-current-word editor current-pos))]
                    [defn (and current-word
                               (ormap (lambda (defn) (and (string=? current-word (defn-name defn))
                                                          defn))
                                      (get-definitions #f editor)))])
               (when defn
                 (instantiate separator-menu-item% () (parent menu))
                 (instantiate menu-item% () 
                   (parent menu)
                   (label (format (string-constant jump-to-defn) (defn-name defn)))
                   (callback (lambda (x y)
                               (send editor set-position (defn-start-pos defn))))))))
           (old menu editor event))))

      ;; get-current-word : editor number -> string
      ;; returns the string that is being clicked on
      (define (get-current-word editor pos)
        (let* ([search
                (lambda (dir offset)
                  (let loop ([pos pos])
                    (cond
                      [(or (= pos 0) 
                           (= pos (send editor last-position)))
                       pos]
                      [(memq (send editor get-character pos) '(#\space #\return #\newline #\( #\) #\[ #\] #\tab))
                       (offset pos)]
                      [else (loop (dir pos))])))]
               [before (search sub1 add1)]
               [after (search add1 (lambda (x) x))])
          (send editor get-text before after)))
        
      (define func-defs-canvas%
        (class canvas%
	  (init parent)
          (init-field frame)
	  (init-field fallback-text)
          (override on-paint on-event)
	  (inherit get-client-size get-dc popup-menu min-height min-width
		   stretchable-width
		   stretchable-height)
	  (rename [super-on-event on-event])
                    
          (define inverted? #f)
          
          (define label "(define ...)")
          
          (define (on-paint)
            (let ([dc (get-dc)])
              (let-values ([(w h) (get-client-size)])
                (draw-button-label dc label w h inverted?))))
          
          (define sort-by-name? #f)
          (define sorting-name (string-constant sort-by-name))
          (define (change-sorting-order)
            (set! sort-by-name? (not sort-by-name?))
            (set! sorting-name (if sort-by-name?
                                   (string-constant sort-by-position) 
                                   (string-constant sort-by-name))))
          
          (define (on-event evt)
            (cond
              [(send evt button-down?)
               (set! inverted? #t)
               (on-paint)
               (let* ([text (let ([active-text (send frame get-edit-target-object)])
                              (if (and (object? active-text)
                                       (is-a? active-text definitions-text<%>))
                                  active-text
                                  fallback-text))]
                      [menu (make-object popup-menu% #f
                              (lambda x
                                (set! inverted? #f)
                                (on-paint)))]
                      [unsorted-defns (get-definitions (not sort-by-name?) text)]
                      [defns (if sort-by-name?
                                 (quicksort 
                                  unsorted-defns
                                  (lambda (x y) (string-ci<=? (defn-name x) (defn-name y))))
                                 unsorted-defns)])
                 (make-object menu:can-restore-menu-item% sorting-name
                   menu
                   (lambda x
                     (change-sorting-order)))
                 (make-object separator-menu-item% menu)
                 (if (null? defns)
                     (send (make-object menu:can-restore-menu-item%
                             (string-constant no-definitions-found)
                             menu
                             void)
                           enable #f)
                     (let loop ([defns defns])
                       (unless (null? defns)
                         (let* ([defn (car defns)]
                                [checked? 
                                 (let ([t-start (send text get-start-position)]
                                       [t-end (send text get-end-position)]
                                       [d-start (defn-start-pos defn)]
                                       [d-end (defn-end-pos defn)])
                                   (or (<= t-start d-start t-end)
                                       (<= t-start d-end t-end)
                                       (<= d-start t-start t-end d-end)))]
                                [item
                                 (make-object (if checked?
                                                  menu:can-restore-checkable-menu-item%
                                                  menu:can-restore-menu-item%)
                                   (defn-name defn)
                                   menu
                                   (lambda x
                                     (set! inverted? #f)
                                     (on-paint)
                                     (send text set-position (defn-start-pos defn) (defn-start-pos defn))
                                     (let ([canvas (send text get-canvas)])
                                       (when canvas
                                         (send canvas focus)))))])
                           (when checked?
                             (send item check #t))
                           (loop (cdr defns))))))
                 (popup-menu menu
                             0
                             height))]
              [else (super-on-event evt)]))
          
          (super-make-object parent)
          
          (define-values (width height) (calc-button-min-sizes (get-dc) label))
          (min-width width)
          (min-height height)
          (stretchable-width #f)
          (stretchable-height #f)))

      ;; defn = (make-defn number string number number)
      (define-struct defn (indent name start-pos end-pos))
      (define tag-string "(define")

      ;; get-definitions : boolean text -> (listof defn)
      (define (get-definitions indent? text)
        (let* ([min-indent 0]
               [defs (let loop ([pos 0])
                       (let ([defn-pos (send text find-string tag-string 'forward pos 'eof #t #f)])
                         (cond
                           [(not defn-pos) null]
                           [(in-semicolon-comment? text defn-pos)
                            (loop (+ defn-pos (string-length tag-string)))]
                           [else
                            (let ([indent (get-defn-indent text defn-pos)]
                                  [name (get-defn-name text (+ defn-pos (string-length tag-string)))])
                              (set! min-indent (min indent min-indent))
                              (cons (make-defn indent name defn-pos defn-pos)
                                    (loop (+ defn-pos (string-length tag-string)))))])))])
          
          ;; update end-pos's based on the start pos of the next defn
          (unless (null? defs)
            (let loop ([first (car defs)]
                       [defs (cdr defs)])
              (cond
                [(null? defs) 
                 (set-defn-end-pos! first (send text last-position))]
                [else (set-defn-end-pos! first (max (- (defn-start-pos (car defs)) 1)
                                                    (defn-start-pos first)))
                      (loop (car defs) (cdr defs))])))
          
          (when indent?
            (for-each (lambda (defn)
                        (set-defn-name! defn
                                        (string-append
                                         (apply string
                                                (vector->list
                                                 (make-vector 
                                                  (- (defn-indent defn) min-indent) #\space)))
                                         (defn-name defn))))
                      defs))
          defs))

      ;; in-semicolon-comment: text number -> boolean
      ;; returns #t if `define-start-pos' is in a semicolon comment and #f otherwise
      (define (in-semicolon-comment? text define-start-pos)
        (let* ([para (send text position-paragraph define-start-pos)]
               [start (send text paragraph-start-position para)])
          (let loop ([pos start])
            (cond
              [(pos . >= . define-start-pos) #f]
              [(char=? #\; (send text get-character pos)) #t]
              [else (loop (+ pos 1))]))))
      
      ;; get-defn-indent : text number -> number
      ;; returns the amount to indent a particular definition
      (define (get-defn-indent text pos)
        (let* ([para (send text position-paragraph pos)]
               [para-start (send text paragraph-start-position para #t)])
          (let loop ([c-pos para-start]
                     [offset 0])
            (if (< c-pos pos)
                (let ([char (send text get-character c-pos)])
                  (cond
                    [(char=? char #\tab)
                     (loop (+ c-pos 1) (+ offset (- 8 (modulo offset 8))))]
                    [else
                     (loop (+ c-pos 1) (+ offset 1))]))
                offset))))
      
      ;; skip-to-whitespace/paren : text number -> number
      ;; skips to the next parenthesis or whitespace after `pos', returns that position.
      (define (skip-to-whitespace/paren text pos)
        (let loop ([pos pos])
          (if (>= pos (send text last-position))
              (send text last-position)
              (let ([char (send text get-character pos)])
                (cond
                  [(or (char=? #\) char)
                       (char=? #\( char)
                       (char=? #\] char)
                       (char=? #\[ char)
                       (char-whitespace? char))
                   pos]
                  [else (loop (+ pos 1))])))))
      
      ;; skip-whitespace/paren : text number -> number
      ;; skips past any parenthesis or whitespace
      (define (skip-whitespace/paren text pos)
        (let loop ([pos pos])
          (if (>= pos (send text last-position))
              (send text last-position)
              (let ([char (send text get-character pos)])
                (cond
                  [(or (char=? #\) char)
                       (char=? #\( char)
                       (char=? #\] char)
                       (char=? #\[ char)
                       (char-whitespace? char))
                   (loop (+ pos 1))]
                  [else pos])))))
      
      ;; get-defn-name : text number -> string
      ;; returns the name of the definition starting at `define-pos'
      (define (get-defn-name text define-pos)
        (if (>= define-pos (send text last-position))
            (string-constant end-of-buffer-define)
            (let* ([start-pos (skip-whitespace/paren text (skip-to-whitespace/paren text define-pos))]
                   [end-pos (skip-to-whitespace/paren text start-pos)])
              (send text get-text start-pos end-pos))))
      
      (define (set-box/f! b v) (when (box? b) (set-box! b v)))
      

                                    
                                    
                                    
   ;;                               
  ;                                 
  ;                                 
 ;;;  ; ;;  ;;;   ; ;;; ;;     ;;;  
  ;   ;;   ;   ;  ;;  ;;  ;   ;   ; 
  ;   ;        ;  ;   ;   ;   ;   ; 
  ;   ;     ;;;;  ;   ;   ;   ;;;;; 
  ;   ;    ;   ;  ;   ;   ;   ;     
  ;   ;    ;   ;  ;   ;   ;   ;     
  ;   ;     ;;;;; ;   ;   ;    ;;;; 
                                    
                                    
                                    

      
      (define vertical-dragable/def-int%
        (class panel:vertical-dragable%
          (init-field unit-frame)
          (inherit get-percentages)
          (define/override (after-percentage-change)
            (let ([percentages (get-percentages)])
              (when (and (= 1
                            (length (send unit-frame get-definitions-canvases))
                            (length (send unit-frame get-interactions-canvases)))
                         (= 2 (length percentages)))
                (preferences:set 'drscheme:unit-window-size-percentage (car percentages)))))
          (super-instantiate ())))
      
      (define super-frame%
        (drscheme:frame:mixin
         (drscheme:frame:basics-mixin 
          (frame:searchable-text-mixin 
           (frame:searchable-mixin
            (frame:file-mixin
             (frame:text-info-mixin 
              (frame:delegate-mixin
               (frame:status-line-mixin
                (frame:info-mixin
                 (frame:text-mixin
                  (frame:open-here-mixin
                   (frame:editor-mixin
                    (frame:standard-menus-mixin
                     frame:basic%))))))))))))))
      
      (define -frame<%>
        (interface ()
          clear-annotations
          get-special-menu
          get-interactions-text
          get-definitions-text
          get-interactions-canvas
          get-definitions-canvas
          execute-callback))

      (define -frame%
        (class* super-frame% (drscheme:rep:context<%> -frame<%>)
          (init filename)
          (inherit set-label-prefix get-show-menu
                   show get-menu%
                   get-area-container
                   update-info
                   get-file-menu
                   file-menu:get-open-item
                   file-menu:get-new-item
                   file-menu:get-save-item
                   file-menu:get-save-as-item
                   file-menu:get-revert-item
                   file-menu:get-print-item)

          ;; logging : (union #f string[directory-name])
          (field [logging #f]
                 [definitions-log-counter 0]  ;; number
                 [interactions-log-counter 0] ;; number
                 [logging-parent-panel #f]    ;; panel (unitialized short time only)
                 [logging-panel #f]           ;; panel (unitialized short time only)
                 [logging-menu-item #f])      ;; menu-item (unitialized short time only)
          ;; log-definitions : -> void
          (define/private (log-definitions)
            (when logging
              (set! definitions-log-counter (+ definitions-log-counter 1))
              (send definitions-text save-file 
                    (build-path logging (format "~a-definitions" (pad-two definitions-log-counter)))
                    'copy)))
          
          ;; log-ineractions : -> void
          (define/private (log-interactions)
            (when logging
              (set! interactions-log-counter (+ interactions-log-counter 1))
              (send interactions-text save-file 
                    (build-path logging (format "~a-interactions" (pad-two interactions-log-counter)))
                    'copy)))
          
          ;; pad-two : number -> string
          ;; pads a number to two digitsﬂ
          (define (pad-two n)
            (cond
              [(<= 0 n 9) (format "0~a" n)]
              [else (format "~a" n)]))
          
          ;; start-logging : -> void
          ;; turns on the logging and shows the logging gui
          (define (start-logging)
            (let ([log-directory (mred:get-directory
                                  (string-constant please-choose-a-log-directory)
                                  this)])
              (when (and log-directory
                         (ensure-empty log-directory))
                (send logging-menu-item set-label (string-constant stop-logging))
                (set! logging log-directory)
                (set! definitions-log-counter 0)
                (set! interactions-log-counter 0)
                (build-logging-panel)
                (log-definitions))))
          
          ;; stop-logging : -> void
          ;; turns off the logging procedure
          (define (stop-logging)
            (log-interactions)
            (send logging-menu-item set-label (string-constant log-definitions-and-interactions))
            (set! logging #f)
            (send logging-panel change-children (lambda (l) null)))
          
          ;; build-logging-panel : -> void
          ;; builds the contents of the logging panel
          (define (build-logging-panel)
            (define hp (make-object horizontal-panel% logging-panel '(border)))
            (make-object message% (string-constant logging-to) hp)
            (send (make-object message% logging hp) stretchable-width #t)
            (make-object button% (string-constant stop-logging) hp (lambda (x y) (stop-logging))))
          
          ;; remove-logging-pref-callback : -> void
          ;; removes the callback that shows and hides the logging panel
          (field [remove-logging-pref-callback
                  (preferences:add-callback
                   'framework:show-status-line
                   (lambda (p v)
                     (when (is-a? logging-parent-panel panel%)
                       (send logging-parent-panel change-children
                             (lambda (l)
                               (if v 
                                   (list logging-panel)
                                   null))))))])
          
          ;; ensure-empty : string[directory] -> boolean
          ;; if the log-directory is empty, just return #t
          ;; if not, ask the user about emptying it. 
          ;;   if they say yes, try to empty it.
          ;;     if that fails, report the error and return #f.
          ;;     if it succeeds, return #t.
          ;;   if they say no, return #f.
          (define (ensure-empty log-directory)
            (let ([dir-list (directory-list log-directory)])
              (or (null? dir-list)
                  (let ([query (message-box 
                                (string-constant drscheme)
                                (format (string-constant erase-log-directory-contents) log-directory)
                                this
                                '(yes-no))])
                    (cond
                      [(eq? query 'no) 
                       #f]
                      [(eq? query 'yes)
                       (with-handlers ([not-break-exn?
                                        (lambda (exn)
                                          (message-box 
                                           (string-constant drscheme)
                                           (format (string-constant error-erasing-log-directory)
                                                   (if (exn? exn)
                                                       (format "~a" (exn-message exn))
                                                       (format "~s" exn)))
                                           this)
                                          #f)])
                         (for-each (lambda (file) (delete-file (build-path log-directory file)))
                                   dir-list)
                         #t)])))))

          (rename [super-make-root-area-container make-root-area-container])
          (define/override (make-root-area-container cls parent)
            (let* ([outer-panel (super-make-root-area-container module-browser-dragable-panel% parent)]
                   [saved-p (preferences:get 'drscheme:module-browser-size-percentage)]
                   [_module-browser-panel (instantiate vertical-panel% ()
                                            (parent outer-panel)
                                            (stretchable-width #f))]
                   [louter-panel (make-object vertical-panel% outer-panel)]
                   [root (make-object cls louter-panel)])
              (set! module-browser-panel _module-browser-panel)
              (set! module-browser-parent-panel outer-panel)
              (send outer-panel change-children (lambda (l) (remq module-browser-panel l)))
              (preferences:set 'drscheme:module-browser-size-percentage saved-p)
              (set! logging-parent-panel (instantiate horizontal-panel% ()
                                           (parent louter-panel)
                                           (stretchable-height #f)))
              (set! logging-panel (make-object horizontal-panel% logging-parent-panel))
              (unless (preferences:get 'framework:show-status-line)
                (send logging-parent-panel change-children (lambda (l) null)))
              root))

          (inherit get-currently-running?)
          (rename [super-can-close? can-close?])
          (define/override (can-close?)
            (and (cond
                   [(get-currently-running?)
                    (equal? (message-box/custom
                             (string-constant drscheme)
                             (string-constant program-is-still-running)
                             (string-constant yes)
                             (string-constant no)
                             #f
                             this
                             '(default=1 caution)
                             2)
                            1)]
                   [(let ([user-eventspace (send interactions-text get-user-eventspace)])
                      (and user-eventspace
                           (parameterize ([current-eventspace user-eventspace])
                             (not (null? (get-top-level-windows))))))
                    (equal? (message-box/custom
                             (string-constant drscheme)
                             (string-constant program-has-open-windows)
                             (string-constant yes)
                             (string-constant no)
                             #f
                             this
                             '(default=1 caution)
                             2)
                            1)]
                   [else #t])
                 (super-can-close?)))
          
          (public clear-annotations)
          [define clear-annotations
            (lambda ()
              (send interactions-text reset-highlighting))]
          
          (rename [super-update-shown update-shown]
                  [super-on-close on-close])
          (public get-directory needs-execution?)
          [define get-directory
            (lambda ()
              (let ([filename (send definitions-text get-filename)])
                (if (and (string? filename)
                         (not (string=? "" filename)))
                    (let-values ([(base _1 _2) (split-path (mzlib:file:normalize-path filename))])
                      base)
                    #f)))]
          [define needs-execution?
            (lambda ()
              (send definitions-text needs-execution?))]
          
          [define definitions-item #f]
          [define interactions-item #f]
          [define name-message #f]
          [define save-button #f]
          [define save-init-shown? #f]
          
          [define set-save-init-shown? (lambda (x) (set! save-init-shown? x))]
          
          [define canvas-show-mode #f]
          [define allow-split? #f]
          [define forced-quit? #f]
          [define search-canvas #f]
          
          (public make-searchable)
          [define make-searchable
            (lambda (canvas)
              (update-info)
              (set! search-canvas canvas))]
          (override get-text-to-search)
          [define get-text-to-search
            (lambda ()
              (if search-canvas
                  (send search-canvas get-editor)
                  (get-editor)))]
          
          [define was-locked? #f]
          [define execute-menu-item #f]
          
          [define/public disable-evaluation
            (lambda ()
              (when execute-menu-item
                (send execute-menu-item enable #f))
              (send execute-button enable #f)
              (send definitions-text lock #t)
              (send interactions-text lock #t))]
          [define/public enable-evaluation
            (lambda ()
              (when execute-menu-item
                (send execute-menu-item enable #t))
              (send execute-button enable #t)
              (send definitions-text lock #f)
              (unless (send interactions-text eval-busy?)
                (send interactions-text lock #f)))]
          
          (inherit set-label)
          (public update-save-button update-save-message)
          [define update-save-button
            (lambda (mod?)
              (if save-button
                  (unless (eq? mod? (send save-button is-shown?))
                    (send save-button show mod?))
                  (set! save-init-shown? mod?)))]

          ;; update-save-message : (union #f string) -> void
          ;; sets the save message. If input is #f, uses the frame's
          ;; title.
          [define update-save-message
            (lambda (name)
              (when name-message
                (send name-message set-message 
                      #t
                      (or name (get-label)))))]

          (override get-canvas%)
          [define get-canvas% (lambda () (drscheme:get/extend:get-definitions-canvas))]
          
          (define/public (ensure-defs-shown)
            (unless definitions-shown?
              (toggle-show/hide-definitions)
              (update-shown)))
          (define/public (ensure-rep-shown)
            (unless interactions-shown?
              (toggle-show/hide-interactions)
              (update-shown)))
          (define/public (ensure-rep-hidden)
            (when interactions-shown?
              (toggle-show/hide-interactions)
              (update-shown)))
          
          (override get-editor%)
          [define get-editor% (lambda () (drscheme:get/extend:get-definitions-text))]
          (define/public (still-untouched?)
            (and (= (send definitions-text last-position) 0)
                 (not (send definitions-text is-modified?))
                 (not (send definitions-text get-filename))
                 (let* ([prompt (send interactions-text get-prompt)]
                        [first-prompt-para
                         (let loop ([n 0])
                           (cond
                             [(n . <= . (send interactions-text last-paragraph))
                              (if (string=?
                                   (send interactions-text get-text 
                                         (send interactions-text paragraph-start-position n)
                                         (+ (send interactions-text paragraph-start-position n)
                                            (string-length prompt)))
                                   prompt)
                                  n
                                  (loop (+ n 1)))]
                             [else #f]))])
                   
                   (and first-prompt-para
                        (= first-prompt-para (send interactions-text last-paragraph))
                        (equal? 
                         (send interactions-text get-text
                               (send interactions-text paragraph-start-position first-prompt-para)
                               (send interactions-text paragraph-end-position first-prompt-para))
                         (send interactions-text get-prompt))))))
          (define/public (change-to-file name)
            (cond
              [(and name (file-exists? name))
               (ensure-rep-hidden)
               (send definitions-text load-file/gui-error name)]
              [name
               (send definitions-text set-filename name)]
              [else (send definitions-text clear)])
            (send definitions-canvas focus))
          
          [define (toggle-show/hide-definitions)
            (set! definitions-shown? (not definitions-shown?))
            (unless definitions-shown?
              (set! interactions-shown? #t))]
          [define (toggle-show/hide-interactions)
            (set! interactions-shown? (not interactions-shown?))
            (unless  interactions-shown?
              (set! definitions-shown? #t))]
          
          [define file-menu:print-transcript-item #f]
          
          (rename
           [super-file-menu:between-open-and-revert file-menu:between-open-and-revert])
          (override file-menu:between-open-and-revert
                    file-menu:save-string
                    file-menu:save-as-string
                    file-menu:between-save-as-and-print
                    file-menu:print-string
                    file-menu:between-print-and-close)
          [define file-menu:between-open-and-revert
            (lambda (file-menu)
              (super-file-menu:between-open-and-revert file-menu)
              (make-object separator-menu-item% file-menu))]
          [define file-menu:save-string (lambda () (string-constant save-definitions))]
          [define file-menu:save-as-string (lambda () (string-constant save-definitions-as))]
          [define file-menu:between-save-as-and-print
            (lambda (file-menu)
              (let ([sub-menu (make-object menu% (string-constant save-other) file-menu)])
                (make-object menu:can-restore-menu-item%
                  (string-constant save-definitions-as-text)
                  sub-menu
                  (lambda (_1 _2)
                    (let ([filename (send definitions-text put-file #f #f)])
                      (when filename
                        (send definitions-text save-file/gui-error filename 'text)))))
                (make-object menu:can-restore-menu-item%
                  (string-constant save-interactions)
                  sub-menu
                  (lambda (_1 _2) 
                    (send interactions-text save-file/gui-error)))
                (make-object menu:can-restore-menu-item%
                  (string-constant save-interactions-as)
                  sub-menu
                  (lambda (_1 _2)
                    (let ([filename (send interactions-text put-file #f #f)])
                      (when filename
                        (send interactions-text save-file/gui-error filename 'standard)))))
                (make-object menu:can-restore-menu-item%
                  (string-constant save-interactions-as-text)
                  sub-menu
                  (lambda (_1 _2)
                    (let ([filename (send interactions-text put-file #f #f)])
                      (when filename
                        (send interactions-text save-file/gui-error filename 'text)))))
                (make-object separator-menu-item% file-menu)
                (set! logging-menu-item
                      (make-object menu:can-restore-menu-item%
                        (string-constant log-definitions-and-interactions)
                        file-menu
                        (lambda (x y)
                          (if logging
                              (stop-logging)
                              (start-logging)))))
                (make-object separator-menu-item% file-menu)))]
          
          [define file-menu:print-string (lambda () (string-constant print-definitions))]
          [define file-menu:between-print-and-close
            (lambda (file-menu)
              (set! file-menu:print-transcript-item
                    (make-object menu:can-restore-menu-item%
                      (string-constant print-interactions)
                      file-menu
                      (lambda (_1 _2)
                        (send interactions-text print
                              #t 
                              #t
                              (preferences:get 'framework:print-output-mode)))))
              (make-object separator-menu-item% file-menu))]
          
          (inherit get-edit-target-window)
          [define (split)
            (let* ([canvas-to-be-split (get-edit-target-window)]
                   [update
                    (lambda (set-canvases! canvases canvas% text)
                      (let-values ([(ox oy ow oh cursor-y)
                                    (get-visible-region canvas-to-be-split)])
                        (let ([orig-percentages (send resizable-panel get-percentages)]
                              [orig-canvases (send resizable-panel get-children)]
                              [new-canvas (make-object canvas% resizable-panel text)])
                          
                          (set-canvases!
                           (let loop ([canvases canvases])
                             (cond
                               [(null? canvases) (error 'split "couldn't split; didn't find canvas")]
                               [else
                                (let ([canvas (car canvases)])
                                  (if (eq? canvas canvas-to-be-split)
                                      (list* new-canvas
                                             canvas
                                             (cdr canvases))
                                      (cons canvas (loop (cdr canvases)))))])))
                          
                          (update-shown)
                          
                          ;; with-handlers prevents bad calls to set-percentages
                          ;; might still leave GUI in bad state, however.
                          (with-handlers ([not-break-exn? (lambda (x) (void))])
                            (send resizable-panel set-percentages
                                  (let loop ([canvases orig-canvases]
                                             [percentages orig-percentages])
                                    (cond
                                      [(null? canvases)
                                       (error 'split "couldn't split; didn't find canvas")]
                                      [(null? percentages)
                                       (error 'split "wrong number of percentages: ~s ~s"
                                              orig-percentages
                                              (send resizable-panel get-children))]
                                      [else (let ([canvas (car canvases)])
                                              (if (eq? canvas-to-be-split canvas)
                                                  (list* (/ (car percentages) 2)
                                                         (/ (car percentages) 2)
                                                         (cdr percentages))
                                                  (cons
                                                   (car percentages)
                                                   (loop (cdr canvases)
                                                         (cdr percentages)))))]))))
                          
                          (set-visible-region new-canvas ox oy ow oh cursor-y)
                          (set-visible-region canvas-to-be-split ox oy ow oh cursor-y)
                          
                          (send new-canvas focus))))])
              (cond
                [(memq canvas-to-be-split definitions-canvases)
                 (update (lambda (x) (set! definitions-canvases x))
                         definitions-canvases
                         definitions-canvas%
                         definitions-text)]
                [(memq canvas-to-be-split interactions-canvases)
                 (update (lambda (x) (set! interactions-canvases x))
                         interactions-canvases
                         interactions-canvas%
                         interactions-text)]
                [else (bell)]))]
          
          ;; split-demand : menu-item -> void
          ;; enables the menu-item if splitting is allowed, disables otherwise
          (define (split-demand item)
            (let ([canvas-to-be-split (get-edit-target-window)])
              (send item enable
                    (or (memq canvas-to-be-split definitions-canvases)
                        (memq canvas-to-be-split interactions-canvases))))) 
          
          ;; collapse-demand : menu-item -> void
          ;; enables the menu-item if collapsing is allowed, disables otherwise
          (define (collapse-demand item)
            (let ([canvas-to-be-split (get-edit-target-window)])
              (cond
                [(memq canvas-to-be-split definitions-canvases)
                 (send item enable (2 . <= . (length definitions-canvases)))]
                [(memq canvas-to-be-split interactions-canvases)
                 (send item enable (2 . <= . (length interactions-canvases)))]
                [else
                 (send item enable #f)])))
          
          ;; get-visible-region : editor-canvas -> number number number number (union #f number)
          ;; calculates the visible region of the editor in this editor-canvas, returning
          ;; four numbers for the x, y, width and height of the visible region
          ;; also, the last two booleans indiciate if the beginning and the end
          ;; of the selection was visible before the split, respectively.
          (define (get-visible-region canvas)
            (send canvas call-as-primary-owner
                  (lambda ()
                    (let* ([text (send canvas get-editor)]
                           [admin (send text get-admin)]
                           [start (send text get-start-position)]
                           [end (send text get-end-position)])
                      (let-values ([(x y w h) (get-visible-area admin)])
                        (let ([ysb (box 0)])
                          (send text position-location (send text get-start-position) #f ysb)
                          (values x y w h
                                  (and (= start end)
                                       (<= y (unbox ysb) (+ y h))
                                       (unbox ysb)))))))))
          
          ;; set-visible-region : editor-canvas number number number number (union #f number) -> void
          ;; sets the visible region of the text displayed by the editor canvas
          ;; to be the middle of the region (vertically) specified by x, y, w, and h.
          ;; if start-visible? and/or end-visible? are true, some special handling
          ;; is done to try to keep the start and end visible, with precendence
          ;; given to start if both are #t.
          (define (set-visible-region canvas x y w h cursor-y)
            (send canvas call-as-primary-owner
                  (lambda ()
                    (let* ([text (send canvas get-editor)]
                           [admin (send text get-admin)]
                           [nwb (box 0)]
                           [nhb (box 0)])
                      (send admin get-view #f #f nwb nhb)
                      (let* ([nw (unbox nwb)]
                             [nh (unbox nhb)]
                             
                             [nx x]
                             [raw-y (- (+ y (/ h 2)) (/ nh 2))]
                             [ny (if (and cursor-y 
                                          (not (<= raw-y cursor-y (+ raw-y nh))))
                                     (- cursor-y (/ nh 2))
                                     raw-y)])
                        (send canvas scroll-to nx ny nw nh #t)
                        (void))))))
          
          ;; get-visible-area : admin -> number number number number
          ;; returns the visible area for this admin
          (define (get-visible-area admin)
            (let ([bx (box 0)]
                  [by (box 0)]
                  [bw (box 0)]
                  [bh (box 0)])
              (send admin get-view bx by bw bh)
              (values (unbox bx)
                      (unbox by)
                      (unbox bw)
                      (unbox bh))))
          
          [define (collapse)
            (let* ([target (get-edit-target-window)]
                   [handle-collapse
                    (lambda (get-canvases set-canvases!)
                      (if (= 1 (length (get-canvases)))
                          (bell)
                          (let* ([old-percentages (send resizable-panel get-percentages)]
                                 [soon-to-be-bigger-canvas #f]
                                 [percentages
                                  (if (eq? (car (get-canvases)) target)
                                      (begin
                                        (set! soon-to-be-bigger-canvas (cadr (get-canvases)))
                                        (cons (+ (car old-percentages)
                                                 (cadr old-percentages))
                                              (cddr old-percentages)))
                                      (let loop ([canvases (cdr (get-canvases))]
                                                 [prev-canvas (car (get-canvases))]
                                                 [percentages (cdr old-percentages)]
                                                 [prev-percentage (car old-percentages)])
                                        (cond
                                          [(null? canvases)
                                           (error 'collapse "internal error.1")]
                                          [(null? percentages)
                                           (error 'collapse "internal error.2")]
                                          [else
                                           (if (eq? (car canvases) target)
                                               (begin
                                                 (set! soon-to-be-bigger-canvas prev-canvas)
                                                 (cons (+ (car percentages)
                                                          prev-percentage)
                                                       (cdr percentages)))
                                               (cons prev-percentage
                                                     (loop (cdr canvases)
                                                           (car canvases)
                                                           (cdr percentages)
                                                           (car percentages))))])))])
                            (unless soon-to-be-bigger-canvas
                              (error 'collapse "internal error.3"))
                            (set-canvases! (remq target (get-canvases)))
                            (update-shown)
                            
                            (let ([target-admin 
                                   (send target call-as-primary-owner
                                         (lambda ()
                                           (send (send target get-editor) get-admin)))]
                                  [to-be-bigger-admin 
                                   (send soon-to-be-bigger-canvas call-as-primary-owner
                                         (lambda ()
                                           (send (send soon-to-be-bigger-canvas get-editor) get-admin)))])
                              (let-values ([(bx by bw bh) (get-visible-area target-admin)])
                                
                                ;; this line makes the soon-to-be-bigger-canvas bigger
                                ;; if it fails, we're out of luck, but at least we don't crash.
                                (with-handlers ([not-break-exn? (lambda (x) (void))])
                                  (send resizable-panel set-percentages percentages))
                                
                                (let-values ([(ax ay aw ah) (get-visible-area to-be-bigger-admin)])
                                  (send soon-to-be-bigger-canvas scroll-to
                                        bx
                                        (- by (/ (- ah bh) 2))
                                        aw
                                        ah
                                        #t))))
                            
                            (send soon-to-be-bigger-canvas focus))))])
              (cond
                [(memq target definitions-canvases)
                 (handle-collapse
                  (lambda () definitions-canvases)
                  (lambda (c) (set! definitions-canvases c)))]
                [(memq target interactions-canvases)
                 (handle-collapse
                  (lambda () interactions-canvases)
                  (lambda (c) (set! interactions-canvases c)))]
                [else (bell)]))]
          

          
          (define interactions-shown? #t)
          (define definitions-shown? #t)
          (override update-shown on-close)
          [define update-shown
            (lambda ()
              (super-update-shown)
              
              (let ([new-children
                     (foldl
                      (lambda (shown? children sofar)
                        (if shown?
                            (append children sofar)
                            sofar))
                      null
                      (list interactions-shown?
                            definitions-shown?)
                      (list interactions-canvases
                            definitions-canvases))]
                    [p (preferences:get 'drscheme:unit-window-size-percentage)])
                
                (send definitions-item set-label 
                      (if definitions-shown?
                          (string-constant hide-definitions-menu-item-label)
                          (string-constant show-definitions-menu-item-label)))
                (send interactions-item set-label 
                      (if interactions-shown?
                          (string-constant hide-interactions-menu-item-label)
                          (string-constant show-interactions-menu-item-label)))
                
                (send resizable-panel begin-container-sequence)
                
                ;; this might change the unit-window-size-percentage, so save/restore it
                (send resizable-panel change-children (lambda (l) new-children))
                
                (preferences:set 'drscheme:unit-window-size-percentage p)
                
                ;; restore preferred interactions/definitions sizes
                (when (and (= 1 (length definitions-canvases))
                           (= 1 (length interactions-canvases))
                           (= 2 (length new-children)))
                  (with-handlers ([not-break-exn? (lambda (x) (void))])
                    (send resizable-panel set-percentages
                          (list p (- 1 p))))))
              
              (send resizable-panel end-container-sequence)
              
              (when (ormap (lambda (child)
                             (and (is-a? child editor-canvas%)
                                  (not (send child has-focus?))))
                           (send resizable-panel get-children))
                 (let loop ([children (send resizable-panel get-children)])
                   (cond
                     [(null? children) (void)]
                     [else (let ([child (car children)])
                             (if (is-a? child editor-canvas%)
                                 (send child focus)
                                 (loop (cdr children))))])))
               
               
               (for-each
                (lambda (get-item)
                  (let ([item (get-item)])
                    (when item
                      (send item enable definitions-shown?))))
                (list (lambda () (file-menu:get-revert-item))
                      (lambda () (file-menu:get-save-item))
                      (lambda () (file-menu:get-save-as-item))
                      ;(lambda () (file-menu:save-as-text-item)) ; Save As Text...
                      (lambda () (file-menu:get-print-item))))
               (send file-menu:print-transcript-item enable interactions-shown?))]
           
           [define on-close
             (lambda ()
               (when (eq? this created-frame)
                 (set! created-frame #f))
               (when logging
                 (stop-logging))
               (remove-logging-pref-callback)
               (send interactions-text shutdown)
               (send interactions-text on-close)
               (super-on-close))]
          
          (field [thread-to-break-box (make-weak-box #f)]
                 [custodian-to-kill-box (make-weak-box #f)]
                 [offer-kill? #f])
          
          ;; break-callback : -> void
          (define/public (break-callback)
            (cond
              [(or (not (weak-box-value thread-to-break-box))
                   (not (weak-box-value custodian-to-kill-box)))
               (bell)]
              [offer-kill? 
               (if (user-wants-kill?)
                   (when (weak-box-value thread-to-break-box)
                     (break-thread (weak-box-value thread-to-break-box)))
                   (when (weak-box-value custodian-to-kill-box)
                     (custodian-shutdown-all (weak-box-value custodian-to-kill-box))))]
              [else
               (break-thread (weak-box-value thread-to-break-box))
               ;; only offer a kill the next time if 
               ;; something got broken.
               (set! offer-kill? #t)]))

          ;; user-wants-kill? : -> boolean
          ;; handles events, so be sure to check state
          ;; after calling to avoid race conditions.
          (define/private (user-wants-kill?)
            (gui-utils:get-choice
             (string-constant kill-evaluation?)
             (string-constant just-break)
             (string-constant kill)
             (string-constant kill?)
             'diallow-close
             this))

          ;; reset-offer-kill
          (define/public (reset-offer-kill)
            (set! offer-kill? #f))
          
          ;; get-breakables : -> (union #f thread) (union #f cust) -> void
          (define/public (get-breakables)
            (values (weak-box-value thread-to-break-box) (weak-box-value custodian-to-kill-box)))

          ;; set-breakables : (union #f thread) (union #f cust) -> void
          (define/public (set-breakables thd cust)
            (set! thread-to-break-box (make-weak-box thd))
            (set! custodian-to-kill-box (make-weak-box cust)))
          
          (define/public (execute-callback)
            (check-if-save-file-up-to-date)
            (ensure-rep-shown)
            (when logging
              (log-definitions)
              (log-interactions))
            (send definitions-text just-executed)
            (send interactions-canvas focus)
            (send interactions-text reset-console)
            (send interactions-text clear-undos)
            (let ([start (if (and ((send definitions-text last-position) . >= . 2)
                                  (char=? (send definitions-text get-character 0) #\#)
                                  (char=? (send definitions-text get-character 1) #\!))
                             (send definitions-text paragraph-start-position 1)
                             0)])
              (send definitions-text split-snip start)
              (send interactions-text do-many-text-evals
                    definitions-text 
                    start
                    (send definitions-text last-position)
                    #t))
            (send interactions-text clear-undos))
          
          (inherit revert save)
          (define/private (check-if-save-file-up-to-date)
            (when (send definitions-text save-file-out-of-date?)
              (let ([user-choice 
                     (message-box/custom
                      (string-constant drscheme)
                      (string-constant definitions-modified)
                      (string-constant ignore)
                      (string-constant revert)
                      #f
                      this
                      '(caution no-default number-order)
                      1)])
                (case user-choice
                  [(1) (void)]
                  [(2) (revert)]))))
          
          (inherit get-menu-bar get-focus-object get-edit-target-object)
          [define language-menu 'uninited-language-menu]
          
          (rename [super-on-size on-size])
          (override on-size)
          [define on-size
            (lambda (w h)
              (preferences:set 'drscheme:unit-window-width w)
              (preferences:set 'drscheme:unit-window-height h)
              (super-on-size w h))]
          
          (override get-editor get-canvas)
          [define get-editor (lambda () definitions-text)]
          [define get-canvas (lambda () definitions-canvas)]
          
          (define/override (get-delegated-text) definitions-text)
          (define/override (get-open-here-editor) definitions-text)
          
          [define definitions-text (make-object (drscheme:get/extend:get-definitions-text))]
          [define interactions-text (make-object 
                                        (drscheme:get/extend:get-interactions-text)
                                      this)]
          (public get-definitions-text get-interactions-text)
          [define get-definitions-text (lambda () definitions-text)]
          [define get-interactions-text (lambda () interactions-text)]
          
          (define (update-teachpack-menu)
            (define user-teachpack-cache (send (get-interactions-text) get-user-teachpack-cache))
            (for-each (lambda (item) (send item delete)) teachpack-items)
            (set! teachpack-items
                  (map (lambda (name)
                         (make-object menu:can-restore-menu-item%
                           (format (string-constant clear-teachpack) 
                                   (mzlib:file:file-name-from-path name))
                           language-menu
                           (lambda (item evt)
                             (let ([new-teachpacks 
                                    (drscheme:teachpack:new-teachpack-cache
                                     (remove
                                      name
                                      (drscheme:teachpack:teachpack-cache-filenames
                                       user-teachpack-cache)))])
                               (send (get-interactions-text) set-user-teachpack-cache new-teachpacks)
                               (preferences:set 'drscheme:teachpacks new-teachpacks)))))
                       (drscheme:teachpack:teachpack-cache-filenames
                        user-teachpack-cache))))
          
          (define/public (get-definitions/interactions-panel-parent)
            (get-area-container))
          
          (inherit delegated-text-shown? hide-delegated-text show-delegated-text)
          (rename [super-add-show-menu-items add-show-menu-items])
          (define/override (add-show-menu-items show-menu)
            (super-add-show-menu-items show-menu)
            (set! definitions-item
                  (make-object menu:can-restore-menu-item%
                    (string-constant hide-definitions-menu-item-label)
                    (get-show-menu)
                    (lambda (_1 _2) 
                      (toggle-show/hide-definitions)
                      (update-shown))
                    #\d
                    (string-constant definitions-menu-item-help-string)))
            (set! interactions-item
                  (make-object menu:can-restore-menu-item%
                    (string-constant show-interactions-menu-item-label)
                    (get-show-menu)
                    (lambda (_1 _2) 
                      (toggle-show/hide-interactions)
                      (update-shown))
                    #\e
                    (string-constant interactions-menu-item-help-string)))
            
            (instantiate menu:can-restore-menu-item% ()
              (shortcut #\u)
              (label 
               (if (delegated-text-shown?)
                   (string-constant hide-overview)
                   (string-constant show-overview)))
              (parent (get-show-menu))
              (callback
               (lambda (menu evt)
                 (if (delegated-text-shown?)
                     (begin
                       (send menu set-label (string-constant show-overview))
                       (preferences:set 'framework:show-delegate? #f)
                       (hide-delegated-text))
                     (begin
                       (send menu set-label (string-constant hide-overview))
                       (preferences:set 'framework:show-delegate? #t)
                       (show-delegated-text))))))
            
            ;; waiting for support from compiler from matthew for this one.
            (instantiate menu:can-restore-menu-item% ()
              (label (if module-browser-shown?
                         (string-constant hide-module-browser)
                         (string-constant show-module-browser)))
              (parent (get-show-menu))
              (callback
               (lambda (menu evt)
                 (set! module-browser-shown? (not module-browser-shown?))
                 (if module-browser-shown?
                     (begin (send menu set-label (string-constant hide-module-browser))
                            (show-module-browser))
                     (begin (send menu set-label (string-constant show-module-browser))
                            (hide-module-browser)))))))
          
          (field [module-browser-shown? #f]
                 [module-browser-parent-panel #f]
                 [module-browser-panel #f]
                 [module-browser-ec #f]
                 [module-browser-button #f]
                 [module-browser-lib-path-check-box #f]
                 [module-browser-pb #f])

          (inherit open-status-line close-status-line update-status-line)
          
          (define/private (show-module-browser)
            (when module-browser-panel
              (update-module-browser-pane)))
          
          (define/private (hide-module-browser)
            (when module-browser-panel
              (close-status-line 'plt:module-browser:mouse-over)
              (send module-browser-parent-panel change-children
                    (lambda (l)
                      (remq module-browser-panel l)))
              (let loop ()
                (let ([snip (send module-browser-pb find-first-snip)])
                  (when snip
                    (send module-browser-pb delete snip)
                    (loop))))))
          
          (define/private (update-module-browser-pane)
            (open-status-line 'plt:module-browser:mouse-over)
            
            ;; open twice here -- once for the callbacks from make-module-overview-pasteboard
            ;; and once for the status messages during drscheme:init:expand-program
            (open-status-line 'plt:module-browser)
            (open-status-line 'plt:module-browser)
            
            (update-status-line 'plt:module-browser status-compiling-definitions)
            (send module-browser-panel begin-container-sequence)
            (unless module-browser-ec 
              (set! module-browser-pb 
                    (drscheme:module-overview:make-module-overview-pasteboard
                     #t
                     #f 
                     (lambda (x) ;=drscheme thread=
                       (mouse-currently-over x))
                     (lambda (msg) ;=user compile thread=
                       (if (eq? msg 'done)
                           (close-module-browser-status)
                           (show-module-browser-status msg)))))
              (set! module-browser-ec (make-object editor-canvas%
                                        module-browser-panel
                                        module-browser-pb))
              (set! module-browser-lib-path-check-box
                    (instantiate check-box% ()
                      (parent module-browser-panel)
                      (label show-lib-paths)
                      (value (preferences:get 'drscheme:module-browser:show-lib-paths?))
                      (callback 
                       (lambda (x y) 
                         (let ([val (send module-browser-lib-path-check-box get-value)])
                           (preferences:set 'drscheme:module-browser:show-lib-paths? val)
                           (send module-browser-pb show-lib-paths val))))))
              (set! module-browser-button (instantiate button% ()
                                            (parent module-browser-panel)
                                            (label refresh)
                                            (callback (lambda (x y) (update-module-browser-pane)))
                                            (stretchable-width #t))))
            
            (let ([p (preferences:get 'drscheme:module-browser-size-percentage)])
              (send module-browser-parent-panel change-children
                    (lambda (l)
                      (cons module-browser-panel
                            (remq module-browser-panel l))))
              (send module-browser-parent-panel set-percentages
                    (list p (- 1 p)))
              (send module-browser-parent-panel end-container-sequence)
              (calculate-module-browser)))
            
          (field [status-callback-running? #f]
                 [status-callback-close? #f]
                 [status-callback-str #f]
                 [status-callback-sema (make-semaphore 1)])
          (define/private (show-module-browser-status msg)
            (semaphore-wait status-callback-sema)
            (set! status-callback-str msg)
            (start-status-callback)
            (semaphore-post status-callback-sema))
          
          (define/private (close-module-browser-status)
            (semaphore-wait status-callback-sema)
            (set! status-callback-close? #t)
            (start-status-callback)
            (semaphore-post status-callback-sema))
          
          ;; @pre in atomic region, wrt close-module-browser-status and show-module-browser-status
          (define (start-status-callback)
            (unless status-callback-running? 
              (set! status-callback-running? #t)
              (thread
               (lambda ()
                 (sleep 0.5)
                 (parameterize ([current-eventspace drscheme:init:system-eventspace]) 
                   ;; must run in the eventspace of this frame (which is drscheme:init:system-eventspace)
                   (queue-callback
                    (lambda ()
                      (semaphore-wait status-callback-sema)
                      (if status-callback-close?
                          (close-status-line 'plt:module-browser)
                          (update-status-line
                           'plt:module-browser 
                           (format module-browser-progress-constant status-callback-str)))
                      (set! status-callback-running? #f)
                      (set! status-callback-str #f)
                      (set! status-callback-close? #f)
                      (semaphore-post status-callback-sema))
                    #f))))))
          
          (define/private (mouse-currently-over snips)
            (if (null? snips)
                (update-status-line 'plt:module-browser:mouse-over #f)
                (let* ([snip (car snips)]
                       [lines (send snip get-lines)]
                       [name (or (send snip get-filename)
                                 (send snip get-word))]
                       [str (if lines
                                (format (string-constant module-browser-filename-format) name lines)
                                name)])
                  (update-status-line 'plt:module-browser:mouse-over str))))
            
          (define/private (calculate-module-browser) ;=drscheme thread=
            (let* ([defs (get-definitions-text)]
                   [text-pos (drscheme:language:make-text/pos 
                              defs
                              0
                              (send defs last-position))]
                   [shutdown
                    (lambda () ;=user compile thread=
                      (send module-browser-pb render-snips)
                      (send module-browser-button enable #t)
                      (send module-browser-lib-path-check-box enable #t)
                      (close-status-line 'plt:module-browser))]
                   [language-settings (send (get-definitions-text) get-next-settings)]
                   [kill-termination (lambda () (shutdown))]
                   [init (lambda () (set-directory defs))]
                   [complete-program? #t]
                   [iter 
                    (lambda (exp cont) ;=user compile thread=
                      (cond
                        [(eof-object? exp) 
                         (shutdown)]
                        [(pair? exp) 
                         (shutdown)]
                        [else
                         (let ([err (send module-browser-pb add-connections exp)])
                           (when err
                             (message-box (string-constant drscheme) err)))
                         (update-status-line 'plt:module-browser status-compiling-definitions)
                         (cont)]))])
              (send module-browser-button enable #f)
              (send module-browser-lib-path-check-box enable #f)
              ((drscheme:eval:traverse-program/multiple
                language-settings init kill-termination)
               text-pos iter complete-program?)))
          
          ;; set-directory : text -> void
          ;; sets the current-directory and current-load-relative-directory
          ;; based on the file saved in the definitions-text
          (define (set-directory definitions-text)
            (let* ([tmp-b (box #f)]
                   [fn (send definitions-text get-filename tmp-b)])
              (unless (unbox tmp-b)
                (when fn
                  (let-values ([(base name dir?) (split-path fn)])
                    (current-directory base)
                    (current-load-relative-directory base))))))
          
          (super-instantiate ()
            (filename filename)
            (width (preferences:get 'drscheme:unit-window-width))
            (height (preferences:get 'drscheme:unit-window-height)))
          
          (let* ([mb (get-menu-bar)]
                 [language-menu-on-demand
                  (lambda (menu-item)
                    (update-teachpack-menu))]
                 [_ (set! language-menu (make-object (get-menu%) 
                                          (string-constant language-menu-name)
                                          mb
                                          #f
                                          language-menu-on-demand))]
                 [scheme-menu (make-object (get-menu%) (string-constant scheme-menu-name) mb)]
                 [send-method
                  (lambda (method)
                    (lambda (_1 _2)
                      (let ([text (get-focus-object)])
                        (when (is-a? text scheme:text<%>)
                          (method text)))))])
            
            (make-object menu:can-restore-menu-item%
              (string-constant choose-language-menu-item-label)
              language-menu
              (lambda (_1 _2)
                (let ([new-settings (drscheme:language-configuration:language-dialog
                                     #f
                                     (send definitions-text get-next-settings)
                                     this)])
                  (when new-settings
                    (send definitions-text set-next-settings new-settings)
                    (preferences:set
                     drscheme:language-configuration:settings-preferences-symbol
                     new-settings))))
              #\l)
            (make-object separator-menu-item% language-menu)
            (make-object menu:can-restore-menu-item%
              (string-constant add-teachpack-menu-item-label)
              language-menu
              (lambda (_1 _2)
                (drscheme:language-configuration:add-new-teachpack this)))
            (let ([clear-all-on-demand
                   (lambda (menu-item)
                     (send menu-item enable
                           (not (null? (drscheme:teachpack:teachpack-cache-filenames
                                        (preferences:get 'drscheme:teachpacks))))))])
              (make-object menu:can-restore-menu-item% 
                (string-constant clear-all-teachpacks-menu-item-label)
                language-menu
                (lambda (_1 _2) (drscheme:language-configuration:clear-all-teachpacks))
                #f
                #f
                clear-all-on-demand))
            
            (set! execute-menu-item
                  (make-object menu:can-restore-menu-item%
                    (string-constant execute-menu-item-label)
                    scheme-menu
                    (lambda (_1 _2) (execute-callback))
                    #\t
                    (string-constant execute-menu-item-help-string)))
            (make-object menu:can-restore-menu-item%
              (string-constant break-menu-item-label)
              scheme-menu
              (lambda (_1 _2) (break-callback))
              #\b
              (string-constant break-menu-item-help-string))
            (make-object menu:can-restore-menu-item%
              (string-constant kill-menu-item-label)
              scheme-menu
              (lambda (_1 _2) (send interactions-text kill-evaluation))
              #\k
              (string-constant kill-menu-item-help-string))
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant clear-error-highlight-menu-item-label))
              (parent scheme-menu)
              (callback (lambda (_1 _2) (drscheme:rep:reset-error-ranges)))
              (help-string (string-constant clear-error-highlight-item-help-string))
              (demand-callback
               (lambda (item)
                 (send item enable (drscheme:rep:get-error-ranges)))))
            (make-object separator-menu-item% scheme-menu)
            (make-object menu:can-restore-menu-item%
              (string-constant create-executable-menu-item-label)
              scheme-menu
              (lambda (x y) (create-executable this)))
            (make-object menu:can-restore-menu-item%
              (string-constant module-browser...)
              scheme-menu
              (lambda (x y) (drscheme:module-overview:module-overview this)))
            (make-object separator-menu-item% scheme-menu)
            (make-object menu:can-restore-menu-item%
              (string-constant reindent-menu-item-label)
              scheme-menu
              (send-method (lambda (x) (send x tabify-selection))))
            (make-object menu:can-restore-menu-item%
              (string-constant reindent-all-menu-item-label)
              scheme-menu
              (send-method (lambda (x) (send x tabify-all)))
              #\i)
            (make-object menu:can-restore-menu-item%
              (string-constant semicolon-comment-out-menu-item-label)
              scheme-menu
              (send-method (lambda (x) (send x comment-out-selection))))
            (make-object menu:can-restore-menu-item%
              (string-constant box-comment-out-menu-item-label)
              scheme-menu
              (send-method (lambda (x) (send x box-comment-out-selection))))
            (make-object menu:can-restore-menu-item%
              (string-constant uncomment-menu-item-label)
              scheme-menu
              (lambda (x y)
                (let ([text (get-focus-object)])
                  (when (is-a? text text%)
                    (let ([admin (send text get-admin)])
                      (cond
                        [(is-a? admin editor-snip-editor-admin<%>)
                         (let ([es (send admin get-snip)])
                           (cond
                             [(is-a? es comment-box:snip%)
                              (let ([es-admin (send es get-admin)])
                                (when es-admin
                                  (let ([ed (send es-admin get-editor)])
                                    (when (is-a? ed scheme:text<%>)
                                      (send ed uncomment-box/selection)))))]
                             [else (send text uncomment-selection)]))]
                        [else (send text uncomment-selection)])))))))

          (inherit get-menu-item%)
          (field [special-menu (make-object (get-menu%) (string-constant special-menu) (get-menu-bar))])
          (define/public (get-special-menu) special-menu)

          (let ([has-editor-on-demand
                 (lambda (menu-item)
                   (let ([edit (get-edit-target-object)])
                     (send menu-item enable (and edit (is-a? edit editor<%>)))))]
                [callback
                 (lambda (menu evt)
                   (let ([edit (get-edit-target-object)])
                     (when (and edit
                                (is-a? edit editor<%>))
                       (let ([number (get-fraction-from-user this)])
                         (when number
                           (send edit insert
                                 (drscheme:number-snip:make-fraction-snip number #f)))))
                     #t))]
                [insert-lambda
                 (lambda ()
                   (let ([edit (get-edit-target-object)])
                     (when (and edit
                                (is-a? edit editor<%>))
                       (send edit insert (instantiate lambda-snip% ()))))
                   #t)]
                [insert-large-semicolon-letters
                 (lambda ()
                   (let ([edit (get-edit-target-object)])
                     (when edit
                       (let ([str (get-text-from-user (string-constant large-semicolon-letters)
                                                      (string-constant text-to-insert)
                                                      this)])
                         (when str
                           (let ()
                             (define bdc (make-object bitmap-dc% (make-object bitmap% 1 1 #t)))
                             (define-values (tw th td ta) (send bdc get-text-extent str))
                             (define tmp-color (make-object color%))
                             (define (get-char x y)
                               (send bdc get-pixel x y tmp-color)
                               (if (zero? (send tmp-color red))
                                   #\;
                                   #\space))
                             
                             (define bitmap
                               (make-object bitmap% 
                                 (+ 3 (inexact->exact tw))
                                 (inexact->exact th) 
                                 #t))
                             
                             (define (fetch-line y)
                               (let loop ([x (send bitmap get-width)]
                                          [chars null])
                                 (cond
                                   [(zero? x) (apply string chars)]
                                   [else (loop (- x 1) (cons (get-char (- x 1) y) chars))])))
                             
                             (send bdc set-bitmap bitmap)
                             (send bdc clear)
                             (send bdc draw-line 0 0 0 th)
                             (send bdc draw-text str 3 0)
                             
                             (send edit begin-edit-sequence)
                             (let ([start (send edit get-start-position)]
                                   [end (send edit get-end-position)])
                               (send edit delete start end)
                               (send edit insert "\n" start start)
                               (let loop ([y (send bitmap get-height)])
                                 (unless (zero? y)
                                   (send edit insert (fetch-line (- y 1)) start start)
                                   (send edit insert "\n" start start)
                                   (loop (- y 1)))))
                             (send edit end-edit-sequence)))))))]
                [c% (get-menu-item%)])
            (frame:add-snip-menu-items special-menu (get-menu-item%))

            (make-object c% (string-constant insert-fraction-menu-item-label)
              special-menu callback 
              #f #f
              has-editor-on-demand)
            (make-object c% (string-constant insert-large-letters...)
              special-menu
              (lambda (x y) (insert-large-semicolon-letters))
              #f #f
              has-editor-on-demand)
            (make-object c% (string-constant insert-lambda)
              special-menu
              (lambda (x y) (insert-lambda))
              #\\
              #f
              has-editor-on-demand))
          
          (frame:reorder-menus this)
           
          (make-object separator-menu-item% (get-show-menu))
          
          (instantiate menu:can-restore-menu-item% ()
            (shortcut #\m)
            (label (string-constant split-menu-item-label))
            (parent (get-show-menu))
            (callback (lambda (x y) (split)))
            (demand-callback (lambda (item) (split-demand item))))
          (instantiate menu:can-restore-menu-item% () 
            (shortcut #\r)
            (label (string-constant collapse-menu-item-label))
            (parent (get-show-menu))
            (callback (lambda (x y) (collapse)))
            (demand-callback (lambda (item) (collapse-demand item))))
          
          [define top-panel (make-object horizontal-panel% (get-area-container))]
          [define name-panel (instantiate vertical-panel% ()
                               (parent top-panel)
                               (stretchable-width #f)
                               (stretchable-height #f))]
          [define resizable-panel (instantiate vertical-dragable/def-int% ()
                                    (unit-frame this)
                                    (parent (get-definitions/interactions-panel-parent)))]
          
          [define definitions-canvas (make-object (drscheme:get/extend:get-definitions-canvas)
                                       resizable-panel)]
          [define definitions-canvases (list definitions-canvas)]
          [define interactions-canvas (make-object (drscheme:get/extend:get-interactions-canvas)
                                        resizable-panel)]
          [define interactions-canvases (list interactions-canvas)]
          
          (define/public (get-definitions-canvases) 
            ;; before definition, just return null
            (if (pair? definitions-canvases)
                definitions-canvases
                null))
          (define/public (get-interactions-canvases)
            ;; before definition, just return null
            (if (pair? interactions-canvases)
                interactions-canvases
                null))
          
          (public get-definitions-canvas get-interactions-canvas)
          [define get-definitions-canvas (lambda () definitions-canvas)]
          [define get-interactions-canvas (lambda () interactions-canvas)]
          
          (send interactions-text auto-wrap #t)
          (send interactions-canvas set-editor interactions-text)
          (send definitions-canvas set-editor definitions-text)
          
          (set! save-button
                (make-object button% 
                  (make-save-bitmap this)
                  top-panel
                  (lambda args
                    (when definitions-text
                      (save)
                      (send definitions-canvas focus)))))
          
          (set! name-message (make-object drs-name-message% name-panel))
          [define teachpack-items null]
          [define break-button (void)]
          [define execute-button (void)]
          [define button-panel (make-object horizontal-panel% top-panel)]
          (public get-execute-button get-break-button get-button-panel)
          [define get-execute-button (lambda () execute-button)]
          [define get-break-button (lambda () break-button)]
          [define get-button-panel (lambda () button-panel)]
          
          [define func-defs-canvas (make-object func-defs-canvas% name-panel this definitions-text)]
          
          (set! execute-button
                (make-object button%
                  (make-execute-bitmap this)
                  button-panel
                  (lambda (button evt) (execute-callback))))
          (set! break-button
                (make-object button%
                  (make-break-bitmap this) 
                  button-panel
                  (lambda (x y)
		    (break-callback))))
          (send button-panel stretchable-height #f)
          (send button-panel stretchable-width #f) 
          
          (send top-panel change-children
                (lambda (l)
                  (list name-panel save-button
                        (make-object vertical-panel% top-panel) ;; spacer
                        button-panel)))
          
          (send top-panel stretchable-height #f)
          (inherit get-label)
          (let ([m (send definitions-canvas get-editor)])
            (set-save-init-shown?
             (and m (send m is-modified?))))
	  
          (let ([ed-filename (send definitions-text get-filename)])
            (send name-message set-message
                  (if ed-filename #t #f)
                  (or ed-filename (get-label) (string-constant untitled))))
          
          (update-save-button #f)
          
          (send interactions-text initialize-console)
          
          (cond
            [filename
             (set! definitions-shown? #t)
             (set! interactions-shown? #f)]
            [else
             (set! definitions-shown? #t)
             (set! interactions-shown? #t)])
          
          (update-shown)
          
          (when (= 2 (length (send resizable-panel get-children)))
            ;; should really test this, but too lazy to add inspector to framework (for now)
            (with-handlers ([not-break-exn? (lambda (x) (void))])
              (send resizable-panel set-percentages
                    (let ([p (preferences:get 'drscheme:unit-window-size-percentage)])
                      (list p (- 1 p))))))
          
          (set-label-prefix (string-constant drscheme))
          
          (send definitions-canvas focus)
          (cond
            [(eq? created-frame 'nothing-yet)
             (set! created-frame this)]
            [created-frame
             (set! created-frame #f)]
            [else (void)])))

      (define module-browser-dragable-panel%
        (class panel:horizontal-dragable%
          (inherit get-percentages)
          (define/override (after-percentage-change)
            (let ([percentages (get-percentages)])
              (when (and (pair? percentages)
                         (pair? (cdr percentages))
                         (null? (cddr percentages)))
                (preferences:set 'drscheme:module-browser-size-percentage
                                 (car percentages)))))
          (super-instantiate ())))
      
      (define drs-name-message%
        (class name-message%
          (define/override (on-choose-directory dir)
            (let ([file (finder:get-file dir)])
              (when file
                (handler:edit-file file))))
          (super-instantiate ())))
      
      (define lambda-snipclass
        (make-object (class snip-class% ()
                       (define/override (read p)
                         (make-object lambda-snip%))
                       (super-instantiate ()))))
      (send lambda-snipclass set-version 1)
      (send lambda-snipclass set-classname "drscheme:lambda-snip%")
      (send (get-the-snip-class-list) add lambda-snipclass)
      
      (define lambda-snip% 
        (class* snip% (readable-snip<%>)
          (define/public (read-one-special index source line column position)
            (values 'lambda 1 #t))
          
          (define/private (get-normal-font)
            (send the-font-list find-or-create-font
                  (preferences:get 'drscheme:font-size)
                  'modern 'normal 'normal #f))
          (define/private (get-lambda-font)
            (send the-font-list find-or-create-font 
                  (preferences:get 'drscheme:font-size)
                  'symbol 'normal 'normal #f))
          (define/public (get-string) "lambda")
          (define/override get-text
            (case-lambda
              [(x y) " lambda "]
              [(x y z) " lambda "]))
          (define/override (copy)
            (make-object lambda-snip%))
          (define/override (write p)
            (void))
          (define/override (get-extent dc x y wb hb descentb spaceb lspaceb rspaceb)
            (let-values ([(w h d s) (send dc get-text-extent "W" (get-normal-font))])
              (set-box/f! wb w)
              (set-box/f! hb h)
              (set-box/f! descentb d)
              (set-box/f! spaceb s)
              (set-box/f! lspaceb 0)
              (set-box/f! rspaceb 0)))
          (define/override (draw dc x y left top right bottom dx dy draw-caret)
            (let ([font (send dc get-font)])
              (let-values ([(ww wh wd ws) (send dc get-text-extent "W" (get-normal-font))])
                (send dc set-font (get-lambda-font))
                (let-values ([(lw lh ld ls) (send dc get-text-extent "l")])
                  (send dc draw-text "l" 
                        (+ x (/ (- ww lw) 2))
                        (+ y (- (- wh wd) (- lh ld)))))
                (send dc set-font font))))
          (inherit set-snipclass)
          (super-instantiate ())
          (set-snipclass lambda-snipclass)))
      
      (define created-frame 'nothing-yet)
      
      (define open-drscheme-window
        (case-lambda
         [() (open-drscheme-window #f)]
         [(name)
          (if (and created-frame
                   name
                   (not (eq? created-frame 'nothing-yet)) 
                   (send created-frame still-untouched?))
              (begin (send created-frame change-to-file name)
                     (send created-frame show #t)
                     created-frame)
              (let* ([drs-frame% (drscheme:get/extend:get-unit-frame)]
		     [frame (instantiate drs-frame% () (filename name))])
                (send frame show #t)
                frame))]))
      
      (handler:insert-format-handler 
       "Units"
       (lambda (filename) #t)
       open-drscheme-window))))
