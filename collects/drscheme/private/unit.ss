
(module unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
           "drsig.ss"
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix mzlib:file: (lib "file.ss"))
           (prefix mzlib:list: (lib "list.ss"))
           (prefix mzlib:date: (lib "date.ss")))
  
  (provide unit@)
  
  (define unit@
    (unit/sig drscheme:unit^
      (keymap:add-to-right-button-menu
       (lambda (menu text event)
         (when (and (is-a? text text%)
                    (is-a? event mouse-event%))
           (let* ([end (send text get-end-position)]
                  [start (send text get-start-position)]
                  [non-letter? (lambda (x)
                                 (or (char-whitespace? x)
                                     (memq x ' (#\` #\' #\, #\;
                                                 #\{ #\( #\[ #\] #\) #\}))))])
             (unless (= 0 (send text last-position))
               (let ([str
                      (if (= end start)
                          (let* ([pos 
                                  (call-with-values
                                   (lambda ()
                                     (send text dc-location-to-editor-location
                                           (send event get-x)
                                           (send event get-y)))
                                   (lambda (x y)
                                     (send text find-position x y)))]
                                 [before
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
                            (apply string (append before after)))
                          (send text get-text start end))])
                 (unless (string=? str "")
                   (make-object separator-menu-item% menu)
                   (make-object menu-item%
                     (format "Search in Help Desk for \"~a\"" str)
                     menu
                     (lambda x (drscheme:help-desk:help-desk str #f 'keyword+index 'contains)))
                   (make-object menu-item%
                     (format "Exact lucky search in Help Desk for \"~a\"" str)
                     menu
                     (lambda x (drscheme:help-desk:help-desk str #t 'keyword+index 'exact))))))))))
      
      (define (get-fraction-from-user)
        (let* ([dlg (make-object dialog% "Enter Fraction")]
               [hp (make-object horizontal-panel% dlg)]
               [_1 (make-object message% "Whole Part" hp)]
               [whole (make-object text-field% #f hp void)]
               [vp (make-object vertical-panel% hp)]
               [hp2 (make-object horizontal-panel% vp)]
               [num (make-object text-field% #f hp2 void)]
               [num-m (make-object message% "Numerator" hp2)]
               [hp3 (make-object horizontal-panel% vp)]
               [den (make-object text-field% #f hp3 void)]
               [den-m (make-object message% "Denominator" hp3)]
               [bp (make-object horizontal-panel% dlg)]
               [ok? #f]
               [validate-number
                (lambda ()
                  (let ([num-s (string->number (send num get-value))]
                        [den-s (string->number (send den get-value))]
                        [whole-s (string->number (send whole get-value))])
                    (if (and num-s den-s whole-s)
                        (let ([ans (+ whole-s (/ num-s den-s))])
                          (if (and (exact? ans)
                                   (real? ans)
                                   (not (integer? ans)))
                              ans
                              #f))
                        #f)))]
               [ok (make-object button% "OK" bp 
                     (lambda x
                       (cond
                         [(validate-number)
                          (set! ok? #t)
                          (send dlg show #f)]
                         [else 
                          (message-box
                           "DrScheme"
                           "Invalid number: must be an exact, real, non-integral number.")]))
                     '(border))]
               [cancel (make-object button% "Cancel" bp (lambda x (send dlg show #f)))])
          (let ([mw (max (send den-m get-width) (send num-m get-width))])
            (send den-m min-width mw)
            (send num-m min-width mw))
          (send bp set-alignment 'right 'center)
          (send dlg show #t)
          (and ok? (validate-number))))
      
      (define (basename fn)
        (if fn
            (let* ([file-name (mzlib:file:file-name-from-path fn)]
                   [ext (mzlib:file:filename-extension file-name)])
              (if ext
                  (substring file-name 0 (- (string-length file-name)
                                            (string-length ext)
                                            1))
                  file-name))
            "Untitled"))
      
      (define (create-launcher frame)
        (let* ([program-filename (send (ivar frame definitions-text) get-filename)]
               [executable-filename
                (if (eq? (system-type) 'windows)
                    (string-append (basename program-filename) ".exe")
                    (basename program-filename))]
               [settings (preferences:get drscheme:language:settings-preferences-symbol)])
          
          (cond
            [(not program-filename)
             (message-box "Create Launcher"
                               "You must save your program before creating a launcher"
                               frame)]
            
            [else
             (let* ([filename 
                     (parameterize ([finder:dialog-parent-parameter frame]
                                    [finder:default-extension "exe"])
                       (finder:put-file
                        executable-filename
                        #f #f "Save a Launcher"))]
                    [in-mz? (regexp-match "MzScheme" (basis:setting-name settings))]
                    [teachpacks (preferences:get 'drscheme:teachpack-file)])
               (when filename
                 (cond
	     ;; this condition should guarantee that the language
	     ;; matches the default mred or mzscheme initial language.
	     ;; in that case, we don't need to load any of the
	     ;; zodiac or drs support so that program starts up much
	     ;; faster.
                   [(and (null? teachpacks)
                         (basis:full-language? settings)
                         (not (basis:zodiac-vocabulary? settings))
                         (ormap (lambda (x) (equal? x settings)) basis:settings))
                    ((if in-mz?
                         launcher:make-mzscheme-launcher
                         launcher:make-mred-launcher)
                     (list "-qmve"
                           (format "((require-library \"launcher-raw-bootstrap.ss\" \"userspce\") ~s)"
                                   program-filename))
                     filename)]
                   [else
                    (let* ([v-settings (struct->vector settings)]
                           [definitions (list "-e" (format "(define filename ~s)" program-filename)
                                              "-e" (format "(define settings ~s)" v-settings)
                                              "-e" (format "(define teachpacks '~s)" teachpacks))])
                      ((if (and in-mz? (null? teachpacks))
                           launcher:make-mzscheme-launcher
                           launcher:make-mred-launcher)
                       (append '("-qmv") definitions '("-L" "launcher-bootstrap.ss" "userspce"))
                       filename))])))])))
      
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
      
      (define make-execute-bitmap (make-bitmap "execute"))
      (define make-save-bitmap (make-bitmap "save"))
      (define make-break-bitmap (make-bitmap "break"))
      
  ;; this is the old definition of the interactions canvas.
  ;; It should be integrated into canvas:wide-snip% 
  ;; becuase it uses a better algorithm to find the snip
  ;; wide widths.
      '(class-asi wide-snip-canvas% ; to match rep-new.ss, inherit from wrapping-canvas% 
         (inherit get-editor)
         (rename [super-on-size on-size]
                 [super-set-media set-media])
         (public)
         (private
           [snips null]
           [autowrap-snips? (preferences:get 'auto-set-wrap?)]
           [update-snip-size
            (lambda (s)
              (if (is-a? s editor-snip%)
                  (let* ([snip-x-pos&margins
                          (let loop ([snip s])
                            (let* ([snip-x-pos (box 0)]
                                   [containing-media (send (send snip get-admin) get-editor)]
                                   [containing-admin (send containing-media get-admin)])
                              (send containing-media get-snip-position-and-location
                                    snip (box 0) snip-x-pos (box 0))
                              (+ (let ([lmargin (box 0)]
                                       [rmargin (box 0)])
                                   (send snip get-margin lmargin (box 0) rmargin (box 0))
                                   (+ (unbox lmargin) (unbox rmargin)))
                                 (if (is-a? containing-admin editor-snip-editor-admin<%>)
                                     (+ (unbox snip-x-pos)
                                        (loop (send containing-admin get-snip)))
                                     (unbox snip-x-pos)))))]
                         [outer-snip (let loop ([snip s])
                                       (let ([containing-admin
                                              (send (send (send snip get-admin)
                                                          get-editor) get-admin)])
                                         (if (is-a? containing-admin editor-snip-editor-admin<%>)
                                             (loop (send containing-admin get-snip))
                                             snip)))]					
                         [view-width (let* ([width (box 0)]
                                            [extra-space 2] ;; this is to allow the edit room
					;; to show the caret at the end
					;; of the line
                                            [media (get-editor)])
                                       (send (send media get-admin)
                                             get-view null null width null)
                                       (- (unbox width)
                                          extra-space))]
                         [snip-width (- view-width snip-x-pos&margins)])
                    (send s set-min-width snip-width)
                    (send s set-max-width snip-width)
                    (when (is-a? s editor-snip%)
                      (let ([snip-media (send s get-this-media)])
                        (unless (null? snip-media)
                          (send snip-media set-max-width
                                (if autowrap-snips?
                                    snip-width
                                    0))))))
                  (send (send s get-admin) resized s #t)))])
         (public
           [set-media
            (lambda (x)
              (super-set-media x)
              (send x on-set-media this))]))
      
  ;; this sends a message to it's frame when it gets the focus
      (define make-searchable-canvas%
        (lambda (%)
          (class100-asi %
            (inherit get-top-level-window)
            (rename [super-on-focus on-focus])
            (override
              [on-focus
               (lambda (on?)
                 (when on?
                   (send (get-top-level-window) make-searchable this))
                 (super-on-focus on?))]))))
      
      (define interactions-canvas% (make-searchable-canvas%
                                    (canvas:info-mixin
                                     canvas:wide-snip%)))
      
      (define definitions-canvas%
        (class100 (make-searchable-canvas% canvas:info%) args
          (sequence
            (apply super-init args))))
      
      (define (program-editor-mixin text%)
        (class text%
	  (init-rest args)
          (override after-insert after-delete)
	  (inherit get-top-level-window)
	  (rename [super-after-insert after-insert]
		  [super-after-delete after-delete])
          
          (define (reset-highlighting)
            (let ([f (get-top-level-window)])
              (when f
                (let ([interactions-text (ivar f interactions-text)])
                  (when (object? interactions-text)
                    (let ([reset (ivar interactions-text reset-highlighting)])
                      (when (procedure? reset)
                        (reset))))))))
          
          (define (after-insert x y)
            (reset-highlighting)
            (super-after-insert x y))
          
          (define (after-delete x y)
            (reset-highlighting)
            (super-after-delete x y))
          
          (apply super-init args)))
      
      (define definitions-super%
        (program-editor-mixin
         (scheme:text-mixin
          (drscheme:rep:drs-bindings-keymap-mixin
           text:info%))))
      
      (define definitions-text%
        (class100 definitions-super% args
          
          (public
            [clear-annotations
             (lambda ()
               (void))])
          
          (inherit get-top-level-window)
          (private
            [reset-highlighting
             (lambda ()
               (let ([f (get-top-level-window)])
                 (when f
                   (let ([interactions-text (ivar f interactions-text)])
                     (when (object? interactions-text)
                       (let ([reset (ivar interactions-text reset-highlighting)])
                         (when (procedure? reset)
                           (reset))))))))])
          (rename [super-on-insert on-insert]
                  [super-on-delete on-delete])
          (override
            [on-insert
             (lambda (x y)
               (reset-highlighting)
               (super-on-insert x y))]
            [on-delete
             (lambda (x y)
               (reset-highlighting)
               (super-on-delete x y))])
          
          (rename
           [super-set-modified set-modified]
           [super-set-filename set-filename])
          (inherit is-modified? run-after-edit-sequence)
          (override
            [set-modified
             (lambda (mod?)
               (super-set-modified mod?)
               (run-after-edit-sequence
                (lambda ()
                  (let ([f (get-top-level-window)])
                    (when f
                      (send f update-save-button (is-modified?)))))))]
            [set-filename
             (case-lambda
              [(fn) (set-filename fn #f)]
              [(fn tmp?)
               (let ([f (get-top-level-window)])
                 (when f
                   (send f update-save-message fn)))
               (super-set-filename fn tmp?)])])
          
          (rename [super-after-insert after-insert]
                  [super-after-delete after-delete])
          (private
            [needs-execution-state #f]
            [already-warned-state #f]
            [execute-language (preferences:get drscheme:language:settings-preferences-symbol)])
          (public
            [needs-execution? 
             (lambda ()
               (or needs-execution-state
                   (not (equal? execute-language
                                (preferences:get drscheme:language:settings-preferences-symbol)))))]
            [teachpack-changed
             (lambda ()
               (set! needs-execution-state #t))]
            [just-executed
             (lambda ()
               (set! execute-language (preferences:get drscheme:language:settings-preferences-symbol))
               (set! needs-execution-state #f)
               (set! already-warned-state #f))]
            [already-warned? (lambda () already-warned-state)]
            [already-warned
             (lambda ()
               (set! already-warned-state #t))])
          (override
            [after-insert
             (lambda x
               (set! needs-execution-state #t)
               (apply super-after-insert x))]
            [after-delete
             (lambda x
               (set! needs-execution-state #t)
               (apply super-after-delete x))])
          
          (inherit get-filename)
          (private-field
            [tmp-date-string #f])
	  (private
            [get-date-string
             (lambda ()
               (string-append
                (mzlib:date:date->string (seconds->date (current-seconds)))
                " "
                (let ([fn (get-filename)])
                  (if (string? fn)
                      fn
                      "Untitled"))))])
          
          (rename [super-on-paint on-paint])
          (override
            [on-paint
             (lambda (before dc left top right bottom dx dy draw-caret)
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
                 (void)))])
          (sequence
            (apply super-init args))))
      
      (define func-defs-canvas%
        (class canvas%
	  (init parent)
	  (init-field text)
          (override on-paint on-event)
	  (inherit get-client-size get-dc popup-menu min-height min-width
		   stretchable-width
		   stretchable-height)
	  (rename [super-on-event on-event])
          
          (define-struct defn (indent name start-pos end-pos))
          
          (define tag-string "(define")
          (define (get-definitions)
            (let* ([min-indent 0]
                   [defs (let loop ([pos 0])
                           (let ([defn-pos (send text find-string tag-string 'forward pos 'eof #t #f)])
                             (if defn-pos
                                 (let ([indent (get-defn-indent defn-pos)]
                                       [name (get-defn-name (+ defn-pos (string-length tag-string)))])
                                   (set! min-indent (min indent min-indent))
                                   (cons (make-defn indent name defn-pos defn-pos)
                                         (loop (+ defn-pos (string-length tag-string)))))
                                 null)))])
              
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
              
              (unless sort-by-name?
                (for-each (lambda (defn)
                            (set-defn-name! defn
                                            (string-append
                                             (apply string
                                                    (vector->list
                                                     (make-vector (- (defn-indent defn) min-indent) #\space)))
                                             (defn-name defn))))
                          defs))
              (if sort-by-name?
                  (mzlib:list:quicksort 
                   defs
                   (lambda (x y) (string-ci<=? (defn-name x) (defn-name y))))
                  defs)))
          
          (define (get-defn-indent pos)
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
          
          (define (skip-to-whitespace/paren pos)
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
          
          (define (skip-whitespace/paren pos)
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
          
          (define (get-defn-name define-pos)
            (if (>= define-pos (send text last-position))
                "<end of buffer>"
                (let* ([start-pos (skip-whitespace/paren (skip-to-whitespace/paren define-pos))]
                       [end-pos (skip-to-whitespace/paren start-pos)])
                  (send text get-text start-pos end-pos))))
          
          (define inverted? #f)
          
          (define label "(define ...)")
          
          (define (on-paint)
            (let ([dc (get-dc)])
              (let-values ([(w h) (get-client-size)])
                (drscheme:frame:draw-button-label dc label w h inverted?))))
          
          (define sort-by-name? #f)
          (define sorting-name "Sort by name")
          (define (change-sorting-order)
            (set! sort-by-name? (not sort-by-name?))
            (set! sorting-name (if sort-by-name? "Sort by position in file" "Sort by name"))
            (void))
          
          (define (on-event evt)
            (cond
              [(send evt button-down?)
               (set! inverted? #t)
               (on-paint)
               (let ([menu (make-object popup-menu% #f
                             (lambda x
                               (set! inverted? #f)
                               (on-paint)))]
                     [defns (get-definitions)])
                 (make-object menu:can-restore-menu-item% sorting-name
                   menu
                   (lambda x
                     (change-sorting-order)))
                 (make-object separator-menu-item% menu)
                 (if (null? defns)
                     (send (make-object menu:can-restore-menu-item%
                             "<< no definitions found >>"
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
          
          (super-init parent)
          
          (define-values (width height) (drscheme:frame:calc-button-min-sizes (get-dc) label))
          (min-width width)
          (min-height height)
          (stretchable-width #f)
          (stretchable-height #f)))
      
      (define (set-box/f! b v) (when (box? b) (set-box! b v)))
      
      (define super-frame%
        (drscheme:frame:mixin
         (drscheme:frame:basics-mixin frame:searchable%)))
      
      (define vertical-resizable/pref%
        (class100 panel:vertical-resizable% (_unit-frame . args)
	  (private-field
	    [unit-frame _unit-frame])
          (inherit get-percentages)
          (override
            [on-percentage-change
             (lambda ()
               (let ([percentages (get-percentages)])
                 (when (and (= 1
                               (length (ivar unit-frame definitions-canvases))
                               (length (ivar unit-frame interactions-canvases)))
                            (= 2 (length percentages)))
                   (preferences:set 'drscheme:unit-window-size-percentage (car percentages)))))])
          (sequence (apply super-init args))))
      
      (define frame%
        (class100* super-frame% (drscheme:rep:context<%>) (filename)
          (inherit set-label-prefix show-menu
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
          
          (rename [super-update-shown update-shown]
                  [super-on-close on-close])
          (public
            [get-directory
             (lambda ()
               (let ([filename (send definitions-text get-filename)])
                 (if (string? filename)
                     (let-values ([(base _1 _2) (split-path (mzlib:file:normalize-path filename))])
                       base)
                     #f)))]
            [needs-execution?
             (lambda ()
               (send definitions-text needs-execution?))])
          
          (public
            [clear-annotations
             (lambda ()
               (send definitions-text clear-annotations))])
          
          (public-field
            [definitions-item #f]
            [interactions-item #f]
            ;[imports-id #f]
            
            [name-message #f]
            [save-button #f]
            [save-init-shown? #f])
	  (public
            [set-save-init-shown? (lambda (x) (set! save-init-shown? x))])
          
          (public-field
            [canvas-show-mode #f]
            [allow-split? #f]
            [forced-quit? #f])
          
          (private-field
            [search-canvas #f])
          (public
            [make-searchable
             (lambda (canvas)
               (update-info)
               (set! search-canvas canvas))])
          (override
            [get-text-to-search
             (lambda ()
               (if search-canvas
                   (send search-canvas get-editor)
                   (get-editor)))])
          
          (private-field
	    [was-locked? #f]
	    [execute-menu-item #f])
          (public
            [disable-evaluation
             (lambda ()
               (when execute-menu-item
                 (send execute-menu-item enable #f))
               (send execute-button enable #f)
               (send definitions-text lock #t)
               (send interactions-text lock #t))]
            [enable-evaluation
             (lambda ()
               (when execute-menu-item
                 (send execute-menu-item enable #t))
               (send execute-button enable #t)
               (send definitions-text lock #f)
               (unless (send interactions-text eval-busy?)
                 (send interactions-text lock #f)))])
          
          (inherit set-label)
          (public
            [update-save-button
             (lambda (mod?)
               (if save-button
                   (unless (eq? mod? (send save-button is-shown?))
                     (send save-button show mod?))
                   (set! save-init-shown? mod?)))]
            [update-save-message
             (lambda (name)
               (when name-message
                 (send name-message set-message #t name)))])
          (override
            [get-canvas% (lambda () (drscheme:get/extend:get-definitions-canvas%))])
          (public
            [ensure-defs-shown
             (lambda ()
               (when (hidden? definitions-item)
                 (toggle-show/hide definitions-item)
                 (update-shown)))]
            [ensure-rep-shown
             (lambda ()
               (when (hidden? interactions-item)
                 (toggle-show/hide interactions-item)
                 (update-shown)))])
          
          (override
            [get-editor% (lambda () (drscheme:get/extend:get-definitions-text%))])
          (public
            [still-untouched?
             (lambda ()
               (and (= (send definitions-text last-position) 0)
                    (not (send definitions-text is-modified?))
                    (not (send definitions-text get-filename))))]
            [change-to-file
             (lambda (name)
               (cond
                 [(and name (file-exists? name))
                  (send definitions-text load-file name)]
                 [name
                  (send definitions-text set-filename name)]
                 [else (send definitions-text clear)])
               (send definitions-canvas focus))])
          
	  (private
            [hidden?
             (lambda (item)
               (let ([label (send item get-label)])
                 (and (string? label)
                      (>= (string-length label) 4)
                      (string=? (substring label 0 4) "Show"))))]
            [save-as-text-from-text
             (lambda (text)
               (let ([file (parameterize ([finder:dialog-parent-parameter this])
                             (finder:put-file))])
                 (when file
                   (send text save-file file 'text))))])
          
          (public
            [toggle-show/hide
             (lambda (item)
               (let ([label (send item get-label)])
                 (when (and (string? label)
                            (>= (string-length label) 4))
                   (let ([new-front
                          (if (string=? "Hide" (substring label 0 4))
                              "Show"
                              "Hide")]
                         [back (substring label 4 (string-length label))])
                     (send item set-label (string-append new-front back))))))])
          
          (private
            [file-menu:print-transcript-item #f])
          
          (rename
           [super-file-menu:between-open-and-revert file-menu:between-open-and-revert])
          (override
            [file-menu:between-open-and-revert
             (lambda (file-menu)
               (super-file-menu:between-open-and-revert file-menu)
               (make-object separator-menu-item% file-menu))]
            [file-menu:save-string (lambda () "Definitions")]
            [file-menu:save-as-string (lambda () "Definitions")]
            [file-menu:between-save-as-and-print
             (lambda (file-menu)
               (let ([sub-menu (make-object menu% "Save Other" file-menu)])
                 (make-object menu:can-restore-menu-item%
                   "Save Definitions As Text..."
                   sub-menu
                   (lambda (_1 _2)
                     (save-as-text-from-text definitions-text)))
                 (make-object menu:can-restore-menu-item%
                   "Save Interactions"
                   sub-menu
                   (lambda (_1 _2) (send interactions-text save-file)))
                 (make-object menu:can-restore-menu-item%
                   "Save Interactions As..."
                   sub-menu
                   (lambda (_1 _2) 
                     (let ([file (parameterize ([finder:dialog-parent-parameter this])
                                   (finder:put-file))])
                       (when file
                         (send interactions-text save-file 
                               file 'standard)))))
                 (make-object menu:can-restore-menu-item%
                   "Save Interactions As Text..."
                   sub-menu
                   (lambda (_1 _2)
                     (save-as-text-from-text interactions-text)))
                 ;	   (make-object separator-menu-item% file-menu)
                 ;	   (make-object menu:can-restore-menu-item%
                 ;	     "Show Interactions History"
                 ;	     file-menu
                 ;	     (lambda (_1 _2)
                 ;	       (drscheme:rep:show-interactions-history)))
                 (make-object separator-menu-item% file-menu)))]
            [file-menu:print-string (lambda () "Definitions")]
            [file-menu:between-print-and-close
             (lambda (file-menu)
               (set! file-menu:print-transcript-item
                     (make-object menu:can-restore-menu-item%
                       "Print Interactions..."
                       file-menu
                       (lambda (_1 _2)
                         (send interactions-text print
                               #t 
                               #t
                               (preferences:get 'framework:print-output-mode)))))
               (make-object separator-menu-item% file-menu))])
          
          (inherit get-edit-target-window)
          (private
            [split
             (lambda ()
               (let* ([target (get-edit-target-window)]
                      [update
                       (lambda (set-canvases! canvases canvas% text)
                         (let ([orig-percentages (send resizable-panel get-percentages)]
                               [new-canvas (make-object canvas% resizable-panel text)])
                           
                           (send new-canvas focus)
                           
                           (send resizable-panel set-percentages
                                 (let loop ([canvases (append definitions-canvases
                                                              interactions-canvases)]
                                            [percentages orig-percentages])
                                   (cond
                                     [(null? canvases)
                                      (error 'split "couldn't split; didn't find canvas")]
                                     [(null? percentages)
                                      (error 'split "wrong number of percentages: ~s ~s"
                                             orig-percentages
                                             (append definitions-canvases interactions-canvases))]
                                     [else (let ([canvas (car canvases)])
                                             (if (eq? target canvas)
                                                 (list* (/ (car percentages) 2)
                                                        (/ (car percentages) 2)
                                                        (cdr percentages))
                                                 (cons
                                                  (car percentages)
                                                  (loop (cdr canvases)
                                                        (cdr percentages)))))])))
                           
                           (set-canvases!
                            (let loop ([canvases canvases])
                              (cond
                                [(null? canvases) (error 'split "couldn't split; didn't find canvas")]
                                [else
                                 (let ([canvas (car canvases)])
                                   (if (eq? canvas target)
                                       (list* new-canvas
                                              canvas
                                              (cdr canvases))
                                       (cons canvas (loop (cdr canvases)))))])))
                           
                           (send resizable-panel change-children
                                 (lambda (l)
                                   (append definitions-canvases interactions-canvases)))))])
                 (cond
                   [(memq target definitions-canvases)
                    (update (lambda (x) (set! definitions-canvases x))
                            definitions-canvases
                            definitions-canvas%
                            definitions-text)]
                   [(memq target interactions-canvases)
                    (update (lambda (x) (set! interactions-canvases x))
                            interactions-canvases
                            interactions-canvas%
                            interactions-text)]
                   [else (bell)])))]
            [collapse (lambda ()
                        (let* ([target (get-edit-target-window)]
                               [handle-collapse
                                (lambda (get-canvases set-canvases!)
                                  (if (= 1 (length (get-canvases)))
                                      (bell)
                                      (let* ([old-percentages (send resizable-panel get-percentages)]
                                             [percentages
                                              (if (eq? (car (get-canvases)) target)
                                                  (cons (+ (car old-percentages)
                                                           (cadr old-percentages))
                                                        (cddr old-percentages))
                                                  (let loop ([canvases (get-canvases)]
                                                             [prev-percentage
                                                              (car old-percentages)]
                                                             [percentages (cdr old-percentages)])
                                                    (cond
                                                      [(null? canvases)
                                                       (error 'collapse "internal error.1")]
                                                      [(null? percentages)
                                                       (error 'collapse "internal error.2")]
                                                      [else
                                                       (if (eq? (car canvases) target)
                                                           (if prev-percentage
                                                               (cons (+ (car percentages)
                                                                        prev-percentage)
                                                                     (cdr percentages))
                                                               (cons (+ (car percentages)
                                                                        (cadr percentages))
                                                                     (cddr percentages)))
                                                           (cons prev-percentage
                                                                 (loop (cdr canvases)
                                                                       (car percentages)
                                                                       (cdr percentages))))])))])
                                        (set-canvases!
                                         (mzlib:list:remq target (get-canvases)))
                                        (send resizable-panel change-children
                                              (lambda (l)
                                                (append definitions-canvases
                                                        interactions-canvases)))
                                        (send resizable-panel set-percentages percentages)
                                        (send (car (get-canvases)) focus))))])
                          (cond
                            [(memq target definitions-canvases)
                             (handle-collapse
                              (lambda () definitions-canvases)
                              (lambda (c) (set! definitions-canvases c)))]
                            [(memq target interactions-canvases)
                             (handle-collapse
                              (lambda () interactions-canvases)
                              (lambda (c) (set! interactions-canvases c)))]
                            [else (bell)])))])
          (rename [super-edit-menu:between-select-all-and-find
                   edit-menu:between-select-all-and-find])
          (override
            [edit-menu:between-select-all-and-find
             (lambda (edit-menu)
               (super-edit-menu:between-select-all-and-find edit-menu)
               (make-object menu-item% "&Split" edit-menu (lambda x (split)))
               (make-object menu-item% "C&ollapse" edit-menu (lambda x (collapse)))
               (make-object separator-menu-item% edit-menu))])
          
          (rename [super-add-edit-menu-snip-items add-edit-menu-snip-items])
          (inherit get-menu-item%)
          (override
            [add-edit-menu-snip-items
             (lambda (edit-menu)
               (super-add-edit-menu-snip-items edit-menu)
               (let ([c% (class100 (get-menu-item%) args
                           (inherit enable)
                           (rename [super-on-demand on-demand])
                           (override
                             [on-demand
                              (lambda ()
                                (let ([edit (get-edit-target-object)])
                                  (enable (and edit (is-a? edit editor<%>)))))])
                           (sequence (apply super-init args)))])
                 (make-object c% "Insert Fraction..." edit-menu
                   (lambda (menu evt)
                     (let ([edit (get-edit-target-object)])
                       (when (and edit
                                  (is-a? edit editor<%>))
                         (let ([number (get-fraction-from-user)])
                           (when number
                             (send edit insert
                                   (make-object drscheme:snip:whole/part-number-snip%
                                     number)))))
                       #t)))))])
          (private
            [item->children
             (lambda (item)
               (cond
                 [(eq? item interactions-item) interactions-canvases]
                 [(eq? item definitions-item) definitions-canvases]
                 [else (error 'item->children "unknown item: ~a" item)]))])
          (private
            [get-sub-items
             (lambda ()
               (list interactions-item definitions-item
                     ;imports-item
                     ))]
            [update-shown/ensure-one
             (lambda (last-one)
               (when (andmap hidden? (get-sub-items))
                 (toggle-show/hide last-one))
               (update-shown))])
          (override
            [update-shown
             (lambda ()
               (super-update-shown)
               (send resizable-panel change-children
                     (lambda (l)
                       top-panel
                       (mzlib:list:foldl
                        (lambda (item sofar)
                          (if (hidden? item)
                              sofar
                              (append (item->children item) sofar)))
                        null
                        (get-sub-items))))
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
               
               
               (let ([defs-show? (not (hidden? definitions-item))])
                 (for-each
                  (lambda (get-item)
                    (send (get-item) enable defs-show?))
                  (list file-menu:get-revert-item
                        file-menu:get-save-item
                        file-menu:get-save-as-item
                        ;file-menu:save-as-text-item ; Save As Text...
                        file-menu:get-print-item)))
               (send file-menu:print-transcript-item enable
                     (not (hidden? interactions-item))))]
            
            [on-close
             (lambda ()
               (remove-teachpack-callback)
               (when (eq? this created-frame)
                 (set! created-frame #f))
               (send interactions-text shutdown)
               (send interactions-text on-close)
               (super-on-close))])
          
          (public-field
            [running? #t])
	  (public
            [execute-callback
             (lambda ()
               (cond
                 [(send definitions-text save-file-out-of-date?)
                  (message-box 
                   "DrScheme"
                   "The definitions text has been modified in the file-system; please save or revert the definitions text.")]
                 [else
                  (ensure-rep-shown)
                  (send definitions-text just-executed)
                  (send interactions-canvas focus)
                  (send interactions-text reset-console)
                  (send interactions-text clear-undos)
                  
                  (let ([start (if (and (>= (send definitions-text last-position) 2)
                                        (char=? (send definitions-text get-character 0) #\#)
                                        (char=? (send definitions-text get-character 1) #\!))
                                   (send definitions-text paragraph-start-position 1)
                                   0)])
                    (send interactions-text do-many-text-evals
                          definitions-text start
                          (send definitions-text last-position)))
                  (send interactions-text clear-undos)]))])
          
          (public
            [after-change-name (lambda () (void))])
          
          (inherit get-menu-bar get-focus-object get-edit-target-object)
          (private-field
            [language-menu 'uninited-language-menu])
          
          (rename [super-on-size on-size])
          (override
            [on-size
             (lambda (w h)
               (preferences:set 'drscheme:unit-window-width w)
               (preferences:set 'drscheme:unit-window-height h)
               (super-on-size w h))])
          
          (override
            [get-editor (lambda () definitions-text)]
            [get-canvas (lambda () definitions-canvas)])
          
          (public-field
            [definitions-text (make-object (drscheme:get/extend:get-definitions-text%))]
            [interactions-text (make-object 
                                   (drscheme:get/extend:get-interactions-text%)
                                 this)])
          
          (sequence
            (super-init filename
                        #f
                        (preferences:get 'drscheme:unit-window-width)
                        (preferences:get 'drscheme:unit-window-height))
            
            (let* ([mb (get-menu-bar)]
                   [_ (set! language-menu (make-object (get-menu%) "&Language" mb))]
                   [scheme-menu (make-object (get-menu%) "S&cheme" mb)]
                   [send-method
                    (lambda (method)
                      (lambda (_1 _2)
                        (let ([text (get-focus-object)])
                          (when (or (eq? text definitions-text)
                                    (eq? text interactions-text))
                            ((ivar/proc text method))))))])
              
              (drscheme:language:fill-language-menu this language-menu)
              
              (set! execute-menu-item
                    (make-object menu:can-restore-menu-item%
                      "Execute"
                      scheme-menu
                      (lambda (_1 _2) (execute-callback))
                      #\t
                      "Restart the program in the definitions window"))
              (make-object menu:can-restore-menu-item%
                "Break"
                scheme-menu
                (lambda (_1 _2) (send interactions-text break))
                #\b
                "Break the current evaluation")
              (make-object menu:can-restore-menu-item%
                "Kill"
                scheme-menu
                (lambda (_1 _2) (send interactions-text kill-evaluation))
                #\k
                "Kill the current evaluation")
              (make-object separator-menu-item% scheme-menu)
              (make-object menu:can-restore-menu-item% "Create Launcher..." scheme-menu (lambda x (create-launcher this)))
              (make-object separator-menu-item% scheme-menu)
              (make-object menu:can-restore-menu-item%
                "&Reindent"
                scheme-menu
                (send-method 'tabify-selection))
              (make-object menu:can-restore-menu-item%
                "Reindent &All"
                scheme-menu
                (send-method 'tabify-all)
                #\i)
              (make-object menu:can-restore-menu-item%
                "&Comment Out"
                scheme-menu
                (send-method 'comment-out-selection))
              (make-object menu:can-restore-menu-item%
                "&Uncomment"
                scheme-menu
                (send-method 'uncomment-selection)))
            
            (frame:reorder-menus this)
            
            (set! definitions-item
                  (make-object menu:can-restore-menu-item%
                    "Hide &Definitions"
                    show-menu
                    (lambda (_1 _2) 
                      (toggle-show/hide definitions-item)
                      (update-shown/ensure-one interactions-item))
                    #\d
                    "Show/Hide the definitions window"))
            (set! interactions-item
                  (make-object menu:can-restore-menu-item%
                    "Show &Interactions"
                    show-menu
                    (lambda (_1 _2) 
                      (toggle-show/hide interactions-item)
                      (update-shown/ensure-one definitions-item))
                    #\e
                    "Show/Hide the interactions window")))
          (private-field
            [top-panel (make-object horizontal-panel% (get-area-container))]
            [name-panel (make-object vertical-panel% top-panel)]
            [resizable-panel (make-object vertical-resizable/pref% this (get-area-container))])
          (sequence
            (send name-panel stretchable-width #f)
            (send name-panel stretchable-height #f))
          
          (public-field
            [definitions-canvas (make-object (drscheme:get/extend:get-definitions-canvas%)
                                  resizable-panel)]
            [definitions-canvases (list definitions-canvas)]
            [interactions-canvas (make-object (drscheme:get/extend:get-interactions-canvas%)
                                   resizable-panel)]
            [interactions-canvases (list interactions-canvas)])
          
          (sequence
            (send interactions-text auto-wrap #t)
            (send interactions-canvas set-editor interactions-text)
            (send definitions-canvas set-editor definitions-text)
            
            (set! save-button
                  (make-object button% 
                    (make-save-bitmap this)
                    top-panel
                    (lambda args
                      (let* ([text definitions-text])
                        (when text
                          (send text save-file)
                          (send definitions-canvas focus))))))
            
            (set! name-message (make-object drscheme:frame:name-message% name-panel)))
          (private-field
            [teachpack-items null])
	  (private
            [update-teachpack-menu
             (lambda (names)
               (for-each (lambda (item) (send item delete)) teachpack-items)
               (set! teachpack-items
                     (map (lambda (name)
                            (make-object menu:can-restore-menu-item%
                              (format "Clear ~a Teachpack" (mzlib:file:file-name-from-path name))
                              language-menu
                              (lambda (item evt)
                                (preferences:set
                                 'drscheme:teachpack-file
                                 (mzlib:list:remove
                                  name
                                  (preferences:get 'drscheme:teachpack-file))))))
                          names)))])
          (sequence
            (update-teachpack-menu
             (preferences:get 'drscheme:teachpack-file)))
          
          (public-field
            [stop-execute-button (void)]
            [execute-button (void)]
            [button-panel (make-object horizontal-panel% top-panel)])
          
          (private-field
            [func-defs-canvas (make-object func-defs-canvas% name-panel definitions-text)])
          
          (sequence
            (set! execute-button
                  (make-object button%
                    (make-execute-bitmap this)
                    button-panel
                    (lambda (button evt) (execute-callback))))
            (set! stop-execute-button
                  (make-object button%
                    (make-break-bitmap this) 
                    button-panel
                    (lambda args
                      (send interactions-text break)
                      (ensure-rep-shown)
                      (send (send interactions-text get-canvas) focus))))
            (send button-panel stretchable-height #f)
            (send button-panel stretchable-width #f) 
            
            (send top-panel change-children
                  (lambda (l)
                    (list name-panel save-button
                          (make-object vertical-panel% top-panel) ;; spacer
                          button-panel)))
            (send top-panel stretchable-height #f))
          
          (private-field
            [remove-teachpack-callback
             (preferences:add-callback
              'drscheme:teachpack-file
              (lambda (p v)
                (update-teachpack-menu v)
                (send definitions-text teachpack-changed)))])
          
          (inherit get-label)
          (sequence
            
            
            (let ([m (send definitions-canvas get-editor)])
              (set-save-init-shown?
               (and m (send m is-modified?))))
            
	;; (get-label) shouldn't be #f, but I'm not sure....
            (send name-message set-message
                  (if filename #t #f)
                  (or filename (get-label) "Untitled"))
            
            (update-save-button #f)
            
            (send interactions-text initialize-console)
            
            (unless filename
              (toggle-show/hide interactions-item))
            
            (update-shown)
            
            (send resizable-panel set-percentages
                  (let ([p (preferences:get 'drscheme:unit-window-size-percentage)])
                    (list p (- 1 p))))
            
            (set-label-prefix "DrScheme")
            
            (send definitions-canvas focus)
            (cond
              [(eq? created-frame 'nothing-yet)
               (set! created-frame this)]
              [created-frame
               (set! created-frame #f)]
              [else (void)]))))
      
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
              (let* ([frame% (drscheme:get/extend:get-unit-frame%)]
                     [frame (make-object frame% name)])
                (send frame show #t)
                frame))]))
      
      (handler:insert-format-handler 
       "Units"
       (lambda (filename) #t)
       open-drscheme-window))))

#|
;; old lambda snipclass
;; nixed becuase of poor support from reader

(make-object separator-menu-item% scheme-menu)
(make-object menu:can-restore-menu-item%
  "Insert &Lambda"
  scheme-menu
  (lambda x (let ([editor (get-edit-target-object)])
              (when editor
                (send editor insert (make-object lambda-snip%)))))
  #\l)


(define lambda-snipclass
  (make-object (class snip-class% ()
                 (override
                   [read
                    (lambda (p)
                      (make-object lambda-snip%))])
                 (sequence
                   (super-init)))))
(send lambda-snipclass set-version 1)
(send lambda-snipclass set-classname "lambda-snip%")
(send (get-the-snip-class-list) add lambda-snipclass)

(define lambda-snip% 
  (class* snip% (gui-utils:text-snip<%>) ()
    (private
      [get-normal-font
       (lambda ()
         (send the-font-list find-or-create-font
               (preferences:get 'drscheme:font-size)
               'modern 'normal 'normal #f))]
      [get-lambda-font
       (lambda ()
         (send the-font-list find-or-create-font 
               (preferences:get 'drscheme:font-size)
               'symbol 'normal 'normal #f))])
    (public
      [get-string
       (lambda () "lambda")])
    (override
      [get-text
       (case-lambda
        [(x y) " lambda "]
        [(x y z) " lambda "])]
      [copy
       (lambda ()
         (make-object lambda-snip%))]
      [write
       (lambda (p)
         (void))]
      [get-extent
       (lambda (dc x y wb hb descentb spaceb lspaceb rspaceb)
         (let-values ([(w h d s) (send dc get-text-extent "W" (get-normal-font))])
           (set-box/f! wb w)
           (set-box/f! hb h)
           (set-box/f! descentb d)
           (set-box/f! spaceb s)
           (set-box/f! lspaceb 0)
           (set-box/f! rspaceb 0)))]
      [draw
       (lambda (dc x y left top right bottom dx dy draw-caret)
         (let ([font (send dc get-font)])
           (let-values ([(ww wh wd ws) (send dc get-text-extent "W" (get-normal-font))])
             (send dc set-font (get-lambda-font))
             (let-values ([(lw lh ld ls) (send dc get-text-extent "l")])
               (send dc draw-text "l" 
                     (+ x (/ (- ww lw) 2))
                     (+ y (- (- wh wd) (- lh ld)))))
             (send dc set-font font))))])
    (inherit set-snipclass)
    (sequence
      (super-init)
      (set-snipclass lambda-snipclass))))
|#
