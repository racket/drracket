; =Kernel= means in DrScheme's thread and parameterization
; 
; =User= means the user's thread and parameterization
; 
; =Handler= means in the handler thread of some eventspace; it must
;  be combined with either =Kernel= or =User=


(module rep mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "class100.ss")
           (lib "list.ss")
           "drsig.ss"
           "string-constant.ss"
           (lib "etc.ss")
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix mzlib:pretty-print: (lib "pretty.ss"))
           (prefix print-convert: (lib "pconvert.ss")))  
  
  (provide rep@)
  
  (define rep@
    (unit/sig drscheme:rep^
      (import (drscheme:init : drscheme:init^)
              (drscheme:snip : drscheme:snip^)
              (drscheme:language : drscheme:language^)
              (drscheme:app : drscheme:app^)
              (drscheme:frame : drscheme:frame^)
              (drscheme:unit : drscheme:unit^)
              (drscheme:text : drscheme:text^)
              (drscheme:load-handler : drscheme:load-handler^)
              (drscheme:help : drscheme:help-interface^))
      
      (define sized-snip<%>
	(interface ((class->interface snip%))
	  ;; get-character-width : -> number
	  ;; returns the number of characters wide the snip is,
	  ;; for use in pretty printing the snip.
	  get-character-width))

      (define (format-source-loc x) (format "~s" x))
      
      (define primitive-eval (current-eval))
      
      ;; current-language-settings : (parameter language-setting)
      ;; set to the current language and its setting on the user's thread.
      (define current-language-settings (make-parameter #f))
      
      ;; a port that accepts values for printing in the repl
      (define current-value-port (make-parameter 'uninitialized-value-port))
            
      ;; bottom-escape-handler : (parameter ( -> A))
      ;; must escape. this is called if the user's error-escape-handler doesn't escape
      (define bottom-escape-handler (make-parameter void))

      ;; (parameter (string (union #f syntax) exn -> void))
      (define error-display/debug-handler
        (make-parameter
         (lambda (msg debug exn)
           ((error-display-handler) msg)
           (if (syntax? debug)
               (string-append (format-source-loc debug debug)
                              msg)
               msg))))
      
      ;; drscheme-error-value->string-handler : TST number -> string
      (define (drscheme-error-value->string-handler x n)
        (let ([port (open-output-string)])
          
          ;; passing this port here means no snips allowed,
          ;; even though this string may eventually end up
          ;; displayed in a place where snips are allowed.
          (print x port)
          
          (let* ([long-string (get-output-string port)])
            (close-output-port port)
            (if (<= (string-length long-string) n)
                long-string
                (let ([short-string (substring long-string 0 n)]
                      [trim 3])
                  (unless (n . <= . trim)
                    (let loop ([i trim])
                      (unless (i . <= . 0)
                        (string-set! short-string (- n i) #\.)
                        (loop (sub1 i)))))
                  short-string)))))

      ;; drscheme-exception-handler : exn -> A
      ;; effect: displays the exn-message and escapes
      (define (drscheme-exception-handler exn)
        (let ([dh (error-display/debug-handler)])
          (if (exn? exn)
              (let* ([marks (exn-continuation-marks exn)])
                (dh (format "~a" (exn-message exn)) #f exn))
              (dh (format "uncaught exception: ~e" exn) #f #f)))
        ((error-escape-handler))
        ((error-display-handler) "Exception handler did not escape")
        ((bottom-escape-handler)))
      
      (define raw-symbol-chars "a-z/!>:%\\+\\*\\?-")
      (define symbol-chars (format "[~a]" raw-symbol-chars))
      (define not-symbol-chars (format "[^~a]" raw-symbol-chars))
      (define fallthru-regexp-str (format "^()(~a*): " symbol-chars))
      (define fallthru-regexp (regexp fallthru-regexp-str))
      (define class-regexp-str (format "^(.*~a)(~a+(<%>|%)).*$" not-symbol-chars symbol-chars))
      (define class-regexp (regexp class-regexp-str))
      (define ivar-regexp-str (format
                               "^(ivar: instance variable not found: )(~a*)"
                               symbol-chars))
      (define ivar-regexp (regexp ivar-regexp-str))
      
      (define drs-bindings-keymap (make-object keymap%))
      (send drs-bindings-keymap add-function
            "execute"
            (lambda (obj evt)
              (when (is-a? obj editor<%>)
                (let ([canvas (send obj get-canvas)])
                  (when canvas
                    (let ([frame (send canvas get-top-level-window)])
                      (when (is-a? frame drscheme:unit:frame%)
                        (send frame execute-callback))))))))
      (send drs-bindings-keymap add-function
            "toggle-focus-between-definitions-and-interactions"
            (lambda (obj evt)
              (when (is-a? obj editor<%>)
                (let ([canvas (send obj get-canvas)])
                  (when canvas
                    (let ([frame (send canvas get-top-level-window)])
                      (when (is-a? frame drscheme:unit:frame%)
                        (cond
                          [(send (send frame get-definitions-canvas) has-focus?)
                           (send (send frame get-interactions-canvas) focus)]
                          [else
                           (send (send frame get-definitions-canvas) focus)]))))))))
      
      (send drs-bindings-keymap map-function "c:x;o" "toggle-focus-between-definitions-and-interactions")
      (send drs-bindings-keymap map-function "f5" "execute")
      
  ;; drs-bindings-keymap-mixin :
  ;;   ((implements editor:keymap<%>) -> (implements editor:keymap<%>))
  ;;   for any x that is an instance of the resulting class,
  ;;     (is-a? (send (send x get-canvas) get-top-level-frame) drscheme:unit:frame%)
      (define (drs-bindings-keymap-mixin editor%)
        (class100 editor% args
          (rename [super-get-keymaps get-keymaps])
          (override
            [get-keymaps
             (lambda ()
               (cons drs-bindings-keymap (super-get-keymaps)))])
          (sequence (apply super-init args))))
      
  ;; Max length of output queue (user's thread blocks if the
  ;; queue is full):
      (define output-limit-size 2000)
      
      (define (printf . args) (apply fprintf drscheme:init:original-output-port args))
      
      (define setup-scheme-interaction-mode-keymap
        (lambda (keymap)
          (send keymap add-function "put-previous-sexp"
                (lambda (text event) 
                  (send text copy-prev-previous-expr)))
          (send keymap add-function "put-next-sexp"
                (lambda (text event) 
                  (send text copy-next-previous-expr)))
          
          (keymap:send-map-function-meta keymap "p" "put-previous-sexp")
          (keymap:send-map-function-meta keymap "n" "put-next-sexp")))
      
      (define scheme-interaction-mode-keymap (make-object keymap%))
      (setup-scheme-interaction-mode-keymap scheme-interaction-mode-keymap)
      
      (define drs-font-delta (make-object style-delta% 'change-family 'decorative))
      
      (define modern-style-delta (make-object style-delta% 'change-family 'modern))
      (define output-delta (make-object style-delta%
                             'change-weight
                             'bold))
      (define result-delta (make-object style-delta%
                             'change-weight
                             'bold))
      (define error-delta (make-object style-delta%
                            'change-style
                            'slant))
      (send error-delta set-delta-foreground (make-object color% 255 0 0))
      (send result-delta set-delta-foreground (make-object color% 0 0 175))
      (send output-delta set-delta-foreground (make-object color% 150 0 150))
      
      (define welcome-delta (make-object style-delta% 'change-family 'decorative))
      (define click-delta (gui-utils:get-clickback-delta))
      (define red-delta (make-object style-delta%))
      (define dark-green-delta (make-object style-delta%))
      (send* red-delta
        (copy welcome-delta)
        (set-delta-foreground "RED"))  
      (send* dark-green-delta
        (copy welcome-delta)
        (set-delta-foreground "dark green"))
      (define warning-style-delta (make-object style-delta% 'change-bold))
      (send* warning-style-delta
        (set-delta-foreground "BLACK")
        (set-delta-background "YELLOW"))
      
      ;; is-default-settings? : language-settings -> boolean
      ;; determines if the settings in `language-settings'
      ;; correspond to the default settings of the language.
      (define (is-default-settings? language-settings)
        (send (drscheme:language:language-settings-language language-settings)
              default-settings?
              (drscheme:language:language-settings-settings language-settings)))
      (define (extract-language-name language-settings)
        (car (last-pair (send (drscheme:language:language-settings-language language-settings)
                              get-language-position))))
                         
      (preferences:set-default 'drscheme:teachpack-file
                                  null
                                  (lambda (x) 
                                    (and (list? x)
                                         (andmap string? x))))

      (define current-rep-text (make-parameter #f))
      
      (define-struct sexp (left right prompt))
      
      (define newline-string (string #\newline))
      
      (define console-max-save-previous-exprs 30)
      (let ([marshall 
             (lambda (lls)
               (map (lambda (ls)
                      (map (lambda (s)
                             (cond
                               [(is-a? s string-snip%)
                                (send s get-text 0 (send s get-count))]
                               [(string? s) s]
                               [else "'non-string-snip"]))
                           ls))
                    lls))]
            [unmarshall (lambda (x) x)])
        (preferences:set-un/marshall
         'console-previous-exprs
         marshall unmarshall))
      (let* ([list-of? (lambda (p?)
                         (lambda (l)
                           (and (list? l)
                                (andmap p? l))))]
             [snip/string? (lambda (s) (or (is-a? s snip%) (string? s)))]
             [list-of-snip/strings? (list-of? snip/string?)]
             [list-of-lists-of-snip/strings? (list-of? list-of-snip/strings?)])
        (preferences:set-default
         'console-previous-exprs
         null
         list-of-lists-of-snip/strings?))
      (define (show-interactions-history)
        (let* ([f (make-object (drscheme:frame:basics-mixin frame:standard-menus%)
                    "Interactions History"
                    #f
                    300
                    400)]
               [panel (send f get-panel)]
               [text (make-object text%)]
               [canvas (make-object editor-canvas% panel text)])
          (send f show #t)))
      
      (define error-color (make-object color% "PINK"))
      (define color? (< 8 (get-display-depth)))
      
      (define (quote-regexp-specials s)
        (list->string
         (let loop ([chars (string->list s)])
           (cond
             [(null? chars) null]
             [else
              (case (car chars)
                [(#\( #\) #\* #\+ #\? #\[ #\] #\. #\^ #\$ #\\)
                 (cons #\\ (cons (car chars) (loop (cdr chars))))]
                [else (cons (car chars) (loop (cdr chars)))])]))))
      
      (define (in-canvas? text)
        (let ([editor-admin (send text get-admin)])
          (cond
            [(is-a? editor-admin editor-snip-editor-admin<%>)
             (let* ([snip (send editor-admin get-snip)]
                    [snip-admin (send snip get-admin)])
               (and snip-admin
                    (in-canvas? (send snip-admin get-editor))))]
            [(is-a? editor-admin editor-admin%)
             (send text get-canvas)]
            [else #f])))
      
      (define context<%>
        (interface ()
          ensure-rep-shown   ;; (-> void)
          ensure-defs-shown  ;; (-> void)
          needs-execution?   ;; (-> boolean)
          
          enable-evaluation  ;; (-> void)
          disable-evaluation ;; (-> void)
          
          running            ;; (-> void)
          not-running        ;; (-> void)
          
          clear-annotations  ;; (-> void)
          
          get-directory))    ;; (-> directory-exists)
      
      (define file-icon
        (let ([bitmap
               (make-object bitmap%
                 (build-path (collection-path "icons") "file.gif"))])
          (if (send bitmap ok?)
              (make-object image-snip% bitmap)
              (make-object string-snip% "[open file]"))))
      (define mf-icon 
        (let ([bitmap
               (make-object bitmap%
                 (build-path (collection-path "icons") "mf.gif"))])
          (if (send bitmap ok?)
              (make-object image-snip% bitmap)
              (make-object string-snip% "[mf]"))))
      (define bug-icon 
        (let ([bitmap
               (make-object bitmap%
                 (build-path (collection-path "icons") "bug09.gif"))])
          (if (send bitmap ok?)
              (make-object image-snip% bitmap)
              (make-object string-snip% "[err]"))))
      
      (define (no-user-evaluation-message frame)
        (message-box
         (string-constant evaluation-terminated)
         (format (string-constant evaluation-terminated-explanation))
         frame))
      
      (define busy-cursor (make-object cursor% 'watch))
      (unless (send busy-cursor ok?)
        (printf "WARNING: could not make busy cursor~n")
        (set! busy-cursor #f))
      
      (define current-backtrace-window #f)
      (define (kill-backtrace-window)
        (when current-backtrace-window
          (send current-backtrace-window close)
          (set! current-backtrace-window #f)))
      (define (show-backtrace-window dis error-text)
        (kill-backtrace-window)
        (set! current-backtrace-window 
              (make-object (class100 (drscheme:frame:basics-mixin (frame:standard-menus-mixin frame:basic%)) args
                             (rename [super-on-size on-size])
                             (override
                               [on-size
                                (lambda (x y)
                                  (preferences:set 'drscheme:backtrace-window-width x)
                                  (preferences:set 'drscheme:backtrace-window-height y)
                                  (super-on-size x y))])
                             (override
                               [file-menu:between-print-and-close
                                (lambda (file-menu)
                                  (void))])
                             (override
                               [edit-menu:between-find-and-preferences
                                (lambda (file-menu)
                                  (void))])
                             (sequence (apply super-init args)))
                "Backtrace - DrScheme" #f
                (preferences:get 'drscheme:backtrace-window-width)
                (preferences:get 'drscheme:backtrace-window-height)))
        (letrec ([text (make-object text:basic%)]
                 [ec (make-object canvas:wide-snip% 
                       (send current-backtrace-window get-area-container)
                       text)]
                 [di-vec (list->vector dis)]
                 [index 0]
                 [how-many-at-once 100]
                 [show-next-dis
                  (lambda ()
                    (let ([start-pos (send text get-start-position)]
                          [end-pos (send text get-end-position)])
                      (send text begin-edit-sequence)
                      (send text set-position (send text last-position))
                      (let loop ([n index])
                        (cond
                          [(and (< n (vector-length di-vec))
                                (< n (+ index how-many-at-once)))
                           (show-di ec text (vector-ref di-vec n))
                           (loop (+ n 1))]
                          [else
                           (set! index n)]))
                      
                  ;; add continuation link
                      (when (< index (vector-length di-vec))
                        (let ([end-of-current (send text last-position)])
                          (send text insert #\newline)
                          (let ([hyper-start (send text last-position)])
                            (send text insert 
                                  (let* ([num-left
                                          (- (vector-length di-vec)
                                             index)]
                                         [num-to-show
                                          (min how-many-at-once
                                               num-left)])
                                    (if (= num-left 1)
                                        (string-constant last-stack-frame)
                                        (format (string-constant more-stack-frames) 
                                                (if (<= num-left num-to-show)
                                                    'last
                                                    'next)
                                                num-to-show))))
                            (let ([hyper-end (send text last-position)])
                              (send text change-style click-delta hyper-start hyper-end)
                              (send text set-clickback
                                    hyper-start hyper-end
                                    (lambda x
                                      (send text begin-edit-sequence)
                                      (send text lock #f)
                                      (send text delete end-of-current (send text last-position))
                                      (show-next-dis)
                                      (send text lock #t)
                                      (send text end-edit-sequence)))
                              
                              (send text insert #\newline)
                              (send text set-paragraph-alignment (send text last-paragraph) 'center)))))
                      
                      (send text set-position start-pos end-pos)
                      (send text end-edit-sequence)))])
          (send current-backtrace-window reflow-container)
          (send text auto-wrap #t)
          (send text set-autowrap-bitmap #f)
          (send text insert error-text)
          (send text insert #\newline)
          (send text insert #\newline)
          (send text change-style error-delta 0 (- (send text last-position) 1))
          (show-next-dis)
          (send text set-position 0 0)
          (send text lock #t)
          (send current-backtrace-window show #t)))
      (define (show-di ec text di)
        (error 'show-di "not yet implemented")
        '(let* ([start (zodiac:zodiac-start di)]
               [finish (zodiac:zodiac-finish di)]
               [file (zodiac:location-file start)]
               [untitled "<<unknown>>"]
               [fn (cond
                     [(string? file)
                      file]
                     [(is-a? file text%)
                      (let ([c (send file get-canvas)])
                        (if c
                            (let* ([win (send c get-top-level-window)]
                                   [def-filename (send win get-filename)])
                              (if def-filename
                                  (format "~a's interactions" def-filename)
                                  "interactions"))
                            untitled))]
                     [(is-a? file editor<%>)
                      (or (send file get-filename)
                          (let ([canvas (send file get-canvas)])
                            (if canvas
                                (let ([frame (send canvas get-top-level-window)])
                                  (if frame
                                      (send frame get-label)
                                      untitled))
                                untitled)))]
                     [else untitled])]
               [start-pos (send text last-position)])
          
         ;; make hyper link to the file
          (send text insert (format "~a: ~a.~a - ~a.~a" 
                                    fn
                                    (zodiac:location-line start)
                                    (zodiac:location-column start)
                                    (zodiac:location-line finish)
                                    (+ 1 (zodiac:location-column finish))))
          (let ([end-pos (send text last-position)])
            (send text insert #\newline)
            (send text change-style click-delta start-pos end-pos)
            (send text set-clickback
                  start-pos end-pos
                  (lambda x
                    (open-and-highlight-in-file di))))
          
          ; show context
          (when (or (and (string? file)
                         (file-exists? file))
                    (is-a? file text%))
            (let ([context-text (make-object text:basic%)])
              (let-values ([(from-text close-text)
                            (cond
                              [(string? file)
                               (let ([text (make-object text:basic%)])
                                 (send text load-file file)
                                 (values text
                                         (lambda ()
                                           (send text on-close))))]
                              [(is-a? file text%) (values file void)])])
                (let* ([start-pos (send from-text paragraph-start-position 
                                        (send from-text position-paragraph (zodiac:location-offset start)))]
                       [from-start (- (zodiac:location-offset start) start-pos)]
                       [end-pos (send from-text paragraph-end-position
                                      (send from-text position-paragraph (zodiac:location-offset finish)))]
                       [from-end (+ 1 from-start (- (zodiac:location-offset finish) (zodiac:location-offset start)))])
                  (send from-text split-snip start-pos)
                  (send from-text split-snip end-pos)
                  (lambda (di)
                    (let* ([start (zodiac:zodiac-start di)]
                           [finish (zodiac:zodiac-finish di)]
                           [file (zodiac:location-file start)]
                           [untitled "<<unknown>>"]
                           [fn (cond
                                 [(string? file)
                                  file]
                                 [(is-a? file text%)
                                  (let ([c (send file get-canvas)])
                                    (if c
                                        (let* ([win (send c get-top-level-window)]
                                               [def-filename (send (send win get-definitions-text) get-filename)])
                                          (if def-filename
                                              (format "~a's interactions" def-filename)
                                              "interactions"))
                                        untitled))]
                                 [(is-a? file editor<%>)
                                  (or (send file get-filename)
                                      (let ([canvas (send file get-canvas)])
                                        (if canvas
                                            (let ([frame (send canvas get-top-level-window)])
                                              (if frame
                                                  (send frame get-label)
                                                  untitled))
                                            untitled)))]
                                 [else untitled])]
                           [start-pos (send text last-position)])
                      
                    ;; make hyper link to the file
                      (send text insert (format "~a: ~a.~a - ~a.~a" 
                                                fn
                                                (zodiac:location-line start)
                                                (zodiac:location-column start)
                                                (zodiac:location-line finish)
                                                (+ 1 (zodiac:location-column finish))))
                      (let ([end-pos (send text last-position)])
                        (send text insert #\newline)
                        (send text change-style click-delta start-pos end-pos)
                        (send text set-clickback
                              start-pos end-pos
                              (lambda x
                                (open-and-highlight-in-file di))))
                      
                    ;; show context
                      (when (or (and (string? file)
                                     (file-exists? file))
                                (is-a? file text%))
                        (let ([context-text (make-object text:basic%)])
                          (let-values ([(from-text close-text)
                                        (cond
                                          [(string? file)
                                           (let ([text (make-object text:basic%)])
                                             (send text load-file file)
                                             (values text
                                                     (lambda ()
                                                       (send text on-close))))]
                                          [(is-a? file text%) (values file void)])])
                            (let* ([start-pos (send from-text paragraph-start-position 
                                                    (send from-text position-paragraph (zodiac:location-offset start)))]
                                   [from-start (- (zodiac:location-offset start) start-pos)]
                                   [end-pos (send from-text paragraph-end-position
                                                  (send from-text position-paragraph (zodiac:location-offset finish)))]
                                   [from-end (+ 1 from-start (- (zodiac:location-offset finish) (zodiac:location-offset start)))])
                              (send from-text split-snip start-pos)
                              (send from-text split-snip end-pos)
                              (let loop ([snip (send from-text find-snip start-pos 'after-or-none)])
                                (when (and snip
                                           (< (send from-text get-snip-position snip) end-pos))
                                  (send context-text insert (send snip copy))
                                  (loop (send snip next))))
                              (send context-text change-style modern-style-delta 0 (send context-text last-position))
                              (send context-text highlight-range from-start from-end error-color #f #f 'high)
                              (send text insert "  ")
                              (let ([snip (make-object editor-snip% context-text)])
                                (send ec add-wide-snip snip)
                                (send text insert snip))
                              (send text insert #\newline))
                            (close-text))))
                      (send text insert #\newline)))
                  (let loop ([snip (send from-text find-snip start-pos 'after-or-none)])
                    (when (and snip
                               (< (send from-text get-snip-position snip) end-pos))
                      (send context-text insert (send snip copy))
                      (loop (send snip next))))
                  (send context-text change-style modern-style-delta 0 (send context-text last-position))
                  (send context-text highlight-range from-start from-end error-color #f #f 'high)
                  (send text insert "  ")
                  (let ([snip (make-object editor-snip% context-text)])
                    (send ec add-wide-snip snip)
                    (send text insert snip))
                  (send text insert #\newline))
                (close-text))))
          (send text insert #\newline)))
      
      ;; open-and-higlight-in-file : syntax -> void
      ;; opens the window displaying this piece of syntax (if there is one)
      ;; and highlights the right position in the file
      (define (open-and-highlight-in-file stx)
        (void))
      
      (define arrow-cursor (make-object cursor% 'arrow))
      (define eof-icon-snip%
        (class100 image-snip% (_rep)
          (private-field
           [rep _rep])
          (rename [super-on-event on-event])
          (override
            [on-event
             (lambda (dc x y editor-x editor-y evt)
               (cond
                 [(send evt get-left-down) (send rep submit-eof)]
                 [else (super-on-event dc x y editor-x editor-y evt)]))]
            [adjust-cursor
             (lambda (dc x y editorx editory evt)
               arrow-cursor)])
          (inherit get-flags set-flags)
          (sequence
            (super-init (build-path (collection-path "icons") "eof.gif"))
            (set-flags (cons 'handles-events (get-flags))))))
      
      (define (make-text% super%)
        (rec rep-text%
          (class super%
            (init-field context)
            (inherit insert change-style get-canvas
                     get-active-canvas
                     set-styles-sticky
                     clear-undos set-caret-owner
                     clear-previous-expr-positions
                     get-end-position
                     set-clickback
                     do-post-eval
                     insert-prompt
                     erase 
                     get-prompt-mode
                     ready-non-prompt
                     set-prompt-mode
                     delete lock is-locked?
                     paragraph-start-position
                     last-position
                     set-resetting
                     position-line
                     set-position
                     begin-edit-sequence
                     end-edit-sequence
                     reset-pretty-print-width
                     scroll-to-position
                     get-admin
                     set-prompt-position
                     get-canvases find-snip
                     release-snip)
            (rename [super-on-insert on-insert]
                    [super-on-delete on-delete]
                    [super-initialize-console initialize-console]
                    [super-reset-console reset-console]
                    [super-on-close on-close])
            
            (override on-insert on-delete on-close)
            
            (override get-prompt eval-busy? do-eval
                      initialize-console
                      reset-console)
            
            (public
              get-transparent-text
              get-transparent-snip
              cleanup-transparent-io
              init-transparent-io
              init-transparent-input
              init-transparent-io-do-work
              
              this-in-char-ready?
              this-in-read-char
              this-in-peek-char
              generic-write
              this-result-write
              this-out-write
              this-err-write/exn
              this-err-write
              get-this-err
              get-this-out
              get-this-in
              get-this-result
              setup-display/write-handlers
              display-results
              
              reset-highlighting
              format-source-loc
              
              get-user-custodian
              get-user-eventspace
              get-user-thread
              
              insert-warning
              
              cleanup
              cleanup-interaction
              
              do-many-evals
              do-many-text-evals
              
              reset-break-state
              break
              
              run-in-evaluation-thread
              
              shutdown
              kill-evaluation
              
              submit-eof
              show-eof-icon
              hide-eof-icon)
            
            (unless (is-a? context context<%>)
              (error 'drscheme:rep:text% 
                     "expected an object that implements drscheme:rep:context<%> as initialization argument, got: ~e"
                     context))
            
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;					     ;;;
	;;;            User -> Kernel                ;;;
	;;;					     ;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
            (define protect
              (lambda (proc)
                (let ([ut user-thread])
                  (call-in-nested-thread
                   (lambda ()
                     (break-enabled #f)
                     (proc ut))
                   drscheme:init:system-custodian))))
            
            (define queue-system-callback
              (case-lambda
               [(ut thunk) (queue-system-callback ut thunk #f)]
               [(ut thunk always?)
                (parameterize ([current-eventspace drscheme:init:system-eventspace])
                  (queue-callback 
                   (lambda ()
                     (when (or always? (eq? ut user-thread))
                       (thunk)))
                   #f))]))
            
            (define queue-system-callback/sync
              (lambda (ut thunk)
                (let ([s (make-semaphore)])
                  (queue-system-callback ut (lambda () (thunk) (semaphore-post s)))
                  (semaphore-wait s))))
            
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;					     ;;;
	;;;                  I/O                     ;;;
	;;;					     ;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
            (field (transparent-text #f)
                   (transparent-snip #f))

            (define (get-transparent-text) transparent-text)
            (define (get-transparent-snip) transparent-snip)
            
            (define cleanup-transparent-io ; =Kernel=, =Handler=
              (lambda ()
                (when transparent-text
                  (set! saved-newline? #f) 
                  (hide-eof-icon)
                  (send transparent-text shutdown)
                  (set-position (last-position))
                  (set-caret-owner #f)
                  (let ([a (get-admin)])
                    (when a
                      (send a grab-caret)))
                  (send transparent-text lock #t)
                  (set! transparent-text #f)
                  (drop-fetcher))))
            
            (define init-transparent-io ; =Kernel=, =Handler=
              (lambda (grab-focus?)
                (begin-edit-sequence)
                (if transparent-text
                    (when grab-focus?
                      (let ([a (send transparent-text get-admin)])
                        (when a
                          (send a grab-caret))))
                    (init-transparent-io-do-work grab-focus?))
                (when  (eq? (current-thread) user-thread)
                  (set-caret-owner transparent-snip 'display))
                (end-edit-sequence)
                transparent-text))
            
            (define (init-transparent-input) ; =Kernel=, =Handler=
              (let ([text (init-transparent-io #t)])
                (yield) ; to flush output and set `saved-newline?'
                (when saved-newline?
                  (this-out-write "")
                  (yield)) ; flush output again
                text))
            
            (field (eof-received? #f)
                   (eof-snip #f))
            (define (show-eof-icon) 
              (unless eof-snip
                (set! eof-snip (make-object eof-icon-snip% this))
                (let ([c-locked? (is-locked?)])
                  (begin-edit-sequence)
                  (lock #f)
                  (insert eof-snip
                          (- (last-position) 1)
                          (- (last-position) 1))
                  (lock c-locked?)
                  (for-each (lambda (c) (send c recalc-snips))
                            (get-canvases))	
                  (end-edit-sequence))))
            (define (hide-eof-icon) 
              (when eof-snip
                (let ([c-locked? (is-locked?)])
                  (begin-edit-sequence)
                  (lock #f)
                  (release-snip eof-snip)
                  (for-each (lambda (c) (send c recalc-snips))
                            (get-canvases))
                  (lock c-locked?)
                  (end-edit-sequence)
                  (set! eof-snip #f))))
            (define (submit-eof)
              (when transparent-text
                (send transparent-text eof-received))
              (hide-eof-icon))
            
            (define init-transparent-io-do-work  ; =Kernel=, =Handler=
              (lambda (grab-focus?)
                (let ([c-locked? (is-locked?)])
                  (begin-edit-sequence)
                  (lock #f)
                  (let ([starting-at-prompt-mode? (get-prompt-mode)])
                    (set! transparent-text (make-object transparent-io-text% this))
                    
                    (send transparent-text auto-wrap #t)
                    (send transparent-text balance-required #f)
                    (send transparent-text set-styles-fixed #f)
                    
                ;; ensure that there is a newline before the snip is inserted
                    (unless (member 'hard-newline
                                    (send (find-snip (last-position) 'before) get-flags))
                      (insert (string #\newline) (last-position) (last-position) #f))
                    
                    (when starting-at-prompt-mode?
                      (set-prompt-mode #f))
                    
                    (let ([snip (make-object editor-snip% transparent-text)])
                      (set! transparent-snip snip)
                      (insert snip (last-position) (last-position) #f)
                      (insert (string #\newline) (last-position) (last-position) #f)
                      (for-each (lambda (c) (send c add-wide-snip snip))
                                (get-canvases)))
                    (when grab-focus?
                      (let ([a (send transparent-text get-admin)])
                        (when a
                          (send a grab-caret)))))
                  (lock c-locked?)
                  (end-edit-sequence)))) 
            
            (define (make-fetcher)
              (make-object
                  (class100 object% ()
                    (private-field
                     [fetch-char-sema (make-semaphore 1)]
                     [fetcher-spawned? #f]
                     [char-fetched-sema (make-semaphore)]
                     [char-fetched #f])
                    (public
                      [fetch ; =Protected-User=
                       (lambda (ut peek?)
                         ; Only one reader at a time:
                         (semaphore-wait/enable-break fetch-char-sema)
                         ; Now we're the active reader...
                         (unless fetcher-spawned?
                           (set! fetcher-spawned? #t)
                           ; Spawn a fetcher:
                           (queue-system-callback
                            ut
                            (lambda () ; =Kernel=, =Handler=
                              (if eof-received?
                                  (set! char-fetched eof)
                                  (let ([text (init-transparent-input)])
                                    (set! char-fetched (send text fetch-char))
                                    (when (eof-object? char-fetched)
                                      (set! eof-received? #t))))
                              (semaphore-post char-fetched-sema))))
                         ; Wait for a char, allow breaks:
                         (with-handlers ([void (lambda (x)
                                                 ; Let someone else try to read...
                                                 (semaphore-post fetch-char-sema)
                                                 (raise x))])
                           (semaphore-wait/enable-break char-fetched-sema))
                         ; Got the char (no breaks)
                         (if peek?
                             ; preserve the fecthed cahr
                             (semaphore-post char-fetched-sema)
                             ; Next reader'll have to spawn a fetcher
                             (set! fetcher-spawned? #f))
                         (begin0
                           char-fetched
                           ; Got our char; let another reader go
                           (semaphore-post fetch-char-sema)))])
                    (sequence (super-init)))))
            (field (fetcher #f)
                   (fetcher-semaphore (make-semaphore 1)))
            (define drop-fetcher ; =Kernel=, =Handler=
              (lambda ()
                (semaphore-wait fetcher-semaphore)
                (set! fetcher #f)
                (semaphore-post fetcher-semaphore)))
            (define this-in-fetch-char ; =User=
              (lambda (peek?)
                (protect
                 (lambda (ut) ; =Protected-User=
                   (semaphore-wait fetcher-semaphore)
                   (unless fetcher (set! fetcher (make-fetcher)))
                   (semaphore-post fetcher-semaphore)
                   (send fetcher fetch ut peek?)))))
            
            (define this-in-char-ready?
              (lambda () ; =User=
                (protect
                 (lambda (ut)  ; =Protected-User=
                   (let ([answer #f]
                         [s (make-semaphore 0)])
                     (queue-system-callback
                      ut
                      (lambda () ; =Kernel=, =Handler=
                        (if eof-received?
                            (set! answer #t)
                            (let ([text (init-transparent-input)])
                              (set! answer (send text check-char-ready?))))
                        (semaphore-post s)))
                     ; enable-break in case the thread dies and the callback never 
                     ;  happens:
                     (semaphore-wait/enable-break s)
                     answer)))))
            
            (define this-in-read-char ; =User=
              (lambda ()
                (this-in-fetch-char #f)))
            
            (define this-in-peek-char ; =User=
              (lambda ()
                (this-in-fetch-char #t)))
            
            (field (flushing-event-running (make-semaphore 1))
                   (limiting-sema (make-semaphore output-limit-size))) ; waited once foreach in io-collected-thunks
            
            (field (io-semaphore (make-semaphore 1))
                   (io-collected-thunks null) ; protected by semaphore
                   (io-collected-texts null)) ; always set in the kernel's handler thread
            (define run-io-collected-thunks ; =Kernel=, =Handler=
              (lambda ()
            ;; also need to start edit-sequence in any affected
            ;; transparent io boxes.
                (semaphore-wait io-semaphore)
                (let ([io-thunks io-collected-thunks])
                  (set! io-collected-thunks null)
                  (semaphore-post io-semaphore)
                  
                  (begin-edit-sequence)
                  (for-each (lambda (t) (semaphore-post limiting-sema) (t))
                            (reverse io-thunks))
                  (for-each (lambda (e) (send e end-edit-sequence)) io-collected-texts)
                  (unless (null? io-thunks)
                    (scroll-to-position (last-position)))
                  (end-edit-sequence)
                  
                  (set! io-collected-texts null))))
            
            (define (wait-for-io-to-complete) ; =Kernel=, =Handler=
              (unless (null? io-collected-thunks)
                (let ([semaphore (make-semaphore 0)])
                  (queue-callback
                   (lambda () ; =Kernel=, =Handler=
                     (run-io-collected-thunks)
                     (semaphore-post semaphore))
                   #f)
                  (yield semaphore))))
            (define queue-output ; =User=
              (lambda (thunk)
                (protect
                 (lambda (ut) ; =Protected-User=
                   ; limiting-sema prevents queueing too much output from the user
                   (semaphore-wait/enable-break limiting-sema)
                   ; Queue the output:
                   (let ([this-eventspace user-eventspace])
                     (semaphore-wait io-semaphore)
                     (if (eq? ut user-thread)
                         ; Queue output:
                         (set! io-collected-thunks
                               (cons thunk io-collected-thunks))
                         ; Release limit allocation, instead:
                         (semaphore-post limiting-sema))
                     (semaphore-post io-semaphore))
                   ; If there's not one, queue an event that will flush the output queue
                   (when (semaphore-try-wait? flushing-event-running)
                     ; Unlike most callbacks, this one has to run always, even if
                     ;   the user thread changes.
                     (queue-system-callback
                      ut
                      (lambda () ; =Kernel=, =Handler=
                        (semaphore-post flushing-event-running)
                        (run-io-collected-thunks))
                      #t))))))
            
            (define generic-write ; =Kernel=, =Handler=
              (lambda (text s style-func)
                
                (let ([add-text
                       (lambda (text)
                         (unless (or (eq? this text)
                                     (member text io-collected-texts))
                           (set! io-collected-texts (cons text io-collected-texts))
                           (send text begin-edit-sequence)))])
                  (add-text text))
                
                (when (get-prompt-mode)
                  (insert (string #\newline) (last-position) (last-position) #f))
                
                (let* ([start (if (is-a? text transparent-io-text<%>)
                                  (send text get-insertion-point)
                                  (send text last-position))]
                       [c-locked? (send text is-locked?)])
                  (send text begin-edit-sequence)
                  (send text lock #f)
                  (when (is-a? text transparent-io-text<%>)
                    (send text set-program-output #t))
                  (let ([to-be-inserted
                         (cond
                           [(is-a? s snip%) (send s copy)]
                           [else s])])
                    (send text insert to-be-inserted start start #t)
                    (let ([end (+ start (cond
                                          [(string? to-be-inserted)
                                           (string-length to-be-inserted)]
                                          [(is-a? to-be-inserted snip%)
                                           (send to-be-inserted get-count)]))])
                      (style-func start end)
                      (send text set-prompt-position end)))
                  
                  (when (is-a? text transparent-io-text<%>)
                    (send text set-program-output #f))
                  
                  (set-prompt-mode #f)
                  
                  (send text lock c-locked?)
                  (send text end-edit-sequence))))
            
	    ;; this-result-write : (union string (instanceof snip%)) -> void
	    ;; writes `s' as a value produced by the REPL.
            (define this-result-write 
              (lambda (s) ; =User=
                (queue-output
                 (lambda () ; =Kernel=, =Handler=
                   (cleanup-transparent-io)
                   (generic-write this
                                  s
                                  (lambda (start end)
                                    (change-style result-delta
                                                  start end)))))))

            (field (saved-newline? #f))
	    ;; this-out-write : (union string snip%) -> void
            (define this-out-write
              (lambda (s) ; = User=
                (queue-output
                 (lambda () ; =Kernel=, =Handler=
                   (let* ([text (init-transparent-io #f)]
                          [old-saved-newline? saved-newline?]
                          [len (and (string? s)
                                    (string-length s))]
                          [s1 (if (and len
                                       (> len 0)
                                       (char=? (string-ref s (- len 1)) #\newline))
                                  (begin 
                                    (set! saved-newline? #t)
                                    (substring s 0 (- len 1)))
                                  (begin
                                    (set! saved-newline? #f)
                                    s))]
                          [gw
                           (lambda (s)
                             (generic-write
                              text
                              s
                              (lambda (start end)
                                (send text change-style output-delta start end))))])
                     (when old-saved-newline?
                       (gw (string #\newline)))
                     (gw s1))))))
            
	    ;; this-err-write/exn : (union string snip) (union exn #f) -> void
	    ;; writes the string to the console error and highlights the
	    ;; variable portion of the message if the second argument is
	    ;; a variable:exn. 
	    (define (this-err-write/exn s exn) ; =User=
              (queue-output
               (lambda () ; =Kernel=, =Handler=
                 (cleanup-transparent-io)
                 (generic-write
                  this
                  s
                  (lambda (start end)
                    (change-style error-delta start end)
                    (cond
                      [(exn:variable? exn)
                       (let* ([var (symbol->string (exn:variable-id exn))]
                              [regexp (format "^(.*)(~a)" (quote-regexp-specials var))]
                              [match (with-handlers ([(lambda (x) #t)
                                                      (lambda (x)
                                                        ((error-display-handler)
                                                         (format "error constructing regexp: ~s~n"
                                                                 regexp))
                                                        #f)])
                                       (regexp-match regexp s))])
                         (when match
                           (let* ([var-start (+ start (string-length (cadr match)))]
                                  [var-end (+ var-start (string-length (caddr match)))])
                             (change-style click-delta var-start var-end)
                             (set-clickback var-start var-end
                                            (lambda x
                                              (drscheme:help:help-desk var))))))]
                      [else
                       (let ([bind-to-help
                              (lambda (regexp s)
                                (let ([match (regexp-match regexp s)])
                                  (if match
                                      (let* ([prefix (cadr match)]
                                             [var (caddr match)]
                                             [var-start (+ start (string-length prefix))]
                                             [var-end (+ var-start (string-length var))])
                                        (change-style click-delta var-start var-end)
                                        (set-clickback
                                         var-start var-end
                                         (lambda x
                                           (drscheme:help:help-desk var)))
                                        prefix)
                                      #f)))])
                         (let loop ([s s])
                           (when s
                             (loop (bind-to-help class-regexp s))))
                         (bind-to-help ivar-regexp s)
                         (bind-to-help fallthru-regexp s))]))))))

	    ;; this-err-write : (union string (instanceof snip%)) -> void
            (define this-err-write ; =User=
              (lambda (s)
                (this-err-write/exn s #f)))
            
            (field (this-err (make-output-port (lambda (x) (this-err-write x))
                                               void))
                   (this-out (make-output-port (lambda (x) (this-out-write x))
                                               void))
                   (this-in (make-input-port (lambda () (this-in-read-char))
                                             (lambda () (this-in-char-ready?))
                                             void
                                             (lambda () (this-in-peek-char))))
                   (this-result (make-output-port (lambda (x) (this-result-write x)) 
                                                  void)))
            
            (define (get-this-err) this-err)
            (define (get-this-out) this-out)
            (define (get-this-in) this-in)
            (define (get-this-result) this-result)

            ;; setup-display/write-handlers : -> void
            ;; sets the port-display-handler and the port-write-handler
            ;; for the initial output port, initial error port and the
            ;; value port.
            (define (setup-display/write-handlers)
              (let* ([use-number-snip?
                      (lambda (x)
                        (and (number? x)
                             (exact? x)
                             (real? x)
                             (not (integer? x))))]
                     [pretty-print-size-hook
                      (lambda (x _ port)
                        (cond
			  [(is-a? x sized-snip<%>) (send x get-character-width)]
                          [(is-a? x snip%) 1]
                          [(use-number-snip? x)
                           (+ (string-length (number->string (floor x)))
                              (max (string-length
                                    (number->string 
                                     (numerator (- x (floor x)))))
                                   (string-length
                                    (number->string 
                                     (numerator (- x (floor x)))))))]
                          [else #f]))]
		     [make-pretty-print-print-hook
		      (lambda (port-out-write)
		      ;; port and port-out-write must match at this point.
			(lambda (x _ port)
			  (let ([snip/str
				 (cond
				   [(use-number-snip? x)
				    (make-object drscheme:snip:whole/part-number-snip% x)]
				   [else x])])
			    (port-out-write snip/str))))]
                     [make-setup-handler
                      (lambda (port port-out-write)
                        (lambda (port-handler pretty)
                          (let ([original-handler (port-handler port)])
                            (port-handler
                             port
                             (rec console-pp-handler
                               (lambda (v p)
                                 ;; avoid infinite recursion by calling original-handler
                                 ;; for strings, since `pretty' calls write/display with
                                 ;; strings
                                 (if (string? v)
                                     (original-handler v p)
                                     (parameterize ([mzlib:pretty-print:pretty-print-size-hook
                                                     pretty-print-size-hook]
                                                    [mzlib:pretty-print:pretty-print-print-hook
						     (make-pretty-print-print-hook port-out-write)]
                                                    [mzlib:pretty-print:pretty-print-columns
                                                     'infinity])
                                       (pretty v p)))))))))]
                     
                     [setup-handlers
                      (lambda (setup-handler)
                        (setup-handler port-display-handler 
                                       mzlib:pretty-print:pretty-display)
                        (setup-handler port-write-handler
                                       mzlib:pretty-print:pretty-print))]
                     
                     [setup-out-handler (make-setup-handler this-out (lambda (x) (this-out-write x)))]
                     [setup-err-handler (make-setup-handler this-err (lambda (x) (this-err-write x)))]
                     [setup-value-handler (make-setup-handler this-result (lambda (x) (this-result-write x)))])
                (setup-handlers setup-out-handler)
                (setup-handlers setup-err-handler)
                (setup-handlers setup-value-handler)))
            
            ;; display-results : (listof TST) -> void
            ;; prints each element of anss that is not void as values in the REPL.
            (define (display-results anss) ; =User=, =Handler=, =Breaks=
              (for-each 
               (lambda (v)
                 (unless (void? v)
                   (print v this-result)))
                 anss))
            
            (define (reset-highlighting) (void))
            
            ;; format-source-loc : syntax -> string
            (define format-source-loc ;; =Kernel=, =Handler=
              (lambda (stx)
                (format "~s" stx)))
            
            (define (on-set-media) (void))
            
            (define on-insert
              (lambda (x y)
                (reset-highlighting)
                (super-on-insert x y)))
            (define on-delete
              (lambda (x y)
                (reset-highlighting)
                (super-on-delete x y)))
            
            
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;                Parameters                  ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ;; drscheme-port-print-handler : TST port -> void
            ;; effect: prints the value on the port
            ;; default setting for the behavior of the `print' primitive.
            (define (drscheme-port-print-handler value port)
              (let ([language (drscheme:language:language-settings-language
			       (current-language-settings))])
                (send language render-value
		      value 
                      port 
                      (cond
                        [(eq? port this-out) (lambda (x) (this-out-write x))]
                        [(eq? port this-result) (lambda (x) (this-result-write x))]
                        [(eq? port this-err) (lambda (x) (this-err-write x))]
                        [else #f]))))
            
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;                Evaluation                  ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
            (define (get-prompt) "> ")
            (define (eval-busy?)
              (not (and user-thread
                        (thread-running? user-thread))))
            
            (field (user-language-settings 'uninitialized-user-language-settings)
		   (user-custodian (make-custodian))
                   (user-eventspace #f)
                   (user-thread #f))
            
            (define (get-user-custodian) user-custodian)
            (define (get-user-eventspace) user-eventspace)
            (define (get-user-thread) user-thread)
            
            (field (in-evaluation? #f) ; a heursitic for making the Break button send a break
                   (should-collect-garbage? #f)
                   (ask-about-kill? #f))
            
            (define (insert-warning)
              (begin-edit-sequence)
              (insert #\newline (last-position) (last-position))
              (let ([start (last-position)])
                (insert
                 "WARNING: Interactions window is out of sync with the definitions window. Click Execute."
                 start start)
                (let ([end (last-position)])
                  (change-style warning-style-delta start end)))
              (end-edit-sequence))
            
            (field (already-warned? #f))
            
            (field (eval-count 0))
            (define (do-eval start end)
              (set! eval-count (add1 eval-count))
              (when (5 . <= . eval-count)
                (collect-garbage)
                (set! eval-count 0))
              (let* ([needs-execution? (send context needs-execution?)])
                (when (if (preferences:get 'drscheme:execute-warning-once)
                          (and (not already-warned?)
                               needs-execution?)
                          needs-execution?)
                  (set! already-warned? #t)
                  (insert-warning)))
              (do-many-text-evals this start end))
            
            (define (cleanup)
              (set! in-evaluation? #f)
              (update-running)
              (unless (and user-thread (thread-running? user-thread))
                (lock #t)
                (unless shutting-down?
                  (no-user-evaluation-message
                   (let ([canvas (get-active-canvas)])
                     (and canvas
                          (send canvas get-top-level-window)))))))
            (field (need-interaction-cleanup? #f))
            
            (field (saved-cursor #f))
            
            (define (cleanup-interaction) ; =Kernel=, =Handler=
              (set! need-interaction-cleanup? #f)
              (send (get-canvas) set-cursor saved-cursor)
              (begin-edit-sequence)
              (wait-for-io-to-complete)
              (cleanup-transparent-io)
              (set-caret-owner #f 'display)
              (when (and user-thread (thread-running? user-thread))
                (let ([c-locked? (is-locked?)])
                  (lock #f)
                  (insert-prompt)
                  (lock c-locked?)))
              (cleanup)
              (end-edit-sequence)
              (send context enable-evaluation))
            
            (define (do-many-text-evals text start end)
              (do-many-evals
               (lambda (single-loop-eval)
                 (drscheme:load-handler:process-text
                  ; BUG: is it possible that a macro turns on breaking?
                  text
                  (lambda (expr recur) ; =User=, =Handler=, =No-Breaks=
                    (cond
                      [(drscheme:load-handler:process-finish? expr)
                       (void)]
                      [else
                       (single-loop-eval
                        (lambda ()
                          (call-with-values
                           (lambda ()
                             (primitive-eval expr))
                           (lambda x (display-results x)))))
                       (recur)]))
                  start
                  end))))
            
            
	    ;; do-many-evals : ((((-> void) -> void) -> void) -> void)
            (define do-many-evals ; =Kernel=, =Handler=
              
	      ;; run-loop has the loop. It expects one argument, a procedure that
	      ;; can be called with a thunk. The argument to run-loop maintains the right
	      ;; breaking state and calls the thunk it was called with.
              (lambda (run-loop)  ;; (((-> void) -> void) -> void)
                (send context disable-evaluation)
                (cleanup-transparent-io)
                (kill-backtrace-window)
                (reset-pretty-print-width)
                (ready-non-prompt)
                (set! saved-cursor (send (get-canvas) get-cursor))
                (send (get-canvas) set-cursor busy-cursor)
                (when should-collect-garbage?
                  (set! should-collect-garbage? #f)
                  (collect-garbage))
                (set! need-interaction-cleanup? #t)
                
                (run-in-evaluation-thread
                 (lambda () ; =User=, =Handler=, =No-Breaks=
                   (reset-break-state)
                   
                   (protect-user-evaluation
                    ; Evaluate the expression(s)
                    (lambda () ; =User=, =Handler=, =No-Breaks=
                      ; This procedure must also ensure that breaks are off before
                      ;  returning or escaping.
                      (run-loop
                       (lambda (thunk) ;; (-> void)
                         ; Evaluate the user's expression. We're careful to turn on
                         ;   breaks as we go in and turn them off as we go out.
                         ;   (Actually, we adjust breaks however the user wanted it.)
                         ; A continuation hop might take us out of this instance of
                         ;   evaluation and into another one, which is fine.
                         (dynamic-wind
                          (lambda () 
                            (break-enabled user-break-enabled)
                            (set! user-break-enabled 'user))
                          (lambda ()
                            (thunk))
                          (lambda () 
                            (set! user-break-enabled (break-enabled))
                            (break-enabled #f))))))
                    
                    ; Cleanup after evaluation:
                    (lambda () ; =User=, =Handler=, =No-Breaks=
                      (queue-system-callback/sync
		       user-thread
		       (lambda () (cleanup-interaction)))))))))
            
            (define shutdown-user-custodian ; =Kernel=, =Handler=
              ; Use this procedure to shutdown when in the middle of other cleanup
              ;  operations, such as when the user clicks "Execute".
              ; Don't use it to kill a thread where other, external cleanup
              ;  actions must occur (e.g., the exit handler for the user's
              ;  thread). In that case, shut down user-custodian directly.
              (lambda ()
                (custodian-shutdown-all user-custodian)
                (set! user-thread #f)
                (semaphore-wait io-semaphore)
                (for-each (lambda (i) (semaphore-post limiting-sema)) io-collected-thunks)
                (set! io-collected-thunks null)
                (semaphore-post io-semaphore)))
            
            (define reset-break-state (lambda () (set! ask-about-kill? #f)))
            (define break
              (lambda () ; =Kernel=, =Handler=
                (cond
                  [(not in-evaluation?)
                   (bell)]
                  [ask-about-kill? 
                   (if (gui-utils:get-choice
                        (string-constant kill-evaluation?)
                        (string-constant just-break)
                        (string-constant kill)
                        (string-constant kill?)
                        'diallow-close
                        (let ([canvas (get-active-canvas)])
                          (and canvas
                               (send canvas get-top-level-window))))
                       (break-thread user-thread)
                       (custodian-shutdown-all user-custodian))]
                  [else
                   (break-thread user-thread)
                   (set! ask-about-kill? #t)])))
            
            (define (kill-evaluation) ; =Kernel=, =Handler=
              (custodian-shutdown-all user-custodian))
            
            (field (error-escape-k void)
                   (user-break-enabled #t))
            
            (field (eval-thread-thunks null)
                   (eval-thread-state-sema 'not-yet-state-sema)
                   (eval-thread-queue-sema 'not-yet-thread-sema)
                   
                   (cleanup-sucessful 'not-yet-cleanup-sucessful)
                   (cleanup-semaphore 'not-yet-cleanup-semaphore)
                   (thread-grace 'not-yet-thread-grace)
                   (thread-killed 'not-yet-thread-killed))
            (define (initialize-killed-thread) ; =Kernel=
              (when (thread? thread-killed)
                (kill-thread thread-killed))
              (set! thread-killed
                    (thread
                     (lambda () ; =Other=
                       (let ([ut user-thread])
                         (thread-wait ut)
                         (queue-callback
                          (lambda ()
                            (when (eq? user-thread ut)
                              (if need-interaction-cleanup?
                                  (cleanup-interaction)
                                  (cleanup))))))))))
            
            (define protect-user-evaluation ; =User=, =Handler=, =No-Breaks=
              (lambda (thunk cleanup)
                ;; We only run cleanup if thunk finishes normally or tries to
                ;; error-escape. Otherwise, it must be a continuation jump
                ;; into a different call to protect-user-evaluation.
                
                ;; `thunk' is responsible for ensuring that breaks are off when
                ;; it returns or jumps out.
                
                (set! in-evaluation? #t)
                (update-running)
                
                (let/ec k
                  (let ([saved-error-escape-k error-escape-k]
                        [cleanup? #f])
                    (dynamic-wind
                     (lambda ()
                       (set! cleanup? #f)
                       (set! error-escape-k (lambda () 
                                              (set! cleanup? #t)
                                              (k (void)))))
                     (lambda () (thunk) 
                       ; Breaks must be off!
                       (set! cleanup? #t))
                     (lambda () 
                       (set! error-escape-k saved-error-escape-k)
                       (when cleanup?
                         (set! in-evaluation? #f)
                         (update-running)
                         (cleanup))))))))
            
            (define run-in-evaluation-thread ; =Kernel=
              (lambda (thunk)
                (semaphore-wait eval-thread-state-sema)
                (set! eval-thread-thunks (append eval-thread-thunks (list thunk)))
                (semaphore-post eval-thread-state-sema)
                (semaphore-post eval-thread-queue-sema)))
            (define init-evaluation-thread ; =Kernel=
              (lambda ()
                (set! user-language-settings
		      (preferences:get drscheme:language:settings-preferences-symbol))
                (set! user-custodian (make-custodian))
                (set! user-eventspace (parameterize ([current-custodian user-custodian])
                                        (make-eventspace)))
                (set! user-break-enabled #t)
                (set! eval-thread-thunks null)
                (set! eval-thread-state-sema (make-semaphore 1))
                (set! eval-thread-queue-sema (make-semaphore 0))
                
                (let ([init-thread-complete (make-semaphore 0)]
                      [goahead (make-semaphore)]
                      [o (current-output-port)]
		      [queue-user/wait
		       (lambda (thnk)
			 (let ([wait (make-semaphore 0)])
			   (parameterize ([current-eventspace user-eventspace])
			     (queue-callback
			      (lambda ()
				(thnk)
				(semaphore-post wait))))
			   (semaphore-wait wait)))])

		  ;; setup standard parameters
		  (queue-user/wait
		   (lambda () ; =User=, =No-Breaks=
		     ;; No user code has been evaluated yet, so we're in the clear...
		     (break-enabled #f)
		     (set! user-thread (current-thread))
		     (initialize-parameters)))

		  ;; initialize the language
		  (send (drscheme:language:language-settings-language user-language-settings)
			on-execute
			(drscheme:language:language-settings-settings user-language-settings)
			queue-user/wait)
                
		  (parameterize ([current-eventspace user-eventspace])
		    (queue-callback
		     (lambda ()
		       (let ([drscheme-error-escape-handler
			      (lambda ()
				(error-escape-k))])
			 (error-escape-handler drscheme-error-escape-handler)
			 (bottom-escape-handler drscheme-error-escape-handler))
                       
		       (set! in-evaluation? #f)
		       (update-running)
		     
                       ;; let init-thread procedure return,
                       ;; now that parameters are set
                       (semaphore-post init-thread-complete)
                       
                       ; We're about to start running user code.
                       
                       ; Pause to let killed-thread get initialized
                       (semaphore-wait goahead)
                       
                       (let loop () ; =User=, =Handler=, =No-Breaks=
                         ; Wait for something to do
                         (unless (semaphore-try-wait? eval-thread-queue-sema)
                           ; User event callbacks run here; we turn on
                           ;  breaks in the dispatch handler.
                           (yield eval-thread-queue-sema))
                         ; About to eval something
                         (semaphore-wait eval-thread-state-sema)
                         (let ([thunk (car eval-thread-thunks)])
                           (set! eval-thread-thunks (cdr eval-thread-thunks))
                           (semaphore-post eval-thread-state-sema)
                           ; This thunk evals the user's expressions with appropriate
                           ;   protections.
                           (thunk))
                         (loop)))))
                  (semaphore-wait init-thread-complete)
                  ; Start killed-thread
                  (initialize-killed-thread)
                  ; Let user expressions go...
                  (semaphore-post goahead))))
            
            (field (shutting-down? #f))
            
            (define (on-close)
              (shutdown)
              (super-on-close))
            
            (define shutdown ; =Kernel=, =Handler=
              (lambda ()
                (set! shutting-down? #t)
                (when (thread? thread-killed)
                  (kill-thread thread-killed)
                  (set! thread-killed #f))
                (shutdown-user-custodian)))
            
            (define update-running ; =User=, =Handler=, =No-Breaks=
              (lambda ()
                (queue-system-callback
                 user-thread
                 (lambda ()
                   (if in-evaluation?
                       (send context running)
                       (send context not-running))))))
            
            
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;					     ;;;
	;;;                Execution                 ;;;
	;;;					     ;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
            (define insert-delta
              (lambda (s . deltas)
                (let ([before (last-position)])
                  (insert s before before #f)
                  (let ([after (last-position)])
                    (for-each (lambda (delta)
                                (change-style delta before after))
                              deltas)
                    (values before after)))))
            
            (field (repl-initially-active? #f))
            
            (define initialize-parameters ; =User=
              (lambda ()
              	(current-language-settings user-language-settings)
                (error-value->string-handler drscheme-error-value->string-handler)
                (current-exception-handler drscheme-exception-handler)
                (current-load-relative-directory #f)
                (initial-exception-handler drscheme-exception-handler)
                (current-custodian user-custodian)
                (global-port-print-handler drscheme-port-print-handler)
                
                (break-enabled #t)
                (read-curly-brace-as-paren #t)
                (read-square-bracket-as-paren #t)
    
                (error-print-width 250)
                
                (current-rep-text this)
                (current-output-port this-out)
                (current-error-port this-err)
                (current-value-port this-result)
                (current-input-port this-in)
                
                (print-convert:current-print-convert-hook
                 (lambda (expr basic-convert sub-convert)
                   (let ([ans (if (is-a? expr snip%)
                                  expr
                                  (basic-convert expr))])
                     ans)))
                
                (current-load drscheme:load-handler:drscheme-load-handler)
                
                (error-display-handler
                 (rec drscheme-error-display-handler
                   (lambda (msg)
                     (message-box
                      "Uncaught Error"
                      msg
		      (let ([rep (current-rep-text)])
			(and rep
			     (let ([canvas (send rep get-active-canvas)])
			       (and canvas
				    (send canvas get-top-level-window)))))))))
                
                (let ([dir (or (send context get-directory)
                               drscheme:init:first-dir)])
                  (current-directory dir)
                  (current-load-relative-directory dir))
                
                (exit-handler (lambda (arg) ; =User=
                                (custodian-shutdown-all user-custodian)))
                
                (current-namespace (make-namespace 'empty))
                
	    ;; set all parameters before constructing eventspace
	    ;; so that the parameters are set in the eventspace's
	    ;; parameterization
                (let* ([primitive-dispatch-handler (event-dispatch-handler)])
                  (event-dispatch-handler
                   (rec drscheme-event-dispatch-handler ; <= a name for #<...> printout
                     (lambda (eventspace) ; =User=, =Handler=
                       ; Breaking is enabled if the user turned on breaks and
                       ;  is in a `yield'. If we get a break, that's ok, because
                       ;  the kernel never queues an event in the user's eventspace.
                       (cond
                         [(eq? eventspace user-eventspace)
                          ; =User=, =Handler=, =No-Breaks=
                          
                          (let* ([ub? (eq? user-break-enabled 'user)]
                                 [break-ok? (if ub?
                                                (break-enabled)
                                                user-break-enabled)])
                            (break-enabled #f)
                            
                            ; We must distinguish between "top-level" events and
                            ;  those within `yield' in the user's program.
                            
                            (cond
                              [(not in-evaluation?)
                               
                               (reset-break-state) ; Is this a good idea?
                               
                               (protect-user-evaluation
                                ; Run the dispatch:
                                (lambda () ; =User=, =Handler=, =No-Breaks=
                                  ; This procedure is responsible for adjusting breaks to
                                  ;  match the user's expectations:
                                  (dynamic-wind
                                   (lambda () 
                                     (break-enabled break-ok?)
                                     (unless ub?
                                       (set! user-break-enabled 'user)))
                                   (lambda ()
                                     (primitive-dispatch-handler eventspace))
                                   (lambda ()
                                     (unless ub?
                                       (set! user-break-enabled (break-enabled)))
                                     (break-enabled #f))))
                                ; Cleanup after dispatch
                                void)
                               
                               ; Restore break:
                               (when ub?
                                 (break-enabled break-ok?))]
                              [else
                               ; Nested dispatch; don't adjust interface, and restore break:
                               (break-enabled break-ok?)
                               (primitive-dispatch-handler eventspace)]))]
                         [else 
                          ; =User=, =Non-Handler=, =No-Breaks=
                          (primitive-dispatch-handler eventspace)])))))))
            
            (define  reset-console
              (lambda ()
                (when (thread? thread-killed)
                  (kill-thread thread-killed))
                (let ([fr (send (get-canvas) get-top-level-window)])
                  (send context clear-annotations))
                (shutdown-user-custodian)
                (cleanup-transparent-io)
                (clear-previous-expr-positions)
                (set! should-collect-garbage? #t)
                
                (set! eof-received? #f)
                
	    ;; in case the last evaluation thread was killed, clean up some state.
                (lock #f)
                (set! in-evaluation? #f)
                (update-running)
                
                ;; must init-evaluation-thread before determining
                ;; the language's name, since this updates user-language-settings
                (init-evaluation-thread)
                
                (begin-edit-sequence)
                (set-resetting #t)
                (delete (paragraph-start-position 1) (last-position))
                (set-prompt-mode #f)
                (set-resetting #f)
                (set-position (last-position) (last-position))
                
                (insert-delta (string-append (string-constant language) ": ") welcome-delta)
                (insert-delta (extract-language-name user-language-settings) dark-green-delta)
                (unless (is-default-settings? user-language-settings)
                  (insert-delta (string-append " " (string-constant custom)) dark-green-delta))
                (insert-delta (format ".~n") welcome-delta)
                
                (for-each
                 (lambda (fn)
                   (insert-delta (string-append (string-constant teachpack) ": ") welcome-delta)
                   (insert-delta fn dark-green-delta)
                   (insert-delta (format ".~n") welcome-delta))
                 (list "<<teachpacks go here>>"))
                
                (set! repl-initially-active? #t)
                (end-edit-sequence)
                
                (super-reset-console)))
            
            (define initialize-console
              (lambda ()
                (super-initialize-console)
                
                (insert-delta (string-append (string-constant welcome-to) " ") welcome-delta)
                (let-values ([(before after)
                              (insert-delta (string-constant drscheme) click-delta drs-font-delta)])
                  (insert-delta (format (string-append ", " (string-constant version) " ~a.~n") (version:version))
                                welcome-delta)
                  (set-clickback before after 
                                 (lambda args (drscheme:app:about-drscheme))
                                 click-delta))
                (reset-console)
                (insert-prompt)
                (clear-undos)))
            
            (setup-display/write-handlers)
            
            (super-instantiate ())
            
            (set-styles-sticky #f))))
      
      (define make-console-text%
        (lambda (super%)
          (rec console-text%
            (class super% args
              (init-rest args)
              (inherit position-line position-location
                       line-location get-admin
                       set-position set-caret-owner
                       clear-undos insert delete
                       begin-edit-sequence
                       end-edit-sequence
                       run-after-edit-sequence
                       change-style split-snip
                       scroll-to-position is-locked? lock
                       last-position get-start-position get-end-position
                       get-text get-snip-position
                       get-character find-snip find-string
                       erase
                       invalidate-bitmap-cache
                       get-extent get-style-list)
              (rename [super-on-local-char on-local-char]
                      [super-on-paint on-paint]
                      [super-after-set-size-constraint after-set-size-constraint]
                      [super-get-keymaps get-keymaps])
              (rename [super-can-insert? can-insert?]
                      [super-after-insert after-insert]
                      
                      [super-can-delete? can-delete?]
                      [super-after-delete after-delete]
                      
                      [super-can-change-style? can-change-style?]
                      [super-after-change-style after-change-style]
                      
                      [super-on-edit-sequence on-edit-sequence]
                      [super-after-edit-sequence after-edit-sequence]
                      
                      [super-after-set-position after-set-position])
              (override autosave?
                        get-keymaps
                        can-insert?
                        can-delete?
                        can-change-style?
                        after-insert
                        after-delete
                        after-change-style
                        on-edit-sequence
                        after-edit-sequence
                        after-set-position
                        on-local-char)
              
              (public set-resetting
		      get-resetting
                      
                      copy-prev-previous-expr
                      copy-next-previous-expr
                      copy-previous-expr
                      clear-previous-expr-positions
                      set-prompt-mode
                      get-prompt-mode
                      ready-non-prompt
                      
                      balance-required
                      
                      initialize-console
                      
                      reset-pretty-print-width
                      
                      get-prompt
                      insert-prompt
                      set-prompt-position
                      get-prompt-position
                      reset-console
                      
                      do-pre-eval
                      do-eval
                      do-post-eval
                      eval-busy?)
              
              (define autosave? (lambda () #f))
              
              (define edit-sequence-count 0)
              
              (define orig-stdout (current-output-port))
              (define orig-stderr (current-error-port))
              (define normal-delta #f)
              
              (define get-keymaps
                (lambda ()
                  (cons scheme-interaction-mode-keymap (super-get-keymaps))))
              
          ;; used to highlight the prompt that the caret is "in the range of".
          ;; not currently used at all.
              (define find-which-previous-sexp
                (lambda ()
                  (let*-values ([(x y) (values (get-start-position) (get-end-position))])
                    (let loop ([sexps previous-expr-positions])
                      (cond
                        [(null? sexps) (values #f #f)]
                        [(pair? sexps) (let* ([hd (car sexps)]
                                              [left (car hd)]
                                              [right (cdr hd)]
                                              [tl (cdr sexps)])
                                         (if (and (<= left x right)
                                                  (<= left y right))
                                             (values left right)
                                             (loop tl)))])))))
              (define can-something
                (opt-lambda (super start len)
                  (cond
                    [(or resetting?
                         (not (number? prompt-position))
                         (>= start prompt-position))
                     (super start len)]
                    [else #f])))
              (define after-something
                (lambda (combine start len)
                  (when (or resetting?
                            (and prompt-mode? (< start prompt-position)))
                    (set! prompt-position (combine prompt-position len)))))
              
              (field (resetting? #f))
	      (define (get-resetting) resetting?)
              (define (set-resetting v) (set! resetting? v))
              
              (define can-insert?
                (lambda (start len)
                  (can-something (lambda (x y) (super-can-insert? x y)) start len)))
              (define can-delete?
                (lambda (start len)
                  (can-something (lambda (x y) (super-can-delete? x y)) start len)))
              (define can-change-style?
                (lambda (start len)
                  (can-something (lambda (x y) (super-can-change-style? x y)) start len)))
              (define after-insert
                (lambda (start len)
                  (after-something + start len)
                  (super-after-insert start len)))
              (define after-delete
                (lambda (start len)
                  (after-something - start len)
                  (super-after-delete start len)))
              (define after-change-style
                (lambda (start len)
                  (after-something (lambda (start len) start) start len)
                  (super-after-change-style start len)))
              (define on-edit-sequence
                (lambda ()
                  (super-on-edit-sequence)))
              (define after-edit-sequence
                (lambda ()
                  (super-after-edit-sequence)))
              (define after-set-position
                (lambda ()
                  (super-after-set-position)))
              
              (define last-str (lambda (l)
                                 (if (null? (cdr l))
                                     (car l)
                                     (last-str (cdr l)))))
              
              (field (prompt-mode? #f)
		     (prompt-position 0))
              (define (get-prompt-mode) prompt-mode?)
              (define (set-prompt-mode x) (set! prompt-mode? x))
              (define get-prompt (lambda () "> "))
              (define set-prompt-position (lambda (v) (set! prompt-position v)))
	      (define (get-prompt-position) prompt-position)
              (define find-prompt 
                (lambda (pos) 
                  (if (> pos prompt-position)
                      prompt-position
                      0)))

	      (field (balance-required-cell #t))
              (define balance-required
		(case-lambda
		 [() balance-required-cell]
		 [(x) (set! balance-required-cell x)]))
              
              (field (previous-expr-pos -1)
		     (previous-expr-positions null))

              (define clear-previous-expr-positions
                (lambda ()
                  (set! previous-expr-positions null)))
              (define copy-previous-expr
                (lambda ()
                  (let ([snip/strings (list-ref (preferences:get
                                                 'console-previous-exprs) 
                                                previous-expr-pos)])
                    (begin-edit-sequence)
                    (unless prompt-mode?
                      (insert-prompt))
                    (delete prompt-position (last-position) #f)
                    (for-each (lambda (snip/string)
                                (insert (if (is-a? snip/string snip%)
                                            (send snip/string copy)
                                            snip/string)
                                        prompt-position))
                              snip/strings)
                    (set-position (last-position))
                    (end-edit-sequence))))
              (define copy-next-previous-expr
                (lambda ()
                  (let ([previous-exprs (preferences:get 'console-previous-exprs)])
                    (unless (null? previous-exprs)
                      (set! previous-expr-pos
                            (if (< (add1 previous-expr-pos) (length previous-exprs))
                                (add1 previous-expr-pos)
                                0))
                      (copy-previous-expr)))))
              (define copy-prev-previous-expr
                (lambda ()
                  (let ([previous-exprs (preferences:get 'console-previous-exprs)])
                    (unless (null? previous-exprs)
                      (set! previous-expr-pos
                            (if (previous-expr-pos . <= . 0)
                                (sub1 (length previous-exprs))
                                (sub1 previous-expr-pos)))
                      (copy-previous-expr)))))
              
              (define do-save-and-eval
                (lambda (start end)
                  (split-snip start)
                  (split-snip end)
                  (let ([snips
                         (let loop ([snip (find-snip start 'after-or-none)]
                                    [snips null])
                           (cond
                             [(not snip) snips]
                             [((get-snip-position snip) . <= . end)
                              (loop (send snip next)
                                    (cons (send snip copy) snips))]
                             [else snips]))])
                    (set! previous-expr-positions (cons (cons start end) previous-expr-positions))
                    (set! previous-expr-pos -1)
                    (let* ([previous-exprs (preferences:get 'console-previous-exprs)]
                           [new-previous-exprs 
                            (let* ([trimmed-previous-exprs
                                    (if (>= (length previous-exprs) console-max-save-previous-exprs)
                                        (cdr previous-exprs)
                                        previous-exprs)])
                              (let loop ([l trimmed-previous-exprs])
                                (if (null? l)
                                    (list snips)
                                    (cons (car l) (loop (cdr l))))))])
                      (preferences:set 'console-previous-exprs new-previous-exprs))
                    (do-eval start end))))
              
              (define reset-pretty-print-width
                (lambda ()
                  (let* ([standard (send (get-style-list) find-named-style "Standard")])
                    (when standard
                      (let* ([admin (get-admin)]
                             [width
                              (let ([bw (box 0)]
                                    [b2 (box 0)])
                                (send admin get-view b2 b2 bw b2)
                                (unbox bw))]
                             [dc (send admin get-dc)]
                             [new-font (send standard get-font)]
                             [old-font (send dc get-font)])
                        (send dc set-font new-font)
                        (let* ([char-width (send dc get-char-width)]
                               [min-columns 50]
                               [new-columns (max min-columns 
                                                 (floor (/ width char-width)))])
                          (send dc set-font old-font)
                          (mzlib:pretty-print:pretty-print-columns new-columns)))))))
              (define do-eval
                (lambda (start end)
                  (error 'do-eval "abstract method")))
              (define do-pre-eval
                (lambda ()
                  (ready-non-prompt)))
              (define do-post-eval
                (lambda ()
                  (insert-prompt)))
              
              (define only-spaces-after
                (lambda (pos)
                  (let ([last (last-position)])
                    (let loop ([pos pos])
                      (if (= pos last)
                          #t
                          (let ([c (get-character pos)])
                            (if (char-whitespace? c)
                                (loop (add1 pos))
                                #f)))))))
              
              (define eval-busy? (lambda () #f))
              
              (define on-local-char
                (lambda (key)
                  (let ([start (get-start-position)]
                        [end (get-end-position)]
                        [last (last-position)]
                        [code (send key get-key-code)]
                        [copy-to-end/set-position
                         (lambda (start end)
                           (split-snip start)
                           (split-snip end)
                           (let loop ([snip (find-snip start 'after)])
                             (cond
                               [(not snip) (void)]
                               [(< (get-snip-position snip) end)
                                (insert (send snip copy) (last-position))
                                (loop (send snip next))]
                               [else (void)]))
                           (set-position (last-position)))])
                    (cond
                      [(not (and (char? code) 
                                 (or (char=? code #\return) (char=? code #\newline))))
                       (super-on-local-char key)]
                      [(and (< start end) (< end prompt-position)
                            (not (eval-busy?)))
                       (begin-edit-sequence)
                       (when (not prompt-mode?)
                         (insert-prompt))
                       (copy-to-end/set-position start end)
                       (end-edit-sequence)]
                      [(and (= start last) 
                            (not prompt-mode?)
                            (not (eval-busy?)))
                       (insert-prompt)]
                      [(and (prompt-position . <= . start)
                            (only-spaces-after start)
                            (not (eval-busy?)))
                       (if (balance-required)
                           (let ([balanced? (scheme-paren:balanced?
                                             this
                                             prompt-position
                                             last)])
                             (if balanced?
                                 (begin
                                   (delete start last)
                                   (do-save-and-eval prompt-position start))
                                 (super-on-local-char key)))
                           (begin
                             (delete start last)
                             (do-save-and-eval prompt-position start)))]
                      [(< start prompt-position)
                       (let ([match (scheme-paren:backward-match
                                     this start 0)])
                         (if match
                             (begin
                               (begin-edit-sequence)
                               (copy-to-end/set-position match start)
                               (end-edit-sequence))
                             (super-on-local-char key)))]
                      [else
                       (super-on-local-char key)]))))
              
              (field (inserting-prompt #f))
              (define (insert-prompt)
                (set! prompt-mode? #t)
                (fluid-let ([inserting-prompt #t])
                  (begin-edit-sequence)
                  (let* ([last (last-position)]
                         [c-locked? (is-locked?)]
                         [start-selection (get-start-position)]
                         [end-selection (get-end-position)]
                         [last-str (if (= last 0)
                                       ""
                                       (get-text (- last 1) last))])
                    (lock #f)
                    (unless (or (string=? last-str newline-string)
                                (= last 0))
                      (insert #\newline last))
                    (let ([last (last-position)])
                      (insert (get-prompt) last)
                      (change-style normal-delta last (last-position)))
                    (set! prompt-position (last-position))
                    ;(clear-undos)
                    (lock c-locked?)
                    (end-edit-sequence)
                    (scroll-to-position start-selection #f (last-position) 'start))))
              (define reset-console
                (lambda ()
                  (void)))
              (define ready-non-prompt
                (lambda ()
                  (when prompt-mode?
                    (set! prompt-mode? #f)
                    (let ([c-locked (is-locked?)])
                      (begin-edit-sequence)
                      (lock #f)
                      (insert #\newline (last-position))
                      (lock c-locked)	   
                      (end-edit-sequence)))))	  
              (define initialize-console
                (lambda ()
                  #t))
              (super-make-object)))))
      
      (define input-delta (make-object style-delta%))
      (send input-delta set-delta-foreground (make-object color% 0 150 0))
      (define transparent-io-text<%> 
        (interface ()
          set-program-output
          get-insertion-point))
      
      (define transparent-io-super% 
        (make-console-text%
         (scheme:text-mixin
          text:searching%)))
      
      (define consumed-delta (make-object style-delta% 'change-bold))
	   
      (define transparent-io-text%
        (class100* transparent-io-super% (transparent-io-text<%>) (_rep-text)
          (inherit change-style
                   get-resetting set-resetting lock get-text
                   set-position last-position get-character
                   clear-undos set-cursor
                   do-pre-eval do-post-eval balance-required)
          (rename [super-after-insert after-insert]
                  [super-on-local-char on-local-char])
          (private-field
	   [rep-text _rep-text]
           [data null]
           [stream-start 0]
           [stream-end 0]
           [shutdown? #f])
          
          (private-field
           [stream-start/end-protect (make-semaphore 1)]
           [wait-for-sexp (make-semaphore 0)]
           [eof-submitted? #f])
          (public
            [get-insertion-point
             (lambda ()
               stream-start)]
            [shutdown
             (lambda ()
               (set! shutdown? #t)
               (semaphore-post wait-for-sexp)
               (lock #t))]
            [mark-consumed
             (lambda (start end)
               (let ([old-resetting (get-resetting)])
                 (set-resetting #t)
                 (change-style consumed-delta start end)
                 (set-resetting old-resetting)))]
            [check-char-ready? ; =Reentrant=
             (lambda ()
               (semaphore-wait stream-start/end-protect)
               (begin0
                 (cond
                   [(< stream-start stream-end) #t]
                   [else #f])
                 (semaphore-post stream-start/end-protect)))]
            
            [eof-received
             (lambda () ; =Kernel=, =Handler=
               (set! eof-submitted? #t)
               (set! stream-end (last-position))
               (semaphore-post wait-for-sexp))]
            [fetch-char ; =Kernel=, =Handler=, =Non-Reentrant= (queue requests externally)
             (lambda ()
               (send rep-text show-eof-icon)
               (let* ([ready-char #f])
                 (let loop ()
                   (semaphore-wait stream-start/end-protect)
                   (cond
                     [(< stream-start stream-end)
                      (let ([s stream-start])
                        (mark-consumed s (add1 s))
                        (set! stream-start (add1 s))
                        (set! ready-char (get-character s)))]
                     [eof-submitted?
                      (set! ready-char eof)]
                     [else (void)])
                   (semaphore-post stream-start/end-protect)
                   (or ready-char
                       (begin
                         (yield wait-for-sexp)
                         (if shutdown? 
                             eof
                             (loop)))))))])
          (override
            [get-prompt (lambda () "")])
          (private-field
           [program-output? #f])
          (public
            [set-program-output
             (lambda (_program-output?)
               (set! program-output? _program-output?))])
          (override
            [after-insert
             (lambda (start len)
               (super-after-insert start len)
               (when program-output?
                 (when (start . <= . stream-start)
                   (set! stream-start (+ stream-start len))))
               (unless program-output?
                 (let ([old-r (get-resetting)])
                   (set-resetting #t)
                   (change-style input-delta start (+ start len))
                   (set-resetting old-r))))])
          (override
            [do-eval
             (lambda (start end)
               (do-pre-eval)
               (set! stream-end (+ end 1))
               (semaphore-post wait-for-sexp)
               (do-post-eval))])
          (inherit insert-prompt)
          (sequence
            (super-init)
            (insert-prompt))))
      
      (define text% 
        (drs-bindings-keymap-mixin
         (make-text% 
          (make-console-text% scheme:text%)))))))
