; =Kernel= means in DrScheme's thread and parameterization
; 
; =User= means the user's thread and parameterization
; 
; =Handler= means in the handler thread of some eventspace; it must
;  be combined with either =Kernel= or =User=

;; WARNING: printf is rebound in this module to always use the 
;;          original stdin/stdout of drscheme, instead of the 
;;          user's io ports, to aid any debugging printouts.
;;          (esp. useful when debugging the users's io)
(module rep mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "file.ss")
           (lib "pretty.ss")
           (lib "etc.ss")
           (lib "list.ss")
           "drsig.ss"
           "syntax-browser.ss"
           (lib "string-constant.ss" "string-constants")
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
	   (lib "external.ss" "browser")
           (lib "default-lexer.ss" "syntax-color"))
  
  (provide rep@)
  
  (define rep@
    (unit/sig drscheme:rep^
      (import (drscheme:init : drscheme:init^)
              (drscheme:number-snip : drscheme:number-snip^)
              (drscheme:language-configuration : drscheme:language-configuration/internal^)
	      (drscheme:language : drscheme:language^)
              (drscheme:app : drscheme:app^)
              (drscheme:frame : drscheme:frame^)
              (drscheme:unit : drscheme:unit^)
              (drscheme:text : drscheme:text^)
              (drscheme:help-desk : drscheme:help-desk^)
              (drscheme:teachpack : drscheme:teachpack^)
              (drscheme:debug : drscheme:debug^)
              [drscheme:eval : drscheme:eval^])
      
      (rename [-text% text%]
              [-text<%> text<%>])

      ;; locs = (listof (list text% number number))
      (define-struct (exn:locs exn) (locs))
      
      (define sized-snip<%>
	(interface ((class->interface snip%))
	  ;; get-character-width : -> number
	  ;; returns the number of characters wide the snip is,
	  ;; for use in pretty printing the snip.
	  get-character-width))

      ;; use-number-snip : (parameter (TST -> boolean))
      ;; returns true if this value can be turned into a number snip for displaying
      (define use-number-snip
        (make-parameter
         (lambda (x)
           (and (number? x)
                (exact? x)
                (rational? x)
                (not (integer? x))))))

      ;; which-number-snip : (parameter
      ;;                      (number -> (union 'mixed-fraction
      ;;                                        'repeating-decimal-e
      ;;                                        'repeating-decimal)))
      (define which-number-snip
        (make-parameter
         (lambda (x)
           'mixed-fraction)))
      
      ;; current-language-settings : (parameter language-setting)
      ;; set to the current language and its setting on the user's thread.
      (define current-language-settings (make-parameter #f))
      
      ;; current-rep : (parameter (union #f (instanceof rep:text%)))
      ;; the repl that controls the evaluation in this thread.
      (define current-rep (make-parameter #f))

      ;; a port that accepts values for printing in the repl
      (define current-value-port (make-parameter #f))

      ;; an error escape continuation that the user program can't
      ;; change; DrScheme sets it, we use a parameter instead of an
      ;; object field so that there's no non-weak pointer to the
      ;; continuation from DrScheme.
      (define current-error-escape-k (make-parameter void))
            
      ;; drscheme-error-display-handler : (string (union #f exn) -> void
      ;; =User=
      ;; the timing is a little tricky here. 
      ;; the file icon must appear before the error message in the text, so that happens first.
      ;; the highlight must be set after the error message, because inserting into the text resets
      ;;     the highlighting.
      (define (drscheme-error-display-handler msg exn)
        (let ([rep (current-rep)]
              [user-dir (current-directory)])
          (cond
            [(and (is-a? rep -text<%>)
                  (eq? (current-error-port) (send rep get-this-err)))
             (send rep queue-output
                   (lambda ()  ;; =Kernel=, =Handler=
                     (insert-error-in-text rep rep msg exn user-dir)
                     (let ([context (send rep get-context)])
                       (send context ensure-rep-shown))))]
            [else
             (display msg (current-error-port))
             (newline (current-error-port))])))

      ;; insert-error-in-text : (is-a?/c text%)
      ;;                        (union #f (is-a?/c drscheme:rep:text<%>))
      ;;                        string?
      ;;                        exn?
      ;;                        (union false? (and/c string? directory-exists?))
      ;;                        ->
      ;;                        void?
      (define (insert-error-in-text text interactions-text msg exn user-dir)
        (insert-error-in-text/highlight-errors
         text
         (lambda (l) (send interactions-text highlight-errors l))
         msg
         exn
         user-dir))
      
      ;; insert-error-in-text/highlight-errors : (is-a?/c text%)
      ;;                                         ((listof (list text% number number)) -> void)
      ;;                                         string?
      ;;                                         exn?
      ;;                                         (union false? (and/c string? directory-exists?))
      ;;                                         ->
      ;;                                         void?
      (define (insert-error-in-text/highlight-errors text highlight-errors msg exn user-dir)
        (let ([locked? (send text is-locked?)]
              [insert-file-name/icon
               ;; insert-file-name/icon : string number number number number -> void
               (lambda (source-name start span row col)
                 (let* ([range-spec
                         (cond
                           [(and row col)
                            (format ":~a:~a" row col)]
                           [start
                            (format "::~a" start)]
                           [else ""])])
                   (cond
                     [(file-exists? source-name)
                      (let* ([normalized-name (normalize-path source-name)]
                             [short-name (if user-dir
                                             (find-relative-path user-dir normalized-name)
                                             source-name)])
                        (let-values ([(icon-start icon-end) (insert/delta text (send file-icon copy))]
                                     [(space-start space-end) (insert/delta text " ")]
                                     [(name-start name-end) (insert/delta text short-name)]
                                     [(range-start range-end) (insert/delta text range-spec)]
                                     [(colon-start colon-ent) (insert/delta text ": ")])
                          (when (number? start)
                            (send text set-clickback icon-start range-end
                                  (lambda (_1 _2 _3)
                                    (open-file-and-highlight normalized-name
                                                             (- start 1) 
                                                             (if span
                                                                 (+ start -1 span)
                                                                 start)))))))]
                     [else
                      (insert/delta text source-name)
                      (insert/delta text range-spec)
                      (insert/delta text ": ")])))])
          (send text begin-edit-sequence)
          (send text lock #f)
          (cond
            [(exn:syntax? exn)
             (let* ([expr (exn:syntax-expr exn)]
                    [src (and (syntax? expr) (syntax-source expr))]
                    [pos (and (syntax? expr) (syntax-position expr))]
                    [span (and (syntax? expr) (syntax-span expr))]
                    [col (and (syntax? expr) (syntax-column expr))]
                    [line (and (syntax? expr) (syntax-line expr))])
               (when (and (string? src)
                          (number? pos)
                          (number? span)
                          (number? line)
                          (number? col))
                 (insert-file-name/icon src pos span line col))
               (insert/delta text (format "~a" (exn-message exn)) error-delta)
               (when (syntax? expr)
                 (insert/delta text " in: ")
                 (insert/delta text (format "~s" (syntax-object->datum expr)) error-text-style-delta))
               (insert/delta text "\n")
               (when (and (is-a? src text:basic%)
                          (number? pos)
                          (number? span))
                 (highlight-errors (list (list src (- pos 1) (+ pos -1 span))))))]
            [(exn:read? exn)
             (let ([src (exn:read-source exn)]
                   [pos (exn:read-position exn)]
                   [span (exn:read-span exn)]
                   [line (exn:read-line exn)]
                   [col (exn:read-column exn)])
               (when (and (string? src)
                          (number? pos)
                          (number? span)
                          (number? line)
                          (number? col))
                 (insert-file-name/icon src pos span line col))
               (insert/delta text (format "~a" (exn-message exn)) error-delta)
               (insert/delta text "\n")
               (when (and (is-a? src text:basic%)
                          (number? pos)
                          (number? span))
                 (highlight-errors (list (list src (- pos 1) (+ pos -1 span))))))]
            [(exn:locs? exn)
             (let ([locs (exn:locs-locs exn)])
               (insert/delta text (format "~a" (exn-message exn)) error-delta)
               (insert/delta text "\n")
               (when (andmap (lambda (loc)
                               (and (list? loc)
                                    (= 3 (length loc))
                                    (is-a? (first loc) text:basic%)
                                    (number? (second loc))
                                    (number? (third loc))))
                             locs)
                 (highlight-errors locs)))]
            [(exn? exn)
             (insert/delta text (format "~a" (exn-message exn)) error-delta)
             (insert/delta text "\n")]
            [else
             (insert/delta text "uncaught exception: " error-delta)
             (insert/delta text (format "~s" exn) error-delta)
             (insert/delta text "\n")])
          (send text lock locked?)
          (send text end-edit-sequence)))
      
      ;; open-file-and-highlight : string (union number #f) (union number #f)
      ;; =Kernel, =Handler=
      ;; opens the file named by filename. If position is #f,
      ;; doesn't highlight anything. If position is a number and other-position
      ;; is #f, highlights the range from position to the end of sexp.
      ;; if other-position is a number, highlights from position to 
      ;; other position.
      (define (open-file-and-highlight filename position other-position)
        (let ([file (handler:edit-file filename)])
          (when (and (is-a? file drscheme:unit:frame%)
                     position)
            (if other-position
                (send (send file get-interactions-text)
                      highlight-error
                      (send file get-definitions-text)
                      position
                      other-position)
                (send (send file get-interactions-text)
                      highlight-error/forward-sexp
                      (send file get-definitions-text)
                      position)))))
      
      ;; drscheme-error-value->string-handler : TST number -> string
      (define (drscheme-error-value->string-handler x n)
        (let ([port (open-output-string)])
          
          ;; using a string port here means no snips allowed,
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

      (define drs-bindings-keymap (make-object keymap:aug-keymap%))
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
            "search-help-desk"
            (lambda (obj evt)
              (cond
                [(is-a? obj text%)
                 (let* ([start (send obj get-start-position)]
                        [end (send obj get-end-position)]
                        [str (if (= start end)
                                 (drscheme:unit:find-symbol obj start)
                                 (send obj get-text start end))])
                   (if (equal? "" str)
                       (drscheme:help-desk:help-desk)
                       (drscheme:help-desk:help-desk str #f 'keyword+index 'contains)))]
                [else                   
                 (drscheme:help-desk:help-desk)])))
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
      (send drs-bindings-keymap map-function "f1" "search-help-desk")
      
      (define (get-drs-bindings-keymap) drs-bindings-keymap)

      ;; drs-bindings-keymap-mixin :
      ;;   ((implements editor:keymap<%>) -> (implements editor:keymap<%>))
      ;;   for any x that is an instance of the resulting class,
      ;;     (is-a? (send (send x get-canvas) get-top-level-frame) drscheme:unit:frame%)
      (define drs-bindings-keymap-mixin
	(mixin (editor:keymap<%>) (editor:keymap<%>)
	  (rename [super-get-keymaps get-keymaps])
	  (override get-keymaps)
	  [define (get-keymaps)
	    (cons drs-bindings-keymap (super-get-keymaps))]
	  (super-instantiate ())))
      
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
      
      (define scheme-interaction-mode-keymap (make-object keymap:aug-keymap%))
      (setup-scheme-interaction-mode-keymap scheme-interaction-mode-keymap)
      
      (define drs-font-delta (make-object style-delta% 'change-family 'decorative))
      
      (define output-delta (make-object style-delta%)) ; used to be 'change-weight 'bold
      (define result-delta (make-object style-delta%)) ; used to be 'change-weight 'bold
      (define error-delta (make-object style-delta%
                            'change-style
                            'slant))
      (send error-delta set-delta-foreground (make-object color% 255 0 0))
      (send result-delta set-delta-foreground (make-object color% 0 0 175))
      (send output-delta set-delta-foreground (make-object color% 150 0 150))
      
      (define error-text-style-delta (make-object style-delta%))
      (send error-text-style-delta set-delta-foreground (make-object color% 200 0 0))

      (define grey-delta (make-object style-delta%))
      (send grey-delta set-delta-foreground "GREY")
      
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
        (send (drscheme:language-configuration:language-settings-language language-settings)
              default-settings?
              (drscheme:language-configuration:language-settings-settings language-settings)))

      (define (extract-language-name language-settings)
        (send (drscheme:language-configuration:language-settings-language language-settings)
              get-language-name))
      (define (extract-language-style-delta language-settings)
        (send (drscheme:language-configuration:language-settings-language language-settings)
              get-style-delta))
      (define (extract-language-url language-settings)
        (send (drscheme:language-configuration:language-settings-language language-settings)
              get-language-url))

      (define-struct sexp (left right prompt))
      
      (define newline-string "\n")
      
      (define console-max-save-previous-exprs 30)
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
      
      (define error-color (make-object color% "PINK"))
      (define color? ((get-display-depth) . > . 8))
            
      ;; instances of this interface provide a context for a rep:text%
      ;; its connection to its graphical environment (ie frame) for
      ;; error display and status infromation is all mediated
      ;; through an instance of this interface.
      (define context<%>
        (interface ()
          ensure-rep-shown   ;; (-> void)
	  ;; make the rep visible in the frame

          needs-execution?   ;; (-> boolean)
	  ;; ask if things have changed that would mean the repl is out
	  ;; of sync with the program being executed in it.
          
          enable-evaluation  ;; (-> void)
	  ;; make the context enable all methods of evaluation
	  ;; (disable buttons, menus, etc)

          disable-evaluation ;; (-> void)
	  ;; make the context enable all methods of evaluation
	  ;; (disable buttons, menus, etc)
          
          set-breakables ;; (union thread #f) (union custodian #f) -> void
          ;; the context might initiate breaks or kills to
          ;; the thread passed to this function

          get-breakables ;; -> (values (union thread #f) (union custodian #f))
          ;; returns the last values passed to set-breakables.

          reset-offer-kill ;; (-> void)
          ;; the next time the break button is pushed, it will only
          ;; break. (if the break button is clicked twice without
          ;; this method being called in between, it will offer to
          ;; kill the user's program)

          running            ;; (-> void)
	  ;; a callback to indicate that the repl is evaluating code

          not-running        ;; (-> void)
	  ;; a callback to indicate that the repl is not evaluating code
          
          clear-annotations  ;; (-> void)
	  ;; clear any error highlighting context
          
          get-directory      ;; (-> (union #f string[existing directory]))
	  ;; returns the directory that should be the default for
	  ;; the `current-directory' and `current-load-relative-directory'
	  ;; parameters in the repl.
	  ))

      
      (define file-icon
        (let ([bitmap
               (make-object bitmap%
                 (build-path (collection-path "icons") "file.gif"))])
          (if (send bitmap ok?)
              (make-object image-snip% bitmap)
              (make-object string-snip% "[open file]"))))
      (define docs-icon
        (let ([bitmap
               (make-object bitmap%
                 (build-path (collection-path "icons") "book.gif"))])
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
        (set! busy-cursor #f))
      
      (define arrow-cursor (make-object cursor% 'arrow))
      (define eof-icon-snip%
        (class image-snip% 
          (init-field rep)
          (rename [super-get-extent get-extent])
          (define/override (get-extent dc x y w h descent space lspace rspace)
            (super-get-extent dc x y w h descent space lspace rspace)
            (when (box? descent) (set-box! descent 7)))
          (rename [super-on-event on-event])
          (define/override (on-event dc x y editor-x editor-y evt)
            (cond
              [(send evt get-left-down) 
               (send rep submit-eof)]
              [else (super-on-event dc x y editor-x editor-y evt)]))
          (define/override (adjust-cursor dc x y editorx editory evt)
            arrow-cursor)
          (inherit get-flags set-flags)
          (super-make-object (build-path (collection-path "icons") "eof.gif"))
          (set-flags (cons 'handles-events (get-flags)))))
      
      ;; error-ranges : (union false? (cons (list file number number) (listof (list file number number))))
      (define error-ranges #f)
      ;; error-arrows : (union #f (listof (cons editor<%> number)))
      (define error-arrows #f)
      (define (get-error-ranges) error-ranges)
      (define internal-reset-callback void)
      (define internal-reset-error-arrows-callback void)
      (define (reset-error-ranges) 
        (internal-reset-callback)
        (internal-reset-error-arrows-callback))
      
      (define error-range/reset-callback-semaphore (make-semaphore 1))
      
      ;; insert/delta : (instanceof text%) (union snip string) (listof style-delta%) *-> (values number number)
      ;; inserts the string/stnip into the text at the end and changes the
      ;; style of the newly inserted text based on the style deltas.
      (define (insert/delta text s . deltas)
        (let ([before (send text last-position)])
          (send text insert s before before #f)
          (let ([after (send text last-position)])
            (for-each (lambda (delta)
                        (when (is-a? delta style-delta%)
                          (send text change-style delta before after)))
                      deltas)
            (values before after))))

      (define console-text<%>
        (interface ()
          reset-console
          set-resetting
          get-resetting
          
          get-inserting-prompt
          
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
          
          
          do-pre-eval
          do-eval
          do-post-eval
          eval-busy?))

      (define -text<%>
        (interface ()
          reset-highlighting
          highlight-errors
          highlight-error
          highlight-error/forward-sexp
          
          get-user-custodian
          get-user-eventspace
          get-user-thread
          get-user-namespace
          get-user-teachpack-cache
          set-user-teachpack-cache
          
          kill-evaluation
          
          display-results
          
          run-in-evaluation-thread
          do-many-evals
          do-many-text-evals
          after-many-evals
          
          shutdown
          
          cleanup-transparent-io
          get-this-err
          this-err-write
          get-this-out
          this-out-write
          get-this-result
          this-result-write
          
          get-this-in
          submit-eof
          show-eof-icon
          hide-eof-icon
          
          wait-for-io-to-complete/user
          wait-for-io-to-complete
          queue-output
          
          get-error-ranges))

      (define text-mixin
        (mixin ((class->interface text%) editor:basic<%> scheme:text<%> console-text<%> color:text<%>) (-text<%>)
          (init-field context)
          (inherit insert change-style get-canvas
                   get-active-canvas
                   set-styles-sticky
                   get-style-list
                   clear-undos set-caret-owner
                   clear-previous-expr-positions
                   get-end-position
                   set-clickback
                   do-post-eval
                   erase 
                   get-prompt-mode
                   ready-non-prompt
                   set-prompt-mode
                   delete lock is-locked?
                   paragraph-start-position
                   last-position
                   set-resetting
                   position-line
                   position-paragraph
                   set-position
                   begin-edit-sequence
                   end-edit-sequence
                   reset-pretty-print-width
                   scroll-to-position
                   get-admin
                   set-prompt-position
                   get-canvases find-snip
                   release-snip
                   reset-region update-region-end)
          (rename [super-initialize-console initialize-console]
                  [super-reset-console reset-console]
                  [super-on-close on-close])
          
          (override on-close)
          
          (override get-prompt eval-busy? do-eval
                    initialize-console
                    reset-console)
          
          (public
            reset-highlighting
            highlight-error
            highlight-error/forward-sexp
            
            
            kill-evaluation
            
            display-results
            
            run-in-evaluation-thread
            do-many-evals
            do-many-text-evals
            
            shutdown
            
            cleanup-transparent-io
            get-this-err
            this-err-write
            get-this-out
            this-out-write
            get-this-result
            this-result-write
            
            get-this-in
            submit-eof
            show-eof-icon
            hide-eof-icon)
          
          (unless (is-a? context context<%>)
            (error 'drscheme:rep:text% 
                   "expected an object that implements drscheme:rep:context<%> as initialization argument, got: ~e"
                   context))
          
          (define/public (get-context) context)
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;					       ;;;
          ;;;            User -> Kernel                ;;;
          ;;;					       ;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          (define protect
            (lambda (proc)
              (let ([ut (get-user-thread)]
		    [breaks-on? (break-enabled)])
                (call-in-nested-thread
                 (lambda ()
                   (break-enabled #f)
                   (proc ut breaks-on?))
                 drscheme:init:system-custodian))))
          
          ;; =User= (probably doesn't matter)
          (define queue-system-callback
            (opt-lambda (ut thunk [always? #f])
              (parameterize ([current-eventspace drscheme:init:system-eventspace])
                (queue-callback 
                 (lambda ()
                   (when (or always? (eq? ut (get-user-thread)))
                     (thunk)))
                 #f))))
          
          ;; =User=
          (define queue-system-callback/sync
            (lambda (ut thunk)
              (let ([s (make-semaphore 0)])
                (queue-system-callback 
                 ut 
                 (lambda ()
                   (when (eq? ut (get-user-thread))
                     (thunk))
                   (semaphore-post s))
                 #t)
                (semaphore-wait s))))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;                                          ;;;
          ;;;                  I/O                     ;;;
          ;;;                                          ;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          (field (transparent-text #f)
                 (transparent-snip #f))
          
          (define (cleanup-transparent-io) ; =Kernel=, =Handler=
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
		(drop-fetcher)))
          
          (define init-transparent-io ; =Kernel=, =Handler=
            (lambda (grab-focus?)
              (begin-edit-sequence)
              (if transparent-text
                  (when grab-focus?
                    (let ([a (send transparent-text get-admin)])
                      (when a
                        (send a grab-caret))))
                  (init-transparent-io-do-work grab-focus?))
              (when  (eq? (current-thread) (get-user-thread))
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
                  (send transparent-text set-styles-fixed #t)
                  
                  ;; ensure that there is a newline before the snip is inserted
                  (unless (member 'hard-newline
                                  (send (find-snip (last-position) 'before) get-flags))
                    (insert newline-string (last-position) (last-position) #f))
                  
                  (when starting-at-prompt-mode?
                    (set-prompt-mode #f))
                  
                  (let ([snip (make-object editor-snip% transparent-text)])
                    (set! transparent-snip snip)
                    (insert snip (last-position) (last-position) #f)
                    (insert newline-string (last-position) (last-position) #f)
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
                (class object%
                  (field
                   [fetch-char-sema (make-semaphore 1)]
                   [fetcher-spawned? #f]
                   [char-fetched-sema (make-semaphore)]
                   [char-fetched #f]
		   [prefetched (cons 'dummy null)]) ; for peek lookaheads
                  [define/public fetch ; =Protected-User=
		     ;; Returns one of:
		     ;;   - character (for success)
		     ;;   - #f (nothing ready, wait on maybe-char-ready-sema)
		     ;;   - a semaphore (nothing ready, wait on the given semaphore)
                     (lambda (ut peek? skip)
		       ;; Only one reader at a time:
                       (if (semaphore-try-wait? fetch-char-sema)
			   ;; Now we're the active reader...
			   ;; First look for a pre-fetched char:
			   (let ([prefetched-char
				  (if (not peek?)
				      ;; No peek, so skip is 0
				      (if (pair? (cdr prefetched))
					  (begin0
					   (cadr prefetched)
					   (set-cdr! prefetched (cddr prefetched))
					   ;; If prefetched went empty, then an in-progress
					   ;; spawned fetcher is now fetching a normal
					   ;; character, instead of a pre-character;
					   ;; set up a thread to propagate the sema post:
					   (when fetcher-spawned?
					     (thread
					      (lambda ()
						;; Wait until lookahead done
						(semaphore-wait maybe-later-char-ready-sema)
						;; Let others know that the lookahead is done
						(semaphore-post maybe-later-char-ready-sema)
						;; Let others know that an immediate char is ready
						(semaphore-post maybe-char-ready-sema)))))
					  #f)
				      ;; Peeking...
				      (let loop ([prefetched (cdr prefetched)]
						 [skip skip])
					(cond
					 [(null? prefetched) #f]
					 [(zero? skip) (car prefetched)]
					 [else (loop (cdr prefetched) (sub1 skip))])))])
			     (if prefetched-char
				 ;; Got our char; let another reader go and return
				 (begin
				   (semaphore-post fetch-char-sema)
				   prefetched-char)
				 ;; Need to work with a fetcher...
				 (begin
				   (unless fetcher-spawned?
				     ;; Need to give the fetched a sema to post when it's done;
				     ;; this semaphore is the one attached to the port,
				     ;; or returned by the peek-char function.
				     (let ([maybe-ready-sema (if (zero? skip)
								 ;; Normal sema:
								 maybe-char-ready-sema
								 ;; New sema for this look-ahead:
								 (let ([s (make-semaphore 1)])
								   (set! maybe-later-char-ready-sema s)
								   s))])
				       ;; Going to spawn a catcher; no new chars
				       ;; ready until the fetcher succeeds.
				       (semaphore-wait maybe-ready-sema)
				       (set! fetcher-spawned? #t)
				       ;; Spawn a fetcher:
				       (queue-system-callback
					ut
					(lambda () ; =Kernel=, =Handler=
					  (if eof-received?
					      (begin 
						(set! char-fetched eof)
						(set! eof-received? #t))
					      (let ([text (init-transparent-input)])
						(set! char-fetched (send text fetch-char))
						;; fetch-char might return void
						(when (eof-object? char-fetched)
						  (set! eof-received? #t))))
					  (semaphore-post maybe-ready-sema)
					  (semaphore-post char-fetched-sema)))))
				   ;; Look for a char
				   (if (semaphore-try-wait? char-fetched-sema)
				       ;; Got a char from the most recent fetcher
				       (begin
					 ;; Next reader'll have to spawn a fetcher
					 ;; for new chars.
					 (set! fetcher-spawned? #f)
					 (begin0
					  (if (not peek?)
					      ;; Return the result
					      char-fetched
					      ;; Enqueue the char, then return #f so
					      ;; another call will check the queue:
					      (begin
						(let loop ([pf prefetched])
						  (if (null? (cdr pf))
						      (set-cdr! pf (cons char-fetched null))
						      (loop (cdr pf))))
						#f))
					  ;; Got our char; let another reader go
					  (semaphore-post fetch-char-sema)))
				       ;; No char
				       (begin0
					(if (and peek? (positive? skip))
					    ;; Return a sema to block on; this
					    ;; will get posted when a lookahead succeeds
					    maybe-later-char-ready-sema
					    ;; Not peeking: just return #f
					    #f)
					;; let another reader go
					(semaphore-post fetch-char-sema))))))
			   ;; No char -- couldn't even check for one
			   #f))]
                  (super-instantiate ()))))
          (field (fetcher #f)
                 (fetcher-semaphore (make-semaphore 1))
		 (maybe-char-ready-sema (make-semaphore 1))
		 (maybe-later-char-ready-sema #f)) ; generated as it's needed
          (define drop-fetcher ; =Kernel=, =Handler=
            (lambda ()
              (semaphore-wait fetcher-semaphore)
              (set! fetcher #f)
	      (semaphore-post fetcher-semaphore)))
          (define this-in-fetch-char ; =User=
            (lambda (peek? skip)
              (protect
               (lambda (ut breaks-on?) ; =Protected-User=
                 (semaphore-wait fetcher-semaphore)
                 (unless fetcher (set! fetcher (make-fetcher)))
                 (semaphore-post fetcher-semaphore)
                 (send fetcher fetch ut peek? skip)))))

	  (define this-in-read-char ; =User=
            (lambda ()
	      (this-in-fetch-char #f 0)))
	  (define this-in-peek-char ; =User=
            (lambda (skip)
	      (this-in-fetch-char #t skip)))
          
          (field (flushing-event-running (make-semaphore 1))
                 (limiting-sema (make-semaphore output-limit-size))) ; waited once foreach in io-collected-thunks
          
          (field (io-semaphore (make-semaphore 1))
                 (io-collected-thunks null) ; protected by semaphore
                 (io-collected-texts null)) ; always set in the kernel's handler thread
          
          (define (run-io-collected-thunks) ; =Kernel=, =Handler=
            ; also need to start edit-sequence in any affected
            ; transparent io boxes.
            (semaphore-wait io-semaphore)
            (let ([io-thunks io-collected-thunks])
              (set! io-collected-thunks null)
              (semaphore-post io-semaphore)
              (unless (null? io-thunks)
                (begin-edit-sequence)
                (for-each (lambda (t) (semaphore-post limiting-sema) (t))
                          (reverse io-thunks))
                (for-each (lambda (e) (send e end-edit-sequence)) io-collected-texts)
                (set! io-collected-texts null)
                (scroll-to-position (last-position))
                (end-edit-sequence))))
          
          (define (clear-io-collected-thunks)
            (semaphore-wait io-semaphore)
            (set! io-collected-thunks null)
            (semaphore-post io-semaphore))
          
          (define/public (wait-for-io-to-complete) ; =Kernel=, =Handler=
            (let ([semaphore (make-semaphore 0)])
              (queue-callback
               (lambda () ; =Kernel=, =Handler=
                 (run-io-collected-thunks)
                 (semaphore-post semaphore))
               #f)
              (yield semaphore)))
          
          (define/public (wait-for-io-to-complete/user) ; =User=, =Handler=
            (queue-system-callback/sync
             (lambda () ; =Kernel=, =Handler=
               (run-io-collected-thunks))
             (get-user-thread)))
          
          (define/public queue-output
	    (opt-lambda (thunk [nonblock? #f]) ; =User=
	      (protect
	       (lambda (ut breaks-on?) ; =Protected-User=
					; limiting-sema prevents queueing too much output from the user
		 (and ((if nonblock?
			   semaphore-try-wait? 
			   (if breaks-on? 
			       semaphore-wait/enable-break 
			       semaphore-wait))
		       limiting-sema)
		      (begin
					; Queue the output:
			(semaphore-wait io-semaphore)
			(if (eq? ut (get-user-thread))
					; Queue output:
			    (set! io-collected-thunks (cons thunk io-collected-thunks))
					; Release limit allocation, instead:
			    (semaphore-post limiting-sema))
			(semaphore-post io-semaphore)
					; If there's not one, queue an event that will flush the output queue
			(when (semaphore-try-wait? flushing-event-running)
					; Unlike most callbacks, this one has to run always, even if
					;   the user thread changes.
			  (queue-system-callback
			   ut
			   (lambda () ; =Kernel=, =Handler=
			     (semaphore-post flushing-event-running)
			     (if (eq? ut (get-user-thread))
				 (run-io-collected-thunks)
				 (clear-io-collected-thunks)))
			   #t))
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
                  (insert newline-string (last-position) (last-position) #f))
                
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
          
          ;; this-result-write : (union string (instanceof snip%)) [bool] -> void
          ;; writes `s' as a value produced by the REPL.
          (define this-result-write 
            (opt-lambda (s [nonblock? #f]) ; =User=
              (queue-output
               (lambda () ; =Kernel=, =Handler=
                 (cleanup-transparent-io)
                 (generic-write this
                                s
                                (lambda (start end)
                                  (change-style result-delta
                                                start end))))
	       nonblock?)))
          
          (field (saved-newline? #f))
          ;; this-out-write : (union string snip%) [bool] -> void
          (define this-out-write
            (opt-lambda (s [nonblock? #f]) ; = User=
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
                     (gw newline-string))
                   (gw s1)))
	       nonblock?)))
          
          ;; this-err-write : (union string (instanceof snip%)) [bool] -> void
          (define this-err-write 
	    (opt-lambda (s [nonblock? #f]) ; =User=
            (queue-output
             (lambda () ; =Kernel=, =Handler=
               (cleanup-transparent-io)
               (generic-write
                this
                s
                (lambda (start end)
                  (change-style error-delta start end))))
	     nonblock?)))
          
          (field (this-err (make-custom-output-port (lambda () (make-semaphore-peek limiting-sema))
						    (lambda (s start end flush?) 
						      (if (this-err-write (substring s start end) flush?)
							  (- end start)
							  0))
						    void
						    void))
                 (this-out (make-custom-output-port (lambda () (make-semaphore-peek limiting-sema))
						    (lambda (s start end flush?) 
						      (if (this-out-write (substring s start end) flush?)
							  (- end start)
							  0))
						    void
						    void))
                 (this-in (make-custom-input-port (lambda (s) 
						    (let ([c (this-in-read-char)])
						      (cond
						       [(char? c)
							(string-set! s 0 c)
							1]
						       [(not c) (make-semaphore-peek maybe-char-ready-sema)]
						       [else c])))
						  (lambda (s skip) 
						    (let ([c (this-in-peek-char skip)])
						      (cond
						       [(char? c)
							(string-set! s 0 c)
							1]
						       [(not c) (make-semaphore-peek maybe-char-ready-sema)]
						       [else c])))
						  void))
                 (this-result (make-custom-output-port (lambda () (make-semaphore-peek limiting-sema))
						       (lambda (s start end flush?) 
							 (if (this-result-write (substring s start end) flush?)
							     (- end start)
							     0))
						       void
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
            (let* ([make-setup-handler
                    (lambda (port port-out-write)
                      (lambda (port-handler pretty)
                        (let ([original-handler (port-handler port)])
                          (port-handler
                           port
                           (rec drscheme-port-handler
                             (lambda (v p)
                               ;; avoid looping by calling original-handler
                               ;; for strings, since `pretty' calls write/display with
                               ;; strings
                               (if (string? v)
                                   (original-handler v p)
                                   (parameterize ([pretty-print-columns 'infinity])
                                     (pretty v p)))))))))]
                   
                   [setup-handlers
                    (lambda (setup-handler)
                      (setup-handler port-display-handler pretty-display)
                      (setup-handler port-write-handler pretty-print))]
                   
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
                 (let* ([ls (current-language-settings)]
                        [lang (drscheme:language-configuration:language-settings-language ls)]
                        [settings (drscheme:language-configuration:language-settings-settings ls)])
                   (send lang render-value/format
                         v
                         settings
                         this-result
                         (lambda (x) (this-result-write x))
			 (get-repl-char-width)))))
             anss))

        ;; get-repl-char-width : -> (and/c exact? integer?)
        ;; returns the width of the repl in characters, or 80 if the
        ;; answer cannot be found.
        (define/private (get-repl-char-width)
          (let ([admin (get-admin)]
                [standard (send (get-style-list) find-named-style "Standard")])
            (if (and admin standard)
                (let ([bw (box 0)])
                  (send admin get-view #f #f bw #f)
                  (let* ([dc (send admin get-dc)]
                         [standard-font (send standard get-font)]
                         [old-font (send dc get-font)])
                    (send dc set-font standard-font)
                    (let* ([char-width (send dc get-char-width)]
                           [answer (inexact->exact (floor (/ (unbox bw) char-width)))])
                      (send dc set-font old-font)
                      answer)))
                80)))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;                                            ;;;
          ;;;            Error Highlighting              ;;;
          ;;;                                            ;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          (override after-insert after-delete)
          (rename [super-after-insert after-insert]
                  [super-after-delete after-delete])
          (inherit get-inserting-prompt)
          
          (public get-error-ranges)
          (define (get-error-ranges) error-ranges)
          
          ;; =User= and =Kernel=
          (define (highlight-error/forward-sexp text start)
            (let ([end (if (is-a? text scheme:text<%>)
                           (or (send text get-forward-sexp start)
                               (+ start 1))
                           (+ start 1))])
              (highlight-error text start end)))
          
          ;; =User= and =Kernel=
          (define (highlight-error/line-col text start-line start-col span)
            (let ([start
                   (+ (send text paragraph-start-position start-line)
                      start-col)])
              (highlight-error
               text
               start
               (+ start span))))
          
          ;; highlight-error : file number number -> void
          (define (highlight-error file start end)
            (highlight-errors (list (list file start end))))
          
          ;; =User= and =Kernel= (maybe simultaneously)
          ;; highlight-errors :    (cons (list file number number) (listof (list file number number)))
          ;;                       (union #f (listof (list (is-a?/c text:basic<%>) number number)))
          ;;                    -> (void)
          (define/public highlight-errors
            (opt-lambda (locs [error-arrows #f])
              (reset-highlighting)  ;; grabs error-range/reset-callback-sempahore
              
              (semaphore-wait error-range/reset-callback-semaphore)
              (set! error-ranges locs)
              
              (let ([defs               
                     (let ([f (get-top-level-window)])
                       (and f
                            (is-a? f drscheme:unit:frame<%>)
                            (send f get-definitions-text)))])
                (when locs
                  (let* ([first-error-range (car locs)]
                         [first-file (car first-error-range)]
                         [first-start (cadr first-error-range)]
                         [first-finish (caddr first-error-range)])
                    (for-each (lambda (loc) 
                                (let ([file (car loc)])
                                  (when (is-a? file text:basic<%>)
                                    (send file begin-edit-sequence))))
                              locs)
                    
                    (when color?
                      (let ([resets
                             (map (lambda (loc)
                                    (let ([file (car loc)]
                                          [start (cadr loc)]
                                          [finish (caddr loc)])
                                      (if (is-a? file text:basic<%>)
                                          (send file highlight-range start finish error-color #f #f 'high)
                                          void)))
                                  locs)])
                        
                        (when (and defs error-arrows)
                          (let ([filtered-arrows
                                 (remove-duplicate-error-arrows
				  (filter
				   (lambda (arr)
				     (embedded-in? (car arr) defs))
				   error-arrows))])
                            (send defs set-error-arrows filtered-arrows)))
                        
                        (set! internal-reset-callback
                              (lambda ()
                                (unless (get-inserting-prompt)
                                  (semaphore-wait error-range/reset-callback-semaphore)
                                  (set! error-ranges #f)
                                  (when defs
                                    (send defs set-error-arrows #f))
                                  (set! internal-reset-callback void)
                                  (for-each (lambda (x) (x)) resets)
                                  (semaphore-post error-range/reset-callback-semaphore))))))
                    
                    (unless (is-a? first-file -text<%>)
                      (send first-file set-position first-start first-start)
                      (send first-file scroll-to-position first-start #f first-finish))
                    (for-each (lambda (loc)
                                (let ([file (car loc)])
                                  (when (is-a? file text:basic<%>)
                                    (send file end-edit-sequence))))
                              locs)
                    (send first-file set-caret-owner #f 'global))))
              
              (semaphore-post error-range/reset-callback-semaphore)))
          
          (define (reset-highlighting)
            (reset-error-ranges))
          
          ;; remove-duplicate-error-arrows : (listof X) -> (listof X)
          ;; duplicate arrows point from and to the same place -- only
          ;; need one arrow for each pair of locations they point to.
          (define/private (remove-duplicate-error-arrows error-arrows)
            (let ([ht (make-hash-table 'equal)])
              (let loop ([arrs error-arrows]
                         [n 0])
                (unless (null? arrs)
                  (hash-table-put! ht (car arrs) n)
                  (loop (cdr arrs) (+ n 1))))
              (let* ([unsorted (hash-table-map ht list)]
                     [sorted (quicksort unsorted (lambda (x y) (<= (cadr x) (cadr y))))]
                     [arrs (map car sorted)])
                arrs)))
                    
          (define/private (embedded-in? txt-inner txt-outer)
            (let loop ([txt-inner txt-inner])
              (cond
                [(eq? txt-inner txt-outer) #t]
                [else (let ([admin (send txt-inner get-admin)])
                        (and (is-a? admin editor-snip-editor-admin<%>)
                             (loop (send (send (send admin get-snip) get-admin) get-editor))))])))
          
          (define (on-set-media) (void))
          
          (define after-insert
            (lambda (x y)
              (reset-highlighting)
              (super-after-insert x y)))
          (define after-delete
            (lambda (x y)
              (reset-highlighting)
              (super-after-delete x y)))
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;                                            ;;;
          ;;;                Parameters                  ;;;
          ;;;                                            ;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          ;; drscheme-port-print-handler : TST port -> void
          ;; effect: prints the value on the port
          ;; default setting for the behavior of the `print' primitive.
          (define (drscheme-port-print-handler value port)
            (let ([language (drscheme:language-configuration:language-settings-language
                             (current-language-settings))]
                  [settings (drscheme:language-configuration:language-settings-settings
                             (current-language-settings))])
              (send language render-value
                    value
                    settings
                    port 
                    (cond
                      [(eq? port this-out) (lambda (x) (this-out-write x))]
                      [(eq? port this-err) (lambda (x) (this-err-write x))]
                      ;; this case should never happen.
                      [(eq? port this-result) (lambda (x) (this-result-write x))]
                      [else #f]))))
          
          (inherit get-dc)
          (define (drscheme-pretty-print-size-hook x _ port)
            (and (or (eq? port this-out)
                     (eq? port this-err)
                     (eq? port this-result))
                 (cond
                   [(is-a? x sized-snip<%>) (send x get-character-width)]
                   [(is-a? x snip%) 
                    (let ([dc (get-dc)]
                          [wbox (box 0)])
                      (send x get-extent dc 0 0 wbox #f #f #f #f #f)
                      (let-values ([(xw xh xa xd) (send dc get-text-extent "x")])
                        (max 1 (inexact->exact (ceiling (/ (unbox wbox) xw))))))]
                   [(syntax? x) 
                    ;; two spaces is about how big the turn down triangle
                    ;; and the extra space accounts for. Of course, when
                    ;; it is opened, this will be all wrong.
                    (+ 2 (string-length (format "~s" x)))]
                   [((use-number-snip) x)
                    (let ([number-snip-type ((which-number-snip) x)])
                      (cond
                        [(memq number-snip-type '(repeating-decimal 
                                                  repeating-decimal-e
                                                  mixed-fraction
                                                  mixed-fraction-e))
                         1] ;; no idea of size yet
                        [else 
                         (error 'which-number-snip
                                "unexpected result from parameter: ~e" 
                                number-snip-type)]))]
                   [else #f])))
          
          (define (drscheme-pretty-print-print-hook x _ port)
            (let ([port-out-write
                   (cond
                     [(eq? port this-out) (lambda (x) (this-out-write x))]
                     [(eq? port this-err) (lambda (x) (this-err-write x))]
                     [(eq? port this-result) (lambda (x) (this-result-write x))]
                     ;; this case should only happen if the user's program overrides the pretty-print-size-hook
                     ;; and doesnt' override the pretty-print-print-hook to match.
                     [else #f])])
              (if port-out-write
                  (let ([snip/str
                         (cond
                           [(syntax? x) (render-syntax/snip x)]
                           [((use-number-snip) x)
                            (let ([number-snip-type ((which-number-snip) x)])
                              (cond
                                [(eq? number-snip-type 'repeating-decimal)
                                 (drscheme:number-snip:make-repeating-decimal-snip x #f)]
                                [(eq? number-snip-type 'repeating-decimal-e)
                                 (drscheme:number-snip:make-repeating-decimal-snip x #t)]
                                [(eq? number-snip-type 'mixed-fraction)
                                 (drscheme:number-snip:make-fraction-snip x #f)]
                                [(eq? number-snip-type 'mixed-fraction-e)
                                 (drscheme:number-snip:make-fraction-snip x #t)]
                                [else
                                 (error 'which-number-snip
                                        "expected either 'repeating-decimal, 'repeating-decimal-e, 'mixed-fraction, or 'mixed-fraction-e got : ~e"
                                        number-snip-type)]))]
                           [else x])])
                    (port-out-write snip/str))
                  (display x))))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;                                            ;;;
          ;;;                Evaluation                  ;;;
          ;;;                                            ;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          (define (get-prompt) "> ")
          (define (eval-busy?)
            (not (and (get-user-thread)
                      (thread-running? (get-user-thread)))))
          
          (field (user-language-settings #f)
                 (user-teachpack-cache (preferences:get 'drscheme:teachpacks))
                 (user-custodian #f)
                 (user-eventspace-box (make-weak-box #f))
                 (user-namespace-box (make-weak-box #f))
                 (user-thread-box (make-weak-box #f)))

          (define/public (get-user-language-settings) user-language-settings)
          (define/public (get-user-custodian) user-custodian)
          (define/public (get-user-teachpack-cache) user-teachpack-cache)
          (define/public (set-user-teachpack-cache tpc) (set! user-teachpack-cache tpc))
          (define/public (get-user-eventspace) (weak-box-value user-eventspace-box))
          (define/public (get-user-thread) (weak-box-value user-thread-box))
          (define/public (get-user-namespace) (weak-box-value user-namespace-box))
          
          (field (in-evaluation? #f) ; a heursitic for making the Break button send a break
                 (should-collect-garbage? #f)
                 (ask-about-kill? #f))
          
          (define (insert-warning)
            (begin-edit-sequence)
            (insert #\newline (last-position) (last-position))
            (let ([start (last-position)])
              (insert
               (string-constant interactions-out-of-sync)
               start start)
              (let ([end (last-position)])
                (change-style warning-style-delta start end)))
            (end-edit-sequence))
          
          (field (already-warned? #f))
          
          (field (eval-count 0))
          (define (do-eval start end)
            (update-region-end end)
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
            (do-many-text-evals this start end #f))
          
          (define (cleanup)
            (set! in-evaluation? #f)
            (update-running)
            (unless (and (get-user-thread) (thread-running? (get-user-thread)))
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
            (when (and (get-user-thread) (thread-running? (get-user-thread)))
              (let ([c-locked? (is-locked?)])
                (lock #f)
                (insert-prompt)
                (lock c-locked?)))
            (cleanup)
            (end-edit-sequence)
            (send context set-breakables #f #f)
            (send context enable-evaluation))
          
          ; =Kernel, =Handler=
          (define (do-many-text-evals text start end complete-program?)
            (do-many-evals
             (lambda (single-loop-eval)  ; =User=, =Handler=
               (let* ([text/pos (drscheme:language:make-text/pos text start end)]
                      [settings (current-language-settings)]
                      [lang (drscheme:language-configuration:language-settings-language settings)]
                      [settings (drscheme:language-configuration:language-settings-settings settings)]
                      [get-sexp/syntax/eof 
                       (if complete-program?
                           (send lang front-end/complete-program text/pos settings user-teachpack-cache)
                           (send lang front-end/interaction text/pos settings user-teachpack-cache))])
                 (let loop () 
                   (let ([sexp/syntax/eof (get-sexp/syntax/eof)])
                     (cond
                       [(eof-object? sexp/syntax/eof)
                        (void)]
                       [else
                        (single-loop-eval
                         (lambda ()
                           (call-with-values
                            (lambda ()
                              (eval-syntax sexp/syntax/eof))
                            (lambda x (display-results x)))
                           (wait-for-io-to-complete/user)))
                        (loop)])))))))
          
          ;; do-many-evals : ((((-> void) -> void) -> void) -> void)
          (define do-many-evals ; =Kernel=, =Handler=
            
            ;; run-loop has the loop. It expects one argument, a procedure that
            ;; can be called with a thunk. The argument to run-loop maintains the right
            ;; breaking state and calls the thunk it was called with.
            (lambda (run-loop)  ;; (((-> void) -> void) -> void)
              (send context disable-evaluation)
              (send context set-breakables (get-user-thread) (get-user-custodian))
              (cleanup-transparent-io)
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
                 (send context reset-offer-kill)
                 
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
                     (get-user-thread)
                     (lambda () ; =Kernel=, =Handler= 
                       (after-many-evals)
                       (cleanup-interaction)))))))))
          
          (define/public (after-many-evals) (void))
          
          (define shutdown-user-custodian ; =Kernel=, =Handler=
            ; Use this procedure to shutdown when in the middle of other cleanup
            ;  operations, such as when the user clicks "Execute".
            ; Don't use it to kill a thread where other, external cleanup
            ;  actions must occur (e.g., the exit handler for the user's
            ;  thread). In that case, shut down user-custodian directly.
            (lambda ()
              (when user-custodian
                (custodian-shutdown-all user-custodian))
	      (set! user-custodian #f)
              (set! user-thread-box (make-weak-box #f))
              (semaphore-wait io-semaphore)
              (for-each (lambda (i) (semaphore-post limiting-sema)) io-collected-thunks)
              (set! io-collected-thunks null)
              (semaphore-post io-semaphore)))
          
          (define (kill-evaluation) ; =Kernel=, =Handler=
            (when user-custodian
              (custodian-shutdown-all user-custodian))
	    (set! user-custodian #f))
          
          (field (user-break-enabled #t))
          
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
                   (lambda () ; =Kernel=
                     (let ([ut (get-user-thread)])
                       (thread-wait ut)
                       (queue-system-callback
                        ut
                        (lambda () ; =Kernel=, =Handler=
                          (if need-interaction-cleanup?
                              (cleanup-interaction)
                              (cleanup)))))))))
          
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
                (let ([saved-error-escape-k (current-error-escape-k)]
                      [cleanup? #f])
                  (dynamic-wind
                   (lambda ()
                     (set! cleanup? #f)
                     (current-error-escape-k (lambda () 
					       (set! cleanup? #t)
					       (k (void)))))
		  (lambda () 
                     (thunk) 
                     ; Breaks must be off!
                     (set! cleanup? #t))
                   (lambda () 
                     (current-error-escape-k saved-error-escape-k)
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
          
          (inherit get-top-level-window)
          (define init-evaluation-thread ; =Kernel=
            (lambda ()
              (let ([default (preferences:get drscheme:language-configuration:settings-preferences-symbol)]
                    [frame (get-top-level-window)])
                (if frame
                    (let ([defs (send frame get-definitions-text)])
                      (set! user-language-settings (send defs get-next-settings)))
                    (set! user-language-settings default)))
              
              (set! user-custodian (make-custodian))
	      ; (custodian-limit-memory user-custodian 10000000 user-custodian)
              (set! user-eventspace-box (make-weak-box
					 (parameterize ([current-custodian user-custodian])
					   (make-eventspace))))
              (set! user-break-enabled #t)
              (set! eval-thread-thunks null)
              (set! eval-thread-state-sema (make-semaphore 1))
              (set! eval-thread-queue-sema (make-semaphore 0))
              
              (let* ([init-thread-complete (make-semaphore 0)]
                     [goahead (make-semaphore)]
                     [queue-user/wait
                      (lambda (thnk)
                        (let ([wait (make-semaphore 0)])
                          (parameterize ([current-eventspace (get-user-eventspace)])
                            (queue-callback
                             (lambda ()
                               (thnk)
                               (semaphore-post wait))))
                          (semaphore-wait wait)))])
                
                ; setup standard parameters
                (let ([snip-classes
                       ; the snip-classes in the DrScheme eventspace's snip-class-list
                       (drscheme:eval:get-snip-classes)])
                  (queue-user/wait
                   (lambda () ; =User=, =No-Breaks=
                     ; No user code has been evaluated yet, so we're in the clear...
                     (break-enabled #f)
                     (set! user-thread-box (make-weak-box (current-thread)))
                     (initialize-parameters snip-classes))))
                
                ;; disable breaks until an evaluation actually occurs
                (send context set-breakables #f #f)
                                
                ;; initialize the language
                (send (drscheme:language-configuration:language-settings-language user-language-settings)
                      on-execute
                      (drscheme:language-configuration:language-settings-settings user-language-settings)
                      queue-user/wait)
                
                ;; installs the teachpacks
                ;; must happen after language is initialized.
                (queue-user/wait
                 (lambda () ; =User=, =No-Breaks=
                   (with-handlers ([not-break-exn?
                                    (lambda (exn)
                                      (raise exn))])
                     (drscheme:teachpack:install-teachpacks 
                      user-teachpack-cache))))
                
                
                (parameterize ([current-eventspace (get-user-eventspace)])
                  (queue-callback
                   (lambda ()
                     (let ([drscheme-error-escape-handler
                            (lambda ()
			      ((current-error-escape-k)))])
                       (error-escape-handler drscheme-error-escape-handler))
                     
                     (set! in-evaluation? #f)
                     (update-running)
		     (send context set-breakables #f #f)
                     
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
               (get-user-thread)
               (lambda ()
                 (if in-evaluation?
                     (send context running)
                     (send context not-running))))))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;                                          ;;;
          ;;;                Execution                 ;;;
          ;;;                                          ;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          (field (repl-initially-active? #f))
          
          ;; initialize-paramters : (listof snip-class%) -> void
          (define initialize-parameters ; =User=
            (lambda (snip-classes)
              
              (current-language-settings user-language-settings)
              (error-value->string-handler drscheme-error-value->string-handler)
              (error-print-source-location #f)
              (error-display-handler drscheme-error-display-handler)
              (current-load-relative-directory #f)
              (current-custodian user-custodian)
              (current-load text-editor-load-handler)
              
              (global-port-print-handler drscheme-port-print-handler)
              (setup-display/write-handlers)
              (pretty-print-size-hook drscheme-pretty-print-size-hook)
              (pretty-print-print-hook drscheme-pretty-print-print-hook)
              
              (drscheme:eval:set-basic-parameters snip-classes)
              (current-rep this)
              (let ([dir (or (send context get-directory)
                           drscheme:init:first-dir)])
                (current-directory dir)
                (current-load-relative-directory dir))
              
              (set! user-namespace-box (make-weak-box (current-namespace)))
              
              (current-output-port this-out)
              (current-error-port this-err)
              (current-value-port this-result)
              (current-input-port this-in)
              (break-enabled #t)
              (let* ([primitive-dispatch-handler (event-dispatch-handler)])
                (event-dispatch-handler
                 (rec drscheme-event-dispatch-handler ; <= a name for #<...> printout
                   (lambda (eventspace) ; =User=, =Handler=
                     ; Breaking is enabled if the user turned on breaks and
                     ;  is in a `yield'. If we get a break, that's ok, because
                     ;  the kernel never queues an event in the user's eventspace.
                     (cond
                       [(eq? eventspace (get-user-eventspace))
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
                             (send context reset-offer-kill)
                             (send context set-breakables (get-user-thread) (get-user-custodian))
                             
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
                              (lambda ()
                                ;; in principle, the line below might cause
                                ;; a "race conditions" in the GUI. That is, there might
                                ;; be many little events that the user won't quite
                                ;; be able to break.
                                (send context set-breakables #f #f)))
                             
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
          
          (define (reset-console)
            (when (thread? thread-killed)
              (kill-thread thread-killed))
            (let ([fr (send (get-canvas) get-top-level-window)])
              (send context clear-annotations)
              (drscheme:debug:hide-backtrace-window))
            (shutdown-user-custodian)
            (cleanup-transparent-io)
            (clear-previous-expr-positions)
            (set! should-collect-garbage? #t)
            
            (set! eof-received? #f)
            
            ;; in case the last evaluation thread was killed, clean up some state.
            (lock #f)
            (set! in-evaluation? #f)
            (update-running)
            
            ;; clear out repl first before doing any work.
            (begin-edit-sequence)
            (set-resetting #t)
            (delete (paragraph-start-position 1) (last-position))
            (set-resetting #f)
            (end-edit-sequence)
            
            ;; must init-evaluation-thread before determining
            ;; the language's name, since this updates user-language-settings
            (init-evaluation-thread)
            
            (begin-edit-sequence)
            (set-resetting #t)
            (set-prompt-mode #f)
            (set-resetting #f)
            (set-position (last-position) (last-position))
            
            (insert/delta this (string-append (string-constant language) ": ") welcome-delta)
            (let-values (((before after)
                          (insert/delta
                           this
                           (extract-language-name user-language-settings)
                           dark-green-delta
                           (extract-language-style-delta user-language-settings)))
                         ((url) (extract-language-url user-language-settings)))
              (when url
                (set-clickback before after (lambda args (send-url url))
                               click-delta)))
            (unless (is-default-settings? user-language-settings)
              (insert/delta this (string-append " " (string-constant custom)) dark-green-delta))
            (insert/delta this (format ".~n") welcome-delta)
            
            (for-each
             (lambda (fn)
               (insert/delta this
                             (string-append (string-constant teachpack) ": ")
                             welcome-delta)
               (insert/delta this fn dark-green-delta)
               (insert/delta this (format ".~n") welcome-delta))
             (drscheme:teachpack:teachpack-cache-filenames 
              user-teachpack-cache))
            
            (set! repl-initially-active? #t)
            (set! already-warned? #f)
            (end-edit-sequence)
            
            (super-reset-console)
            (reset-region 0 'end))
          
          (define (initialize-console)
            (super-initialize-console)
            
            (insert/delta this (string-append (string-constant welcome-to) " ") welcome-delta)
            (let-values ([(before after)
                          (insert/delta this (string-constant drscheme) click-delta drs-font-delta)])
              (insert/delta this (format (string-append ", " (string-constant version) " ~a.~n") (version:version))
                            welcome-delta)
              (set-clickback before after 
                             (lambda args (drscheme:app:about-drscheme))
                             click-delta))
            (reset-console)
            (insert-prompt)
            (clear-undos))
          (inherit get-prompt-position)
          (rename (super-insert-prompt insert-prompt))
          (define/override (insert-prompt)
            (super-insert-prompt)
            (reset-region (get-prompt-position) 'end))
          
          (super-instantiate ())
          
          (set-styles-sticky #f)))
      
      (define console-text-mixin
        (mixin ((class->interface text%)
                color:text<%>
                editor:basic<%>
                editor:keymap<%>) (console-text<%>)
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
          (override get-keymaps
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
                  
                  get-inserting-prompt
                  
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
                  
                  
                  do-pre-eval
                  do-eval
                  do-post-eval
                  eval-busy?)
          
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
                [else 
                 #f])))
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
                      (pretty-print-columns new-columns)))))))
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
          
          (field [submit-predicate
                  (lambda (text prompt-position)
                    #t)])
          (define/public (set-submit-predicate p)
            (set! submit-predicate p))

          (inherit backward-match)
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
                  [(not (or (eq? code 'numpad-enter)
                            (equal? code #\return)
                            (equal? code #\newline)))
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
                        (not (eval-busy?)))
                   (if (and (balance-required)
                            (not (or (send key get-alt-down)
				     (send key get-meta-down))))
                       (let ([at-end-of-expression? 
                              (and (only-spaces-after start)
                                   (submit-predicate this prompt-position))])
                         (cond
                           [at-end-of-expression?
                            (delete start last)
                            (do-save-and-eval prompt-position start)]
                           [(eq? 'numpad-enter code)
                            (begin-edit-sequence)
                            (set-position (last-position))
                            (end-edit-sequence)
                            (do-save-and-eval prompt-position (last-position))]
                           [else
                            (super-on-local-char key)]))
                       (begin
                         (delete start last)
                         (do-save-and-eval prompt-position start)))]

;; need to change this case to bring down entire interaction always.
                  [(start . < . prompt-position)
                   (let ([match (backward-match start 0)])
                     (if match
                         (begin
                           (begin-edit-sequence)
                           (copy-to-end/set-position match start)
                           (end-edit-sequence))
                         (super-on-local-char key)))]
                  [else
                   (super-on-local-char key)]))))
          
          (field (inserting-prompt #f))
          (define (get-inserting-prompt) inserting-prompt)
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
          (public reset-console)
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
          (super-instantiate ())))
      
      (define input-delta (make-object style-delta%))
      (send input-delta set-delta-foreground (make-object color% 0 150 0))
      (define transparent-io-text<%> 
        (interface ()
          set-program-output
          get-insertion-point))
      
      (define transparent-io-super% 
        (console-text-mixin
         (scheme:text-mixin
          (color:text-mixin
           (mode:host-text-mixin
            (text:searching-mixin
             text:autowrap%))))))
      
      (define consumed-delta (make-object style-delta%))
      (send (send consumed-delta get-foreground-mult) set 0.75 0.75 0.75)
	   
      (define transparent-io-text%
        (class* transparent-io-super% (transparent-io-text<%>) 
          (init-field _rep-text)
          (inherit change-style
                   get-resetting set-resetting lock get-text
                   set-position last-position get-character
                   clear-undos set-cursor
                   do-pre-eval do-post-eval balance-required)
          (rename [super-after-insert after-insert]
                  [super-on-local-char on-local-char])
          (field
	   [rep-text _rep-text]
           [data null]
           [old-stream-sections null]
           [new-stream-start 0]
           [new-stream-end 0]
           [shutdown? #f])

          (field
           [stream-start/end-protect (make-semaphore 1)]
           [wait-for-sexp (make-semaphore 0)]
           [eof-submitted? #f])

          [define/public get-insertion-point
            (lambda ()
              new-stream-start)]
          [define/public shutdown
            (lambda ()
              (set! shutdown? #t)
              (semaphore-post wait-for-sexp)
              (lock #t))]
          [define/public mark-consumed
            (lambda (start end)
              (let ([old-resetting (get-resetting)])
                (set-resetting #t)
                (change-style consumed-delta start end)
                (set-resetting old-resetting)))]
	    
          [define/public eof-received
            (lambda () ; =Kernel=, =Handler=
              (set! eof-submitted? #t)
              (unless (= new-stream-start (last-position))
                (set! old-stream-sections 
		      (append old-stream-sections 
			      (list (cons new-stream-start (last-position))))))
              (set! new-stream-start (last-position))
              (set! new-stream-end (last-position))
              (semaphore-post wait-for-sexp))]
          
          [define/public fetch-char ; =Kernel=, =Handler=, =Non-Reentrant= (queue requests externally)
            (lambda ()
              (send rep-text show-eof-icon)
              (let* ([ready-char #f])
                (let loop ()
                  (semaphore-wait stream-start/end-protect)
                  (cond
                    [(not (null? old-stream-sections))
                     (let* ([old-section (car old-stream-sections)]
                            [old-section-start (car old-section)]
                            [old-section-end (cdr old-section)])
                       (set! ready-char (get-character old-section-start))
                       (mark-consumed old-section-start (add1 old-section-start))
                       (if (= (+ 1 old-section-start) old-section-end)
                           (set! old-stream-sections (cdr old-stream-sections))
                           (set-car! old-section (+ old-section-start 1))))]
                    [eof-submitted?
                     (set! ready-char eof)]
                    [else (void)])
                  (semaphore-post stream-start/end-protect)
                  (or ready-char
                      (begin
                        (yield wait-for-sexp)
                        (if shutdown? 
                            (void)
                            (loop)))))))]
          [define/override get-prompt (lambda () "")]
          (field
           [program-output? #f])
          [define/public set-program-output
            (lambda (_program-output?)
              (set! program-output? _program-output?))]
          (rename [super-can-insert? can-insert?]
                  [super-can-change-style? can-change-style?]
                  [super-after-delete after-delete])
          (define/override can-insert?
            (lambda (start len)
              (or program-output?
                  (super-can-insert? start len))))
          [define/override can-change-style?
            (lambda (start len)
              (let ([super? (super-can-change-style? start len)])
                (or program-output? super?)))]
          (define/override (after-delete start len)
            (super-after-delete start len)
            ;; assume that when start is in the stream
            ;; is the only case of interest and that
            ;; (in that case) start+len is also inside the range
            (when (<= new-stream-start start new-stream-end)
              (set! new-stream-end (- new-stream-end len))))
          [define/override after-insert
            (lambda (start len)
              (super-after-insert start len)
              (cond
                [program-output?
                 (when (start . <= . new-stream-start)
                   (change-style output-delta start (+ start len))
                   (set! new-stream-start (+ new-stream-start len))
                   (set! new-stream-end (+ new-stream-end len)))]
                [else
                 (when (<= new-stream-start start new-stream-end)
                   (set! new-stream-end (+ new-stream-end len)))
                 (let ([old-r (get-resetting)])
                   (set-resetting #t)
                   (change-style input-delta start (+ start len))
                   (set-resetting old-r))]))]
          [define/override do-eval
            (lambda (start end)
              (do-pre-eval)
              (set! old-stream-sections
		    (append old-stream-sections
			    (list (cons new-stream-start new-stream-end))))
              (set! new-stream-start (+ end 1))
              (set! new-stream-end (+ end 1))
              (semaphore-post wait-for-sexp)
              (do-post-eval))]
          (inherit insert-prompt)
          (super-make-object)
          (insert-prompt)
          (inherit start-colorer)
          (start-colorer symbol->string default-lexer '((|(| |)|)
                                                        (|[| |]|)
                                                        (|{| |}|)))))
          
          
      (define -text% 
        (drs-bindings-keymap-mixin
         (text-mixin 
          (console-text-mixin
           (scheme:text-mixin
            (color:text-mixin
             (text:info-mixin
              (editor:info-mixin
               (text:searching-mixin
                (text:nbsp->space-mixin
                 (mode:host-text-mixin
                  text:clever-file-format%))))))))))))))
