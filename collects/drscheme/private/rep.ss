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
           (lib "class100.ss")
           (lib "list.ss")
           (lib "pretty.ss")
           (lib "etc.ss")
           (prefix print-convert: (lib "pconvert.ss"))
           "drsig.ss"
           (lib "string-constant.ss" "string-constants")
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
	   (lib "moddep.ss" "syntax"))
  
  (provide rep@)
  
  (define rep@
    (unit/sig drscheme:rep^
      (import (drscheme:init : drscheme:init^)
              (drscheme:snip : drscheme:snip^)
              (drscheme:language-configuration : drscheme:language-configuration/internal^)
	      (drscheme:language : drscheme:language^)
              (drscheme:app : drscheme:app^)
              (drscheme:frame : drscheme:frame^)
              (drscheme:unit : drscheme:unit^)
              (drscheme:text : drscheme:text^)
              (drscheme:help : drscheme:help-desk^)
              (drscheme:teachpack : drscheme:teachpack^))
      
      (rename [-text% text%])

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
                (real? x)
                (not (integer? x))))))
      
      ;; current-language-settings : (parameter language-setting)
      ;; set to the current language and its setting on the user's thread.
      (define current-language-settings (make-parameter #f))
      
      ;; current-rep : (parameter (instanceof rep:text%))
      ;; the repl that controls the evaluation in this thread.
      (define current-rep (make-parameter 'current-rep-not-yet-set))

      ;; a port that accepts values for printing in the repl
      (define current-value-port (make-parameter 'uninitialized-value-port))
            
      ;; drscheme-load-handler : string ??? ->* TST
      ;; =User=
      ;; the default load handler for programs running in DrScheme
      (define (drscheme-load-handler filename expected-module)
        (unless (string? filename)
	  (raise-type-error 'drscheme-load-handler "string" filename))
        (let* ([input (build-input filename)]
               [ls (current-language-settings)]
               [language (drscheme:language-configuration:language-settings-language ls)]
               [settings (drscheme:language-configuration:language-settings-settings ls)]
               [thnk (send language front-end input settings)])
          (parameterize ([read-accept-compiled #t])
	    (if expected-module
		(let* ([first (thnk)]
                       [module-ized-exp (check-module-form first expected-module filename)]
                       [second (thnk)])
		  (unless (eof-object? second)
		    (raise-syntax-error
		     'drscheme-load-handler
		     (format "expected only a `module' declaration for `~s', but found an extra expression"
			     expected-module)
		     second))
		  (eval module-ized-exp))
		(let loop ([last-time-values (list (void))])
		  (let ([exp (thnk)])
		    (if (eof-object? exp)
			(apply values last-time-values)
			(call-with-values
			 (lambda () (eval exp))
			 (lambda x (loop x))))))))))
      
      ;; build-input : string[file-exists?] -> input
      ;; returns an input to be used with a language's `front-end' method
      (define (build-input filename)
        (let* ([p (open-input-file filename)]
               [chars (list (read-char p)
                            (read-char p)
                            (read-char p)
                            (read-char p))])
          (close-input-port p)
          (cond
            [(equal? chars (string->list "WXME"))
             (let ([text (make-object text%)])
               (send text load-file filename)
               (drscheme:language:make-text/pos text 0 (send text last-position)))]
            [else filename])))
      
      ;; drscheme-error-display-handler : (string (union #f exn) -> void
      ;; =User=
      ;; the timing is a little tricky here. 
      ;; the file icon must appear before the error message in the text, so tha happens first.
      ;; the highlight must be set after the error message because inserting into the text resets
      ;;     the highlighting.
      (define (drscheme-error-display-handler msg exn)
	(let-values ([(src position other-position module form)
		      (if (exn? exn)
			  (extract-info-from-exn exn)
			  (values #f #f #f #f #f))])
	  (let ([rep (current-rep)])
	    
            (let ([locked? (send rep is-locked?)])
              (send rep lock #f)
              (when (string? src)
                (let ([pos (send rep last-position)])
                  (send rep insert file-icon pos pos)
                  (send rep insert #\space (+ pos 1) (+ pos 1))
                  (send rep set-clickback pos (+ pos 1)
                        (lambda (txt start end)
                          (open-file-and-highlight src position other-position)))))
              (when module
                (let ([pos (send rep last-position)])
                  (send rep insert (if (string? docs-icon)
				       docs-icon
				       (send docs-icon copy))
			pos pos)
                  (send rep insert #\space (+ pos 1) (+ pos 1))
                  (send rep set-clickback pos (+ pos 1)
                        (lambda (txt start end)
                          (show-documentation module form)))))
              (send rep lock locked?))
            
	    (display msg (current-error-port))
            (newline (current-error-port))

            (send rep wait-for-io-to-complete/user)
	    
	    (when (and (object? src) (is-a? src text:basic%))
	      (if other-position
		  (send rep highlight-error src position other-position)
		  (send rep highlight-error/forward-sexp src position))))))

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
      
      ;; show-documentation : symbol (union #f symbol) -> void
      (define (show-documentation module form)
        (message-box "open docs here"
                     (format "module: ~s~nform: ~s~n" module form)))
      
      ;; extract-info-from-exn : exn ->
      ;;                         (values (union #f string (instanceof text:basic%))
      ;;                                 (union #f number)
      ;;                                 (union #f number)
      ;;                                 (union #f symbol)
      ;;                                 (union #f symbol))
      ;; the first result is the filename or editor where the error occurred
      ;; the second result is the position in the file or editor where it occured
      ;; the third result is #f when the starting position should be considered
      ;;     the beginning of an sexp and it is the end of the
      ;;     region to highlight otherwise.
      ;; the fourth result is a symbol naming the module defining the form that
      ;;     signalled the error
      ;; the fifth result is a symbol naming the form that signaled the error.
      (define (extract-info-from-exn exn)
	(cond
	  [(exn:syntax? exn)
           (let ([stx (exn:syntax-expr exn)])
             (if stx
                 (let* ([start (and (syntax-position stx)
                                    (- (syntax-position stx) 1))]
                        [end (and (syntax-span stx)
                                  start
                                  (+ start (syntax-span stx)))])
                   (values (syntax-source stx)
                           start
                           end
                           (exn:syntax-module exn)
                           (exn:syntax-form exn)))
                 (values #f #f #f
                         (exn:syntax-module exn)
                         (exn:syntax-form exn))))]
          [(exn:read? exn)
           (values (exn:read-source exn)
		   (- (exn:read-position exn) 1)
		   (exn:read-position exn)
		   #f #f)]
	  [else
	   (values #f #f #f #f #f)]))

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
      
      (define scheme-interaction-mode-keymap (make-object keymap%))
      (setup-scheme-interaction-mode-keymap scheme-interaction-mode-keymap)
      
      (define drs-font-delta (make-object style-delta% 'change-family 'decorative))
      
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
        (car (last-pair (send (drscheme:language-configuration:language-settings-language language-settings)
                              get-language-position))))

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
                     position-paragraph
                     set-position
                     begin-edit-sequence
                     end-edit-sequence
                     reset-pretty-print-width
                     scroll-to-position
                     get-admin
                     set-prompt-position
                     get-canvases find-snip
                     release-snip)
            (rename [super-initialize-console initialize-console]
                    [super-reset-console reset-console]
                    [super-on-close on-close])
            
            (override on-close)
            
            (override get-prompt eval-busy? do-eval
                      initialize-console
                      reset-console)
            
            (public
              expand-program
              reset-highlighting
	      highlight-error
	      highlight-error/forward-sexp
              
              get-user-custodian
              get-user-eventspace
              get-user-thread
              get-user-namespace
              
              kill-evaluation
              
              break
              set-offer-break-state
              
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
            
            ;; =User= (probably doesn't matter)
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
            
            ;; =User=
            (define queue-system-callback/sync
              (lambda (ut thunk)
                (let ([s (make-semaphore 0)])
                  (queue-system-callback 
                   ut 
                   (lambda ()
                     (when (eq? ut user-thread)
                       (thunk))
                     (semaphore-post s))
                   #t)
                  (semaphore-wait s))))
            
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;					     ;;;
	;;;                  I/O                     ;;;
	;;;					     ;;;
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
            
            (define (this-in-read-char) ; =User=
              (this-in-fetch-char #f))
            
            (define (this-in-peek-char) ; =User=
              (this-in-fetch-char #t))
            
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
               user-thread))
            
            (define (queue-output thunk) ; =User=
              (protect
               (lambda (ut) ; =Protected-User=
                 ; limiting-sema prevents queueing too much output from the user
                 (semaphore-wait/enable-break limiting-sema)
                 ; Queue the output:
                 (semaphore-wait io-semaphore)
                 (if (eq? ut user-thread)
                     ; Queue output:
                     (set! io-collected-thunks
                           (cons thunk io-collected-thunks))
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
                      (run-io-collected-thunks))
                    #t)))))
            
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
                       (gw newline-string))
                     (gw s1))))))
            
	    ;; this-err-write : (union string (instanceof snip%)) -> void
	    (define (this-err-write s) ; =User=
              (queue-output
               (lambda () ; =Kernel=, =Handler=
                 (cleanup-transparent-io)
                 (generic-write
                  this
                  s
                  (lambda (start end)
                    (change-style error-delta start end))))))

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
              (let* ([make-setup-handler
                      (lambda (port port-out-write)
                        (lambda (port-handler pretty)
                          (let ([original-handler (port-handler port)])
                            (port-handler
                             port
                             (rec drscheme-port-handler
                               (lambda (v p)
                                 ;; avoid infinite recursion by calling original-handler
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
                           (lambda (x) (this-result-write x))))))
               anss))
                        
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;            Error Highlighting              ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	    (override after-insert after-delete)
	    (rename [super-after-insert after-insert]
		    [super-after-delete after-delete])
	    (inherit get-inserting-prompt)
	    (field (error-range #f)
		   (reset-callback void)
                   (error-range/reset-callback-semaphore 
                    (make-semaphore 1)))
            (public get-error-range)
            (define (get-error-range) error-range)

            ;; =User= and =Kernel=
	    (define (highlight-error/forward-sexp text start)
	      (let ([end (if (is-a? text scheme:text<%>)
                             (or (send text get-forward-sexp start)
                                 (+ start 1))
                             (+ start 1))])
                (highlight-error text start end)))

            ;; =User= and =Kernel= (maybe simultaneously)
	    (define (highlight-error file start finish)
              (when (is-a? file text:basic<%>)
		(send file begin-edit-sequence)
		(reset-highlighting)
                (semaphore-wait error-range/reset-callback-semaphore)
		(set! error-range (cons start finish))
		(if color?
		    (let ([reset (send file highlight-range start finish error-color #f #f 'high)])
                      (when (is-a? file drscheme:unit:definitions-text<%>)
                        (send file set-position start start))
		      (set! reset-callback
			    (lambda ()
                              (unless (get-inserting-prompt)
                                (semaphore-wait error-range/reset-callback-semaphore)
                                (set! error-range #f)
				(set! reset-callback void)
                                (reset)
                                (semaphore-post error-range/reset-callback-semaphore)))))
		    (send file set-position start finish))
		(send file scroll-to-position start #f finish)
		(send file end-edit-sequence)
		(send file set-caret-owner #f 'global)
                (semaphore-post error-range/reset-callback-semaphore)))

            (define (reset-highlighting) (reset-callback))
            
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
            
            ;; set-basic-parameters : -> void
            ;; sets the parameters that are shared between the repl's initialization
            ;; and expand-program
            (define (set-basic-parameters snip-classes)
              (for-each (lambda (snip-class) (send (get-the-snip-class-list) add snip-class))
                        snip-classes)
              (current-rep this)
              (read-curly-brace-as-paren #t)
              (read-square-bracket-as-paren #t)
              (error-print-width 250)
              (current-load drscheme-load-handler)
              (let ([dir (or (send context get-directory)
                             drscheme:init:first-dir)])
                (current-directory dir)
                (current-load-relative-directory dir))
              
              (let ([user-custodian (current-custodian)])
                (exit-handler (lambda (arg) ; =User=
                                (custodian-shutdown-all user-custodian))))
              (current-namespace (make-namespace 'empty)))
            
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
            
            (define (drscheme-pretty-print-size-hook x _ port)
              (and (or (eq? port this-out)
                       (eq? port this-err)
                       (eq? port this-result))
                   (cond
                     [(is-a? x sized-snip<%>) (send x get-character-width)]
                     [(is-a? x snip%) 1]
                     [((use-number-snip) x)
                      (+ (string-length (number->string (floor x)))
                         (max (string-length
                               (number->string 
                                (numerator (- x (floor x)))))
                              (string-length
                               (number->string 
                                (denominator (- x (floor x)))))))]
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
                             [((use-number-snip) x)
                              (make-object drscheme:snip:whole/part-number-snip% x)]
                             [else x])])
                      (port-out-write snip/str))
                    (display x))))
            
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;                Evaluation                  ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
            (define (expand-program input language-settings iter)
              (let* ([eventspace (make-eventspace)]
                     [language (drscheme:language-configuration:language-settings-language
                                language-settings)]
                     [settings (drscheme:language-configuration:language-settings-settings
                                language-settings)]
                     [user-custodian (make-custodian)]
                     [exn-raised? #f]
                     [err-msg #f]
                     [err-exn #f]
                     [run-in-eventspace
                      (lambda (thnk)
                        (parameterize ([current-eventspace eventspace])
                          (set! exn-raised? #f)
                          (let ([sema (make-semaphore 0)]
                                [ans #f])
                            (queue-callback
                             (lambda ()
                               (let/ec k
                                 (parameterize ([error-escape-handler
                                                 (let ([drscheme-expand-program-error-escape-handler
                                                        (lambda ()
                                                          (set! exn-raised? #t)
                                                          (k (void)))])
                                                   drscheme-expand-program-error-escape-handler)])
                                   (set! ans (thnk))))
                               (semaphore-post sema)))
                            (semaphore-wait sema)
                            ans)))]
                     [drs-snip-classes (get-snip-classes)])
                (run-in-eventspace
                 (lambda ()
                   (error-display-handler
                    (lambda (msg exn)
                      (set! err-msg msg)
                      (set! err-exn exn)))
                   (current-custodian user-custodian)
                   (set-basic-parameters drs-snip-classes)
                   (current-language-settings language-settings)
                   (break-enabled #t)))
                (send language on-execute settings run-in-eventspace)
                (let ([read-thnk
                       (run-in-eventspace
                        (lambda ()
                          (send language front-end input settings)))])
                  (let loop ()
                    (let ([in (run-in-eventspace
                               (lambda ()
                                 (let ([rd (read-thnk)])
                                   (if (eof-object? rd)
                                       rd
                                       (expand rd)))))])
                      (unless (eof-object? in)
                        (iter
                         exn-raised?
                         (if exn-raised? (cons err-msg err-exn) in)
                         run-in-eventspace
                         (if exn-raised?
                             (lambda () (void))
                             (lambda () (loop))))))))))
            
            (define (get-prompt) "> ")
            (define (eval-busy?)
              (not (and user-thread
                        (thread-running? user-thread))))
            
            (field (user-language-settings 'uninitialized-user-language-settings)
		   (user-custodian (make-custodian))
                   (user-eventspace #f)
                   (user-namespace #f)
                   (user-thread #f))
            
            (define (get-user-custodian) user-custodian)
            (define (get-user-eventspace) user-eventspace)
            (define (get-user-thread) user-thread)
            (define (get-user-namespace) user-namespace)
            
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
            
            ; =Kernel, =Handler=
            (define (do-many-text-evals text start end)
              (do-many-evals
               (lambda (single-loop-eval)  ; =User=, =Handler=
                 (let* ([text/pos (drscheme:language:make-text/pos text start end)]
                        [settings (current-language-settings)]
                        [lang (drscheme:language-configuration:language-settings-language settings)]
                        [settings (drscheme:language-configuration:language-settings-settings settings)]
                        [get-sexp/syntax/eof (send lang front-end text/pos settings)])
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
                                (eval sexp/syntax/eof))
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
                   (set-offer-break-state #f)
                   
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
		       (lambda () ; =Kernel=, =Handler= 
                         (cleanup-interaction)))))))))
            
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
            
            (define (set-offer-break-state new-ask-about-kill?)
	      (set! ask-about-kill? new-ask-about-kill?))
            (define (break)  ; =Kernel=, =Handler=
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
		 (set-offer-break-state #t)]))
            
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
                     (lambda () ; =Kernel=
                       (let ([ut user-thread])
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
                  (let ([saved-error-escape-k error-escape-k]
                        [cleanup? #f])
                    (dynamic-wind
                     (lambda ()
                       (set! cleanup? #f)
                       (set! error-escape-k (lambda () 
                                              (set! cleanup? #t)
                                              (k (void)))))
                     (lambda () 
                       (thunk) 
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

            ;; get-snip-classes : -> (listof snipclass)
            ;; returns a list of the snip classes in the current eventspace
            (define (get-snip-classes)
              (let loop ([n (send (get-the-snip-class-list) number)])
                (if (zero? n)
                    null
                    (cons (send (get-the-snip-class-list) nth (- n 1))
                          (loop (- n 1))))))

            (define init-evaluation-thread ; =Kernel=
              (lambda ()
                (set! user-language-settings
		      (preferences:get drscheme:language-configuration:settings-preferences-symbol))
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

		  ; setup standard parameters
                  (let ([snip-classes
                         ; the snip-classes in the DrScheme eventspace's snip-class-list
                         (get-snip-classes)])
                    (queue-user/wait
                     (lambda () ; =User=, =No-Breaks=
                       ; No user code has been evaluated yet, so we're in the clear...
                       (break-enabled #f)
                       (set! user-thread (current-thread))
                       (initialize-parameters snip-classes))))

                                  
                  ;; re-loads any teachpacks that have changed
                  ;; re-invokes all of the teachpacks
                  ;; must happen after user-namespace is initialized (in initialize-parameters)
                  (drscheme:teachpack:load-teachpacks user-namespace (preferences:get 'drscheme:teachpacks))
	      
                  ;; installs the teachpacks
                  (queue-user/wait
                   (lambda () ; =User=, =No-Breaks=
                     (drscheme:teachpack:install-teachpacks (preferences:get 'drscheme:teachpacks))))
                  
		  ;; initialize the language
		  (send (drscheme:language-configuration:language-settings-language user-language-settings)
			on-execute
			(drscheme:language-configuration:language-settings-settings user-language-settings)
			queue-user/wait)

		  (parameterize ([current-eventspace user-eventspace])
		    (queue-callback
		     (lambda ()
		       (let ([drscheme-error-escape-handler
			      (lambda ()
				(error-escape-k))])
			 (error-escape-handler drscheme-error-escape-handler))
                       
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
            
            ;; initialize-paramters : (listof snip-class%) -> void
            (define initialize-parameters ; =User=
              (lambda (snip-classes)
                
              	(current-language-settings user-language-settings)
                (error-value->string-handler drscheme-error-value->string-handler)
		(error-display-handler drscheme-error-display-handler)
                (current-load-relative-directory #f)
                (current-custodian user-custodian)
                
                (global-port-print-handler drscheme-port-print-handler)
                (setup-display/write-handlers)
                (pretty-print-size-hook drscheme-pretty-print-size-hook)
                (pretty-print-print-hook drscheme-pretty-print-print-hook)
                (print-convert:current-print-convert-hook
                 (lambda (expr basic-convert sub-convert)
		   (if (is-a? expr snip%)
		       expr
		       (basic-convert expr))))
                
                (set-basic-parameters snip-classes)
                (set! user-namespace (current-namespace))
                
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
                               
                               (set-offer-break-state #f)
                               
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
            
            (define (reset-console)
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
	       (lambda (fn applies?)
		 (insert-delta (string-append (string-constant teachpack) ": ")
                               (if applies? welcome-delta grey-delta))
		 (insert-delta fn
                               (if applies? dark-green-delta grey-delta))
		 (insert-delta (format ".~n") 
                               (if applies? welcome-delta grey-delta)))
	       (drscheme:teachpack:teachpack-cache-filenames (preferences:get 'drscheme:teachpacks))
               (drscheme:teachpack:teachpack-cache-applies (preferences:get 'drscheme:teachpacks)))
	      
	      (set! repl-initially-active? #t)
	      (end-edit-sequence)
	      
	      (super-reset-console))
            
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
                       (if (balance-required)
                           (let ([at-end-of-sexp?
				  (and
				   (only-spaces-after start)
				   (scheme-paren:balanced? this
							   prompt-position
							   last))])
                             (cond
			       [at-end-of-sexp?
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
                      [(start . < . prompt-position)
                       (let ([match (scheme-paren:backward-match this start 0)])
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
      
      (define -text% 
        (drs-bindings-keymap-mixin
         (make-text% 
          (make-console-text% scheme:text%)))))))
