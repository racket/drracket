
(module main mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "proxy-prefs.ss" "help")
           (lib "unitsig.ss")
           "drsig.ss"
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "unitsig.ss")
           (lib "class.ss")
           (prefix pretty-print: (lib "pretty.ss"))
           (prefix print-convert: (lib "pconvert.ss"))
	   (lib "include.ss")
           (lib "list.ss")
           (lib "file.ss")
           (lib "plt-installer.ss" "setup"))
  
  (provide main@)
  (define argv (current-command-line-arguments))
  
  (define main@
    (unit/sig ()
      (import [drscheme:app : drscheme:app^]
              [drscheme:unit : drscheme:unit^]
              [drscheme:get/extend : drscheme:get/extend^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^]
	      [drscheme:language : drscheme:language^]
              [drscheme:teachpack : drscheme:teachpack^]
              [drscheme:module-language : drscheme:module-language^]
              [drscheme:snip : drscheme:snip^]
              [drscheme:tools : drscheme:tools^]
              [drscheme:debug : drscheme:debug^]
              [drscheme:frame : drscheme:frame^])
      
      (finder:default-filters (cons '("Scheme (.scm)" "*.scm") (finder:default-filters)))
      (application:current-app-name (string-constant drscheme))
      ;(version:add-spec 'd 7)
      
      (preferences:set-default 'drscheme:unit-window-size-percentage 1/2 
                               (lambda (x) (and (number? x) (<= 0 x 1))))
      
      (let ([frame-width 600]
            [frame-height 650]
            [window-trimming-upper-bound-width 20]
            [window-trimming-upper-bound-height 50])
        (let-values ([(w h) (get-display-size)])
          (set! frame-width (min frame-width (- w window-trimming-upper-bound-width)))
          (set! frame-height (min frame-height (- h window-trimming-upper-bound-height))))
        (preferences:set-default 'drscheme:unit-window-width frame-width number?)
        (preferences:set-default 'drscheme:unit-window-height frame-height number?))
      
      (preferences:set-default 'drscheme:backtrace-window-width 400 number?)
      (preferences:set-default 'drscheme:backtrace-window-height 300 number?)
      
      (preferences:set-default 'drscheme:profile-how-to-count 'time
                               (lambda (x)
                                 (memq x '(time number))))
      (preferences:set-default 'drscheme:profile:low-color
                               (make-object color% 150 255 150)
                               (lambda (x) (is-a? x color%)))
      (preferences:set-default 'drscheme:profile:high-color
                               (make-object color% 255 150 150)
                               (lambda (x) (is-a? x color%)))
      (preferences:set-default 'drscheme:profile:scale
                               'linear
                               (lambda (x) (memq x '(sqrt linear square))))
      (let ([marshall-color 
             (lambda (c)
               (list (send c red) (send c green) (send c blue)))]
            [unmarshall-color
             (lambda (l)
               (if (and (list? l) 
                        (= 3 (length l))
                        (andmap (lambda (x) (and number? (<= 0 x 255)))
                                l))
                   (make-object color% (car l) (cadr l) (caddr l))
                   (make-object color% 0 0 0)))])
        (preferences:set-un/marshall 
         'drscheme:profile:low-color
         marshall-color
         unmarshall-color)
        (preferences:set-un/marshall 
         'drscheme:profile:high-color
         marshall-color
         unmarshall-color))
      
      (preferences:set-default 
       'drscheme:keybindings-window-size
       (cons 200 400)
       (lambda (x) (and (pair? x)
                        (number? (car x))
                        (number? (cdr x)))))
      
      (preferences:set-default
       'drscheme:execute-warning-once
       #f
       (lambda (x)
         (or (eq? x #t)
             (not x))))
      
      (preferences:set-default
       'drscheme:teachpacks
       (drscheme:teachpack:new-teachpack-cache) 
       drscheme:teachpack:teachpack-cache?)
      (preferences:set-un/marshall
       'drscheme:teachpacks
       drscheme:teachpack:marshall-teachpack-cache
       drscheme:teachpack:unmarshall-teachpack-cache)
      
      (define get-fixed-faces
        (cond
          [(eq? (system-type) 'unix) 
           (lambda () (get-face-list))]
          [else
           (lambda ()
             (let* ([canvas (make-object canvas% (make-object frame% "bogus"))]
                    [dc (send canvas get-dc)]
                    [ans
                     (let loop ([faces (get-face-list)])
                       (cond
                         [(null? faces) null]
                         [else (let* ([face (car faces)]
                                      [font (make-object font% 12 face 'default 'normal 'normal #f)])
                                 (let*-values ([(wi _1 _2 _3) (send dc get-text-extent "i" font)]
                                               [(ww _1 _2 _3) (send dc get-text-extent "w" font)])
                                   (if (and (= ww wi) 
                                            (not (zero? ww)))
                                       (cons face (loop (cdr faces)))
                                       (loop (cdr faces)))))]))])
               (set! get-fixed-faces (lambda () ans))
               ans))]))
      
      (define get-all-faces
        (cond
          [(eq? (system-type) 'unix) #f]
          [else (lambda () (get-face-list))]))
      
      (define default-font-name (get-family-builtin-face 'modern))
      
      (preferences:set-default 'drscheme:font-name default-font-name string?)
      
      (preferences:set-default
       'drscheme:font-size
       (send (send (send (make-object text%) 
                         get-style-list)
                   basic-style)
             get-size)
       (lambda (x) (and (number? x) (exact? x) (= x (floor x)))))
      
      (define (set-font-size size)
        (let* ([scheme-standard (send (scheme:get-style-list)
                                      find-named-style "Standard")]
               [scheme-delta (make-object style-delta%)])
          (send scheme-standard get-delta scheme-delta)
          (send scheme-delta set-size-mult 0)
          (send scheme-delta set-size-add size)
          (send scheme-standard set-delta scheme-delta)))
      
      (define (set-font-name name)
        (let* ([scheme-standard (send (scheme:get-style-list)
                                      find-named-style "Standard")]
               [scheme-delta (make-object style-delta%)])
          (send scheme-standard get-delta scheme-delta)
          (send scheme-delta set-delta-face name)
          (send scheme-delta set-family 'modern)
          (send scheme-standard set-delta scheme-delta)))
      
      (set-font-size (preferences:get 'drscheme:font-size))
      (set-font-name (preferences:get 'drscheme:font-name))
      
      (preferences:add-callback
       'drscheme:font-size
       (lambda (p v)
         (set-font-size v)))
      
      (preferences:add-callback
       'drscheme:font-name
       (lambda (p v)
         (set-font-name v)))
      
      (unless (member (preferences:get 'drscheme:font-name)
                      (get-fixed-faces))
        (preferences:set 'drscheme:font-name default-font-name))
      
      (preferences:add-panel
       (string-constant font-prefs-panel-title)
       (lambda (panel)
         (let* ([main (make-object vertical-panel% panel)]
                [options-panel (make-object horizontal-panel% main)]
                [size (make-object slider% (string-constant font-size) 1 72 options-panel
                        (lambda (size evt)
                          (preferences:set 'drscheme:font-size (send size get-value)))
                        (preferences:get 'drscheme:font-size))]
                
                [font-name-control
                 (case (system-type)
                   [(windows macos macosx)
                    (let ([choice
                           (make-object choice% (string-constant font-name)
                             (get-fixed-faces)
                             options-panel
                             (lambda (font-name evt)
                               (preferences:set 
                                'drscheme:font-name
                                (send font-name get-string-selection))))])
                      (send choice set-string-selection (preferences:get 'drscheme:font-name))
                      choice)]
                   [(unix)
                    (make-object button%
                      (string-constant set-font)
                      options-panel
                      (lambda xxx
                        (let* ([faces (get-fixed-faces)]
                               [init-choices
                                (let ([init (preferences:get 'drscheme:font-name)])
                                  (let loop ([faces faces]
                                             [num 0])
                                    (cond
                                      [(null? faces) null]
                                      [else
                                       (let ([face (car faces)])
                                         (if (equal? init face)
                                             (list num)
                                             (loop (cdr faces)
                                                   (+ num 1))))])))]
                               [choice (get-choices-from-user
                                        (string-constant select-font-name)
                                        (string-constant select-font-name)
                                        (get-fixed-faces)
                                        #f
                                        init-choices)])
                          (when choice
                            (preferences:set 
                             'drscheme:font-name 
                             (list-ref (get-fixed-faces) (car choice)))))))]
                   [else (error 'font-name-control "unknown system type: ~s~n" (system-type))])]
                
                [text (make-object text%)]
                [ex-panel (make-object horizontal-panel% main)]
                [msg (make-object message% (string-constant example-text) ex-panel)]
                [canvas (make-object editor-canvas% main text)]
                [update-text
                 (lambda (setting)
                   (send text begin-edit-sequence)
                   (send text lock #f)
                   (send text erase)
                   (send text insert 
                         (format
                          ";; howmany : list-of-numbers -> number~
                   \n;; to determine how many numbers are in `a-lon'~
                   \n(define (howmany a-lon)~
                       \n  (cond~
                            \n    [(empty? a-lon) 0]~
                            \n    [else (+ 1 (howmany (rest a-lon)))]))~
                            \n~
                            \n;; examples as tests~
                            \n(howmany empty)~
                            \n=~
                            \n0~
                            \n~
                            \n(howmany (cons 1 (cons 2 (cons 3 empty))))~
                            \n=~
                            \n3"))
                   (send text set-position 0 0)
                   (send text lock #t)
                   (send text end-edit-sequence))])
           
           (preferences:add-callback
            'drscheme:font-size
            (lambda (p v) (send size set-value v)))
           (preferences:add-callback
            drscheme:language-configuration:settings-preferences-symbol
            (lambda (p v)
              (update-text v)))
           (update-text (preferences:get drscheme:language-configuration:settings-preferences-symbol))
           (send ex-panel set-alignment 'left 'center)
           (send ex-panel stretchable-height #f)
           (send canvas allow-tab-exit #t)
           (send options-panel stretchable-height #f)
           (send options-panel set-alignment 'center 'top)
           (send text set-style-list (scheme:get-style-list))
           (send text lock #t)
           main)))
      
      (scheme:add-preferences-panel)
      (preferences:add-editor-checkbox-panel)
      (preferences:add-warnings-checkbox-panel)
      (preferences:add-scheme-checkbox-panel)
      (preferences:add-to-warnings-checkbox-panel
       (lambda (warnings-panel)
         (let ([make-check-box
                (lambda (pref-sym string)
                  (let ([q (make-object check-box%
                             string
                             warnings-panel
                             (lambda (checkbox evt)
                               (preferences:set 
                                pref-sym 
                                (send checkbox get-value))))])
                    (send q set-value (preferences:get pref-sym))))])
           (make-check-box 'drscheme:execute-warning-once (string-constant only-warn-once)))))
      (add-proxy-prefs-panel)
      (drscheme:debug:add-prefs-panel)
      
      ;; add a handler to open .plt files.
      (handler:insert-format-handler 
       "PLT Files"
       (lambda (filename)
         (and (equal? "plt" (filename-extension filename))
              (gui-utils:get-choice 
               (format (string-constant install-plt-file) filename)
               (string-constant install-plt-file/yes)
               (string-constant install-plt-file/no))))
       (lambda (filename)
         (run-installer filename)))
      
      (drscheme:tools:load/invoke-all-tools
       (lambda ()
         (void))
       (lambda ()
         (drscheme:language-configuration:add-built-in-languages)
         (drscheme:module-language:add-module-language)
         (drscheme:language-configuration:add-info-specified-languages)))
      
      ;; no more extension after this point
      (drscheme:get/extend:get-interactions-canvas)
      (drscheme:get/extend:get-definitions-canvas)
      (drscheme:get/extend:get-unit-frame)
      (drscheme:get/extend:get-interactions-text)
      (drscheme:get/extend:get-definitions-text)
      (drscheme:language-configuration:get-languages)
      
      (scheme:set-sexp-snip-class
       (class* (scheme:get-sexp-snip-class) (drscheme:snip:special<%>)
         (inherit-field saved-snips)
         (define/public (read-special file line col pos)
           (let ([text (make-object text:basic%)])
             (for-each
              (lambda (s) (send text insert (send s copy)
                                (send text last-position)
                                (send text last-position)))
              saved-snips)
             (values (datum->syntax-object
                      #f
                      (read (drscheme:language:open-input-text 
                             text
                             0
                             (send text last-position)))
                      (list file line col pos 1))
                     1)))
         (super-instantiate ())))
      
      ;; this default can only be set *after* the
      ;; languages have all be registered by tools
      (preferences:set-default
       drscheme:language-configuration:settings-preferences-symbol
       (drscheme:language-configuration:get-default-language-settings)
       drscheme:language-configuration:language-settings?)
      
      ;; if the unmarshaller returns #f, that will fail the
      ;; test for this preference, reverting back to the default.
      ;; In that case, the default is specified in the pref.ss file
      ;; of the default collection and may not be the default
      ;; specified below.
      (preferences:set-un/marshall
       drscheme:language-configuration:settings-preferences-symbol
       (lambda (x)
	 (let ([lang (drscheme:language-configuration:language-settings-language x)]
	       [settings (drscheme:language-configuration:language-settings-settings x)])
	   (list (send lang get-language-position)
		 (send lang marshall-settings settings))))
       (lambda (x)
	 (and (list? x)
	      (= 2 (length x))
	      (let* ([lang-position (first x)]
		     [marshalled-settings (second x)]
		     [lang (ormap
			    (lambda (x)
			      (and (equal? lang-position
					   (send x get-language-position))
				   x))
			    (drscheme:language-configuration:get-languages))])
		(and lang
		     (let ([settings (send lang unmarshall-settings marshalled-settings)])
		       (drscheme:language-configuration:make-language-settings
			lang
			(or settings (send lang default-settings)))))))))
      

      (handler:set-recent-items-frame-superclass
       (drscheme:frame:basics-mixin
        (frame:standard-menus-mixin
         frame:basic%)))
      
      ;;
      ;; Show expanded language dialog when version changes
      ;; 
      (preferences:set-default 'drscheme:last-version #f (lambda (x) (or (string? x) (not x))))
      (preferences:set-default 'drscheme:last-language #f (lambda (x) (or (symbol? x) (not x))))
      (drscheme:app:check-new-version)
      
      ;; the initial window doesn't set the 
      ;; unit object's state correctly, yet.
      (define (make-basic)
	(let* ([frame (drscheme:unit:open-drscheme-window)]
	       [interactions-edit (send frame get-interactions-text)]
	       [definitions-edit (send frame get-interactions-text)]
	       [filename (send definitions-edit get-filename)])
	  (unless filename
	    (send frame update-shown)
	    (send (send frame get-interactions-canvas) focus))
          (send frame show #t)))
      
      (define (remove-duplicates files)
        (let loop ([files files])
          (cond
            [(null? files) null]
            [else (if (member (car files) (cdr files))
                      (loop (cdr files))
                      (cons (car files) (loop (cdr files))))])))
      
      (define get-dropped-files (dynamic-require '(lib "splash.ss" "framework") 'get-dropped-files))
      (let* ([files-to-open (append (reverse (get-dropped-files))
				    (reverse (vector->list argv)))]
             [normalized/filtered
              (let loop ([files files-to-open])
                (cond
                  [(null? files) null]
                  [else (let ([file (car files)])
                          (if (file-exists? file)
                              (cons (normalize-path file) (loop (cdr files)))
                              (begin
                                (message-box
                                 (string-constant drscheme)
                                 (format (string-constant cannot-open-because-dne) file))
                                (loop (cdr files)))))]))]
             [no-dups (remove-duplicates normalized/filtered)])
        (if (null? no-dups)
            (make-basic)
            (for-each (lambda (f) (handler:edit-file
                                   f
                                   (lambda () (drscheme:unit:open-drscheme-window f))))
                      no-dups))))))
