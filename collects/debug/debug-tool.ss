(module debug-tool mzscheme
  (require (lib "unitsig.ss")
           (lib "stacktrace.ss" "errortrace")
           (lib "class.ss")
           (lib "list.ss")
           (lib "tool.ss" "drscheme")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants"))
  
  (provide tool@)
  (define tool@
    (unit/sig () 
      (import drscheme:tool^)
      
      ;; type debug-source = (union symbol (instanceof editor<%>))
      
      ;; preferences defaults
      (preferences:set-default 'drscheme:backtrace-window-width 600 number?)
      (preferences:set-default 'drscheme:backtrace-window-height 400 number?)
      (preferences:set-default 'drscheme:backtrace-window-x 0 number?)
      (preferences:set-default 'drscheme:backtrace-window-y 0 number?)
      
      ;; original-output-port : output-port
      ;; for debugging -- be sure to print to here, not the current output port
      (define original-output-port (current-output-port))
      
      ;; cm-key : symbol
      ;; the key used to put information on the continuation
      (define cm-key (gensym 'debug-tool-continuation-mark-key))

      ;; error-delta : (instanceof style-delta%)
      (define error-delta (make-object style-delta% 'change-style 'slant))
      (send error-delta set-delta-foreground (make-object color% 255 0 0))
      
      ;; modern-style-delta : (instanceof style-delta%)
      (define modern-style-delta (make-object style-delta% 'change-family 'modern))

      ;; error-color : (instanceof color%)
      (define error-color (make-object color% "PINK"))
      
      ;; bug-snip : (union string (instanceof snip%))
      (define bug-snip (let ([b (make-object bitmap% 
                                  (build-path (collection-path "icons") "bug09.gif"))])
                         (if (send b ok?)
                             (make-object image-snip% b)
                             "[err]")))
      
      ;; simple-scheme-text% : (implements scheme:text<%>)
      (define simple-scheme-text% (scheme:text-mixin (editor:keymap-mixin text:basic%)))
      
      ;; debug-lang-mixin : (implements language<%>) -> (implements language<%>)
      (define (debug-lang-mixin %)
        (class %
          (override on-execute)
          (rename [super-on-execute on-execute])
          (define (on-execute settings run-in-user-thread)
            (super-on-execute settings run-in-user-thread)
            (run-in-user-thread
             (lambda ()
               (current-eval (add-debugging (current-eval)))
               (error-display-handler
                (make-debug-tool-error-display-handler (error-display-handler))))))
          (super-instantiate ())))

      ;; add-debugging : (sexp -> value) -> sexp -> value
      ;; adds debugging information to `sexp' and calls `oe'
      (define (add-debugging oe)
        (let ([debug-tool-eval-handler
               (lambda (exp)
                 (let ([annotated
                        (if (compiled-expression? 
                             (if (syntax? exp) (syntax-e exp) exp))
                            exp
                            (annotate-top (expand exp) null #f))])
                   (oe annotated)))])
          debug-tool-eval-handler))

      ;; debug-tool-error-display-handler : (string exn -> void) -> string exn -> void
      ;; adds in the bug icon, if there are contexts to display
      (define (make-debug-tool-error-display-handler orig-error-display-handler)
        (define (debug-tool-error-display-handler msg exn)
          (let ([cms (continuation-mark-set->list (exn-continuation-marks exn) cm-key)]
                [rep (drscheme:rep:current-rep)])
          
            (let ([locked? (send rep is-locked?)])
              (send rep lock #f)
              (let ([before (send rep last-position)])
                (send rep insert (send bug-snip copy) before before)
                (let ([after (send rep last-position)])
                  (send rep insert #\space after after)
                  (send rep set-clickback before after
                        (lambda (txt start end)
                          (show-backtrace-window rep msg cms))))
                (send rep lock locked?)))
            
            (orig-error-display-handler msg exn)
            
            (unless (null? cms)
              (let* ([first-cms (car cms)]
                     [src (car first-cms)]
                     [position (cdr first-cms)])
                (when (and (object? src)
                           (is-a? src text:basic%)
                           (number? position))
                  (send rep highlight-error/forward-sexp src position))))))
        debug-tool-error-display-handler)
      
      ;; wrap : syntax syntax -> syntax
      ;; a member of stacktrace-imports^
      ;; guarantees that the continuation marks associated with cm-key are
      ;; members of the debug-source type
      (define (with-mark mark expr)
        (let ([source (cond
                        [(string? (syntax-source mark))
                         (string->symbol (syntax-source mark))]
                        [(is-a? (syntax-source mark) editor<%>)
                         (syntax-source mark)]
                        [else #f])])
          (if source
              (with-syntax ([expr expr]
                            [source source]
                            [offset (- (syntax-position mark) 1)]
                            [cm-key cm-key])
                (syntax
                 (with-continuation-mark
                  'cm-key
                  '(source . offset)
                  expr)))
              expr)))

      ;; an unused stacktrace-import^
      (define (profile-point body name expr env trans?) body)
      
      (define-values/invoke-unit/sig stacktrace^ stacktrace@ #f stacktrace-imports^)

      ;; current-backtrace-window : (union #f (instanceof frame:basic<%>))
      ;; the currently visible backtrace window, or #f, if none
      (define current-backtrace-window #f)

      ;; reset-backtrace-window : -> void
      ;; effect: updates current-backtrace-window
      ;; closes the current backtrace window and creates a new (unshown) one
      (define (reset-backtrace-window)
        (when current-backtrace-window
          (send current-backtrace-window close)
          (set! current-backtrace-window #f))
        
        (set! current-backtrace-window 
              (make-object backtrace-frame%
                "Backtrace - DrScheme"
                #f
                (preferences:get 'drscheme:backtrace-window-width)
                (preferences:get 'drscheme:backtrace-window-height)
                (preferences:get 'drscheme:backtrace-window-x)
                (preferences:get 'drscheme:backtrace-window-y))))
      
      ;; backtrace-frame% : (extends frame:basic<%>)
      (define backtrace-frame%
        (class (drscheme:frame:basics-mixin (frame:standard-menus-mixin frame:basic%))
          (rename [super-on-size on-size]
                  [super-on-move move])
          (override on-size file-menu:between-print-and-close edit-menu:between-find-and-preferences)
          (define (on-size x y)
            (preferences:set 'drscheme:backtrace-window-width x)
            (preferences:set 'drscheme:backtrace-window-height y)
            (super-on-size x y))
          (define (on-move x y)
            (preferences:set 'drscheme:backtrace-window-x x)
            (preferences:set 'drscheme:backtrace-window-y y)
            (super-on-move x y))
          (define (file-menu:between-print-and-close file-menu) (void))
          (define (edit-menu:between-find-and-preferences edit-menu) (void))
          (super-instantiate ())))
            
      ;; show-backtrace-window : (union #f (instanceof drscheme:rep:text%) 
      ;;                         string
      ;;                         (listof (cons debug-source number)) 
      ;;                         -> 
      ;;                         void
      (define (show-backtrace-window rep error-text dis)
        (reset-backtrace-window)
        (letrec ([text (make-object text:basic%)]
                 [ec (make-object canvas:wide-snip% 
                       (send current-backtrace-window get-area-container)
                       text)]
                 [di-vec (list->vector dis)]
                 [index 0]
                 [how-many-at-once 15]
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
                           (show-frame rep ec text (vector-ref di-vec n))
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
                              (send text change-style (gui-utils:get-clickback-delta)
                                    hyper-start hyper-end)
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
          (send text insert "\n\n")
          (send text change-style error-delta 0 (- (send text last-position) 1))
          (show-next-dis)
          (send text set-position 0 0)
          (send text lock #t)
          (send text hide-caret #t)
          (send current-backtrace-window show #t)))
      
      ;; show-frame : (union #f (instanceof drscheme:rep:text%))
      ;;              (instanceof editor-canvas%)
      ;;              (instanceof text%) 
      ;;              (cons debug-source number) 
      ;;              -> 
      ;;              void 
      ;; shows one frame of the continuation
      (define (show-frame rep editor-canvas text di)
        (let* ([start (cdr di)]
               [debug-source (car di)]
               [fn (get-filename debug-source)]
               [start-pos (send text last-position)])
          
          ;; make hyper link to the file
          (send text insert (format "~a: ~a" fn start))
          (let ([end-pos (send text last-position)])
            (send text insert #\newline)
            (send text change-style (gui-utils:get-clickback-delta) start-pos end-pos)
            (send text set-clickback
                  start-pos end-pos
                  (lambda x
                    (open-and-highlight-in-file rep di))))
          
          (insert-context editor-canvas text debug-source start)
          (send text insert #\newline)))
      
      ;; insert-context : (instanceof editor-canvas%)
      ;;                  (instanceof text%)
      ;;                  debug-info
      ;;                  number
      ;;                  -> 
      ;;                  void
      (define (insert-context editor-canvas text file start)
        (let-values ([(from-text close-text finish)
                      (cond
                        [(symbol? file)
                         (let ([text (make-object simple-scheme-text%)])
                           (send text load-file (symbol->string file))
                           (values text (lambda () (send text on-close))
                                   (or (send text get-forward-sexp start)
                                       (+ start 1))))]
                        [(is-a? file scheme:text<%>) 
                         (values file 
                                 void
                                 (or (send file get-forward-sexp start)
                                     (+ start 1)))]
                        [(is-a? file editor<%>)
                         (values file void (+ start 1))])])
          (let ([context-text (copy/highlight-text from-text start finish)])
            (send context-text change-style modern-style-delta 0 (send context-text last-position))
            (send context-text lock #t)
            (send context-text hide-caret #t)
            (send text insert "  ")
            (let ([snip (make-object editor-snip% context-text)])
              (send editor-canvas add-wide-snip snip)
              (send text insert snip))
            (send text insert #\newline))
          (close-text)))

      ;; copy/highlight-text : (instanceof scheme:text<%>) number number -> (instanceof scheme:text<%>)
      ;; copies the range from `start' to `finish', including the entire paragraph at
      ;; each end and highlights the characters corresponding the original range,
      ;; in the resulting text
      (define (copy/highlight-text from-text start finish)
        (let* ([to-text (make-object simple-scheme-text%)]
               [para-start-pos (send from-text paragraph-start-position 
                                     (send from-text position-paragraph start))]
               [para-end-pos (send from-text paragraph-end-position
                                   (send from-text position-paragraph 
                                         finish))]
               [from-start (- start para-start-pos)]
               [from-end (+ from-start (- finish start))])
          (send from-text split-snip para-start-pos)
          (send from-text split-snip para-end-pos)
          (let loop ([snip (send from-text find-snip para-start-pos 'after-or-none)])
            (when (and snip
                       (< (send from-text get-snip-position snip) para-end-pos))
              (send to-text insert (send snip copy))
              (loop (send snip next))))
          (send to-text highlight-range from-start from-end error-color #f #f 'high)
          to-text))
      
      ;; get-filename : debug-source -> string
      (define (get-filename file)
        (let ([untitled "<<unknown>>"])
          (cond
            [(symbol? file) (symbol->string file)]
            [(is-a? file editor<%>)
             (let* ([canvas (send file get-canvas)]
                    [frame (and canvas (send canvas get-top-level-window))])
               (if (is-a? frame drscheme:unit:frame%)
                   (let ([filename (send (send frame get-definitions-text) get-filename)])
                     (cond
                       [(and filename (eq? file (send frame get-interactions-text)))
                        (string-append filename "'s interactions")]
                       [(eq? file (send frame get-interactions-text))
                        "interactions"]
                       [filename filename]
                       [else "definitions"]))
                   (or (send file get-filename) 
                       untitled)))])))

      ;; open-and-highlight-in-file : (union #f (instanceof drscheme:rep:text%))
      ;;                              (cons debug-source number)
      ;;                              -> 
      ;;                              void
      ;; opens the window displaying this piece of syntax (if there is one)
      ;; and highlights the right position in the file
      (define (open-and-highlight-in-file rep di)
        (let* ([debug-source (car di)]
               [position (cdr di)]
               [frame (cond
                        [(symbol? debug-source) (handler:edit-file (symbol->string debug-source))]
                        [(is-a? debug-source editor<%>)
                         (let ([canvas (send debug-source get-canvas)])
                           (and canvas
                                (send canvas get-top-level-window)))])]
               [editor (cond
                         [(symbol? debug-source)
                          (cond
                            [(and frame (is-a? frame drscheme:unit:frame%))
                             (send frame get-definitions-text)]
                            [(and frame (is-a? frame frame:editor<%>))
                             (send frame get-editor)]
                            [else #f])]
                         [(is-a? debug-source editor<%>) debug-source])])
          (when frame
            (send frame show #t))
          (when (and rep editor)
            (cond
              [(is-a? editor scheme:text<%>)
               (send rep highlight-error/forward-sexp editor position)]
              [(is-a? editor text:basic<%>)
               (send rep highlight-error editor position (+ position 1))]))))
      
      ;; debug-lang% : (implements drscheme:language-tower:language<%>)
      (define debug-lang%
        (debug-lang-mixin 
         (drscheme:language-tower:module-based-language->language-mixin
          (drscheme:language-tower:simple-module-based-language->module-based-language-mixin
           drscheme:language-tower:simple-module-based-language%))))
      
      ;; add-debug-lang : module-spec (cons string (listof string)) -> void
      ;; adds a debugging language to drscheme's list of languages
      (define (add-debug-lang module position)
        (drscheme:language:add-language
         (instantiate debug-lang% ()
           (module module)
           (language-position position)
           (teachpack-names null))
         #t))

      (add-debug-lang '(lib "full-mzscheme.ss" "lang") '("Full" "Textual (MzScheme)"))
      (add-debug-lang '(lib "full-mred.ss" "lang") '("Full" "Graphical (MrEd)")))))