(module debug mzscheme
  (require (lib "unitsig.ss")
           (lib "stacktrace.ss" "errortrace")
           (lib "class.ss")
           (lib "list.ss")
           (lib "date.ss")
           "drsig.ss"
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants"))
  
  (provide debug@)
  (define debug@
    (unit/sig drscheme:debug^
      (import [drscheme:rep : drscheme:rep^]
              [drscheme:frame : drscheme:frame^]
              [drscheme:unit : drscheme:unit^]
              [drscheme:language : drscheme:language^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^])
      
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
      (define cm-key (gensym 'drscheme-debug-continuation-mark-key))

      ;; error-delta : (instanceof style-delta%)
      (define error-delta (make-object style-delta% 'change-style 'slant))
      (send error-delta set-delta-foreground (make-object color% 255 0 0))
      
      ;; modern-style-delta : (instanceof style-delta%)
      (define modern-style-delta (make-object style-delta% 'change-family 'modern))

      ;; error-color : (instanceof color%)
      (define error-color (make-object color% "PINK"))
      
      ;; bug-note : (union string (instanceof snip%))
      (define bug-note 
        (let ([b (make-object bitmap% 
                   (build-path (collection-path "icons") "bug09.gif"))])
          (if (send b ok?)
              (make-object image-snip% b)
              "[err trace]")))

      ;; mf-note : (union string (instanceof snip%))
      (define mf-note
        (let ([bitmap
               (make-object bitmap%
                 (build-path (collection-path "icons") "mf.gif"))])
          (if (send bitmap ok?)
              (make-object image-snip% bitmap)
              "[err trace]")))
      
      ;; file-note : (union string (instanceof snip%))
      (define file-note (let ([b (make-object bitmap% 
                                  (build-path (collection-path "icons") "file.gif"))])
                         (if (send b ok?)
                             (make-object image-snip% b)
                             "[file]")))
      
      ;; add-debugging : (sexp -> value) -> sexp -> value
      ;; adds debugging information to `sexp' and calls `oe'
      (define (make-debug-eval-handler oe)
        (let ([debug-tool-eval-handler
               (lambda (exp)
                 (let ([annotated
                        (if (compiled-expression? 
                             (if (syntax? exp) (syntax-e exp) exp))
                            exp
                            (annotate-top (expand exp) null #f))])
                   (oe annotated)))])
          debug-tool-eval-handler))

      ;; simple-scheme-text% : (implements scheme:text<%>)
      (define simple-scheme-text% (scheme:text-mixin (editor:keymap-mixin text:basic%)))
      
      ;; make-debug-error-display-handler : (string (union TST exn) -> void) -> string exn -> void
      ;; adds in the bug icon, if there are contexts to display
      (define (make-debug-error-display-handler orig-error-display-handler)
        (define (debug-error-display-handler msg exn)
          (let ([rep (drscheme:rep:current-rep)])
            (cond
              [(eq? (send rep get-this-err) (current-error-port))
               (let ([cms (and (exn? exn) 
                               (continuation-mark-set? (exn-continuation-marks exn))
                               (continuation-mark-set->list 
                                (exn-continuation-marks exn)
                                cm-key))])
                 
                 (send rep begin-edit-sequence)
                 (send rep wait-for-io-to-complete/user)
                 
                 (when (and cms
                            (not (null? cms)))
                   (let ([locked? (send rep is-locked?)]
                         [mf-bday?
                          (let ([date (seconds->date (current-seconds))])
                            (and (= (date-month date) 10)
                                 (= (date-day date) 29)))])
                     (send rep lock #f)
                     (insert/clickback rep
                                       (if mf-bday? mf-note bug-note)
                                       (lambda ()
                                         (show-backtrace-window rep msg cms mf-bday?)))
                     (let ([debug-source (car (car cms))])
                       (when (symbol? debug-source)
                         (insert/clickback 
                          rep file-note
                          (lambda ()
                            (open-and-highlight-in-file (car cms))))))
                     (send rep lock locked?)))
                 
                 (orig-error-display-handler msg exn)
                 
                 (send rep wait-for-io-to-complete/user)
                 (when (and cms
                            (not (null? cms)))
                   (let* ([first-cms (car cms)]
                          [src (car first-cms)]
                          [position (cadr first-cms)]
                          [span (cddr first-cms)])
                     (when (and (object? src)
                                (is-a? src text:basic%))
                       (send rep highlight-error src position (+ position span)))))
                 
                 (send rep end-edit-sequence))]
              [else 
               (orig-error-display-handler msg exn)])))
        debug-error-display-handler)

      ;; insert/clickback : (instanceof text%) (union string (instanceof snip%)) (-> void)
      ;; inserts `note' and a space at the end of `rep'
      ;; also sets a clickback on the inserted `note' (but not the space).
      (define (insert/clickback rep note clickback)
	(let ([before (send rep last-position)])
	  (send rep insert (if (string? note)
			       note
			       (send note copy))
		before before)
	  (let ([after (send rep last-position)])
	    (send rep insert #\space after after)
	    (send rep set-clickback before after
		  (lambda (txt start end)
		    (clickback))))))

      ;; with-mark : syntax syntax -> syntax
      ;; a member of stacktrace-imports^
      ;; guarantees that the continuation marks associated with cm-key are
      ;; members of the debug-source type
      (define (with-mark mark expr)
        (let ([source (cond
                        [(string? (syntax-source mark))
                         (string->symbol (syntax-source mark))]
                        [(is-a? (syntax-source mark) editor<%>)
                         (syntax-source mark)]
                        [else #f])]
              [position (syntax-position mark)]
              [span (syntax-span mark)])
          (if (and source
                   (number? position)
                   (number? span))
              (with-syntax ([expr expr]
                            [source source]
                            [offset (- position 1)]
                            [span span]
                            [cm-key cm-key])
                (syntax
                 (with-continuation-mark
                  'cm-key
                  '(source offset . span)
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
                (string-constant backtrace-window-title)
                #f
                (preferences:get 'drscheme:backtrace-window-width)
                (preferences:get 'drscheme:backtrace-window-height)
                (preferences:get 'drscheme:backtrace-window-x)
                (preferences:get 'drscheme:backtrace-window-y))))

      ;; hide-backtrace-window : -> void
      (define (hide-backtrace-window)
        (when current-backtrace-window
          (send current-backtrace-window close)
          (set! current-backtrace-window #f)))
      
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
      ;;                         (listof (cons debug-source (cons number number)))
      ;;                         boolean
      ;;                         -> 
      ;;                         void
      (define (show-backtrace-window rep error-text dis mf-bday?)
        (reset-backtrace-window)
        (letrec ([text (make-object text:basic%)]
                 [mf-bday-note (when mf-bday?
                                 (instantiate message% ()
                                   (label "Happy Birthday, Matthias!")
                                   (parent current-backtrace-window)))]
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
                                        (format (if (<= num-left num-to-show)
                                                    (string-constant next-stack-frames)
                                                    (string-constant last-stack-frames))
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
          (send current-backtrace-window set-alignment 'center 'center)
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
      ;;              (cons debug-source (cons number number))
      ;;              -> 
      ;;              void 
      ;; shows one frame of the continuation
      (define (show-frame rep editor-canvas text di)
        (let* ([start (cadr di)]
               [span (cddr di)]
               [debug-source (car di)]
               [fn (get-filename debug-source)]
               [start-pos (send text last-position)])
          
          ;; make hyper link to the file
          (send text insert (format "~a: ~a-~a" fn start (+ start span)))
          (let ([end-pos (send text last-position)])
            (send text insert #\newline)
            (send text change-style (gui-utils:get-clickback-delta) start-pos end-pos)
            (send text set-clickback
                  start-pos end-pos
                  (lambda x
                    (open-and-highlight-in-file di))))
          
          (insert-context editor-canvas text debug-source start span)
          (send text insert #\newline)))
      
      ;; insert-context : (instanceof editor-canvas%)
      ;;                  (instanceof text%)
      ;;                  debug-info
      ;;                  number
      ;;                  -> 
      ;;                  void
      (define (insert-context editor-canvas text file start span)
        (let-values ([(from-text close-text)
                      (cond
                        [(symbol? file)
                         (let ([text (make-object simple-scheme-text%)])
                           (send text load-file (symbol->string file))
                           (values text 
                                   (lambda () (send text on-close))))]
                        [(is-a? file editor<%>)
                         (values file void)])])
          (let* ([finish (+ start span)]
                 [context-text (copy/highlight-text from-text start finish)])
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
        (let ([untitled (string-constant unknown-debug-frame)])
          (cond
            [(symbol? file) (symbol->string file)]
            [(is-a? file editor<%>)
             (let* ([canvas (send file get-canvas)]
                    [frame (and canvas (send canvas get-top-level-window))])
               (if (is-a? frame drscheme:unit:frame%)
                   (let ([filename (send (send frame get-definitions-text) get-filename)])
                     (cond
                       [(and filename (eq? file (send frame get-interactions-text)))
                        (format (string-constant files-interactions) filename)]
                       [(eq? file (send frame get-interactions-text))
                        (string-constant stack-frame-in-current-interactions)]
                       [filename filename]
                       [else (string-constant stack-frame-in-current-definitions)]))
                   (or (send file get-filename) 
                       untitled)))])))

      ;; open-and-highlight-in-file : (cons debug-source (cons number number))
      ;;                              -> 
      ;;                              void
      ;; opens the window displaying this piece of syntax (if there is one)
      ;; and highlights the right position in the file
      (define (open-and-highlight-in-file di)
        (let* ([debug-source (car di)]
               [position (cadr di)]
               [span (cddr di)]
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
                         [(is-a? debug-source editor<%>) debug-source])]
               [rep (and (is-a? frame drscheme:unit:frame%)
                         (send frame get-interactions-text))])
          (when frame
            (send frame show #t))
          (when (and rep editor)
            (cond
              [(is-a? editor text:basic<%>)
               (send rep highlight-error editor position (+ position span))])))))))
