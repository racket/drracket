#|

profile todo:

   - clickable filenames
   - remove highlighting when program edited.
   - show/hide for profile window
     - dialog box saying "no profile info"
   - separate out debugging and profiling in language dialog
   - document drscheme:debug stuff
   
|#

(module debug mzscheme
  (require (lib "unitsig.ss")
           (lib "stacktrace.ss" "errortrace")
           (lib "class.ss")
           (lib "list.ss")
           (lib "date.ss")
           "drsig.ss"
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "thread.ss")
           (lib "string-constant.ss" "string-constants"))
  
  (provide debug@)
  (define debug@
    (unit/sig drscheme:debug^
      (import [drscheme:rep : drscheme:rep^]
              [drscheme:frame : drscheme:frame^]
              [drscheme:unit : drscheme:unit^]
              [drscheme:language : drscheme:language^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^])
      
      

                                                        
;;                             ;                        
 ;                                                      
 ;                                                      
 ;;;;  ;;  ;;   ;;; ;        ;;;     ;;;    ;;;  ; ;;;  
 ;   ;  ;   ;  ;   ;           ;    ;   ;  ;   ;  ;;  ; 
 ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
 ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
 ;   ;  ;   ;  ;   ;           ;    ;   ;  ;   ;  ;   ; 
; ;;;    ;;; ;  ;;;;         ;;;;;   ;;;    ;;;  ;;;  ;;
                   ;                                    
                   ;                                    
                ;;;                                     

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
      
      ;; make-debug-eval-handler : (sexp -> value) -> sexp -> value
      ;; adds debugging information to `sexp' and calls `oe'
      (define (make-debug-eval-handler oe)
        (let ([debug-tool-eval-handler
               (lambda (exp)
                 (let ([annotated
                        (if (compiled-expression? 
                             (if (syntax? exp) (syntax-e exp) exp))
                            exp
                            (annotate-top (expand exp) #f))])
                   (oe annotated)))])
          debug-tool-eval-handler))

      ;; simple-scheme-text% : (implements scheme:text<%>)
      (define simple-scheme-text% (scheme:text-mixin
                                   (text:hide-caret/selection-mixin
                                    (editor:keymap-mixin 
                                     text:basic%))))
      
      ;; make-debug-error-display-handler : (string (union TST exn) -> void) -> string (union TST exn) -> void
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
                 (send rep queue-output
                       (lambda ()
                         
                         (send rep begin-edit-sequence)
                         
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
                         (send rep end-edit-sequence)))
                 (orig-error-display-handler msg exn)
                 (send rep queue-output
                       (lambda ()
                         (when (and cms
                                    (not (null? cms)))
                           (let* ([first-cms (car cms)]
                                  [src (car first-cms)]
                                  [position (cadr first-cms)]
                                  [span (cddr first-cms)])
                             (when (and (object? src)
                                        (is-a? src text:basic%))
                               (send rep highlight-error src position (+ position span))))))))]
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
        (letrec ([text (make-object text:hide-caret/selection%)]
                 [mf-bday-note (when mf-bday?
                                 (instantiate message% ()
                                   (label (string-constant happy-birthday-matthias))
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
                                        (format (if (num-left . <= . num-to-show)
                                                    (string-constant last-stack-frames)
                                                    (string-constant next-stack-frames))
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
                                      (send text set-position 
                                            (send text last-position)
                                            (send text last-position))
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
               (send rep highlight-error editor position (+ position span))]))))

      
      
                                                               
                        ;;;    ;    ;;;      ;                 
                       ;              ;                        
                       ;              ;                        
; ;;;   ; ;;;   ;;;   ;;;;;  ;;;      ;    ;;;   ; ;;;    ;;; ;
 ;   ;   ;     ;   ;   ;       ;      ;      ;    ;;  ;  ;   ; 
 ;   ;   ;     ;   ;   ;       ;      ;      ;    ;   ;  ;   ; 
 ;   ;   ;     ;   ;   ;       ;      ;      ;    ;   ;  ;   ; 
 ;   ;   ;     ;   ;   ;       ;      ;      ;    ;   ;  ;   ; 
 ;;;;   ;;;;    ;;;   ;;;;   ;;;;;  ;;;;;; ;;;;; ;;;  ;;  ;;;; 
 ;                                                           ; 
 ;                                                           ; 
;;;                                                       ;;;  

      
      (define profile-key (gensym))
      (define profiling-enabled (make-parameter #t))
      (define currently-collecting-profile-info (make-parameter #t))

      ;; prof-info =
      ;; (make-prof-info
      ;;    boolean ;; protect against nested calls
      ;;    number[number of calls]
      ;;    number[time spent in all calls]
      ;;   (union #f symbol) 
      ;;   expression)
      (define-struct prof-info (nest num time name expr))
      
      ;; type prof-map =
      ;;   symbol -o> prof-info
      ;; profile-info : (parameter (union #f prof-map))
      ;; this parameter maps from user threads to hash tables
      ;; and is initialized in `initialize-profile-point'. the init code
      ;; ensures that even if the user creates multiple threads, only
      ;; one hash table is created. thus, the register-profile-start
      ;; and register-profile-done thunks could be running on multiple threads

      (define-values (profile-info get-profile-info/thd)
        (let ([ht (make-hash-table 'weak)])
          (values
           (case-lambda
             [() (hash-table-get ht (current-thread) (lambda () #f))]
             [(v) (hash-table-put! ht (current-thread) v)])
           (lambda (thd)
             (hash-table-get ht thd (lambda () #f))))))
      
      (define (initialize-profile-point key name expr)
        (unless (profile-info)
          (profile-info (make-hash-table)))
        (hash-table-put! (profile-info) 
                         key 
                         (make-prof-info #f 0 0 (and name (syntax-e name)) expr)))
  
      ;; register-profile-start : sym -> (union #f number)
      ;; =user=
      (define (register-profile-start key)
        (and (currently-collecting-profile-info)
             (let ([info (hash-table-get (profile-info) key)])
               (set-prof-info-num! info (+ (prof-info-num info) 1))
               (if (prof-info-nest info)
                   #f
                   (begin
                     (set-prof-info-nest! info #t)
                     (current-process-milliseconds))))))
      
      ;; register-profile-done : sym (union #f number) -> void
      ;; =user=
      (define (register-profile-done key start)
        (when start
          (let ([info (hash-table-get (profile-info) key)])
            (set-prof-info-nest! info #f)
            (set-prof-info-time! info
                                 (+ (- (current-process-milliseconds) start)
                                    (prof-info-time info))))))
     

      ;; get-color-value : number number -> (is-a?/c color%)
      ;; returns the profiling color
      ;; for `val' if `max-val' is the largest
      ;; of any profiling amount.
      (define (get-color-value val max-val)
        (let* ([color-min (preferences:get 'drscheme:profile:low-color)]
               [color-max (preferences:get 'drscheme:profile:high-color)]
               [adjust
                (case (preferences:get 'drscheme:profile:scale)
                  [(sqrt) sqrt]
                  [(square) (lambda (x) (* x x))]
                  [(linear) (lambda (x) x)])]
               [factor (adjust (if (zero? max-val) 0 (/ val max-val)))]
               [get-rgb-value
                (lambda (sel)
                  (let ([small (sel color-min)]
                        [big (sel color-max)])
                    (inexact->exact (floor (+ (* factor (- big small)) small)))))])
          (make-object color% 
            (get-rgb-value (lambda (x) (send x red)))
            (get-rgb-value (lambda (x) (send x green)))
            (get-rgb-value (lambda (x) (send x blue))))))
      
      ;; extract-maximum : prof-map -> number
      ;; gets the maximum value of the currently preferred profiling info.
      (define (extract-maximum ht)
        (let ([max-value 0]
              [sel (if (eq? (preferences:get 'drscheme:profile-how-to-count) 'time)
                       prof-info-num
                       prof-info-time)])
          (hash-table-for-each
           ht
           (lambda (key val)
             (set! max-value (max max-value (sel val)))))
          max-value))
      
      ;; profile-unit-frame-mixin : mixin
      ;; adds profiling to the unit frame
      (define (profile-unit-frame-mixin %)
        (class %

          (rename [super-make-root-area-container make-root-area-container])
          (field [profile-info-outer-panel #f])
          (define/override (make-root-area-container % parent)
            (set! profile-info-outer-panel
                  (super-make-root-area-container
                   vertical-panel%
                   parent))
            (make-object % profile-info-outer-panel))
          
          (super-instantiate ())
          
          (define profile-info-text (instantiate profile-text% (this)))
          (define profile-info-panel (instantiate horizontal-panel% ()
                                       (parent profile-info-outer-panel)
                                       (stretchable-height #f)))
          (define profile-left-side (instantiate vertical-panel% (profile-info-panel)))
          (define profile-info-editor-canvas (instantiate canvas:basic% (profile-info-panel profile-info-text)))
          (define profile-message (instantiate message% ()
                                    (label (string-constant profiling))
                                    (parent profile-left-side)))
          (define profile-choice (instantiate radio-box% ()
                                   (label #f)
                                   (parent profile-left-side)
                                   (callback
                                    (lambda (x y)
                                      (preferences:set 'drscheme:profile-how-to-count
                                                       (case (send profile-choice get-selection)
                                                         [(0) 'time]
                                                         [(1) 'count]))
                                      (send profile-info-text refresh-profile)))
                                   (choices (list (string-constant profiling-time)
                                                  (string-constant profiling-number)))))
          (define update-profile-button
            (instantiate button% ()
              (label (string-constant profiling-update))
              (parent profile-left-side)
              (callback
               (lambda (x y)
                 (send profile-info-text refresh-profile)))))
          (define clear-profile-button 
            (instantiate button% ()
              (label (string-constant profiling-clear))
              (parent profile-left-side)
              (callback
               (lambda (x y)
                 (send profile-info-text clear-prof-map)))))
          (send profile-choice set-selection (case (preferences:get 'drscheme:profile-how-to-count)
                                               [(time) 0]
                                               [(count) 1]))
          
          (send profile-left-side stretchable-width #f)
          
          (let ([wid (max (send update-profile-button get-width)
                          (send clear-profile-button get-width)
                          (send profile-choice get-width)
                          (send profile-message get-width))])
            (send update-profile-button min-width wid)
            (send clear-profile-button min-width wid)
            (send profile-choice min-width wid))
          (send profile-left-side set-alignment 'left 'center)))
      
      ;; profile-text% : extends text:basic%
      ;; this class keeps track of a single thread's
      ;; profiling information.
      (define profile-text% 
        (class text:basic%

          (init-field definitions-frame)

          (define/private (get-user-thread)
            (let ([t (send definitions-frame get-interactions-text)])
              (send t get-user-thread)))

          ;; clear-prof-map : (lambda () (get-user-thread)) . -> . void?
          ;; resets the profiling information for this thread
          ;; =kernel=
          ;; since this thread runs on a drscheme thread,
          ;; we just bang on the hash table so the user's program
          ;; cannot grab the semaphore and block us forever.
          (define/public (clear-prof-map)
            (hash-table-for-each
             (get-profile-info/thd (get-user-thread))
             (lambda (k info)
               (set-prof-info-num! info 0)
               (set-prof-info-time! info 0)))
            (erase)
            (clear-profile-results))

          ;; refresh-profile : -> void
          ;; called when this editor becomes visible
          ;; shows the profiling information with text highlighting 
          ;; and in this editor
          (define/public (refresh-profile)
            (when (get-user-thread)
              (refresh-profile/work)))
          
          ;; hide-profile : -> void
          ;; called when this editor is hidden.
          ;; clears the profile color information and the text in this editor
          (define/public (hide-profile)
            (clear-profile-results))

          ;; can-show-profile? : -> boolean
          ;; indicates if there is any profiling information to be shown.
          ;; used by the show menu to grey out the `show-profile' menu item
          (define/public (can-show-profile?)
            (and (get-user-thread)
                 (get-profile-info/thd (get-user-thread))))
          
          (inherit begin-edit-sequence end-edit-sequence erase insert)
          (field [clear-old-results void])
          
          ;; clear-profile-results : -> void
          ;; does the work to erase the profiling info.
          (define/public (clear-profile-results)
            (begin-edit-sequence)
            (clear-old-results)
            (set! clear-old-results void)
            (erase)
            (end-edit-sequence))
          
          ;; refresh-profile-results : ((lambda () (get-user-thread)) . -> . void?)
          ;; does the work to erase any existing profile info
          ;; and make new profiling info.
          (define (refresh-profile/work)
            (begin-edit-sequence)
            (clear-profile-results)
            (let* ([ht (get-profile-info/thd (get-user-thread))])
              (when ht
                (let* (
                       ;; each editor that gets some highlighting is put
                       ;; into this table and an edit sequence is begun for it.
                       ;; after all ranges are updated, the edit sequences are all closed.
                       [in-edit-sequence (make-hash-table)]
                       [thnk void]
                       [max-value (extract-maximum ht)]
                       [infos (hash-table-map ht (lambda (key val) val))]
                       [show-highlight
                        (lambda (info)
                          (let* ([count (prof-info-num info)]
                                 [time (prof-info-time info)]
                                 [expr (prof-info-expr info)]
                                 [src (syntax-source expr)]
                                 [pos (syntax-position expr)]
                                 [span (syntax-span expr)])
                            (when (and src 
                                       (is-a? src text:basic<%>)
                                       (number? pos)
                                       (number? span))
                              (unless (hash-table-get in-edit-sequence src (lambda () #f))
                                (hash-table-put! in-edit-sequence src #t)
                                (send src begin-edit-sequence))
                              (let* ([color (get-color-value time max-value)]
                                     [clr (send src highlight-range (- pos 1) (+ pos span -1) color)])
                                (let ([old-thnk thnk])
                                  (set! thnk
                                        (lambda ()
                                          (clr)
                                          (old-thnk))))))))]
                       [smaller-range?
                        (lambda (x y)
                          (let ([x-span (syntax-span (prof-info-expr x))]
                                [y-span (syntax-span (prof-info-expr y))])
                            (if (and x-span y-span)
                                (< x-span y-span)
                                #f)))]
                       
                       [show-line
                        (lambda (info newline?)
                          (let ([expr (prof-info-expr info)]
                                [count (prof-info-num info)]
                                [time (prof-info-time info)]
                                [name (prof-info-name info)])
                            (when newline? (send src-editor insert "\n"))
                            (send src-editor insert "clickable name here")

                            (when newline? (send time-editor insert "\n"))
                            (send time-editor insert (format "~a" time))
                            (send time-editor set-paragraph-alignment (send time-editor last-paragraph) 'right)
                            
                            (when newline? (send count-editor insert "\n"))
                            (send count-editor insert (format "~a" count))
                            (send count-editor set-paragraph-alignment (send time-editor last-paragraph) 'right)
                            
                            (when newline? (send name-editor insert "\n"))
                            (when name
                              (send name-editor insert (format "~a" name)))))]
                       
                       [bigger-value?
                        (lambda (x y)
                          (let ([sel (if (eq? 'count (preferences:get 'drscheme:profile-how-to-count))
                                         prof-info-num
                                         prof-info-time)])
                            (> (sel x) (sel y))))]
                       
                       [update-max-width
                        (lambda (ed)
                          (let ([admin (send ed get-admin)]
                                [bw (box 0)])
                            (send admin get-view #f #f bw #f)
                            (let ([w (+ (unbox bw) 4)])
                              (send ed set-max-width w)
                              (send ed set-min-width w))))])
                  (for-each show-highlight (quicksort infos smaller-range?))
                  (initialize-editors)
                  (let loop ([infos (quicksort infos bigger-value?)]
                             [newline? #f])
                    (cond
                      [(null? infos) (void)]
                      [else 
                       (show-line (car infos) newline?)
                       (loop (cdr infos) #t)]))
                  (update-max-width src-editor)
                  (update-max-width time-editor)
                  (update-max-width count-editor)
                  (update-max-width name-editor)
                  
                  (hash-table-for-each
                   in-edit-sequence
                   (lambda (key val)
                     (send key end-edit-sequence)))
                  (set! clear-old-results thnk))))
            (end-edit-sequence))
          
          (field (src-editor #f)
                 (name-editor #f)
                 (time-editor #f)
                 (count-editor #f))
          (define (clear-editors)
            (set! src-editor #f)
            (set! name-editor #f)
            (set! time-editor #f)
            (set! count-editor #f))
          (define (initialize-editors)
            (set! src-editor (instantiate text% ()))
            (set! name-editor (instantiate text% ()))
            (set! time-editor (instantiate text% ()))
            (set! count-editor (instantiate text% ()))
            (insert (instantiate editor-snip% (src-editor)))
            (insert (instantiate editor-snip% (name-editor)))
            (insert (instantiate editor-snip% (time-editor)))
            (insert (instantiate editor-snip% (count-editor)))
            (insert-title (string-constant profiling-col-filename) src-editor)
            (insert-title (string-constant profiling-col-name) name-editor)
            (insert-title (string-constant profiling-col-time) time-editor)
            (insert-title (string-constant profiling-col-count) count-editor))
          
          (define (insert-title str txt)
            (send txt insert str)
            (send txt insert "\n")
            (send txt change-style bold-delta 0 (- (send txt last-position) 1))
            (send txt set-paragraph-alignment 0 'center))
          
          (super-instantiate ())))

      ;; bold-delta : style-delta
      (define bold-delta (make-object style-delta% 'change-bold))
      
      ;; get-src-filename : tst -> (union #f string)
      (define (get-src-filename src)
        (cond
          [(string? src) src]
          [(is-a? src text%)
           (send src get-filename)]
          [else #f]))
      
      ;; get-src-loc : syntax -> string
      (define (get-src-lock expr)
        (cond
          [(and (number? (syntax-line expr))
                (number? (syntax-column expr))
                (number? (syntax-span expr)))
           (format " ~a.~a [~a]" 
                   (syntax-line expr) 
                   (syntax-column expr)
                   (syntax-span expr))]
          [(and (number? (syntax-position expr))
                (number? (syntax-span expr)))
           (format " ~a-~a" 
                   (syntax-position expr)
                   (syntax-span expr))]
          [else ""]))
      
      (define (add-prefs-panel)
        (preferences:add-panel
         (string-constant profiling)
         (lambda (s-parent)
           (letrec ([parent (make-object vertical-panel% s-parent)]
                    [msg (make-object message% 
                           (string-constant profiling-color-config) 
                           parent)]
                    [hp (make-object horizontal-pane% parent)]
                    [low (make-object button% (string-constant profiling-low-color) hp 
                           (lambda (x y) (color-callback #t)))]
                    [color-bar (make-object color-bar% hp)]
                    [high (make-object button% (string-constant profiling-high-color) hp
                            (lambda (x y) (color-callback #f)))]
                    
                    [scale (instantiate radio-box% ()
                             (label (string-constant profiling-scale))
                             (parent parent)
                             (callback (lambda (x y) (scale-callback)))
                             (choices
                              (list (string-constant profiling-sqrt)
                                    (string-constant profiling-linear)
                                    (string-constant profiling-square))))]
                    
                    [color-callback
                     (lambda (low?)
                       (let ([color (get-color-from-user 
                                     (if low?
                                         (string-constant profiling-choose-low-color)
                                         (string-constant profiling-choose-high-color))
                                     #f
                                     (preferences:get
                                      (if low?
                                          'drscheme:profile:low-color
                                          'drscheme:profile:high-color)))])
                         (when color
                           (preferences:set 
                            (if low? 'drscheme:profile:low-color 'drscheme:profile:high-color)
                            color))))]
                    [scale-callback
                     (lambda ()
                       (preferences:set 
                        'drscheme:profile:scale
                        (case (send scale get-selection)
                          [(0) 'sqrt]
                          [(1) 'linear]
                          [(2) 'square])))])
             (preferences:add-callback
              'drscheme:profile:scale
              (lambda (p v)
                (send scale set-selection
                      (case v
                        [(sqrt) 0]
                        [(linear) 1]
                        [(square) 2]))))
             (send parent set-alignment 'left 'center)
             (send hp stretchable-height #f)
             parent))))
      
      (define color-bar%
        (class canvas%
          (inherit get-client-size get-dc)
          (field [pen (make-object pen% "black" 1 'solid)]
                 [in-on-paint? #f])
          (define/override (on-paint)
            (set! in-on-paint? #t)
            (let* ([dc (get-dc)]
                   [dummy-pen (send dc get-pen)])
              (let-values ([(w h) (get-client-size)])
                (let loop ([n 0])
                  (when (n . <= . w)
                    (send pen set-color (get-color-value n w))
                    (send dc set-pen pen)
                    (send dc draw-line n 0 n h)
                    (send dc set-pen dummy-pen)
                    (loop (+ n 1))))
                (let-values ([(tw th ta td) (send dc get-text-extent 
                                              (string-constant profiling-example-text))])
                  (send dc draw-text
                        (string-constant profiling-example-text)
                        (floor (- (/ w 2) (/ tw 2)))
                        (floor (- (/ h 2) (/ th 2)))))))
            (set! in-on-paint? #f))
          
          ;; queue callbacks here so that the preferences
          ;; values are actually set by the time on-paint
          ;; is called.
          (preferences:add-callback
           'drscheme:profile:scale
           (lambda (p v)
             (unless in-on-paint?
               (queue-callback
                (lambda ()
                  (on-paint))))))
          (preferences:add-callback
           'drscheme:profile:low-color
           (lambda (p v)
             (unless in-on-paint?
               (queue-callback
                (lambda ()
                  (on-paint))))))
          (preferences:add-callback
           'drscheme:profile:high-color
           (lambda (p v)
             (unless in-on-paint?
               (queue-callback
                (lambda ()
                  (on-paint))))))

          (super-instantiate ())))
      
                                                                      
                                                                      
                                     ;                                
                                     ;                                
  ;;;   ; ;;;  ; ;;;   ;;;   ; ;;;  ;;;;;  ; ;;;  ;;;;    ;;;    ;;;  
 ;   ;   ;      ;     ;   ;   ;      ;      ;         ;  ;   ;  ;   ; 
 ;;;;;   ;      ;     ;   ;   ;      ;      ;      ;;;;  ;      ;;;;; 
 ;       ;      ;     ;   ;   ;      ;      ;     ;   ;  ;      ;     
 ;   ;   ;      ;     ;   ;   ;      ;   ;  ;     ;   ;  ;   ;  ;   ; 
  ;;;   ;;;;   ;;;;    ;;;   ;;;;     ;;;  ;;;;    ;;; ;  ;;;    ;;;  
                                                                      
                                                                      
                                                                      
      (define-values/invoke-unit/sig stacktrace^ stacktrace@ #f stacktrace-imports^))))
