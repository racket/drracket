#|

profile todo:
  - use origin fields
   
|#

(module debug mzscheme
  (require (lib "unitsig.ss")
           (lib "stacktrace.ss" "errortrace")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           "drsig.ss"
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           (lib "bday.ss" "framework" "private")
	   "bindings-browser.ss")

  (define orig (current-output-port))
  
  (provide debug@)
  (define debug@
    (unit/sig drscheme:debug^
      (import [drscheme:rep : drscheme:rep^]
              [drscheme:frame : drscheme:frame^]
              [drscheme:unit : drscheme:unit^]
              [drscheme:language : drscheme:language^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^])

      (define (oprintf . args) (apply fprintf orig args))
      

                                                        
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

      (define (get-cm-key) cm-key)

      ;; error-delta : (instanceof style-delta%)
      (define error-delta (make-object style-delta% 'change-style 'slant))
      (send error-delta set-delta-foreground (make-object color% 255 0 0))
      
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
      
      ;; display-stats : (syntax -> syntax)
      ;; count the number of syntax expressions & number of with-continuation-marks in an 
      ;; expanded expression ... except that it counts keywords, too.
      ;; returns its argument.
      ;(define (display-stats stx)
      ;  (let ([exps 0]
      ;        [wcms 0])
      ;    (let loop ([stx stx])
      ;      (kernel-syntax-case stx ()
      ;        [(#%with-continuation-mark key mark body)
      ;         (set! wcms (+ wcms 1))
      ;         (loop #`body)]
      ;        [(subexps ...)
      ;         (set! exps (+ exps 1))
      ;         (for-each loop (syntax->list stx))]
      ;        [exp
      ;         (set! exps (+ exps 1))]))
      ;    (fprintf (current-error-port) "exps: ~v\nwcms: ~v\n" exps wcms))
      ;  stx)
      
      ;; make-debug-eval-handler : (sexp -> value) -> sexp -> value
      ;; adds debugging information to `sexp' and calls `oe'
      (define (make-debug-eval-handler oe)
        (let ([debug-tool-eval-handler
               (lambda (orig-exp)
		 (if (compiled-expression? (if (syntax? orig-exp)  
					       (syntax-e orig-exp)  
					       orig-exp))
		     (oe orig-exp)
		     (let loop ([exp (if (syntax? orig-exp)
					 orig-exp
					 (namespace-syntax-introduce
					  (datum->syntax-object #f orig-exp)))])
		       (let ([top-e (expand-syntax-to-top-form exp)]) 
			 (syntax-case top-e (begin) 
			   [(begin expr ...)
			    ;; Found a `begin', so expand/eval each contained 
			    ;; expression one at a time 
			    (foldl (lambda (e old-val) (loop e)) 
				   (void)
				   (syntax->list #'(expr ...)))]
			   [_else 
			    ;; Not `begin', so proceed with normal expand and eval 
			    (let* ([annotated
				    (annotate-top (expand-syntax top-e) #f)])
			      (oe annotated))])))))])
	  debug-tool-eval-handler))
      
      ;; make-debug-error-display-handler/text  : (-> (union #f (is-a?/c text%)))
      ;;                                                ((is-a?/c rep:text%) (-> void) -> void)
      ;;                                                ((listof (list text% number number)) -> void)
      ;;                                                (string (union TST exn) -> void)
      ;;                                             -> string (union TST exn)
      ;;                                             -> void
      (define (make-debug-error-display-handler/text get-text 
                                                     queue-output
                                                     highlight-errors
                                                     orig-error-display-handler)
        (define (debug-error-display-handler msg exn)
          (let ([text (get-text)])
            (cond
              [text
	       (let* ([cms (and (exn? exn) 
				(continuation-mark-set? (exn-continuation-marks exn))
				(continuation-mark-set->list 
                                 (exn-continuation-marks exn)
                                 cm-key))]
                      [k #f; no continuing from exceptions for the moment.
			   ;(and (exn:break? exn)
			   ;     (exn:break-continuation exn))
			 ]
		      [src-to-display (find-src-to-display exn 
                                                           (and cms
                                                                (map st-mark-source cms)))])
                 
                 (queue-output
                  text
                  (lambda ()
                    (let ([locked? (send text is-locked?)])
                      (send text begin-edit-sequence)
                      (send text lock #f)
                      (when (and cms
                                 (not (null? cms)))
                        (insert/clickback text
                                          (if (mf-bday?) mf-note bug-note)
                                          (lambda ()
                                            (show-backtrace-window msg cms k))))
                      (when src-to-display
                        (let ([src (car src-to-display)])
                          (when (symbol? src)
                            (insert/clickback 
                             text file-note
                             (lambda ()
                               (open-and-highlight-in-file src-to-display))))))
                      (send text lock locked?)
                      (send text end-edit-sequence))))
                 (orig-error-display-handler msg exn)
                 (queue-output
                  text
                  (lambda ()
                    (when src-to-display
                      (let* ([src (car src-to-display)]
                             [position (cadr src-to-display)]
                             [span (cddr src-to-display)])
                        (when (and (object? src)
                                   (is-a? src text:basic%))
                          (highlight-errors text 
                                            (list (list src position (+ position span)))
                                            (filter 
                                             (lambda (x)
                                               (and (pair? x)
                                                    (is-a? (car x) text:basic<%>)
                                                    (pair? (cdr x))
                                                    (number? (cadr x))
                                                    (number? (cddr x))))
                                             (map st-mark-source cms)))))))))]
              [else 
               (orig-error-display-handler msg exn)])))
        debug-error-display-handler)
      
      ;; make-debug-error-display-handler : (string (union TST exn) -> void) -> string (union TST exn) -> void
      ;; adds in the bug icon, if there are contexts to display
      (define (make-debug-error-display-handler orig-error-display-handler)
        (make-debug-error-display-handler/text
         (lambda ()
           (let ([rep (drscheme:rep:current-rep)])
             (and (is-a? rep drscheme:rep:text<%>)
                  (eq? (send rep get-this-err) (current-error-port))
                  rep)))
         (lambda (rep t) (send rep queue-output t))
         (lambda (rep x y) (send rep highlight-errors x y))
         orig-error-display-handler))

      ;; find-src-to-display : exn (union #f (listof (cons <src> (cons number number))))
      ;;                    -> (union #f (cons (union symbol <src>) (cons number number)))
      ;; finds the source location to display, choosing between
      ;; the stack trace and the exception record.
      ;; returns #f if the source isn't a string.
      (define (find-src-to-display exn cms)
	(cond
	  [(or (exn:read? exn)
               (exn:syntax? exn))
           ;; assume that the original error-display-handler displays the 
           ;; error in this case.
           #f]
	  [else (and (pair? cms)
                     (car cms))]))

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

      ;; with-mark : mark-stx syntax (any? -> syntax) -> syntax
      ;; a member of stacktrace-imports^
      ;; guarantees that the continuation marks associated with cm-key are
      ;; members of the debug-source type, after unwrapped with st-mark-source
      (define (with-mark src-stx mark-maker expr)
        (let ([source (cond
                        [(string? (syntax-source src-stx))
                         (string->symbol (syntax-source src-stx))]
                        [(is-a? (syntax-source src-stx) editor<%>)
                         (syntax-source src-stx)]
                        [else #f])]
              [position (syntax-position src-stx)]
              [span (syntax-span src-stx)])
          (if (and source
                   (number? position)
                   (number? span))
              (let* ([mark-src `(,source ,(- position 1) . ,span)])
                (with-syntax ([expr expr]
                              [mark (mark-maker mark-src)]
                              [cm-key cm-key])
                  (syntax
                   (with-continuation-mark
                    'cm-key
                    mark
                    expr))))
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
          (define/override (on-size x y)
            (preferences:set 'drscheme:backtrace-window-width x)
            (preferences:set 'drscheme:backtrace-window-height y)
            (super-on-size x y))
          (define/override (on-move x y)
            (preferences:set 'drscheme:backtrace-window-x x)
            (preferences:set 'drscheme:backtrace-window-y y)
            (super-on-move x y))
          (define/override (edit-menu:between-find-and-preferences edit-menu) (void))
          (define/override (edit-menu:between-select-all-and-find edit-menu) (void))
          (define/override (file-menu:between-save-as-and-print file-menu) (void))
          (rename [super-on-close on-close])
          (define/override (on-close) 
            (set! current-backtrace-window #f)
            (super-on-close))
          (super-instantiate ())))
            
      ;; show-backtrace-window : string
      ;;                         (listof mark?)
      ;;                         (union continuation? #f)
      ;;                         -> 
      ;;                         void
      (define (show-backtrace-window error-text dis k)
        (reset-backtrace-window)
        (letrec ([text (make-object text:hide-caret/selection%)]
                 [mf-bday-note (when (mf-bday?)
                                 (instantiate message% ()
                                   (label (string-constant happy-birthday-matthias))
                                   (parent (send current-backtrace-window get-area-container))))]
                 [k-button (when k
                             (make-object button%
                               "Continue"
                               (send current-backtrace-window get-area-container)
                               (lambda (a b) (k (void)))))] ; should also foreground DrS window?
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
                           (show-frame ec text (vector-ref di-vec n))
                           (loop (+ n 1))]
                          [else
                           (set! index n)]))
                      
                      ;; add 'more frames' link
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
      
      ;; show-frame : (instanceof editor-canvas%)
      ;;              (instanceof text%) 
      ;;              st-mark?
      ;;              -> 
      ;;              void 
      ;; shows one frame of the continuation
      (define (show-frame editor-canvas text di)
        (let* ([di-source-info (st-mark-source di)]
               [start (cadr di-source-info)]
               [span (cddr di-source-info)]
               [debug-source (car di-source-info)]
               [fn (get-filename debug-source)]
               [start-pos (send text last-position)])
          
          ;; make hyper link to the file
          (send text insert (format "~a: ~a-~a" fn start (+ start span)))
          (let ([end-pos (send text last-position)])
            (send text insert " ")
            (send text change-style (gui-utils:get-clickback-delta) start-pos end-pos)
            (send text set-clickback
                  start-pos end-pos
                  (lambda x
                    (open-and-highlight-in-file di-source-info))))
          
          ;; make bindings hier-list
          (let ([bindings (st-mark-bindings di)])
            (when (not (null? bindings))
              (send text insert (render-bindings/snip bindings))))
          (send text insert #\newline)
          
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
                         (let ([text (new text:basic%)])
                           (if (send text load-file (symbol->string file))
			       (values text 
				       (lambda () (send text on-close)))
			       (values #f (lambda () (void)))))]
                        [(is-a? file editor<%>)
                         (values file void)])])
	  (when from-text
	    (let* ([finish (+ start span)]
		   [context-text (copy/highlight-text from-text start finish)])
	      (send context-text lock #t)
	      (send context-text hide-caret #t)
	      (send text insert "  ")
	      (let ([snip (make-object editor-snip% context-text)])
		(send editor-canvas add-wide-snip snip)
		(send text insert snip))
	      (send text insert #\newline))
	    (close-text))))

      ;; copy/highlight-text : text number number -> text
      ;; copies the range from `start' to `finish', including the entire paragraph at
      ;; each end and highlights the characters corresponding the original range,
      ;; in the resulting text
      (define (copy/highlight-text from-text start finish)
        (let* ([to-text (new text:standard-style-list%)]
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
        (cond
          [(symbol? file) (symbol->string file)]
          [(is-a? file editor<%>)
           (get-filename-from-editor file)]))
      
      ;; get-filename-from-editor : (is-a?/c editor<%>) -> string
      (define (get-filename-from-editor editor)
        (let* ([untitled (string-constant unknown-debug-frame)]
               [canvas (send editor get-canvas)]
               [frame (and canvas (send canvas get-top-level-window))])
          (if (is-a? frame drscheme:unit:frame%)
              (let ([filename (send (send frame get-definitions-text) get-filename)])
                (cond
                  [(and filename (eq? editor (send frame get-interactions-text)))
                   (format (string-constant files-interactions) filename)]
                  [(eq? editor (send frame get-interactions-text))
                   (string-constant current-interactions)]
                  [filename filename]
                  [else (string-constant current-definitions)]))
              (or (send editor get-filename) 
                  untitled))))

      ;; open-and-highlight-in-file : (cons debug-source (cons number number))
      ;;                              -> 
      ;;                              void
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

      
 
;                                                                                      
;                                                                                      
;                                                                                      
;                                                                                      
;                                                                                      
;   ;                  ;                                                               
;  ;;;;   ;;;    ;;;  ;;;;       ;;;    ;;;   ;     ;  ;;;   ; ;  ;;;     ;; ;    ;;;  
;   ;    ;   ;  ;      ;        ;   ;  ;   ;   ;   ;  ;   ;  ;;  ;   ;   ;  ;;   ;   ; 
;   ;   ;    ;  ;;     ;       ;      ;     ;  ;   ; ;    ;  ;       ;  ;    ;  ;    ; 
;   ;   ;;;;;;   ;;    ;       ;      ;     ;   ; ;  ;;;;;;  ;    ;;;;  ;    ;  ;;;;;; 
;   ;   ;          ;   ;       ;      ;     ;   ; ;  ;       ;   ;   ;  ;    ;  ;      
;   ;    ;         ;   ;        ;   ;  ;   ;     ;    ;      ;   ;   ;   ;  ;;   ;     
;    ;;   ;;;;  ;;;     ;;       ;;;    ;;;      ;     ;;;;  ;    ;;;;;   ;; ;    ;;;; 
;                                                                            ;         
;                                                                       ;    ;         
;                                                                        ;;;;          

      
      (define test-coverage-enabled (make-parameter #f))

      (define current-test-coverage-info (make-parameter #f))

      (define (initialize-test-coverage-point key expr)
        (unless (current-test-coverage-info)
          (let ([rep (drscheme:rep:current-rep)])
            (when rep
              (let ([ht (make-hash-table)])
                (current-test-coverage-info ht)
                (send rep set-test-coverage-info ht)))))
        (let ([ht (current-test-coverage-info)])
          (when ht ;; if rep isn't around, we don't do test coverage...
                   ;; this can happen when check syntax expands, for example
            (hash-table-put! ht key (list #f expr)))))
  
      (define (test-covered key)
        (let ([ht (current-test-coverage-info)])
          (when ht ;; as in the `when' test in `initialize-test-coverage-point'
            (let ([v (hash-table-get ht key)])
              (set-car! v #t)))))
      
      (define test-coverage-interactions-text<%>
        (interface ()
          set-test-coverage-info
          get-test-coverage-info))
      
      (define test-coverage-frame<%>
        (interface ()
          show-test-coverage-annotations ;; hash-table (union #f style) (union #f style) boolean -> void
          get-test-coverage-info-visible?
          ask-about-clearing-test-coverage?))
      
      (define test-coverage-interactions-text-mixin
        (mixin (drscheme:rep:text<%> text:basic<%>) (test-coverage-interactions-text<%>)
          (field [test-coverage-info #f]
                 [test-coverage-on-style #f]
                 [test-coverage-off-style #f]
                 [ask-about-reset? #f])
          (define/public set-test-coverage-info
            (opt-lambda (ht [on-style #f] [off-style #f] [ask? #t])
              (set! test-coverage-info ht)
              (set! test-coverage-on-style on-style)
              (set! test-coverage-off-style off-style)
              (set! ask-about-reset? ask?)))
          (define/public (get-test-coverage-info) test-coverage-info)
          
          (rename [super-after-many-evals after-many-evals])
          (inherit get-top-level-window)
          (define/override (after-many-evals)
            (let ([tlw (get-top-level-window)])
              (when (and (is-a? tlw test-coverage-frame<%>)
                         test-coverage-info)
                (send tlw show-test-coverage-annotations 
                      test-coverage-info
                      test-coverage-on-style
                      test-coverage-off-style
                      ask-about-reset?)))
            (super-after-many-evals))
          
          (super-instantiate ())))
      
      (define test-coverage-definitions-text-mixin
        (mixin ((class->interface text%) drscheme:unit:definitions-text<%>) ()
          (inherit get-canvas)
          
          (define/private (clear-test-coverage?)
            (if (preferences:get 'drscheme:test-coverage-ask-about-clearing?)
                (let ([msg-box-result
                       (message-box/custom
                        (string-constant drscheme)
                        (string-constant test-coverage-clear?)
                        (string-constant yes)
                        (string-constant no)
                        (string-constant test-coverage-clear-and-do-not-ask-again)
                        (send (get-canvas) get-top-level-window)
                        '(default=1)
                        2)])
                  (case msg-box-result
                    [(1) #t]
                    [(2) #f]
                    [(3)
                     (preferences:set 'drscheme:test-coverage-ask-about-clearing? #f)
                     #t]))
                #t))
          
          (define/private (clear-test-coverage)
            (let ([canvas (get-canvas)])
              (when canvas
                (let ([frame (send canvas get-top-level-window)])
                  (when (send frame get-test-coverage-info-visible?)
                    (send frame clear-test-coverage-display)
                    (let ([it (send frame get-interactions-text)])
                      (when (is-a? it test-coverage-interactions-text<%>)
                        (send it set-test-coverage-info #f))))))))
          
          (define/private (can-clear-coverage?)
            (let ([canvas (get-canvas)])
              (or (not canvas)
                  (let ([frame (send canvas get-top-level-window)])
                    (or (not (send frame get-test-coverage-info-visible?))
                        (not (send frame ask-about-clearing-test-coverage?))
                        (clear-test-coverage?))))))
            
          (rename [super-can-insert? can-insert?])
          (define/override (can-insert? x y)
            (and (super-can-insert? x y)
                 (can-clear-coverage?)))
          
          (rename [super-can-delete? can-delete?])
          (define/override (can-delete? x y)
            (and (super-can-delete? x y)
                 (can-clear-coverage?)))
          
          (rename [super-after-insert after-insert])
          (define/override (after-insert x y)
            (super-after-insert x y)
            (clear-test-coverage))
          
          (rename [super-after-delete after-delete])
          (define/override (after-delete x y)
            (super-after-delete x y)
            (clear-test-coverage))
          
          (super-instantiate ())))
      
      ;(define test-covered-color (send the-color-database find-color "lime green"))
      ;(define test-not-covered-color (send the-color-database find-color "pink"))

      (define test-covered-style-delta (make-object style-delta%))
      (send test-covered-style-delta set-delta-foreground "forest green")
      
      (define test-not-covered-style-delta (make-object style-delta%))
      (send test-not-covered-style-delta set-delta-foreground "firebrick")
      
      (define erase-test-coverage-style-delta (make-object style-delta% 'change-normal-color))
      
      ;; test-coverage-unit-frame-mixin
      (define test-coverage-unit-frame-mixin
        (mixin (drscheme:unit:frame<%>) (test-coverage-frame<%>)
          
          (field [internal-clear-test-coverage-display #f])
          
          (define/public (clear-test-coverage-display)
            (when internal-clear-test-coverage-display
              (internal-clear-test-coverage-display)
              (set! internal-clear-test-coverage-display #f)))
      
          (field [ask-about-reset? #t])
          (define/public (ask-about-clearing-test-coverage?) ask-about-reset?)
          
          (define/public (get-test-coverage-info-visible?)
            (not (not internal-clear-test-coverage-display)))
          
          (define/public (show-test-coverage-annotations ht on-style off-style ask?)
            (set! ask-about-reset? ask?)
            (let* ([edit-sequence-ht (make-hash-table)]
                   [locked-ht (make-hash-table)]
                   [actions-ht (make-hash-table 'equal)]
                   [on/syntaxes (hash-table-map ht (lambda (_ pr) pr))]

                   ;; can-annotate : (listof (list boolean syntax))
                   ;; boolean is #t => code was run
                   ;;            #f => code was not run
                   ;; remove those that cannot be annotated
                   [can-annotate
                    (filter (lambda (pr)
                              (let ([stx (cadr pr)])
                                (and (syntax? stx)
                                     (let ([src (syntax-source stx)]
                                           [pos (syntax-position stx)]
                                           [span (syntax-span stx)])
                                       (and (is-a? src text:basic<%>)
                                            pos
                                            span)))))
                            on/syntaxes)]
                   
                   ;; filtered : (listof (list boolean syntax))
                   ;; remove redundant expressions
                   [filtered
                    (let (;; actions-ht : (list src number number) -> (list boolean syntax)
                          [actions-ht (make-hash-table 'equal)])
                      (for-each
                       (lambda (pr)
                         (let* ([stx (cadr pr)]
                                [on? (car pr)]
                                [key (list (syntax-source stx)
                                           (syntax-position stx)
                                           (syntax-span stx))]
                                [old (hash-table-get actions-ht key (lambda () 'nothing))])
                           (cond
                             [(eq? old 'nothing) (hash-table-put! actions-ht key (list on? stx))]
                             [(car old) ;; recorded as executed
                              (void)]
                             [(not (car old)) ;; recorded as unexected
                              (when on?
                                (hash-table-put! actions-ht key (list #t stx)))])))
                       can-annotate)
                      (hash-table-map actions-ht (lambda (k v) v)))])
              
              ;; if everything is covered *and* no coloring has been done, do no coloring.
              (unless (and (andmap car filtered)
                           (not (get-test-coverage-info-visible?)))
                (let (;; sorted : (listof (list boolean syntax))
                      ;; sorting predicate:
                      ;;  x < y if
                      ;;    x's span is bigger than y's (ie, do larger expressions first)
                      ;;    unless x and y are the same source location.
                      ;;    in that case, color red first and then green
                      [sorted
                       (quicksort
                        filtered
                        (lambda (x y)
                          (let* ([x-stx (cadr x)]
                                 [y-stx (cadr y)]
                                 [x-pos (syntax-position x-stx)]
                                 [y-pos (syntax-position y-stx)]
                                 [x-span (syntax-span x-stx)]
                                 [y-span (syntax-span y-stx)]
                                 [x-on (car x)]
                                 [y-on (car y)])
                            (cond
                              [(and (= x-pos y-pos)
                                    (= x-span x-span))
                               (or y-on
                                   (not x-on))]
                              [else (>= x-span y-span)]))))])
                  
                  ;; turn on edit-sequences in all editors to be touched by new annotations
                  ;; also fill in the edit-sequence-ht
                  (for-each
                   (lambda (pr)
                     (let ([src (syntax-source (cadr pr))])
                       (hash-table-get 
                        edit-sequence-ht
                        src
                        (lambda ()
                          (hash-table-put! edit-sequence-ht src #f)
                          (send src begin-edit-sequence #f)
                          (when (send src is-locked?)
                            (hash-table-put! locked-ht src #t)
                            (send src lock #f))))))
                   sorted)
                  
                  ;; clear out old annotations (and thaw colorers)
                  (when internal-clear-test-coverage-display
                    (internal-clear-test-coverage-display)
                    (set! internal-clear-test-coverage-display #f))
                  
                  ;; freeze the colorers (possibly re-freeze them)
                  (hash-table-for-each
                   edit-sequence-ht
                   (lambda (src _)
                     (send src freeze-colorer)))
                  
                  ;; set new annotations
                  (for-each
                   (lambda (pr)
                     (let ([stx (cadr pr)]
                           [on? (car pr)])
                       (when (syntax? stx)
                         (let* ([src (syntax-source stx)]
                                [pos (syntax-position stx)]
                                [span (syntax-span stx)])
                           (send src change-style
                                 (if on?
                                     (or on-style test-covered-style-delta)
                                     (or off-style test-not-covered-style-delta))
                                 (- pos 1)
                                 (+ (- pos 1) span)
                                 #f)))))
                   sorted)
                  
                  ;; relock editors
                  (hash-table-for-each 
                   locked-ht
                   (lambda (txt _) (send txt lock #t)))
                  
                  ;; end edit sequences
                  (hash-table-for-each 
                   edit-sequence-ht
                   (lambda (txt _) (send txt end-edit-sequence)))
                  
                  ;; save thunk to reset these new annotations
                  (set! internal-clear-test-coverage-display
                        (lambda ()
                          (hash-table-for-each
                           edit-sequence-ht
                           (lambda (txt _) 
                             (send txt begin-edit-sequence #f)))
                          (hash-table-for-each
                           edit-sequence-ht
                           (lambda (txt _) 
                             (let ([locked? (send txt is-locked?)])
                               (when locked? (send txt lock #f))
                               (send txt change-style 
                                     erase-test-coverage-style-delta
                                     0
                                     (send txt last-position)
                                     #f)
                               (when locked? (send txt lock #t)))))
                          (hash-table-for-each
                           edit-sequence-ht
                           (lambda (txt _) 
                             (let ([locked? (send txt is-locked?)])
                               (when locked? (send txt lock #f))
                               (send txt thaw-colorer)
                               (when locked? (send txt lock #t)))
                             (send txt end-edit-sequence)))))))))

          (rename [super-clear-annotations clear-annotations])
          (define/override (clear-annotations)
            (super-clear-annotations)
            (clear-test-coverage-display))
          
          (super-instantiate ())))
      
 
      
      
      
                                                               
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

      ;; prof-info =
      ;; (make-prof-info
      ;;    boolean ;; protect against nested calls
      ;;    number[number of calls]
      ;;    number[time spent in all calls]
      ;;   (union #f symbol) 
      ;;   expression)
      (define-struct prof-info (nest num time name expr))

      ;; copy-prof-info : prof-info -> prof-info
      (define (copy-prof-info prof-info)
        (make-prof-info (prof-info-nest prof-info)
                        (prof-info-num prof-info)
                        (prof-info-time prof-info)
                        (prof-info-name prof-info)
                        (prof-info-expr prof-info)))

      ;; any-info? : prof-info -> boolean
      (define (any-info? prof-info)
        (or (not (zero? (prof-info-num prof-info)))
            (not (zero? (prof-info-time prof-info)))))
     
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; profiling runtime support

      ;; parameter
      ;; imported into errortrace
      (define profiling-enabled (make-parameter #f))

      ;; parameter
      ;; holds a hash-table for the profiling information
      (define current-profile-info (make-parameter #f))

      
      
      ;; initialize-profile-point : sym syntax syntax -> void
      ;; called during compilation to register this point as
      ;; a profile point. 
      ;; =user=
      ;; imported into errortrace
      (define (initialize-profile-point key name expr)
	(unless (current-profile-info)
          (let ([rep (drscheme:rep:current-rep)])
            (when rep
              (let ([ht (make-hash-table)])
                (current-profile-info ht)
                (send rep set-profile-info ht)))))
        (let ([profile-info (current-profile-info)])
          (hash-table-put! profile-info
			   key 
			   (make-prof-info #f 0 0 (and name (syntax-e name)) expr))))
  
      ;; register-profile-start : sym -> (union #f number)
      ;; =user=
      ;; imported into errortrace
      (define (register-profile-start key)
	(let ([info (hash-table-get (current-profile-info) key)])
	  (set-prof-info-num! info (+ (prof-info-num info) 1))
	  (if (prof-info-nest info)
	      #f
	      (begin
		(set-prof-info-nest! info #t)
		(current-process-milliseconds)))))
      
      ;; register-profile-done : sym (union #f number) -> void
      ;; =user=
      ;; imported into errortrace
      (define (register-profile-done key start)
        (when start
	  (let ([info (hash-table-get (current-profile-info) key)])
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
      
      ;; extract-maximum : (listof prof-info) -> number
      ;; gets the maximum value of the currently preferred profiling info.
      (define (extract-maximum infos)
        (let ([max-value 0]
              [sel (if (eq? (preferences:get 'drscheme:profile-how-to-count) 'time)
                       prof-info-time
                       prof-info-num)])
          (for-each
           (lambda (val)
             (set! max-value (max max-value (sel val))))
           infos)
          max-value))
      
      ;; extract-total-time : (listof prof-info) -> number
      (define (extract-total-time infos)
        (let ([sum 0])
          (for-each
           (lambda (val)
             (set! sum (+ sum (prof-info-time val))))
           infos)
          sum))
      
      ;; profile-definitions-mixin : mixin
      (define profile-definitions-text-mixin
        (mixin ((class->interface text%) drscheme:unit:definitions-text<%>) ()
          (inherit get-canvas)
          
          (define (clear-profiling?)
            (eq? (message-box (string-constant drscheme)
                              (string-constant profiling-clear?)
                              (send (get-canvas) get-top-level-window)
                              '(yes-no))
                 'yes))

          (rename [super-can-insert? can-insert?])
          (define/override (can-insert? x y)
            (and (super-can-insert? x y)
                 (let ([canvas (get-canvas)])
                   (or (not canvas)
                       (let ([frame (send canvas get-top-level-window)])
                         (or (not (send frame get-profile-info-visible?))
                             (clear-profiling?)))))))
          
          (rename [super-can-delete? can-delete?])
          (define/override (can-delete? x y)
            (and (super-can-delete? x y)
                 (let ([canvas (get-canvas)])
                   (or (not canvas)
                       (let ([frame (send canvas get-top-level-window)])
                         (or (not (send frame get-profile-info-visible?))
                             (clear-profiling?)))))))
          
          (rename [super-on-insert on-insert])
          (define/override (on-insert x y)
            (super-on-insert x y)
            (let ([canvas (get-canvas)])
              (when canvas
                (let ([frame (send canvas get-top-level-window)])
                  (when (send frame get-profile-info-visible?)
                    (send frame clear-profile-display)
                    (send frame clear-profile-info))))))
          
          (rename [super-on-delete on-delete])
          (define/override (on-delete x y)
            (super-on-delete x y)
            (let ([canvas (get-canvas)])
              (when canvas
                (let ([frame (send canvas get-top-level-window)])
                  (when (send frame get-profile-info-visible?)
                    (send frame clear-profile-display)
                    (send frame clear-profile-info))))))

          (super-instantiate ())))

      (define profile-interactions-text<%>
        (interface ()
          get-profile-info
          set-profile-info))
                
      (define profile-interactions-text-mixin
        (mixin (drscheme:rep:text<%>) (profile-interactions-text<%>)
          ;; profile-info : symbol -o> prof-info
          (field [profile-info (make-hash-table)])
          (define/public (set-profile-info ht) (set! profile-info ht))
          (define/public (get-profile-info) profile-info)
          (super-instantiate ())))
          
      ;; profile-unit-frame-mixin : mixin
      ;; adds profiling to the unit frame
      (define profile-unit-frame-mixin
        (mixin (drscheme:unit:frame<%> drscheme:frame:<%>) ()

          (inherit get-interactions-text)
          
          ;; clear-profile-info : -> void
          ;; clears the profiling data, but doesn't change the GUI
          (define/public (clear-profile-info)
            (let ([ht (send (get-interactions-text) get-profile-info)])
              (hash-table-for-each
               ht
               (lambda (k info)
                 (set-prof-info-num! info 0)
                 (set-prof-info-time! info 0)))))

          ;; clear-profile-display : -> void
          ;; clears the profiling information from the GUI.
          (define/public (clear-profile-display)
            (when profile-info-visible?
              (set! profile-info-visible? #f)
	      (when profile-gui-constructed?
		(send profile-info-outer-panel change-children
		      (lambda (l)
			(remq profile-info-panel l)))
		(send profile-info-text clear-profile-display)
		(update-shown))))

          ;; can-show-profile? : -> boolean
          ;; indicates if there is any profiling information to be shown.
          (define/public (can-show-profile?)
            (let ([ht (send (get-interactions-text) get-profile-info)])
              (let/ec esc-k
                (hash-table-for-each
                 ht
                 (lambda (key v)
                   (when (any-info? v)
                     (esc-k #t))))
                #f)))

          ;; execute-callback : -> void
          (rename [super-execute-callback execute-callback])
          (define/override (execute-callback)
            (send (get-interactions-text) set-profile-info (make-hash-table))
            (super-execute-callback))

          ;; clear-annotations
          (rename [super-clear-annotations clear-annotations])
          (define/override (clear-annotations)
            (super-clear-annotations)
            (clear-profile-display))
          
          ;; update-shown : -> void
          ;; updates the state of the profile item's show menu
          (rename [super-update-shown update-shown])
          (define/override (update-shown)
            (super-update-shown)
            (send show-profile-menu-item set-label
                  (if profile-info-visible?
                      (string-constant profiling-hide-profile)
                      (string-constant profiling-show-profile))))
          
          ;; add-show-menu-items : menu -> void
          ;; adds the show profile menu item
          (rename [super-add-show-menu-items add-show-menu-items])
          (define/override (add-show-menu-items show-menu)
            (super-add-show-menu-items show-menu)
            (set! show-profile-menu-item 
                  (instantiate menu:can-restore-menu-item% ()
                    (label (string-constant profiling-hide-profile))
                    (parent show-menu)
                    (callback
                     (lambda (x y)
                       (toggle-profile-visible))))))
          
          ;; toggle-profile-visible : -> void
          (define (toggle-profile-visible)
            (cond
              [profile-info-visible?
               (clear-annotations)]
              [(not profile-info-visible?)
               (cond
                 [(can-show-profile?)
		  (construct-profile-gui)
                  (clear-annotations)
                  (send profile-info-text refresh-profile)
                  (set! profile-info-visible? #t)
                  (send profile-info-outer-panel change-children
                        (lambda (l)
                          (append l (list profile-info-panel))))
                  (update-shown)]
                 [else
                  (message-box (string-constant drscheme)
                               (string-constant profiling-no-information-available))])]))

          (field (profile-info-visible? #f))
          (field (show-profile-menu-item #f))
	  (field (profile-gui-constructed? #f))

          ;; get-profile-info-visible? : -> boolean
          ;; returns #t when the profiling information is visible in the frame.
          (define/public (get-profile-info-visible?) profile-info-visible?)
          
          (rename [super-make-root-area-container make-root-area-container])
          (field [profile-info-outer-panel #f])
          (define/override (make-root-area-container % parent)
            (set! profile-info-outer-panel
                  (super-make-root-area-container
                   vertical-panel%
                   parent))
            (make-object % profile-info-outer-panel))
          
          (super-instantiate ())
          
	  (define profile-info-panel #f)
	  (define profile-info-text #f)

	  (inherit begin-container-sequence end-container-sequence)
	  (define/private (construct-profile-gui)
	    (unless profile-gui-constructed?
	      (set! profile-gui-constructed? #t)
	      (begin-container-sequence)
	      (let ()
		(define _2
		  (set! profile-info-panel (instantiate horizontal-panel% ()
					     (parent profile-info-outer-panel)
					     (stretchable-height #f))))
		(define _3
		  (set! profile-info-text (instantiate profile-text% (this))))
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
		(define _1
		  (send profile-choice set-selection
			(case (preferences:get 'drscheme:profile-how-to-count)
			  [(time) 0]
			  [(count) 1])))
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
		       (clear-profile-display)
		       (clear-profile-info)))))
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
		(send profile-left-side set-alignment 'left 'center)
		
		;; hide profiling info initially, but reflow the container
		;; so that the invisible children get the right size.
		(send this reflow-container)
		(send profile-info-outer-panel change-children
		      (lambda (l)
			(remq profile-info-panel l))))
	      (end-container-sequence)))))
      
      ;; profile-text% : extends text:basic%
      ;; this class keeps track of a single thread's
      ;; profiling information. these methods are not
      ;; to be called directly, but only by the frame class, since
      ;; they do not completely implement the abstraction for the
      ;; GUI. They only manage the profiling information reported
      ;; in the bottom window
      (define profile-text% 
        (class text:basic%
          (init-field definitions-frame)

          ;; refresh-profile : -> void
          ;; shows the profiling information with text highlighting 
          ;; in this editor
          (define/public (refresh-profile)
            (refresh-profile/work))

          ;; clear-profile-display : -> void
          ;; clears out the GUI showing the profile results
          (define/public (clear-profile-display)
            (begin-edit-sequence)
            (let ([locked? (is-locked?)])
              (lock #f)
              (clear-old-results)
              (erase)
              (lock locked?)
              (end-edit-sequence)))
          
          (define/private (get-profile-info)
            (let ([t (send definitions-frame get-interactions-text)])
              (send t get-profile-info)))

          (inherit lock is-locked?
                   get-canvas hide-caret get-snip-location
                   begin-edit-sequence end-edit-sequence 
                   erase insert)

          ;; clear-old-results : -> void
          ;; removes the profile highlighting
          (field [clear-old-results void])

          ;; refresh-profile-results : -> void
          ;; does the work to erase any existing profile info
          ;; and make new profiling info.
          (define/private (refresh-profile/work)
            (begin-edit-sequence)
            (lock #f)
            (erase)
            (clear-old-results)
            (let* (;; must copy them here in case the program is still running
                   ;; and thus updating them.
                   [infos 
                    (filter
                     any-info?
                     (map copy-prof-info (hash-table-map (get-profile-info) (lambda (key val) val))))]
                   ;; each editor that gets some highlighting is put
                   ;; into this table and an edit sequence is begun for it.
                   ;; after all ranges are updated, the edit sequences are all closed.
                   [in-edit-sequence (make-hash-table)]
                   [thnk void]
                   [max-value (extract-maximum infos)]
                   [total-time (extract-total-time infos)]
                   [show-highlight
                    (lambda (info)
                      (let* ([expr (prof-info-expr info)]
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
                          (let* ([color (get-color-value 
                                         (if (eq? (preferences:get 'drscheme:profile-how-to-count) 'time)
                                             (prof-info-time info)
                                             (prof-info-num info))
                                         max-value)]
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
                    (lambda (info newline? highlight-line?)
                      (let* ([expr (prof-info-expr info)]
                             [expr-src (syntax-source expr)]
                             [count (prof-info-num info)]
                             [time (prof-info-time info)]
                             [name (prof-info-name info)])
                        (when newline? (send src-loc-editor insert "\n"))
                        (when highlight-line? (small-blank-line src-loc-editor))
                        (let ([before (send src-loc-editor last-position)])
                          (insert-profile-src-loc src-loc-editor expr name)
                          (let ([after (send src-loc-editor last-position)])
                            (cond
                              [(string? expr-src)
                               (send src-loc-editor change-style (gui-utils:get-clickback-delta) before after)
                               (let ([after (send src-loc-editor last-position)])
                                 (send src-loc-editor set-clickback 
                                       before after 
                                       (lambda (text start end)
                                         (open-file-and-goto-position expr-src (syntax-position expr)))))]
                              [(is-a? expr-src editor:basic<%>)
                               (send src-loc-editor change-style (gui-utils:get-clickback-delta) before after)
                               (send src-loc-editor set-clickback
                                     before after
                                     (lambda (text start end)
                                       (let ([window (send expr-src get-top-level-window)]
                                             [pos (syntax-position expr)])
                                         (when window (send window show #t))
                                         (when pos (send expr-src set-position (- pos 1)))
                                         (send expr-src set-caret-owner #f 'global))))]
                              [else (void)])))
                        
                        (when newline? (send percentage-editor insert "\n"))
                        (when highlight-line? (small-blank-line percentage-editor))
                        (send percentage-editor insert (format-percentage 
                                                        (if (= total-time 0)
                                                            1
                                                            (/ time total-time))))
                        (send percentage-editor set-paragraph-alignment
                              (send percentage-editor last-paragraph)
                              'right)
                        
                        (when newline? (send time-editor insert "\n"))
                        (when highlight-line? (small-blank-line time-editor))
                        (send time-editor insert (format "~a" time))
                        (send time-editor set-paragraph-alignment (send time-editor last-paragraph) 'right)
                        
                        (when newline? (send count-editor insert "\n")) 
                        (when highlight-line? (small-blank-line count-editor))
                        (send count-editor insert (format "~a" count))
                        (send count-editor set-paragraph-alignment (send count-editor last-paragraph) 'right)))]
                   
                   [bigger-value?
                    (lambda (x y)
                      (let ([sel (if (eq? 'count (preferences:get 'drscheme:profile-how-to-count))
                                     prof-info-num
                                     prof-info-time)])
                        (> (sel x) (sel y))))]
                   
                   [cleanup-editor
                    (lambda (ed)
                      (let* ([ed-admin (send ed get-admin)]
                             [snip (send ed-admin get-snip)]
                             [bl (box 0)]
                             [br (box 0)])
                        (get-snip-location snip bl #f #f)
                        (get-snip-location snip br #f #t)
                        (let ([w (+ (- (unbox br) (unbox bl)) 4)])
                          (send ed set-max-width w)
                          (send ed set-min-width w)))
                      (send ed hide-caret #t)
                      (send ed lock #t))]
                   
                   [top-infos (top 100 (quicksort infos bigger-value?))])
              (for-each show-highlight top-infos)
              (initialize-editors)
              (let loop ([infos top-infos]
                         [newline? #f]
                         [highlight-counter 0])
                (cond
                  [(null? infos) (void)]
                  [else 
                   (show-line (car infos) newline? (and newline? (zero? highlight-counter)))
                   (loop (cdr infos) #t (modulo (+ highlight-counter 1) 2))]))
              (cleanup-editor count-editor)
              (cleanup-editor time-editor)
              (cleanup-editor percentage-editor)
              (cleanup-editor src-loc-editor)
              
              (hash-table-for-each
               in-edit-sequence
               (lambda (key val)
                 (send key end-edit-sequence)))
              (set! clear-old-results 
                    (lambda ()
                      (hash-table-for-each
                       in-edit-sequence
                       (lambda (key val) (send key begin-edit-sequence)))
                      (thnk)
                      (hash-table-for-each
                       in-edit-sequence
                       (lambda (key val) (send key end-edit-sequence)))
                      (set! clear-old-results void))))
            (lock #t)
            (end-edit-sequence)
            (let ([canvas (get-canvas)])
              (when canvas
                (send canvas scroll-to 0 0 1 1 #t 'start))))
          
          ;; top : number (listof X) -> (listof X)
          ;; extracts the first `n' elements from a list.
          (define (top n lst)
            (let loop ([n n]
                       [lst lst])
              (cond
                [(null? lst) null]
                [(= 0 n) null]
                [else (cons (car lst) (loop (- n 1) (cdr lst)))])))

          (field (src-loc-editor #f)
                 (percentage-editor #f)
                 (time-editor #f)
                 (count-editor #f))
          (define (clear-editors)
            (set! src-loc-editor #f)
            (set! percentage-editor #f)
            (set! time-editor #f)
            (set! count-editor #f))
          (define (initialize-editors)
            (set! src-loc-editor (instantiate text% ()))
            (set! percentage-editor (instantiate text% ()))
            (set! time-editor (instantiate text% ()))
            (set! count-editor (instantiate text% ()))
            (send src-loc-editor set-styles-sticky #f)            
            (send percentage-editor set-styles-sticky #f)
            (send time-editor set-styles-sticky #f)
            (send count-editor set-styles-sticky #f)
            (insert (instantiate editor-snip% (percentage-editor)))
            (insert (instantiate editor-snip% (time-editor)))
            (insert (instantiate editor-snip% (count-editor)))
            (insert (instantiate editor-snip% (src-loc-editor)))
            (insert-title (string-constant profiling-col-function) src-loc-editor)
            (insert-title (string-constant profiling-col-time-in-msec) time-editor)
            (insert-title (string-constant profiling-col-percent-time) percentage-editor)
            (insert-title (string-constant profiling-col-calls) count-editor))
          
          (define (insert-title str txt)
            (send txt insert str)
            (send txt insert "\n")
            (send txt change-style bold-delta 0 (- (send txt last-position) 1))
            (send txt set-paragraph-alignment 0 'center))
          
          (super-instantiate ())
          (hide-caret #t)))
      
      ;; format-percentage : number[0 <= n <= 1] -> string
      ;; formats the number as a percentage string with trailing zeros,
      ;; to 3 decimal places.
      (define (format-percentage n)
        (let* ([number-of-places 3]
               [whole-part (floor (* n 100))]
               [decimal-part (- (* n 100) whole-part)]
               [truncated/moved-decimal-part (floor (* (expt 10 number-of-places) decimal-part))]
               [pad
                (lambda (str)
                  (if ((string-length str) . < . number-of-places)
                      (string-append (make-string (- number-of-places (string-length str)) #\0) 
                                     str)
                      str))])
          (string-append (format "~a" whole-part)
                         "."
                         (pad (format "~a" truncated/moved-decimal-part)))))

      (define (small-blank-line txt)
        (let ([before (send txt last-position)])
          (send txt insert "\n")
          (let ([after (send txt last-position)])
            (send txt change-style small-font-style before after))))
      
      (define small-font-style (make-object style-delta% 'change-size 6))
      
      ;; bold-delta : style-delta
      (define bold-delta (make-object style-delta% 'change-bold))
      
      ;; insert-profile-src-loc : syntax name -> string
      (define (insert-profile-src-loc editor stx name)
        (cond
          [name
           (let ([before (send editor last-position)])
             (send editor insert (format "~a" name)))]
          [else
           (let* ([src (syntax-source stx)]
                  [filename 
                   (cond
                     [(string? src) src]
                     [(is-a? src editor<%>) (get-filename-from-editor src)]
                     [else (string-constant profiling-unknown-src)])]
                  [col (syntax-column stx)]
                  [line (syntax-line stx)]
                  [pos (syntax-position stx)]
                  [span (syntax-span stx)]
                  [src
                   (cond
                     [(and col line)
                      (format "~a: ~a.~a" filename line col)]
                     [pos
                      (format "~a: ~a" filename pos)]
                     [else 
                      filename])])
             (send editor insert src))]))
      
      ;; open-file-and-goto-position : string (union #f number) -> void
      (define (open-file-and-goto-position filename pos)
        (let ([frame (handler:edit-file filename)])
          (when (and frame
                     pos
                     (is-a? frame drscheme:unit:frame%))
            (let ([defs (send frame get-definitions-text)])
              (send defs set-position (- pos 1))))))
      
      ;; get-src-filename : tst -> (union #f string)
      (define (get-src-filename src)
        (cond
          [(string? src) src]
          [(is-a? src text%)
           (send src get-filename)]
          [else #f]))
      
      ;; get-src-loc : syntax -> string
      (define (get-src-loc expr)
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
