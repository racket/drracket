;;; util.ss

;;; utility functions for DrScheme GUI testing 

;;; Authors: Robby Findler, Paul Steckler

(unit/sig drscheme:test-util^
  
  (import mred^
	  [fw : framework^]
	  test-utils:gui^)
  
  ;; save-drscheme-window-as : string -> void
  ;; use the "save as" dialog in drscheme to save the definitions
  ;; window to a file.
  (define (save-drscheme-window-as filename)
    (use-get/put-dialog
     (lambda ()
       (fw:test:menu-select "File" "Save Definitions As..."))
     filename))

  ;; use-get/put-dialog : (-> void) string -> void
  ;; open-dialog is a thunk that should open the dialog
  ;; filename is a string naming a file that should be typed into the dialog
  (define (use-get/put-dialog open-dialog filename)
    (unless (procedure? open-dialog)
      (error 'use-open/close-dialog "expected procedure as first argument, got: ~e, other arg: ~e"
	     open-dialog filename))
    (unless (string? filename)
      (error 'use-open/close-dialog "expected string as second argument, got: ~e, other arg: ~e"
	     filename open-dialog))
    (let ([drs (wait-for-drscheme-frame)]
	  [old-pref (fw:preferences:get 'framework:file-dialogs)])
      (with-handlers ([(lambda (x) #t)
		       (lambda (x)
			 (fw:preferences:set 'framework:file-dialogs old-pref)
			 (raise x))])
	(fw:preferences:set 'framework:file-dialogs 'common)
	(open-dialog)
	(let ([dlg (wait-for-new-frame drs)])
	  (send (find-labelled-window "Full pathname") focus)
	  (fw:test:keystroke #\a (list (case (system-type)
					 [(windows) 'control]
					 [(macos) 'command]
					 [(unix) 'meta])))
	  (for-each fw:test:keystroke (string->list filename))
	  (fw:test:button-push "OK")
	  (wait-for-new-frame dlg))
	(fw:preferences:set 'framework-file-dialogs old-pref))))

  ;; -> eventspace
  ;; returns the eventspace used by the program in the current drscheme window
  (define (get-user-eventspace)
    (ivar (wait-for-drscheme-frame) user-eventspace))
  
  (define (test-util-error fmt . args)
    (raise (make-exn (apply fmt args) (current-continuation-marks))))
  
  (define poll-until
    (case-lambda
     [(pred) (poll-until pred 10)]
     [(pred secs)
      (let ([step 1/20])
	(let loop ([counter secs])
	  (if (<= counter 0)
	      (error 'poll-until "timeout after ~e secs, ~e never returned a true value" secs pred)
	      (let ([result (pred)])
		(or result
		    (begin
		      (sleep step)
		      (loop (- counter step))))))))]))
  
  (define (drscheme-frame? frame)
    (ivar-in-interface? 'execute-button (object-interface frame)))
  
  (define (wait-for-drscheme-frame)
    (let ([wait-for-drscheme-frame-pred
	   (lambda ()
	     (yield)
	     (let ([active (get-top-level-focus-window)])
	       (if (and active
			(drscheme-frame? active))
		   active
		   #f)))])
      (or (wait-for-drscheme-frame-pred)
	  (begin
	    (printf "Select DrScheme frame~n")
	    (poll-until wait-for-drscheme-frame-pred)))))
  
  (define (wait-for-new-frame old-frame)
    (let ([wait-for-new-frame-pred
	   (lambda ()
	     (let ([active (get-top-level-focus-window)])
	       (if (and active
			(not (eq? active old-frame)))
		   active
		   #f)))])
      (poll-until wait-for-new-frame-pred)))

  (define (wait-for-computation frame)
    (verify-drscheme-frame-frontmost 'wait-for-computation frame)
    (let* ([button (ivar frame execute-button)]
	   [wait-for-computation-pred
	    (lambda ()
	      (fw:test:reraise-error)
	      (send button is-enabled?))])
      (poll-until
       wait-for-computation-pred
       60)))

  (define do-execute 
    (case-lambda
     [(frame)
      (do-execute frame #t)]
     [(frame wait-for-finish?)
      (verify-drscheme-frame-frontmost 'do-execute frame)
      (let ([button (ivar frame execute-button)])
	(fw:test:button-push button)
	(when wait-for-finish?
	  (wait-for-computation frame)))]))
  
  (define (verify-drscheme-frame-frontmost function-name frame)
    (unless (and (eq? frame (get-top-level-focus-window))
		 (drscheme-frame? frame))
      (error function-name "drscheme frame not frontmost: ~e" frame)))
  
  (define (clear-definitions frame)
    (verify-drscheme-frame-frontmost 'clear-definitions frame)
    (fw:test:new-window (ivar frame definitions-canvas))
    (let ([window (send frame get-focus-window)])
      (let-values ([(cw ch) (send window get-client-size)]
		   [(w h) (send window get-size)])
	(fw:test:mouse-click 'left
			     (+ cw (floor (/ (- w cw) 2)))
			     (+ ch (floor (/ (- h ch) 2))))))
    (fw:test:menu-select "Edit" "Select All")
    (fw:test:menu-select "Edit" (if (eq? (system-type) 'macos)
				    "Clear"
				    "Delete")))
    
  
  (define (type-in-definitions frame str)
    (type-in-definitions/interactions 'definitions-canvas frame str))
  (define (type-in-interactions frame str)
    (type-in-definitions/interactions 'interactions-canvas frame str))

  (define (type-in-definitions/interactions canvas-ivar frame str/sexp)
    (let ([str (if (string? str/sexp)
		   str/sexp
		   (let ([port (open-output-string)])
		     (parameterize ([current-output-port port])
		       (write str/sexp port))
		     (get-output-string port)))])
      (verify-drscheme-frame-frontmost 'type-in-definitions/interactions frame)
      (let ([len (string-length str)]
	    [canvas (ivar/proc frame canvas-ivar)])
	(fw:test:new-window canvas)
	(send (send canvas get-editor) set-caret-owner #f)
	(let loop ([i 0])
	  (unless (>= i len)
	    (let ([c (string-ref str i)])
	      (fw:test:keystroke
	       (if (char=? c #\newline)
		   #\return
		   c)))
	    (loop (+ i 1)))))))
  
  (define wait
    (case-lambda 
     [(test desc-string) (wait test desc-string 5)]
     [(test desc-string time)
      (let ([int 1/2])
	(let loop ([sofar 0])
	  (cond
	    [(> sofar time) (error 'wait desc-string)]
	    [(test) (void)]
	    [else (sleep int)
		  (loop (+ sofar int))])))]))
  
  (define (wait-pending)
    (wait (lambda () (= 0 (fw:test:number-pending-actions)))
	  "Pending actions didn't terminate")
    (fw:test:reraise-error))
  
  
;;; get-sub-panel takes 
;;;    a list of integers describing the path from a frame to the desired panel
;;;    the frame
;;;    based on code by Mark Krentel
  
;;;    Examples:
;;;    (get-sub-panel '() frame) gets the top-panel in frame
;;;    (get-sub-panel '(2) frame) gets the 2nd child of the top-panel 
;;;    (get-sub-panel '(2 0) frame) gets the 0th child of the 2nd child of the top-panel 
  
  (define (get-sub-panel path frame)
    (letrec ([loop 
	      (lambda (path panel)
		(if (null? path)
		    (if (is-a? panel panel%)
			panel
			(test-util-error "not a panel")) 
		    (loop
		     (cdr path)
		     (list-ref (send panel get-children) (car path)))))])
      (loop path frame)))
  
;;; get-text-pos returns the offset in an text buffer of the beginning
;;; of the last line
  
  (define (get-text-pos text)
    (let* ([last-pos (send text last-position)]
	   [last-line (send text position-line last-pos)])
      (send text line-start-position last-line)))
  
  ; poll for enabled button
  
  (define (wait-for-button button)
    (poll-until
     (let ([wait-for-button-pred
	    (lambda ()
	      (send button is-enabled?))])
       wait-for-button-pred)))
  
  (define (push-button-and-wait button)
    (fw:test:button-push button)
    (poll-until
     (let ([button-push-and-wait-pred
	    (lambda ()
	      (fw:test:reraise-error)
	      (= 0 (fw:test:number-pending-actions)))])
       button-push-and-wait-pred))
    (wait-for-button button))
  
  ; set language level in the frontmost DrScheme frame
  (define set-language-level! 
    (case-lambda
     [(level)
      (set-language-level! level #t)]
     [(level close-dialog?)
      (let ([frame (get-top-level-focus-window)])
        (fw:test:menu-select "Language" "Choose Language...")
        
        (wait-for-new-frame frame)
        (let ([language-choice (find-labelled-window "Language" choice%)])
          (cond
            [(member level (let loop ([n (send language-choice get-number)])
                             (cond
                               [(zero? n) null]
                               [else (cons (send language-choice get-string (- n 1))
                                           (loop (- n 1)))])))
             (fw:test:set-choice! language-choice level)]
            [else
             (fw:test:set-choice! language-choice "Full Scheme")
             (fw:test:set-radio-box!
              (find-labelled-window #f radio-box% (send language-choice get-parent))
              level)]))
        
        (when close-dialog?
          (let ([language-dialog (get-top-level-focus-window)])
            (fw:test:button-push "OK")
            (wait-for-new-frame language-dialog))))]))
  
  (define (repl-in-edit-sequence?)
    (send (ivar (wait-for-drscheme-frame) interactions-text) refresh-delayed?))
 
  (define (has-error? frame)
    (verify-drscheme-frame-frontmost 'had-error? frame)
    (let* ([interactions-text (ivar frame interactions-text)]
	   [last-para (send interactions-text last-paragraph)])
      (unless (>= last-para 2)
	(error 'has-error? "expected at least 2 paragraphs in interactions window, found ~a"
	       (+ last-para 1)))
      (let ([start (send interactions-text paragraph-start-position 2)]
	    [end (send interactions-text paragraph-end-position
		       (- (send interactions-text last-paragraph) 1))])
	(send interactions-text split-snip start)
	(send interactions-text split-snip end)
	(let loop ([pos start])
	  (cond
	   [(<= end pos) #f]
	   [else
	    (let ([snip (send interactions-text find-snip pos 'after-or-none)])
	      (cond
	       [(not snip) #f]
	       [else
		(let ([color (send (send snip get-style) get-foreground)])
		  (if (and (= 255 (send color red))
			   (= 0 (send color blue) (send color green)))
		      #t
		      (loop (+ pos (send snip get-count)))))]))])))))

  (define fetch-output
    (case-lambda
     [(frame)
      (verify-drscheme-frame-frontmost 'fetch-output frame)
      (let* ([interactions-text (ivar frame interactions-text)]
	     [last-para (send interactions-text last-paragraph)])
	(unless (>= last-para 2)
	  (error 'fetch-output "expected at least 2 paragraphs in interactions window, found ~a"
		 (+ last-para 1)))
	(fetch-output frame
		      (send interactions-text paragraph-start-position 2)
		      (send interactions-text paragraph-end-position
			    (- (send interactions-text last-paragraph) 1))))]
     [(frame start end)
      (verify-drscheme-frame-frontmost 'fetch-output frame)
      (let ([interactions-text (ivar frame interactions-text)])
	(send interactions-text split-snip start)
	(send interactions-text split-snip end)
	(let loop ([snip (send interactions-text find-snip end 'before)]
		   [strings null])
	  (cond
	    [(< (send interactions-text get-snip-position snip) start)
	     (apply string-append strings)]
	    [else 
	     (cond
	       [(is-a? snip string-snip%)
		(loop (send snip previous)
		      (cons (send snip get-text 0 (send snip get-count)) strings))]
	       [(is-a? snip editor-snip%)
		(let ([editor (send snip get-editor)])
		  (cond
		   [(is-a? editor pasteboard%)
		    (loop (send snip previous)
			  (cons "<pasteboard>" strings))]
		   [(is-a? editor text%)
		    (loop (send snip previous)
			  (list* "["
				 (send editor get-text)
				 "]"
				 strings))]))]
	       [(is-a? snip image-snip%)
		(loop (send snip previous)
		      (cons "<image>"
			    strings))]
	       [else (error 'find-output "unknown snip: ~e~n" snip)])])))])))