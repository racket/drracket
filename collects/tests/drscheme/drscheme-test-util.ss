;;; util.ss

;;; utility functions for DrScheme GUI testing 

;;; Authors: Robby Findler, Paul Steckler

(unit/sig drscheme:test-util^
  
  (import mred^
	  [fw : framework^]
	  test-utils:gui^)
  
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
    (let* ([pred (lambda ()
		   (yield)
		   (let ([active (get-top-level-focus-window)])
		     (if (and active
			      (drscheme-frame? active))
			 active
			 #f)))])
      (or (pred)
	  (begin
	    (printf "Select DrScheme frame~n")
	    (poll-until pred)))))
  
  (define (wait-for-new-frame old-frame)
    (poll-until
     (lambda ()
       (let ([active (get-top-level-focus-window)])
	 (if (and active
		  (not (eq? active old-frame)))
	     active
	     #f)))))

  (define (wait-for-computation frame)
    (verify-drscheme-frame-frontmost 'wait-for-computation frame)
      (let ([button (ivar frame execute-button)])
	(poll-until
	 (lambda ()
	   (fw:test:reraise-error)
	   (send button is-enabled?))
	 30)))

  (define do-execute 
    (lambda (frame)
      (verify-drscheme-frame-frontmost 'do-execute frame)
      (let ([button (ivar frame execute-button)])
	(fw:test:button-push button)
	(wait-for-computation frame))))
  
  (define (verify-drscheme-frame-frontmost function-name frame)
    (unless (and (eq? frame (get-top-level-focus-window))
		 (drscheme-frame? frame))
      (error function-name "drscheme frame not frontmost: ~e" frame)))
  
  (define (clear-definitions frame)
    (verify-drscheme-frame-frontmost 'clear-definitions frame)
    (fw:test:new-window (ivar frame definitions-canvas))
    (fw:test:menu-select "Edit" "Select All")
    (fw:test:menu-select "Edit" (if (eq? (system-type) 'macos)
				    "Clear"
				    "Delete")))
    
  
  (define (type-in-definitions frame str)
    (type-in-definitions/interactions 'definitions-canvas frame str))
  (define (type-in-interactions frame str)
    (type-in-definitions/interactions 'interactions-canvas frame str))

  (define (type-in-definitions/interactions canvas-ivar frame str)
    (verify-drscheme-frame-frontmost 'type-in-definitions/interactions frame)
    (let ([len (string-length str)])
      (fw:test:new-window (ivar/proc frame canvas-ivar))
      (let loop ([i 0])
	(unless (>= i len)
	  (let ([c (string-ref str i)])
	    (fw:test:keystroke
	     (if (char=? c #\newline)
		 #\return
		 c)))
	  (loop (+ i 1))))))
  
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
     (lambda ()
       (send button is-enabled?))))
  
  (define (push-button-and-wait button)
    (fw:test:button-push button)
    (poll-until
     (lambda ()
       (fw:test:reraise-error)
       (= 0 (fw:test:number-pending-actions))))
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
	       [(is-a? snip image-snip%
		       (loop (send snip previous)
			     (cons "<image>"
				   strings)))]
	       [else (error 'find-output "unknown snip: ~e~n" snip)])])))])))