;;; util.ss

;;; utility functions for DrScheme GUI testing 

;;; Authors: Robby Findler, Paul Steckler

(unit/sig drscheme:test-util^
  
  (import [mred : mred^]
	  [fw : framework^]
	  test-utils:gui^)
  
  ;; HACK!
  (define (get-drscheme-eventspace)
    (global-defined-value 'drscheme-eventspace))
  
  ;; -> eventspace
  ;; returns the eventspace used by the program in the current drscheme window
  (define (get-user-eventspace)
    ((in-parameterization
      (ivar (ivar (wait-for-drscheme-frame) interactions-edit)
	    user-param)
      mred:current-eventspace)))
  
  (define (test-util-error fmt . args)
    (raise (make-exn (apply fmt args) ((debug-info-handler)))))
  
  (define poll-until
    (case-lambda
     [(pred) (poll-until pred 10)]
     [(pred secs)
      (let ([step 1/2])
	(let loop ([counter secs])
	  (if (<= counter 0)
	      (error 'poll-until "timeout after ~e secs, ~e never returned a true value" secs pred)
	      (let ([result (pred)])
		(or result
		    (begin
		      (sleep step)
		      (loop (- counter step))))))))]))
  
  (define (wait-for-drscheme-frame)
    (let* ([pred (lambda ()
		   (mred:yield)
		   (let ([active (mred:get-top-level-focus-window)])
		     (printf "active: ~a~n" active)
		     (if (and active
			      (begin
				(ivar-in-class? 'execute-button (object-class active))))
			 active
			 #f)))])
      (or (pred)
	  (begin
	    (printf "Select DrScheme frame~n")
	    (poll-until pred)))))
  
  (define (wait-for-new-frame old-frame)
    (poll-until
     (lambda ()
       (let ([active (mred:get-top-level-focus-window)])
	 (if (and active
		  (not (eq? active old-frame)))
	     active
	     #f)))))
  
  (define (clear-definitions frame)
    (fw:test:new-window (ivar frame definitions-canvas))
    (fw:test:menu-select "Edit" "Select All")
    (fw:test:menu-select "Edit" (if (eq? (system-type) 'macos)
				    "Clear"
				    "Delete")))
  
  (define (type-in-definitions frame str)
    (let ([len (string-length str)])
      (fw:test:new-window (ivar frame definitions-canvas))
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
		    (if (is-a? panel mred:panel%)
			panel
			(test-util-error "not a panel")) 
		    (loop
		     (cdr path)
		     (list-ref (send panel get-children) (car path)))))])
      (loop path frame)))
  
;;; get-text-pos returns the offset in an edit buffer of the beginning
;;; of the last line
  
  (define (get-text-pos edit)
    (let* ([last-pos (send edit last-position)]
	   [last-line (send edit position-line last-pos)])
      (send edit line-start-position last-line)))
  
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
  (define (set-language-level! level)
    (let ([frame (mred:get-top-level-focus-window)])
      (fw:test:menu-select "Language" "Configure Language...")
      (wait-for-new-frame frame)
      (fw:test:set-choice! (find-labelled-window "Language" mred:choice%) level)
      (fw:test:button-push "OK")))
  
  (define (repl-in-edit-sequence?)
    (send (ivar (wait-for-drscheme-frame) interactions-edit) refresh-delayed?))
  
  (define (fetch-output)
    (mred:get-top-level-focus-window)))