;;; util.ss

;;; utility functions for DrScheme GUI testing 

;;; Authors: Robby Findler, Paul Steckler

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
		(let ([active (get-top-level-focus-window)])
		  (if (and active
			   (is-a? active drscheme:export:unit:frame%))
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
	    (lambda (path curr-panel)
	      (if (null? path)
		  (if (is-a? panel% panel)
		      panel
		      (test-util-error "not a panel")) 
		  (loop
		   (cdr path)
		   (list-ref (send panel get-children) (car path)))))])
    (loop path frame)))

;;; find-labelled-window : (union (string-> window<%>)
;;;                               (string (union #f class) -> window<%>)
;;;                               (string (union class #f) window<%> -> window<%>))
;;;;  may call error, if no control with the label is found
(define find-labelled-window
  (case-lambda
   [(label) (find-labelled-window label #f)]
   [(label class) (find-labelled-window label class (mred:get-top-level-focus-window))]
   [(label class window)
    (unless (string? label)
      (error 'find-labelled-window "first argument must be a string, got ~e; other args: ~e ~e"
	     label class window))
    (unless (or (class? class)
		(not class))
      (error 'find-labelled-window "second argument must be a class or #f, got ~e; other args: ~e ~e"
	     class label window))
    (unless (is-a? window mred:window<%>)
      (error 'find-labelled-window "third argument must be a window<%>, got ~e; other args: ~e ~e"
	     window label class))
    (let ([ans
	   (let loop ([window window])
	     (cond
	       [(and (or (not class)
			 (is-a? window class))
		     (string=? label (send window get-label)) )
		window]
	       [(is-a? window mred:area-container<%>) (ormap loop (send window get-children))]
	       [else #f]))])
      (or ans
	  (error 'find-labelled-window "no window labelled ~e in ~e~a"
		 label
		 window 
		 (if class
		     (format " matching class ~e" class)
		     ""))))]))

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
	 