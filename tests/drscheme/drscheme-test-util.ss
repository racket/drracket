;;; util.ss

;;; utility functions for DrScheme GUI testing 

;;; Authors: Robby Findler, Paul Steckler

(define (test-util-error fmt . args)
  (raise (make-exn (apply fmt args) ((debug-info-handler)))))

(define (poll-until pred)
  (letrec 
      ([loop
	(lambda ()
	  (let ([result (pred)])
	    (if result
		result
		(begin
		  (sleep 1/2)
		  (loop)))))])
    (loop)))

(define (wait-for-drscheme-frame)
  (let* ([pred (lambda ()
		(let ([active (mred:test:get-active-frame)])
		  (if (and active
			   (is-a? active drscheme:export:unit:frame%))
		      active
		      #f)))]
	 [result (pred)])
    (if result
	result
	(begin
	  (printf "Select DrScheme frame~n")
	  (poll-until pred)))))

(define (wait-for-new-drscheme-frame old-frame)
  (poll-until 
   (lambda ()
     (let ([active (mred:test:get-active-frame)])
       (if (and active
		(not (eq? active old-frame)))
	   active
	   #f)))))

(define (clear-definitions frame)
  (mred:test:new-window (ivar frame definitions-canvas))
  (mred:test:menu-select "Edit" "Select All")
  (mred:test:menu-select "Edit" (if (eq? wx:platform 'macintosh)
				    "Clear"
				    "Delete")))

(define (type-in-definitions frame str)
  (let ([len (string-length str)])
    (mred:test:new-window (ivar frame definitions-canvas))
    (let loop ([i 0])
      (unless (>= i len)
	(let ([c (string-ref str i)])
	  (mred:test:keystroke
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
  (wait (lambda () (= 0 (mred:test:number-pending-actions)))
	"Pending actions didn't terminate")
  (mred:test:reraise-error))


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
		  (if (is-a? mred:panel% panel)
		      panel
		      (test-util-error "not a panel")) 
		  (loop
		   (cdr path)
		   (list-ref (ivar panel children) (car path)))))])
    (loop path (send frame get-top-panel))))

;;; get-text-pos returns the offset in an edit buffer of the beginning
;;; of the last line

(define (get-text-pos edit)
  (let* ([last-pos (send edit last-position)]
	 [last-line (send edit position-line last-pos)])
    (send edit line-start-position last-line)))

; poll for enabled button

(define (wait-for-button button)
  (poll-until
   (lambda () (send button is-enabled?))))

(define (push-button-and-wait button)
  (mred:test:button-push button)
  (wait-for-button button))

; set language level in a given DrScheme frame

(define (set-language-level! level drs-frame)
  (mred:test:menu-select "Language" "Configure Language...")
  (mred:test:new-window (wx:find-window-by-name "Language" null))
  (let* ([frame (wait-for-new-drscheme-frame drs-frame)]
	 [o-panel (send frame get-top-panel)]
	 [o-children (ivar o-panel children)]
	 [i-panel (car o-children)]
	 [i-children (ivar i-panel children)]
	 [choice (cadr i-children)])
    (mred:test:set-choice! choice level)
    (mred:test:button-push "OK")))




