;;; config-lang-test.ss

;;; tests the toggle options in the dialog started from the 
;;; Language | Configure Language... menu item

;;; Author: Paul Steckler

(load-relative "drscheme-test-util.ss")

(letrec* ([_ (wait-for-drscheme-frame)]
	  [drscheme-frame (mred:test:get-active-frame)]
	  [eq-frame? (lambda () (eq? (mred:test:get-active-frame) drscheme-frame))]
	  [interactions-edit (ivar drscheme-frame interactions-edit)]
	  [interactions-canvas (ivar drscheme-frame interactions-canvas)]
	  [definitions-edit (ivar drscheme-frame definitions-edit)]
	  [definitions-canvas (ivar drscheme-frame definitions-canvas)]
	  [execute-button (ivar drscheme-frame execute-button)]
	  [get-int-pos (lambda () (get-start-of-last-line interactions-edit))]
	  [wait-for-events
	   (lambda (nevents)
	     (let loop ()
	       (unless (= nevents (mred:test:number-pending-actions))
		       (sleep 1/2)
		       (loop))))]
	  [run-test
	   (lambda (cb state code expected)

	     ; click on menu item
	     
	     (mred:test:menu-select "Language" "Configure Language...")
	     (mred:test:new-window (wx:find-window-by-name "Language" null))

	     ; open sub-dialog

	     (with-handlers ([(lambda (_) #t) (lambda (x) (printf "~a~n" (exn-message x)))])
			    (mred:test:button-push "Show Details")
			    (wait-for-events 1)
			    (mred:test:reraise-error))

	     (mred:test:set-check-box! cb state)
	     
	     ; close dialog
	     
	     (mred:test:button-push "OK")

	     ; enter code in definitions window

	     (wait-for-drscheme-frame)
	     (mred:test:new-window definitions-canvas)
	     (clear-definitions drscheme-frame)
	     (mred:test:button-push execute-button)

	     (let ([answer-begin (send interactions-edit last-position)])

	       (type-in-definitions drscheme-frame code)
	       (mred:test:keystroke #\return)

	       ; compare actual answer to expected

	       (push-button-and-wait execute-button)

	       (let* ([answer-end (- (send interactions-edit last-position) 3)]
		      [actual (send interactions-edit get-text
				     answer-begin answer-end)])
		 (unless (string=? actual expected)
			 (printf "Expected: ~a~n Actual: ~a~n~n"
				 expected actual)))))])
	 
	 ; now toggle items and test

	 (mred:test:run-interval 500)

	 (run-test "Case sensitive" #f
		   "(eq? 'foo 'FOO)"
		   "#t"))

