;;; menu-test.ss

;;; tests the various menu items in the DrScheme menubar

;;; Author: Paul Steckler, based on earlier code by Robby Findler   

(load "drscheme-test-util.ss")

;; Under X, the validity of these tests requires that the window
;; with the mouse cursor is active.  That's not necessarily the case.

(let* ([frame (wait-for-drscheme-frame)]

       [eq-frame? (lambda () (eq? (mred:test:get-active-frame) frame))]

       [dialog-test  
	(lambda (menu)
	  (lambda (item)
		 (mred:test:menu-select menu item)
		 (wait (lambda () (not (eq-frame?)))
		       (string-append 
			"Didn't get a new frame after selecting " 
			menu "|" item))
		 (mred:test:button-push "Cancel")
	    (wait-pending)
	    (wait eq-frame?
		  (string-append 
		   "Original DrScheme frame not active after cancelling File|"
		   item))))]

	[file-dialog-test (dialog-test "File")]
	[edit-dialog-test (dialog-test "Edit")]
	[language-dialog-test (dialog-test "Language")]

	[file-dialog-items 
	 '("Open..." 
	   "Open URL..." 
	   "Save Definitions As..." 
	   "Save Definitions As Text..." 
	   "Save Interactions"
	   "Save Interactions As..."
	   "Save Interactions As Text..."

	   ; we omit the print dialogs, because the test primitives
	   ; only work with MrEd-derived classes

	   "Close"    ; do this 3 times, per Robby
	   "Close"
	   "Close"

	   ; the Quit dialog also seems not to work with the test primitives

	   )]

       [edit-dialog-items 
	'("Preferences...")]

       [language-dialog-items 
	'("Configure Language..." 
	  "Set Library To...")])

       ; this makes sure REPL is loaded

       (type-in-definitions frame "a") 

       (for-each file-dialog-test file-dialog-items)
       (printf "File menu tests complete~n")

       (for-each edit-dialog-test edit-dialog-items)
       (printf "Edit menu tests complete~n")

       (for-each language-dialog-test language-dialog-items)
       (printf "Language menu tests complete~n")

       (printf "All menu tests complete~n"))

; in old autosave+prompt-save.ss, we had:

; ((load-relative (build-path 'up "mred" "gui-main.ss"))
; "New Unit"
; "Save Definitions"
; wx:frame%)
