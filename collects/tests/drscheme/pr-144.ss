;;; pr-144.ss

;;; Open the preferences dialog, go to the check syntax section. 
;;; Wait for the autosave delay and make sure no autosaves appear.

(require-library "function.ss")

(load-relative "drscheme-test-util.ss")

(let* ([drs-frame (wait-for-drscheme-frame)] 
       [seconds 5]
       [autosave-prefix "#mredauto#"] 
       [autosave-prefix-len (string-length autosave-prefix)]
       [definitions-edit (ivar drs-frame definitions-edit)]
       [autosave-save (mred:get-preference 'mred:autosaving-on?)]
       [autosave-delay-save (mred:get-preference 'mred:autosave-delay)]
       [get-font-cbs
	(lambda (lst)
	  (let ([get-cb-with-label
		 (lambda (label)
		   (car (memf (lambda (elt)
				(and (is-a? elt mred:check-box%)
				     (string=? (send elt get-label) label)))
			      lst)))])
	    (map get-cb-with-label '("Slant" "Bold" "Underline"))))]
       [autosave-file? 
	(lambda (filename)
	  (and (> (string-length filename) autosave-prefix-len)
	       (string=? (substring filename 0 autosave-prefix-len)
			 autosave-prefix)))]
       [open-preferences
	(lambda ()
	  (mred:test:menu-select "Edit" "Preferences...")
	  (let* ([frame 
		   (letrec ([loop 
			     (lambda () 
			       (let ([active (mred:test:get-active-frame)])
				 (if (or (eq? active #f)
					 (eq? active drs-frame))
				     (begin
				       (sleep 1/2)
				       (loop))
				     active)))])
		     (loop))]
		 [panel (send frame get-top-panel)]
		 [children (ivar panel children)]
		 [choice-box (car children)]
		 [choice-box-event 
		  (let ([event-obj 
			 (make-object wx:command-event% 
				      wx:const-event-type-choice-command)])
		  (send event-obj set-event-object choice-box)
		  event-obj)])
	    (send choice-box-event set-command-int 
		  (send choice-box find-string "Check Syntax"))
	    (send choice-box command choice-box-event)

	    (let* ([upper-panel (cadr children)]
		   [check-syntax-panel (send upper-panel active-child)]
		   [check-box-panels (ivar check-syntax-panel children)]

		   [syntax-panel (car check-box-panels)]
		   [syntax-check-boxes (get-font-cbs (ivar syntax-panel children))]
	           [curr-states (map (lambda (cb) (send cb get-value))
				     syntax-check-boxes)])

	      ; toggle current states of syntax checkboxes
	      ; we're going to hit Cancel, so nothing should take effect

	      (map (lambda (cb state)
		     (mred:test:set-check-box! cb (not state)))
		   syntax-check-boxes
		   curr-states))))])

  ; delete any existing autosave files

  (for-each 
   (lambda (filename)
     (when (autosave-file? filename)
	   (delete-file filename)))
   (directory-list))

  (mred:set-preference 'mred:autosaving-on? #t)
  (mred:set-preference 'mred:autosave-delay seconds)

  (open-preferences)

  (sleep (+ seconds 5))

  ; now see if there are any autosave files 

  (if (ormap autosave-file? (directory-list))
      (printf "Autosave test failed~n")
      (printf "Autosave test succeeded~n"))

  (mred:test:button-push "Cancel")

  (mred:set-preference 'mred:autosaving-on? autosave-save)
  (mred:set-preference 'mred:autosave-delay autosave-delay-save))




  

	    
