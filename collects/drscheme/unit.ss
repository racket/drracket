
(define drscheme:unit@
  (unit/sig drscheme:unit^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [drscheme:basis : drscheme:basis^]
	    [drscheme:setup : drscheme:setup^]
	    [drscheme:compound-unit : drscheme:compound-unit^]
	    [drscheme:tool : drscheme:tool^])
    
    (mred:debug:printf 'invoke "drscheme:frame@")

    (define do-help
      (lambda ()
	(mred:open-hyper-view (mzlib:file@:build-absolute-path 
			       mred:plt-home-directory
			       "drscheme" "help" "index.htx"))))
    
    (define unit-snip%
      (class wx:snip% (name)
	(inherit get-admin)
	(rename [super-get-flags get-flags])
	(private
	  [width 40]
	  [height 20])
	(public
	  [resize
	   (lambda (w h)
	     (set! width w)
	     (set! height h)
	     (send (get-admin) resized this #t)
	     #t)]
	  [open
	   (lambda ()
	     (wx:message-box (format "opening snip: ~a" name)))]
	  [add-dependant
	   (lambda (x)
	     (set! dependants (cons x dependants)))]
	  [dependants null]
	  [draw
	   (lambda (dc x y left top right bottom dx dy draw-caret)
	     (send dc draw-text name x y)
	     (send dc draw-rounded-rectangle x y width height -1/3))]
	  [get-extent
	   (lambda (dc x y width-box height-box descent-box 
		       space-box lspace-box rspace-box)
	     (let ([size
		    (lambda (v)
		      (lambda (x)
			(unless (null? x)
			  (set-box! x v))))])
	       ((size width) width-box)
	       ((size height) height-box)
	       (for-each (size 3) 
			 (list descent-box space-box lspace-box rspace-box))))])
	(sequence
	  (super-init))))

    (define unit-frame%
      (class mred:editor-frame% (filename frameset [show? #t])
	(inherit canvas add-canvas remove-canvas
		 show menu-bar% make-menu
		 open-file check-saved active-edit active-canvas panel
		 file-menu file-menu:open-id file-menu:new-id file-menu:save-id 
		 file-menu:save-as-id file-menu:revert-id file-menu:print-id)
	(rename [super-make-menu-bar make-menu-bar]
		[super-on-close on-close])
	(private
	  show-menu
	  show-both
	  [name-message #f]
	  [save-button #f]
	  [save-init-shown? #f])

	(public
	  [canvas-show-mode #f]
	  [frames frameset]
	  [project (ivar frameset project)]
	  [allow-split? #f]
	  [forced-quit? #f])

	(public
	  [canvas% 
	   (class mred:simple-frame-canvas% args
	     (inherit get-media)
	     (rename [super-edit-modified edit-modified])
	     (public
	       [edit-renamed
		(lambda (name)
		  (when save-button
		    (let ([msg (make-object mred:message% top-panel
					    (if (null? name) 
						"Untitled" 
						(or (mzlib:file@:file-name-from-path name)
						    "Untitled")))])
		      (set! name-message msg)
		      (send top-panel change-children (lambda (l) (build-top-panel-children))))))]
	       [edit-modified
		(lambda (mod?)
		  (if save-button
		      (send save-button show mod?)
		      (set! save-init-shown? mod?))
		  (super-edit-modified mod?))])
	     (sequence
	       (mred:debug:printf 'super-init "before.2")
	       (apply super-init args)
	       (mred:debug:printf 'super-init "after.2")
	       (let ([m (get-media)])
		 (set! save-init-shown? (and (not (null? m)) (send m modified?))))))]
	  [check-console-shown
	   (lambda ()
	     (when (eq? canvas-show-mode 'program)
	       (send show-menu dispatch show-both)))])
	
	(public
	  [get-edit%
	   (lambda ()
	     (class mred:scheme-mode-edit% args
	       (rename [super-scroll-to-position scroll-to-position]
		       [super-set-position set-position]
		       [super-set-position-bias-scroll set-position-bias-scroll])
	       (public
		 [set-position
		  (lambda args
		    (printf "set-position: ~a~n" args)
		    (apply super-set-position args))]
		 [set-position-bias-scroll
		  (lambda args
		    (printf "set-position-bias-scroll: ~a~n" args)
		    (apply super-set-position-bias-scroll args))]
		 [scroll-to-position
		  (lambda args
		    (printf "scroll-to-position: ~a~n" args)
		    (apply super-scroll-to-position args))]
		 [auto-set-wrap? (mred:get-preference 'drscheme:wrap-program?)])
	       (sequence
		 (mred:debug:printf 'super-init "before.5")
		 (apply super-init args)
		 (mred:debug:printf 'super-init "after.5"))))]
	  
	  [get-program-edit
	   (lambda ()
	     (send program-canvas get-media))]
	  [get-console-edit
	   (lambda ()
	     (send console-canvas get-media))]
	  
	  [get-buffer-group
	   (lambda ()
	     (ivar (ivar project group) buffers))]
	  
	  [break (lambda () (send (ivar project console-edit) break))]
	  
	  [set-project
	   (lambda (p)
	     (set! project p))]
	  [has-project? #f]
	  [is-project?
	   (lambda (name)
	     (let ([ext (mzlib:file@:filename-extension name)])
	       (and ext (string=? ext "spj"))))]
	  [program-in-project?
	   (lambda ()
	     (and has-project?
		  (let ([name (send (get-program-edit) get-filename)])
		    (and (string? name)
			 (send project file-in-project? 
			       (mzlib:file@:normalize-path name))))))]
	  [open-project
	   (lambda (name)
	     (send (ivar project group) remove-frame this)
	     (send (get-buffer-group) kill-buffer (get-program-edit))
	     (let ([ok? (send project read-project name)])
	       (send (ivar project group) insert-frame this)
	       (when ok?
		 (send (get-buffer-group) add-buffer 
		       'file #f (get-program-edit))
		 (set! has-project? name)
		 (send project show name)
		 (if name
		     (let ([last-file #f])
		       (send project for-each-file
			     (lambda (f) (set! last-file f)))
		       (if last-file
			   (change-to-file last-file)))))))]
	  [project-closed
	   (lambda ()
	     (set! has-project? #f)
	     (send (ivar project group) clear)
	     (send (ivar (ivar project group) buffers) add-buffer 
		   'file #f (get-program-edit))
	     (send (ivar project group) insert-frame this)
	     (send (ivar project group) set-frame-title-prefix "DrScheme"))]
	  [change-to-file
	   (lambda (name)
	     (let ([buffer (send program-canvas get-media)])
	       (if (open-file name program-canvas #f)
		   (release-buffer buffer))))]
	  
	  [release-buffer
	   (lambda (buffer)
	     ; If this buffer is not displayed anywhere, drop it
	     (let ([name (if has-project?
			     (send buffer get-filename)
			     #f)])
	       (unless (and has-project?
			    (string? name)
			    (send project file-in-project? 
				  (mzlib:file@:normalize-path name)))
		 (send (get-buffer-group) kill-buffer buffer)
		 (send buffer remove-autosave))))]
	  
	  [set-show-mode
	   (lambda (which)
	     (unless (eq? which canvas-show-mode)
	       (let* ([children null]
		      [children (if (eq? which 'scheme)
				    (cons scheme-only-panel children)
				    (cons top-panel children))]
		      [children (if (or (eq? which 'program)
					(eq? which 'both))
				    (cons program-canvas children)
				    children)]
		      [children (if (or (eq? which 'scheme)
					(eq? which 'both))
				    (cons console-canvas children)
				    children)])
		 (send panel change-children
		       (lambda (l)
			 (reverse children)))
		 (send (get-console-edit) scroll-to-position 
		       (send (get-console-edit) get-end-position)
		       #f
		       (send (get-console-edit) get-start-position)
		       1)
		 (send (get-program-edit) scroll-to-position 
		       (send (get-program-edit) get-end-position)
		       #f
		       (send (get-program-edit) get-start-position)	
		       1))
	       (set! canvas-show-mode which)
	       (map 
		(lambda (id)
		  (send file-menu enable id (not (eq? which 'scheme))))
		(list file-menu:open-id
		      file-menu:new-id
		      file-menu:save-id
		      file-menu:save-as-id 
		      file-menu:revert-id
		      file-menu:print-id))
	       (send file-menu enable file-menu:print-transcript-id 
		     (not (eq? which 'program)))
	       (case which
		 [(scheme) (send console-canvas set-focus)]
		 [(program) (send program-canvas set-focus)]
		 [else (void)])))])
	
	(public
	  [file-menu:new-string " Program"]
	  [file-menu:new
	   (lambda ()
	     (if (or (program-in-project?)
		     (check-saved program-canvas "New Program"))
		 (change-to-file #f)))]
	  [file-menu:between-new-and-open
	   (lambda (file-menu)
	     (send file-menu append-item "New Project..."
		   (lambda ()
		     (if (if has-project?
			     (send project check-project-all-saved "Open Project")
			     (check-saved program-canvas "Open Project"))
			 (let ([name (mred:put-file
				      () () #f "New Project Name"
				      ".*\\.spj"
				      "Project names must end with \".spj\".")])
			   (if name
			       (open-project name)))))))]
	  [file-menu:open-string " Program"]
	  [file-menu:open
	   (lambda ()
	     (if (or (program-in-project?)
		     (check-saved program-canvas "Open File"))
		 (let ([name (mred:get-file () "Select a File")])
		   (when name
		     (if (is-project? name)
			 (if (or (not has-project?)
				 (send project check-project-all-saved
				       "Open Project"))
			     (open-project name))
			 (change-to-file name))))))]
	  [file-menu:between-open-and-save
	   (lambda (file-menu)
	     (send file-menu append-item "Open Project..."
		   (lambda ()
		     (if (if has-project?
			     (send project check-project-all-saved "Open Project")
			     (check-saved program-canvas "Open Project"))
			 (let ([name 
				(mred:get-file 
				 () "Select a Project"
				 ".*\\.spj"
				 "Project names use the extension \".spj\".")])
			   (when name
			     (open-project name))))))
	     (send file-menu append-separator))]
	  [file-menu:save (lambda () (send (get-program-edit) save-file))]
	  [file-menu:save-as 
	   (lambda () 
	     (send (get-program-edit) save-file ""))]
	  [file-menu:between-save-and-print (lambda (edit-menu) (void))]
	  [file-menu:revert 
	   (lambda () 
	     (let ([e (get-program-edit)]
		   [b (box #f)])
		 (if (or (null? (send e get-filename b)) (unbox b))
		     (wx:bell)
		     (send e load-file))))]
	  [file-menu:other (lambda () (send (ivar (ivar project group) buffers)
					    pick program-canvas))]
	  [file-menu:print (lambda () (send (get-program-edit) print '()))]
	  [file-menu:print-string " Program"]
	  
	  [file-menu:print-transcript-id #f]
	  [file-menu:between-print-and-close
	   (lambda (file-menu)
	     (set! file-menu:print-transcript-id
		   (send file-menu append-item "Print Scheme Transcript..."
			 (lambda () (send (ivar project console-edit)
					  print '()))))
	     (send file-menu append-separator))]
	  
	  [file-menu:close-string (if mred:debug:on? "DEBUG" "")]
	  [file-menu:close (if mred:debug:on?
			       (lambda () (show #f))
			       #f)]
	  
	  [edit-menu:between-select-all-and-preferences
	   (lambda (edit-menu)
	     (send edit-menu append-separator)
	     (send edit-menu append-item "Find" 
		   (lambda ()
		     (let ([bx (box 0)]
			   [by (box 0)]) 
		       (send (get-program-edit) position-location 
			     (send (active-edit) get-start-position) 
			     bx by)
		       (mred:find-string (active-canvas) null
					 (unbox bx) (unbox by)
					 null)))
		   "" #f "f")
	     (send edit-menu append-separator))]
	  [edit-menu:after-standard-items (lambda (edit-menu) (void))]
	  
	  [allow-font-menu? #f]
	  [make-menu-bar
	   (lambda ()
	     (let ([mb (super-make-menu-bar)]
		   [scheme-menu (make-menu)]
		   [tools-menu (make-menu)]
		   [language-menu (make-menu)])
	       (set! show-menu (make-menu))
	       
	       (send* mb
		      (append show-menu "S&how")
		      (append scheme-menu "S&cheme")
		      (append tools-menu "&Tools")
		      (append language-menu "&Language"))

	       (send* language-menu
		      (append-item "Select Library..."
				   (lambda ()
				     (let ([lib-file (mred:get-file null "Select a Library" ".*\\.ss$")])
				       (when lib-file
					 (mred:set-preference 'drscheme:library-file lib-file)))))
		      (append-item "Clear Library"
				   (lambda () (mred:set-preference 'drscheme:library-file #f)))
		      (append-separator)
		      (append-check-set
		       (map cons drscheme:basis:level-strings
			    drscheme:basis:level-symbols)
		       (let ([state #t])
			 (lambda (s)
			   (mred:set-preference 'drscheme:scheme-level s)
			   (when state
			     (set! state #f)
			     (unless (mred:get-choice "Changes to the language level will not take effect until DrScheme is restarted"
						      "Continue Working"
						      "Exit")
			       (mred:exit)))))
		       (drscheme:basis:level->number
			(mred:get-preference 'drscheme:scheme-level))))

		 

	       
	       (for-each 
		(lambda (x)
		  (let* ([id #f]
			 [callback
			  (lambda ()
			    (unless mred:debug:on?
			      (send tools-menu enable id #f))
			    (drscheme:tool:load/invoke-tool x))])
		    (set! id (send tools-menu append-item (drscheme:tool:tool-name x) callback))))
		drscheme:tool:tools)

	       (send* scheme-menu
		      (append-item "&Indent" 
				   (lambda () (send (ivar (active-edit) mode) tabify-selection (active-edit))))
		      (append-item "Indent &All"
				   (lambda ()
				     (send (ivar (active-edit) mode) tabify-all (active-edit)))
				   "" #f "i")
		      (append-item "&Comment Out"
				   (lambda ()
				     (send (ivar (active-edit) mode) comment-out-selection (active-edit))))
		      (append-item "&Uncomment"
				   (lambda ()
				     (send (ivar (active-edit) mode) uncomment-selection (active-edit)))))
	       (when (mred:get-preference 'drscheme:use-setup?)
		 (send* scheme-menu
			(append-separator)
			(append-item "Setup H&omework..." (lambda () (drscheme:setup:do-setup "hw")))
			(append-item "Setup &Lab..." (lambda () (drscheme:setup:do-setup "lab")))))
	       (set! show-both
		     (caddr
		      (send show-menu append-check-set
			    '(("&Definitions Only" . program) 
			      ("&Interactions Only" . scheme) 
			      ("&Both" . both))
			    set-show-mode 2)))
	       mb))]
	  
	  [on-close
	   (lambda ()
	     (and (super-on-close)
		  (begin
		    (send (ivar project group) remove-frame this)
		    (when (and (null? (send (ivar project group) get-frames))
			       (not (mred:get-preference 'drscheme:project-visible?)))
		      (mred:exit))
		    #t)))]
	  
	  [running? #t] ; For project to know the console is OK
	  
	  [on-forced-quit
	   (lambda ()
	     (set! forced-quit? #t)
	     (let loop ()
	       (if (not (on-close))
		   (loop)))
	     (show #f))]
	  [on-quit
	   (lambda ()
	     (if (on-close)
		 (show #f)))]
	  [set-title-prefix void]
	  [execute-callback
	   (lambda (button evt)
	     (let* ([program-edit (get-program-edit)]
		    [console-edit (get-console-edit)])
	       (send console-edit reset-console)
	       (send console-edit do-many-aries-evals
		     program-edit
		     0 (send program-edit last-position)
		     (lambda () (send console-edit busy? #t))
		     (lambda ()
		       (send console-edit busy? #f)
		       (send (send console-edit get-canvas) set-focus)
		       (send console-edit insert-prompt)))))])
	
	(sequence
	  (mred:debug:printf 'super-init "before.6: frameset:~a" frameset)
	  (super-init filename #f frameset)
	  (mred:debug:printf 'super-init "after.6"))
	
	(private
	  [top-panel (make-object mred:horizontal-panel% panel)])
	(sequence
	  (let* ([canvas (active-canvas)]
		 [edit (and canvas (send canvas get-media))]) 
	    (set! name-message
		  (make-object mred:message% top-panel
			       (cond
				 [(or (not edit) (null? edit)) "Untitled"]
				 [(mzlib:file@:file-name-from-path (send edit get-filename))]
				 [else "Untitled"]))))
	  
	  (set! save-button
		(make-object mred:button% 
			     top-panel
			     (lambda args
			       (let* ([edit (get-program-edit)])
				 (unless (or (null? edit) (not edit))
				   (send edit save-file))))
			     "Save")))
	(private 
	  [make-library-name-msg
	   (lambda (panel n)
	     (make-object mred:message% panel 
			  (if n
			      (let-values ([(base name must-be-dir) (split-path n)])
				name)
			      "")))]
	  [space1 (make-object mred:horizontal-panel% top-panel)]
	  [library-msg (make-library-name-msg
			top-panel
			(mred:get-preference 'drscheme:library-file))]
	  [space2 (make-object mred:horizontal-panel% top-panel)])
	
	(public
	  [stop-execute-button (void)]
	  [execute-button (void)]
	  [debug-button (void)]
	  [button-panel (make-object mred:horizontal-panel% top-panel)]
	  [scheme-only-panel (make-object mred:horizontal-panel% panel)])
	
	(private
	 [scheme-only-library-msg
	  (make-library-name-msg scheme-only-panel
				 (mred:get-preference 'drscheme:library-file))]
	 [scheme-only-space
	  (make-object mred:vertical-panel% scheme-only-panel)]
	 [scheme-only-stop-executing
	  (make-object mred:button% scheme-only-panel
		       (lambda args (send (ivar project console-edit) break))
		       "Stop Executing")]
	 [scheme-only-help
	  (make-object mred:button% scheme-only-panel
		       (lambda args (do-help))
		       "Help")])
	 (sequence
	  (send panel delete-child scheme-only-panel)

	  (send save-button show save-init-shown?))
	
	 (sequence
	  (make-object mred:button% button-panel
		       (lambda x (wx:message-box
				  "The debugger is not available in DrScheme version 44."))
		       "Debug")
	  (set! execute-button
	    (make-object mred:button% button-panel
			 execute-callback
			 "Execute"))
	  (set! stop-execute-button
		(make-object mred:button% button-panel 
			     (lambda args (send (ivar project console-edit) break))
			     "Stop Executing"))
	 (make-object mred:button% button-panel
		       (lambda args (do-help))
		       "Help")
	  
	  (send scheme-only-panel border 1)
	  (send button-panel border 1)
	  (send top-panel border 1)
	  (send top-panel spacing 10)
	  (send scheme-only-panel stretchable-in-y #f)
	  (send button-panel stretchable-in-y #f)
	  (send button-panel stretchable-in-x #f) 
	  (send top-panel stretchable-in-y #f)
	  
	  (mred:add-preference-callback
	   'drscheme:library-file
	   (lambda (p v)
	     (set! scheme-only-library-msg
		   (make-library-name-msg scheme-only-panel v))
	     (set! library-msg (make-library-name-msg top-panel v))
	     (send scheme-only-panel change-children
		   (lambda (l) (list scheme-only-library-msg
				     scheme-only-space
				     scheme-only-stop-executing
				     scheme-only-help)))
	     (send top-panel change-children (lambda (l) (build-top-panel-children))))))
		     
	(private
	  [build-top-panel-children
	   (lambda ()
	     (list name-message save-button space1 library-msg space2 button-panel))])
	
	(public
	  [program-canvas canvas])
	
	(sequence
	  (send (send canvas get-media) set-mode (make-object mred:scheme-mode%))
	  (send (ivar project console-edit) set-auto-set-wrap #t))
	
	(public
	  [console-canvas
	   (make-object mred:simple-frame-canvas% panel
			-1 -1 -1 -1 ""
			wx:const-mcanvas-no-h-scroll
			100)])
	
	(sequence
	  (send console-canvas set-media (ivar project console-edit))
	  (add-canvas console-canvas)

	  ;; show both windows initially
	  (set-show-mode 'both)

	  (send program-canvas set-focus)
	  (when show?
	    (show #t)))))))