
  (unit/sig drscheme:unit^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [drscheme:app : drscheme:app^]
	    [drscheme:setup : drscheme:setup^]
	    [drscheme:compound-unit : drscheme:compound-unit^]
	    [drscheme:frame : drscheme:frame^]
	    [drscheme:edit : drscheme:edit^]
	    [drscheme:rep : drscheme:rep^]
	    [drscheme:language : drscheme:language^]
	    [drscheme:parameters : drscheme:parameters^])
    
    (mred:debug:printf 'invoke "drscheme:unit@")
    
    (define make-bitmap
      (let* ([font (send wx:the-font-list find-or-create-font
			 (if (eq? wx:platform 'windows) 8 12)
			 wx:const-system
			 wx:const-normal
			 wx:const-normal)]
	     [outside-margin 2]
	     [middle-margin 3])
	(lambda (filename text)
	  (let* ([clear-bitmap? #f]
		 [img-bitmap
		  (let ([q (make-object wx:bitmap% filename wx:const-bitmap-type-bmp)])
		    (if (send q ok?)
			q
			(begin (set! clear-bitmap? #t)
			       (make-object wx:bitmap% 1 1))))]
		 [img-width (send img-bitmap get-width)]
		 [img-height (send img-bitmap get-height)]
		 [img-memory-dc (make-object wx:memory-dc%)]
		 [width (box 0.)]
		 [height (box 0.)]
		 [descent (box 0.)]
		 [leading (box 0.)])
	    (send img-memory-dc select-object img-bitmap)
	    (when clear-bitmap?
	      (send img-memory-dc clear))
	    (send img-memory-dc get-text-extent text width height descent leading font)
	    (let* ([new-width (+ outside-margin
				 img-width
				 middle-margin
				 (unbox width)
				 outside-margin)]
		   [new-height (+ outside-margin
				  (max img-height
				       (+ (unbox height)
					  (unbox descent)
					  (unbox leading)))
				  outside-margin)]
		   [memory-dc (make-object wx:memory-dc%)]
		   [new-bitmap (make-object wx:bitmap% new-width new-height -1)])
	      (send memory-dc select-object new-bitmap)
	      (send memory-dc set-font font)
	      (send memory-dc clear) 
	      (send memory-dc set-font font)
	      (send memory-dc draw-text text (+ outside-margin img-width middle-margin)
		    (- (/ new-height 2) (/ (unbox height) 2)))
	      (send memory-dc blit
		    outside-margin
		    (- (/ new-height 2) (/ img-height 2))
		    img-width img-height
		    img-memory-dc 0 0 wx:const-copy)
	      (send memory-dc select-object null)
	      new-bitmap)))))

    (define-values (execute-bitmap help-bitmap save-bitmap break-bitmap)
      (apply values (map 
		     (lambda (filename)
		       (let* ([capd (string-copy filename)]
			      [path (build-path
				     mred:constants:plt-home-directory
				     "icons"
				     (string-append filename ".bmp"))])
			 (string-set! capd 0 (char-upcase (string-ref capd 0)))
			 (make-bitmap path capd)))
		     (list "execute" "help" "save" "break"))))
    
    ;; this is the old definition of the interactions canvas.
    ;; It should be integrated into mred:wide-snip-canvas% 
    ;; becuase it uses a better algorithm to find the snip
    ;; wdie widths.
    '(class-asi mred:wide-snip-canvas% ; to match rep-new.ss, inherit from wrapping-canvas% 
	(inherit get-media)
	(rename [super-on-size on-size]
		[super-set-media set-media])
	(public)
	(private
	  [snips null]
	  [autowrap-snips? (mred:get-preference 'mred:auto-set-wrap?)]
	  [update-snip-size
	   (lambda (s)
	     (if (is-a? s wx:media-snip%)
		 (let* ([snip-x-pos&margins
			 (let loop ([snip s])
			   (let* ([snip-x-pos (box 0)]
				  [containing-media (send (send snip get-admin) get-media)]
				  [containing-admin (send containing-media get-admin)])
			     (send containing-media get-snip-position-and-location
				   snip (box 0) snip-x-pos (box 0))
			     (+ (let ([lmargin (box 0)]
				      [rmargin (box 0)])
				  (send snip get-margin lmargin (box 0) rmargin (box 0))
				  (+ (unbox lmargin) (unbox rmargin)))
				(if (is-a? containing-admin wx:media-snip-media-admin%)
				    (+ (unbox snip-x-pos)
				       (loop (send containing-admin get-snip)))
				    (unbox snip-x-pos)))))]
			[outer-snip (let loop ([snip s])
				      (let ([containing-admin
					     (send (send (send snip get-admin)
							 get-media) get-admin)])
					(if (is-a? containing-admin wx:media-snip-media-admin%)
					    (loop (send containing-admin get-snip))
					    snip)))]					
			[view-width (let* ([width (box 0)]
					   [extra-space 2] ;; this is to allow the edit room
					                   ;; to show the caret at the end
							   ;; of the line
					   [media (get-media)])
				      (send (send media get-admin)
					    get-view null null width null)
				      (- (unbox width)
					 extra-space))]
			[snip-width (- view-width snip-x-pos&margins)])
		   (send s set-min-width snip-width)
		   (send s set-max-width snip-width)
		   (when (is-a? s wx:media-snip%)
		     (let ([snip-media (send s get-this-media)])
		       (unless (null? snip-media)
			 (send snip-media set-max-width
			       (if autowrap-snips?
				   snip-width
				   0))))))
		 (send (send s get-admin) resized s #t)))])
	(public
	  [set-media
	   (lambda (x)
	     (super-set-media x)
	     (send x on-set-media this))]
	  [NO-GOOD-wrap-snips
	   (lambda (x)
	     (set! autowrap-snips? x)
	     (for-each update-snip-size snips))]
	  [NOGOOD-add-wide-snip
	   (lambda (snip)
	     (set! snips (cons snip snips))
	     (update-snip-size snip))]
	  [NOGOOD-on-size
	   (lambda (width height)
	     (super-on-size width height)
	     (for-each update-snip-size snips))]))

    (define do-help
      (lambda ()
	(mred:show-busy-cursor
	 (lambda ()
	   (mred:open-hyper-view
	    (string-append
	     "file:"
	     (build-path mred:constants:plt-home-directory
			 "doc"
			 "drscheme"
			 "index.htm")))))))

    ;; this sends a message to it's frame when it gets the focus
    (define make-searchable-canvas%
      (lambda (%)
	(class-asi %
	  (inherit frame)
	  (rename [super-on-set-focus on-set-focus])
	  (public
	    [on-set-focus
	     (lambda ()
	       (send frame make-searchable this)
	       (super-on-set-focus))]))))

    (define interactions-canvas% (make-searchable-canvas% mred:wide-snip-canvas%))

    (define definitions-canvas%
      (class (make-searchable-canvas% mred:frame-title-canvas%) args
	(inherit get-media frame)
	(rename [super-edit-modified edit-modified])
	(public
	  [edit-renamed
	   (lambda (name)
	     (send frame update-save-message name))]
	  [edit-modified
	   (lambda (mod?)
	     (send frame update-save-button mod?)
	     (super-edit-modified mod?))])
	(sequence
	  (mred:debug:printf 'super-init "before drscheme:frame::get-canvas%")
	  (apply super-init args)
	  (mred:debug:printf 'super-init "after drscheme:frame::get-canvas%")
	  (let ([m (get-media)])
	    (send frame set-save-init-shown?
		  (and (not (null? m)) (send m modified?)))))))

    (define super-frame% (mred:make-searchable-frame%
			  (mred:make-info-frame%
			   drscheme:frame:frame%)))
    (define frame%
      (class super-frame% (filename snip [show? #t])
	(inherit get-canvas get-edit imports-panel
		 set-title-prefix show-menu
		 show menu-bar% make-menu
		 active-edit active-canvas panel update-info
		 file-menu file-menu:open-id file-menu:new-id file-menu:save-id 
		 file-menu:save-as-id file-menu:revert-id file-menu:print-id)
	(rename [super-make-menu-bar make-menu-bar]
		[super-update-shown update-shown]
		[super-on-close on-close])
	(public
	  [definitions-id #f]
	  [interactions-id #f]

	  [name-message #f]
	  [save-button #f]
	  [save-init-shown? #f]
	  [set-save-init-shown? (lambda (x) (set! save-init-shown? x))])

	(public
	  [canvas-show-mode #f]
	  [allow-split? #f]
	  [forced-quit? #f])

	(private
	  [search-canvas #f])
	(public
	  [make-searchable
	   (lambda (canvas)
	     (update-info)
	     (set! search-canvas canvas))]
	  [get-edit-to-search
	   (lambda ()
	     (if search-canvas
		 (send search-canvas get-media)
		 (get-edit)))]
	  [get-info-edit
	   (lambda ()
	     (get-edit-to-search))])

	(private [was-locked? #f])
	(public
	  [disable-evaluation
	   (lambda ()
	     (send execute-button enable #f)
	     (set! was-locked? (ivar definitions-edit locked?))
	     (send definitions-edit lock #t)
	     (send interactions-edit lock #t))]
	  [enable-evaluation
	   (lambda ()
	     (send execute-button enable #t)
	     (send definitions-edit lock was-locked?)
	     (send interactions-edit lock #f))])

	(public
	  [update-save-button
	   (lambda (mod?)
	     (if save-button
		 (send save-button show mod?)
		 (set! save-init-shown? mod?)))]
	  [update-save-message
	   (lambda (name)
	     (when save-button
	       (let ([msg (make-object 
			   mred:message% top-panel
			   (if (null? name)
			       "Untitled" 
			       (or (mzlib:file@:file-name-from-path name)
				   "Untitlesd")))])
		 (set! name-message msg)
		 (send top-panel change-children
		       (lambda (l) (build-top-panel-children))))))]
	  [get-canvas% (lambda () (drscheme:parameters:current-definitions-canvas%))]
	  [ensure-interactions-shown
	   (lambda ()
	     (unless (send show-menu checked? interactions-id)
	       (send show-menu check interactions-id #t)
	       (update-shown)))])
	
	(public
	  [get-edit% (lambda () (drscheme:parameters:current-definitions-edit%))]
	  [still-untouched?
	   (lambda ()
	     (and (= (send definitions-edit last-position) 0)
		  (not (send definitions-edit modified?))
		  (null? (send definitions-edit get-filename))))]
	  [change-to-file
	   (lambda (name)
	     (cond
	       [(and name (file-exists? name))
		(send definitions-edit load-file name)]
	       [name
		(send definitions-edit set-filename name)]
	       [else (send definitions-edit clear)])
	     (send definitions-canvas set-focus))])
	
	(public
	  [file-menu:print-string "Definitions"]
	  [file-menu:print-transcript-id #f]
	  [file-menu:between-print-and-close
	   (lambda (file-menu)
	     (set! file-menu:print-transcript-id
		   (send file-menu append-item "Print Interactions..."
			 (lambda () (send interactions-edit print '()))))
	     (send file-menu append-separator))]
	  [id->child
	   (lambda (id)
	     (cond
	       [(= id interactions-id) interactions-canvas]
	       [(= id definitions-id) definitions-canvas]
	       [else imports-panel]))]
	  [update-shown
	   (lambda ()
	     (super-update-shown)
	     (send panel change-children
		   (lambda (l)
		     (cons (if (send show-menu checked? definitions-id)
			       top-panel
			       scheme-only-panel)
			   (mzlib:function@:foldl
			    (lambda (id sofar)
			      (if (send show-menu checked? id)
				  (cons (id->child id) sofar)
				  sofar))
			    null
			    (list interactions-id definitions-id)))))

	     (send interactions-edit scroll-to-position 
		   (send interactions-edit get-end-position)
		   #f
		   (send interactions-edit get-start-position)
		   1)
	     (send definitions-edit scroll-to-position 
		   (send definitions-edit get-end-position)
		   #f
		   (send definitions-edit get-start-position)	
		   1)
	     (map 
	      (lambda (id)
		(send file-menu enable id (send show-menu checked? definitions-id)))
	      (list file-menu:open-id
		    file-menu:new-id
		    file-menu:save-id
		    file-menu:save-as-id 
		    file-menu:revert-id
		    file-menu:print-id))
	     (send file-menu enable file-menu:print-transcript-id 
		   (send show-menu checked? interactions-id)))]

	  [make-menu-bar
	   (lambda ()
	     (let ([mb (super-make-menu-bar)]
		   [scheme-menu (make-menu)]
		   [language-menu (make-menu)])
	       
	       (send* mb
		      (append scheme-menu "S&cheme")
		      (append language-menu "&Language"))

	       (drscheme:language:fill-language-menu language-menu)

	       (send* scheme-menu
		 (append-item "Execute"
			      (lambda ()
				(execute-callback))
			      "Execute the definitions window" #f "t")
		 (append-item "Break"
			      (lambda ()
				(send interactions-edit break))
			      "Break the current evaluation" #f "b")
		 (append-separator)
		 (append-item "&Indent" 
			      (lambda () 
				(send (ivar (active-edit) mode) 
				      tabify-selection (active-edit))))
		 (append-item "Indent &All"
			      (lambda ()
				(send (ivar (active-edit) mode) tabify-all (active-edit)))
			      "" #f "i")
		 (append-item "&Comment Out"
			      (lambda ()
				(send (ivar (active-edit) mode) 
				      comment-out-selection (active-edit))))
		 (append-item "&Uncomment"
			      (lambda ()
				(send (ivar (active-edit) mode)
				      uncomment-selection (active-edit)))))
	       (when (mred:get-preference 'drscheme:use-setup?)
		 (send* scheme-menu
			(append-separator)
			(append-item "Setup H&omework..." 
				     (lambda () (drscheme:setup:do-setup "hw")))
			(append-item "Setup &Lab..."
				     (lambda () (drscheme:setup:do-setup "lab")))))

	       (set! definitions-id
		     (send show-menu append-item "&Definitions"
			   (lambda () (update-shown))
			   "Show the definitions in this unit"
			   #t))
	       (set! interactions-id
		     (send show-menu append-item "&Interactions"
			   (lambda () (update-shown))
			   "Show the interactions with this unit"
			   #t))
	       mb))]

	  [on-close
	   (lambda ()
	     (remove-library-callback)
	     (when (eq? this created-frame)
	       (set! created-frame #f))
             (when snip
	       (send snip on-close-frame (send definitions-edit get-filename)))
	     (send interactions-edit shutdown)
	     (super-on-close))]
	  
	  [running? #t]; is this necessary?
	  
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
	  [execute-callback
	   (lambda ()
	     (let* ([definitions-edit definitions-edit]
		    [interactions-edit interactions-edit])
	       (ensure-interactions-shown)
	       (send interactions-canvas set-focus)
	       (dynamic-wind
		(lambda () (send interactions-edit begin-edit-sequence))
		(lambda ()
		  (send interactions-edit reset-console)
		  (send interactions-edit do-many-buffer-evals
			definitions-edit 0
			(send definitions-edit last-position)))
		(lambda () (send interactions-edit end-edit-sequence)))))])
	(sequence
	  (mred:debug:printf 'super-init "before drscheme:unit-frame%")
	  (super-init (cond 
			[snip (ivar snip name)]
			[filename filename]
			[else "Untitled"])
		      snip)
	  (mred:debug:printf 'super-init "after drscheme:unit-frame%"))
	
	(private
	  [top-panel (make-object mred:horizontal-panel% panel)])
	
	(public
	  [definitions-canvas (get-canvas)]
	  [definitions-edit (get-edit)]
	  [interactions-canvas (make-object (drscheme:parameters:current-interactions-canvas%) panel)]
	  [interactions-edit (make-object (drscheme:parameters:current-interactions-edit%))])
	
	(sequence
	  (send definitions-edit set-mode (make-object mred:scheme-mode%))
	  (send* interactions-canvas 
	    ;(scroll-with-bottom-base #t)
	    (set-media interactions-edit))
	  (send interactions-edit set-auto-set-wrap #t)
	  (change-to-file filename))

	(sequence
	  (set! name-message
		(make-object mred:message% top-panel
			     (let ([fn (send definitions-edit get-filename)])
			       (cond
				 [(null? fn) "Untitled"]
				 [(mzlib:file@:file-name-from-path fn)]
				 [else "Untitled"]))))


	  (set! save-button
		(make-object mred:button% 
			     top-panel
			     (lambda args
			       (let* ([edit definitions-edit])
				 (unless (or (null? edit) (not edit))
				   (send edit save-file)
				   (send definitions-canvas set-focus))))
			     save-bitmap)))
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
		       (lambda args
			 (send interactions-edit break)
			 (ensure-interactions-shown)
			 (send (send interactions-edit get-canvas) set-focus))
		       break-bitmap)]
	 [scheme-only-help
	  (make-object mred:button% scheme-only-panel
		       (lambda args (do-help))
		       help-bitmap)])
	 (sequence
	  (send panel delete-child scheme-only-panel)

	  (send save-button show save-init-shown?))
	
	 (sequence
	  (set! execute-button
	    (make-object mred:button% button-panel
			 (lambda (button evt) (execute-callback))
			 execute-bitmap))
	  (set! stop-execute-button
		(make-object mred:button% button-panel 
			     (lambda args
			       (send interactions-edit break)
			       (ensure-interactions-shown)
			       (send (send interactions-edit get-canvas) set-focus))
			     break-bitmap))
	 (make-object mred:button% button-panel
		       (lambda args (do-help))
		       help-bitmap)
	  
	  (send scheme-only-panel stretchable-in-y #f)
	  (send button-panel stretchable-in-y #f)
	  (send button-panel stretchable-in-x #f) 
	  (send top-panel stretchable-in-y #f))
	  
	 (private
	   [remove-library-callback
	    (mred:add-preference-callback
	     'drscheme:library-file
	     (let ([last-one (mred:get-preference 'drscheme:library-file)])
	       (lambda (p v)
		 (unless (or (and (not last-one) (not v))
			     (and last-one v
				  (string=? v last-one)))
		   (set! last-one v)
		   (set! scheme-only-library-msg
			 (make-library-name-msg scheme-only-panel v))
		   (set! library-msg (make-library-name-msg top-panel v))
		   (send scheme-only-panel change-children
			 (lambda (l) (list scheme-only-library-msg
					   scheme-only-space
					   scheme-only-stop-executing
					   scheme-only-help)))
		   (send top-panel change-children 
			 (lambda (l) (build-top-panel-children)))))))])
		     
	(private
	  [build-top-panel-children
	   (lambda ()
	     (list name-message save-button space1 library-msg space2 button-panel))])
	
	(sequence
	  (send show-menu check definitions-id #t)
	  (send show-menu check interactions-id #t)

	  (send interactions-edit initialize-console)
	  (send interactions-edit enable-autoprompt)
	  (send interactions-edit insert-prompt)
	  (send interactions-edit clear-undos)

	  (update-shown)

	  (set-title-prefix "DrScheme")

	  (send definitions-canvas set-focus)
	  (unless created-frame
	    (set! created-frame this))
	  (mred:debug:printf 'super-init "drscheme:frame% finished ivars~n"))))

    (define snip%
      (let ([f% frame%])
      (class wx:snip% (n fn)
	(inherit get-admin set-snipclass)
	(rename [super-get-flags get-flags])
	(private
	 [invalidate-to
	  (lambda (c)
	    (let-values ([(left1 top1 right1 bottom1) (get-pos)]
			 [(left2 top2 right2 bottom2) (send c get-pos)])
	      (let ([media (send (get-admin) get-media)]
		    [top (min top1 top2)]
		    [left (min left1 left2)]
		    [bottom (max bottom1 bottom2)]
		    [right (max right1 right2)])
		(send media invalidate-bitmap-cache
		      left top (- right left) (- bottom top)))))])
	(public
	 [frame% f%]
	 [this% snip%]
	 [width 70] [set-width (lambda (v) (set! width v))]
	 [height 30] [set-height (lambda (v) (set! height v))]
	 [parents null]
	 [children null])
	
	(private
	 ;; frame is either a filename or a frame object.
	 [frame fn])
	(public
	 ;; this is called with a filename when the frame is closed
	 [on-close-frame
	  (lambda (s)
	    (set! frame s))]
	 [open
	  (lambda ()
	    (if (is-a? frame wx:frame%)
		(send frame show #t)
		(set! frame (make-object frame% frame #f this))))]
	 
	 [get-pos
	  (lambda ()
	    (let ([media (send (get-admin) get-media)])
	      (let ([lbox (box 0)]
		    [rbox (box 0)]
		    [tbox (box 0)]
		    [bbox (box 0)])
		(send media get-snip-location this lbox tbox #f)
		(send media get-snip-location this rbox bbox #t)
		(values (unbox lbox) (unbox tbox)
			(unbox rbox) (unbox bbox)))))]
	 [add-child
	  (lambda (c)
	    (if (eq? c this)
		(wx:message-box "A unit cannot be a parent or child of itself.")
		(unless (member c children)
		  (set! children (cons c children))
		  (invalidate-to c))))]
	 [add-parent
	  (lambda (c)
	    (unless (or (member c parents)
			(eq? c this))
	      (set! parents (cons c parents))
	      (invalidate-to c)
	      (when (object? frame)
		(send frame on-change-imports))))]
	 [name n]
	 [copy (lambda () (make-object this% name frame))]
	 [write (lambda (s) 
		  (let* ([filename
			  (if (object? wx:frame%)
			      (send (ivar frame edit) get-filename)
			      frame)]
			 [string (format "~s" (list name filename))])
		    (send s put string)))]
	 [resize
	  (lambda (w h)
	    (set! width w)
	    (set! height h)
	    (send (get-admin) resized this #t)
	    #t)]
	 [draw
	  (lambda (dc x y left top right bottom dx dy draw-caret)
	    '(printf "x: ~a y:~a width: ~a height: ~a~n" x y width height)
	    (let ([xbox (box 0)]
		  [ybox (box 0)]
		  [wbox (box 0)]
		  [hbox (box 0)])
	      (send dc get-clipping-region xbox ybox wbox hbox)
	      (let* ([old-left (unbox xbox)]
		     [old-top (unbox ybox)]
		     [old-width (unbox wbox)]
		     [old-height (unbox hbox)])
		'(printf "old-left: ~a old-top: ~a old-width: ~a old-height: ~a~n"
			old-left old-top old-width old-height)
		(if (< old-width 0)
		    '(send dc set-clipping-region x y width height)
		    (let* ([old-right (+ left old-width)]
			   [old-bottom (+ top old-height)]
			   [new-left (max old-left x)]
			   [new-top (max old-top y)]
			   [new-width (- (min old-right (+ x width)) new-left)]
			   [new-height (- (min old-bottom (+ y height)) new-top)])
		      '(send dc set-clipping-region 
			    new-left new-top new-width new-height)
		      '(printf "new-left: ~a new-top: ~a new-width: ~a new-height: ~a~n"
			      new-left new-top new-width new-height)))
		(send dc draw-rectangle x y width height)
		(let ([h (box 0)]
		      [w (box 0)])
		  (send dc get-text-extent name w h)
		  (send dc draw-text name 
			(+ x (/ (- width (unbox w)) 2))
			(+ y (/ (- height (unbox h)) 2))))
		(if (< old-width 0)
		    (send dc destroy-clipping-region)
		    (send dc set-clipping-region 
			  (unbox xbox) (unbox ybox) (unbox wbox) (unbox hbox))))))]
	 [get-extent
	  (opt-lambda (dc x y
			  [width-box null] [height-box null]
			  [descent-box null] [space-box null]
			  [lspace-box null] [rspace-box null])
	    (let ([size
		   (lambda (v)
		     (lambda (x)
		       (unless (null? x)
			 (set-box! x v))))])
	      ((size width) width-box)
	      ((size height) height-box)
	      (for-each (size 3) 
			(list descent-box space-box lspace-box rspace-box))))]
	 [snipclassq unit-snipclass])
	(sequence
	  (super-init)
	  (set-snipclass snipclassq)))))

    (define snip-class%
      (let ([s% snip%])
	(class wx:snip-class% args
	  (inherit set-classname set-version)
	  (public
	    [snip% s%] 
	    [classname "drscheme:unit:snip%"]
	    [version 3]
	    [write-header
	     (lambda (p)
	       (send p put "h"))]
	    [read-header
	     (lambda (p)
	       (send p get-string (box 0)))]
	    [read
	     (lambda (p)
	       (let ([l (mzlib:string@:read-string (send p get-string null))])
		 (make-object snip% (car l) (cadr l))))])
	  (sequence
	    (apply super-init args)
	    (set-classname classname)
	    (set-version version)
	    (send (wx:get-the-snip-class-list) add this)))))

    (define unit-snipclass (make-object snip-class%))

    (define created-frame #f)

    (mred:insert-format-handler 
     "Units"
     (list "ss" "scm" "sch" "mredrc")
     (opt-lambda (name)
       (if (and created-frame
		(send created-frame still-untouched?))
	   (send created-frame change-to-file name)
	   (let ([f (make-object (drscheme:parameters:current-frame%) name #f)])
	     (send f show #t)
	     f)))))