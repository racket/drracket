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
	  [drscheme:get/extend : drscheme:get/extend^]
	  [drscheme:face : drscheme:face^])
  
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
				   (collection-path "icons")
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
	   (build-path (collection-path "doc")
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
  
  (define definitions-edit%
    (class mred:backup-autosave-edit% (unit . args)
      (rename [super-after-insert after-insert]
	      [super-after-delete after-delete])
      (public
	[needs-execution? #f]
	[library-changed
	 (lambda ()
	   (set! needs-execution? #t))]
	[just-executed
	 (lambda ()
	   (set! needs-execution? #f)
	   (set! already-warned? #f))]
	[already-warned? #f]
	[already-warned
	 (lambda ()
	   (set! already-warned? #t))]
	[after-insert
	 (lambda x
	   (set! needs-execution? #t)
	   (apply super-after-insert x))]
	[after-delete
	 (lambda x
	   (set! needs-execution? #t)
	   (apply super-after-delete x))])
      (sequence
	(apply super-init args))))
  
  (define super-frame% (mred:make-searchable-frame%
			(mred:make-edit-info-frame%
			 (mred:make-info-frame%
			  (mred:make-file-frame%
			   (drscheme:frame:make-frame%
			    mred:simple-menu-frame%))))))
  (define frame%
    (class* super-frame% (drscheme:face:unit-frameI) (unit)
      (inherit get-canvas
	       set-title-prefix show-menu
	       show menu-bar% make-menu
	       active-edit active-canvas panel update-info
	       file-menu file-menu:open-id file-menu:new-id file-menu:save-id 
	       file-menu:save-as-id file-menu:revert-id file-menu:print-id)
      (rename [super-make-menu-bar make-menu-bar]
	      [super-update-shown update-shown]
	      [super-do-close do-close])
      (public
	[definitions-id #f]
	[interactions-id #f]
	[imports-id #f]
	
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
	[get-canvas% (lambda () (drscheme:get/extend:get-definitions-canvas%))]
	[ensure-interactions-shown
	 (lambda ()
	   (unless (send show-menu checked? interactions-id)
	     (send show-menu check interactions-id #t)
	     (update-shown)))])
      
      (public
	[get-edit
	 (lambda ()
	   (send unit get-buffer))]
	[get-edit% (lambda () (drscheme:get/extend:get-definitions-edit%))]
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
	[file-menu:between-open-and-save
	 (lambda (file-menu)
	   (send file-menu append-separator))]
	[file-menu:save-string "Definitions"]
	[file-menu:save-as-string "Definitions"]
	[file-menu:between-save-and-print
	 (lambda (file-menu)
	   (send file-menu append-item
		 "Save Interactions"
		 (lambda () (send interactions-edit save-file)))
	   (send file-menu append-item
		 "Save Interactions As..."
		 (lambda () 
		   (let ([file (mred:put-file)])
		     (when file
		       (send interactions-edit save-file 
			     file wx:const-media-ff-std)))))
	   (send file-menu append-item
		 "Save Interactions As Text..."
		 (lambda () 
		   (let ([file (mred:put-file)])
		     (when file
		       (send interactions-edit save-file 
			     file wx:const-media-ff-text)))))
	   (send file-menu append-separator))]
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
	     [(= id imports-id) imports-panel]
	     [else (error 'id->child "unknown id: ~a" id)]))]
	[update-shown
	 (lambda ()
	   (super-update-shown)
	   (send panel change-children
		 (lambda (l)
		   (cons top-panel
			 (mzlib:function@:foldl
			  (lambda (id sofar)
			    (if (send show-menu checked? id)
				(cons (id->child id) sofar)
				sofar))
			  null
			  (list interactions-id definitions-id imports-id)))))
	   
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
	     
	     (set! imports-id 
		   (send show-menu append-item
			 "&Imports"
			 (lambda () (update-shown))
			 "Show the imports to this unit"
			 #t))
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
	
	[do-close
	 (lambda ()
	   (remove-library-callback)
	   (when (eq? this created-frame)
	     (set! created-frame #f))
	   (send unit frame-closed)
	   (send interactions-edit shutdown)
	   (super-do-close))]
	
	[running? #t]; is this necessary?
	[execute-callback
	 (lambda ()
	   (let* ([definitions-edit definitions-edit]
		  [interactions-edit interactions-edit])
	     (ensure-interactions-shown)
	     (send definitions-edit just-executed)
	     (send interactions-canvas set-focus)
	     (dynamic-wind
	      (lambda () (send interactions-edit begin-edit-sequence))
	      (lambda ()
		(send interactions-edit reset-console)
		(send interactions-edit do-many-buffer-evals
		      definitions-edit 0
		      (send definitions-edit last-position)))
	      (lambda () (send interactions-edit end-edit-sequence)))))])
  
      (public
	[after-change-name void]
	[after-add-import (lambda () (update-imports))]
	[after-add-export (lambda () (update-imports))]
	[after-remove-import (lambda () (update-imports))]
	[after-remove-export (lambda () (update-imports))]
	[get-unit (lambda () unit)])

      (sequence
	(mred:debug:printf 'super-init "before drscheme:unit-frame%")
	(super-init unit)
	(mred:debug:printf 'super-init "after drscheme:unit-frame%"))
      
      (private
	[top-panel (make-object mred:horizontal-panel% panel)])
      
      (public
	[imports-panel (make-object mred:horizontal-panel% panel)]
	[imports-message
	 (make-object mred:message% imports-panel "imports")]
	[imports-space
	 (make-object mred:horizontal-panel% imports-panel)]
	[update-imports
	 (lambda ()
	   (let ([make-message
		  (lambda (unit)
		    (make-object mred:message% imports-panel
				 (send unit get-name)))])
	     (send imports-panel change-children
		   (lambda (l) (list imports-message imports-space)))
	     (for-each make-message
		       (send unit get-imports))))]
	[add-import (lambda (import) (update-imports))]
	[remove-import (lambda (import) (update-imports))])
      
      (public
	[definitions-canvas (get-canvas)]
	[definitions-edit (get-edit)]
	[interactions-canvas (make-object 
			      (drscheme:get/extend:get-interactions-canvas%)
			      panel)]
	[interactions-edit (make-object 
			    (drscheme:get/extend:get-interactions-edit%))])

      (sequence
	(send definitions-edit set-mode (make-object mred:scheme-mode%))
	(send* interactions-canvas 
	  ;(scroll-with-bottom-base #t)
	  (set-media interactions-edit))
	(send interactions-edit set-auto-set-wrap #t)

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
			   save-bitmap))
	(update-save-button #f))
      (private 
	[make-library-name-msg
	 (lambda (panel n)
	   (make-object mred:message% panel 
			(if n
			    (let-values ([(base name must-be-dir) (split-path n)])
			      name)
			    "")))]
	[library-msg (make-library-name-msg
		      top-panel
		      (mred:get-preference 'drscheme:library-file))])
      
      (public
	[stop-execute-button (void)]
	[execute-button (void)]
	[button-panel (make-object mred:horizontal-panel% top-panel)])
      
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
	
	(send imports-panel stretchable-in-y #f)
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
		(set! library-msg (make-library-name-msg top-panel v))
		(send definitions-edit library-changed)
		(send top-panel change-children 
		      (lambda (l) (build-top-panel-children)))))))])
      
      (private
	[build-top-panel-children
	 (lambda ()
	   (list name-message save-button
		 library-msg
		 button-panel))])
      
      (sequence
	(send show-menu check imports-id #f)
	(send show-menu check definitions-id #t)
	(send show-menu check interactions-id 
	      (or (ivar interactions-edit repl-initially-active?)
		  (mred:get-preference 'drscheme:repl-always-active)))
	
	(send interactions-edit initialize-console)
	(send interactions-edit enable-autoprompt)
	(send interactions-edit insert-prompt)
	(send interactions-edit clear-undos)
	
	(update-imports)
	(update-shown)
	
	(set-title-prefix "DrScheme")
	
	(send definitions-canvas set-focus)
	(cond
	  [(eq? created-frame 'nothing-yet)
	   (set! created-frame this)]
	  [created-frame
	   (set! created-frame #f)]
	  [else (void)])
	(mred:debug:printf 'super-init "drscheme:frame% finished ivars~n"))))
  
  (define snip%
    (class* mred:node-snip% (drscheme:face:unit-snipI) (unit)
      (inherit get-admin set-snipclass invalidate-to)
      (rename [super-get-flags get-flags])
      (public
	[after-change-name (lambda () (invalidate-to this))]
	[after-add-import void]
	[after-add-export void]
	[after-remove-import void]
	[after-remove-export void]
	[get-unit (lambda () unit)])

      (rename
	[super-release-from-owner release-from-owner])
      (public
	[release-from-owner
	 (lambda ()
	   (and (super-release-from-owner)
		(send unit remove-snip this)))]
	[copy (lambda () (send unit create-snip))]
	[snipclass unit-snipclass])
      (sequence
	(super-init)
	(set-snipclass snipclass))))

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
  
  (define next-untitled-name
    (let ([n 0])
      (lambda ()
	(set! n (add1 n))
	(format "<untitled ~a>" n))))
    
  (define unit%
    (let ([s% snip%])
      (class* () (drscheme:face:unitI) (filename-arg . collections-arg)
	(private
	  [filename filename-arg]
	  [collections collections-arg])
	(public
	  [get-filename (lambda () filename)]
	  [get-collections (lambda () collections)])
	
	(private
	  [name (next-untitled-name)])
	(public
	  [get-name (lambda () name)]
	  [set-name (lambda (n) (set! name n))])
	
	(private
	  [imports null]
	  [exports #f])
	(public
	  [get-imports (lambda () imports)]
	  [get-exports (lambda () exports)]
	  [add-import 
	   (lambda (i)
	     (set! imports (cons i imports))
	     (update-displays (lambda (d) (send d after-add-import i))))]
	  [remove-import
	   (lambda (i)
	     (when (member i imports)
	       (set! imports (mzlib:function@:remq i imports))))]
	  [add-export 
	   (lambda (e)
	     (set! exports (cons e exports)))]
	  [remove-export
	   (lambda (e)
	     (when (member e exports)
	       (set! exports (mzlib:function@:remq e exports))))])
	
	(private
	  [snips null]
	  [frame #f]
	  [update-displays
	   (lambda (f)
	     (for-each f snips)
	     (when frame
	       (f frame)))])
	(public
	  [frame% (drscheme:get/extend:get-unit-frame%)]
	  [snip% s%]
	  [get-snips (lambda () snips)]
	  [get-frame (lambda () frame)]
	  [frame-closed (lambda () (set! frame #f))]
	  [remove-snip (lambda (snip)
			 (set! snips (mzlib:function@:remq snip snips)))] 
	  [create-frame (opt-lambda ([show? #t])
			  (unless frame
			    (set! frame (make-object frame% this))
			    (when show?
			      (send frame show #t))
			    frame))]
	  [create-snip 
	   (lambda ()
	     (let ([new (make-object snip% this)])
	       (set! snips (cons new snips))
	       new))])
	
	(private
	  [buffer #f])
	(public
	  [buffer% (drscheme:get/extend:get-definitions-edit%)]
	  [get-buffer (lambda () buffer)]
	  [set-filename
	   (lambda (fn . cn)
	     (set! filename fn)
	     (set! collections cn)
	     (when fn
	       (let ([collections (get-collections)])
		     (send buffer load-file
			   (if (null? collections)
			       (get-filename)
			       (build-path (apply collection-path collections)
					   (get-filename)))))))])
	(sequence
	  (super-init)
	  (set! buffer (make-object buffer% this))
	  (apply set-filename filename-arg collections-arg)))))

  (define make-unit
    (lambda (filename . collections)
      (apply make-object unit% filename collections)))

  (define created-frame 'nothing-yet)
  
  (mred:set-preference-default 'drscheme:open-all-files-in-scheme-mode
			       #t
			       boolean?)
  (mred:insert-format-handler 
   "Units"
   (lambda (filename)
     (or (mred:get-preference 'drscheme:open-all-files-in-scheme-mode)
	 (let ([filename-ext (mzlib:file@:filename-extension filename)])
	   (and filename-ext
		(ormap (lambda (extension)
			 (string=? filename-ext extension))
		       (list "ss" "scm" "sch" "mredrc"))))))
   (lambda (name)
     (if (and created-frame
	      (not (eq? created-frame 'nothing-yet)) 
	      (send created-frame still-untouched?))
	 (send created-frame change-to-file name)
	 (let* ([unit (make-unit name)]
		[f (begin (send unit create-frame) 
			  (send unit get-frame))])
	   (send f show #t)
	   f)))))