
(unit/sig drscheme:unit^
  (import [mred : mred-interfaces^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:compound-unit : drscheme:compound-unit^]
	  [drscheme:frame : drscheme:frame^]
	  [drscheme:edit : drscheme:edit^]
	  [drscheme:rep : drscheme:rep^]
	  [drscheme:language : drscheme:language^]
	  [drscheme:get/extend : drscheme:get/extend^]
	  [drscheme:face : drscheme:face^]
	  [drscheme:graph : drscheme:graph^])
  
  (define (make-bitmap button-name)
    (lambda (area-container-window)
      (let*-values ([(outside-margin) 2]
		    [(middle-margin) 3]

		    [(text) (let ([capd (string-copy button-name)])
			      (string-set! capd 0 (char-upcase (string-ref capd 0)))
			      capd)]
		    [(filename) (build-path
				 (collection-path "icons")
				 (string-append button-name ".bmp"))]
		    [(font) (send area-container-window get-control-font)]
		    [(img-bitmap-dc img-width img-height)
		     (let ([mdc (make-object mred:bitmap-dc%)]
			   [q (make-object mred:bitmap% filename 'bmp)])
		       (if (send q ok?)
			   (begin (send mdc set-bitmap q)
				  (values mdc
					  (send q get-width)
					  (send q get-height)))
			   (let ([b (make-object mred:bitmap% 1 1)])
			     (send mdc set-bitmap b)
			     (send mdc clear)
			     (values mdc 0 0))))]
		    [(width height descent leading)
		     (begin (send img-bitmap-dc set-scale 1 1)
			    (send img-bitmap-dc get-text-extent text font))]
		    [(new-width) (+ outside-margin
				    img-width
				    middle-margin
				    (unbox width)
				    outside-margin)]
		    [(new-height) (+ outside-margin
				     (max img-height
					  (unbox height))
				     outside-margin)]
		    [(bitmap-dc) (make-object mred:bitmap-dc%)]
		    [(new-bitmap) (make-object mred:bitmap% new-width new-height -1)])
	(send* bitmap-dc
	       (select-object new-bitmap)
	       (set-scale 1 1)
	       (set-font font)
	       (clear)
	       (set-font font)
	       (draw-text text (+ outside-margin img-width middle-margin)
			  (- (/ new-height 2) (/ (unbox height) 2))))
	(unless (or (= img-width 0)
		    (= img-height 0))
	  (send bitmap-dc draw-bitmap
		(send img-bitmap-dc get-bitmap)
		outside-margin
		(- (/ new-height 2) (/ img-height 2))))
	(send bitmap-dc select-object #f)
	new-bitmap)))
  
  (define make-execute-bitmap (make-bitmap "execute"))
  (define make-save-bitmap (make-bitmap "save"))
  (define make-break-bitmap (make-bitmap "break"))
  
  ;; this is the old definition of the interactions canvas.
  ;; It should be integrated into fw:canvas:wide-snip% 
  ;; becuase it uses a better algorithm to find the snip
  ;; wdie widths.
  '(class-asi mred:wide-snip-canvas% ; to match rep-new.ss, inherit from wrapping-canvas% 
     (inherit get-media)
     (rename [super-on-size on-size]
	     [super-set-media set-media])
     (public)
     (private
       [snips null]
       [autowrap-snips? (fw:preferences:get 'mred:auto-set-wrap?)]
       [update-snip-size
	(lambda (s)
	  (if (is-a? s mred:editor-snip%)
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
			     (if (is-a? containing-admin mred:editor-snip-editor-admin%)
				 (+ (unbox snip-x-pos)
				    (loop (send containing-admin get-snip)))
				 (unbox snip-x-pos)))))]
		     [outer-snip (let loop ([snip s])
				   (let ([containing-admin
					  (send (send (send snip get-admin)
						      get-media) get-admin)])
				     (if (is-a? containing-admin mred:editor-snip-editor-admin%)
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
		(when (is-a? s mred:editor-snip%)
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
  
  (define interactions-canvas% (make-searchable-canvas% fw:canvas:wide-snip%))
  
  (define definitions-canvas%
    (class (make-searchable-canvas% mred:editor-canvas%) args
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
	(apply super-init args)
	(let ([m (get-media)])
	  (send frame set-save-init-shown?
		(and (not (null? m)) (send m modified?)))))))
  
  (define definitions-edit%
    (class fw:scheme:text% (unit . args)
      (rename [super-after-insert after-insert]
	      [super-after-delete after-delete])
      (public
	[needs-execution? #f]
	[library-changed
	 (lambda ()
	   (set! needs-execution? #t))]
	[language-changed
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
  
  (define super-frame% (drscheme:frame:mixin fw:frame:text-info-file%))
  
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
	      [super-do-close do-close]
	      [help-menu:super-insert-items help-menu:insert-items])
      (public
	[definitions-id #f]
	[interactions-id #f]
	;[imports-id #f]
	
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
      
      (private [was-locked? #f]
	       [execute-menu-item #f]
	       [scheme-menu #f])
      (public
	[disable-evaluation
	 (lambda ()
	   (when (and scheme-menu execute-menu-item)
	     (send scheme-menu enable execute-menu-item #f))
	   (send execute-button enable #f)
	   (set! was-locked? (ivar definitions-edit locked?))
	   (send definitions-edit lock #t)
	   (send interactions-edit lock #t))]
	[enable-evaluation
	 (lambda ()
	   (when (and scheme-menu execute-menu-item)
	     (send scheme-menu enable execute-menu-item #t))
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
			     (or (mzlib:file:file-name-from-path name)
				 "Untitlesd")))])
	       (set! name-message msg)
	       (send top-panel change-children
		     (lambda (l) (build-top-panel-children))))))]
	[get-canvas% (lambda () (drscheme:get/extend:get-definitions-canvas%))]
	[ensure-interactions-shown
	 (lambda ()
	   (when (hidden? show-menu interactions-id)
	     (toggle-show/hide show-menu interactions-id)
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

      (private
       [drscheme-manual 
	"PLT DrScheme: Programming Environment Manual"]
       [hidden?
	(lambda (menu id)
	  (let ([item (send menu get-label id)])
	    (and (string? item)
		 (>= (string-length item) 4)
		 (string=? (substring item 0 4) "Show"))))]
	[save-as-text-from-edit
	 (lambda (win)
	   (let ([file (mred:put-file)])
	     (when file
		   (send win save-file file 'text))))])

      (public
	[toggle-show/hide
	 (lambda (menu id)
	   (let ([label (send menu get-label id)])
	     (when (and (string? label)
			(>= (string-length label) 4))
	       (let ([new-front
		      (if (string=? "Hide" (substring label 0 4))
			  "Show"
			  "Hide")])
		 (let loop ([i (string-length new-front)])
		   (unless (zero? i)
		     (string-set! label (sub1 i) 
				  (string-ref new-front (sub1 i)))
		     (loop (sub1 i))))
		 (send menu set-label id label)))))])

      (public
	[file-menu:between-open-and-save
	 (lambda (file-menu)
	   (send file-menu append-separator))]
	[file-menu:save-string "Definitions"]
	[file-menu:save-as-string "Definitions"]
	[file-menu:between-save-and-print

	 (lambda (file-menu)

	   (send file-menu append-item
		 "Save Definitions As Text..."
		 (lambda ()
		   (save-as-text-from-edit definitions-edit)))
	   (send file-menu append-item
		 "Save Interactions"
		 (lambda () (send interactions-edit save-file)))
	   (send file-menu append-item
		 "Save Interactions As..."
		 (lambda () 
		   (let ([file (mred:put-file)])
		     (when file
		       (send interactions-edit save-file 
			     file 'standard)))))
	   (send file-menu append-item
		 "Save Interactions As Text..."
		 (lambda ()
		   (save-as-text-from-edit interactions-edit)))
	   (send file-menu append-separator)
	   (send file-menu append-item
		 "Show Interactions History"
		 drscheme:rep:show-interactions-history)
	   (send file-menu append-separator))]
	[file-menu:print-string "Definitions"]
	[file-menu:print-transcript-id #f]
	[file-menu:between-print-and-close
	 (lambda (file-menu)
	   (set! file-menu:print-transcript-id
		 (send file-menu append-item "Print Interactions..."
		       (lambda () (send interactions-edit print '()
					#t 
					#t
					(fw:preferences:get 'mred:print-output-mode)))))
	   (send file-menu append-separator))]

	[help-menu:compare

	 ; override default comparison so drscheme-manual is first

	 (lambda (s1 s2)
	   (if (string=? s1 drscheme-manual)
	       -1
	       (if (string=? s2 drscheme-manual)
		   1
		   (string-ci<? s1 s2))))]
	[help-menu:insert-items
	    (lambda (items)
	      (if (string=? (caar items) drscheme-manual)
		  (begin
		    (apply (ivar (ivar this help-menu) append-item) 
			   (car items))
		    (send (ivar this help-menu) append-separator)
		    (help-menu:super-insert-items (cdr items)))
		  (help-menu:super-insert-items items)))]
	[id->child
	 (lambda (id)
	   (cond
	     [(= id interactions-id) interactions-canvas]
	     [(= id definitions-id) definitions-canvas]
	     ;[(= id imports-id) imports-panel]
	     [else (error 'id->child "unknown id: ~a" id)]))]
	[update-shown
	 (lambda ()
	   (super-update-shown)
	   (send panel change-children
		 (lambda (l)
		   (cons top-panel
			 (mzlib:function:foldl
			  (lambda (id sofar)
			    (if (hidden? show-menu id)
				sofar
				(cons (id->child id) sofar)))
			  null
			  (list interactions-id definitions-id 
				;imports-id
				)))))
	   (when (ormap (lambda (child)
			  (and (is-a? child mred:editor-canvas%)
			       (not (send child is-focus-on?))))
			(ivar panel children))
	     (let loop ([children (ivar panel children)])
	       (cond
		[(null? children) (void)]
		[else (let ([child (car children)])
			(if (is-a? child mred:editor-canvas%)
			    (send child set-focus)
			    (loop (cdr children))))])))
	   
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
	   (let ([defs-show? (not (hidden? show-menu definitions-id))])
	     (for-each
	      (lambda (id)
		(send file-menu enable id defs-show?))
	      (list file-menu:revert-id
		    file-menu:save-id
		    file-menu:save-as-id 
		    (add1 file-menu:save-as-id) ; Save As Text...
		    file-menu:print-id)))
	   (send file-menu enable file-menu:print-transcript-id 
		 (not (hidden? show-menu interactions-id))))]
	[make-menu-bar
	 (lambda ()
	   (let ([mb (super-make-menu-bar)]
		 [language-menu (make-menu)])

	     (set! scheme-menu (make-menu))

	     (send* mb
	       (append scheme-menu "S&cheme")
	       (append language-menu "&Language"))
	     
	     (drscheme:language:fill-language-menu language-menu)
	     
	     (set! execute-menu-item
		   (send scheme-menu
			 append-item "Execute"
			 (lambda ()
			   (execute-callback))
			 "Restart the program in the definitions window" #f "t"))
	     (send* scheme-menu
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
	     
	     '(set! imports-id 
		   (send show-menu append-item
			 "&Imports"
			 (lambda () 
			   (toggle-show/hide show-menu imports-id)
			   (update-shown))
			 "Show the imports to this unit"
			 #t))
	     (set! definitions-id
		   (send show-menu append-item "Hide &Definitions"
			 (lambda () 
			   (toggle-show/hide show-menu definitions-id)
			   (update-shown))
			 "Show the definitions window"
			 #f
			 "d"))
	     (set! interactions-id
		   (send show-menu append-item 
			 "Show &Interactions"
			 (lambda () 
			   (toggle-show/hide show-menu interactions-id)
			   (update-shown))
			 "Show the interactions window"
			 #f
			 "e"))
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
	   (ensure-interactions-shown)
	   (send definitions-edit just-executed)
	   (send interactions-canvas set-focus)
	   (send interactions-edit reset-console)
	   (send interactions-edit clear-undos)
	   (send interactions-edit do-many-buffer-evals
		 definitions-edit 0
		 (send definitions-edit last-position))
	   (send interactions-edit clear-undos))])
  
      (public
	[after-change-name void]
	[after-add-import (lambda () (update-imports))]
	[after-add-export (lambda () (update-imports))]
	[after-remove-import (lambda () (update-imports))]
	[after-remove-export (lambda () (update-imports))]
	[get-unit (lambda () unit)])

      (sequence
	(super-init unit))
      
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
	(send* interactions-canvas 
	  ;(scroll-with-bottom-base #t)
	  (set-media interactions-edit))
	(send interactions-edit set-auto-set-wrap #t)

	(set! name-message
	      (make-object mred:message% top-panel
			   (let ([fn (send definitions-edit get-filename)])
			     (cond
			       [(null? fn) "Untitled"]
			       [(mzlib:file:file-name-from-path fn)]
			       [else "Untitled"]))))
	
	
	(set! save-button
	      (make-object mred:button% 
			   top-panel
			   (lambda args
			     (let* ([edit definitions-edit])
			       (unless (or (null? edit) (not edit))
				 (send edit save-file)
				 (send definitions-canvas set-focus))))
			   (make-save-bitmap)))
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
		      (fw:preferences:get 'drscheme:library-file))])
      
      (public
	[stop-execute-button (void)]
	[execute-button (void)]
	[button-panel (make-object mred:horizontal-panel% top-panel)])
      
      (sequence
	(set! execute-button
	      (make-object mred:button% button-panel
			   (lambda (button evt) (execute-callback))
			   (make-execute-bitmap)))
	(set! stop-execute-button
	      (make-object mred:button% button-panel 
			   (lambda args
			     (send interactions-edit break)
			     (ensure-interactions-shown)
			     (send (send interactions-edit get-canvas) set-focus))
			   (make-break-bitmap)))
	(send imports-panel stretchable-in-y #f)
	(send button-panel stretchable-in-y #f)
	(send button-panel stretchable-in-x #f) 
	(send top-panel stretchable-in-y #f))
      
      (private
	[remove-library-callback
	 (fw:preferences:add-callback
	  'drscheme:library-file
	  (let ([last-one (fw:preferences:get 'drscheme:library-file)])
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
	(send interactions-edit initialize-console)

	(when (or (ivar interactions-edit repl-initially-active?)
		  (fw:preferences:get 'drscheme:repl-always-active))
	  (toggle-show/hide show-menu interactions-id))
	
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
	  [else (void)]))))
  
  (define snip%
    (class* drscheme:graph:node-snip% (drscheme:face:unit-snipI) (unit)
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
      (class mred:snip-class% args
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
	     (let ([l (mzlib:string:read-from-string (send p get-string null))])
	       (make-object snip% (car l) (cadr l))))])
	(sequence
	  (apply super-init args)
	  (set-classname classname)
	  (set-version version)
	  (send (mred:get-the-snip-class-list) add this)))))
  
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
	       (set! imports (mzlib:function:remq i imports))))]
	  [add-export 
	   (lambda (e)
	     (set! exports (cons e exports)))]
	  [remove-export
	   (lambda (e)
	     (when (member e exports)
	       (set! exports (mzlib:function:remq e exports))))])
	
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
			 (set! snips (mzlib:function:remq snip snips)))] 
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
  
  (fw:preferences:set-default 'drscheme:open-all-files-in-scheme-mode
			      #t
			      boolean?)
  
  (define (open-as-unit name)
    (if (and created-frame
	     (not (eq? created-frame 'nothing-yet)) 
	     (send created-frame still-untouched?))
	(send created-frame change-to-file name)
	(let* ([unit (make-unit name)]
	       [f (begin (send unit create-frame) 
			 (send unit get-frame))])
	  (send f show #t)
	  f)))
    
  (fw:handler:insert-format-handler 
   "Units"
   (lambda (filename)
     (or (fw:preferences:get 'drscheme:open-all-files-in-scheme-mode)
	 (let ([filename-ext (mzlib:file:filename-extension filename)])
	   (and filename-ext
		(ormap (lambda (extension)
			 (string=? filename-ext extension))
		       (list "ss" "scm" "sch" "mredrc"))))))
   open-as-unit))
