
(unit/sig drscheme:unit^
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:frame : drscheme:frame^]
	  [drscheme:text : drscheme:text^]
	  [drscheme:rep : drscheme:rep^]
	  [drscheme:language : drscheme:language^]
	  [drscheme:get/extend : drscheme:get/extend^]
	  [drscheme:graph : drscheme:graph^])
  
  (define make-bitmap 
    (case-lambda 
     [(button-name) (make-bitmap 
		     (let ([capd (string-copy button-name)])
		       (string-set! capd 0 (char-upcase (string-ref capd 0)))
		       capd)
		     (build-path
		      (collection-path "icons")
		      (string-append button-name ".bmp")))]
     [(text filename)
      (lambda (area-container-window)
	(let*-values ([(outside-margin) 2]
		      [(middle-margin) 3]
		      [(font) (send area-container-window get-control-font)]
		      [(img-bitmap-dc img-width img-height)
		       (let ([mdc (make-object mred:bitmap-dc%)]
			     [q (make-object mred:bitmap% filename)])
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
		      [(new-width) (inexact->exact
				    (floor
				     (+ outside-margin
					img-width
					middle-margin
					width
					outside-margin)))]
		      [(new-height) (inexact->exact
				     (floor (+ outside-margin
					       (max img-height height)
					       outside-margin)))]
		      [(bitmap-dc) (make-object mred:bitmap-dc%)]
		      [(new-bitmap) (make-object mred:bitmap% new-width new-height)])
	  (cond
	   [(or (= img-width 0)
		(= img-height 0))
	    text]
	   [else
	    (send* bitmap-dc
		   (set-bitmap new-bitmap)
		   (set-scale 1 1)
		   (set-font font)
		   (clear)
		   (draw-text text (+ outside-margin img-width middle-margin)
			      (- (/ new-height 2) (/ height 2))))
	    (let ([bm (send img-bitmap-dc get-bitmap)])
	      (send img-bitmap-dc set-bitmap #f)
	      (send bitmap-dc draw-bitmap
		    bm
		    outside-margin
		    (- (/ new-height 2) (/ img-height 2)))
	      (send bitmap-dc set-bitmap #f)
	      new-bitmap)])))]))
  
  (define make-execute-bitmap (make-bitmap "execute"))
  (define make-save-bitmap (make-bitmap "save"))
  (define make-break-bitmap (make-bitmap "break"))
  
  ;; this is the old definition of the interactions canvas.
  ;; It should be integrated into fw:canvas:wide-snip% 
  ;; becuase it uses a better algorithm to find the snip
  ;; wdie widths.
  '(class-asi mred:wide-snip-canvas% ; to match rep-new.ss, inherit from wrapping-canvas% 
     (inherit get-editor)
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
			       [containing-media (send (send snip get-admin) get-editor)]
			       [containing-admin (send containing-media get-admin)])
			  (send containing-media get-snip-position-and-location
				snip (box 0) snip-x-pos (box 0))
			  (+ (let ([lmargin (box 0)]
				   [rmargin (box 0)])
			       (send snip get-margin lmargin (box 0) rmargin (box 0))
			       (+ (unbox lmargin) (unbox rmargin)))
			     (if (is-a? containing-admin mred:editor-snip-editor-admin<%>)
				 (+ (unbox snip-x-pos)
				    (loop (send containing-admin get-snip)))
				 (unbox snip-x-pos)))))]
		     [outer-snip (let loop ([snip s])
				   (let ([containing-admin
					  (send (send (send snip get-admin)
						      get-editor) get-admin)])
				     (if (is-a? containing-admin mred:editor-snip-editor-admin<%>)
					 (loop (send containing-admin get-snip))
					 snip)))]					
		     [view-width (let* ([width (box 0)]
					[extra-space 2] ;; this is to allow the edit room
					;; to show the caret at the end
					;; of the line
					[media (get-editor)])
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
	  (send x on-set-media this))]))
  
  ;; this sends a message to it's frame when it gets the focus
  (define make-searchable-canvas%
    (lambda (%)
      (class-asi %
	(inherit get-top-level-window)
	(rename [super-on-focus on-focus])
	(override
	  [on-focus
	   (lambda (on?)
	     (when on?
	       (send (get-top-level-window) make-searchable this))
	     (super-on-focus on?))]))))
  
  (define interactions-canvas% (make-searchable-canvas%
				(fw:canvas:info-mixin
				 fw:canvas:wide-snip%)))
  
  (define definitions-canvas%
    (class (make-searchable-canvas% fw:canvas:info%) args
      (inherit get-top-level-window get-editor)
      (sequence
	(apply super-init args)
	(let ([m (get-editor)])
	  (send (get-top-level-window) set-save-init-shown?
		(and m (send m is-modified?)))))))
  
  (define definitions-super%
    (fw:scheme:text-mixin
     fw:text:info%))

  (define definitions-text%
    (class definitions-super% ()

      (inherit get-top-level-window)
      (private
        [reset-highlighting
         (lambda ()
           (let ([f (get-top-level-window)])
             (when f
	       (let ([interactions-text (ivar f interactions-text)])
		 (when (object? interactions-text)
		   (let ([reset (ivar interactions-text reset-highlighting)])
		     (when (procedure? reset)
		       (reset))))))))])
      (rename [super-on-insert on-insert]
              [super-on-delete on-delete])
      (override
        [on-insert
         (lambda (x y)
           (reset-highlighting)
           (super-on-insert x y))]
        [on-delete
         (lambda (x y)
           (reset-highlighting)
           (super-on-delete x y))])

      (rename
       [super-set-modified set-modified]
       [super-set-filename set-filename])
      (override
       [set-modified
	(lambda (mod?)
	  (let ([f (get-top-level-window)])
	    (when f
	      (send f update-save-button mod?)))
	  (super-set-modified mod?))]
       [set-filename
	(case-lambda
	 [(fn) (set-filename fn #f)]
	 [(fn tmp?)
	  (let ([f (get-top-level-window)])
	    (when f
	      (send f update-save-message fn)))
	  (super-set-filename fn tmp?)])])

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
	   (set! already-warned? #t))])
      (override
	[after-insert
	 (lambda x
	   (set! needs-execution? #t)
	   (apply super-after-insert x))]
	[after-delete
	 (lambda x
	   (set! needs-execution? #t)
	   (apply super-after-delete x))])
      (sequence
	(super-init))))
  
  (define super-frame%
    (drscheme:frame:mixin
     (drscheme:frame:basics-mixin fw:frame:text-info-file%)))
  
  (define frame%
    (class* super-frame% () (filename)
      (inherit get-canvas
	       set-label-prefix show-menu
	       show get-menu%
	       get-editor
	       get-area-container update-info
	       get-file-menu
	       file-menu:get-open-item
	       file-menu:get-new-item
	       file-menu:get-save-item
	       file-menu:get-save-as-item
	       file-menu:get-revert-item
	       file-menu:get-print-item)
      (rename [super-update-shown update-shown]
	      [super-on-close on-close])
      (public
	[definitions-item #f]
	[interactions-item #f]
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
	   (set! search-canvas canvas))])
      (override
	[get-text-to-search
	 (lambda ()
	   (if search-canvas
	       (send search-canvas get-editor)
	       (get-editor)))])
      
      (private [was-locked? #f]
	       [execute-menu-item #f])
      (public
	[disable-evaluation
	 (lambda ()
	   (when execute-menu-item
	     (send execute-menu-item enable #f))
	   (send execute-button enable #f)
	   (send definitions-text lock #t)
	   (send interactions-text lock #t))]
	[enable-evaluation
	 (lambda ()
	   (when execute-menu-item
	     (send execute-menu-item enable #t))
	   (send execute-button enable #t)
	   (send definitions-text lock #f)
	   (unless (send interactions-text eval-busy?)
	     (send interactions-text lock #f)))])
      
      (inherit set-label)
      (public
        [update-save-button
	 (lambda (mod?)
	   (if save-button
	       (send save-button show mod?)
	       (set! save-init-shown? mod?)))]
	[update-save-message
	 (lambda (name)
	   (when save-button
		 (let* ([fname (and name (mzlib:file:file-name-from-path name))]
			[msg (make-object mred:message% (or fname "") top-panel)])
		   (set! name-message msg)
		   (set-label fname)
		   (send top-panel change-children
			 (lambda (l) (build-top-panel-children))))))])
      (override
	[get-canvas% (lambda () (drscheme:get/extend:get-definitions-canvas%))])
      (public
	[ensure-interactions-shown
	 (lambda ()
	   (when (hidden? interactions-item)
	     (toggle-show/hide interactions-item)
	     (update-shown)))])
      
      (override
	[get-editor% (lambda () (drscheme:get/extend:get-definitions-text%))])
      (public
	[still-untouched?
	 (lambda ()
	   (and (= (send definitions-text last-position) 0)
		(not (send definitions-text is-modified?))
		(not (send definitions-text get-filename))))]
	[change-to-file
	 (lambda (name)
	   (cond
	     [(and name (file-exists? name))
	      (send definitions-text load-file name)]
	     [name
	      (send definitions-text set-filename name)]
	     [else (send definitions-text clear)])
	   (send definitions-canvas focus))])

      (private
       [drscheme-manual 
	"PLT DrScheme: Programming Environment Manual"]
       [hidden?
	(lambda (item)
	  (let ([label (send item get-label)])
	    (and (string? label)
		 (>= (string-length label) 4)
		 (string=? (substring label 0 4) "Show"))))]
	[save-as-text-from-text
	 (lambda (win)
	   (let ([file (mred:put-file)])
	     (when file
		   (send win save-file file 'text))))])

      (public
	[toggle-show/hide
	 (lambda (item)
	   (let ([label (send item get-label)])
	     (when (and (string? label)
			(>= (string-length label) 4))
	       (let ([new-front
		      (if (string=? "Hide" (substring label 0 4))
			  "Show"
			  "Hide")]
		     [back (substring label 4 (string-length label))])
		 (send item set-label (string-append new-front back))))))])

      (private
	[file-menu:print-transcript-item #f])

      (rename
       [super-file-menu:between-open-and-revert file-menu:between-open-and-revert])
      (override
	[file-menu:between-open-and-revert
	 (lambda (file-menu)
           (super-file-menu:between-open-and-revert file-menu)
	   (make-object mred:separator-menu-item% file-menu))]
	[file-menu:save-string (lambda () "Definitions")]
	[file-menu:save-as-string (lambda () "Definitions")]
	[file-menu:between-save-as-and-print
	 (lambda (file-menu)
	   (make-object mred:menu-item%
	     "Save Definitions As Text..."
	     file-menu
	     (lambda (_1 _2)
	       (save-as-text-from-text definitions-text)))
	   (make-object mred:menu-item%
	     "Save Interactions"
	     file-menu
	     (lambda (_1 _2) (send interactions-text save-file)))
	   (make-object mred:menu-item%
	     "Save Interactions As..."
	     file-menu
	     (lambda (_1 _2) 
	       (let ([file (mred:put-file)])
		 (when file
		   (send interactions-text save-file 
			 file 'standard)))))
	   (make-object mred:menu-item%
	     "Save Interactions As Text..."
	     file-menu
	     (lambda (_1 _2)
	       (save-as-text-from-text interactions-text)))
	   (make-object mred:separator-menu-item% file-menu))]
	[file-menu:print-string (lambda () "Definitions")]
	[file-menu:between-print-and-close
	 (lambda (file-menu)
	   (set! file-menu:print-transcript-item
		 (make-object mred:menu-item%
		   "Print Interactions..."
		   file-menu
		   (lambda (_1 _2)
		     (send interactions-text print
			   #t 
			   #t
			   (fw:preferences:get 'framework:print-output-mode)))))
	   (make-object mred:separator-menu-item% file-menu))])
      (private
	[item->child
	 (lambda (item)
	   (cond
	     [(eq? item interactions-item) interactions-canvas]
	     [(eq? item definitions-item) definitions-canvas]
	     [else (error 'item->child "unknown item: ~a" item)]))])
      (private
	[get-sub-items
	 (lambda ()
	   (list interactions-item definitions-item
		 ;imports-item
		 ))]
	[update-shown/ensure-one
	 (lambda (last-one)
	   (when (andmap hidden? (get-sub-items))
	     (toggle-show/hide last-one))
	   (update-shown))])
      (override
	[update-shown
	 (lambda ()
	   (let ([panel (get-area-container)])
	     (super-update-shown)
	     (send panel change-children
		   (lambda (l)
		     (cons top-panel
			   (mzlib:function:foldl
			    (lambda (item sofar)
			      (if (hidden? item)
				  sofar
				  (cons (item->child item) sofar)))
			    null
			    (get-sub-items)))))
	     (when (ormap (lambda (child)
			    (and (is-a? child mred:editor-canvas%)
				 (not (send child has-focus?))))
			  (send panel get-children))
	       (let loop ([children (send panel get-children)])
		 (cond
		  [(null? children) (void)]
		  [else (let ([child (car children)])
			  (if (is-a? child mred:editor-canvas%)
			      (send child focus)
			      (loop (cdr children))))])))
	     
	     (send interactions-text scroll-to-position 
		   (send interactions-text get-end-position)
		   #f
		   (send interactions-text get-start-position)
		   'start)
	     (send definitions-text scroll-to-position 
		   (send definitions-text get-end-position)
		   #f
		   (send definitions-text get-start-position)	
		   'start)
	     (let ([defs-show? (not (hidden? definitions-item))])
	       (for-each
		(lambda (get-item)
		  (send (get-item) enable defs-show?))
		(list file-menu:get-revert-item
		      file-menu:get-save-item
		      file-menu:get-save-as-item
		      ;file-menu:save-as-text-item ; Save As Text...
		      file-menu:get-print-item)))
	     (send file-menu:print-transcript-item enable
		   (not (hidden? interactions-item)))))]
	
	[on-close
	 (lambda ()
	   (remove-library-callback)
	   (when (eq? this created-frame)
	     (set! created-frame #f))
	   (send interactions-text shutdown)
	   (super-on-close))])
	
      (public
	[running? #t]; is this necessary?
	[execute-callback
	 (lambda ()
	   (ensure-interactions-shown)
	   (send definitions-text just-executed)
	   (send interactions-canvas focus)
	   (send interactions-text reset-console)
	   (send interactions-text clear-undos)
	   (send interactions-text do-many-buffer-evals
		 definitions-text 0
		 (send definitions-text last-position))
	   (send interactions-text clear-undos))])
  
      (public
	[after-change-name void])

      (inherit get-menu-bar get-focus-object)
      (sequence
	(super-init filename)

	(let* ([mb (get-menu-bar)]
	       [language-menu (make-object (get-menu%) "&Language" mb)]
	       [scheme-menu (make-object (get-menu%) "S&cheme" mb)]
	       [send-method
		(lambda (method)
		  (lambda (_1 _2)
		    (let ([text (get-focus-object)])
		      (when (or (eq? text definitions-text)
				(eq? text interactions-text))
			((ivar/proc text method))))))])
	  
	  (drscheme:language:fill-language-menu language-menu)
	  
	  (set! execute-menu-item
		(make-object mred:menu-item%
		  "Execute"
		  scheme-menu
		  (lambda (_1 _2) (execute-callback))
		  (and
		   (fw:preferences:get 'framework:menu-bindings)
		   #\t)
		  "Restart the program in the definitions window"))
	  (make-object mred:menu-item%
	    "Break"
	    scheme-menu
	    (lambda (_1 _2) (send interactions-text break))
	    (and
	     (fw:preferences:get 'framework:menu-bindings)
	     #\b)
	    "Break the current evaluation")
	  (make-object mred:separator-menu-item% scheme-menu)
	  (make-object mred:menu-item%
	    "&Reindent"
	    scheme-menu
	    (send-method 'tabify-selection))
	  (make-object mred:menu-item%
	    "Reindent &All"
	    scheme-menu
	    (send-method 'tabify-all)
	    (and
	     (fw:preferences:get 'framework:menu-bindings)
	     #\i))
	  (make-object mred:menu-item%
	    "&Comment Out"
	    scheme-menu
	    (send-method 'comment-out-selection))
	  (make-object mred:menu-item%
	    "&Uncomment"
	    scheme-menu
	    (send-method 'uncomment-selection)))

	(fw:frame:reorder-menus this)
	     
	(set! definitions-item
	      (make-object mred:menu-item%
		"Hide &Definitions"
		show-menu
		(lambda (_1 _2) 
		  (toggle-show/hide definitions-item)
		  (update-shown/ensure-one interactions-item))
		(and
		 (fw:preferences:get 'framework:menu-bindings)
		 #\d)
		"Show the definitions window"))
	(set! interactions-item
	      (make-object mred:menu-item%
                           "Show &Interactions"
                           show-menu
                           (lambda (_1 _2) 
                             (toggle-show/hide interactions-item)
                             (update-shown/ensure-one definitions-item))
                           (and
                            (fw:preferences:get 'framework:menu-bindings)
                            #\e)
                           "Show the interactions window")))
      
      (private
	[top-panel (make-object mred:horizontal-panel% (get-area-container))])
      
      (public
	[definitions-canvas (get-canvas)]
	[definitions-text (get-editor)]
	[interactions-canvas (make-object 
			      (drscheme:get/extend:get-interactions-canvas%)
			      (get-area-container))]
	[interactions-text (make-object 
			    (drscheme:get/extend:get-interactions-text%))])

      (sequence
	(send* interactions-canvas 
	  ;(scroll-with-bottom-base #t)
	  (set-editor interactions-text))
	(send interactions-text auto-wrap #t)

	(set! save-button
	      (make-object mred:button% 
			   (make-save-bitmap this)
			   top-panel
			   (lambda args
			     (let* ([text definitions-text])
			       (when text
				 (send text save-file)
				 (send definitions-canvas focus))))))
	
	(set! name-message (make-object mred:message% "" top-panel)))
      (private 
	[make-library-name-msg
	 (lambda (panel n)
	   (make-object mred:message%
	     (if n
		 (let-values ([(base name must-be-dir) (split-path n)])
		   name)
		 "") 
	     panel))]
	[library-msg (make-library-name-msg
		      top-panel
		      (fw:preferences:get 'drscheme:library-file))])
      
      (public
	[stop-execute-button (void)]
	[execute-button (void)]
	[button-panel (make-object mred:horizontal-panel% top-panel)])
      
      (sequence
	(set! execute-button
	      (make-object mred:button%
		(make-execute-bitmap this)
		button-panel
		(lambda (button evt) (execute-callback))))
	(set! stop-execute-button
	      (make-object mred:button%
		(make-break-bitmap this) 
		button-panel
		(lambda args
		  (send interactions-text break)
		  (ensure-interactions-shown)
		  (send (send interactions-text get-canvas) focus))))
	(send button-panel stretchable-height #f)
	(send button-panel stretchable-width #f) 
	(send top-panel stretchable-height #f))
      
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
		; (send top-panel change-children (lambda (l) (mzlib:function:remq library-msg l)))
		(set! library-msg (make-library-name-msg top-panel v))
		(send definitions-text library-changed)
		(send top-panel change-children 
		      (lambda (l) (build-top-panel-children)))))))])
      
      (private
	[build-top-panel-children
	 (lambda ()
	   (list name-message save-button
		 library-msg
		 button-panel))])
      
      (inherit get-label)
      (sequence
	
	(update-save-button #f)

	(update-save-message (get-label))
	
	 '(let ([fn (send definitions-text get-filename)])
	   (cond
	    [(and fn (mzlib:file:file-name-from-path fn)) => (lambda (x) x)]
	    [else (next-untitled-name)]))

	(send interactions-text initialize-console)

	(unless filename
	  (toggle-show/hide interactions-item))
	
	(send interactions-text insert-prompt)
	(send interactions-text clear-undos)
	
	(update-shown)
	
	(set-label-prefix "DrScheme")
	
	(send definitions-canvas focus)
	(cond
	  [(eq? created-frame 'nothing-yet)
	   (set! created-frame this)]
	  [created-frame
	   (set! created-frame #f)]
	  [else (void)]))))
  
  (define created-frame 'nothing-yet)
  
  (define open-drscheme-window
    (case-lambda
     [() (open-drscheme-window #f)]
     [(name)
      (if (and created-frame
	       name
	       (not (eq? created-frame 'nothing-yet)) 
	       (send created-frame still-untouched?))
	  (begin (send created-frame change-to-file name)
		 created-frame)
	  (let* ([frame% (drscheme:get/extend:get-unit-frame%)]
		 [frame (make-object frame% name)])
	    (send frame show #t)
	    frame))]))

  (fw:handler:insert-format-handler 
   "Units"
   (lambda (filename) #t)
   open-drscheme-window))
