(unit/sig drscheme:unit^
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [mzlib:date : mzlib:date^]
	  [fw : framework^]
          [launcher : launcher-maker^]
	  [basis : plt:basis^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:frame : drscheme:frame^]
	  [drscheme:text : drscheme:text^]
	  [drscheme:rep : drscheme:rep^]
	  [drscheme:language : drscheme:language^]
	  [drscheme:get/extend : drscheme:get/extend^]
	  [drscheme:graph : drscheme:graph^])
  
  (define (create-launcher frame)
    (let ([program-filename (send (ivar frame definitions-text) get-filename)])
      (cond
       [(not program-filename)
	(mred:message-box "Create Launcher"
			  "You must save your program before creating a launcher"
			  frame)]
       [else
	(let* ([settings (fw:preferences:get drscheme:language:settings-preferences-symbol)]
	       [v-settings (struct->vector settings)]
	       [teachpacks (fw:preferences:get 'drscheme:teachpack-file)]
	       [in-mz? (regexp-match "MzScheme" (basis:setting-name settings))]
	       [filename 
		(parameterize ([fw:finder:dialog-parent-parameter frame])
		  (fw:finder:put-file"Untitled" #f #f "Save a Launcher"))])
	  (when filename
	    (let ([definitions (list "-e" (format "(define filename ~s)" program-filename)
				     "-e" (format "(define settings ~s)" v-settings)
				     "-e" (format "(define teachpacks '~s)" teachpacks))])
	      ((if (and in-mz? (null? teachpacks))
		   launcher:make-mzscheme-launcher
		   launcher:make-mred-launcher)
	       (append '("-qmv") definitions '("-L" "launcher-bootstrap.ss" "userspce"))
	       filename))))])))
  
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
  ;; wide widths.
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
      (sequence
	(apply super-init args))))
  
  (define (program-editor-mixin text%)
    (class/d text% args
      ((override after-insert after-delete)
       (inherit get-top-level-window)
       (rename [super-after-insert after-insert]
	       [super-after-delete after-delete]))

      (define (reset-highlighting)
	(let ([f (get-top-level-window)])
	  (when f
	    (let ([interactions-text (ivar f interactions-text)])
	      (when (object? interactions-text)
		(let ([reset (ivar interactions-text reset-highlighting)])
		  (when (procedure? reset)
		    (reset))))))))

      (define (after-insert x y)
	(reset-highlighting)
	(super-after-insert x y))

      (define (after-delete x y)
	(reset-highlighting)
	(super-after-delete x y))

      (apply super-init args)))

  (define definitions-super%
    (program-editor-mixin
     (fw:scheme:text-mixin
      fw:text:info%)))

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
      (inherit is-modified? run-after-edit-sequence)
      (override
       [set-modified
	(lambda (mod?)
	  (super-set-modified mod?)
	  (run-after-edit-sequence
	   (lambda ()
	     (let ([f (get-top-level-window)])
	       (when f
		 (send f update-save-button (is-modified?)))))))]
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
      (private
        [needs-execution-state #f]
        [already-warned-state #f]
        [execute-language (fw:preferences:get drscheme:language:settings-preferences-symbol)])
      (public
	[needs-execution? 
         (lambda ()
           (or needs-execution-state
               (not (equal? execute-language
                            (fw:preferences:get drscheme:language:settings-preferences-symbol)))))]
	[teachpack-changed
	 (lambda ()
	   (set! needs-execution-state #t))]
	[just-executed
	 (lambda ()
           (set! execute-language (fw:preferences:get drscheme:language:settings-preferences-symbol))
	   (set! needs-execution-state #f)
	   (set! already-warned-state #f))]
	[already-warned? (lambda () already-warned-state)]
	[already-warned
	 (lambda ()
	   (set! already-warned-state #t))])
      (override
	[after-insert
	 (lambda x
	   (set! needs-execution-state #t)
	   (apply super-after-insert x))]
	[after-delete
	 (lambda x
	   (set! needs-execution-state #t)
	   (apply super-after-delete x))])

      (inherit get-filename)
      (private
	[tmp-date-string #f]
	[get-date-string
	 (lambda ()
	   (string-append
	    (mzlib:date:date->string (seconds->date (current-seconds)))
	    " "
	    (let ([fn (get-filename)])
	      (if (string? fn)
		  fn
		  "Untitled"))))])
	 
      (rename [super-on-paint on-paint])
      (override
       [on-paint
	(lambda (before dc left top right bottom dx dy draw-caret)
	  (when (and before
		     (or (is-a? dc mred:post-script-dc%)
			 (is-a? dc mred:printer-dc%)))
	    (set! tmp-date-string (get-date-string))
	    (let-values ([(w h d s) (send dc get-text-extent tmp-date-string)])
	      (send (mred:current-ps-setup) set-editor-margin 0 (inexact->exact (ceiling h)))))
	  (super-on-paint before dc left top right bottom dx dy draw-caret)
	  (when (and (not before)
		     (or (is-a? dc mred:post-script-dc%)
			 (is-a? dc mred:printer-dc%)))
	    (send dc draw-text (get-date-string) 0 0)
	    (void)))])
      (sequence
	(super-init))))

  (define button-label-inset 1)
  (define drop-shadow-size 2)

  (define black-color (make-object mred:color% "BLACK"))

  (define button-label-font
    (send mred:the-font-list find-or-create-font
	  (case (system-type)
	    [(windows) 8]
	    [else 10])
	  'decorative 'normal 'normal #f))

  (define (calc-button-min-sizes dc label)
    (send dc set-font button-label-font)
    (let-values ([(w h a d) (send dc get-text-extent label button-label-font)])
      (values
       (+ button-label-inset button-label-inset
	  drop-shadow-size
	  1 ;; for the outer drop shadow
	  1 ;; becuase "(define ...)" has the wrong size under windows
	  (inexact->exact (ceiling w)))
       (+ button-label-inset button-label-inset
	  drop-shadow-size
	  1 ;; for the outer drop shadow
	  (inexact->exact (ceiling h))))))

  (define (offset-color color offset-one)
    (make-object mred:color%
      (offset-one (send color red))
      (offset-one (send color green))
      (offset-one (send color blue))))

  (define light-button-color (offset-color (mred:get-panel-background)
					   (lambda (v) (floor (+ v (/ (- 255 v) 2))))))
  (define dark-button-color (offset-color (mred:get-panel-background)
					  (lambda (v) (floor (- v (/ v 2))))))

  (define (draw-button-label dc label w h inverted?)
    (send dc set-text-foreground black-color)
    (send dc set-text-background (mred:get-panel-background))
    (send dc set-pen (send mred:the-pen-list find-or-create-pen
			   (mred:get-panel-background) 1 'solid))
    (send dc set-brush (send mred:the-brush-list find-or-create-brush
			     (mred:get-panel-background) 'solid))

    (send dc draw-rectangle 0 0 w h)

    (send dc set-pen (send mred:the-pen-list find-or-create-pen
			   "BLACK" 1 'solid))
    (send dc set-brush
	  (send mred:the-brush-list find-or-create-brush
		(if inverted? dark-button-color light-button-color) 'solid))

    (let ([border
	   (lambda (d)
	     (send dc draw-rectangle
		   d d
		   (- w drop-shadow-size)
		   (- h drop-shadow-size)))])
      (if inverted?
	  (let loop ([n 0])
	    (cond
	     [(= n drop-shadow-size) (void)]
	     [else
	      (border n)
	      (loop (+ n 1))]))
	  (let loop ([n drop-shadow-size])
	    (cond
	     [(zero? n) (void)]
	     [else
	      (border (- n 1))
	      (loop (- n 1))]))))

    (when label
      (send dc set-font button-label-font)
      (let-values ([(tw th _2 _3) (send dc get-text-extent label)])

	;; 1 is for the outer drop shadow box
	(send dc draw-text label
	      (+ button-label-inset
		 (if inverted? drop-shadow-size 1))
	      (+ button-label-inset
		 (if inverted? drop-shadow-size 1))))))

  (define name-message%
    (class/d mred:canvas% (parent)
      ((inherit get-dc get-size get-client-size min-width min-height stretchable-width stretchable-height)
       (public set-message) ;; set-message : (union #f string) string -> void
       (override on-event on-paint))

      ;(define font (send parent get-label-font))
      
      (define label #f)
      (define short-label "Untitled")
      (define (set-message name short-name)
	(set! label name)
	(set! short-label short-name)
	(update-min-sizes))
      
      (define full-name-window #f)

      (define (show-full-name-window)
	(if label
	    (mred:message-box "Full Name" label)
	    (mred:message-box
	     "Full Name"
	     "The file does not have a full name because it has not yet been saved.")))

      (define mouse-grabbed? #f)
      (define (on-event evt)
	(cond
          [(send evt moving?)
           (when mouse-grabbed?
             (let-values ([(max-x max-y) (get-size)])
               (let ([inside? (and (<= 0 (send evt get-x) max-x)
                                   (<= 0 (send evt get-y) max-y))])
                 (unless (eq? inside? inverted?)
                   (set! inverted? inside?)
                   (on-paint)))))]
          [(send evt button-up? 'left)
           (set! mouse-grabbed? #f)
           (cond
             [inverted?
              (set! inverted? #f)
              (on-paint)
              (show-full-name-window)]
             [else
              (void)])]
          [(send evt button-down? 'left)
           (set! mouse-grabbed? #t)
           (set! inverted? #t)
           (on-paint)]
          [else (void)]))
		
      (define (update-min-sizes)
	(let-values ([(w h) (calc-button-min-sizes (get-dc) short-label)])
	  (min-width w)
	  (min-height h))
	(send parent reflow-container))

      (define inverted? #f)

      (define (on-paint)
	(let ([dc (get-dc)])
	  (let-values ([(w h) (get-client-size)])
	    (draw-button-label dc short-label w h inverted?))))

      (super-init parent)
      (stretchable-width #f)
      (stretchable-height #f)))

  (define func-defs-canvas%
    (class/d mred:canvas% (parent text)
      ((override on-paint on-event)
       (inherit get-client-size get-dc popup-menu min-height min-width
		stretchable-width
		stretchable-height)
       (rename [super-on-event on-event]))

      (define-struct defn (indent name pos))

      (define tag-string "(define")
      (define (get-definitions)
	(let* ([min-indent 0]
	       [defs (let loop ([pos 0])
		       (let ([defn-pos (send text find-string tag-string 'forward pos 'eof #t #f)])
			 (if defn-pos
			     (let ([indent (get-defn-indent defn-pos)]
				   [name (get-defn-name (+ defn-pos (string-length tag-string)))])
			       (set! min-indent (min indent min-indent))
			       (cons (make-defn indent name defn-pos)
				     (loop (+ defn-pos (string-length tag-string)))))
			     null)))])
	  (for-each (lambda (defn)
		      (set-defn-name! defn
				      (string-append
				       (apply string
					      (vector->list
					       (make-vector (- (defn-indent defn) min-indent) #\space)))
				       (defn-name defn))))
		    defs)
	  defs))

      (define (get-defn-indent pos)
	(let* ([para (send text position-paragraph pos)]
	       [para-start (send text paragraph-start-position para #t)])
	  (let loop ([c-pos para-start]
		     [offset 0])
	    (if (< c-pos pos)
		(let ([char (send text get-character c-pos)])
		  (cond
		   [(char=? char #\tab)
		    (loop (+ c-pos 1) (+ offset (- 8 (modulo offset 8))))]
		   [else
		    (loop (+ c-pos 1) (+ offset 1))]))
		offset))))

      (define (skip-to-whitespace/paren pos)
	(let loop ([pos pos])
	  (if (>= pos (send text last-position))
	      (send text last-position)
	      (let ([char (send text get-character pos)])
		(cond
		 [(or (char=? #\) char)
		      (char=? #\( char)
		      (char=? #\] char)
		      (char=? #\[ char)
		      (char-whitespace? char))
		  pos]
		 [else (loop (+ pos 1))])))))

      (define (skip-whitespace/paren pos)
	(let loop ([pos pos])
	  (if (>= pos (send text last-position))
	      (send text last-position)
	      (let ([char (send text get-character pos)])
		(cond
		 [(or (char=? #\) char)
		      (char=? #\( char)
		      (char=? #\] char)
		      (char=? #\[ char)
		      (char-whitespace? char))
		  (loop (+ pos 1))]
		 [else pos])))))

      (define (get-defn-name define-pos)
	(if (>= define-pos (send text last-position))
	    "<end of buffer>"
	    (let* ([start-pos (skip-whitespace/paren (skip-to-whitespace/paren define-pos))]
		   [end-pos (skip-to-whitespace/paren start-pos)])
	      (send text get-text start-pos end-pos))))

      (define inverted? #f)

      (define label "(define ...)")

      (define (on-paint)
	(let ([dc (get-dc)])
	  (let-values ([(w h) (get-client-size)])
	    (draw-button-label dc label w h inverted?))))

      (define sorted? #f)

      (define (on-event evt)
	(cond
	 [(send evt button-down?)
	  (set! inverted? #t)
	  (on-paint)
	  (let ([menu (make-object mred:popup-menu% #f
				   (lambda x
				     (set! inverted? #f)
				     (on-paint)))]
		[defns (get-definitions)])
	    (if (null? defns)
		(send (make-object mred:menu-item%
			"<< no definitions found >>"
			menu
			void)
		      enable #f)
		(let loop ([defns defns])
		  (unless (null? defns)
		    (let* ([defn (car defns)]
			   [next-start (if (null? (cdr defns))
					   (+ (send text last-position) 1)
					   (defn-pos (car (cdr defns))))]
			   
			   [checked? (and (<= (defn-pos defn)
					      (send text get-start-position))
					  (<  (send text get-end-position)
					      next-start))]
			   [item
			    (make-object (if checked?
					     mred:checkable-menu-item%
					     mred:menu-item%)
			      (defn-name defn)
			      menu
			      (lambda x
				(set! inverted? #f)
				(on-paint)
				(send text set-position (defn-pos defn) (defn-pos defn))
				(let ([canvas (send text get-canvas)])
				  (when canvas
				    (send canvas focus)))))])
		      (when checked?
			(send item check #t))
		      (loop (cdr defns))))))
	    (popup-menu menu
			0
			height))]
	 [else (super-on-event evt)]))

      (super-init parent)

      (define-values (width height) (calc-button-min-sizes (get-dc) label))
      (min-width width)
      (min-height height)
      (stretchable-width #f)
      (stretchable-height #f)))

  (define (set-box/f! b v) (when (box? b) (set-box! b v)))

  (define super-frame%
    (drscheme:frame:mixin
     (drscheme:frame:basics-mixin fw:frame:searchable%)))

  (define frame%
    (class* super-frame% (drscheme:rep:context<%>) (filename)
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
	[get-directory
	 (lambda ()
	   (let ([filename (send definitions-text get-filename)])
	     (if (string? filename)
		 (let-values ([(base _1 _2) (split-path (mzlib:file:normalize-path filename))])
		   base)
		 #f)))]
        [needs-execution?
         (lambda ()
           (send definitions-text needs-execution?))])

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
	       (unless (eq? mod? (send save-button is-shown?))
		 (send save-button show mod?))
	       (set! save-init-shown? mod?)))]
	[update-save-message
	 (lambda (name)
	   (when name-message
	     (let* ([long-name (mzlib:file:normalize-path name)]
		    [short-name (mzlib:file:file-name-from-path long-name)])
	       (send name-message set-message long-name short-name))))])
      (override
       [get-canvas% (lambda () (drscheme:get/extend:get-definitions-canvas%))])
      (public
	[ensure-defs-shown
	 (lambda ()
	   (when (hidden? definitions-item)
	     (toggle-show/hide definitions-item)
	     (update-shown)))]
	[ensure-rep-shown
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
	 (lambda (text)
	   (let ([file (parameterize ([fw:finder:dialog-parent-parameter this])
			 (fw:finder:put-file))])
	     (when file
	       (send text save-file file 'text))))])

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
	   (let ([sub-menu (make-object mred:menu% "Save Other" file-menu)])
	     (make-object mred:menu-item%
	       "Save Definitions As Text..."
	       sub-menu
	       (lambda (_1 _2)
		 (save-as-text-from-text definitions-text)))
	     (make-object mred:menu-item%
	       "Save Interactions"
	       sub-menu
	       (lambda (_1 _2) (send interactions-text save-file)))
	     (make-object mred:menu-item%
	       "Save Interactions As..."
	       sub-menu
	       (lambda (_1 _2) 
		 (let ([file (parameterize ([fw:finder:dialog-parent-parameter this])
			       (fw:finder:put-file))])
		   (when file
		     (send interactions-text save-file 
			   file 'standard)))))
	     (make-object mred:menu-item%
	       "Save Interactions As Text..."
	       sub-menu
	       (lambda (_1 _2)
		 (save-as-text-from-text interactions-text)))
					;	   (make-object mred:separator-menu-item% file-menu)
					;	   (make-object mred:menu-item%
					;	     "Show Interactions History"
					;	     file-menu
					;	     (lambda (_1 _2)
					;	       (drscheme:rep:show-interactions-history)))
	     (make-object mred:separator-menu-item% file-menu)))]
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
	     
	     ;; these should not be necessary anymore?
	     '(send interactions-text scroll-to-position 
		   (send interactions-text get-end-position)
		   #f
		   (send interactions-text get-start-position)
		   'start)
	     '(send definitions-text scroll-to-position 
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
	   (remove-teachpack-callback)
	   (when (eq? this created-frame)
	     (set! created-frame #f))
	   (send interactions-text shutdown)
	   (send interactions-text on-close)
	   (super-on-close))])
	
      (public
	[running? #t]; is this necessary?
	[execute-callback
	 (lambda ()
	   (ensure-rep-shown)
	   (send definitions-text just-executed)
	   (send interactions-canvas focus)
	   (send interactions-text reset-console)
	   (send interactions-text clear-undos)

	   (let ([start (if (and (>= (send definitions-text last-position) 2)
				 (char=? (send definitions-text get-character 0) #\#)
				 (char=? (send definitions-text get-character 1) #\!))
			    (send definitions-text paragraph-start-position 1)
			    0)])
	     (send interactions-text do-many-text-evals
		   definitions-text start
		   (send definitions-text last-position)))
	   (send interactions-text clear-undos))])
  
      (public
	[after-change-name void])

      (inherit get-menu-bar get-focus-object get-edit-target-object)
      (private
	[language-menu 'uninited-language-menu])
      (sequence
	(super-init filename)

	(let* ([mb (get-menu-bar)]
	       [_ (set! language-menu (make-object (get-menu%) "&Language" mb))]
	       [scheme-menu (make-object (get-menu%) "S&cheme" mb)]
	       [send-method
		(lambda (method)
		  (lambda (_1 _2)
		    (let ([text (get-focus-object)])
		      (when (or (eq? text definitions-text)
				(eq? text interactions-text))
			((ivar/proc text method))))))])
	  
	  (drscheme:language:fill-language-menu this language-menu)
	  
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
	  (make-object mred:menu-item%
	    "Kill"
	    scheme-menu
	    (lambda (_1 _2) (send interactions-text kill-evaluation))
	    (and
	     (fw:preferences:get 'framework:menu-bindings)
	     #\k)
	    "Kill the current evaluation")
	  (make-object mred:separator-menu-item% scheme-menu)
          (make-object mred:menu-item% "Create Launcher..." scheme-menu (lambda x (create-launcher this)))
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
	[top-panel (make-object mred:horizontal-panel% (get-area-container))]
	[name-panel (make-object mred:vertical-panel% top-panel)])
      (sequence
	(send name-panel stretchable-width #f)
	(send name-panel stretchable-height #f))
      
      (public
	[definitions-canvas (get-canvas)]
	[definitions-text (get-editor)]
	[interactions-canvas (make-object 
			      (drscheme:get/extend:get-interactions-canvas%)
			      (get-area-container))]
	[interactions-text (make-object 
			    (drscheme:get/extend:get-interactions-text%)
                             this)])

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
	
	(set! name-message (make-object name-message% name-panel)))
      (private 
	[teachpack-items null]
	[update-teachpack-menu
	 (lambda (names)
	   (for-each (lambda (item) (send item delete)) teachpack-items)
	   (set! teachpack-items
		 (map (lambda (name)
			(make-object mred:menu-item%
			  (format "Clear ~a Teachpack" (mzlib:file:file-name-from-path name))
			  language-menu
			  (lambda (item evt)
			    (fw:preferences:set
			     'drscheme:teachpack-file
			     (mzlib:function:remove
			      name
			      (fw:preferences:get 'drscheme:teachpack-file))))))
		      names)))])
      (sequence
	(update-teachpack-menu
	 (fw:preferences:get 'drscheme:teachpack-file)))
      
      (public
	[stop-execute-button (void)]
	[execute-button (void)]
	[button-panel (make-object mred:horizontal-panel% top-panel)])
      
      (private
	[func-defs-canvas (make-object func-defs-canvas% name-panel definitions-text)])

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
		  (ensure-rep-shown)
		  (send (send interactions-text get-canvas) focus))))
	(send button-panel stretchable-height #f)
	(send button-panel stretchable-width #f) 

	(send top-panel change-children
	      (lambda (l)
		(list name-panel save-button
		      (make-object mred:vertical-panel% top-panel) ;; spacer
		      button-panel)))
	(send top-panel stretchable-height #f))
      
      (private
	[remove-teachpack-callback
	 (fw:preferences:add-callback
	  'drscheme:teachpack-file
	  (lambda (p v)
	    (update-teachpack-menu v)
	    (send definitions-text teachpack-changed)))])
      
      (inherit get-label)
      (sequence
	

	(let ([m (send definitions-canvas get-editor)])
	  (set-save-init-shown?
	   (and m (send m is-modified?))))

	;; (get-label) shouldn't be #f, but I'm not sure....
	(send name-message set-message
	      (if filename
		  (mzlib:file:normalize-path filename)
		  #f)
	      (or (get-label) "Untitled"))

	(update-save-button #f)

	(send interactions-text initialize-console)

	(unless filename
	  (toggle-show/hide interactions-item))
	
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
		 (send created-frame show #t)
		 created-frame)
	  (let* ([frame% (drscheme:get/extend:get-unit-frame%)]
		 [frame (make-object frame% name)])
	    (send frame show #t)
	    frame))]))

  (fw:handler:insert-format-handler 
   "Units"
   (lambda (filename) #t)
   open-drscheme-window))

#|
;; old lambda snipclass
;; nixed becuase of poor support from reader

          (make-object mred:separator-menu-item% scheme-menu)
          (make-object mred:menu-item%
            "Insert &Lambda"
            scheme-menu
            (lambda x (let ([editor (get-edit-target-object)])
                        (when editor
                          (send editor insert (make-object lambda-snip%)))))
	    (and
	     (fw:preferences:get 'framework:menu-bindings)
	     #\l))


  (define lambda-snipclass
    (make-object (class mred:snip-class% ()
                   (override
                     [read
                      (lambda (p)
                        (make-object lambda-snip%))])
                   (sequence
                     (super-init)))))
  (send lambda-snipclass set-version 1)
  (send lambda-snipclass set-classname "mred:lambda-snip%")
  (send (mred:get-the-snip-class-list) add lambda-snipclass)

  (define lambda-snip% 
    (class* mred:snip% (fw:gui-utils:text-snip<%>) ()
      (private
        [get-normal-font
         (lambda ()
           (send mred:the-font-list find-or-create-font
                 (fw:preferences:get 'drscheme:font-size)
                 'modern 'normal 'normal #f))]
        [get-lambda-font
         (lambda ()
           (send mred:the-font-list find-or-create-font 
                 (fw:preferences:get 'drscheme:font-size)
                 'symbol 'normal 'normal #f))])
      (public
        [get-string
         (lambda () "lambda")])
      (override
        [get-text
         (case-lambda
          [(x y) " lambda "]
          [(x y z) " lambda "])]
        [copy
         (lambda ()
           (make-object lambda-snip%))]
        [write
         (lambda (p)
           (void))]
        [get-extent
         (lambda (dc x y wb hb descentb spaceb lspaceb rspaceb)
           (let-values ([(w h d s) (send dc get-text-extent "W" (get-normal-font))])
             (set-box/f! wb w)
             (set-box/f! hb h)
             (set-box/f! descentb d)
             (set-box/f! spaceb s)
             (set-box/f! lspaceb 0)
             (set-box/f! rspaceb 0)))]
        [draw
         (lambda (dc x y left top right bottom dx dy draw-caret)
           (let ([font (send dc get-font)])
             (let-values ([(ww wh wd ws) (send dc get-text-extent "W" (get-normal-font))])
               (send dc set-font (get-lambda-font))
               (let-values ([(lw lh ld ls) (send dc get-text-extent "l")])
                 (send dc draw-text "l" 
                       (+ x (/ (- ww lw) 2))
                       (+ y (- (- wh wd) (- lh ld)))))
               (send dc set-font font))))])
      (inherit set-snipclass)
      (sequence
        (super-init)
        (set-snipclass lambda-snipclass))))
|#
