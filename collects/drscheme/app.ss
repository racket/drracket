
  (unit/sig drscheme:app^
    (import [mred : mred-interfaces^]
	    [mzlib : mzlib:core^]
	    [fw : framework^]
	    [drscheme:unit : drscheme:unit^]
	    [drscheme:frame : drscheme:frame^])
    
    (define about-frame%
      (class (drscheme:frame:basics-mixin fw:frame:standard-menus%) (main-media)
	(private
	  [edit-menu:do 
	   (lambda (const)
	     (lambda (_1 _2)
	       (send main-media do-edit-operation const)))])
	(override
	 [file-menu:revert #f]
	 [file-menu:save #f]
	 [file-menu:save-as #f]
	 [file-menu:between-close-and-quit (lambda (x) (void))]
	 [file-menu:between-print-and-close (lambda (x) (void))]
	 [edit-menu:between-redo-and-cut (lambda (x) (void))]
	 [edit-menu:between-select-all-and-find (lambda (x) (void))]
	 [edit-menu:copy (edit-menu:do 'copy)]
	 [edit-menu:select-all (edit-menu:do 'select-all)]
	 [edit-menu:find #f])
	(sequence
	  (super-init "About DrScheme"))))

    (define about-drscheme
      (lambda ()
	(let* ([names (string-append
		       "Gann Bierner, Corky Cartwright, Richard Cobbe, Moy Easwaran, "
		       "Matthias Felleisen, Robby Findler, Cormac Flanagan, Matthew Flatt, "
		       "Sebastian Good, Mark Krentel, Shriram Krishnamurthi, "
		       "Paul Steckler, and Stephanie Weirich")]
	       [wrap-edit% 
		(class-asi mred:text%
		  (inherit begin-edit-sequence end-edit-sequence
			   get-max-width find-snip position-location)
		  (rename [super-after-set-size-constraint after-set-size-constraint])
		  (override
		   [on-set-size-constraint
		    (lambda ()
		      (begin-edit-sequence)
		      (let ([snip (find-snip 1 'after-or-none)])
			(when (is-a? snip mred:original:editor-snip%)
			  (send (send snip get-editor) begin-edit-sequence))))]
		    [after-set-size-constraint
		     (lambda ()
		       (super-after-set-size-constraint)
		       (let ([width (get-max-width)]
			     [snip (find-snip 1 'after-or-none)])
			 (when (is-a? snip mred:original:editor-snip%)
			   (let ([b (box 0)])
			     (position-location 1 b #f #f #t)
			     (let ([new-width (- width 4 (unbox b))])
			       (when (> new-width 0)
				 (send snip resize new-width
				       17) ; smallest random number
				 (send snip set-max-height 'none))))
			   (send (send snip get-editor) end-edit-sequence)))
		       (end-edit-sequence))]))]
	       [e (make-object wrap-edit%)]
	       [main-media (make-object wrap-edit%)]
	       [image-snip 
		(let ([filename (build-path (collection-path "icons")
					    (if (< (mred:get-display-depth) 8)
						"pltbw.gif"
						"plt.gif"))])
		  (if (file-exists? filename)
		      (make-object mred:image-snip% 
				   filename
				   'gif)
		      (let ([i (make-object mred:string-snip%)])
			(send i insert "[lambda]")
			i)))]
	       [media-snip (make-object mred:editor-snip% e #f)]
	       [f (make-object about-frame% main-media)]
	       [c (make-object mred:editor-canvas% (send f get-area-container))]
	       [top (make-object mred:style-delta% 'change-alignment 'top)]
	       [d-usual (make-object mred:style-delta% 'change-family 'decorative)]
	       [d-dr (make-object mred:style-delta%)]
	       [d-http (make-object mred:style-delta%)])
	  (send* d-http 
	    (copy d-usual)
	    (set-delta-foreground "BLUE")
	    (set-delta 'change-underline #t))
	  (send* d-usual 
	    (set-delta-foreground "BLACK")
	    (set-delta 'change-underline #f))

	  (send* d-dr (copy d-usual) (set-delta 'change-bold))
	  (send d-usual set-weight-on 'normal)
	  (send* c (set-editor main-media) (stretchable-width #t) (stretchable-height #t))
	  (send* e 
		 (change-style d-dr)
		 (insert "DrScheme")
		 (change-style d-usual)
		 (insert ", by PLT, Rice University.")
		 (insert #\newline)
		 (insert names)
		 (insert #\newline)
		 (insert "See: ")
		 (change-style d-http))
	  (let* ([before (send e get-start-position)]
		 [url "http://www.cs.rice.edu/CS/PLT/"]
		 [_ (send e insert url)]
		 [after (send e get-start-position)])
	    (send e set-clickback before after 
		  (lambda args (mred:message-box
				"DrScheme"
				(format "Browser not yet supported in DrScheme ~a"
					(version))))
		  d-http))
	  (send* e
	    (insert #\newline)
	    (change-style d-usual)
	    (insert "For licensing information see LICENSE, included with the PLT release.")
	    (insert #\newline)
	    (insert "Based on:")
	    (insert #\newline)
	    (insert "  MrEd version ")
	    (insert (fw:version:version))
	    (insert ", Copyright (c) 1995-1998 PLT, Rice University (Matthew Flatt and Robert Bruce Findler)")
	    (insert #\newline)
	    (auto-wrap #t)
	    (set-autowrap-bitmap #f)
	    (lock #t))
	  (send* main-media 
		 (set-autowrap-bitmap #f)
		 (auto-wrap #t)
		 (insert image-snip) (insert media-snip)
		 (change-style top 0 2)
		 (set-position 1)
		 (hide-caret #t)
		 (scroll-to-position 0)
		 (lock #t))
	  (send f reflow-container)
	  (send f min-width 550)
	  (send f min-height 400)
	  (send f show #t)
	  f))))
