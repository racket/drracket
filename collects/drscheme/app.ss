
  (unit/sig drscheme:app^
    (import [mred : mred-interfaces^]
	    [mzlib : mzlib:core^]
	    [fw : framework^])
    
    (define about-drscheme
      (lambda ()
	(let* ([names (string-append
		       "Gann Bierner, Corky Cartwright, Richard Cobbe, Moy Easwaran, "
		       "Matthias Felleisen, Robby Findler, Cormac Flanagan, Matthew Flatt, "
		       "Sebastian Good, Mark Krentel, Shriram Krishnamurthi, "
		       "Paul Steckler, and Stephanie Weirich")]
	       [wrap-edit% 
		(class-asi mred:text%
		  (inherit get-max-width find-snip  set-autowrap-bitmap position-location)
		  (rename [super-after-set-size-constraint after-set-size-constraint])
		  (public
		    [after-set-size-constraint
		     (lambda ()
		       (super-after-set-size-constraint)
		       (let ([width (get-max-width)])
			 (let* ([snip (find-snip 1 'after-or-none)])
			   (when (is-a? snip mred:editor-snip%)
			     (let ([b (box 0)]
				   [media (send snip get-this-media)])
			       (position-location 1 b null #f #t)
			       (send snip resize (if (< width 0)
						     width
						     (- width 4 (unbox b)))
				     17) ; smallest random number
			       (send snip set-max-height -1))))))]
		    [auto-set-wrap? #t]
		    [autowrap-bitmap null]))]
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
	       [f (make-object (class-asi fw:frame:standard-menus%
				 (private
				   [edit-menu:do 
				    (lambda (const)
				      (lambda () 
					(send main-media do-edit const)))])
				 (public
				   [get-panel%
				    (lambda ()
				      (class-asi mred:vertical-panel%
					(public
					  [default-x-stretch #t]
					  [default-y-stretch #t])))]
				   [file-menu:new #f]
				   [file-menu:revert #f]
				   [file-menu:save #f]
				   [file-menu:save-as #f]
				   [file-menu:between-close-and-quit (lambda (x) (void))]
				   [file-menu:between-open-and-save  (lambda (x) (void))]
				   [file-menu:between-print-and-close (lambda (x) (void))]
				   [file-menu:between-save-and-quit (lambda (x) (void))]
				   [edit-menu:between-redo-and-cut (lambda (x) (void))]
				   [edit-menu:between-select-all-and-preferences (lambda (x) (void))]
				   [edit-menu:between-select-all-and-find (lambda (x) (void))]
				   [edit-menu:copy (edit-menu:do 'copy)]
				   [edit-menu:select-all (edit-menu:do 'select-all)]
				   [edit-menu:find #f]
				   [help-menu:about (lambda () (about-drscheme))]
				   [help-menu:about-string "DrScheme"]))
			       '() "About DrScheme")]
	       [p (ivar f panel)]
	       [c (make-object mred:editor-canvas% p)]
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

	  (send* p (user-min-width 600) (user-min-height 400))
	  (send* d-dr (copy d-usual) (set-delta 'change-bold))
	  (send d-usual set-weight-on 'normal)
	  (send* c (set-media main-media) (stretchable-in-x #t) (stretchable-in-y #t))
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
		  (lambda args (mred:message-box "Browser not yet supported in DrScheme 100"))
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
	    ;(insert (mred:credits-proc "  "))
	    (lock #t))
	  (send* main-media 
		 (insert image-snip) (insert media-snip)
		 (change-style top 0 1)
		 (set-position 1)
		 (scroll-to-position 0)
		 (lock #t))
	  (send f show #t)
	  f))))
