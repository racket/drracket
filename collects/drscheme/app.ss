
  (unit/sig drscheme:app^
    (import [wx : wx^]
	    [mred : mred^]
	    [mzlib : mzlib:core^])
    
    (mred:debug:printf 'invoke "drscheme:application@")

    (define about-drscheme
      (lambda ()
	(let* ([names (string-append
		       "Gann Bierner, Corky Cartwright, Richard Cobbe, Moy Easwaran, "
		       "Matthias Felleisen, Robby Findler, Cormac Flanagan, Matthew Flatt, "
		       "Sebastian Good, Mark Krentel, Shriram Krishnamurthi, and Stephanie Weirich")]
	       [wrap-edit% 
		(class-asi mred:media-edit%
		  (inherit get-max-width find-snip  set-autowrap-bitmap position-location)
		  (rename [super-after-set-size-constraint after-set-size-constraint])
		  (public
		    [after-set-size-constraint
		     (lambda ()
		       (super-after-set-size-constraint)
		       (let ([width (get-max-width)])
			 (let* ([snip (find-snip 1 wx:const-snip-after-or-null)])
			   (when (is-a? snip wx:media-snip%)
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
					    (if (< (wx:display-depth) 8)
						"pltbw.gif"
						"plt.gif"))])
		  (if (file-exists? filename)
		      (make-object wx:image-snip% 
				   filename
				   wx:const-bitmap-type-gif)
		      (let ([i (make-object wx:text-snip%)])
			(send i insert "[lambda]")
			i)))]
	       [media-snip (make-object wx:media-snip% e #f)]
	       [f (make-object (class-asi mred:standard-menus-frame%
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
				   [edit-menu:copy (edit-menu:do wx:const-edit-copy)]
				   [edit-menu:select-all (edit-menu:do wx:const-edit-select-all)]
				   [edit-menu:find #f]
				   [help-menu:about (lambda () (about-drscheme))]
				   [help-menu:about-string "DrScheme"]))
			       '() "About DrScheme")]
	       [p (ivar f panel)]
	       [c (make-object mred:wrapping-canvas% p)]
	       [top (make-object wx:style-delta% wx:const-change-alignment wx:const-align-top)]
	       [d-usual (make-object wx:style-delta% wx:const-change-family wx:const-decorative)]
	       [d-dr (make-object wx:style-delta%)]
	       [d-http (make-object wx:style-delta%)])
	  (send* d-http 
	    (copy d-usual)
	    (set-delta-foreground "BLUE")
	    (set-delta wx:const-change-underline 1))
	  (send* d-usual 
	    (set-delta-foreground "BLACK")
	    (set-delta wx:const-change-underline 0))
	  (send* p (user-min-width 600) (user-min-height 400))
	  (send* d-dr (copy d-usual) (set-delta wx:const-change-bold 0))
	  (send d-usual set-weight-on wx:const-normal)
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
		  (lambda args (make-object mred:hyper-view-frame% url))
		  d-http))
	  (send* e
	    (insert #\newline)
	    (change-style d-usual)
	    (insert "For licensing information see LICENSE, included with the plt release.")
	    (insert #\newline)
	    (insert "Based on:")
	    (insert #\newline)
	    (insert "  MrEd version ")
	    (insert (mred:version))
	    (insert ", Copyright (c) 1995-1998 PLT, Rice University (Matthew Flatt and Robert Bruce Findler)")
	    (insert #\newline)
	    (insert (mred:credits-proc "  "))
	    (lock #t))
	  (send* main-media 
		 (insert image-snip) (insert media-snip)
		 (change-style top 0 1)
		 (set-position 1)
		 (scroll-to-position 0)
		 (lock #t))
	  (send f show #t)
	  f))))
