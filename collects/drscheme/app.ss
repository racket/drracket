
(unit/sig drscheme:app^
  (import mred^
          [mzlib : mzlib:core^]
          framework^
          [drscheme:unit : drscheme:unit^]
          [drscheme:frame : drscheme:frame^]
	  [help-desk : help:drscheme-interface^])
  
  (define about-frame%
    (class (drscheme:frame:basics-mixin frame:standard-menus%) (main-text)
      (private
        [edit-menu:do 
         (lambda (const)
           (lambda (_1 _2)
             (send main-text do-edit-operation const)))])
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
  
  (define (check-new-version)
    (let ([this-version (version:version)]
          [last-version (preferences:get 'drscheme:last-version)])
      
      (when (or (not last-version)
                (not (equal? last-version this-version)))
        
        (about-drscheme))))
  
  (define (same-widths items)
    (let ([max-width (apply max (map (lambda (x) (send x get-width)) items))])
      (for-each (lambda (x) (send x min-width max-width)) items)))

  (define names
    (string-append
     "PLT is "
     "John Clements, Matthias Felleisen, Robby Findler, "
     "Cormac Flanagan, Matthew Flatt, "
     "Shriram Krishnamurthi, "
     "and "
     "Paul Steckler."))

  (define wrap-edit% 
    (class-asi text%
      (inherit begin-edit-sequence end-edit-sequence
	       get-max-width find-snip position-location)
      (rename [super-after-set-size-constraint after-set-size-constraint])
      (override
	[on-set-size-constraint
	 (lambda ()
	   (begin-edit-sequence)
	   (let ([snip (find-snip 1 'after-or-none)])
	     (when (is-a? snip editor-snip%)
	       (send (send snip get-editor) begin-edit-sequence))))]
	[after-set-size-constraint
	 (lambda ()
	   (super-after-set-size-constraint)
	   (let ([width (get-max-width)]
		 [snip (find-snip 1 'after-or-none)])
	     (when (is-a? snip editor-snip%)
	       (let ([b (box 0)])
		 (position-location 1 b #f #f #t)
		 (let ([new-width (- width 4 (unbox b))])
		   (when (> new-width 0)
		     (send snip resize new-width
			   17) ; smallest random number
		     (send snip set-max-height 'none))))
	       (send (send snip get-editor) end-edit-sequence)))
	   (end-edit-sequence))])))

  (define (about-drscheme)
    (let* ([e (make-object wrap-edit%)]
	   [main-text (make-object wrap-edit%)]
	   [plt-bitmap (make-object bitmap%
			 (build-path (collection-path "icons")
				     (if (< (get-display-depth) 8)
					 "pltbw.gif"
					 "plt.gif")))]
	   [plt-icon (if (send plt-bitmap ok?)
			 (make-object image-snip% plt-bitmap)
			 (let ([i (make-object string-snip%)])
			   (send i insert "[lambda]")
			   i))]
	   [editor-snip (make-object editor-snip% e #f)]
	   [f (make-object about-frame% main-text)]
	   [main-panel (send f get-area-container)]
	   [editor-canvas (make-object editor-canvas% main-panel)]
	   [button-panel (make-object horizontal-panel% main-panel)]
	   [top (make-object style-delta% 'change-alignment 'top)]
	   [d-usual (make-object style-delta% 'change-family 'decorative)]
	   [d-dr (make-object style-delta%)]
	   [d-http (make-object style-delta%)]

	   [this-version (version:version)]
           [last-version (preferences:get 'drscheme:last-version)]

	   [insert-url
	    (lambda (str url)
	      (send e change-style d-http)
	      (let* ([before (send e get-start-position)]
		     [_ (send e insert str)]
		     [after (send e get-start-position)])
		(send e set-clickback before after 
		      (lambda args (help-desk:open-url url))
		      d-http))
	      (send e change-style d-usual))])


	   
      (send* d-http 
	     (copy d-usual)
	     (set-delta-foreground "BLUE")
	     (set-delta 'change-underline #t))
      (send* d-usual 
	     (set-delta-foreground "BLACK")
	     (set-delta 'change-underline #f))
      
      (send* d-dr (copy d-usual) (set-delta 'change-bold))
      (send d-usual set-weight-on 'normal)
      (send* editor-canvas
	     (set-editor main-text)
	     (stretchable-width #f)
	     (stretchable-height #f))

      ;; 50 is close enough to the space
      (if (send plt-bitmap ok?)
	  (send* editor-canvas
		 (min-width (+ (* 2 (send plt-bitmap get-width)) 50))
		 (min-height (+ (send plt-bitmap get-height) 50)))
	  (send* editor-canvas
		 (min-width 500)
		 (min-height 400)))

      (send* e 
	     (change-style d-dr)
	     (insert (format "Welcome to DrScheme version ~a"this-version))
	     (change-style d-usual))

      (when (and last-version 
                 (not (equal? this-version last-version)))
	(send e insert (format " (previous version ~a)" last-version)))

      (send e insert " by ")

      (insert-url "PLT, Rice University"
		  "http://www.cs.rice.edu/CS/PLT/")

      (send* e
	     (insert ".")
	     (insert #\newline)
	     (insert names)
	     (insert #\newline)
	     (insert "For licensing information see "))

      (let ([copying.lib
	     (mzlib:file:normalize-path
	      (build-path (collection-path "mzlib")
			  'up
			  'up
			  "notes"
			  "COPYING.LIB"))])
	(insert-url "COPYING.LIB" (string-append "file:" copying.lib)))

      (send* e
	     (insert ".")
	     (insert #\newline)
	     (insert "Based on:")
	     (insert #\newline)
	     (insert "  MrEd version ")
	     (insert (version))
	     (insert ", Copyright (c) 1995-1999 PLT, Rice University (Matthew Flatt and Robert Bruce Findler)")
	     (insert #\newline)
	     (insert "  McMicMac (c) 1995-1998 PLT, Rice University (Shriram Krishnamurthi)")
	     (insert #\newline)
	     (auto-wrap #t)
	     (set-autowrap-bitmap #f)
	     (lock #t))
      (send* main-text 
	     (set-autowrap-bitmap #f)
	     (auto-wrap #t)
	     (insert plt-icon)
	     (insert editor-snip)
	     (change-style top 0 2)
	     (set-position 1)
	     (hide-caret #t)
	     (scroll-to-position 0)
	     (lock #t))

      (preferences:set 'drscheme:last-version this-version)
      
      (let* ([tour-button
	      (make-object button% "Take a Tour!" button-panel
			   (lambda x 
			     (help-desk:open-url
			      (string-append
			       "file:"
			       (build-path (collection-path "doc" "help" "tour")
					   "index.html"))))
			   '(border))]
	     [release-notes-button
	      (make-object button% "Release Notes" button-panel
			   (lambda x 
			     (help-desk:open-url 
			      (string-append
			       "file:"
			       (build-path (collection-path "doc" "help" "release")
					   "notes.html")))))])
        (same-widths (list tour-button release-notes-button))
	(send tour-button focus))
      (send button-panel stretchable-height #f)
      (send button-panel set-alignment 'center 'center)
      (send f show #t)
      f)))
