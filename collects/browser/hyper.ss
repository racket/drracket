
(unit/sig browser^
  (import browser:html^
	  mzlib:function^
	  mzlib:file^
	  mzlib:string^
	  mzlib:url^
	  mred^)

  (define-struct (exn:file-saved-instead struct:exn) (pathname))
  (define-struct (exn:cancelled struct:exn) ())

  (define history-limit 20)

  (define-struct hyperlink (anchor-start 
			    anchor-end
			    url-string))

  (define-struct hypertag (name position))

#|
  (define scrolling-canvas%
    (class canvas% (loi frame)
      (private
	[bitmaps
	 (map (lambda (i)
		(cond
		 [(string? i)
		  (make-object bitmap% i 'gif)]
		 [(is-a? i bitmap%) i]
		 [else (error 'scrolling-canvas% "expected a list of bitmaps or filenames, got: ~e"
			      i)]))
	      loi)]
	[current-bitmap (if (null? bitmaps)
			    (error 'scrolling-canvas%
				   "expected at least one bitmap, got none")
			    (car bitmaps))]

	[width (apply max (map (lambda (x) (send x get-width)) bitmaps))]
	[height (apply max (map (lambda (x) (send x get-height)) bitmaps))])
      
      (inherit get-dc)
      (override
       [on-paint
	(lambda ()
	  (let ([dc (get-dc)])
	    (send dc clear)
	    (send dc draw-bitmap current-bitmap
		  (floor (- (/ width 2) (/ (send current-bitmap get-width) 2)))
		  (floor (- (/ height 2) (/ (send current-bitmap get-height) 2))))))])

      (private
	[thread-desc #f])
      (public
	[start
	 (lambda ()
	   (unless thread-desc
	     (set! thread-desc
		   (thread
		    (letrec ([f
			      (lambda (first?)
				(with-handlers ([void void])
				  (let loop ([l (if first?
						    (cdr bitmaps)
						    bitmaps)])
				    (cond
				     [(null? l) (f #f)]
				     [else
				      (queue-callback
				       (lambda ()
					 (set! current-bitmap (car l))
					 (on-paint)))
				      (sleep 2/3)
				      (loop (cdr l))]))))])
		      (lambda ()
			(f #t)))))))]
	[stop
	 (lambda ()
	   (when thread-desc
	     (break-thread thread-desc)
	     (queue-callback
	      (lambda ()
		(set! current-bitmap (car bitmaps))
		(on-paint)))
	     (set! thread-desc #f)))])

      (inherit min-width min-height stretchable-width stretchable-height)
      (sequence
	(super-init frame)
	(min-width width)
	(min-height height)
	(stretchable-width #f)
	(stretchable-height #f))))

  (define bitmaps
    (let* ([images
	    (list "mini-plt.gif"
		  "animate1.gif"
		  "animate2.gif"
		  "animate3.gif")])
      (map 
       (lambda (x)
	 (make-object bitmap%
	   (build-path (collection-path "icons") x)
	   'gif))
       images)))

|#

  (define (run-installer file)
    (letrec ([f (make-object (class dialog% ()
			       (override
				 [can-close? (lambda () (send done is-enabled?))])
			       (sequence
				 (super-init "Install Progress"
					     #f 400 300))))]
	     [c (make-object editor-canvas% f)]
	     [e (make-object text%)]
	     [s (make-semaphore)]
	     [done (make-object button% "Ok" f (lambda (b e) 
						 (semaphore-post s)))]
	     [output (make-output-port
		      (lambda (s)
			(send e lock #f)
			(send e insert s (send e last-position) 'same 
			      ; Scroll on newlines only:
			      (regexp-match (string #\newline) s))
			(send e lock #t))
		      void)]
	     [cust (make-custodian)])
      (send done enable #f)
      (send e lock #t)
      (send c set-editor e)
      (let ([t (parameterize ([current-custodian cust])
		 (thread
		  (lambda ()
		    (current-output-port output)
		    (current-error-port output)
		    (parameterize ([current-namespace (make-namespace 'mred)]
				   [exit-handler (lambda (v) (custodian-shutdown-all cust))])
		      (printf "Loading installer...~n")
		      (global-defined-value 'argv (vector file))
		      (require-library "setup.ss" "compiler")))))])
	(thread (lambda () (send f show #t) (semaphore-post s)))
	(thread (lambda () 
		  (thread-wait t) 
		  (semaphore-post s)))
	(yield s)
	(custodian-shutdown-all cust)
	(end-busy-cursor)
	(send done enable #t)
	(fprintf output "(Click Ok to close this progess window.)~n")
	(send e lock #f)
	(send e change-style
	      (make-object style-delta% 'change-bold)
	      (send e line-start-position (sub1 (send e last-line)))
	      (send e last-position))
	(yield s)
	(begin-busy-cursor)
	(send f show #f)
	(yield s))))

  (define hyper-text-mixin
    (lambda (super%)
      (class super% (url top-level-window . args)
	(inherit begin-edit-sequence end-edit-sequence lock erase clear-undos
		 change-style get-style-list set-modified auto-wrap get-view-size
		 find-snip get-snip-position set-clickback get-canvas
		 get-visible-position-range insert last-position hide-caret
		 get-end-position)
	(rename [super-after-set-position after-set-position])

	(override
	 [after-set-position
	  (lambda ()
	    (unless (zero? (get-end-position))
	      (hide-caret #f))
	    (super-after-set-position))])
	(private
	  [doc-notes null]
	  [title #f]
	  [htmling? #f]
	  [hypertags-list (list (make-hypertag "top" 0))])

	(public
	  [add-document-note
	   (lambda (note)
	     (set! doc-notes (append doc-notes (list note))))]
	  [get-document-notes
	   (lambda () doc-notes)]

	  [map-shift-style 
	   (lambda (start end shift-style)
	     (let loop ([pos start])
	       (unless (>= pos end)
		 (let* ([curr-snip (find-snip pos 'after-or-none)]
			[curr-snip-end (when curr-snip
					 (+ (get-snip-position curr-snip)
					    (send curr-snip get-count)))])
		   (when curr-snip
		     (change-style 
		      (send (get-style-list) find-or-create-join-style 
			    (send curr-snip get-style) shift-style)
		      pos (min curr-snip-end end))
		     (loop curr-snip-end))))))]
	  [make-link-style       
	   (lambda (start end)
	     (map-shift-style start end 
			      (send (get-style-list) find-named-style "h-link-style")))]
	  [get-url (lambda () (and (url? url) url))])
	(private
	  [make-clickback-funct
	   (lambda (url-string)
	     (lambda (edit start end)
	       (on-url-click
		(lambda (url-string)
		  (with-handlers ([void (lambda (x)
					  (unless (or (exn:misc:user-break? x)
						      (exn:file-saved-instead? x)
						      (exn:cancelled? x))
					    (message-box
					     "Error"
					     (format "Cannot display ~s: ~a~n" 
						     url-string
						     (if (exn? x)
							 (exn-message x)
							 x)))))])
		    (send (get-canvas) goto-url url-string (get-url))))
		url-string)))])
	(public
	  [on-url-click (lambda (f x) 
			  (let ([c (get-canvas)])
			    (if c
				(send c on-url-click f x)
				(f x))))]
	  [get-title (lambda () (or title (and (url? url) (url->string url))))]
	  [set-title (lambda (t) (set! title t))])
	(public
	  [hyper-delta (make-object style-delta% 'change-underline #t)])
	(sequence
	  (let ([mult (send hyper-delta get-foreground-mult)]
		[add (send hyper-delta get-foreground-add)])
	    (send mult set 0 0 0)
	    (send add set 0 0 255)))
	
	(private
	  [add-h-link-style
	   (lambda ()
	     (let ([style-list (get-style-list)])
	       (send style-list replace-named-style  "h-link-style"
		     (send style-list find-or-create-style  
			   (send style-list find-named-style "Standard") 
			   hyper-delta))))])

	(public
	  [add-tag 
	   (lambda (name pos)
	     (for-each (lambda (tag) 
			 (when (string=? name (hypertag-name tag))
			   (remove-tag  name)))
		       hypertags-list)
	     (let ([new-tag (make-hypertag name pos)])
	       (set! hypertags-list
		     (let insert-loop ([tags-left hypertags-list])
		       (cond [(null? tags-left)(cons new-tag ())]
			     [(> pos (hypertag-position (car tags-left)))
			      (cons new-tag tags-left)]
			     [else (cons (car tags-left)
					 (insert-loop (cdr tags-left)))])))))]
	  [find-tag
	   (lambda (name)
	     (if (and (integer? name) (positive? name))
		 name
		 (and (string? name)
		      (ormap (lambda (x) (and (string=? name (hypertag-name x)) 
					      (hypertag-position x)))
			     hypertags-list))))]
	  [remove-tag 
	   (lambda (name)
	     (set! hypertags-list
		   (filter (lambda (x) (not (string=? name (hypertag-name x))))
			   hypertags-list)))]
	  [add-link 
	   (lambda (start end url-string)
	     (let* ([new-link (make-hyperlink start end url-string)])
	       (set-clickback start end (make-clickback-funct url-string))))]
	  [add-scheme-callback
	   (lambda (start end scheme-string)
	     (set-clickback start end (lambda (edit start end)
					(eval-scheme-string scheme-string))))]
	  [eval-scheme-string
	   (lambda (s)
	     (let ([v (eval-string s)])
	       (when (string? v)
		 (send (get-canvas) goto-url (open-input-string v) (get-url)))))])
	(sequence
	  (apply super-init args)
	  (add-h-link-style)
	  (when url
	    (let-values ([(p mime-headers)
			  (if (port? url)
			      (values url null)
			      ; Try to get mime info, but use get-pure-port if that fails:
			      (with-handlers ([void (lambda (x) (values (get-pure-port url) null))])
				(let ([p (get-impure-port url)])
				  (let ([headers (purify-port p)])
				    (values p headers)))))])
	      (dynamic-wind (lambda ()
			      (begin-busy-cursor)
			      ; (send progress start)
			      (begin-edit-sequence #f))
			    (lambda () 
			      (set! htmling? #t)
			      (erase)
			      (clear-undos)
			      (let ([mime-type (ormap (lambda (mh)
							(and (string=? (mime-header-name mh) "content-type")
							     (mime-header-value mh)))
						      mime-headers)])
				(cond
				 [(or (and mime-type (regexp-match "application/" mime-type))
				      (and (url? url)
					   (regexp-match "[.]plt$" (url-path url))))
				  ; Save the file
				  (end-busy-cursor)
				  (let* ([orig-name (and (url? url)
							   (let ([m (regexp-match "([^/]*)$" (url-path url))])
							     (and m (cadr m))))]
					 [size (ormap (lambda (mh)
							(and (string=? (mime-header-name mh) "content-length")
							     (let ([m (regexp-match "[0-9]+" (mime-header-value mh))])
							       (and m (string->number (car m))))))
						      mime-headers)]
					 [install? (and (and orig-name (regexp-match "[.]plt$" orig-name))
							(let ([d (make-object dialog% "Install?")]
							      [d? #f]
							      [i? #f])
							  (make-object message% "You have selected an installable package." d)
							  (make-object message% "Do you want to install it?" d)
							  (when size
							    (make-object message% (format "(The file is ~a bytes)" size) d))
							  (let ([hp (make-object horizontal-panel% d)])
							    (send hp set-alignment 'center 'center)
							    (send (make-object button% "Download && Install" hp
									       (lambda (b e) (set! i? #t) (send d show #f))
									       '(border))
								  focus)
							    (make-object button% "Download" hp
									 (lambda (b e) (set! d? #t) (send d show #f)))
							    (make-object button% "Cancel" hp
									 (lambda (b e) (send d show #f))))
							  (send d center)
							  (send d show #t)
							  (unless (or d? i?)
							    (raise (make-exn:cancelled "Package cancelled"
										       (current-continuation-marks))))
							  i?))]
					 [f (if install?
						(make-temporary-file "tmp~a.plt")
						(put-file (format "Save downloaded file~a as"
								  (if size
								      (format " (~a bytes)" size)
								      ""))
							  #f ; should be calling window!
							  #f
							  orig-name))])
				    (begin-busy-cursor)
				    (when f
				      (let* ([d (make-object dialog% "Downloading" top-level-window)]
					     [message (make-object message% "Downloading file..." d)]
					     [gauge (if size
							(make-object gauge% #f 100 d)
							#f)]
					     [exn #f]
					     ; Semaphores to avoid race conditions:
					     [wait-to-start (make-semaphore 0)]
					     [wait-to-break (make-semaphore 0)]
					     ; Thread to perform the download:
					     [t (thread
						 (lambda ()
						   (semaphore-wait wait-to-start)
						   (with-handlers ([void
								    (lambda (x) (when (not (exn:misc:user-break? x))
										  (set! exn x)))])
						     (semaphore-post wait-to-break)
						     (with-output-to-file f
						       (lambda ()
							 (let loop ([total 0])
							   (when gauge
							     (send gauge set-value 
								   (inexact->exact (floor (* 100 (/ total size))))))
							   (let ([s (read-string 1024 p)])
							     (unless (eof-object? s)
							       (display s)
							       (loop (+ total (string-length s)))))))
						       'binary 'truncate))
						   (send d show #f)))])
					(send d center)
					(make-object button% "&Stop" d (lambda (b e)
									 (semaphore-wait wait-to-break)
									 (set! f #f)
									 (send d show #f)
									 (break-thread t)))
					; Let thread run only after the dialog is shown
					(queue-callback (lambda () (semaphore-post wait-to-start)))
					(send d show #t)
					(when exn (raise exn)))
				      (when (and f install?)
					(run-installer f)
					(delete-file f)))
				    (raise
				     (if f
					 (make-exn:file-saved-instead
					  (if install?
					      "The package was installed."
					      "The downloaded file was saved.")
					  (current-continuation-marks)
					  f)
					 (make-exn:cancelled "The download was cancelled."
							     (current-continuation-marks)))))]
				 [(or (port? url)
				      (and (url? url)
					   (regexp-match "[.]html?$" (url-path url)))
				      (and mime-type
					   (regexp-match "text/html" mime-type)))
				  ; HTML
				  (let* ([d #f]
					 [e #f]
					 [e-text ""]
					 [exn #f]
					 [done? #f]
					 ; Semaphores to avoid race conditions:
					 [wait-to-continue (make-semaphore 0)]
					 [wait-to-break (make-semaphore 0)]
					 [wait-to-show (make-semaphore 1)]
					 ; Thread to perform the download:
					 [t (parameterize ([break-enabled #f])
					      (thread
					       (lambda ()
						 (with-handlers ([void (lambda (x) (set! exn x))])
						   (parameterize ([break-enabled #t])
						     (semaphore-post wait-to-break)
						     (parameterize ([html-status-handler
								     (lambda (s) 
								       (set! e-text s)
								       (semaphore-wait wait-to-show)
								       (when e
									 (send e erase)
									 (send e insert s))
								       (semaphore-post wait-to-show))])
						       (html-convert p this))))
						 (set! done? #t)
						 (semaphore-wait wait-to-show)
						 (when d
						   (send d show #f))
						 (semaphore-post wait-to-show)
						 (semaphore-post wait-to-continue))))]
					 [make-dialog
					  (lambda ()
					    (semaphore-wait wait-to-show)
					    (unless done?
					      (set! d (make-object dialog% "Getting Page" top-level-window 400))
					      (let ([c (make-object editor-canvas% d #f '(no-hscroll no-vscroll))])
						(set! e (make-object text%))
						(send e insert e-text)
						(send e auto-wrap #t)
						(send c set-editor e)
						(send c set-line-count 3)
						(send c enable #f))
					      (send (make-object button% "&Stop" d (lambda (b e)
										     (semaphore-wait wait-to-break)
										     (semaphore-post wait-to-break)
										     (break-thread t)))
						    focus)
					      (send d center)
					      (thread (lambda () (send d show #t)))
					      (let loop () (sleep) (unless (send d is-shown?) (loop)))
					      (semaphore-post wait-to-show)))])
				    (thread (lambda () (sleep 1) (unless done? (semaphore-post wait-to-continue))))
				    (semaphore-wait wait-to-continue)
				    (unless done?
				      (make-dialog)
				      (yield wait-to-continue))
				    (when exn (raise exn)))]
				 [else
				  ; Text
				  (begin-edit-sequence)
				  (let loop ()
				    (let ([r (read-line p 'any)])
				      (unless (eof-object? r)
					(insert r)
					(insert #\newline)
					(loop))))
				  (change-style (make-object style-delta% 'change-family 'modern)
						0 (last-position))
				  (end-edit-sequence)])))
			    (lambda ()
			      (end-edit-sequence)
			      (end-busy-cursor)
			      ; (send progress stop)
			      (set! htmling? #f)
			      (close-input-port p)))
	      (set-modified #f)
	      (auto-wrap #t)
	      (lock #t)))))))

  (define hyper-text% (hyper-text-mixin text%))

  (define (hyper-canvas-mixin super%)
    (class super% args
      (inherit get-editor set-editor refresh get-parent get-top-level-window)
      (public
	[make-editor (lambda (url) (make-object hyper-text% url (get-top-level-window)))]
	[current-page
	 (lambda ()
	   (let ([e (get-editor)])
	     (and e 
		  (let ([sbox (box 0)]
			[ebox (box 0)])
		    (send e get-visible-position-range sbox ebox)
		    (list e (unbox sbox) (unbox ebox))))))]
	[on-url-click (lambda (k url) (send (get-parent) on-url-click k url))]
	[goto-url
	 (lambda (url relative)
	   (let* ([url (if (or (url? url) (port? url))
			   url
			   (if relative
			       (combine-url/relative 
				relative
				url)
			       (string->url url)))]
		  [e (make-editor url)]
		  [tag-pos (send e find-tag (and (url? url) (url-fragment url)))])
	     (when tag-pos
	       (send e set-position tag-pos))
	     (unless (and tag-pos (positive? tag-pos))
	       (send e hide-caret #t))
	     (set-page (list e (or tag-pos 0) (send e last-position)) #t)))]
	[set-page
	 (lambda (page notify?)
	   (let ([e (car page)]
		 [spos (cadr page)]
		 [epos (caddr page)]
		 [curr (get-editor)]
		 [current (current-page)])
	     ; Pre-size the editor to avoid visible reflow
	     (when curr
	       (let ([wbox (box 0)])
		 (send curr get-view-size wbox (box 0))
		 (when (send e auto-wrap)
		   (send e set-max-width (unbox wbox)))))
	     (send e begin-edit-sequence)
	     (when notify?
	       (send (get-parent) leaving-page current (list e 0 0)))
	     (set-editor e (and current (zero? (cadr current)) (zero? spos)))
	     (send e scroll-to-position spos #f epos 'start)
	     (send e end-edit-sequence)
	     (when (or (positive? spos) (not current) (positive? (cadr current)))
	       (refresh))))])
      (sequence
	(apply super-init args))))

  (define hyper-canvas% (hyper-canvas-mixin editor-canvas%))

  (define info-canvas%
    (class canvas% (parent)
      (inherit min-client-height get-dc stretchable-height
	       enable refresh show)
      (private
	[text ""])
      (override
	[on-paint
	 (lambda ()
	   (let ([dc (get-dc)])
	     (send dc clear)
	     (send dc draw-text text 4 2)))])
      (public
	[erase-info (lambda ()
		      (unless (string=? text "")
			(set! text "")
			(let ([dc (get-dc)])
			  (send dc clear))))]
	[set-info (lambda (t)
		    (set! text t)
		    (if (string=? t "")
			(show #f)
			(let ([dc (get-dc)])
			  (send dc clear)
			  (show #t)
			  (refresh))))])
      (sequence 
	(super-init parent)
	(stretchable-height #f)
	(enable #f)
	(show #f)
	(let ([font (make-object font% 
				 (send (send parent get-label-font) get-point-size) 
				 'default 'normal 'normal)]
	      [dc (get-dc)])
	  (send dc set-font font)
	  (send dc set-text-foreground (make-object color% "FOREST GREEN"))
	  (let-values ([(w h d a) (send dc get-text-extent "X" font)])
	    (min-client-height (+ 4 (inexact->exact (ceiling h)))))))))

  (define (hyper-panel-mixin super%)
    (class super% (info-line? . args)
      (inherit reflow-container)
      (sequence (apply super-init args))
      (private
	[clear-info (lambda () 
		      (when info (send info erase-info)))]
	[update-info (lambda (page) 
		       (when (and info page)
			 (let ([notes (send (page->editor page) get-document-notes)])
			   (send info set-info (filter-notes notes)))))]
	[go (lambda (page)
	      (clear-info)
	      (update-buttons page)
	      (send c set-page page #f)
	      (update-info page)
	      (on-navigate))])
      (public
	[rewind 
	 (lambda ()
	   (unless (null? past)
	     (let ([page (car past)])
	       (set! future (cons (send c current-page) future))
	       (set! past (cdr past))
	       (go page))))]
	[forward
	 (lambda ()
	   (unless (null? future)
	     (let ([page (car future)])
	       (set! past (cons (send c current-page) past))
	       (set! future (cdr future))
	       (go page))))]
	[make-canvas (lambda () (make-object hyper-canvas% this))])
      (private
	[past null] [future null] [init-page #f]
	[hp (make-object horizontal-panel% this)]
	[back (make-object button% "< Rewind" hp
			   (lambda (b ev) 
			     (rewind)))]
	[forw (make-object button% "Forward >" hp
			   (lambda (b ev) 
			     (forward)))]
	[home (make-object button% "Home" hp
			   (lambda (b ev)
			     (when init-page
			       (send c set-page init-page #t))))]
	[update-buttons (lambda (page)
			  (unless init-page
			    (set! init-page page))
			  (send back enable (pair? past))
			  (send forw enable (pair? future))
			  (send home enable (and init-page
						 (not (same-page? init-page page))))
			  (send choice clear)
			  (for-each
			   (lambda (p)
			     (send choice append 
				   (let ([s (send (car p) get-title)])
				     (or s "Untitled"))))
			   (append (reverse future)
				   (if page (list page) null)
				   past))
			  (let ([c (send choice get-number)])
			    (unless (zero? c)
			      (send choice set-selection (length future)))))]
	[choice (make-object choice% #f null hp
			     (lambda (ch e)
			       (let* ([l (append (reverse past)
						 (list (send c current-page))
						 future)]
				      [pos (- (send choice get-number) (send choice get-selection) 1)])
				 (let loop ([l l][pre null][pos pos])
				   (cond
				    [(zero? pos)
				     (set! past pre)
				     (set! future (cdr l))
				     (go (car l))]
				    [else (loop (cdr l)
						(cons (car l) pre)
						(sub1 pos))])))))]
	; [progress (make-object scrolling-canvas% bitmaps hp)]
	[info (and info-line?
		   (make-object info-canvas% this))]
	[c (make-canvas)])
      (public
	; [get-progress (lambda () progress)]
	[on-navigate void]
	[filter-notes (lambda (l) (apply string-append l))]
	[get-canvas (lambda () c)]
	[on-url-click (lambda (k url) (k url))]
	[leaving-page (lambda (page new-page)
			(set! future null)
			(when page
			  (set! past (cons page past)))
			(when (> (length past) history-limit)
			  (set! past
				(let loop ([l past])
				  (if (null? (cdr l))
				      null
				      (cons (car l) (loop (cdr l)))))))
			(clear-info)
			(update-buttons new-page)
			(update-info new-page))])
      (sequence
	(send choice stretchable-width #t)
	(send hp stretchable-height #f)
	(update-buttons #f))))

  (define hyper-panel% (hyper-panel-mixin vertical-panel%))

  (define (hyper-frame-mixin super%)
    (class super% (start-url . args)
      (inherit show)
      (sequence 
	(apply super-init args)
	(let ([p (make-object hyper-panel% this)])
	  (show #t)
	  (send (send p get-canvas) goto-url start-url #f)))))

  (define hyper-frame% (hyper-frame-mixin frame%))

  (define (editor->page e) (list e 0 0))
  (define (page->editor e) (car e))

  (define (same-page? a b)
    (eq? (car a) (car b)))

  (define (open-url file)
    (make-object hyper-frame% file "Browser" #f 500 450)))