(define start-file "file:D:/Program Files/PLT/collects/doc/mzscheme/index.htm")

(require-library "url.ss" "net")
(load-relative "html.ss")

(define history-limit 40)

(define-struct hyperlink (anchor-start 
			  anchor-end
			  url-string))

(define-struct hypertag (name position))

(define hyper-text-mixin
  (lambda (super%)
    (class super% (url)
      (inherit begin-edit-sequence end-edit-sequence lock erase clear-undos
	       change-style get-style-list set-modified auto-wrap get-view-size
	       find-snip get-snip-position set-clickback get-canvas
	       get-visible-position-range)

      (private
	[title #f]
	[htmling? #f]
	[reverse-links
	 (lambda ()
	   (set! hyperlinks-list (reverse! hyperlinks-list)))])
      
      (public  
	[hypertags-list (list (make-hypertag "top" 0))]
	[hyperlinks-list ()]
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
	[make-editor (lambda (url) (make-object (object-class this) url))]
	[get-url (lambda () url)]
	[goto-url
	 (lambda (url-string)
	   (let* ([url
		   (if (get-url)
		       (combine-url/relative 
			(get-url)
			url-string)
		       (string->url url-string))]
		  [e (make-editor url)]
		  [tag-pos (send e find-tag (url-fragment url))])
	     (set-page (list e (or tag-pos 0) (send e last-position)) #t)))]
	[set-page
	 (lambda (page notify?)
	   (let ([e (car page)]
		 [spos (cadr page)]
		 [epos (caddr page)]
		 [c (get-canvas)])
	     ; Pre-size the editor to avoid visible reflow
	     (let ([wbox (box 0)])
	       (get-view-size wbox (box 0))
	       (send e set-max-width (unbox wbox)))
	     (send e begin-edit-sequence)
	     (when notify?
	       (send (send c get-top-level-window) leaving-page (current-page) (list e 0 0)))
	     (send c set-editor e (zero? spos))
	     (send e scroll-to-position spos #f epos 'start)
	     (send e end-edit-sequence)
	     (when (positive? spos)
	       (send c refresh))))]
	[current-page
	 (lambda ()
	   (let ([sbox (box 0)]
		 [ebox (box 0)])
	     (get-visible-position-range sbox ebox)
	     (list this (unbox sbox) (unbox ebox))))]
	[make-clickback-funct
	 (lambda (url-string)
	   (lambda (edit start end)
	     (with-handlers ([void (lambda (x)
				     (message-box
				      "Error"
				      (format "Unable to find destination position ~s: ~a~n" 
					      url-string
					      (if (exn? x)
						  (exn-message x)
						  x))))])
	       (goto-url url-string))))]	
	[install-clickbacks 
	 (lambda ()
	   (let install-loop ([links-left hyperlinks-list])
	     (unless (null? links-left)
	       (set-clickback  (hyperlink-anchor-start (car links-left))
			       (hyperlink-anchor-end (car links-left))
			       (make-clickback-funct
				(hyperlink-url-string (car links-left))))
	       (install-loop (cdr links-left)))))]
	[get-title (lambda () (or title (url->string url)))]
	[set-title (lambda (t) (set! title t))])
      (public
	[hyper-delta (make-object style-delta% 'change-underline #t)])
      (sequence
	(let ([mult (send hyper-delta get-foreground-mult)]
	      [add (send hyper-delta get-foreground-add)])
	  (send mult set 0 0 0)
	  (send add set 0 0 255)))
      
      (public
	[add-h-link-style
	 (lambda ()
	   (let ([style-list (get-style-list)])
	     (send style-list replace-named-style  "h-link-style"
		   (send style-list find-or-create-style  
			 (send style-list find-named-style "Standard") 
			 hyper-delta))))]
	
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
	   (and (string? name)
		(ormap (lambda (x) (and (string=? name (hypertag-name x)) 
					(hypertag-position x)))
		       hypertags-list)))]
	[remove-tag 
	 (lambda (name)
	   (set! hypertags-list
		 (filter (lambda (x) (not (string=? name (hypertag-name x))))
			 hypertags-list)))]
	[add-link 
	 (lambda (start end url-string)
	   (let* ([new-link (make-hyperlink start end url-string)])
	     <(set-clickback start end 
			    (make-clickback-funct url-string))
	     (set! hyperlinks-list
		   (cons new-link hyperlinks-list))))])
      (sequence
	(super-init)
	(add-h-link-style)
	(let* ([p (get-pure-port url)])
	  (dynamic-wind (lambda ()
			  (begin-busy-cursor)
			  (begin-edit-sequence #f))
			(lambda () 
			  (set! htmling? #t)
			  (erase)
			  (clear-undos)
			  (html-convert p this))
			(lambda ()
			  (end-edit-sequence)
			  (end-busy-cursor)
			  (set! htmling? #f)
			  (close-input-port p)))
	  (reverse-links)
	  (set-modified #f)
	  (install-clickbacks)
	  (auto-wrap #t)
	  (lock #t))))))

(define hyper-text% (hyper-text-mixin text%))

(define (hyper-frame-mixin super%)
  (class super% args
    (inherit show)
    (sequence
      (apply super-init args))
    (private
      [past null] [future null]
      [hp (make-object horizontal-panel% this)]
      [back (make-object button% "< Back" hp
			 (lambda (b ev) 
			   (let ([page (car past)]
				 [e (send c get-editor)])
			     (set! future (cons (send e current-page) future))
			     (set! past (cdr past))
			     (update-buttons page)
			     (send (send c get-editor) set-page page #f))))]
      [forward (make-object button% "Forward >" hp
			 (lambda (b ev) 
			   (let ([page (car future)]
				 [e (send c get-editor)])
			     (set! past (cons (send e current-page) past))
			     (set! future (cdr future))
			     (update-buttons page)
			     (send e set-page page #f))))]
      [update-buttons (lambda (page)
			(send back enable (pair? past))
			(send forward enable (pair? future))
			(send choice clear)
			(for-each
			 (lambda (page)
			   (send choice append (send (car page) get-title)))
			 (append (reverse past)
				 (list page)
				 future))
			(send choice set-selection (length past)))]
      [choice (make-object choice% #f null hp
			   (lambda (ch e)
			     (let* ([e (send c get-editor)]
				    [l (append (reverse past)
					       (list (send e current-page))
					       future)]
				    [pos (send choice get-selection)])
			       (let loop ([l l][pre null][pos pos])
				 (cond
				  [(zero? pos)
				   (set! past pre)
				   (set! future (cdr l))
				   (update-buttons (car l))
				   (send e set-page (car l) #f)]
				  [else (loop (cdr l)
					      (cons (car l) pre)
					      (sub1 pos))])))))]
      [c (make-object editor-canvas% this)])
    (public
      [leaving-page (lambda (page new-page)
		      (set! future null)
		      (set! past (cons page past))
		      (when (> (length past) history-limit)
			(set! past
			      (let loop ([l past])
				(if (null? (cdr l))
				    null
				    (cons (car l) (loop (cdr l)))))))
		      (update-buttons new-page))])
    (sequence
      (send choice stretchable-width #t)
      (send hp stretchable-height #f)
      (send c set-editor (make-object hyper-text% (string->url start-file)))
      (update-buttons (send (send c get-editor) current-page))
      (show #t))))

(define f (make-object (hyper-frame-mixin frame%) "Browser" #f 400 300))
