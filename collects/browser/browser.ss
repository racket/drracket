
(require-library "url.ss" "net")
(load-relative "html.ss")

(define history-limit 20)

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
	[htmling? #f])
      
      (public  
	[hypertags-list (list (make-hypertag "top" 0))]
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
	[get-url (lambda () (and (url? url) url))]
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
	       (send (get-canvas) goto-url url-string (get-url)))))]	
	[get-title (lambda () (or title (and (url? url) (url->string url))))]
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
	(super-init)
	(add-h-link-style)
	(when url
	  (let* ([p (if (port? url)
			url
			(get-pure-port url))])
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
	    (set-modified #f)
	    (auto-wrap #t)
	    (lock #t)))))))

(define hyper-text% (hyper-text-mixin text%))

(define (hyper-canvas-mixin super%)
  (class super% args
    (inherit get-editor set-editor refresh get-parent)
    (public
      [make-editor (lambda (url) (make-object hyper-text% url))]
      [current-page
       (lambda ()
	 (let ([e (get-editor)])
	   (and e 
		(let ([sbox (box 0)]
		      [ebox (box 0)])
		  (send e get-visible-position-range sbox ebox)
		  (list e (unbox sbox) (unbox ebox))))))]
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

(define (hyper-panel-mixin super%)
  (class super% args
    (inherit reflow-container)
    (sequence (apply super-init args))
    (private
      [past null] [future null]
      [hp (make-object horizontal-panel% this)]
      [back (make-object button% "< Rewind" hp
			 (lambda (b ev) 
			   (let ([page (car past)])
			     (set! future (cons (send c current-page) future))
			     (set! past (cdr past))
			     (update-buttons page)
			     (send c set-page page #f))))]
      [forward (make-object button% "Forward >" hp
			 (lambda (b ev) 
			   (let ([page (car future)])
			     (set! past (cons (send c current-page) past))
			     (set! future (cdr future))
			     (update-buttons page)
			     (send c set-page page #f))))]
      [update-buttons (lambda (page)
			(send back enable (pair? past))
			(send forward enable (pair? future))
			(send choice clear)
			(for-each
			 (lambda (p)
			   (send choice append 
				 (let ([s (send (car p) get-title)])
				   (or s "Untitled"))))
			 (append (reverse past)
				 (if page (list page) null)
				 future))
			(let ([c (send choice get-number)])
			  (unless (zero? c)
			    (send choice set-selection (length past)))))]
      [choice (make-object choice% #f null hp
			   (lambda (ch e)
			     (let* ([l (append (reverse past)
					       (list (send c current-page))
					       future)]
				    [pos (send choice get-selection)])
			       (let loop ([l l][pre null][pos pos])
				 (cond
				  [(zero? pos)
				   (set! past pre)
				   (set! future (cdr l))
				   (update-buttons (car l))
				   (send c set-page (car l) #f)]
				  [else (loop (cdr l)
					      (cons (car l) pre)
					      (sub1 pos))])))))]
      [c (make-object hyper-canvas% this)])
    (public
      [get-canvas (lambda () c)]
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
		      (update-buttons new-page))])
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

(define (editor->page e) (list e 0 0))

(define (open-url file)
  (make-object (hyper-frame-mixin frame%) file "Browser" #f 500 450))
