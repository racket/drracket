
(unit/sig browser:html^
  (import mzlib:file^
	  mzlib:string^
	  relative-btree^
	  mzlib:url^
	  bullet-snip^
	  mred^)
    
  (define NUM-CACHED 10)
  (define cached (make-vector 10 null))
  (define cached-name (make-vector 10 ""))
  (define cached-use (make-vector 10 0))

  (define html-status-handler
    (make-parameter
     void
     (lambda (f)
       (unless (and (procedure? f)
		    (procedure-arity-includes? f 1))
	 (raise-type-error 'html-status-handler
			   "procedure of arity 1"
			   f))
       f)))

  (define (status . args)
    ((html-status-handler) (apply format args)))

  (define status-stack (make-parameter null))

  (define (load-status push? what url)
    (let ([s (format "Loading ~a ~a~a..." what 
		     (or (and url (url-host url)) "") 
		     (or (and url (url-path url)) ""))])
      (status-stack (cons s (if push? (status-stack) null)))
      (status "~a" s)))
  (define (pop-status)
    (status-stack (cdr (status-stack)))
    (status "~a" (car (status-stack))))
  
  (define (not-break x) (not (exn:misc:user-break? x)))

  (define (call-with-output-file* file proc flag)
    ; Closes on escape
    (let ([p (open-output-file file flag)])
      (dynamic-wind
       void
       (lambda () (proc p))
       (lambda () (close-output-port p)))))

  (define get-image-from-url
    (lambda (url)
      (let ([tmp-filename (make-temporary-file "mredimg~a")])
	(load-status #t "image" url)
	(call-with-output-file* tmp-filename
	  (lambda (op)
	    (with-handlers ([not-break void])
	      (call/input-url 
	       url
	       get-pure-port
	       (lambda (ip)
		 (let loop ()
		   (let ([c (read-char ip)])
		     (unless (eof-object? c)
		       (write-char c op)
		       (loop))))))))
	  'truncate)
	(pop-status)
	(let* ([upath (url-path url)]
	       [bitmap (make-object bitmap% tmp-filename)])
	  (delete-file tmp-filename)
	  (if (send bitmap ok?)
	      (let ([is (make-object image-snip% #f)])
		(send is set-bitmap bitmap)
		is)
	      #f)))))

  (define cache-image
    (lambda (url)
      (let loop ([n 0])
	(cond
	 [(= n NUM-CACHED)
	  ;; Look for item to uncache
	  (vector-set! cached-use 0 (max 0 (sub1 (vector-ref cached-use 0))))
	  (let ([m (let loop ([n 1][m (vector-ref cached-use 0)])
		     (if (= n NUM-CACHED)
			 m
			 (begin
			   (vector-set! cached-use n (max 0 (sub1 (vector-ref cached-use n))))
			   (loop (add1 n) (min m (vector-ref cached-use n))))))])
	    (let loop ([n 0])
	      (if (= (vector-ref cached-use n) m)
		  (let ([image (get-image-from-url url)])
		    (cond
		     [image
		      (vector-set! cached n image)
		      (vector-set! cached-name n url)
		      (vector-set! cached-use n 5)
		      (send image copy)]
		     [else #f]))
		  (loop (add1 n)))))]
	 [(equal? url (vector-ref cached-name n))
	  (vector-set! cached-use n (min 10 (add1 (vector-ref cached-use n))))
	  (send (vector-ref cached n) copy)]
	 [else
	  (loop (add1 n))]))))

  (define (make-get-field str)
    (let ([s (apply
	      string-append
	      (map
	       (lambda (c)
		 (format "[~a~a]"
			 (char-upcase c)
			 (char-downcase c)))
	       (string->list str)))]
	  [spc (string #\space #\tab #\newline #\return #\vtab)])
      (let ([re:plain (regexp (format "(^|[~a])~a[~a]*=[~a]*([^ ]*)" spc s spc spc))]
	    [re:quote (regexp (format "(^|[~a])~a[~a]*=[~a]*\"([^\"]*)\"" spc s spc spc))])
	(lambda (args)
	  (let ([m (or (regexp-match re:quote args)
		       (regexp-match re:plain args))])
	    (and m (caddr m)))))))

  (define html-convert
    (lambda (p b)
      (letrec 
	  ([indents (make-btree)]
	   
	   [normal-style (send (send b get-style-list)
			       find-named-style
			       "Standard")]
	   [get-character (ivar b get-character)]
	   [set-position (ivar b set-position)]
	   [insert (let ([insert (ivar b insert)])
		     (lambda (what pos)
		       (let ([len (if (string? what)
				      (string-length what)
				      1)])
			 (btree-shift! indents pos len))
		       (insert what pos)))]
	   [delete (let ([delete (ivar b delete)])
		     (lambda (start end)
		       (btree-shift! indents end (- start end))
		       (delete start end)))]
	   [get-text (ivar b get-text)]
	   [set-title (ivar b set-title)]
	   [set-clickback (ivar b set-clickback)]
	   [change-style (ivar b change-style)]
	   [last-position (ivar b last-position)]
	   [add-link (ivar b add-link)]
	   [add-scheme-callback (ivar b add-scheme-callback)]
	   [make-link-style (ivar b make-link-style)]
	   [add-tag (ivar b add-tag)]
	   [get-url (ivar b get-url)]
	   [set-modified (ivar b set-modified)]
	   [find-first-snip (ivar b find-first-snip)]
	   [get-snip-position (ivar b get-snip-position)]
	   [position-paragraph (ivar b position-paragraph)]
	   [set-paragraph-margins (ivar b set-paragraph-margins)]
	   [add-document-note (ivar b add-document-note)]

	   [inserted-chars #f]
	   [get-char (lambda ()
		       (if inserted-chars
			   (begin0
			    (car inserted-chars)
			    (set! inserted-chars (cdr inserted-chars)))
			   (let ([v (read-char p)])
			     (if (eof-object? v)
				 #\nul
				 v))))]

	   [base-path (get-url)]
	   
	   [whitespaces (string #\space #\tab #\newline #\return)]

	   [verbatim-tags '(listing xmp plaintext)]
	   [preformatted-tags '(pre blockquote)]
	   [comment-tags '(script)]
	   [atomic-tags '(p br hr li dd dt img html !-- meta link input)]
	   [enum-tags '(ul dl ol menu)]

	   [delta:fixed (make-object style-delta% 'change-family 'modern)]
	   [delta:bold (make-object style-delta% 'change-bold)] 
	   [delta:underline (make-object style-delta% 'change-underline #t)]
	   [delta:italic (make-object style-delta% 'change-italic)]
	   [delta:h1 (let ([d (make-object style-delta% 'change-bold)])
		       (send d set-size-mult 2.0)
		       d)]
	   [delta:h2 (let ([d (make-object style-delta% 'change-bold)])
		       (send d set-size-mult 1.5)
		       d)]
	   [delta:h3 (let ([d (make-object style-delta% 'change-bold)])
		       (send d set-size-mult 1.2)
		       d)]

	   [html-error
	    (lambda args
	      (when #f ; treat them all as ignored warnings
		(apply error 'html args)))]

	   [i-buffer null]
	   [buffer-pos 0]
	   [buffer-insert
	    (lambda (char pos)
	      (when (null? i-buffer)
		(set! buffer-pos pos))
	      (set! i-buffer (cons char i-buffer)))]
	   [flush-i-buffer
	    (lambda ()
	      (when (pair? i-buffer)
		(insert (list->string (reverse! i-buffer)) buffer-pos)
		(set! i-buffer null)))]

	   [parse-image-source
	    (let ([get-src (make-get-field "src")])
	      (lambda (s)
		(let ([src (get-src s)])
		  (and src
		       (with-handlers ([not-break (lambda (x) #f)])
			 (combine-url/relative base-path src))))))]

	   [parse-image-alt
	    (let ([get-src (make-get-field "alt")])
	      (lambda (s)
		(get-src s)))]
	   
	   [get-mzscheme-arg (let ([get (make-get-field "mzscheme")])
			       (lambda (s)
				 (let ([v (get s)])
				   (and v (regexp-replace* "[|]" v "\"")))))]
		  
	   [parse-href
	    (let ([get-href (make-get-field "href")]
		  [get-name (make-get-field "name")]
		  [href-error
		   (lambda (s)
		     (html-error "bad reference in ~s" s))])
	      (lambda (s)
		(let* ([url-string
			(cond 
			 [(get-href s)
			  => (lambda (str)
			       (if (string=? str "")
				   (begin (href-error s)
					  #f)
				   str))]
			 [else #f])]
		       [label (if url-string
				  #f
				  (get-name s))]
		       [scheme (get-mzscheme-arg s)])
		  (values url-string label scheme))))]

	   [parse-mzscheme
	    (lambda (args)
	      (get-mzscheme-arg args))]
	    
	   [parse-docnote (make-get-field "docnote")]
	    
	   [parse-name (make-get-field "name")]

	   [parse-type (make-get-field "type")]

	   [parse-font
	    (let ([get-size (make-get-field "size")]
		  [get-color (make-get-field "color")]
		  [get-bg-color (make-get-field "bgcolor")])
	      (lambda (args)
		(let ([size (get-size args)]
		      [color (get-color args)]
		      [bg-color (get-bg-color args)])
		  (let ([size
			 (and size (let* ([n (string->number size)])
				   (and n
					(integer? n)
					(<= -127 n 127)
					(cond
					 [(char=? #\+ (string-ref size 0))
					  (make-object style-delta% 'change-bigger n)]
					 [(negative? n)
					  (make-object style-delta% 'change-smaller (- n))]
					 [else
					  (make-object style-delta% 'change-size n)]))))]
			[color (and color (let ([d (make-object style-delta%)])
					    (send d set-delta-foreground color)))]
			[bg-color (and bg-color (let ([d (make-object style-delta%)])
						  (send d set-delta-background bg-color)))])
		    (let loop ([delta #f][l (list size color bg-color)])
		      (cond
		       [(null? l) delta]
		       [(not (car l)) (loop delta (cdr l))]
		       [else (if delta
				 (loop (begin
					 (send delta collapse (car l))
					 delta)
				       (cdr l))
				 (loop (car l) (cdr l)))]))))))]
	   
	   [make-unsupported
	    (lambda (tag args)
	      (let ([name (parse-name args)]
		    [type (parse-type args)])
		(if (and (eq? tag 'input) type (string=? type "hidden"))
		    "" ; hidden input
		    (format "[~a~a NOT SUPPORTED]"
			    (if name
				(format "~a " name)
				"")
			    (case tag
			      [(select) "POPUP MENU"]
			      [(textarea) "TEXT AREA"]
			      [(input) (if type
					   (case (string->symbol type)
					     [(text) "TEXT FIELD"]
					     [else "BUTTON"])
					   "BUTTON")])))))]
	   
	   ;; Make sure newline strength before pos is count; returns number inserted
	   [try-newline
	    (lambda (pos count maybe-tabbed?)
	      (cond
	       [(zero? count) 0]
	       [(zero? pos) 0]
	       [else (let ([c (get-character (sub1 pos))])
		       (cond
			[(eq? #\newline c)
			 (try-newline (sub1 pos) (sub1 count) maybe-tabbed?)]
			[(and maybe-tabbed? (char-whitespace? c))
			 ; Some whitespace is messing up the newlines (perhaps added
			 ; by a spurious <P> in a list item); delete non-newlines
			 (let loop ([p (sub1 pos)][nl 0])
			   (cond
			    [(or (zero? p) (not (char-whitespace? (get-character (sub1 p)))))
			     (delete p pos)
			     (insert (make-string nl #\newline) p)
			     (+ (- p pos) nl (try-newline (+ p nl) count #f))]
			    [else (loop (sub1 p) 
					(if (char=? #\newline (get-character (sub1 p)))
					    (add1 nl)
					    nl))]))]
			[else
			 (insert #\newline pos)
			 (add1 (try-newline pos (sub1 count) #f))]))]))]
	   
	   [find-string 
	    (lambda (str pos keep?)
	      (let ([first (string-ref str 0)]
		    [len (string-length str)])
		(let loop ([pos pos][c (get-char)])
		  (cond
		   [(char=? #\null c)
		    (flush-i-buffer)
		    -1]
		   [(char-ci=? c first)
		    (let loop2 ([p 1][chars (list c)])
		      (if (= p len)
			  (begin
			    (flush-i-buffer)
			    pos)
			  (let ([c (get-char)])
			    (cond
			     [(char-ci=? c (string-ref str p))
			      (loop2 (add1 p) (cons c chars))]
			     [else
			      (loop
			       (if keep?
				   (let ([s (list->string (reverse! chars))])
				     (flush-i-buffer)
				     (insert s pos)
				     (+ pos (string-length s)))
				   pos)
			       c)]))))]
		   [else
		    (loop
		     (if keep?
			 (begin
			   (buffer-insert c pos)
			   (add1 pos))
			 pos)
		     (get-char))]))))]

	   ;; Find next "<", translating whitespace, &# along the way
	   [find-bracket
	    (lambda (start-pos dewhite? del-white?)
	      (let find-bracket ([pos start-pos][del-white? del-white?])
		(let ([ch (get-char)])
		  (cond
		   [(char=? #\null ch) 
		    (flush-i-buffer)
		    (when (> pos start-pos)
		      (change-style normal-style start-pos pos))
		    (values -1 #f)]
		   [(and (char-whitespace? ch) dewhite?)
		    (if del-white?
			(find-bracket pos #t)
			(begin
			  (buffer-insert #\space pos)
			  (find-bracket (add1 pos) #t)))]
		   [(char=? #\< ch) 
		    (flush-i-buffer)
		    (when (> pos start-pos)
		      (change-style normal-style start-pos pos))
		    (values pos del-white?)]
		   [(char=? #\& ch) 
		    (let ([ch (get-char)]
			  [result
			   (lambda (v)
			     (flush-i-buffer)
			     (insert v pos)
			     (find-bracket (+ pos
					      (if (string? v)
						  (string-length v)
						  1))
					   (eqv? #\space v)))])
		      (if (char=? #\# ch)
			  (let loop ([val 0])
			    (let ([ch (get-char)])
			      (if (char-numeric? ch)
				  (loop (+ (* 10 val) (- (char->integer ch) 48)))
				  (result (case val
					    [(160) #\space]
					    [(169) "(c)"]
					    [else (if (< 0 val 128) 
						      (string (integer->char val))
						      "")])))))
			  (let loop ([l (list ch)])
			    (let ([ch (get-char)])
			      (if (or (char=? #\null ch) (char=? #\; ch))
				  (result
				   (case (string->symbol (list->string (reverse! l)))
				     [(nbsp) #\space]
				     [(gt) #\>]
				     [(lt) #\<]
				     [(quot) #\"]
				     [(amp) #\&]
				     [else ""]))
				  (loop (cons ch l)))))))]
		   [else 
		    (buffer-insert ch pos)
		    (find-bracket (add1 pos) #f)]))))]

	   ;; Read inside of <>; return content-of-string
	   [read-bracket
	    (lambda ()
	      (let ([first (get-char)])
		(if (and (char=? #\! first)
			 (let ([c1 (get-char)])
			   (if (char=? #\- c1)
			       (let ([c2 (get-char)])
				 (if (char=? #\- c2)
				     #t
				     (begin
				       (set! inserted-chars (list* c1 c2 inserted-chars))
				       #f)))
			       (begin
				 (set! inserted-chars (list* c1 inserted-chars))
				 #f))))
		    ;; Comment - special parsing
		    (let loop ([l (list #\space #\- #\- #\!)][dash-count 0])
		      (let ([ch (get-char)])
			(cond
			 [(char=? #\null ch)
			  (html-error "end-of-file looking for closing angle-bracket")
			  (list->string (reverse! l))]
			 [(and (char=? #\> ch) (>= dash-count 2))
			  (list->string (reverse! l))]
			 [(char=? #\- ch)
			  (loop (cons ch l) (add1 dash-count))]
			 [else (loop (cons ch l) 0)])))
		    ;; Not a comment - parse with attention to quotes
		    (let ([done (lambda (name)
				  (list->string (reverse! name)))])
		      (let loop ([ch first][name null][quotes null])
			(cond
			 [(char=? #\null ch)
			  (html-error "end-of-file looking for closing angle-bracket")
			  (done name)]
			 [(and (null? quotes) (char=? #\> ch)) 
			  (done name)]
			 [(char=? #\" ch)
			  (loop (get-char) (cons ch name) 
				(if (or (null? quotes) (not (char=? #\" (car quotes))))
				    (cons #\" quotes)
				    (cdr quotes)))]
			 [else
			  (loop (get-char) (cons ch name) quotes)]))))))]
	   
	   ;; Parse string from inside <> into 
	   ;; (values html-tag-symbol tag-args-str end-tag?)
	   [parse-command
	    (let ([re:start (regexp (format "^([^~a/][^~a/]*)(.*)" whitespaces whitespaces))]
		  [re:end (regexp (format "^/([^~a]*)(.*)" whitespaces))])
	      (lambda (str)
		(let* ([match-start (regexp-match re:start str)]
		       [match-end (regexp-match re:end str)]
		       [match (or match-start match-end)]
		       [tag (if match (cadr match) str)]
		       [args (if match (caddr match) "")])
		  (unless match
		    (html-error "parse error for <~a>" str))
		  (string-lowercase! tag)
		  (values (string->symbol tag) args match-end))))]
	   
	   ;; Given CMD, find </CMD>; remove </CMD> and return position
	   ;; Translate nested <CMD2> ... </CMD2>
	   ;; Returns (values end-pos del-white? found-extra-end extra-args)
	   [find-end
	    (lambda (tag pos dewhite? del-white? enum-depth)
	      (let-values ([(pos del-white?) (find-bracket pos dewhite? del-white?)])
		(if (= pos -1)
		    (begin
		      (html-error "couldn't find </~a>" tag)
		      (values (last-position) del-white? #f #f))
		    (let ([cmd (read-bracket)]
			  [found-end
			   (lambda (pos del-white? found-tag args)
			     (if (eq? tag found-tag)
				 (values pos del-white? #f #f)
				 (begin
				   (html-error "found </~a> looking for </~a>"
					       found-tag tag)
				   (values pos del-white? found-tag args))))]
			  [ignore-end-tag? (lambda (t) (memq t atomic-tags))])
		      (let-values ([(found-tag args end?) (parse-command cmd)])
			(if (not end?)
			    (let-values ([(pos del-white? found-tag args) 
					  (translate-command pos dewhite? del-white? enum-depth
							     found-tag args)])
			      (if found-tag
				  (found-end pos del-white? found-tag args)
				  (find-end tag pos dewhite? del-white? enum-depth)))
			    (if (ignore-end-tag? found-tag)
				(find-end tag pos dewhite? del-white? enum-depth)
				(found-end pos del-white? found-tag args))))))))]

	   [translate-command
	    (lambda (pos dewhite? del-white? enum-depth tag args)
	      (cond
	       [(memq tag atomic-tags)
		(let* ([atomic-values (lambda (pos del-white?)
					(values pos del-white? #f #f))]
		       [break (lambda (bullet? newlines)
				(let ([pos (+ pos (try-newline pos newlines #t))])
				  (when bullet?
				    (insert (make-object bullet-snip% (sub1 enum-depth)) pos)
				    (change-style normal-style pos (+ 1 pos)))
				  (let ([data (list enum-depth bullet?)])
				    (btree-put! indents pos data))
				  (atomic-values (+ pos (if bullet? 1 0))
						 #t)))])
		  (case tag
		    [(!--)
		     (let ([code (parse-mzscheme args)])
		       (when code
			 (let ([s (with-handlers ([not-break void])
				    (eval (read (open-input-string (regexp-replace* "[|]" code "\"")))))])
			   (when (string? s)
			     ; Put result back into the input stream:
			     (set! inserted-chars (append (string->list s) inserted-chars))))))
		     (let ([note (parse-docnote args)])
		       (when note
			 (add-document-note note)))
		     (atomic-values pos del-white?)]
		    [(br) (break #f 1)]
		    [(p hr) (break #f (if del-white? 2 1))] ; del-white? = #f => <P> in <PRE>
		    [(li) (break #t 1)]
		    [(dt) (break #f 2)]
		    [(dd) (break #f 1)]
		    [(img)
		     (let* ([url (parse-image-source args)]
			    [alt (parse-image-alt args)]
			    [b (and url (cache-image url))])
		       (cond
			[(or b (not alt))
			 (insert (or b (make-object image-snip%)) pos)
			 (change-style (make-object style-delta% 'change-alignment 'center) 
				       pos (add1 pos))
			 (atomic-values (add1 pos) #f)]
			[else
			 (insert alt pos)
			 (atomic-values (+ pos (string-length alt)) #f)]))]
		    [(input)
		     (let* ([unsupported (make-unsupported tag args)]
			    [len (string-length unsupported)])
		       (insert unsupported pos)
		       (change-style normal-style pos (+ pos len))
		       (atomic-values (+ pos len) #f))]
		    [else 
		     (html-error "unimplemented (atomic) tag: ~a" tag)
		     (atomic-values pos del-white?)]))]
	       [(memq tag verbatim-tags)
		(let* ([str (format "</~a>" tag)]
		       [end-pos (find-string str pos #t)])
		  (values
		   (if (= -1 end-pos)
		       (begin
			 (html-error "verbatim closing tag </~a> not found" tag)
			 (last-position))
		       (begin
			 (change-style delta:fixed pos end-pos)
			 end-pos))
		   #t #f #f))]
	       [(memq tag comment-tags)
		(let* ([str (format "</~a>" tag)]
		       [end-pos (find-string str pos #f)])
		  (when (negative? end-pos)
		    (html-error "comment closing tag </~a> not found" tag))
		  (values pos del-white? #f #f))]
	       [else
		(let ([enum-depth (+ enum-depth
				     (if (memq tag enum-tags)
					 1
					 0))]
		      [dewhite? (and dewhite?
				     (not (memq tag preformatted-tags)))]
		      [pre-newlines
		       (case tag
			 [(dl ul menu table) (if (< enum-depth 1) 2 1)]
			 [(tr) 1]
			 [(pre) 2]
			 [(h1 h2 h3) 2]
			 [else 0])])
		  (let-values ([(end-pos del-white? extra-tag extra-args) 
				(find-end tag (+ pos (try-newline pos pre-newlines #t)) dewhite? del-white? enum-depth)])
		    (let* ([result (lambda (pos del-white?)
				     (values pos del-white? extra-tag extra-args))]
			   [normal (lambda () (result end-pos del-white?))]
			   [restart (lambda () (result pos del-white?))]
			   [heading (lambda (delta)
				      (insert (string #\newline #\newline) end-pos)
				      (change-style delta pos end-pos)
				      (result (+ end-pos 2) #t))])
		      (case tag
			[(head body center) (normal)]
			[(title)
			 (set-title (get-text pos end-pos))
			 (delete pos end-pos)
			 (result pos #t)]
			[(dl ul menu table tr)
			 (let ([new-end (+ end-pos (try-newline end-pos pre-newlines #t))])
			   ; At end, make sure indentation is reset:
			   (let ([m (btree-get indents new-end)])
			     (when m
			       (set-car! m (sub1 (car m)))))
			   (result new-end #t))]
			[(b strong)
			 (change-style delta:bold pos end-pos)
			 (normal)]
			[(u)
			 (change-style delta:underline pos end-pos)
			 (normal)]
			[(i em var dfn cite)
			 (change-style delta:italic pos end-pos)
			 (normal)]
			[(tt code samp kbd)
			 (change-style delta:fixed pos end-pos)
			 (normal)]
			[(pre)
			 (change-style delta:fixed pos end-pos)
			 (result (+ end-pos (try-newline end-pos 2 #t)) #t)]
			[(font)
			 (let ([delta (parse-font args)])
			   (when delta
			     (change-style delta pos end-pos)))
			 (normal)]
			[(h1) (heading delta:h1)]
			[(h2) (heading delta:h2)]
			[(h3) (heading delta:h3)]
			[(a) (let-values ([(url-string label scheme) (parse-href args)])
			       (cond
				[url-string
				 (add-link pos end-pos url-string)
				 (make-link-style pos end-pos)]
				[label
				 (add-tag label pos)]
				[scheme
				 (add-scheme-callback pos end-pos scheme)
				 (make-link-style pos end-pos)]
				[else (void)])
			       (normal))]
			[(select textarea)
			 (let* ([unsupported (make-unsupported tag args)]
				[len (string-length unsupported)])
			   (delete pos end-pos)
			   (insert unsupported pos)
			   (change-style normal-style pos (+ pos len))
			   (result (+ pos len) #f))]
			[else 
			 (html-error "unimplemented tag: ~s" tag)
			 (normal)]))))]))]

	   ;; Given pos for open bracket, find end and translate contents.
	   ;; Return (values position-for-continuing-search del-white?)
	   [translate
	    (lambda (pos dewhite? del-white? enum-depth)
	      (let-values ([(cmd) (read-bracket)])
		(let-values ([(tag args end?) (parse-command cmd)])
		  (if end? 
		      (begin
			(html-error "closing </~a> without opening" tag)
			(values pos del-white?))
		      (let-values ([(end-pos del-white? extra-tag extra-args) 
				    (translate-command pos dewhite? del-white? enum-depth tag args)])
			(when extra-tag
			  (html-error "closing </~a> without opening" tag))
			(values end-pos del-white?))))))])

	(load-status #f "page" base-path)
	
	(add-tag "top" 0)
	(let loop ([pos 0][del-white? #t])
	  (let-values ([(pos del-white?) (find-bracket pos #t del-white?)])
	    (unless (= pos -1)
	      (call-with-values
	       (lambda () (translate pos #t del-white? 0))
	       loop))))

	;; Install indentation
	(btree-for-each
	 indents
	 (lambda (pos data)
	   (let ([depth (max 0 (car data))]
		 [bullet? (cadr data)])
	     (set-paragraph-margins (position-paragraph pos)
				    (max 0 (- (* 2 bullet-width depth)
					      (if bullet?
						  bullet-width
						  0)))
				    (* 2 bullet-width depth)
				    0))))

	(set-position 0)))))
