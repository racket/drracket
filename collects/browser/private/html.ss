
(module html mzscheme
  (require (lib "unitsig.ss")
           "sig.ss"
           (lib "mred-sig.ss" "mred")
           (lib "file.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "thread.ss")
           (lib "url.ss" "net")
	   (rename (lib "html.ss" "html") read-html-as-xml read-html-as-xml)
	   (rename (lib "html.ss" "html") read-html-comments read-html-comments)
	   (rename (lib "html.ss" "html") use-html-spec use-html-spec)
	   (all-except (lib "xml.ss" "xml") read-comments)
           (lib "class.ss"))

  (provide html@)
  
  (define html@
    (unit/sig html^
      (import relative-btree^
              bullet^
              mred^)
      
      ;; CACHE
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

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Hardwired Scheme colorization; should come from a .css file

      (define (make-color-delta col)
	(let ([d (make-object style-delta%)])
	  (send d set-delta-foreground col)
	  d))

      (define scheme-code-delta (make-color-delta "brown"))
      (define scheme-code-delta/keyword
	(let ([d (make-object style-delta% 'change-bold)])
	  (send d set-delta-foreground (make-object color% #x99 0 0))
	  d))
      (define scheme-code-delta/variable (make-color-delta "navy"))
      (define scheme-code-delta/global (make-color-delta "purple"))
      (define scheme-code-delta/selfeval (make-color-delta "forest green"))
      (define scheme-code-delta/comment (make-color-delta "cornflower blue"))
      (define navigation-delta (let ([d (make-color-delta "red")])
				 (send d set-style-on 'italic)
				 d))

      (define (lookup-class-delta class)
	(cond
	 [(string=? class "scheme") scheme-code-delta]
	 [(string=? class "keyword") scheme-code-delta/keyword]
	 [(string=? class "variable") scheme-code-delta/variable]
	 [(string=? class "global") scheme-code-delta/global]
	 [(string=? class "selfeval") scheme-code-delta/selfeval]
	 [(string=? class "comment") scheme-code-delta/comment]
	 [(string=? class "navigation") navigation-delta]
	 [else #f]))

      (define (lookup-span-class-delta class) (lookup-class-delta class))

      (define re:hexcolor 
	(regexp "^#([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])$"))
      
      (define color-string->color
	(lambda (str)
	  (let ([m (regexp-match re:hexcolor str)])
	    (if m
		(make-object color%
			     (string->number (cadr m) 16)
			     (string->number (caddr m) 16)
			     (string->number (cadddr m) 16))
		(send the-color-database find-color str)))))

      (define get-image-from-url
        (lambda (url)
          (let ([tmp-filename (make-temporary-file "mredimg~a")])
            (load-status #t "image" url)
            (call-with-output-file* tmp-filename
                                    (lambda (op)
                                      (with-handlers ([not-break-exn? void])
                                        (call/input-url 
                                         url
                                         get-pure-port
                                         (lambda (ip)
					   (copy-port ip op)))))
                                    'truncate)
            (pop-status)
            (let* ([upath (url-path url)]
                   [bitmap (make-object bitmap% tmp-filename)])
              (with-handlers ([(lambda (x) #t)
                               (lambda (x)
                                 (message-box "Warning"
                                              (format "Could not delete file ~s~n~n~a"
                                                      tmp-filename
                                                      (if (exn? x)
                                                          (exn-message x)
                                                          x))))])
                (delete-file tmp-filename))
              (if (send bitmap ok?)
                  (let ([is (make-object image-snip% #f)])
                    (send is set-bitmap bitmap)
                    is)
                  #f)))))
      
      (define cache-image
        (lambda (url)
          (let ([url-string (url->string url)])
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
                              (vector-set! cached-name n url-string)
                              (vector-set! cached-use n 5)
                              (send image copy)]
                             [else #f]))
                         (loop (add1 n)))))]
                [(equal? url-string (vector-ref cached-name n))
                 (vector-set! cached-use n (min 10 (add1 (vector-ref cached-use n))))
                 (send (vector-ref cached n) copy)]
                [else
                 (loop (add1 n))])))))
      
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
          (let ([re:plain (regexp (format "(^|[~a])~a[~a]*=[~a]*([^~a]*)" spc s spc spc spc))]
                [re:quote (regexp (format "(^|[~a])~a[~a]*=[~a]*\"([^\"]*)\"" spc s spc spc))])
            (lambda (args)
              (let ([m (or (regexp-match re:quote args)
                           (regexp-match re:plain args))])
                (and m (caddr m)))))))
      
      (define get-mzscheme-arg
	(let ([get-mz (make-get-field "mzscheme")])
	  (lambda (str)
	    (let ([v (get-mz str)])
	      (and v (filter-mzscheme v))))))

      (define filter-mzscheme 
	(lambda (v)
	  (regexp-replace* "[|]" v "\"")))
		      		       
      (define face-list #f)
      
      (define latin-1-symbols
        '#cs((middot 46)
	     (amp 38) (gt 62) (lt 60) (quot 34) (nbsp 160) (iexcl 161)
	     (cent 162) (pound 163) (curren 164) (yen 165) (brvbar 166) (sect 167)
	     (uml 168) (copy 169) (ordf 170) (laquo 171) (not 172) (shy 173)
	     (reg 174) (macr 175) (deg 176) (plusmn 177) (sup2 178) (sup3 179)
	     (acute 180) (micro 181) (para 182) (middot 183) (cedil 184) (sup1 185)
	     (ordm 186) (raquo 187) (frac14 188) (frac12 189) (frac34 190) (iquest 191)
	     (Agrave 192) (Aacute 193) (Acirc 194) (Atilde 195) (Auml 196) (Aring 197)
	     (AElig 198) (Ccedil 199) (Egrave 200) (Eacute 201) (Ecirc 202) (Euml 203)
	     (Igrave 204) (Iacute 205) (Icirc 206) (Iuml 207) (ETH 208) (Ntilde 209)
	     (Ograve 210) (Oacute 211) (Ocirc 212) (Otilde 213) (Ouml 214) (times 215)
	     (Oslash 216) (Ugrave 217) (Uacute 218) (Ucirc 219) (Uuml 220) (Yacute 221)
	     (THORN 222) (szlig 223) (agrave 224) (aacute 225) (acirc 226) (atilde 227)
	     (auml 228) (aring 229) (aelig 230) (ccedil 231) (egrave 232) (eacute 233)
	     (ecirc 234) (euml 235) (igrave 236) (iacute 237) (icirc 238) (iuml 239)
	     (eth 240) (ntilde 241) (ograve 242) (oacute 243) (ocirc 244) (otilde 245)
	     (ouml 246) (divide 247) (oslash 248) (ugrave 249) (uacute 250) (ucirc 251)
	     (uuml 252) (yacute 253) (thorn 254) (yuml 255)))
      
      (define verbatim-tags '(listing xmp plaintext))
      (define preformatted-tags '(pre blockquote))
      (define exact-whitespace-tags (append verbatim-tags
					    preformatted-tags))
      (define comment-tags '(script))
      (define atomic-tags '(p br hr li dd dt img html meta link input))
      (define enum-tags '(ul dl ol menu))

      (define space-eating-tags '(title p div center br h1 h2 h3 h4
					li dt dd
					ul ol dl
					samp kbd pre blockquote
					table tr td))

      (define whitespace-string "[ \t\n\r\v\f]+")
      (define re:whitespace (regexp whitespace-string))
      (define re:starting-whitespace (regexp (format "^~a" whitespace-string)))
      (define re:ending-whitespace (regexp (format "~a$" whitespace-string)))
      (define re:leading-newline (regexp "\r|\n|(\r\n)"))
      
      (define (remove-leading-newline c)
	(cond
	 [(string? c)
	  (let ([s (regexp-match-positions re:leading-newline c)])
	    (cond
	     [(and s (= (cdar s) (length s)))
	      ;; It's all newline:
	      (values "" #t)]
	     [s (values (substring c (cdar s) (string-length c)) #t)]
	     [else (values c #t)]))]
	 [(pair? c)
	  (let loop ([b (cddr c)][accum null])
	    (if (null? b)
		(values (list* (car c) (cadr c) (reverse accum)) #f)
		(let-values ([(d done?) (remove-leading-newline (car b))])
		  (if done?
		      (values (list* (car c) (cadr c) (append (reverse accum)
							      (list d)
							      (cdr b)))
			      #t)
		      (loop (cdr b) (cons d accum))))))]
	 [else (values c #f)]))

      (define (fixup-whitespace c leading-ok?)
	(cond
	 [(string? c)
	  (let ([s (regexp-match-positions re:starting-whitespace c)]
		[e (regexp-match-positions re:ending-whitespace c)])
	    (if (and s e
		     (= (caar s) (caar e)))
		;; It's all whitespace:
		(if leading-ok?
		    (values " " #f)
		    (values "" #f))
		;; Normal case:
		(values
		 (string-append
		  (if (and s leading-ok?) " " "")
		  (regexp-replace* re:whitespace
				   (substring c 
					      (if s
						  (cdar s)
						  0)
					      (if e
						  (caar e)
						  (string-length c)))
				   " ")
		  (if e " " ""))
		 (not e))))]
	 [(symbol? c) (values c #t)]
	 [(number? c) (values c #t)]
	 [(comment? c)
	  (let ([code (get-mzscheme-arg (comment-text c))])
	    (if code
		(let ([s (with-handlers ([not-break-exn?
					  (lambda (exn)
					    (format
					     "<font color=\"red\">Error during &lt;!-- MZSCHEME=... --&gt;: <i>~a</i></font>"
					     (if (exn? exn)
						 (exn-message exn)
						 (format "~s" exn))))])
			   (eval (read (open-input-string code))))])
		  (if (string? s)
		      (let ([content (read-html (open-input-string s))])
			(fixup-whitespace content leading-ok?))
		      (values "" leading-ok?)))
		(values "" leading-ok?)))]
         [(pi? c) (values "" leading-ok?)] ;; processing instruction
	 [else (let ([tag (car c)])
		 (if (memq tag exact-whitespace-tags)
		     (let-values ([(s done?) (remove-leading-newline c)])
		       (values s #f))
		     (let-values ([(body leading-ok?)
				   (let loop ([l (cddr c)][leading-ok? 
							   (and leading-ok?
								(not (memq tag space-eating-tags)))])
				     (if (null? l)
					 (values null leading-ok?)
					 (let*-values ([(f l-ok?)
							(fixup-whitespace (car l) leading-ok?)]
						       [(r l-ok?)
							(loop (cdr l) l-ok?)])
					   (values (cons f r) l-ok?))))])
		       (values
			(list*
			 tag
			 (cadr c) ; attributes
			 body)
			(and leading-ok?
			     (not (memq tag space-eating-tags)))))))]))

      (define (read-html a-port)
	`(html () ,@(map xml->xexpr (parameterize ([read-html-comments #t]
						   [use-html-spec #f])
				      (read-html-as-xml a-port)))))

      (define (parse-html a-port)
	(let ([raw (read-html a-port)])
	  (let-values ([(v ?) (fixup-whitespace raw #f)])
	    v)))

      (define html-convert
        (lambda (a-port a-text)	    
	  (let ([content (parse-html a-port)])
	    (with-method ([a-text-insert (a-text insert)]
			  [current-pos (a-text last-position)]
			  [delete (a-text delete)]
			  [get-character (a-text get-character)]
			  [change-style (a-text change-style)])
	      (letrec ([normal-style (send (send a-text get-style-list)
					   find-named-style
					   "Standard")]
		       [insert 
			(lambda (what)
			  (a-text-insert what (current-pos)))]
		       
		       [insert-newlines
			(lambda (num para-base)
			  (unless (zero? num)
			    (let loop ([pos (current-pos)][num num])
			      (unless (or (zero? num) (<= pos para-base))
				(let ([c (get-character (sub1 pos))])
				  (if (eq? c #\newline)
				      (loop (sub1 pos) (sub1 num))
				      (insert (make-string num #\newline))))))))]
		       
		       [backover-newlines
			(lambda (pos base)
			  (if (= pos base)
			      base
			      (let ([c (get-character (sub1 pos))])
				(if (eq? c #\newline)
				    (backover-newlines (sub1 pos) base)
				    pos))))]
		       
		       [base-path (send a-text get-url)]
		       
		       [whitespaces (string #\space #\tab #\newline #\return)]
		       
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
		       [delta:h4 (make-object style-delta% 'change-bold)]
		       [delta:subscript (let ([d (make-object style-delta%)])
					  (send d set-alignment-on 'bottom)
					  (send d set-size-mult 0.8)
					  d)]
		       [delta:superscript (let ([d (make-object style-delta%)])
					    (send d set-alignment-on 'top)
					    (send d set-size-mult 0.8)
					    d)]
		       [delta:small (let ([d (make-object style-delta%)])
				      (send d set-size-mult 0.75)
				      d)]

		       [delta:center (make-object style-delta% 'change-alignment 'center)]

		       [html-error
			(lambda args
			  (when #f ; treat them all as ignored warnings
			    (apply error 'html args)))]
		       
		       [get-field (lambda (e name)
				    (let ([a (assq name (cadr e))])
				      (and a (cadr a))))]

		       [re:transparent "[Tt][Rr][Aa][Nn][Ss][Pp][Aa][Rr][Ee][Nn][Tt]"]
		       
		       [parse-image-source
			(lambda (s)
			  (let ([src (get-field s 'src)])
			    (and src
				 (with-handlers ([not-break-exn? (lambda (x) #f)])
				   (if base-path
				       (combine-url/relative base-path src)
				       (string->url src))))))]
		       
		       [parse-image-alt
			(let ([get-src (make-get-field "alt")])
			  (lambda (s)
			    (get-src s)))]
		       
		       [parse-href
			(let ([href-error
			       (lambda (s)
				 (html-error "bad reference in ~s" s))])
			  (lambda (s)
			    (let* ([url-string
				    (cond 
				     [(get-field s 'href)
				      => (lambda (str)
					   (if (string=? str "")
					       (begin (href-error s)
						      #f)
					       str))]
				     [else #f])]
				   [label (get-field s 'name)]
				   [scheme (let ([v (get-field s 'mzscheme)])
					     (and v (filter-mzscheme v)))])
			      (values url-string label scheme))))]
		       
		       [parse-docnote (make-get-field "docnote")]
		       
		       [parse-name (make-get-field "name")]
		       
		       [parse-type (make-get-field "type")]
		       
		       [parse-font
			(let ([face-regexp (regexp "([^,]*), *(.*)")])
			  (lambda (args)
			    (let ([size-string (get-field args 'size)]
				  [face-string (get-field args 'face)]
				  [color-string (get-field args 'color)]
				  [bg-color-string (get-field args 'bgcolor)])
			      (let ([size
				     (and size-string
					  (let* ([n (string->number size-string)])
					    (and n
						 (integer? n)
						 (<= -127 n 127)
						 (cond
						  [(char=? #\+ (string-ref size-string 0))
						   (make-object style-delta% 'change-bigger n)]
						  [(negative? n)
						   (make-object style-delta% 'change-smaller (- n))]
						  [else
						   (make-object style-delta% 'change-size n)]))))]
				    [face (and face-string
					       (let ([f (let loop ([f face-string])
							  (let ([m (regexp-match face-regexp f)]
								[try-face (lambda (s)
									    (unless face-list
									      (set! face-list (get-face-list)))
									    (ormap
									     (lambda (s-norm)
									       (and (string-ci=? s s-norm)
										    s-norm))
									     face-list))])
							    (if m
								(or (try-face (cadr m))
								    (loop (caddr m)))
								(try-face f))))])
						 (and f
						      (let ([d (make-object style-delta%)])
							(send d set-delta-face f)))))]
				    [color (let ([clr (and color-string (color-string->color color-string))])
					     (and clr
						  (let ([d (make-object style-delta%)])
						    (send d set-delta-foreground clr))))]
				    [bg-color (let ([bg-clr (and bg-color-string
								 (color-string->color bg-color-string))])
						(and bg-clr
						     (let ([d (make-object style-delta%)])
						       (send d set-delta-background bg-clr))))])
				(let loop ([delta #f][l (list size face color bg-color)])
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
			  (let ([name (get-field args 'name)]
				[type (get-field args 'type)])
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

		       [heading (lambda (delta rest para-base)
				  (insert-newlines 2 para-base)
				  (let ([start-pos (current-pos)]
					[r (rest)]
					[end-pos (current-pos)])
				    (insert-newlines 2 para-base)
				    (lambda ()
				      (change-style delta start-pos end-pos)
				      (r))))]

		       [styler (lambda (delta rest)
				 (let* ([start-pos (current-pos)]
					[r (rest)]
					[end-pos (current-pos)])
				   (lambda ()
				     (change-style delta start-pos end-pos)
				     (r))))]
		       
		       [maybe-bg-color (lambda (e rest)
					 (let* ([c (get-field e 'bgcolor)]
						[color (and c (color-string->color c))])
					   (if color
					       (styler (let ([d (make-object style-delta%)])
							 (send d set-delta-background color)
							 d)
						       rest)
					       (rest))))]
		       
		       [para-aligner (lambda (alignment delta rest)
				       (let* ([start-pos (current-pos)]
					      [r (rest)]
					      [end-pos (current-pos)])
					 (lambda ()
					   (let ([last-para (send a-text position-paragraph 
								  (backover-newlines end-pos start-pos))])
					     (let loop ([para (send a-text position-paragraph start-pos)])
					       (send a-text set-paragraph-alignment para alignment)
					       (when delta
						 (change-style delta start-pos end-pos))
					       (unless (= para last-para)
						 (loop (add1 para)))))
					   (r))))]

		       ;; ========================================
		       ;; This is the main formatting function.
		       ;; It consumes:
		       ;;   e : xexpr - the HTML content
		       ;;   para-base : num - a marker for a paragraph start (e.g.,
		       ;;                     the bullet for <li>), though the actual
		       ;;                     paragraph start may be later
		       ;;   enum-depth : num - current depth of enumerations
		       ;; The result is a function of no arguments that finalizes
		       ;;  the region for `e', which normally means applying font changes.
		       ;;  (The changes have to be applied outside-in, so that local
		       ;;  specifications override enclosing ones.)
		       ;; Translate must not modify any existing text, and the
		       ;;  result function must not move any items.
		       [translate
			(lambda (e para-base enum-depth)
			  (cond
			   [(string? e) (insert e) void]
			   [(symbol? e) (let ([a (assq e latin-1-symbols)])
					  (if a
					      (insert (or (latin-1-integer->char (cadr a)) #\?))
					      (insert (format "&~a;" e)))
					  void)]
			   [(number? e) 
			    (if (<= 0 e 255)
				(insert (or (latin-1-integer->char e) #\?))
				(insert (format "&~a;" e)))
			    void]
                           [(or (comment? e) (pi? e)) void]
			   [else (let* ([tag (car e)]
					[rest/base/depth 
					 (lambda (para-base enum-depth)
					   (let ([l (map (lambda (x) (translate x para-base enum-depth))
							 (cddr e))])
					     (lambda ()
					       (map (lambda (f) (f)) l))))]
					[rest (lambda () (rest/base/depth para-base enum-depth))])
				   (case tag
				     [(title)
				      (let ([pos (current-pos)])
					;; Render content
					(rest)
					(send a-text set-title (send a-text get-text pos (current-pos)))
					(delete pos (current-pos)))
				      void]
				     [(a)
				      (let-values ([(url-string label scheme) (parse-href e)])
					(let* ([style (get-field e 'style)]
					       [pos (current-pos)]
					       [r (rest)]
					       [end-pos (current-pos)])
					  (cond
					   [url-string
					    (send a-text add-link pos end-pos url-string)
					    ;; might have a label, too:
					    (when label
					      (send a-text add-tag label pos))
					    (lambda ()
					      (when (or (not style)
							(not (regexp-match re:transparent style)))
						(send a-text make-link-style pos end-pos))
					      (r))]
					   [label
					    (send a-text add-tag label pos)
					    r]
					   [scheme
					    (send a-text add-scheme-callback pos end-pos scheme)
					    (lambda ()
					      (when (or (not style)
							(not (regexp-match re:transparent style)))
						(send a-text make-link-style pos end-pos))
					      (r))]
					   [else r])))]
				     [(style) void]
				     [(h1) (heading delta:h1 rest para-base)]
				     [(h2) (heading delta:h2 rest para-base)]
				     [(h3) (heading delta:h3 rest para-base)]
				     [(h4) (heading delta:h4 rest para-base)]
				     [(b strong) (styler delta:bold rest)]
				     [(i em var dfn cite) (styler delta:italic rest)]
				     [(u) (styler delta:underline rest)]
				     [(sup) (styler delta:superscript rest)]
				     [(sub) (styler delta:subscript rest)]
				     [(small) (styler delta:small rest)]
				     [(font)
				      (let ([delta (parse-font e)])
					(if delta
					    (styler delta rest)
					    (rest)))]
				     [(li dd dt)
				      (insert-newlines 1 para-base)
				      (let ([pos (current-pos)]
					    [bullet? (eq? tag 'li)])
					(when bullet?
					  (insert (make-object bullet-snip% (sub1 enum-depth))))
					(let* ([r (rest/base/depth (add1 pos) enum-depth)]
					       [end-pos (current-pos)])
					  (lambda ()
					    (change-style normal-style pos (+ 1 pos))
					    (let ([end-para (send a-text position-paragraph 
								  (backover-newlines end-pos pos))]
						  [left-margin (* 2 (get-bullet-width) enum-depth)])
					      (let loop ([para (send a-text position-paragraph pos)]
							 [first? #t])
						(send a-text set-paragraph-margins
						      para
						      (if first?
							  (max 0 (- left-margin
								    (if bullet?
									(get-bullet-width)
									0)))
							  left-margin)
						      left-margin
						      0)
						(unless (= para end-para)
						  (loop (add1 para) #f))))
					    (r))))]
				     [(ul ol dl)
				      (insert-newlines (if (zero? enum-depth) 2 1) para-base)
				      (begin0
				       (rest/base/depth para-base (add1 enum-depth))
				       (insert-newlines (if (zero? enum-depth) 2 1) para-base))]
				     [(p)
				      (insert-newlines 2 para-base)
				      (begin0
				       (rest)
				       (insert-newlines 2 para-base))]
				     [(center)
				      (insert-newlines 2 para-base)
				      (begin0
				       (para-aligner 'center #f rest)
				       (insert-newlines 2 para-base))]
				     [(div)
				      (insert-newlines 2 para-base)
				      (let* ([align (get-field e 'align)]
					     [class (get-field e 'class)]
					     [delta (and class (lookup-class-delta class))])
					(begin0
					 (cond
					  [(and (string? align) (string-ci=? align "center"))
					   (para-aligner 'center delta rest)]
					  [(and (string? align) (string-ci=? align "left"))
					   (para-aligner 'left delta rest)]
					  [(or (and (string? align) (string-ci=? align "right"))
					       (and (string? class) (string-ci=? class "navigation")))
					   (para-aligner 'right delta rest)]
					  [else
					   (rest)])
					 (insert-newlines 2 para-base)))]
				     [(br)
				      (insert-newlines 1 para-base)
				      (rest)]
				     [(table)
				      (insert-newlines 2 para-base)
				      (begin0
				       (maybe-bg-color e rest)
				       (insert-newlines 2 para-base))]
				     [(tr)
				      (insert-newlines 1 para-base)
				      (begin0
				       (maybe-bg-color e rest)
				       (insert-newlines 1 para-base))]
				     [(td)
				      (insert " ")
				      (begin0
				       (maybe-bg-color e rest)
				       (insert " "))]
				     [(img)
				      (let* ([url (parse-image-source e)]
					     [alt (get-field e 'alt)]
					     [b (and url (cache-image url))])
					(cond
					 [(or b (not alt))
					  (let ([pos (current-pos)])
					    (insert (or b (make-object image-snip%)))
					    (change-style delta:center pos (add1 pos)))]
					 [else
					  (insert alt)])
					(rest))]
				     [(input select textarea)
				      (let ([unsupported (make-unsupported tag e)]
					    [pos (current-pos)])
					(insert unsupported)
					(let ([r (rest)]
					      [end-pos (current-pos)])
					  (lambda ()
					    (change-style normal-style pos end-pos)
					    (r))))]
				     [(tt code samp kbd pre blockquote)
				      (when (memq tag '(pre blockquote))
					(insert-newlines 2 para-base))
				      (begin0
				       (let* ([class (get-field e 'class)]
					      [delta (and class (lookup-class-delta class))])
					 (styler (if delta
						     (let ([d (make-object style-delta% 'change-nothing)])
						       (send d copy delta)
						       (send d collapse delta:fixed)
						       d)
						     delta:fixed)
						 rest))
				       (when (memq tag '(pre blockquote))
					 (insert-newlines 2 para-base)))]
				     [(span)
				      (let* ([class (get-field e 'class)]
					     [delta (and class (lookup-class-delta class))])
					(if delta
					    (styler delta rest)
					    (rest)))]
				     [else (rest)]))]))])
		
		(load-status #f "page" base-path)
		
		((translate content 0 0))

		(send a-text add-tag "top" 0)

		(send a-text set-position 0)))))))))
