;; To do:
;;   Handle HTTP/file errors.
;;   Not throw away MIME headers.
;;     Determine file type.

;; ----------------------------------------------------------------------

;; Input ports have two statuses:
;;   "impure" = they have text waiting
;;   "pure" = the MIME headers have been read

(unit/sig mzlib:url^
  (import [file : mzlib:file^])

  (define-struct (url-exception struct:exn) ())

  ;; This is commented out; it's here for debugging.
  ;; It used to be outside the unit.
    
  (quote
    (begin
      (invoke-open-unit/sig mzlib:url@ #f)
      (define url:cs (string->url "http://www.cs.rice.edu/"))
      (define url:me (string->url "http://www.cs.rice.edu/~shriram/"))
      (define comb combine-url/relative)
      (define (test url)
	(call/input-url url
	  get-pure-port
	  display-pure-port))))

  (define url-error
    (lambda (fmt . args)
      (let ((s (apply format fmt (map (lambda (arg)
					(if (url? arg)
					  (url->string arg)
					  arg))
				   args))))
	(raise (make-url-exception s (current-continuation-marks))))))

  ;; if the path is absolute, it just arbitrarily picks the first
  ;; filesystem root.
  (define unixpath->path
    (letrec ([r (regexp "([^/]*)/(.*)")]
	     [translate-dir
	      (lambda (s)
		(cond
		  [(string=? s "") 'same] ;; handle double slashes
		  [(string=? s "..") 'up]
		  [(string=? s ".") 'same]
		  [else s]))]
	     [build-relative-path
	      (lambda (s)
		(let ([m (regexp-match r s)])
		  (cond
		    [(string=? s "") 'same]
		    [(not m) s]
		    [else
		     (build-path (translate-dir (cadr m))
				 (build-relative-path (caddr m)))])))])
      (lambda (s)
	(cond
	  [(string=? s "") ""]
	  [(string=? s "/") (car (filesystem-root-list))]
	  [(char=? #\/ (string-ref s 0))
	    (build-path (car (filesystem-root-list))
	      (build-relative-path
		(substring s 1 (string-length s))))]
	  [else (build-relative-path s)]))))

  ;; scheme : str + #f
  ;; host : str + #f
  ;; port : num + #f
  ;; path : str
  ;; params : str + #f
  ;; query : str + #f
  ;; fragment : str + #f
  (define-struct url (scheme host port path params query fragment))

  ;; name : str (all lowercase; not including the colon)
  ;; value : str (doesn't have the eol delimiter)
  (define-struct mime-header (name value))

  (define url->string 
    (lambda (url)
      (let ((scheme (url-scheme url))
	     (host (url-host url))
	     (port (url-port url))
	     (path (url-path url))
	     (params (url-params url))
	     (query (url-query url))
	     (fragment (url-fragment url)))
	(cond
	  ((and scheme (string=? scheme "file"))
	    (string-append "file:" path))
	  (else
	    (let ((sa string-append))
	      (sa (if scheme (sa scheme "://") "")
		(if host host "")
		(if port (sa ":" (number->string port)) "")
		; There used to be a "/" here, but that causes an
		; extra leading slash -- wonder why it ever worked!
		path
		(if params (sa ";" params) "")
		(if query (sa "?" query) "")
		(if fragment (sa "#" fragment) ""))))))))

  ;; url->default-port : url -> num
  (define url->default-port
    (lambda (url)
      (let ((scheme (url-scheme url)))
	(cond
	  ((not scheme) 80)
	  ((string=? scheme "http") 80)
	  (else
	    (url-error "Scheme ~a not supported" (url-scheme url)))))))

  ;; make-ports : url -> in-port x out-port
  (define make-ports
    (lambda (url)
      (let ((port-number (or (url-port url)
			   (url->default-port url))))
	(tcp-connect (url-host url) port-number))))

  ;; http://get-impure-port : url [x list (str)] -> in-port
  (define http://get-impure-port
    (opt-lambda (url (strings '()))
      (let-values (((server->client client->server)
		     (make-ports url)))
	(let ((access-string
		(url->string
		  (make-url #f #f #f
		    (url-path url) (url-params url)
		    (url-query url) (url-fragment url)))))
	  (for-each (lambda (s)
		      (display s client->server)
		      (newline client->server))
	    (cons (format "GET ~a HTTP/1.0" access-string)
	      (cons (format "Host: ~a" (url-host url))
		strings))))
	(newline client->server)
	(close-output-port client->server)
	server->client)))

  ;; file://get-pure-port : url -> in-port
  (define file://get-pure-port
    (lambda (url)
      (let ((host (url-host url)))
	(if (or (not host)
	      (string=? host "")
	      (string=? host "localhost"))
	  (open-input-file
	    (unixpath->path (url-path url)))
	  (url-error "Cannot get files from remote hosts")))))

  ;; get-impure-port : url [x list (str)] -> in-port
  (define get-impure-port
    (opt-lambda (url (strings '()))
      (let ((scheme (url-scheme url)))
	(cond
	  ((not scheme)
	    (url-error "Scheme unspecified in ~a" url))
	  ((string=? scheme "http")
	    (http://get-impure-port url strings))
	  ((string=? scheme "file")
	    (url-error "There are no impure file:// ports"))
	  (else
	    (url-error "Scheme ~a unsupported" scheme))))))

  ;; get-pure-port : url [x list (str)] -> in-port
  (define get-pure-port
    (opt-lambda (url (strings '()))
      (let ((scheme (url-scheme url)))
	(cond
	  ((not scheme)
	    (url-error "Scheme unspecified in ~a" url))
	  ((string=? scheme "http")
	    (let ((port (http://get-impure-port url strings)))
	      (purify-port port)
	      port))
	  ((string=? scheme "file")
	    (file://get-pure-port url))
	  (else
	    (url-error "Scheme ~a unsupported" scheme))))))

  ;; display-pure-port : in-port -> ()
  (define display-pure-port
    (lambda (server->client)
      (let loop ()
	(let ((c (read-char server->client)))
	  (unless (eof-object? c)
	    (display c)
	    (loop))))
      (close-input-port server->client)))

  (define empty-url?
    (lambda (url)
      (and (not (url-scheme url)) (not (url-params url))
	(not (url-query url)) (not (url-fragment url))
	(andmap (lambda (c) (char=? c #\space))
	  (string->list (url-path url))))))

  ;; combine-url/relative : url x str -> url
  (define combine-url/relative
    (lambda (base string)
      (let ((relative (string->url string)))
	(cond
	  ((empty-url? base)		; Step 1
	    relative)
	  ((empty-url? relative)	; Step 2a
	    base)
	  ((url-scheme relative)	; Step 2b
	    relative)
	  (else				; Step 2c
	    (set-url-scheme! relative (url-scheme base))
	    (cond
	      ((url-host relative)	; Step 3
		relative)
	      (else
		(set-url-host! relative (url-host base))
		(set-url-port! relative (url-port base)) ; Unspecified!
		(let ((rel-path (url-path relative)))
		  (cond
		    ((and rel-path	; Step 4
		       (not (string=? "" rel-path))
		       (char=? #\/ (string-ref rel-path 0)))
		      relative)
		    ((or (not rel-path)	; Step 5
		       (string=? rel-path ""))
		      (set-url-path! relative (url-path base))
		      (or (url-params relative)
			(set-url-params! relative (url-params base)))
		      (or (url-query relative)
			(set-url-query! relative (url-query base)))
		      relative)
		    (else		; Step 6
		      (if (and (url-scheme base)
			    (string=? (url-scheme base) "file"))

			;; ROBBY: Code needs to get fixed here.
			;; Important that:
			;; 1. You set-url-path! the new path into
			;;    `relative'.
			;; 2. You return `relative' as the value
			;;    from here without invoking
			;;    `merge-and-normalize'.
			;; The variable `rel-path' contains the
			;; path portion of the relative URL.

			(let+ ([val base-path (url-path base)]
				[val (values base name must-be-dir?)
				  (split-path base-path)]
				[val base-dir (if must-be-dir? base-path base)]
				[val ind-rel-path (unixpath->path rel-path)]
				[val merged (build-path base-dir
					      ind-rel-path)]
				[val norm (file:normalize-path merged)])
			  (set-url-path! relative norm)
			  relative)
			(merge-and-normalize
			  (url-path base) relative))))))))))))

  (define merge-and-normalize
    (lambda (base-path relative-url)
      (let ((rel-path (url-path relative-url)))
	(let ((base-list (string->list base-path))
	       (rel-list (string->list rel-path)))
	  (let*
	    ((joined-list
	       (let loop ((base (reverse base-list)))
		 (if (null? base)
		   rel-list
		   (if (char=? #\/ (car base))
		     (append (reverse base) rel-list)
		     (loop (cdr base))))))
	      (grouped
		(let loop ((joined joined-list) (current '()))
		  (if (null? joined)
		    (list (list->string (reverse current)))
		    (if (char=? #\/ (car joined))
		      (cons (list->string
			      (reverse (cons #\/ current)))
			(loop (cdr joined) '()))
		      (loop (cdr joined)
			(cons (car joined) current))))))
	      (grouped
		(let loop ((grouped grouped))
		  (if (null? grouped) '()
		    (if (string=? "./" (car grouped))
		      (loop (cdr grouped))
		      (cons (car grouped) (loop (cdr grouped)))))))
	      (grouped
		(let loop ((grouped grouped))
		  (if (null? grouped) '()
		    (if (null? (cdr grouped))
		      (if (string=? "." (car grouped)) '()
			grouped)
		      (cons (car grouped) (loop (cdr grouped)))))))
	      (grouped
		(let remove-loop ((grouped grouped))
		  (let walk-loop ((r-pre '()) (post grouped))
		    (if (null? post)
		      (reverse r-pre)
		      (let ((first (car post))
			     (rest (cdr post)))
			(if (null? rest)
			  (walk-loop (cons first r-pre) rest)
			  (let ((second (car rest)))
			    (if (and (not (string=? first "../"))
				  (string=? second "../"))
			      (remove-loop
				(append (reverse r-pre) (cddr post)))
			      (walk-loop (cons first r-pre) rest)))))))))
	      (grouped
		(let loop ((grouped grouped))
		  (if (null? grouped) '()
		    (if (null? (cdr grouped)) grouped
		      (if (and (null? (cddr grouped))
			    (not (string=? (car grouped) "../"))
			    (string=? (cadr grouped) ".."))
			'()
			(cons (car grouped) (loop (cdr grouped)))))))))
	    (set-url-path! relative-url
	      (apply string-append grouped))
	    relative-url)))))

  ;; call/input-url : url x (url -> in-port) x (in-port -> T)
  ;;                  [x list (str)] -> T
  (define call/input-url
    (let ((handle-port (lambda (server->client handler)
			 (dynamic-wind (lambda () 'do-nothing)
			   (lambda () (handler server->client))
			   (lambda () (close-input-port server->client))))))
      (case-lambda
	((url getter handler)
	  (handle-port (getter url) handler))
	((url getter handler params)
	  (handle-port (getter url params) handler)))))

  (define empty-line?
    (lambda (chars)
      (or (null? chars)
	(and (memv (car chars) '(#\return #\linefeed #\tab #\space))
	  (empty-line? (cdr chars))))))

  (define extract-mime-headers-as-char-lists
    (lambda (port)
      (let headers-loop ((headers '()))
	(let char-loop ((header '()))
	  (let ((c (read-char port)))
	    (if (eof-object? c)
	      (reverse headers)		; CHECK: INCOMPLETE MIME: SERVER BUG
	      (if (char=? c #\newline)
		(if (empty-line? header)
		  (reverse headers)
		  (begin
		    (headers-loop (cons (reverse header) headers))))
		(char-loop (cons c header)))))))))

  ;; purify-port : in-port -> list (mime-header)
  (define purify-port
    (lambda (port)
      (let ((headers-as-chars (extract-mime-headers-as-char-lists port)))
	(let header-loop ((headers headers-as-chars))
	  (if (null? headers)
	    '()
	    (let ((header (car headers)))
	      (let char-loop ((pre '()) (post header))
		(if (null? post)
		  (header-loop (cdr headers))
		  (if (char=? #\: (car post))
		    (cons (make-mime-header
			    (list->string (reverse pre))
			    (list->string post))
		      (header-loop (cdr headers)))
		    (char-loop (cons (char-downcase (car post)) pre)
		      (cdr post)))))))))))

  (define character-set-size 256)

  (define marker-list
    '(#\: #\; #\? #\#))

  (define ascii-marker-list
    (map char->integer marker-list))

  (define marker-locations
    (make-vector character-set-size))

  (define first-position-of-marker
    (lambda (c)
      (vector-ref marker-locations (char->integer c))))

  ;; netscape/string->url : str -> url
  (define netscape/string->url
    (lambda (string)
      (let ((url (string->url string)))
	(if (url-scheme url)
	  url
	  (if (string=? string "")
	    (url-error "Can't resolve empty string as URL")
	    (begin
	      (set-url-scheme! url
		(if (char=? (string-ref string 0) #\/)
		  "file"
		  "http"))
	      url))))))

  ;; string->url : str -> url
  (define string->url
    (lambda (string)
      (let loop ((markers ascii-marker-list))
	(unless (null? markers)
	  (vector-set! marker-locations (car markers) #f)
	  (loop (cdr markers))))
      (let loop ((chars (string->list string)) (index 0))
	(unless (null? chars)
	  (let ((first (car chars)))
	    (when (memq first marker-list)
	      (let ((posn (char->integer first)))
		(unless (vector-ref marker-locations posn)
		  (vector-set! marker-locations posn index)))))
	  (loop (cdr chars) (add1 index))))
      (let
	((first-colon (first-position-of-marker #\:))
	  (first-semicolon (first-position-of-marker #\;))
	  (first-question (first-position-of-marker #\?))
	  (first-hash (first-position-of-marker #\#)))
	(let
	  ((scheme-start (and first-colon 0))
	    (path-start (if first-colon (add1 first-colon) 0))
	    (params-start (and first-semicolon (add1 first-semicolon)))
	    (query-start (and first-question (add1 first-question)))
	    (fragment-start (and first-hash (add1 first-hash))))
	  (let ((total-length (string-length string)))
	    (let*
	      ((scheme-finish (and scheme-start first-colon))
		(path-finish (if first-semicolon first-semicolon
			       (if first-question first-question
				 (if first-hash first-hash
				   total-length))))
		(fragment-finish (and fragment-start total-length))
		(query-finish (and query-start
				(if first-hash first-hash
				  total-length)))
		(params-finish (and params-start
				 (if first-question first-question
				   (if first-hash first-hash
				     total-length)))))
	      (let ((scheme (and scheme-start
			      (substring string
				scheme-start scheme-finish))))
		(if (and scheme
		      (string=? scheme "file"))
		  (make-url
		    scheme
		    #f			; host
		    #f			; port
		    (build-path (substring string path-start total-length))
		    #f			; params
		    #f			; query
		    #f)			; fragment
		  (let-values (((host port path)
				 (parse-host/port/path
				   string path-start path-finish)))
		    (make-url
		      scheme
		      host
		      port
		      path
		      (and params-start
			(substring string params-start params-finish))
		      (and query-start
			(substring string query-start query-finish))
		      (and fragment-start
			(substring string fragment-start
			  fragment-finish))))))))))))

  ;; parse-host/port/path : str x num x num -> (str + #f) + (num + #f) + str
  (define parse-host/port/path
    (lambda (path begin-point end-point)
      (let ((has-host? (and (>= (- end-point begin-point) 2)
			 (char=? (string-ref path begin-point) #\/)
			 (char=? (string-ref path (add1 begin-point))
			   #\/))))
	(let ((begin-point (if has-host?
			     (+ begin-point 2)
			     begin-point)))
	  (let loop ((index begin-point)
		      (first-colon #f)
		      (first-slash #f))
	    (cond
	      ((>= index end-point)
		;; We come here only if the string has not had a /
		;; yet.  This can happen in two cases:
		;; 1. The input is a relative URL, and the hostname
		;;    will not be specified.  In such cases, has-host?
		;;    will be false.
		;; 2. The input is an absolute URL with a hostname,
		;;    and the intended path is "/", but the URL is missing
		;;    a "/" at the end.  has-host? must be true.
		(let ((host/path (substring path begin-point end-point)))
		  (if has-host?
		    (values host/path #f "/")
		    (values #f #f host/path))))
	      ((char=? #\: (string-ref path index))
		(loop (add1 index) (or first-colon index) first-slash))
	      ((char=? #\/ (string-ref path index))
		(if first-colon
		  (values
		    (substring path begin-point first-colon)
		    (string->number (substring path (add1 first-colon)
				      index))
		    (substring path index end-point))
		  (if has-host?
		    (values
		      (substring path begin-point index)
		      #f
		      (substring path index end-point))
		    (values
		      #f
		      #f
		      (substring path begin-point end-point)))))
	      (else
		(loop (add1 index) first-colon first-slash))))))))

  )

