
(define example-list%
  (class '() (parents [filter (lambda (x) (not (void? x)))])
      (public
       [items '()]
       [num-items 0]
       
       [parents-count 
	(if parents
	    (map (lambda (parent)
		   (ivar parent count))
		 parents)
	    '())]
       [parents-choose
	(if parents
	    (map (lambda (parent)
		   (ivar parent choose-example))
		 parents)
	    '())]
       [choose-parent-example
	(lambda (which)
	  (let loop ([pos which][counts parents-count][chooses parents-choose])
	    (if (null? counts)
		(void)
		(let ([c ((car counts))])
		  (if (< pos c)
		      ((car chooses) pos)
		      (loop (- pos c) (cdr counts) (cdr chooses)))))))]

       [count 
	(lambda () (+ num-items (apply + (map (lambda (x) (x)) parents-count))))]
       [set-filter
	(lambda (f)
	  (set! filter f))]
       [add
	(lambda (x)
	  (when (filter x)
		(set! num-items (add1 num-items))
		(set! items (cons x items))))]
       [all-examples
	(lambda ()
	  (apply append items (map (lambda (p) (send p all-examples)) parents)))]
       [choose-example
	(opt-lambda ([which #f])
	  (let ([n (if which 
		       which
		       (let ([c (count)])
			 (if (zero? c)
			     0
			     (random c))))])
	    (if (< n num-items)
		(list-ref items n)
		(choose-parent-example (- n num-items)))))])))

(define boxed-example-list%
  (class () (null-ok? parent)
    (public
     [all-examples
      (lambda ()
	(let ([l (map box (send parent all-examples))])
	  (if null-ok?
	      (cons '() l)
	      l)))]
     [choose-example
      (opt-lambda ([which #f])
	(if (and null-ok? (zero? (random 2)))
	    '()
	    (let ([ex (send parent choose-example)])
	      (if (void? ex)
		  (void)
		  (box ex)))))])))

(define array-example-list%
  (class () (parent)
    (public
     [all-examples
      (lambda ()
	(let ([v1 (cons (send parent choose-example) '())]
	      [v2 (cons (send parent choose-example) '())])
	  (set-cdr! v1 v1) ; cycle
	  (set-cdr! v2 (send parent choose-example)) ; improper
	  (list v1 v2 (send parent all-examples))))]
     [choose-example
      (opt-lambda ([which #f])
	(let ([ex (send parent choose-example)])
	  (if (void? ex)
	      (void)
	      (if (zero? (random 10))
		  ; occasionally pick a mean one
		  (let ([v (cons ex '())])
		    (if (zero? (random 2))
			(set-cdr! v v) ; cycle
			(set-cdr! v (send parent choose-example))) ; improper
		    v)
		  (let loop ([count (random 10)])
		    (cond
		     [(zero? count) '()]
		     [(= count 1) (list ex)]
		     [else
		      (cons (send parent choose-example) (loop (sub1 count)))]))))))])))

(define-macro define-main 
  (lambda list
    (let loop ([l list][rest '()])
      (if (null? l)
	  (cons 'begin rest)
	  (loop (cdr l)
		(let* ([first (car l)]
		       [name (if (symbol? first)
				 first
				 (car first))]
		       [strname (symbol->string name)]
		       [bases (if (symbol? first)
				  ()
				  (cdr first))]
		       [el-name (lambda (s)
				  (if s
				      (string->symbol
				       (string-append
					(symbol->string s)
					"-example-list"))
				      #f))])
		  (cons
		   `(define ,(el-name name)
		      (make-object example-list% (list ,@(map el-name bases))
				   (lambda (v) (if (null? v)
						   (error ,name "got null")))))
		   (if (char=? #\! (string-ref strname (sub1 (string-length strname))))
		       (let* ([base (substring strname 0 (sub1 (string-length strname)))]
			      [caret (string->symbol (string-append base "^"))]
			      [percent (string->symbol (string-append base "%"))])
			 (list*
			  `(define ,(el-name caret)
			     (make-object example-list% (list ,(el-name name))))
			  `(define ,(el-name percent)
			     (make-object example-list% (list ,(el-name name))))
			  `(send ,(el-name caret) add '())
			  rest))
		       rest))))))))

(define-main
  void
  char
  ubyte
  int
  string
  bool
  float

  pathname

  void*
  istream%
  ostream%

  wxFunction
  wxKeyErrorFunction
  wxKeyFunction
  wxMouseFunction
  wxBreakSequenceFunction
  wxGrabMouseFunction
  wxGrabKeyFunction
  wxClickbackFunc
  wxWordbreakFunc

  (wxObject! wxWindow! wxItem! wxColour! wxList!)

  wxPoint!
  wxIntPoint!

  wxButton!
  wxColour!
  wxFont!
  wxBrush!
  wxPen!

  wxFontList!
  wxPenList!
  wxBrushList!
  wxColourDatabase!
  wxFontNameDirectory!

  wxColourMap!
  wxCursor!
  wxIcon!
  wxBitmap!

  (wxEvent! wxCommandEvent! wxMouseEvent! wxKeyEvent!)
  wxCommandEvent!
  wxMouseEvent!
  wxKeyEvent!

  (wxDC! wxCanvasDC! wxPanelDC! wxMemoryDC! wxPostScriptDC!)
  wxCanvasDC!
  wxPanelDC!
  wxMemoryDC!
  wxPostScriptDC!

  basePrinterDC!
  baseMetaFileDC!
  
  baseMetaFile!

  (wxWindow! wxFrame! wxCanvas! wxItem!)

  wxFrame!
  wxTextWindow!
  (wxCanvas! wxPanel! wxMediaCanvas!)
  (wxPanel! wxDialogBox!)
  wxDialogBox!
  wxMediaCanvas!

  (wxItem! wxButton! wxCheckBox! wxChoice!
	   wxListBox! wxSlider! wxsGauge! wxText! wxMultiText! 
	   wxRadioBox! wxMessage! wxGroupBox!)
  wxButton!
  wxCheckBox!
  wxChoice!
  wxListBox!
  wxSlider!
  wxsGauge!
  wxText!
  wxMultiText!
  wxMessage!
  wxRadioBox!
  wxGroupBox!

  wxMenu!
  wxMenuBar!

  wxNode!
  wxList!

  wxHashTable!
  wxPathList!
  wxStringList!

  wxConnection!
  (wxIPCObject! wxClient! wxServer!)
  wxClient!
  wxServer!

  wxTimer!
  wxTypeTree!

  wxToolBarTool!
  wxToolBar!

  wxLayoutConstraints!

  wxAddColour!
  wxMultColour!
  wxStyleDelta!
  wxStyle!
  wxStyleList!

  (wxMediaAdmin! wxCanvasMediaAdmin! wxMediaSnipMediaAdmin!)
  wxCanvasMediaAdmin!
  wxMediaSnipMediaAdmin!
  wxSnipAdmin!
  
  (wxMediaBuffer! wxMediaEdit! wxMediaPasteboard!)
  wxMediaEdit!
  wxMediaPasteboard!

  (wxSnip! wxTextSnip! wxImageSnip! wxMediaSnip!)
  (wxTextSnip! wxTabSnip!)
  wxTabSnip!
  wxImageSnip!
  wxMediaSnip!

  wxSnipClass!
  wxSnipClassList!

  wxBufferData!
  wxBufferDataClass!
  wxBufferDataClassList!

  wxKeymap!
  wxMediaWordbreakMap!

  (wxMediaStreamInBase! wxMediaStreamInStringBase!)
  (wxMediaStreamOutBase! wxMediaStreamOutStringBase!)

  wxMediaStreamInStringBase!
  wxMediaStreamOutStringBase!

  wxMediaStreamIn!
  wxMediaStreamOut!
  
  wxClipboard!
  wxClipboardClient!

  Scheme_Object*)

(send wxBitmap!-example-list set-filter (lambda (bm) (send bm ok?)))

(define-macro define-boxed 
  (lambda list
    (let ([make
	   (lambda (s tag)
	     (string->symbol
	      (string-append
	       (symbol->string s)
	       tag
	       "-example-list")))])
      (let loop ([l list][rest '()])
	(if (null? l)
	    (cons 'begin rest)
	    (loop (cdr l)
		  (cons `(define ,(make (car l) "*")
			   (make-object boxed-example-list% #f ,(make (car l) "")))
			(cons `(define ,(make (car l) "?")
				 (make-object boxed-example-list% #t ,(make (car l) "")))
			      rest))))))))

(define-macro define-array 
  (lambda list
    (let ([make
	   (lambda (s tag)
	     (string->symbol
	      (string-append
	       (symbol->string s)
	       tag
	       "-example-list")))])
      (let loop ([l list][rest '()])
	(if (null? l)
	    (cons 'begin rest)
	    (loop (cdr l)
		  (cons `(define ,(make (car l) "ARRAY")
			   (make-object array-example-list% ,(make (car l) "")))
			rest)))))))

(define nstring-example-list (make-object example-list% (list string-example-list)))
(send nstring-example-list add '())

(define long-example-list int-example-list)
(define Long-example-list int-example-list)
(define short-example-list int-example-list)
(define Bool-example-list int-example-list)
(define _KEY_TYPE-example-list int-example-list)
(define uchar-example-list char-example-list)
(define double-example-list float-example-list)
(define Double-example-list double-example-list)
(define cstring-example-list string-example-list)
(define ustring-example-list string-example-list)
(define custring-example-list string-example-list)
(define ncstring-example-list nstring-example-list)
(define ncustring-example-list nstring-example-list)

(define voidARRAY-example-list (make-object example-list% null))
(define CAPOFunc-example-list (make-object example-list% null))

(define false-example-list (make-object example-list% '()))
(send false-example-list add #f)

(define-boxed
  int
  string
  bool
  ubyte
  float
  Double
  Long
  long
  short
  wxSnip!)

(define Double+-example-list Double*-example-list)
(define long+-example-list long*-example-list)
(define Long+-example-list Long*-example-list)

(define wxBitmap*-example-list wxBitmap!-example-list)
(define wxMenu*-example-list wxMenu!-example-list)

(define-array
  char
  string
  int
  long
  float
  wxBitmap*
  wxMenu*)

(define int**-example-list 
  (make-object array-example-list% int*-example-list))

(send* bool-example-list
       (add #t)
       (add #f))

(send* int-example-list
       (add 0) (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0) (add 0)
       (add -1)
       (add -2)
       (add -3)
       (add -1000)
       (add 1)
       (add 2)
       (add 3)
       (add 4)
       (add 5)
       (add 6)
       (add 7)
       (add 8)
       (add 9)
       (add 10)
       (add 16)
       (add 32)
       (add 64)
       (add 128)
       (add 256)
       (add 255)
       (add 1023)
       (add 1000)
       (add 5.0))

(send* ubyte-example-list
       (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0)
       (add 1)
       (add 2)
       (add 3)
       (add 4)
       (add 5)
       (add 6)
       (add 7)
       (add 8)
       (add 9)
       (add 10)
       (add 16)
       (add 32)
       (add 64)
       (add 128)
       (add 255)
       (add 5))

(send* char-example-list
       (add #\nul)
       (add #\a)
       (add #\1)
       (add #\newline)
       (add #\tab)
       (add #\z)
       (add #\C))

(send* float-example-list
       (add 0.)
       (add 0.)
       (add 0.)
       (add -1.)
       (add -2.)
       (add -3.)
       (add -1000.)
       (add 1.)
       (add 2.)
       (add 3.)
       (add 1000.)
       (add 5))

(send* string-example-list
       (add "")
       (add "hello")
       (add "system/mred.xbm")
       (add "system/mred.bmp")
       (add "mred.gif")
       (add "goodbye adious see you later zai jian seeya bye-bye"))

(send pathname-example-list add "/tmp/x")
(define npathname-example-list (make-object example-list% (list string-example-list)))
(send npathname-example-list add '())

(send wxFunction-example-list add void)
(send wxKeyErrorFunction-example-list add void)
(send wxKeyFunction-example-list add void)
(send wxMouseFunction-example-list add void)
(send wxClickbackFunc-example-list add void)
(send wxWordbreakFunc-example-list add void)

(define classinfo (make-hash-table))

(load-relative "tests.ss")

(define (get-args l)
  (let/ec bad
	  (let loop ([l l])
	    (if (null? l)
		'()
		(let* ([source (car l)]
		       [value (send source choose-example #f)])
		  (if (void? value)
		      (bad #f)
		      (cons value (loop (cdr l)))))))))

(define (get-all-args l)
  (let loop ([l l])
    (if (null? l)
	'()
	(let* ([source (car l)]
	       [values (send source all-examples)]
	       [rest (loop (cdr l))])
	  (if (null? (cdr l))
	      (list values)
	      (apply append
		     (map (lambda (other)
			    (map (lambda (v) (cons v other)) values))
			  rest)))))))

(define thread-output-port 
  (let ([p mred:original-output-port])
    (lambda ()
      p)))
  

(define (apply-args v dest name k)
  (if v
      (begin
	(fprintf (thread-output-port) "~a: ~a" name v)
	(flush-output (thread-output-port))
	(with-handlers ((void (lambda (x)
				(fprintf (thread-output-port)
					 ": error: ~a~n"
					 (exn-message x)))))
		       (send dest add (k v))
		       (wx:flush-display)
		       (fprintf (thread-output-port) ": success~n")))
      (fprintf (thread-output-port) "~a: failure~n" name)))

(define (try-args arg-types dest name k)
  (apply-args (get-args arg-types) dest name k))

(define (try-all-args arg-types dest name k)
  (let ([vs (get-all-args arg-types)])
    (map (lambda (v)
	   (apply-args v dest name k))
	 vs)))

(define (create-some cls try)
  (when (class? cls)
	(let* ([v (hash-table-get classinfo cls)]
	       [dest (car v)]
	       [name (cadr v)]
	       [creators (caddr v)])
	  (let loop ([l creators])
	    (unless (null? l)
		    (try (car l) dest name
			 (lambda (v)
			   (apply make-object cls v)))
		    (loop (cdr l)))))))

(define (create-all-random)
  (fprintf (thread-output-port) "creating all randomly...~n")
  (hash-table-for-each classinfo (lambda (k v)
				   (create-some k try-args))))
(define (create-all-exhaust)
  (fprintf (thread-output-port) "creating all exhaustively...~n")
  (hash-table-for-each classinfo (lambda (k v)
				   (create-some k try-all-args))))

(define (try-methods cls try)
  (let* ([v (hash-table-get classinfo cls)]
	 [source (car v)]
	 [use (if source (send source choose-example) #f)]
	 [name (cadr v)]
	 [methods (cdddr v)])
    (if (void? use)
	(fprintf (thread-output-port) "~s: no examples~n" name)
	(let loop ([l methods])
	  (unless (null? l)
		  (let* ([method (car l)]
			 [iv (car method)]
			 [resulttype (cadr method)]
			 [argtypes (cddr method)])
		    (try argtypes resulttype (list name iv use)
			 (lambda (args)
			   (if use
			       (apply (uq-ivar use iv) args)
			       (apply (global-defined-value iv) args)))))
		  (loop (cdr l)))))))

(define (call-random except)
  (fprintf (thread-output-port) "calling all except ~a randomly...~n" except)
  (hash-table-for-each classinfo (lambda (k v)
				   (unless (member k except)
					   (try-methods k try-args)))))

(define (call-all-random)
  (call-random null))

(define (call-all-non-media)
  (call-random (list wx:media-buffer% wx:media-edit% wx:media-snip% wx:media-pasteboard% 'wxMediaGlobal)))

(define (init)
  (create-all-random)
  (create-all-random)
  (create-all-random)
  (create-all-random))

