
(define my-txt #f)

(define special-font (send wx:the-font-list find-or-create-font
			   20 wx:const-decorative 
			   wx:const-bold wx:const-normal
			   #f))

(define (make-h&s cp f)
  (make-object mred:button% cp
	       (lambda (b e) (send f show #f) (send f show #t))
	       "Hide and Show"))

(define (add-hide name w cp)
  (let ([c
	 (make-object mred:check-box% cp
		      (lambda (c e) (send w show (send c get-value)))
		      (format "Show ~a" name))])
    (send c set-value #t)))

(define (add-disable name w ep)
  (let ([c
	 (make-object mred:check-box% ep
		      (lambda (c e) (send w enable (send c get-value)))
		      (format "Enable ~a" name))])
    (send c set-value #t)))

(define (add-disable-radio name w i ep)
  (let ([c
	 (make-object mred:check-box% ep
		      (lambda (c e) (send w enable i (send c get-value)))
		      (format "Enable ~a" name))])
    (send c set-value #t)))

(define (add-change-label name w lp orig other)
  (make-object mred:button% lp
	       (let ([orig-name (if orig orig (send w get-label))]
		     [changed? #f])
		 (lambda (b e)
		   (if changed?
		       (unless (null? orig-name)
			 (send w set-label orig-name))
		       (send w set-label other))
		   (set! changed? (not changed?))))
	       (format "Relabel ~a" name)))

(define (add-focus-note frame panel)
  (define m (make-object mred:message% panel "focus: ??????????????????????????????"))
  (send
   (make-object
    (class-asi wx:timer%
      (inherit start)
      (public
	[notify
	 (lambda ()
	   (when (send frame is-shown?)
	     (send m set-label
		   (format "focus: ~s" (mred:test:get-focused-window)))
	     (start 1000 #t)))])))
   start 1000 #t))

(define OTHER-LABEL "XXXXXXXXXXXXXXXXXXXXXX")

(define-values (icons-path local-path)
  (let ([d (current-load-relative-directory)])
    (values
     (lambda (n)
       (build-path (collection-path "icons") n))
     (lambda (n)
       (build-path d n)))))

(define popup-test-canvas%
  (class mred:canvas% (objects names . args)
    (inherit popup-menu draw-text clear)
    (public
      [last-m null]
      [last-choice #f]
      [on-paint
       (lambda ()
	 (clear)
	 (draw-text "Left: popup hide state" 0 0)
	 (draw-text "Right: popup previous" 0 20)
	 (draw-text (format "Last pick: ~s" last-choice) 0 40))]
      [on-event
       (lambda (e)
	 (if (send e button-down?)
	     (let ([x (send e get-x)]
		   [y (send e get-y)]
		   [m (if (or (null? last-m)
			      (send e button-down? 1))
			  (let ([m (make-object mred:menu%
						"Title"
						(lambda (m e)
						  (set! last-choice
							(send e get-command-int))
						  (on-paint)))]
				[id 1])
			    (for-each
			     (lambda (obj name)
			       (send m append
				     (begin0 id (set! id (add1 id)))
				     (string-append
				      name ": "
				      (if (send obj is-shown?)
					  "SHOWN"
					  "<h i d d e n>"))))
			     objects names)
			    m)
			  last-m)])
	       (set! last-m m)
	       (popup-menu m x y))))])
    (sequence
      (apply super-init args))))

(define prev-frame #f)

(define bitmap%
  (class wx:bitmap% args
    (inherit ok?)
    (sequence
      (apply super-init args)
      (unless (ok?)
	(printf "bitmap failure: ~s~n" args)))))

(define (make-ctls ip cp lp add-testers ep radio-h? label-h? null-label? stretchy?)
  
  (define return-bmp 
    (make-object bitmap% (icons-path "return.xbm")
		 wx:const-bitmap-type-xbm))
  (define bb-bmp
    (make-object bitmap% (icons-path "bb.gif")
		 wx:const-bitmap-type-gif))
  (define mred-bmp
    (make-object bitmap% (icons-path "mred.xbm")
		 wx:const-bitmap-type-xbm))
  (define nruter-bmp
    (make-object bitmap% (local-path "nruter.xbm")
		 wx:const-bitmap-type-xbm))
  
  (define :::dummy:::
    (when (not label-h?)
      (send ip set-label-position wx:const-vertical)))
  
  (define-values (l il)
    (let ([p (make-object mred:horizontal-panel% ip)])
      (send p stretchable-in-x stretchy?)
      (send p stretchable-in-y stretchy?)
      
      (begin
	(define l (make-object mred:message% p "Me&ssage"))
	(define il (make-object mred:message% p return-bmp))
	
	(add-testers "Message" l)
	(add-change-label "Message" l lp #f OTHER-LABEL)
	
	(add-testers "Image Message" il)
	(add-change-label "Image Message" il lp return-bmp nruter-bmp)
	
	(values l il))))
  
  (define b (make-object mred:button% ip void "He&llo"))
  
  (define ib (make-object mred:button% ip void bb-bmp))
  
  ; (define ib2 (make-object mred:button% ip void return-bmp))
  
  (define lb (make-object mred:list-box% ip void 
			  (if null-label? null "L&ist")
			  0 -1 -1 -1 -1
			  '("Apple" "Banana" "Coconut & Donuts")))
  
  (define cb (make-object mred:check-box% ip void "C&heck"))
  
  (define icb (make-object mred:check-box% ip void mred-bmp))
  
  (define rb (make-object mred:radio-box% ip void 
			  (if null-label? null "R&adio")
			  -1 -1 -1 -1
			  '("First" "Dos" "T&rio")
			  0 (if radio-h?
				wx:const-horizontal
				wx:const-vertical)))
  
  (define irb (make-object mred:radio-box% ip void 
			   (if null-label? null "Image Ra&dio")
			   -1 -1 -1 -1
			   (list return-bmp nruter-bmp)
			   0 (if radio-h?
				 wx:const-horizontal
				 wx:const-vertical)))
  
  (define ch (make-object mred:choice% ip void 
			  (if null-label? null "Ch&oice")
			  -1 -1 -1 -1
			  '("Alpha" "Beta" "Gamma" "Delta & Rest")))
  
  (define txt (make-object mred:text% ip void 
			   (if null-label? null "T&ext")
			   "initial & starting"
			   -1 -1 -1 -1))
  
  (set! my-txt txt)
  
  (add-testers "Button" b)
  (add-change-label "Button" b lp #f OTHER-LABEL)
  
  (add-testers "Image Button" ib)
  (add-change-label "Image Button" ib lp bb-bmp return-bmp)
  
  (add-testers "List" lb)
  (add-change-label "List" lb lp #f OTHER-LABEL)
  
  (add-testers "Checkbox" cb)
  (add-change-label "Checkbox" cb lp #f OTHER-LABEL)
  
  (add-testers "Image Checkbox" icb)
  (add-change-label "Image Checkbox" icb lp mred-bmp bb-bmp)
  
  (add-testers "Radiobox" rb)
  (add-disable-radio "Radio Item `First'" rb 0 ep)
  (add-disable-radio "Radio Item `Dos'" rb 1 ep)
  (add-disable-radio "Radio Item `Trio'" rb 2 ep)
  (add-change-label "Radiobox" rb lp #f OTHER-LABEL)
  
  (add-testers "Image Radiobox" irb)
  (add-disable-radio "Radio Image Item 1" irb 0 ep)
  (add-disable-radio "Radio Image Item 2" irb 1 ep)
  (add-change-label "Image Radiobox" irb lp #f OTHER-LABEL)
  
  (add-testers "Choice" ch)
  (add-change-label "Choice" ch lp #f OTHER-LABEL)
  
  (add-testers "Text" txt)
  (add-change-label "Text" txt lp #f OTHER-LABEL)
  
  (make-object popup-test-canvas% 
	       (list l il 
		     b ib
		     lb
		     cb icb
		     rb irb
		     ch
		     txt)
	       (list "label" "image label"
		     "button" "image button"
		     "list box"
		     "checkbox" "image checkbox"
		     "radio box" "image radiobox"
		     "choice"
		     "text")
	       cp))

(define (big-frame h-radio? v-label? null-label? stretchy? special-font?)
  (define f (make-object mred:frame% null "Tester"))
  
  (define hp (make-object mred:horizontal-panel% f))
  
  (define ip (make-object mred:vertical-panel% hp))
  (define cp (make-object mred:vertical-panel% hp))
  (define ep (make-object mred:vertical-panel% hp))
  (define lp (make-object mred:vertical-panel% hp))
  
  (define (basic-add-testers name w)
    (add-hide name w cp)
    (add-disable name w ep))
  
  (define add-testers
    (if stretchy?
	(lambda (name control)
	  (send control stretchable-in-x #t)
	  (send control stretchable-in-y #t)
	  (basic-add-testers name control))
	basic-add-testers))
  
  (define fp (make-object mred:vertical-panel% ip))
  
  (define tp (make-object mred:vertical-panel% fp))

  (make-h&s cp f)
  
  (add-testers "Sub-panel" fp)
  
  (add-testers "Sub-sub-panel" tp)

  (when special-font?
    (send tp set-label-font special-font))
    
  (make-ctls tp cp lp add-testers ep h-radio? v-label? null-label? stretchy?)
  
  (add-focus-note f ep)
  
  (send f show #t)
  (set! prev-frame f)
  f)

(define (med-frame radio-h? label-h? null-label? stretchy? special-font?)
  (define f2 (make-object mred:frame% null "Tester2"))
  
  (define hp2 (make-object mred:horizontal-panel% f2))
  
  (define ip2 (make-object mred:vertical-panel% hp2))
  (define cp2 (make-object mred:vertical-panel% hp2))
  (define ep2 (make-object mred:vertical-panel% hp2))
  (define lp2 (make-object mred:vertical-panel% hp2))
  
  (define (basic-add-testers2 name w)
    (add-hide name w cp2)
    (add-disable name w ep2))
  
  (define add-testers2
    (if stretchy?
	(lambda (name control)
	  (send control stretchable-in-x #t)
	  (send control stretchable-in-y #t)
	  (basic-add-testers2 name control))
	basic-add-testers2))
  
  (make-h&s cp2 f2)
  
  (add-disable "Previous Tester Frame" prev-frame ep2)
  
  (when (not label-h?)
    (send ip2 set-label-position wx:const-vertical))

  (when special-font?
    (send ip2 set-label-font special-font))
  
  (begin
    (define sh (make-object mred:slider% ip2
			    (lambda (s e)
			      (send gh set-value (send sh get-value)))
			    (if null-label? null "H S&lider")
			    5 0 10 -1 -1 -1
			    wx:const-horizontal))
    
    (define sv (make-object mred:slider% ip2 
			    (lambda (s e)
			      (send gv set-value (send sv get-value)))
			    (if null-label? null "V Sl&ider")
			    5 0 10 -1 -1 -1
			    wx:const-vertical))
    
    (define gh (make-object mred:gauge% ip2
			    (if null-label? null "H G&auge")
			    10 -1 -1 -1 -1
			    wx:const-horizontal))
    
    (define gv (make-object mred:gauge% ip2
			    (if null-label? null "V Ga&uge")
			    10 -1 -1 -1 -1
			    wx:const-vertical))
    
    (define cmt (make-object mred:canvas-message% ip2
			     "Howdy"))
    
    (define cmi (make-object mred:canvas-message% ip2
			     (make-object bitmap% (icons-path "bb.gif")
					  wx:const-bitmap-type-gif)))
    

    (define txt (make-object mred:media-text% ip2 void 
			     (if null-label? null "T&ext")
			     "initial & starting"
			     -1 -1 -1 -1))

    (add-testers2 "Horiz Slider" sh)
    (add-testers2 "Vert Slider" sv)
    (add-testers2 "Horiz Gauge" gh)
    (add-testers2 "Vert Gauge" gv)
    (add-testers2 "Text Message" cmt)
    (add-testers2 "Image Message" cmi)
    (add-testers2 "Text" txt)
    
    (add-change-label "Horiz Slider" sh lp2 #f OTHER-LABEL)
    (add-change-label "Vert Slider" sv lp2 #f OTHER-LABEL)
    (add-change-label "Horiz Gauge" gh lp2 #f OTHER-LABEL)
    (add-change-label "Vert Gauge" gv lp2 #f OTHER-LABEL)
    (add-change-label "Text" txt lp2 #f OTHER-LABEL)
    
    (add-focus-note f2 ep2)
    
    (send f2 show #t)
    (set! prev-frame f2)
    f2))

; Need: check, check-test, and enable via menubar
; All operations on Submenus
(define f%
  (let-enumerate 
      ([ADD-APPLE
	ADD-BANANA
	ADD-COCONUT
	DELETE-APPLE
	DELETE-BANANA
	DELETE-COCONUT-0
	DELETE-COCONUT
	DELETE-COCONUT-2
	COCONUT-ID
	DELETE-ONCE
	APPLE-CHECK-ID])
    (class mred:menu-frame% args
      (inherit next-menu-id make-menu)
      (rename
	[super-make-menu-bar make-menu-bar]
	[super-on-menu-command on-menu-command])
      (private
	offset
	menu-bar
	main-menu
	apple-menu
	banana-menu
	coconut-menu
	baseball-ids
	hockey-ids
	enable-item)
      (public
	[make-menu-bar
	 (lambda ()
	   (let ([mb (super-make-menu-bar)]
		 [menu (make-menu)])
	     (set! offset (next-menu-id))
	     (set! menu-bar mb)
	     (set! main-menu menu)
	     
	     (send menu append (+ offset ADD-APPLE) "Add Apple" "Adds the Apple menu")
	     (send menu append (+ offset ADD-BANANA) "Add Banana")
	     (send menu append (+ offset ADD-COCONUT) "Add Coconut")
	     (send menu append-item "Append Donut"
		   (lambda () (send apple-menu append-item "Donut" void)))
	     (send menu append-separator)
	     (send menu append (+ offset DELETE-COCONUT-0)
		   "Delete Coconut")
	     (send menu append-item "Delete Apple"
		   (lambda () 
		     (send menu-bar delete apple-menu)
		     (set! apple-installed? #f)))
	     
	     (send menu append-separator)
	     (set! enable-item
		   (send menu append-item "Apple Once Disabled"
			 (lambda ()
			   (send apple-menu enable DELETE-ONCE
				 (not (send menu checked? enable-item))))
			 null #t))
	     (send menu append-item "Disable Second"
		   (lambda () (send menu-bar enable-top 1 #f)))
	     (send menu append-item "Enable Second"
		   (lambda () (send menu-bar enable-top 1 #t)))
	     
	     (send menu append-separator)
	     (set! baseball-ids
		   (send menu append-check-set
			 (list "Astros" "Braves" "Cardinals")
			 (lambda (which)
			   (wx:message-box (format "~s Checked" which)))))
	     (send menu append-separator)
	     (set! hockey-ids
		   (send menu append-check-set
			 `(("Aeros" . Houston) 
			   ("Bruins" . Boston)
			   ("Capitols" . Washington))
			 (lambda (which)
			   (wx:message-box (format "~s Checked" which)))))
	     
	     (set! apple-menu (make-menu))
	     (set! banana-menu (make-menu))
	     (set! coconut-menu (make-menu))
	     
	     (send apple-menu append (+ offset DELETE-ONCE)
		   "Delete Once")
	     (send apple-menu append (+ offset DELETE-APPLE)
		   "Delete Apple" "Deletes the Apple menu")
	     (send apple-menu append (+ offset APPLE-CHECK-ID)
		   "Checkable" null #t)
	     
	     (send banana-menu append (+ offset DELETE-BANANA)
		   "Delete Banana")
	     (send coconut-menu append (+ offset DELETE-COCONUT)
		   "Delete Coconut")
	     (send coconut-menu append (+ offset DELETE-COCONUT-2)
		   "Delete Coconut By Position")
	     
	     (send mb append menu "Tester")
	     (send mb append apple-menu "Appul")
	     (send mb enable-top 1 #f)
	     (send mb set-label-top 1 "Apple")
	     mb))]
	[on-menu-command
	 (lambda (orig-op)
	   (let ([op (- orig-op offset)])
	     (cond
	       [(= op ADD-APPLE)
		(send menu-bar append apple-menu "Apple")
		(set! apple-installed? #t)]
	       [(= op ADD-BANANA)
		(send menu-bar append banana-menu "Banana")]
	       [(= op ADD-COCONUT)
		(send apple-menu append (+ offset COCONUT-ID)
		      "Coconut" coconut-menu "Submenu")]
	       [(= op DELETE-ONCE)
		(send apple-menu delete (+ offset DELETE-ONCE))]
	       [(= op DELETE-APPLE)
		(send menu-bar delete apple-menu)
		(set! apple-installed? #f)]
	       [(= op DELETE-BANANA)
		(send menu-bar delete banana-menu)]
	       [(or (= op DELETE-COCONUT) (= op DELETE-COCONUT-0))
		(send apple-menu delete (+ offset COCONUT-ID))]
	       [(= op DELETE-COCONUT-2)
		(send apple-menu delete-by-position 3)]
	       [else
		(super-on-menu-command orig-op)])))])
      (sequence (apply super-init args))
      (public
	[mfp (make-object mred:vertical-panel% (ivar this panel))]
	[mc (make-object mred:wrapping-canvas% mfp -1 -1 200 200)]
	[restp (make-object mred:vertical-panel% mfp)]
	[mfbp (make-object mred:horizontal-panel% restp)]
	[lblp (make-object mred:horizontal-panel% restp)]
	[badp (make-object mred:horizontal-panel% restp)]
	[e (make-object mred:media-edit%)])
      (sequence
	(send restp stretchable-in-y #f)
	(send mc set-media e)
	(send e load-file (local-path "steps.txt")))
      (public
	[make-test-button
	 (lambda (name pnl menu id)
	   (make-object mred:button% pnl 
			(lambda (b e)
			  (wx:message-box
			   (if (send (via menu) checked? id)
			       "yes"
			       "no")
			   "Checked?"))
			(format "Test ~a" name)))]
	[make-bad-test
	 (lambda (method)
	   (lambda args
	     (method 777 #t)
	     (method 777 #f)
	     (method -1 #t)
	     (method -1 #f)))]
	[compare
	 (lambda (expect v kind)
	   (unless (or (and (string? expect) (string? v)
			    (string=? expect v))
		       (eq? expect v))
	     (error 'test-compare "~a mistmatch: ~s != ~s" kind expect v)))]
	[label-test
	 (lambda (menu id expect)
	   (let ([v (send menu get-label id)])
	     (compare expect v "label")))]
	[top-label-test
	 (lambda (pos expect)
	   (let ([v (send menu-bar get-label-top pos)])
	     (compare expect v "top label")))]
	[help-string-test
	 (lambda (menu id expect)
	   (let ([v (send menu get-help-string id)])
	     (compare expect v "help string")))]
	[find-test
	 (lambda (menu title expect string)
	   (let ([v (if use-menubar? 
			(send menu-bar find-menu-item title string)
			(send menu find-item string))])
	     (compare expect v (format "label search: ~a" string))))]
	[tell-ok
	 (lambda ()
	   (printf "ok~n"))]
	[temp-labels? #f]
	[use-menubar? #f]
	[apple-installed? #f]
	[via (lambda (menu) (if use-menubar? menu-bar menu))]
	[tmp-pick (lambda (a b) (if temp-labels? a b))]
	[apple-pick (lambda (x a b) (if (and use-menubar? (not apple-installed?))
					x
					(tmp-pick a b)))])
      (sequence
(make-test-button "Aeros" mfbp main-menu (list-ref hockey-ids 0))
      (make-test-button "Bruins" mfbp main-menu (list-ref hockey-ids 1))
      (make-test-button "Capitols" mfbp main-menu (list-ref hockey-ids 2))
      (make-test-button "Apple Item" mfbp apple-menu APPLE-CHECK-ID)
      (make-object mred:button% mfbp
		   (lambda args
		     (send (via apple-menu) check APPLE-CHECK-ID #t))
		   "Check in Apple")
			   
      (make-object mred:button% lblp 
		   (lambda args
		     (label-test (via main-menu) ADD-APPLE (tmp-pick "Apple Adder" "Add Apple"))
		     (help-string-test (via main-menu) ADD-APPLE (tmp-pick "ADDER" "Adds the Apple menu"))
		     (label-test (via main-menu) (car baseball-ids) (tmp-pick "'Stros" "Astros"))
		     (help-string-test (via main-menu) (car baseball-ids) (tmp-pick "Houston" null))
		     (label-test (via main-menu) (cadr hockey-ids) "Bruins")
		     (label-test (via apple-menu) DELETE-APPLE (apple-pick null "Apple Deleter" "Delete Apple"))
		     (help-string-test (via apple-menu) DELETE-APPLE (apple-pick null "DELETER"
										 "Deletes the Apple menu"))
		     (label-test (via apple-menu) COCONUT-ID (apple-pick null "Coconut!" "Coconut"))
		     (help-string-test (via apple-menu) COCONUT-ID (apple-pick null "SUBMENU" "Submenu"))
		     (label-test (via apple-menu) DELETE-COCONUT (apple-pick null "Coconut Deleter" "Delete Coconut")) ; submenu test
		     (help-string-test (via apple-menu) DELETE-COCONUT (apple-pick null "CDELETER" null))
		     (top-label-test 0 (if temp-labels? "Hi" "Tester"))
		     (top-label-test 1 (if apple-installed? "Apple" null))
		     (tell-ok))
		   "Test Labels")
      (make-object mred:button% lblp
		   (lambda args
		     (find-test main-menu (tmp-pick "Hi" "Tester")
				ADD-APPLE (tmp-pick "Apple Adder" "Add Apple"))
		     (find-test apple-menu "Apple" (apple-pick -1 DELETE-APPLE DELETE-APPLE)
				(tmp-pick "Apple Deleter" "Delete Apple"))
		     (find-test apple-menu "Apple" (apple-pick -1 COCONUT-ID COCONUT-ID)
				(tmp-pick "Coconut!" "Coconut"))
		     (find-test apple-menu "Apple" (apple-pick -1 DELETE-COCONUT DELETE-COCONUT)
				(tmp-pick "Coconut Deleter" "Delete Coconut"))
		     (tell-ok))
		   "Find Labels")
      (make-object mred:button% lblp
		   (lambda args
		     (set! temp-labels? (not temp-labels?))
		     (let ([menu (via main-menu)])
		       (send menu set-label ADD-APPLE (tmp-pick "Apple Adder" "Add Apple"))
		       (send menu set-label (car baseball-ids) (tmp-pick "'Stros" "Astros"))
		       (send apple-menu set-label DELETE-APPLE (tmp-pick "Apple Deleter" "Delete Apple"))
		       (send apple-menu set-label COCONUT-ID (tmp-pick "Coconut!" "Coconut"))
		       (send apple-menu set-label DELETE-COCONUT (tmp-pick "Coconut Deleter" "Delete Coconut"))
		       (send menu set-help-string ADD-APPLE (tmp-pick "ADDER" "Adds the Apple menu"))
		       (send menu set-help-string (car baseball-ids) (tmp-pick "Houston" null))
		       (send apple-menu set-help-string DELETE-APPLE (tmp-pick "DELETER" "Deletes the Apple menu"))
		       (send apple-menu set-help-string COCONUT-ID (tmp-pick "SUBMENU" "Submenu"))
		       (send apple-menu set-help-string DELETE-COCONUT (tmp-pick "CDELETER" null))
		       (send menu-bar set-label-top 0 (if temp-labels? "Hi" "Tester"))))
		   "Toggle Labels")
      (letrec ([by-bar (make-object mred:check-box% lblp
				    (lambda args
				      (set! use-menubar? (send by-bar get-value)))
				    "Via Menubar")])
	by-bar)

      (make-test-button "Bad Item" badp apple-menu 777)
      (make-test-button "Other Bad Item" badp apple-menu -1)
      (make-object mred:button% badp
		   (lambda args
		     (label-test main-menu 777 null)
		     (label-test main-menu -1 null)
		     (help-string-test main-menu 777 null)
		     (help-string-test main-menu -1 null)
		     (top-label-test -1 null)
		     (top-label-test 777 null)
		     (find-test main-menu "No way" -1 "Not in the menus")
		     (tell-ok))
		   "Bad Item Labels")
      (make-object mred:button% badp 
		   (make-bad-test (ivar main-menu check))
		   "Check Bad")
      (make-object mred:button% badp 
		   (make-bad-test (ivar main-menu enable))
		   "Enable Bad")
      (make-object mred:button% badp 
		   (make-bad-test (lambda (a b) (send main-menu delete a)))
		   "Delete Bad")

      #f))))

(define (menu-frame)
  (define mf (make-object f% null "Menu Test"))
  (set! prev-frame mf)
  (send mf show #t)
  mf)

(define (check-callback-event orig got e types silent?)
  (unless (eq? orig got)
	  (error "object not the same"))
  (unless (is-a? e wx:command-event%)
	  (error "bad event object"))
  (unless (eq? got (send e get-event-object))
	  (error "event object mismatch"))
  (let ([type (send e get-event-type)])
    (unless (member type types)
	    (error (format "bad event type: ~a" type))))
  (unless silent?
	  (printf "Callback Ok~n")))

(define (button-frame)
  (define f (make-object mred:frame% null "Button Test"))
  (define p (make-object mred:vertical-panel% f))
  (define old-list null)
  (define commands (list wx:const-event-type-button-command))
  (define sema (make-semaphore))
  (define b (make-object mred:button% p
			 (lambda (bx e)
			   (semaphore-post sema)
			   (set! old-list (cons e old-list))
			   (check-callback-event b bx e commands #f))
			 "Hit Me"))
  (define c (make-object mred:button% p
			 (lambda (c e)
			   (for-each
			    (lambda (e)
			      (check-callback-event b b e commands #t))
			    old-list)
			   (printf "All Ok~n"))
			 "Check"))
  (define e (make-object mred:button% p
			 (lambda (c e)
			   (sleep 1)
			   (wx:yield) ; try to catch a click, but not a callback
			   (set! sema (make-semaphore))
			   (send b enable #f)
			   (thread (lambda () (wx:yield sema)))
			   (when (semaphore-wait-multiple (list sema) 0.5)
				 (printf "un-oh~n"))
			   (send b enable #t)
			   (semaphore-post sema))
			 "Disable Test"))
  (send f show #t))

(define (checkbox-frame)
  (define f (make-object mred:frame% null "Checkbox Test"))
  (define p (make-object mred:vertical-panel% f))
  (define old-list null)
  (define commands (list wx:const-event-type-checkbox-command))
  (define cb (make-object mred:check-box% p
			  (lambda (cx e)
			    (set! old-list (cons e old-list))
			    (unless (eq? (send cb get-value)
					 (send e checked?))
				    (error "event checkstate mismatch"))
			    (check-callback-event cb cx e commands #f))
			  "On"))
  (define t (make-object mred:button% p
			 (lambda (t e)
			   (let ([on? (send cb get-value)])
			     (send cb set-value (not on?))))
			 "Toggle"))
  (define c (make-object mred:button% p
			 (lambda (c e)
			   (for-each
			    (lambda (e)
			      (check-callback-event cb cb e commands #t))
			    old-list)
			   (printf "All Ok~n"))
			 "Check"))
  (send f show #t))

(define (choice-or-list-frame list? list-style empty?)
  (define f (make-object mred:frame% null "Choice Test"))
  (define p (make-object mred:vertical-panel% f))
  (define-values (actual-content actual-user-data)
    (if empty?
	(values null null)
	(values '("Alpha" "Beta" "Gamma")
		(list null null null))))
  (define commands 
    (if list?
	(list wx:const-event-type-listbox-command)
	(list wx:const-event-type-choice-command)))
  (define old-list null)
  (define callback
    (lambda (cx e)
      (when (zero? (send c number))
	    (error "Callback for empty choice/list"))
      (set! old-list (cons (list e
				 (send e get-command-int)
				 (send e get-command-string))
			   old-list))
      (unless (= (send e get-command-int)
		 (send c get-selection))
	      (error "event selection value mismatch"))
      (unless (string=? (send e get-command-string)
			(send c get-string-selection)
			(send c get-string (send c get-selection)))
	      (error "selection string mistmatch"))
      (check-callback-event c cx e commands #f)))
  (define c (if list?
		(make-object mred:list-box% p
			 callback
			 "Tester"
			 list-style
			 -1 -1 -1 -1
			 actual-content)
		(make-object mred:choice% p
			     callback
			     "Tester"
			     -1 -1 -1 -1
			     actual-content)))
  (define counter 0)
  (define append-with-user-data? #f)
  (define ab (make-object mred:button% p
			  (lambda (b e)
			    (set! counter (add1 counter))
			    (let ([naya (format "Extra ~a" counter)]
				  [naya-data (box 0)])
			      (set! actual-content (append actual-content (list naya)))
			      (set! actual-user-data (append actual-user-data (list naya-data)))
			      (if (and list? append-with-user-data?)
				  (send c append naya naya-data)
				  (begin
				    (send c append naya)
				    (when list?
					  (send c set-client-data 
						(sub1 (send c number))
						naya-data))))
			      (set! append-with-user-data?
				    (not append-with-user-data?))))
			  "Append"))
  (define cdp (make-object mred:horizontal-panel% p))
  (define rb (make-object mred:button% cdp
			  (lambda (b e)
			    (set! actual-content null)
			    (set! actual-user-data null)
			    (send c clear))
			  "Clear"))
  (define db (if list?
		 (make-object mred:button% cdp
			      (lambda (b e)
				(let ([p (send c get-selection)])
				  (when (<= 0 p (sub1 (length actual-content)))
					(send c delete p)
					(if (zero? p)
					    (begin
					      (set! actual-content (cdr actual-content))
					      (set! actual-user-data (cdr actual-user-data)))
					    (begin
					      (set-cdr! (list-tail actual-content (sub1 p)) 
							(list-tail actual-content (add1 p)))
					      (set-cdr! (list-tail actual-user-data (sub1 p)) 
							(list-tail actual-user-data (add1 p))))))))
			      "Delete")
		 null))
  (define (make-selectors method numerical?)
    (define p2 (make-object mred:horizontal-panel% p))
    (when numerical?
	  (make-object mred:button% p2
		       (lambda (b e)
			 (method -1))
		       "Select Bad -1"))
    (make-object mred:button% p2
		 (lambda (b e)
		   (method 0))
		 "Select First")
    (make-object mred:button% p2
		 (lambda (b e)
		   (method (floor (/ (send c number) 2))))
		 "Select Middle")
    (make-object mred:button% p2
		 (lambda (b e)
		   (method (sub1 (send c number))))
		 "Select Last")
    (make-object mred:button% p2
		 (lambda (b e)
		   (method (if numerical?
			       (send c number)
			       #f)))
		 "Select Bad X")
    #f)
  (define dummy-1 (make-selectors (ivar c set-selection) #t))
  (define dummy-2 (make-selectors (lambda (p) 
				    (if p
					(when (positive? (length actual-content))
					      (send c set-string-selection 
						    (list-ref actual-content p)))
					(send c set-string-selection "nada")))
				  #f))
  (define tb (make-object mred:button% p
			  (lambda (b e)
			    (let ([c (send c number)])
			      (unless (= c (length actual-content))
				      (error "bad number response")))
			    (let loop ([n 0][l actual-content][lud actual-user-data])
			      (unless (null? l)
				      (let ([s (car l)]
					    [sud (car lud)]
					    [sv (send c get-string n)]
					    [sudv (if list?
						      (send c get-client-data n)
						      #f)])
					(unless (string=? s sv)
						(error "get-string mismatch"))
					(unless (or (not list?) (eq? sud sudv))
						(error "get-user-data mismatch"))
					(unless (= n (send c find-string s))
						(error "bad find-string result")))
				      (loop (add1 n) (cdr l) (cdr lud))))
			    (unless (and (null? (send c get-string -1))
					 (null? (send c get-string (send c number))))
				    (error "out-of-bounds did not return null"))
			    (unless (= -1 (send c find-string "nada"))
				    (error "bad find-string result for nada"))
			    (for-each
			     (lambda (eis)
			       (let ([e (car eis)]
				     [i (cadr eis)]
				     [s (caddr eis)])
				 (unless (= (send e get-command-int) i)
					 (error "event selection value mismatch"))
				 (unless (string=? (send e get-command-string) s)
					 (error "selection string mistmatch"))
				 (check-callback-event c c e commands #t)))
			     old-list)
			    (printf "content: ~s~n" actual-content))
			  "Check"))
  (send f show #t))

(define (gauge-frame)
  (define f (make-object mred:frame% null "Gauge Test"))
  (define p (make-object mred:vertical-panel% f))
  (define g (make-object mred:gauge% p "Tester" 10))
  (define (move d name)
    (make-object mred:button% p
		 (lambda (c e)
		   (send g set-value (+ d (send g get-value))))
		 name))
  (define (size d name)
    (make-object mred:button% p
		 (lambda (c e)
		   (send g set-range (+ d (send g get-range))))
		 name))
  (move 1 "+")
  (move -1 "-")
  (size 1 "Bigger")
  (size -1 "Smaller")
  (send f show #t))

(define (text-frame mred:text% style)
  (define (handler get-this)
    (lambda (c e)
      (unless (eq? c (get-this))
	      (printf "callback: bad item: ~a~n" c))
      (unless (eq? c (send e get-event-object))
	      (printf "callback: bad item in event: ~a~n" (send e get-event-object)))
      (let ([t (send e get-event-type)])
	(cond
	 [(= t wx:const-event-type-text-command)
	  (printf "Changed: ~a~n" (send e get-command-string))]
	 [(= t wx:const-event-type-text-enter-command)
	  (printf "Return: ~a~n" (send e get-command-string))]
	 [(= t wx:const-event-type-set-focus)
	  (printf "Focus in~n")]
	 [(= t wx:const-event-type-kill-focus)
	  (printf "Focus out~n")]))))

  (define f (make-object mred:frame% null "Text Test"))
  (define p (make-object (class-asi mred:vertical-panel%
			    (public
			     [on-default-action
			      (lambda (v)
				(printf "Panel default action~n"))]))
			 f))
  (define t1 (make-object mred:text% p (handler (lambda () t1)) null "This should just fit!"
			  -1 -1 -1 -1 style))
  (define t2 (make-object mred:text% p (handler (lambda () t2)) "Another" "This too!"
			  -1 -1 -1 -1 style))
  (define junk (send p set-label-position wx:const-vertical))
  (define t3 (make-object mred:text% p (handler (lambda () t3)) "Catch Returns" "And, yes, this!"
			  -1 -1 -1 -1 (+ style wx:const-process-enter)))
  (send t1 stretchable-in-x #f)
  (send t2 stretchable-in-x #f)
  (send t3 stretchable-in-x #f)
  (send f show #t))

(define (canvas-frame flags)
  (define f (make-object mred:frame% null "Canvas Test"))
  (define p (make-object mred:vertical-panel% f))
  (define c% (class mred:canvas% (name p)
		    (inherit clear draw-text draw-line set-clipping-region
			     get-scroll-pos get-scroll-range get-scroll-page
			     get-client-size get-virtual-size)
		    (public
		     [on-paint
		      (lambda ()
			(let ([s (format "V: p: ~s r: ~s g: ~s H: ~s ~s ~s"
					 (get-scroll-pos wx:const-vertical)
					 (get-scroll-range wx:const-vertical)
					 (get-scroll-page wx:const-vertical)
					 (get-scroll-pos wx:const-horizontal)
					 (get-scroll-range wx:const-horizontal)
					 (get-scroll-page wx:const-horizontal))]
			      [w (box 0)][w2 (box 0)]
			      [h (box 0)][h2 (box 0)])
			  (get-client-size w h)
			  (get-virtual-size w2 h2)
			  ; (set-clipping-region 0 0 (unbox w2) (unbox h2))
			  (clear)
			  (draw-text name 3 3)
			  ; (draw-line 3 12 40 12)
			  (draw-text s 3 15)
			  (draw-text (format "client: ~s x ~s  virtual: ~s x ~s" 
					     (unbox w) (unbox h)
					     (unbox w2) (unbox h2))
				     3 27)))]
		     [on-scroll
		      (lambda (e) (on-paint))])
		    (sequence
		      (super-init p -1 -1 -1 -1 flags))))
  (define c1 (make-object c% "Unmanaged scroll" p))
  (define c2 (make-object c% "Automanaged scroll" p))
  (define (reset-scrolls)
    (let* ([h? (send ck-h get-value)]
	   [v? (send ck-v get-value)]
	   [small? (send ck-s get-value)]
	   [swap? (send ck-w get-value)])
      (send c1 set-scrollbars (if h? 1 -1) (if v? 1 -1) 10 10 3 3 0 0 swap?)
      (send c2 set-scrollbars (if h? 25 -1) (if v? 10 -1) (if small? 2 20) (if small? 2 20) 
	    3 3 0 0 (not swap?))))
  (define p2 (make-object mred:horizontal-panel% p))
  (define jumk (send p2 stretchable-in-y #f))
  (define ck-v (make-object mred:check-box% p2 (lambda (b e) (reset-scrolls)) "Vertical Scroll"))
  (define ck-h (make-object mred:check-box% p2 (lambda (b e) (reset-scrolls)) "Horizontal Scroll"))
  (define ck-s (make-object mred:check-box% p2 (lambda (b e) (reset-scrolls)) "Small"))
  (define ck-w (make-object mred:check-box% p2 (lambda (b e) (reset-scrolls)) "Swap"))
  (send f show #t))

;----------------------------------------------------------------------

(define selector (make-object mred:frame% null "Test Selector"))
(define ap (make-object mred:vertical-panel% selector))

; Test timers while we're at it:
(let ([clockp (make-object mred:horizontal-panel% ap)]
      [selector selector])
  (make-object mred:vertical-panel% clockp) ; filler
  (let ([time (make-object mred:message% clockp "XX:XX:XX")])
    (make-object
     (class wx:timer% ()
	    (inherit start)
	    (public
	     [notify
	      (lambda ()
		(let* ([now (seconds->date (current-seconds))]
		       [pad (lambda (pc d)
			      (let ([s (number->string d)])
				(if (= 1 (string-length s))
				    (string-append pc s)
				    s)))]
		       [s (format "~a:~a:~a"
				  (pad " " (let ([h (modulo (date-hour now) 12)])
					     (if (zero? h)
						 12
						 h)))
				  (pad "0" (date-minute now))
				  (pad "0" (date-second now)))])
		  (send time set-label s)
		  (when (send selector is-shown?)
			(start 1000 #t))))])
	    (sequence
	      (super-init)
	      (start 1000 #t))))))

(define bp (make-object mred:vertical-panel% ap -1 -1 -1 -1 wx:const-border))
(define bp1 (make-object mred:horizontal-panel% bp))
(define bp2 (make-object mred:horizontal-panel% bp))
(define mp (make-object mred:vertical-panel% ap -1 -1 -1 -1 wx:const-border))
(define mp1 (make-object mred:horizontal-panel% mp))
(define mp2 (make-object mred:horizontal-panel% mp))

(send bp1 set-label-position wx:const-vertical)
(send mp1 set-label-position wx:const-vertical)

(make-object mred:button% ap (lambda (b e) (menu-frame)) "Make Menus Frame")
(make-object mred:button% ap (lambda (b e) (button-frame)) "Make Button Frame")
(make-object mred:button% ap (lambda (b e) (checkbox-frame)) "Make Checkbox Frame")
(define cp (make-object mred:horizontal-panel% ap))
(send cp stretchable-in-x #f)
(make-object mred:button% cp (lambda (b e) (choice-or-list-frame #f 0 #f)) "Make Choice Frame")
(make-object mred:button% cp (lambda (b e) (choice-or-list-frame #f 0 #t)) "Make Empty Choice Frame")
(define lp (make-object mred:horizontal-panel% ap))
(send lp stretchable-in-x #f)
(make-object mred:button% lp (lambda (b e) (choice-or-list-frame #t wx:const-single #f)) "Make List Frame")
(make-object mred:button% lp (lambda (b e) (choice-or-list-frame #t wx:const-single #t)) "Make Empty List Frame")
(make-object mred:button% lp (lambda (b e) (choice-or-list-frame #t wx:const-multiple #f)) "Make Multilist Frame")
(make-object mred:button% ap (lambda (b e) (gauge-frame)) "Make Gauge Frame")
(define tp (make-object mred:horizontal-panel% ap))
(send tp stretchable-in-x #f)
(make-object mred:button% tp (lambda (b e) (text-frame mred:text% 0)) "Make Text Frame")
(make-object mred:button% tp (lambda (b e) (text-frame mred:media-text% 0)) "Make Media Text Frame")
(make-object mred:button% tp (lambda (b e) (text-frame mred:multi-text% 0)) "Make Multitext Frame")
(make-object mred:button% tp (lambda (b e) (text-frame mred:media-multi-text% 0)) "Make Media Multitext Frame")
(define tp2 (make-object mred:horizontal-panel% ap))
(send tp2 stretchable-in-x #f)
(make-object mred:button% tp2 (lambda (b e) (text-frame mred:multi-text% wx:const-hscroll)) "Make Multitext Frame/HScroll")
(make-object mred:button% tp2 (lambda (b e) (text-frame mred:media-multi-text% wx:const-hscroll)) "Make Media Multitext Frame/HScroll")

(define cnp (make-object mred:horizontal-panel% ap))
(send cnp stretchable-in-x #f)
(let ([mkf (lambda (flags name)
	     (make-object mred:button% cnp 
			  (lambda (b e) (canvas-frame flags))
			  (format "Make ~aCanvas Frame" name)))])
  (mkf (+ wx:const-hscroll wx:const-vscroll) "HV")
  (mkf wx:const-hscroll "H")
  (mkf wx:const-vscroll "V")
  (mkf 0 ""))

(define (choose-next radios)
  (let loop ([l radios])
    (let* ([c (car l)]
	   [rest (cdr l)]
	   [n (send c number)]
	   [v (send c get-selection)])
      (if (< v (sub1 n))
	  (send c set-selection (add1 v))
	  (if (null? rest)
	      (map (lambda (c) (send c set-selection 0)) radios)
	      (begin
		(send c set-selection 0)
		(loop rest)))))))

(define make-next-button
  (lambda (p l)
    (make-object mred:button% p
		 (lambda (b e) (choose-next l))
		 "Next Configuration")))

(define make-selector-and-runner
  (lambda (p1 p2 radios? size maker)
    (define radio-h-radio
      (if radios?
	  (make-object mred:radio-box% p1 void "Radio Box Orientation"
		       -1 -1 -1 -1
		       '("Vertical" "Horizontal"))
	  #f))
    (define label-h-radio
      (make-object mred:radio-box% p1 void "Label Orientation"
		   -1 -1 -1 -1
		   '("Vertical" "Horizontal")))
    (define label-null-radio
      (make-object mred:radio-box% p1 void "Optional Labels"
		   -1 -1 -1 -1
		   '("Use Label" "No Label")))
    (define stretchy-radio
      (make-object mred:radio-box% p1 void "Stretchiness"
		   -1 -1 -1 -1
		   '("Normal" "All Stretchy")))
    (define font-radio
      (make-object mred:radio-box% p1 void "Font"
		   -1 -1 -1 -1
		   '("Normal" "Big")))
    (define next-button
      (let ([basic-set (list label-h-radio label-null-radio stretchy-radio font-radio)])
	(make-next-button p2 
			  (if radios?
			      (cons radio-h-radio basic-set)
			      basic-set))))
    (define go-button
      (make-object mred:button% p2
		   (lambda (b e)
		     (maker
		      (if radios?
			  (positive? (send radio-h-radio get-selection))
			  #f)
		      (positive? (send label-h-radio get-selection))
		      (positive? (send label-null-radio get-selection))
		      (positive? (send stretchy-radio get-selection))
		      (positive? (send font-radio get-selection))))
		   (format "Make ~a Frame" size)))
    #t))

(make-selector-and-runner bp1 bp2 #t "Big" big-frame)
(make-selector-and-runner mp1 mp2 #f "Medium" med-frame)

(send selector show #t)

; (define e (make-object wx:key-event% wx:const-event-type-char))
; (send e set-key-code 65)
; (send e set-shift-down #t)
