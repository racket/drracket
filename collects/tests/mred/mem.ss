
(define source-dir (current-load-relative-directory))

(define num-times 12)
(define num-threads 1)

(define dump-stats? #t)

(define edit? #t)
(define insert? #t)
(define load-file? #f) ; adds a lot of messy objects

(define menus? #t)
(define atomic? #t)
(define offscreen? #t)
(define frame? #t)

(define subwindows? #t)

(define allocated '())
(define (remember tag v)
  (set! allocated
	(cons (cons tag (make-weak-box v))
	      allocated))
  v)

(define frame%
  ; Leave this as the (obsolete) make-class form for macro testing
  (make-class mred:editor-frame%
	      (rename [super-show show])
	      (public
	       [prim-show (lambda (arg) (super-show arg))]
	       [show
		(lambda (x) (void))])))

(when subwindows?
      (define sub-collect-frame
	(make-object wx:frame% null "sub-collect" -1 -1 200 200))
      (define sub-collect-panel
	(make-object wx:panel% sub-collect-frame 0 0 100 100)))

(send sub-collect-frame show #t)

(define (maker id n)
  (sleep)
  (collect-garbage)
  (collect-garbage)
  (printf "Thread: ~s Cycle: ~s~n" id n)
  (dump-object-stats)
  (if (and dump-stats? (= id 1))
      (dump-memory-stats))
  (unless (zero? n)
    (let ([tag (cons id n)])
      (let* ([f (if edit? (remember tag (make-object frame%)))]
	     [c (make-custodian)]
	     [es (parameterize ([current-custodian c])
			       (wx:make-eventspace))])

	(parameterize ([wx:current-eventspace es])
		      (send (remember 
			     tag
			     (make-object
			      (class-asi wx:timer%
					 (public
					  [notify void]))))
			    start 100))

	(when edit?
	    (remember tag (send f get-edit)))

	(when (and edit? (zero? (modulo n 2)))
	    (send f prim-show #t)
		(sleep 0.5))

	(if frame?
	    (let* ([f (make-object wx:frame% '() "Tester" -1 -1 200 200)]
		   [p (remember tag (make-object wx:panel% f))])
	      (remember tag (make-object wx:canvas% f))
	      (if (zero? (modulo n 3))
		  (send f show #t))
	      (remember tag (make-object wx:button% p (lambda args #t) "one"))
	      (let ([class wx:check-box%])
		(let loop ([m 10])
		  (unless (zero? m)
			  (remember (cons tag m)
				    (make-object class p (lambda args #t) "another"))
			  (loop (sub1 m)))))
	      (send p new-line)
	      (remember tag (make-object wx:check-box% p (lambda args #t) "check"))
	      (remember tag (make-object wx:choice% p (lambda args #t) "choice"))
	      (remember tag (make-object wx:list-box% p (lambda args #t) "list"
					 wx:const-single -1 -1 -1 -1
					 '("apple" "banana" "coconut")))
	      (remember tag (make-object wx:button% p (lambda args #t) "two"))
	      (send f show #f)))

	(if subwindows?
	    (let ([p (make-object wx:panel% sub-collect-frame 100 100 50 50)]
		  [cv (make-object wx:canvas% sub-collect-frame 150 150 50 50)]
		  [add-objects
		   (lambda (p tag hide?)
		     (let ([b (make-object wx:button% p (lambda args #t) "one" 0 0)]
			   [c (make-object wx:check-box% p (lambda args #t) "check" 0 0)]
			   [co (make-object wx:choice% p (lambda args #t) "choice" 0 0)]
			   [cv (make-object wx:canvas% p 0 0 50 50)]
			   [lb (make-object wx:list-box% p (lambda args #t) "list"
					    wx:const-single 0 0 -1 -1
					    '("apple" "banana" "coconut"))])
		       (when hide?
			     (send b show #f)
			     (send c show #f)
			     (send cv show #f)
			     (send co show #f)
			     (send lb show #f))
		       (remember tag b)
		       (remember tag c)
		       (remember tag cv)
		       (remember tag co)
		       (remember tag lb)))])
	      (add-objects sub-collect-panel (cons 'sc1 tag) #t)
	      (add-objects p (cons 'sc2 tag) #f)
	      (remember (cons 'sc0 tag) p)
	      (remember (cons 'sc0 tag) cv)
	      (send p show #f)
	      (send cv show #f)))
	      

	(if (and edit? insert?)
	    (let ([e (send f get-edit)])
	      (when load-file?
		    (send e load-file (build-path source-dir "mem.ss")))
	      (let loop ([i 20])
		(send e insert (number->string i))
		(unless (zero? i)
			(loop (sub1 i))))
	      (let ([s (make-object wx:media-snip%)])
		(send (send s get-this-media) insert "Hello!")
		(send e insert s))
	      (send e insert #\newline)
	      (send e insert "done")
	      (send e set-modified #f)))
	
	(when menus?
	      (remember tag (make-object wx:menu-bar%))
	      (remember tag (make-object wx:menu%))
	      (let ([mb (remember tag (make-object wx:menu-bar%))]
		    [m (remember tag (make-object wx:menu%))])
		(send m append 5 "Hi" (remember tag (make-object wx:menu%)))
		(send mb append m "x"))
	      
	      (if edit?
		  (let ([m (remember tag (make-object mred:menu%))]
			[m2 (remember tag (make-object mred:menu%))]
			[mb (send f get-menu-bar)])
		    (send m append 4 "ok")
		    (send m2 append 4 "hao")
		    (send m append 5 "Hi" (remember tag (make-object mred:menu%)))
		    (send mb append m "Extra")
		    (send mb append m2 "Other")
		    (send m delete 5)
		    (send mb delete m))))

	(when atomic?
	      (let loop ([m 8])
		(unless (zero? m)
			(remember (cons tag m) (make-object wx:point% n m))
			(remember (cons tag m) (make-object wx:int-point% n m))
			(remember (cons tag m) (make-object wx:brush%))
			(remember (cons tag m) (make-object wx:pen%))
			(loop (sub1 m)))))
	
	(when offscreen?
	      (let ([m (remember tag (make-object wx:memory-dc%))]
		    [b (remember (cons tag 'u) (make-object wx:bitmap% 100 100))]
		    [b2 (remember (cons tag 'x) (make-object wx:bitmap% 100 100))])
		(send m select-object b)))
	
	
	(when edit?
	      (let ([name (wx:get-temp-file-name "hi")])
		(send (send f get-edit) save-file name)
		(send f on-close) 
		(send f prim-show #f)
		(delete-file name)))

	(custodian-shutdown-all c)

	(collect-garbage)

	(maker id (sub1 n))))))
    
(define (still)
  (map (lambda (x)
	 (let ([v (weak-box-value (cdr x))])
	   (if v
	       (printf "~s ~s~n" (send v get-class-name) (car x)))))
       allocated)
  (void))

(define (xthread f)
  (f))

(define (stw t n)
  (thread-weight t (floor (/ (thread-weight t) n))))

(define (do-test)
  (let ([sema (make-semaphore)])
  	 (let loop ([n num-threads])
	   (unless (zero? n)
		 (thread (lambda () 
			   (stw (current-thread) n)
			   (dynamic-wind
			    void
			    (lambda () (maker n num-times))
			    (lambda () (semaphore-post sema)))))
		 (loop (sub1 n))))
	(let loop ([n num-threads])
	  (unless (zero? n)
	    (wx:yield sema)
		(loop (sub1 n)))))
  
  (collect-garbage)
  (collect-garbage)
  (let loop ([n 100]) 
    (if (zero? n) 0 (sub1 (loop (sub1 n)))))
  (collect-garbage)
  (collect-garbage)
  (still)
  (when subwindows?
	(set! sub-collect-frame #f)
	(set! sub-collect-panel #f))
  (when dump-stats?
	(dump-memory-stats)
	(still)))

(define mred:startup
  (let ([old-mred:startup mred:startup])
    (lambda args
      (send mred:the-frame-group set-empty-callback (lambda () #t))
      (do-test)
      (apply old-mred:startup args))))
