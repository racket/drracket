;; load this file as a tool to run the test suites

(unit/sig ()
  (import [mred : mred^]
	  [core : mzlib:core^]
	  [fw : framework^]
	  [pc : mzlib:print-convert^]
	  (drscheme : drscheme:export^)
	  [zodiac : zodiac:system^])
  
  (define test-thread
    (let ([kill-old void])
      (lambda (test thunk)
	(kill-old)
	(let ([thread-desc (thread
			    (lambda ()
			      (printf "t>> ~a started~n" test)
			      (thunk)
			      (printf "t>> ~a finished~n" test)))])
	  (set! kill-old
		(lambda ()
		  (when (thread-running? thread-desc)
		    (kill-thread thread-desc)
		    (printf "t>> killed ~a~n" test))))))))
  
  (define all-tests (map symbol->string (require-library "README" "tests" "drscheme")))

  (define (make-repl)
    (test-thread
     "REPL"
     (lambda ()
       (let ([startup "~/.mzschemerc"])
	 (when (file-exists? startup)
	   (load startup)))
       (read-eval-print-loop))))

  (define (run-test-suite filename)
    (test-thread
     filename
     (lambda ()
       (invoke-unit/sig
	(compound-unit/sig (import [fw : framework^]
				   [mred : mred^])
	  (link
	   [utils : test-utils:gui^ ((require-library "guir.ss" "tests" "utils") mred)]
	   [drs-utils : drscheme:test-util^ ((require-library "drscheme-test-util.ss" "tests" "drscheme") mred fw utils)]
	   [main : ()
		 ((unit/sig ()
		    (import [drs-utils : drscheme:test-util^]
			    [utils : test-utils:gui^])
		    
		    (invoke-unit/sig
		     (eval
		      `(unit/sig ()
			 (import [fw : framework^]
				 mzlib:function^
				 mzlib:file^
				 drscheme:test-util^
				 test-utils:gui^
				 mred^
				 [drscheme : drscheme:export^]
				 [zodiac : zodiac:system^])
			 
			 (include ,(build-path (collection-path "tests" "drscheme") filename))))
		     (fw : framework^)
		     (core:function : mzlib:function^)
		     (core:file : mzlib:file^)
		     (drs-utils : drscheme:test-util^)
		     (utils : test-utils:gui^)
		     (mred : mred^)
		     (drscheme : drscheme:export^)
		     (zodiac : zodiac:system^)))

		  drs-utils utils)])
	  (export))
	(fw : framework^)
	(mred : mred^)))))

  (fw:preferences:set-default 'drscheme:test-suite:file-name "repl-tests.ss" string?)
  (fw:preferences:set-default 'drscheme:test-suite:run-interval 10 number?)

  (fw:preferences:set-default 'drscheme:test-suite:frame-width #f (lambda (x) (or (not x) (number? x))))
  (fw:preferences:set-default 'drscheme:test-suite:frame-height 300 (lambda (x) (or (not x) (number? x))))

  (define current-test-suite-frame #f)

  (define (ask-test-suite)
    (if current-test-suite-frame
	(send current-test-suite-frame show #t)
	(let* ([frame% (class mred:frame% ()
			 (override
			  [on-size
			   (lambda (w h)
			     (fw:preferences:set 'drscheme:test-suite:frame-width w)
			     (fw:preferences:set 'drscheme:test-suite:frame-height h))]
			  [on-close
			   (lambda ()
			     (set! current-test-suite-frame #f))])
			 (sequence
			   (super-init "Test Suites"
				       #f
				       (fw:preferences:get 'drscheme:test-suite:frame-width)
				       (fw:preferences:get 'drscheme:test-suite:frame-height))))]
	       [drscheme-test-dir (collection-path "tests" "drscheme")]
	       [frame (make-object frame%)]
	       [panel (make-object mred:vertical-panel% frame)]
	       [top-panel (make-object mred:vertical-panel% panel)]
	       [bottom-panel (make-object mred:horizontal-panel% panel)])
	  (send top-panel stretchable-height #f)
	  (send (make-object mred:button%
		  "REPL" 
		  bottom-panel
		  (lambda (_1 _2)
		    (send frame show #f)
		    (make-repl)))
		focus)
	  
	  (when drscheme-test-dir
	    (send top-panel stretchable-height #t)
	    (send bottom-panel stretchable-height #f)
	    (letrec ([lb (make-object mred:list-box%
			   #f
			   all-tests
			   top-panel
			   (lambda (b e)
			     (when (eq? (send e get-event-type) 'list-box-dclick)
			       (run-test-suite-callback))))]
		     [run-test-suite-callback
		      (lambda ()
			(let ([selection (send lb get-selection)])
			  (when selection
			    (send frame show #f)
			    (let ([test (list-ref all-tests selection)])
			      (fw:preferences:set
			       'drscheme:test-suite:file-name
			       test)
			      (run-test-suite
			       test)))))])

	      ;; set values from preferences
	      (let* ([test-suite (fw:preferences:get 'drscheme:test-suite:file-name)]
		     [num (send lb find-string test-suite)])
		(when num
		  (send lb set-string-selection test-suite)
		  (send lb set-first-visible-item num)
		  (fw:test:run-interval (fw:preferences:get 'drscheme:test-suite:run-interval))))

	      (send
	       (make-object mred:button%
		 "Run Test Suite"
		 bottom-panel
		 (lambda (_1 _2)
		   (run-test-suite-callback)))
	       focus))

	    (let* ([pre-times (list 0 10 50 100 500)]
		   [times (if (member (fw:test:run-interval) pre-times)
			      pre-times
			      (append pre-times (list (fw:test:run-interval))))]
		   [choice
		    (make-object mred:choice%
		      "Run Interval"
		      (map number->string times)
		      top-panel
		      (lambda (choice event)
			(let ([time (list-ref times (send choice get-selection))])
			  (fw:preferences:set 'drscheme:test-suite:run-interval time)
			  (fw:test:run-interval time))))])
	      (send choice set-selection
		    (let loop ([l times]
			       [n 0])
		      (if (= (car l) (fw:test:run-interval))
			  n
			  (loop (cdr l)
				(+ n 1)))))))
	  (make-object mred:button%
	    "Cancel" 
	    bottom-panel
	    (lambda (_1 _2)
	      (send frame show #f)))
	  (send frame show #t)
	  (set! current-test-suite-frame frame))))

  (drscheme:get/extend:extend-unit-frame
   (lambda (super%)
     (class super% args
       (inherit button-panel)
       (sequence (apply super-init args))
       (private
	 [bitmap (make-object mred:bitmap% 
		   (if (<= (mred:get-display-depth) 1)
		       (build-path (collection-path "icons") "bb-sm-bw.bmp")
		       (build-path (collection-path "icons") "bb-small.bmp"))
		   'bmp)]
	 [button (make-object
		  mred:button%
		  (if (send bitmap ok?)
		      bitmap
		      "Console")
		  button-panel
		  (lambda (button evt)
		    (ask-test-suite)))])
       (sequence
	 (send button-panel change-children
	       (lambda (l)
		 (cons button (core:function:remq button l)))))))))
