(module browser mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "file.ss")
           (prefix raw: (lib "sendurl.ss" "net"))
           (lib "unitsig.ss")
           (lib "tool.ss" "drscheme")
           (prefix fw: (lib "framework.ss" "framework")))
  (provide send-url
	   send-help-desk-url
           (rename raw:browser-preference? browser-preference?)
           update-browser-preference
	   set-plt-browser!
	   install-help-browser-preference-panel
           tool@)
  
  ; : -> bool
  (define (unix-browser?)
    (and (eq? (system-type) 'unix)
	 (not (equal? "ppc-macosxonx" (system-library-subpath)))))
  
  (fw:preferences:set-default
   'external-browser
   (get-preference 'external-browser (lambda () #f))
   raw:browser-preference?)
  
  ; : tst -> bool
  (define (help-browser-preference? x)
    (or (eq? x 'plt) (eq? x 'external)))

  (define help-browser-preference 'plt:help-browser)
  
  (fw:preferences:set-default
   help-browser-preference
   (get-preference help-browser-preference (lambda () 'external))
   help-browser-preference?)
  
  ; : str [bool] -> void
  (define send-help-desk-url
    (lambda (mk-browser url . args)
      (when (or (not (unix-browser?))
		(get-preference 'external-browser (lambda () #f))
		(eq? 'plt (get-preference help-browser-preference (lambda () #f)))
		;; either the preference doesn't exist or is #f
		(update-browser-preference url))
	(parameterize ([raw:external-browser mk-browser])
	  (apply raw:send-url url args)))))

  (define send-url
    (if (unix-browser?)
        (lambda (url . args)
          (when (or (get-preference 'external-browser (lambda () #f))
		    ;; either the preference doesn't exist or is #f
		    (update-browser-preference url))
	    (apply raw:send-url url args)))
        raw:send-url))
  
  ; : str -> void
  ; to prompt the user for a browser preference and update the preference
  (define (update-browser-preference url)
    (or (not (unix-browser?))
	(choose-browser url)))
  
  ; : (U symbol #f) -> void
  ; to set the default browser
  (define (set-browser! browser)
    (fw:preferences:set 'external-browser browser))
  
  (define (set-plt-browser!)
    (put-preferences (list help-browser-preference) '(plt))
    (fw:preferences:set help-browser-preference 'plt))

  (define unix-browser-names
    (map (lambda (s)
	   (let ([l (string->list (symbol->string s))])
	     (list->string (cons (char-upcase (car l)) (cdr l)))))
	 raw:unix-browser-list))

  ;; : (U str #f) -> (U symbol #f)
  ;; to prompt the user for a browser preference
  ;;  #f for the URL indicates a pre-emptive request by Help Desk,
  ;;  and in that case, the user can choose to use the internal
  ;;  broswer.
  (define (choose-browser url)
    (let* ([title (string-constant choose-browser)]
           [d (make-object dialog% title)]
           [main-pane (make-object vertical-pane% d)]
	   [internal-ok? (not url)]
           [ok? #f]
	   [orig-external (fw:preferences:get 'external-browser)]
	   [orig-internal (fw:preferences:get help-browser-preference)])
      (make-object message% title main-pane)
      (when url
	(make-object message% (format "URL: ~a" url) main-pane))
      (let-values ([(panel callbacks) (make-help-browser-preference-panel internal-ok? #f (lambda (f) (f main-pane)))])
	(let*-values ([(button-pane) (instantiate horizontal-panel% (main-pane)
						  (alignment '(right center)))]
		      [(ok-button cancel-button)
		       (fw:gui-utils:ok/cancel-buttons
			button-pane
			(lambda (b e) (set! ok? #t) (send d show #f))
			(lambda (b e) 
			  (fw:preferences:set 'external-browser orig-external)
			  (when internal-ok?
			    (fw:preferences:set help-browser-preference orig-internal))
			  (send d show #f)))]
		      [(enable-button) (lambda (_n _v)
					 (queue-callback
					  (lambda ()
					    (send ok-button enable 
						  (or (fw:preferences:get 'external-browser)
						      (and internal-ok?
							   (eq? 'plt (fw:preferences:get help-browser-preference))))))))])
	  (send ok-button enable #f)
	  (set! callbacks
		(cons
		 (fw:preferences:add-callback 'external-browser enable-button)
		 callbacks))
	  (when internal-ok?
	    (set! callbacks
		(cons
		 (fw:preferences:add-callback help-browser-preference enable-button)
		 callbacks))))
	(send d show #t)
	(map (lambda (f) (f)) callbacks)
	ok?)))
  
  (define prefs-installed? #f)
  (define synchronized? #f)

  (define (install-help-browser-preference-panel)
    (unless prefs-installed?
      (set! prefs-installed? #t)
      (make-help-browser-preference-panel
       #t #t
       (lambda (f) (fw:preferences:add-panel
		    (string-constant browser)
		    (lambda (parent)
		      (let-values ([(panel cbs) (f parent)])
			panel)))))))
  
  (define (make-help-browser-preference-panel set-help? ask-later? mk)
    (unless synchronized?
      ;; Keep low-level pref in sync. Yes, this is clumsy and bad.
      ;; At start-up, let low-level preference dominate if it has
      ;; a value and is different, because Help Desk might have set it.
      (let ([low-level (get-preference help-browser-preference (lambda () #f))])
	(when (and low-level
		   (not (equal? low-level (fw:preferences:get help-browser-preference))))
	  (fw:preferences:set help-browser-preference low-level)))
      (fw:preferences:add-callback help-browser-preference
				   (lambda (name browser)
				     (put-preferences (list help-browser-preference) (list browser)))))
    
    (mk
     (lambda (parent)
       (define callbacks null)
       (let ([pref-panel (instantiate vertical-panel% ()
				       (parent parent) (alignment '(center center)))])
	 (when set-help?
	   (let* ([help-desk-radio
		   (instantiate radio-box% ()
				(label #f)
				(choices (list (string-constant use-internal-browser-for-help)
					       (string-constant use-external-browser-for-help)))
				(parent pref-panel)
				(callback
				 (lambda (radio event)
				   (fw:preferences:set help-browser-preference
						       (case (send radio get-selection)
							 [(0) 'plt]
							 [(1) 'external])))))]
		  [refresh-control (lambda (name val)
				     (send help-desk-radio set-selection
					   (if (eq? val 'plt) 0 1)))])
	     (set! callbacks
		   (cons
		    (fw:preferences:add-callback help-browser-preference refresh-control)
		    callbacks))
	     (refresh-control #f (fw:preferences:get help-browser-preference))))

	 (when (unix-browser?)
	   (unless synchronized?
	     ;; Keep 'external-browser in sync, too
	     (fw:preferences:add-callback 'external-browser
					  (lambda (name browser)
					    (put-preferences (list 'external-browser) (list browser)))))

	   (letrec ([v-panel (instantiate vertical-panel% ()
					  (parent pref-panel)
					  (alignment '(right center))
					  (stretchable-height #f)
					  (style '(border)))]
		    [h-panel (instantiate horizontal-panel% ()
					  (parent v-panel)
					  (alignment '(center bottom)))]
		    [none-index (length raw:unix-browser-list)]
		    [custom-index (add1 none-index)]
		    [r (instantiate radio-box% ()
				    (label (string-constant external-browser-choice-title))
				    (choices (append unix-browser-names
						     (list (string-constant no-browser)
							   (string-constant browser-command-line-label))))
				    (parent h-panel)
				    (callback
				     (lambda (radio event)
				       (let ([n (send radio get-selection)])
					 (set-browser!
					  (cond
					   [(= n none-index) #f]
					   [(= n custom-index) (get-custom)]
					   [else (list-ref raw:unix-browser-list n)]))))))]
		    [select-custom
		     (lambda (_ __)
		       (send r set-selection custom-index)
		       (set-browser! (get-custom)))]
		    [get-custom
		     (lambda () (cons (send pre get-value) (send post get-value)))]
		    [template-panel (instantiate horizontal-panel% (h-panel)
						 (spacing 0)
						 (stretchable-height #f))]
		    [pre (instantiate text-field% ()
				      (label #f) (parent template-panel) (callback select-custom)
				      (horiz-margin 0))]
		    [mess (instantiate message% () (label "<URL>") (parent template-panel)
				       (horiz-margin 0))]
		    [post (instantiate text-field% ()
				       (label #f) (parent template-panel) (callback select-custom)
				       (horiz-margin 0))]
		    [note1 (instantiate message% ((string-constant browser-cmdline-expl-line-1)
						  v-panel))]
		    [note2 (instantiate message% ((string-constant browser-cmdline-expl-line-2)
						  v-panel))]
		    [refresh-controls (lambda (pref)
					(if (pair? pref)
					    (begin
					      (send r set-selection custom-index)
					      (send pre set-value (car pref))
					      (send post set-value (cdr pref)))
					    (let init ([x raw:unix-browser-list] [n 0])
					      (cond
					       [(null? x) (send r set-selection n)]
					       [else (if (eq? pref (car x))
							 (send r set-selection n)
							 (init (cdr x) (add1 n)))]))))])

	     (unless ask-later?
	       (send r enable none-index #f))

	     (refresh-controls (fw:preferences:get 'external-browser))
	     (set! callbacks
		   (cons
		    (fw:preferences:add-callback 'external-browser
						 (lambda (name browser)
						   (refresh-controls browser)))
		    callbacks))

	     (let disable ([x raw:unix-browser-list] [n 0])
	       (cond
		[(null? x) (void)]
		[else (unless (find-executable-path (symbol->string (car x)) #f)
			(send r enable n #f))
		      (disable (cdr x) (add1 n))]))))

	 (set! synchronized? #t)
	 (values pref-panel callbacks)))))

  ;; to add a preference pannel to drscheme that sets the browser preference
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define phase1 void)
      (define phase2 void)
      
      (install-help-browser-preference-panel))))
