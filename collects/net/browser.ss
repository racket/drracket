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
           update-browser-preference
           tool@)
  
  ; : str [bool] -> void
  (define send-url
    (if (eq? (system-type) 'unix)
        (lambda (url . args)
          (unless (get-preference 'external-browser (lambda () #f))
            ; either the preference doesn't exist or is #f
            (update-browser-preference url))
          (apply raw:send-url url args))
        raw:send-url))
  
  ; : -> (U symbol #f)
  ; to prompt the user for a browser preference and update the preference
  (define (update-browser-preference url)
    (let ([browser (choose-browser url)])
      (set-browser! browser)))
  
  ; : (U symbol #f) -> void
  ; to set the default browser
  (define (set-browser! browser)
    (put-preferences '(external-browser) (list browser)))
  
  ; : str -> (U symbol #f)
  ; to prompt the user for a browser preference
  (define (choose-browser url)
    (let* ([title (string-constant choose-browser)]
           [d (make-object dialog% title)]
           [v (make-object vertical-pane% d)]
           [choice (box #f)])
      (make-object message% title v)
      (make-object message% (format "URL: ~a" url) v)
      (for-each (lambda (b)
                  (instantiate button% () (label (symbol->string b)) (parent v)
                    (callback (lambda (? ??)
                                ; ugh.  I want to return it, but there's no control context.
                                (set-box! choice b)
                                (send d show #f)))
                    (enabled (find-executable-path (symbol->string b) #f))))
                raw:unix-browser-list)
      (instantiate button% () (label (string-constant no-browser)) (parent v) (callback (lambda (? ??) (send d show #f))))
      (send d show #t)
      (unbox choice)))
  
  ; to add a preference pannel to drscheme that sets the browser preference
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define phase1 void)
      (define phase2 void)
      
      (when (eq? (system-type) 'unix)
	(let ([box-label (string-constant browser)])
          (fw:preferences:add-panel
           box-label
           (lambda (parent)
             (let* ([v (instantiate vertical-panel% () (parent parent) (alignment '(center center)))]
		    [r (instantiate radio-box% ()
			 (label box-label)
			 (choices (append (map symbol->string raw:unix-browser-list) (list (string-constant no-browser))))
			 (parent v)
			 (callback
			  (lambda (radio event)
			    ; This leaks. Each radio button should have its own callback.
			    (let ([n (send radio get-selection)])
			      (set-browser! (if (= n (length raw:unix-browser-list))
						#f
						(list-ref raw:unix-browser-list n)))))))])
               (let ([pref (get-preference 'external-browser (lambda () #f))])
                 (let init ([x raw:unix-browser-list] [n 0])
		   (cond
		     [(null? x) (send r set-selection n)]
		     [else (if (eq? pref (car x))
			       (send r set-selection n)
			       (init (cdr x) (add1 n)))])))
               (let disable ([x raw:unix-browser-list] [n 0])
                 (cond
                   [(null? x) (void)]
                   [else (unless (find-executable-path (symbol->string (car x)) #f)
                           (send r enable n #f))
                         (disable (cdr x) (add1 n))]))
               v))))))))
