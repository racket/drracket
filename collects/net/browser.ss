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
  
  ; : -> bool
  (define (unix-browser?)
    (and (eq? (system-type) 'unix) (not (equal? "ppc-macosxonx" (system-library-subpath)))))
  
  ; : str [bool] -> void
  (define send-url
    (if (unix-browser?)
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
           [main-pane (make-object vertical-pane% d)]
           [choice (box #f)])
      (make-object message% title main-pane)
      (make-object message% (format "URL: ~a" url) main-pane)
      (let ([pre-configured (make-object horizontal-pane% main-pane)])
        (for-each (lambda (b)
                    (instantiate button% () (label (symbol->string b)) (parent pre-configured)
                      (callback (lambda (? ??)
                                  ; ugh.  I want to return it, but there's no control context.
                                  (set-box! choice b)
                                  (send d show #f)))
                      (enabled (find-executable-path (symbol->string b) #f))))
                  raw:unix-browser-list)
        (instantiate button% ()
          (label (string-constant no-browser))
          (parent pre-configured)
          (callback (lambda (? ??) (send d show #f)))))
      (let ([custom (make-object horizontal-pane% main-pane)])
        ; This letrec relies on evaluation order.
        (letrec ([button (instantiate button% ()
                           (label (string-constant custom))
                           (parent custom)
                           (callback
                            (lambda (_ __)
                              (set-box! choice (cons (send pre get-value)
                                                     (send post get-value)))
                              (send d show #f))))]
                 [pre (instantiate text-field% () (label "") (parent custom) (callback void))]
                 [mess (instantiate message% () (label "URL") (parent custom))]
                 [post (instantiate text-field% () (label "") (parent custom) (callback void))])
          'empty-letrec-body))
      (send d show #t)
      (unbox choice)))
  
  ; to add a preference pannel to drscheme that sets the browser preference
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define phase1 void)
      (define phase2 void)
      
      (when (unix-browser?)
	(fw:preferences:add-panel
         (string-constant browser)
         (lambda (parent)
           (letrec ([v (instantiate vertical-panel% ()
                         (parent parent) (alignment '(center center)))]
                    [h-panel (instantiate horizontal-panel% ()
                               (parent v)
                               (alignment '(center bottom))
                               (stretchable-height #f))]
                    [none-index (length raw:unix-browser-list)]
                    [custom-index (add1 none-index)]
                    [r (instantiate radio-box% ()
                         (label "")
                         (choices (append (map symbol->string raw:unix-browser-list)
                                          (list (string-constant no-browser)
                                                (string-constant custom))))
                         (parent h-panel)
                         (callback
                          (lambda (radio event)
                            ; This leaks. Each radio button should have its own callback.
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
                    [pre (instantiate text-field% ()
                           (label "") (parent h-panel) (callback select-custom))]
                    [mess (instantiate message% () (label "URL") (parent h-panel))]
                    [post (instantiate text-field% ()
                            (label "") (parent h-panel) (callback select-custom))]
                    [pref (get-preference 'external-browser (lambda () #f))])
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
                           (init (cdr x) (add1 n)))])))
             (let disable ([x raw:unix-browser-list] [n 0])
               (cond
                 [(null? x) (void)]
                 [else (unless (find-executable-path (symbol->string (car x)) #f)
                         (send r enable n #f))
                       (disable (cdr x) (add1 n))]))
             v)))))))
