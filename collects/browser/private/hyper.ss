
(module hyper mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           "sig.ss"
           "../browser-sig.ss"
           (lib "file.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "etc.ss")
           (lib "url-sig.ss" "net")
           (lib "head.ss" "net")
           (lib "mred-sig.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants")
           (lib "plt-installer-sig.ss" "setup"))
  
  (provide hyper@)
  
  (define cs-break "Break")
  
  (define error-evalling-scheme-format
    (string-append
     "<html><head><title>Error Evaluating Scheme</title></head>"
     "<body>"
     "<h2>Error Evaluating Scheme Code</h2>"
     "<pre>\n~a\n</pre>"
     "<p><p>"
     "<font color=\"red\">~a</font>"
     "</body></html>"))
  
  (define hyper@
    (unit/sig hyper^
      (import html^
              mred^
              setup:plt-installer^
              net:url^)
      
      (define-struct (exn:file-saved-instead exn) (pathname))
      (define-struct (exn:cancelled exn) ())
      
      (define history-limit 20)
      
      (define-struct hyperlink (anchor-start 
                                anchor-end
                                url-string))
      
      (define-struct hypertag (name position))
      
      (define (same-page-url? a b)
	(or (eq? a b)
	    (and (url? a) (url? b)
		 ;; fragment can be different
		 (equal? (url-scheme a) (url-scheme b))
		 (equal? (url-host a) (url-host b))
		 (equal? (url-path a) (url-path b))
		 (equal? (url-params a) (url-params b))
		 (equal? (url-query a) (url-query b)))))
		 

      (define hyper-text<%>
        (interface ()
          ))
      
      (define hyper-text-mixin
        (lambda (super%)
          (class* super% (hyper-text<%>)
            
            (inherit begin-edit-sequence end-edit-sequence lock erase clear-undos
                     change-style
                     set-modified auto-wrap
                     find-snip get-snip-position set-clickback get-canvas
                     insert last-position hide-caret
                     get-end-position set-autowrap-bitmap)
            
            (init-field url top-level-window)
            (init progress 
                  [post-data #f])
            
            (field [post-string post-data]
                   [url-allows-evaling?
                    (cond
                      [(port? url) #f]
                      [(and (url? url)
                            (equal? "file" (url-scheme url)))
                       (with-handlers ([exn:i/o:filesystem? (lambda (x) #f)])
                         (path-below?
                          (normal-case-path (normalize-path (build-path (collection-path "mzlib") 
                                                                        'up
                                                                        'up)))
                          (normal-case-path (normalize-path (url-path url)))))]
                      [(and (url? url)
                            (equal? "http" (url-scheme url)))
                       (and (string=? (url-host url) "127.0.0.1")
                            (equal? 8000 (url-port url)))]
                      [else #f])])
            
            (rename [super-after-set-position after-set-position])
      
            (rename [super-get-keymaps get-keymaps])
            (define/override (get-keymaps)
              (cons hyper-keymap
                    (super-get-keymaps)))
	    (define/public (get-hyper-keymap)
	      hyper-keymap)
            
            [define/override after-set-position
              (lambda ()
                (unless (zero? (get-end-position))
                  (hide-caret #f))
                (super-after-set-position))]
            (field
             [doc-notes null]
             [title #f]
             [htmling? #f]
             [redirection #f]
             [hypertags-list (list (make-hypertag "top" 0))])
            
            ;; get-redirection : -> (union false? url?)
            ;; #f indicates no redirection, url is where it redirects to
            (define/public (get-redirection) redirection)
            
            [define/public add-document-note
              (lambda (note)
                (set! doc-notes (append doc-notes (list note))))]
            [define/public  get-document-notes
              (lambda () doc-notes)]
            
            [define/public make-link-style
              (lambda (start end)
                (change-style hyper-delta start end))]
            [define/public get-url (lambda () (and (url? url) url))]
            
            (define/public post-url
              (opt-lambda (url-string [post-data #f])
                (on-url-click
                 (lambda (url-string post-data)
                   (with-handlers ([(lambda (x) #t)
                                    (lambda (x)
                                      (unless (or (exn:break? x)
                                                  (exn:file-saved-instead? x)
                                                  (exn:cancelled? x))
                                        ((error-display-handler)
                                         (if (exn? x) (exn-message x) (format "~s" x))
                                         x)))])
                     (send (get-canvas) goto-url url-string (get-url) void post-data)))
                 url-string
                 post-data)))
            
            [define/public on-url-click (lambda (f x post-data) 
                                          (let ([c (get-canvas)])
                                            (if c
                                                (send c on-url-click f x post-data)
                                                (f x post-data))))]
            [define/public get-title (lambda () (or title (and (url? url) (url->string url))))]
            [define/public set-title (lambda (t) (set! title t))]
            (field [hyper-delta (make-object style-delta% 'change-underline #t)])
            (let ([mult (send hyper-delta get-foreground-mult)]
                  [add (send hyper-delta get-foreground-add)])
              (send mult set 0 0 0)
              (send add set 0 0 255))
            
            [define/public add-tag 
              (lambda (name pos)
                (for-each (lambda (tag) 
                            (when (string=? name (hypertag-name tag))
                              (remove-tag  name)))
                          hypertags-list)
                (let ([new-tag (make-hypertag name pos)])
                  (set! hypertags-list
                        (let insert-loop ([tags-left hypertags-list])
                          (cond [(null? tags-left)(cons new-tag ())]
                                [(> pos (hypertag-position (car tags-left)))
                                 (cons new-tag tags-left)]
                                [else (cons (car tags-left)
                                            (insert-loop (cdr tags-left)))])))))]
            [define/public find-tag
              (lambda (name)
                (if (and (integer? name) (positive? name))
                    name
                    (and (string? name)
                         (ormap (lambda (x) 
                                  (and (string=? name (hypertag-name x)) 
                                       (hypertag-position x)))
                                hypertags-list))))]
            [define/public remove-tag 
              (lambda (name)
                (set! hypertags-list
                      (filter (lambda (x) (not (string=? name (hypertag-name x))))
                              hypertags-list)))]
            [define/public add-link 
              (lambda (start end url-string)
                (let* ([new-link (make-hyperlink start end url-string)])
                  (set-clickback start end 
                                 (lambda (x y z)
                                   (post-url url-string)))))]
            
            ;; remember the directory when the callback is added (during parsing)
            ;; and restore it during the evaluation of the callback.
            [define/public add-scheme-callback
              (lambda (start end scheme-string)
                (let ([dir (current-load-relative-directory)])
                  (set-clickback 
                   start end 
                   (lambda (edit start end)
                     (if url-allows-evaling?
                         (parameterize ([current-load-relative-directory dir])
                           (eval-scheme-string scheme-string))
                         (message-box (string-constant help-desk)
                                      "<A MZSCHEME= ...> disabled"))))))]
            [define/public add-thunk-callback
              (lambda (start end thunk)
                (set-clickback 
                 start end 
                 (lambda (edit start end)
                   (thunk))))]
            
            [define/public eval-scheme-string
              (lambda (s)
                (let ([v 
                       (dynamic-wind
                        begin-busy-cursor
                        (lambda () 
                          (with-handlers ([not-break-exn?
                                           (lambda (exn)
                                             (format
                                              error-evalling-scheme-format
                                              s
                                              (if (exn? exn)
                                                  (exn-message exn)
                                                  (format "~s" exn))))])
                            (eval-string s)))
                        end-busy-cursor)])
                  (when (string? v)
                    (send (get-canvas) goto-url
                          (open-input-string v)
                          (get-url)))))]
            
            ;; thread-with-cancel-dialog
            ;;  Should be called the a handler thread.
            ;;  `work' will be called with a status proc
            ;;   in a different thread with breaks enabled.
            (define/private (in-thread-with-cancel work init-status)
              (let* ([progress-dlg #f]
                     [show-progress void]
                     [e-text init-status] ; in case show-progress isn't ready yet
                     [exn #f]
                     [result (void)]
                     [done? #f]
                     ; Semaphores to avoid race conditions:
                     [wait-to-continue (make-semaphore 0)]
                     [wait-to-break (make-semaphore 0)]
                     [wait-to-show (make-semaphore 1)]
                     [orig-eventspace (current-eventspace)]
                     [timeout-thread
                      (parameterize ([break-enabled #f])
                        (thread (lambda () 
                                  (error-display-handler void) 
                                  (break-enabled #t)
                                  (sleep 1))))]
                     [break-button 
                      (and (is-a? top-level-window hyper-frame<%>)
                           (send (send top-level-window get-hyper-panel) get-break-button))]
                     ; Thread to perform the work (e.g., download):
                     [t (parameterize ([break-enabled #f])
                          (thread
                           (lambda ()
                             (define (finished-work)
                               (semaphore-wait wait-to-show)
                               (set! done? #t)
                               (show-progress "")
                               (when progress-dlg
                                 (send progress-dlg show #f))
                               (when break-button
                                 (send break-button enable #f))
                               (close-browser-status-line top-level-window)
                               (break-thread timeout-thread)
                               (semaphore-post wait-to-show))
                             (with-handlers ([void (lambda (x)
                                                     (set! exn x))])
                               (parameterize ([break-enabled #t])
                                 (semaphore-post wait-to-break)
                                 (set! result
                                       (work (lambda (s)
                                               (set! e-text s)
                                               (semaphore-wait wait-to-show)
                                               (show-progress s)
                                               (semaphore-post wait-to-show))
                                             (lambda (finisher)
                                               (finished-work)
                                               (set! finished-work #f)
                                               (let ([wait-for-finish (make-semaphore)]
                                                     [exn #f])
                                                 (parameterize ([current-eventspace orig-eventspace])
                                                   (queue-callback
                                                    (lambda ()
                                                      (with-handlers ([void (lambda (x)
                                                                              (set! exn x))])
                                                        (finisher))
                                                      (semaphore-post wait-for-finish))))
                                                 (semaphore-wait wait-for-finish)
                                                 (when exn (raise exn))))))))
                             (when finished-work
                               (finished-work))
                             (semaphore-post wait-to-continue))))]
                     [make-dialog
                      (lambda ()
                        (semaphore-wait wait-to-show)
                        (unless done?
                          (init-browser-status-line top-level-window)
                          (update-browser-status-line top-level-window e-text)
                          (when break-button
                            (send break-button enable #t)
                            (send (send top-level-window get-hyper-panel)
                                  set-break-callback
                                  (lambda ()
                                    (semaphore-wait wait-to-break)
                                    (semaphore-post wait-to-break)
                                    (break-thread t))))
                          (set! show-progress
                                (lambda (s) (update-browser-status-line top-level-window s))))
                        (semaphore-post wait-to-show))])
                (thread-wait timeout-thread)
                (unless done?
                  (make-dialog))
                (yield wait-to-continue)
                (when exn (raise exn))
                result))
            
            (define/public (init-browser-status-line top-level-window)
              (send top-level-window open-status-line 'browser:hyper.ss))
            (define/public (update-browser-status-line top-level-window s)
              (send top-level-window update-status-line 'browser:hyper.ss s))
            (define/public (close-browser-status-line top-level-window)
              (send top-level-window close-status-line 'browser:hyper.ss))
            
            [define/public reload
              (opt-lambda ([progress void])
                (define (read-from-port p mime-headers)
                  (let-values ([(wrapping-on?) #t])
                    (lock #f)
                    (dynamic-wind
                     (lambda ()
                       (begin-busy-cursor)
                       ; (send progress start)
                       (begin-edit-sequence #f))
                     (lambda () 
                       (set! htmling? #t)
                       (erase)
                       (clear-undos)
                       (let* ([mime-type (extract-field "content-type" mime-headers)]
                              [html? (and mime-type
                                          (regexp-match "text/html" mime-type))])
                         (cond
                           [(or (and mime-type (regexp-match "application/" mime-type))
                                (and (url? url)
                                     (regexp-match "[.]plt$" (url-path url))
                                     ; document-not-found produces HTML:
                                     (not html?)))
                            ; Save the file
                            (progress #f)
                            (end-busy-cursor) ; turn off cursor for a moment...
                            (let* ([orig-name (and (url? url)
                                                   (let ([m (regexp-match "([^/]*)$" (url-path url))])
                                                     (and m (cadr m))))]
                                   [size (let ([s (extract-field "content-length" mime-headers)])
                                           (and s (let ([m (regexp-match
                                                            "[0-9]+"
                                                            s)])
                                                    (and m (string->number (car m))))))]
                                   [install?
                                    (and (and orig-name (regexp-match "[.]plt$" orig-name))
                                         (let ([d (make-object dialog% (string-constant install?))]
                                               [d? #f]
                                               [i? #f])
                                           (make-object message%
                                             (string-constant you-have-selected-an-installable-package)
                                             d)
                                           (make-object message% 
                                             (string-constant do-you-want-to-install-it?) d)
                                           (when size
                                             (make-object message%
                                               (format (string-constant paren-file-size) size) d))
                                           (let ([hp (make-object horizontal-panel% d)])
                                             (send hp set-alignment 'center 'center)
                                             (send (make-object button% 
                                                     (string-constant download-and-install)
                                                     hp
                                                     (lambda (b e)
                                                       (set! i? #t)
                                                       (send d show #f))
                                                     '(border))
                                                   focus)
                                             (make-object button% (string-constant download) hp
                                               (lambda (b e)
                                                 (set! d? #t)
                                                 (send d show #f)))
                                             (make-object button% (string-constant cancel) hp
                                               (lambda (b e)
                                                 (send d show #f))))
                                           (send d center)
                                           (send d show #t)
                                           (unless (or d? i?)
                                             
                                             ; turn the cursor back on before escaping
                                             (begin-busy-cursor)
                                             
                                             (raise (make-exn:cancelled
                                                     "Package Cancelled"
                                                     (current-continuation-marks))))
                                           i?))]
                                   [f (if install?
                                          (make-temporary-file "tmp~a.plt")
                                          (put-file 
                                           (if size
                                               (format 
                                                (string-constant save-downloaded-file/size)
                                                size)
                                               (string-constant save-downloaded-file))
                                           #f ; should be calling window!
                                           #f
                                           orig-name))])
                              (begin-busy-cursor) ; turn the cursor back on
                              (when f
                                (let* ([d (make-object dialog% (string-constant downloading) top-level-window)]
                                       [message (make-object message% 
                                                  (string-constant downloading-file...)
                                                  d)]
                                       [gauge (if size
                                                  (make-object gauge% #f 100 d)
                                                  #f)]
                                       [exn #f]
                                       ; Semaphores to avoid race conditions:
                                       [wait-to-start (make-semaphore 0)]
                                       [wait-to-break (make-semaphore 0)]
                                       ; Thread to perform the download:
                                       [t (thread
                                           (lambda ()
                                             (semaphore-wait wait-to-start)
                                             (with-handlers ([void
                                                              (lambda (x)
                                                                (when (not (exn:break? x))
                                                                  (set! exn x)))]
                                                             [void ; throw away break exceptions
                                                              void])
                                               (semaphore-post wait-to-break)
                                               (with-output-to-file f
                                                 (lambda ()
                                                   (let loop ([total 0])
                                                     (when gauge
                                                       (send gauge set-value 
                                                             (inexact->exact
                                                              (floor (* 100 (/ total size))))))
                                                     (let ([s (read-string 1024 p)])
                                                       (unless (eof-object? s)
                                                         (display s)
                                                         (loop (+ total (string-length s)))))))
                                                 'binary 'truncate))
                                             (send d show #f)))])
                                  (send d center)
                                  (make-object button% (string-constant &stop)
                                    d (lambda (b e)
                                        (semaphore-wait wait-to-break)
                                        (set! f #f)
                                        (send d show #f)
                                        (break-thread t)))
                                  ; Let thread run only after the dialog is shown
                                  (queue-callback (lambda () (semaphore-post wait-to-start)))
                                  (send d show #t)
                                  (when exn (raise exn)))
                                (let ([sema (make-semaphore 0)])
                                  (when (and f install?)
                                    (run-installer f
                                                   (lambda ()
                                                     (semaphore-post sema)))
                                    (yield sema))))
                              (raise
                               (if f
                                   (make-exn:file-saved-instead
                                    (if install?
                                        (string-constant package-was-installed)
                                        (string-constant download-was-saved))
                                    (current-continuation-marks)
                                    f)
                                   (make-exn:cancelled "The download was cancelled."
                                                       (current-continuation-marks)))))]
                           [(or (port? url)
                                (and (url? url)
                                     (regexp-match "[.]html?$" (url-path url)))
                                html?)
                            ; HTML
                            (progress #t)
                            (let* ([directory
                                    (or (if (and (url? url)
                                                 (string=? "file" (url-scheme url)))
                                            (let ([path (url-path url)])
                                              (let-values ([(base name dir?) (split-path path)])
                                                (if (string? base)
                                                    base
                                                    #f)))
                                            #f)
                                        (current-load-relative-directory))])
                              (in-thread-with-cancel
                               (lambda (show-status finish-without-dialog)
                                 (parameterize ([html-status-handler show-status]
                                                [current-load-relative-directory directory]
                                                [html-eval-ok url-allows-evaling?])
                                   (html-convert p this)))
                               "Converting html..."))]
                           [else
                            ; Text
                            (progress #t)
                            (in-thread-with-cancel
                             (lambda (show-status finish-without-dialog)
                               (begin-edit-sequence)
                               (let loop ()
                                 (let ([r (read-line p 'any)])
                                   (unless (eof-object? r)
                                     (insert r)
                                     (insert #\newline)
                                     (loop))))
                               (change-style (make-object style-delta% 'change-family 'modern)
                                             0 (last-position))
                               (set! wrapping-on? #f)
                               (end-edit-sequence))
                             "Loading text...")])))
                     (lambda ()
                       (end-edit-sequence)
                       (end-busy-cursor)
                       ; (send progress stop)
                       (set! htmling? #f)
                       (close-input-port p)))
                    (set-modified #f)
                    (auto-wrap wrapping-on?)
                    (set-autowrap-bitmap #f)
                    (lock #t)))
                
                (when url
                  (let ([headers 
                         (if (port? url)
                             (begin
                               (read-from-port url empty-header)
                               empty-header)
                             (let* ([busy? #t]
                                    [cust (make-custodian)]
                                    [stop-busy (lambda ()
                                                 (when busy?
                                                   (set! busy? #f)
                                                   (end-busy-cursor)))])
                               (dynamic-wind
                                ;; Turn on the busy cursor:
                                begin-busy-cursor
                                (lambda ()
                                  (in-thread-with-cancel
                                   (lambda (show-status finish-without-dialog)
                                     ;; Try to get mime info, but use get-pure-port if it fails.
                                     ;; Use a custodian to capture all TCP connections so we
                                     ;;  can break at any time (in which case a conneciton might
                                     ;;  have been created but not yet delivered to some dyn-wind-based
                                     ;;  close).
                                     (parameterize ([current-custodian cust])
                                       (with-handlers ([(lambda (x)
                                                          (and (not-break-exn? x)
                                                               busy?))
                                                        (lambda (x) 
                                                          (call/input-url 
                                                           url
                                                           (if post-string 
                                                               (lambda (u s) (post-pure-port u post-string s))
                                                               get-pure-port)
                                                           (lambda (p)
                                                             (stop-busy)
                                                             (finish-without-dialog
                                                              (lambda ()
                                                                (read-from-port p empty-header)))
                                                             empty-header)
                                                           null))])
                                         (call/input-url 
                                          url 
                                          (if post-string 
                                              (lambda (u s) (post-impure-port u post-string s))
                                              get-impure-port)
                                          (lambda (p)
                                            (let ([headers (purify-port p)])
                                              (stop-busy)
                                              (finish-without-dialog
                                               (lambda ()
                                                 (read-from-port p headers)))
                                              headers))
                                          null))))
                                   "Fetching page ..."))
                                (lambda ()
                                  (stop-busy)
                                  (custodian-shutdown-all cust)))))])
                    ;; Page is a redirection?
                    (let ([m (regexp-match "^HTTP/[^ ]+ 301 " headers)])
                      (when m
                        (let ([loc (extract-field "location" headers)])
                          (when loc
                            (set! redirection 
                                  (if (url? url)
                                      (combine-url/relative url loc)
                                      (string->url loc))))))))))]
            
            (inherit find-position get-snip-location
                     get-dc get-between-threshold
                     editor-location-to-dc-location
                     dc-location-to-editor-location)
            ;; use on-event rather than on-default-event since we want
            ;; to override the caret handling snip in the case that
            ;; an image-map-snip is there.
            (rename [super-on-event on-event])
            (define/override (on-event event)
              (let* ([edge-close-b (box 0)]
                     [on-it-b (box #f)]
                     [dc-event-x (send event get-x)]
                     [dc-event-y (send event get-y)])
                (let-values ([(editor-event-x editor-event-y) 
                              (dc-location-to-editor-location dc-event-x dc-event-y)])
                  (let ([pos (find-position editor-event-x editor-event-y #f on-it-b edge-close-b)])
                    (cond
                      [(and (unbox on-it-b)
                            (not (<= (abs (unbox edge-close-b))
                                     (get-between-threshold))))
                       (let ([snip (find-snip pos 'after-or-none)])
                         (cond
                           [(and snip (is-a? snip image-map-snip%))
                            (let ([bsnip-left (box 0)]
                                  [bsnip-top (box 0)]
                                  [bsnip-right (box 0)]
                                  [bsnip-bot (box 0)])
                              (get-snip-location snip bsnip-left bsnip-top #f)
                              (get-snip-location snip bsnip-right bsnip-bot #t)
                              (let ([snip-left (unbox bsnip-left)]
                                    [snip-top (unbox bsnip-top)]
                                    [snip-right (unbox bsnip-right)]
                                    [snip-bot (unbox bsnip-bot)])
                                (cond
                                  [(and (<= snip-left editor-event-x snip-right)
                                        (<= snip-top editor-event-y snip-bot))
                                   (let-values ([(x y) (editor-location-to-dc-location snip-left snip-top)])
                                     (send snip on-event (get-dc) x y snip-left snip-top event))]
                                  [else (super-on-event event)])))]
                           [else (super-on-event event)]))]
                      [else (super-on-event event)])))))
            
            (super-instantiate ())

            ;; load url, but the user might break:
            (with-handlers ([exn:break? void])
              ;(printf "url: ~a\n" (url->string url)) ;; handy for debugging help desk
              (reload progress)))))

      (define hyper-text% (hyper-text-mixin text:keymap%))

      (define hyper-keymap (make-object keymap%))
      (send hyper-keymap add-function "rewind" 
            (lambda (txt evt)
              (call-with-hyper-panel
               txt
               (lambda (panel)
                 (send panel rewind)))))
      (send hyper-keymap add-function "forward" 
            (lambda (txt evt)
              (call-with-hyper-panel
               txt
               (lambda (panel)
                 (send panel forward)))))
      (send hyper-keymap add-function "do-wheel" 
            (lambda (txt evt)
	      ;; Redirect the event to the canvas, which should
	      ;;  handle the event
	      (send (send txt get-canvas) on-char evt)))
      (add-text-keymap-functions hyper-keymap)
      (send hyper-keymap map-function "d:[" "rewind")
      (send hyper-keymap map-function "a:[" "rewind")
      (send hyper-keymap map-function "c:[" "rewind")
      (send hyper-keymap map-function "d:left" "rewind")
      (send hyper-keymap map-function "a:left" "rewind")
      (send hyper-keymap map-function "c:left" "rewind")
      (send hyper-keymap map-function "m:left" "rewind")
      (send hyper-keymap map-function "d:]" "forward")
      (send hyper-keymap map-function "a:]" "forward")
      (send hyper-keymap map-function "c:]" "forward")
      (send hyper-keymap map-function "d:right" "forward")
      (send hyper-keymap map-function "a:right" "forward")
      (send hyper-keymap map-function "c:right" "forward")
      (send hyper-keymap map-function "m:right" "forward")
      (send hyper-keymap map-function "wheelup" "do-wheel")
      (send hyper-keymap map-function "pageup" "previous-page")
      (send hyper-keymap map-function "wheeldown" "do-wheel")
      (send hyper-keymap map-function "pagedown" "next-page")
      
      ;; call-with-hyper-panel : object ((is-a?/c hyper-panel<%>) -> void) -> void
      (define (call-with-hyper-panel text f)
        (when (is-a? text hyper-text<%>)
          (let* ([canvas (send text get-canvas)]
                 [panel
                  (and canvas
                       (send (send canvas get-top-level-window) get-hyper-panel))])
            (f panel))))
            
      ;; path-below? : string[normalized-path] string[normalized-path] -> boolean
      ;; returns #t if subpath points to a place below top
      (define (path-below? top longer)
        (let loop ([top (explode-path top)]
                   [longer (explode-path longer)])
          (cond
            [(null? top) #t]
            [(null? longer) #f]
            [(equal? (car top) (car longer))
             (loop (cdr top) (cdr longer))]
            [else #f])))

      (keymap:add-to-right-button-menu/before
       (let ([old (keymap:add-to-right-button-menu/before)])
         (lambda (menu editor event)
           (when (is-a? editor hyper-text<%>)
             (let* ([panel (let ([canvas (send editor get-canvas)])
                             (and canvas
                                  (send (send canvas get-top-level-window) get-hyper-panel)))]
                    [back
                     (instantiate menu-item% ()
                       (label (string-append "< " (string-constant rewind-in-browser-history)))
                       (parent menu)
                       (callback
                        (lambda (_1 _2)
                          (when panel
                            (send panel rewind)))))]
                    [forward
                     (instantiate menu-item% ()
                       (label (string-append (string-constant forward-in-browser-history) " >"))
                       (parent menu)
                       (callback
                        (lambda (_1 _2)
                          (when panel
                            (send panel forward)))))])
               (send back enable (send panel can-rewind?))
               (send forward enable (send panel can-forward?))
               (instantiate separator-menu-item% ()
                 (parent menu))))
           (old menu editor event))))
      
      (define (hyper-canvas-mixin super%)
        (class super%
          (inherit get-editor set-editor refresh get-parent get-top-level-window)
          
          (define/public (get-editor%) hyper-text%)
          (define/public (make-editor url progress post-data) 
            (make-object (get-editor%) url (get-top-level-window) progress post-data))
          (define/public (current-page)
            (let ([e (get-editor)])
              (and e 
                   (let ([sbox (box 0)]
                         [ebox (box 0)])
                     (send e get-visible-position-range sbox ebox)
                     (list e (unbox sbox) (unbox ebox))))))
          (define/public (on-url-click k url post-data)
            (send (get-parent) on-url-click k url post-data))
          (define/public goto-url
            (opt-lambda (in-url relative [progress void] [post-data #f])
              (let* ([pre-url (if (or (url? in-url) (port? in-url))
                                  in-url
                                  (if relative
                                      (combine-url/relative 
                                       relative
                                       in-url)
                                      (string->url in-url)))])
                (let-values ([(e url)
                              (let ([e-now (get-editor)])
                                (if (and e-now
                                         (not post-data)
                                         (same-page-url? pre-url (send e-now get-url)))
                                    (begin
                                      (progress #t)
                                      (values e-now pre-url))
                                    (make-editor/follow-redirections pre-url progress post-data)))])
                  (when e
                    (let* ([tag-pos (send e find-tag (and (url? url) (url-fragment url)))])
                      
                      (unless (and tag-pos (positive? tag-pos))
                        (send e hide-caret #t))
                      (set-page (list e (or tag-pos 0) (send e last-position)) #t)
                      (send (get-parent) update-url-display
                            (format "~s"
                                    (if (url? url)
                                        (list (url->string url) (url-fragment url))
                                        url)))
                      (when tag-pos (send e set-position tag-pos))))))))
          
          ;; remap-url : url? -> (union #f url? string?)
          ;; this method is intended to be overridden so that derived classes can change
          ;; the behavior of the browser.
          (define/public (remap-url url)
            url)
          
          ;; make-editor/follow-redirections : url (boolean??? -> void) ??? -> (values (union #f editor) (union #f url))
          ;; builds an html editor using make-editor and follows any redictions,
          ;; but stops after 10 redirections (just in case there are too many
          ;; of these things, give the user a chance to stop)
          (define (make-editor/follow-redirections init-url progress post-data)
            (let loop ([n 10]
                       [unmapped-url init-url])
              (let ([url (if (url? unmapped-url)
                             (let ([rurl (remap-url unmapped-url)])
                               (cond
                                 [(string? rurl) (string->url rurl)]
                                 [(url? rurl) rurl]
				 [(not rurl) #f]
                                 [else (error 'remap-url "expected a url struct, a string, or #f, got ~e" rurl)]))
                             unmapped-url)])
                (if url
                    (let ([html-editor (make-editor url progress post-data)])
                      (cond
                        [(zero? n) 
                         (values html-editor url)]
                        [(send html-editor get-redirection)
                         =>
                         (lambda (new-url) (loop (- n 1) new-url))]
                        [else
                         (values html-editor url)]))
                    (values #f #f)))))
          
	    [define/public after-set-page (lambda () (void))]
            [define/public set-page
             (lambda (page notify?)
               (let ([e (car page)]
                     [spos (cadr page)]
                     [epos (caddr page)]
                     [curr (get-editor)]
                     [current (current-page)])
                 ; Pre-size the editor to avoid visible reflow
                 (when curr
                   (let ([wbox (box 0)])
                     (send curr get-view-size wbox (box 0))
                     (when (send e auto-wrap)
                       (send e set-max-width (unbox wbox)))))
                 (send e begin-edit-sequence)
                 (when notify?
                   (send (get-parent) leaving-page current (list e 0 0)))
                 (set-editor e (and current (zero? (cadr current)) (zero? spos)))
                 (send e scroll-to-position spos #f epos 'start)
                 (send e end-edit-sequence)
		 (after-set-page)
                 (when (or (positive? spos) (not current) (positive? (cadr current)))
                   (refresh))))]
          (super-instantiate ())))
      
      (define hyper-canvas% (hyper-canvas-mixin editor-canvas%))
      
      (define info-canvas%
        (class canvas%
          (inherit min-client-height get-dc stretchable-height
                   get-parent enable refresh show)
          (field
            [text ""])
          [define/override on-paint
             (lambda ()
               (let ([dc (get-dc)])
                 (send dc clear)
                 (send dc draw-text text 4 2)))]
          [define/public erase-info (lambda ()
                          (unless (string=? text "")
                            (set! text "")
                            (let ([dc (get-dc)])
                              (send dc clear))))]
            [define/public set-info (lambda (t)
                        (set! text t)
                        (if (string=? t "")
                            (show #f)
                            (let ([dc (get-dc)])
                              (send dc clear)
                              (show #t)
                              (refresh))))]
          (super-instantiate ())
          (stretchable-height #f)
          (enable #f)
          (show #f)
          (let ([font (make-object font% 
                        (send (send (get-parent) get-label-font) get-point-size) 
                        'default 'normal 'normal)]
                [dc (get-dc)])
            (send dc set-font font)
            (send dc set-text-foreground (make-object color% "FOREST GREEN"))
            (let-values ([(w h d a) (send dc get-text-extent "X" font)])
              (min-client-height (+ 4 (inexact->exact (ceiling h))))))))
      
      (define (hyper-panel-mixin super%)
        (class super% 
          (init info-line?)
          (inherit reflow-container)
          (super-instantiate ())
          
          (field
           [url-message ;; doesn't work for forwards and backwards in the history
            (and #f
                 (directory-exists? (build-path (collection-path "mzlib")
                                                "CVS"))
                 (make-object message% "" this))])
          (when url-message
            (send url-message stretchable-width #t))
          [define/public update-url-display
            (lambda (str)
              (when url-message
                (send url-message set-label str)))]
          
          
          [define/private clear-info
            (lambda () 
              (when info 
                (send info erase-info)))]
          [define/private update-info
            (lambda (page) 
              (when (and info page)
                (let ([notes (send (page->editor page) get-document-notes)])
                  (send info set-info
                        (filter-notes notes (send (page->editor page) get-url))))))]
          [define/private go
            (lambda (page)
              (clear-info)
              (send c set-page page #f)
              (update-info page)
              (update-buttons page)
              (on-navigate))]
          
          [define/public current-page
            (lambda ()
              (send c current-page))]
          [define/public rewind 
            (lambda ()
              (unless (null? past)
                (let ([page (car past)])
                  (set! future (cons (send c current-page) future))
                  (set! past (cdr past))
                  (go page))))]
          [define/public forward
            (lambda ()
              (unless (null? future)
                (let ([page (car future)])
                  (set! past (cons (send c current-page) past))
                  (set! future (cdr future))
                  (go page))))]
          [define/public can-forward?
            (lambda ()
              (not (null? future)))]
          [define/public can-rewind?
            (lambda ()
              (not (null? past)))]
          [define/public get-canvas% (lambda () hyper-canvas%)]
          [define/public make-canvas (lambda (f) (make-object (get-canvas%) f))]
          [define/public make-control-bar-panel (lambda (f) (make-object horizontal-panel% f))]
          (field
           [past null]
           [future null] 
           
           
           ;; (union #f                             -- no init page
           ;;         string                        -- delayed init page
           ;;         url                           -- delayed init page
           ;;         (list editor number numer))   -- forced init page
           [init-page #f]
           
           
           [hp (make-control-bar-panel this)]
           [control-bar? (is-a? hp area-container<%>)]
           [back (and control-bar?
                      (make-object button%
                        (string-append "< " (string-constant rewind-in-browser-history))
                        hp
                        (lambda (b ev) 
                          (rewind))))]
           [forw (and control-bar?
                      (make-object button% 
                        (string-append (string-constant forward-in-browser-history) " >")
                        hp
                        (lambda (b ev) 
                          (forward))))])
          
          ;(field (home-page #f))
          ;(define/public (set-home-page url)
          ;  (set! init-page url))
          
          [define/private home-callback
            (lambda () 
              (cond
                [(or (url? init-page)
                     (string? init-page))
                 
                 ; handle stopping when loading the home page
                 (with-handlers ([exn:break? 
                                  (lambda (x) (void))])
                   (send c goto-url init-page #f)
                   (set! init-page (send c current-page))
                   (update-buttons init-page))]
                [else 
                 (send c set-page init-page #t)]))]
          (field
           [home (and control-bar?
                      (make-object button% (string-constant home) hp
                        (lambda (b ev)
                          (home-callback))))])
          [define/private update-buttons
            (lambda (page)
              (unless init-page
                (set! init-page page))
              (when control-bar?
                (send back enable (pair? past))
                (send forw enable (pair? future))
                
                (send choice clear)
                (for-each
                 (lambda (p)
                   (send choice append 
                         (let ([s (send (car p) get-title)])
                           (or s (string-constant untitled)))))
                 (append (reverse future)
                         (if page (list page) null)
                         past))
                (let ([c (send choice get-number)])
                  (unless (zero? c)
                    (send choice set-selection (length future))))))]
          (field
           [choice (and control-bar?
                        (make-object choice% #f null hp
                          (lambda (ch e)
                            (let* ([l (append (reverse past)
                                              (list (send c current-page))
                                              future)]
                                   [pos (- (send choice get-number) (send choice get-selection) 1)])
                              (let loop ([l l][pre null][pos pos])
                                (cond
                                  [(zero? pos)
                                   (set! past pre)
                                   (set! future (cdr l))
                                   (go (car l))]
                                  [else (loop (cdr l)
                                              (cons (car l) pre)
                                              (sub1 pos))]))))))]
           [break-callback void]
           [break-button
            (and control-bar?
                 (new button%
                      (label cs-break)
                      (parent hp)
                      (callback
                       (lambda (x y)
                         (break-callback)))))])
          (define/public (get-break-button) break-button)
          (define/public (set-break-callback bc) (set! break-callback bc))
          (when break-button
            (send break-button enable #f))
          
          (field
           [info (and info-line?
                      (make-object info-canvas% this))]
           [c (make-canvas this)])
          
          ;; set-init-page : (union string url) -> void
          [define/public set-init-page
            (lambda (p)
              (set! init-page p))]
          [define/public goto-init-page
            (lambda ()
              (home-callback))]
          
          ; [get-progress (lambda () progress)]
          [define/public on-navigate (lambda () (void))]
          [define/public filter-notes (lambda (l) (apply string-append l))]
          [define/public get-canvas (lambda () c)]
          [define/public on-url-click (lambda (k url post-data) (k url post-data))]
          
          [define/public reload
            (lambda ()
              (let ([c (get-canvas)])
                (and c 
                     (let ([e (send c get-editor)])
                       (and e
                            (send e reload))))))]
          
          [define/public leaving-page
            (lambda (page new-page)
              (set! future null)
              (when page
                (set! past (cons page past)))
              (when (> (length past) history-limit)
                (set! past
                      (let loop ([l past])
                        (if (null? (cdr l))
                            null
                            (cons (car l) (loop (cdr l)))))))
              (clear-info)
              (update-buttons new-page)
              (update-info new-page))]
          (when control-bar?
            (send choice stretchable-width #t)
            (send hp stretchable-height #f))
          (update-buttons #f)))
      
      (define hyper-panel% (hyper-panel-mixin vertical-panel%))
      
      (define hyper-frame<%>
        (interface ()
          get-hyper-panel
          get-hyper-panel%))
      
      (define hyper-no-show-frame-mixin
        (mixin (frame:status-line<%>) (hyper-frame<%>)
          (field [p #f])
          (define/public get-hyper-panel% (lambda () hyper-panel%))
          (define/public get-hyper-panel (lambda () p))
          (inherit show get-area-container)
          (super-instantiate ())
          (set! p (make-object (get-hyper-panel%) #f (get-area-container)))))
      
      (define hyper-frame-mixin
        (compose
         (mixin (hyper-frame<%> top-level-window<%>) ()
           (init start-url)
           (inherit show get-hyper-panel)
           (super-instantiate ())
           (show #t)
           (send (send (get-hyper-panel) get-canvas) goto-url start-url #f))
         hyper-no-show-frame-mixin))
      
      (define hyper-frame% (hyper-frame-mixin (frame:status-line-mixin frame:basic%)))
      (define hyper-no-show-frame% (hyper-no-show-frame-mixin (frame:status-line-mixin frame:basic%)))
      
      (define (editor->page e) (list e 0 0))
      (define (page->editor e) (car e))
      
      (define (same-page? a b)
        (eq? (car a) (car b)))
      
      (define (open-url file)
        (make-object hyper-frame% file (string-constant browser) #f 500 450)))))
