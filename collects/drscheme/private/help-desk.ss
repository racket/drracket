(module help-desk mzscheme
  (require (lib "list.ss")
           (lib "string.ss")
           (lib "file.ss")
           (lib "etc.ss")
	   (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "url.ss" "net")
           (lib "plt-installer.ss" "setup")
           (lib "plt-installer-sig.ss" "setup")
	   (lib "mred.ss" "mred")
	   (lib "mred-sig.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "framework-sig.ss" "framework")
           (lib "startup-url.ss" "help")
	   (lib "help-sig.ss" "help")
	   "drsig.ss")
  
  (provide help-desk@)
  
  (define help-desk@
    (unit/sig  drscheme:help-desk^
      (import [drscheme:frame : drscheme:frame^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^])
      
      (define new-help-frame #f)
      (define open-url-from-user #f)
      (define (set-font-size x) (void))
      (define original-doc-collections-changed void)
      
      (define (doc-collections-changed)
        (original-doc-collections-changed))
      
      ;; help-desk-frame : (instanceof frame%)
      ;; this holds onto a frame, even when the frame
      ;; is closed. Only when a user initiates some
      ;; help desk operation is this link broken
      ;; and the gc can reclaim it.
      (define help-desk-frame #f)
      
      
      ;; what does this do?
      ;(preferences:add-callback
      ; drscheme:language-configuration:settings-preferences-symbol
      ; (lambda (p v) (doc-collections-changed)))
      
      (preferences:add-callback
       'drscheme:font-size
       (lambda (p v)
         (set-font-size v)
         #t))
      
      (define (user-defined-doc-position doc)
        (let ([lang (preferences:get drscheme:language-configuration:settings-preferences-symbol)])
          (case (string->symbol doc)
            [(advanced) 101]
            [(intermediate) 102]
            [(beginning) 103]
            [else #f])))
      
      (define (load-help-desk)
        (define frame-mixin drscheme:frame:basics-mixin)
        (let-values ([(_new-help-frame
                       _open-url-from-user
                       _doc-collections-changed
                       _set-font-size)
                      (let ()
                        (define-values/invoke-unit/sig
                         (new-help-frame
                          open-url-from-user
                          doc-collections-changed
                          set-font-size)
                         (dynamic-require '(lib "help-unit.ss" "help") 'help@)
                         #f
			 setup:plt-installer^
                         mred^
                         framework^
                         (frame-mixin)
                         help:doc-position^)
                        (values new-help-frame
                                open-url-from-user
                                doc-collections-changed
                                set-font-size))])
          (set! new-help-frame _new-help-frame)
          (set! open-url-from-user _open-url-from-user)
          (set! original-doc-collections-changed _doc-collections-changed)
          (set! set-font-size _set-font-size)
          (set! load-help-desk void)))
      
      (define open-url
        (opt-lambda (url [progress void])
          (with-help-desk-frame
           #f
           (lambda ()
             (send help-desk-frame goto-url url progress)))))
      
      (define (open-users-url frame)
        (load-help-desk)
        (open-url-from-user 
         frame 
         (if (method-in-interface? 'goto-url (object-interface frame))
             (lambda (url progress) (send frame goto-url url progress))
             (lambda (url progress) (new-help-frame url #f progress)))))
      
      (define help-desk
        (case-lambda
         [()
          (with-help-desk-frame
           #t
           void)]
         [(key) (help-desk key #t)]
         [(key lucky?) (help-desk key lucky? 'keyword+index)]
         [(key lucky? type) (help-desk key lucky? type 'exact)]
         [(key lucky? type mode)
          (with-help-desk-frame
           #t
           (lambda ()
             (if lucky?
                 (send help-desk-frame search-for-help/lucky key type mode)
                 (send help-desk-frame search-for-help key type mode))))]))
      
      (define (with-help-desk-frame show-immediately? thunk)
        (cond
          [(or (not help-desk-frame)
               (not (send help-desk-frame is-shown?)))
           (begin-busy-cursor)
           (load-help-desk)
           (set! help-desk-frame (new-help-frame startup-url show-immediately?))
           (thunk)
           (end-busy-cursor)]
          [else
           (send help-desk-frame show #t)])))))
