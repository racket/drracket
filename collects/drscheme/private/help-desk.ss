(module help-desk mzscheme
  (require (lib "list.ss")
           (lib "string.ss")
           (lib "file.ss")
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
      (define help-desk-frame #f)
      (define (set-font-size x) (void))
      
      (define doc-collections-changed void)
      
      (preferences:add-callback
       drscheme:language-configuration:settings-preferences-symbol
       (lambda (p v) (doc-collections-changed)))
      
      (preferences:add-callback
       'drscheme:font-size
       (lambda (p v)
         (set-font-size v)
         #t))
      
      (define (user-defined-doc-position doc)
        (let ([lang (preferences:get drscheme:language-configuration:settings-preferences-symbol)])
          (case (string->symbol doc)
            [(advanced) 100]
            [(intermediate) 101]
            [(beginning) 102]
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
          (set! doc-collections-changed _doc-collections-changed)
          (set! set-font-size _set-font-size)
          (set! load-help-desk void)))
      
      (define (open-url url)
        (load-help-desk)
        (new-help-frame url))
      
      (define (open-users-url frame)
        (load-help-desk)
        (open-url-from-user 
         frame 
         (if (method-in-interface? 'goto-url (object-interface frame))
             (lambda (url)
               (send frame goto-url url))
             new-help-frame)))
      
      (define help-desk
        (case-lambda
         [()
          (begin-busy-cursor)
          (load-help-desk)
          (set! help-desk-frame (new-help-frame startup-url))
          (end-busy-cursor)]
         [(key) (help-desk key #t)]
         [(key lucky?) (help-desk key lucky? 'keyword+index)]
         [(key lucky? type) (help-desk key lucky? type 'exact)]
         [(key lucky? type mode)
          (let ([turn-cursor-off? (not help-desk-frame)])
            (if help-desk-frame
                (send help-desk-frame show #t)
                (begin (begin-busy-cursor)
                       (help-desk)))
            (if lucky?
                (send help-desk-frame search-for-help/lucky key type mode)
                (send help-desk-frame search-for-help key type mode))
            (when turn-cursor-off?
              (end-busy-cursor)))])))))
