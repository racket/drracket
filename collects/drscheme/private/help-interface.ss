(module help-interface mzscheme
  (require (lib "list.ss")
           (lib "string.ss")
           (lib "file.ss")
           (lib "url.ss")
           (lib "plt-installer.ss" "setup")
           (lib "info-thingy.ss" "setup.ss")
           (lib "mred-wrap.ss")
           (lib "framework.ss" "framework")
           (lib "startup-url.ss" "help"))
  
  (provide help-interface@)
  
  (define help-interface@
    (unit/sig  drscheme:help-interface^
      (import [drscheme:frame : drscheme:frame^]
              [drscheme:language : drscheme:language^]
              [basis : plt:basis^])
      
      (define new-help-frame #f)
      (define open-url-from-user #f)
      (define help-desk-frame #f)
      (define (set-font-size x) (void))
      
      (define doc-collections-changed void)
      
      (preferences:add-callback
       drscheme:language:settings-preferences-symbol
       (lambda (p v) (doc-collections-changed)))
      
      (preferences:add-callback
       'drscheme:font-size
       (lambda (p v)
         (set-font-size v)
         #t))
      
      (define (user-defined-doc-position doc)
        (let ([lang (preferences:get drscheme:language:settings-preferences-symbol)])
          (cond
            [(basis:beginner-language? lang)
             (case (string->symbol doc)
               [(advanced) -100]
               [(intermediate) -101]
               [(beginning) -102]
               [else #f])]
            [(basis:intermediate-language? lang)
             (case (string->symbol doc)
               [(advanced) -101]
               [(intermediate) -102]
               [(beginning) -100]
               [else #f])]
            [(basis:advanced-language? lang)
             (case (string->symbol doc)
               [(advanced) -102]
               [(intermediate) -101]
               [(beginning) -100]
               [else #f])]
            [else
             (case (string->symbol doc)
               [(advanced) 100]
               [(intermediate) 101]
               [(beginning) 102]
               [else #f])])))
      
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
                         (require-library "helpr.ss" "help")
                         #f
                         mzlib:function^
                         mzlib:string^
                         mzlib:file^
                         mzlib:url^
                         setup:plt-installer^
                         setup:info^
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
         (if (ivar-in-interface? 'goto-url (object-interface frame))
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
