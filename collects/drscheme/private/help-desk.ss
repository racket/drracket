
(module help-desk mzscheme
  (require (lib "unitsig.ss")
           (lib "string-constant.ss" "string-constants")
	   (lib "mred.ss" "mred")
           (lib "external.ss" "browser")
           (lib "help-desk.ss" "help")
           (lib "framework.ss" "framework")
           (lib "class.ss")
	   "drsig.ss")
  
  (provide help-desk@)
  
  (define help-desk@
    (unit/sig  drscheme:help-desk^
      (import [drscheme:frame : drscheme:frame^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^]
              [drscheme:teachpack : drscheme:teachpack^])

      ;; : -> string
      (define (get-computer-language-info)
        (let* ([language/settings (preferences:get 
                                   drscheme:language-configuration:settings-preferences-symbol)]
               [language (drscheme:language-configuration:language-settings-language
                          language/settings)]
               [settings (drscheme:language-configuration:language-settings-settings
                          language/settings)])
          (format
           "~s"
           (list
            (send language get-language-position)
            (send language marshall-settings settings)))))

      ;; get-docs : (listof (cons string[short-dir-name] string[doc full name]))
      (define (get-docs) 
        (let ([dirs (find-doc-names)])
          (map (lambda (pr)
                 (let-values ([(base name dir?) (split-path (car pr))])
                   (cons name (cdr pr))))
               dirs)))
      
      (define (get-teachpack-filenames)
        (format "~s"
                (drscheme:teachpack:teachpack-cache-filenames
                 (preferences:get 'drscheme:teachpacks))))
      
      (set-bug-report-info! "Computer Language" get-computer-language-info)
      (set-bug-report-info! "Teachpack filenames" get-teachpack-filenames)

      (define get-hd-cookie
        (let ([hd-cookie #f])
          (lambda ()
            (unless hd-cookie
              (set! hd-cookie (start-help-server 
                               (lambda (x)
                                 (drscheme:frame:basics-mixin
                                  (drscheme-help-desk-mixin
                                   x))))))
            hd-cookie)))
      
      (define drscheme-help-desk-mixin
        (mixin (frame:standard-menus<%>) ()
          (define/override (file-menu:create-open-recent?) #t)
          
          (define/override (file-menu:new-callback x y)
            (handler:edit-file #f)
            #t)
          
          (define current-language 
            (preferences:get drscheme:language-configuration:settings-preferences-symbol))
          
          (define/override (order-manuals x)
            (send (drscheme:language-configuration:language-settings-language current-language)
                  order-manuals
                  x))
          (define/override (get-language-name) 
            (send (drscheme:language-configuration:language-settings-language current-language)
                  get-language-name))
          
          (rename [super-file-menu:between-new-and-open file-menu:between-new-and-open])
          (define/override (file-menu:between-new-and-open file-menu)
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant plt:hd:new-help-desk))
              (parent file-menu)
              (callback (lambda (x y) 
                          (let ([hd-cookie (get-hd-cookie)])
                            (visit-url-in-new-browser 
                             hd-cookie
                             (make-home-page-url
                              (hd-cookie-port hd-cookie)))))))
            (super-file-menu:between-new-and-open file-menu))

          (super-new)
          
          (inherit get-menu-bar)
          (let* ([language-menu (new menu% 
                                     (parent (get-menu-bar))
                                     (label (string-constant language-menu-name)))]
                 [language-item (new menu-item%
                                     (label (string-constant choose-language-menu-item-label))
                                     (parent language-menu)
                                     (shortcut #\l)
                                     (callback
                                      (lambda (x y)
                                        (let ([new-settings (drscheme:language-configuration:language-dialog
                                                             #f
                                                             current-language
                                                             this 
                                                             #t)])
                                          (when new-settings
                                            (set! current-language new-settings)
                                            (preferences:set
                                             drscheme:language-configuration:settings-preferences-symbol
                                             new-settings))))))])
            (frame:reorder-menus this))))
      
      (define (goto-help manual link)
        (with-handlers ([not-break-exn?
                         (lambda (exn)
                           (message-box 
                            "DrScheme"
                            (format (string-constant plt:hd:error-finding-docs)
                                    (if (exn? exn)
                                        (exn-message exn)
                                        (format "~s" exn)))))])
	  (when (get-hd-cookie)
	    (goto-manual-link (get-hd-cookie) manual link))))
      
      (define (goto-hd-loc cookie where)
	(when cookie
	  (goto-hd-location cookie where)))

      (define (goto-tour)
        (goto-hd-loc (get-hd-cookie) 'hd-tour))
      
      (define (goto-release-notes)
        (goto-hd-loc (get-hd-cookie) 'release-notes))
      
      (define (goto-plt-license)
        (goto-hd-loc (get-hd-cookie) 'plt-license))
      
      (define help-desk
        (case-lambda
          [() 
           (let* ([cookie (get-hd-cookie)]
                  [already-frame ((hd-cookie-find-browser cookie))])
             (if already-frame
                 (send already-frame show #t)
                 (goto-hd-location cookie 'front-page)))]
          [(key) (help-desk key #f)]
          [(key lucky?) (help-desk key lucky? 'keyword+index)]
          [(key lucky? type) (help-desk key lucky? type 'contins)]
          [(key lucky? type mode) 
	   (when (get-hd-cookie)
	     (search-for-docs
	      (get-hd-cookie)
	      key
	      (case type
		[(keyword) "keyword"]
		[(keyword+index) "keyword-index"]
		[(keyword+index+text) "keyword-index-text"]
		[else (error 'drscheme:help-desk:help-desk "unknown type argument: ~s" type)])
	      (case mode
		[(exact) "exact-match"]
		[(contains) "containing-match"]
		[(regexp) "regexp-match"]
		[else (error 'drscheme:help-desk:help-desk "unknown mode argument: ~s" mode)])
	      lucky?
              '()
              #t
              #f))]))
      
      ;; open-url : string -> void
      (define (open-url x) (send-url x)))))
