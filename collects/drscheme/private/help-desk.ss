
(module help-desk mzscheme
  (require (lib "unitsig.ss")
           (lib "string-constant.ss" "string-constants")
	   (lib "mred.ss" "mred")
           (lib "browser.ss" "net")
           (lib "help-desk.ss" "help")
           (lib "framework.ss" "framework")
	   "drsig.ss")
  
  (provide help-desk@)
  
  (define help-desk@
    (unit/sig  drscheme:help-desk^
      (import [drscheme:frame : drscheme:frame^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^])
      
      ; to decide if an internal browser connected to the web server by pipes will be used
      ; : browser-preference -> bool
      (define (use-internal-browser? browser)
        (eq? 'plt browser))

      (define get-hd-cookie
        (let ([hd-cookie #f]
              [internal? (use-internal-browser? (preferences:get 'plt:help-browser))])
          (preferences:add-callback
           'plt:help-browser
           (lambda (k v)
             (let ([new-internal? (use-internal-browser? v)])
               (unless (eq? new-internal? internal?)
                 (when hd-cookie
                   ((hd-cookie->exit-proc hd-cookie))
                   (set! hd-cookie #f)))
               (set! internal? new-internal?))))
          (lambda ()
            (unless hd-cookie
              (set! hd-cookie (start-help-server #f #f drs-browser-mixin)))
            hd-cookie)))
      
      (define (drs-browser-mixin %)
        (drscheme:frame:basics-mixin
         (frame:standard-menus-mixin
          %)))
      
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
      
      (define (goto-front-page)
	(when (get-hd-cookie)
	  (help-desk-browser (get-hd-cookie))))
        
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
          [() (when (get-hd-cookie)
		(help-desk-browser (get-hd-cookie)))]
          [(key) (help-desk key #t)]
          [(key lucky?) (help-desk key lucky? 'keyword+index)]
          [(key lucky? type) (help-desk key lucky? type 'exact)]
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
	      lucky?))]))
      
      ;; open-url : string -> void
      (define (open-url x) (send-url x)))))
