
(module help-desk mzscheme
  (require (lib "unitsig.ss")
           (lib "string-constant.ss" "string-constants")
	   (lib "mred.ss" "mred")
           (lib "browser.ss" "net")
           (lib "help-desk.ss" "help")
	   "drsig.ss")
  
  (provide help-desk@)
  
  (define help-desk@
    (unit/sig  drscheme:help-desk^
      (import [drscheme:frame : drscheme:frame^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^])

      (define get-hd-cookie
        (let ([hd-cookie #f])
          (lambda ()
            (unless hd-cookie
              (set! hd-cookie (start-help-server)))
            hd-cookie)))
      
      (define (goto-help manual link)
        (with-handlers ([not-break-exn?
                         (lambda (exn)
                           (message-box 
                            "DrScheme"
                            (format (string-constant plt:hd:error-finding-docs)
                                    (if (exn? exn)
                                        (exn-message exn)
                                        (format "~s" exn)))))])
          (goto-manual-link (get-hd-cookie) manual link)))
      
      (define (goto-front-page)
        (help-desk-browser (get-hd-cookie)))
        
      (define (goto-tour)
        (send-url (format "http://127.0.0.1:~a/doc/tour"
                          (hd-cookie->port (get-hd-cookie)))))
      
      (define (goto-release-notes)
        (send-url (format "http://127.0.0.1:~a/servlets/releaseinfo.ss"
                          (hd-cookie->port (get-hd-cookie)))))
      
      (define help-desk
        (case-lambda
          [() (help-desk-browser (get-hd-cookie))]
          [(key) (help-desk key #t)]
          [(key lucky?) (help-desk key lucky? 'keyword+index)]
          [(key lucky? type) (help-desk key lucky? type 'exact)]
          [(key lucky? type mode)
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
            lucky?)]))
      
      ;; open-url : string -> void
      (define (open-url x) (send-url x)))))
