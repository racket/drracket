(module help-desk mzscheme
  (require (lib "list.ss")
           (lib "string.ss")
           (lib "file.ss")
           (lib "etc.ss")
	   (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "url.ss" "net")
           (lib "plt-installer.ss" "setup")
           (lib "plt-installer-sig.ss" "setup")
	   (lib "mred.ss" "mred")
	   (lib "mred-sig.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "framework-sig.ss" "framework")
           
           (lib "sendurl.ss" "net")
           
           (lib "help-desk.ss" "help")
           
	   "drsig.ss")
  
  (provide help-desk@)
  
  (define help-desk@
    (unit/sig  drscheme:help-desk^
      (import [drscheme:frame : drscheme:frame^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^])

      (define hd-cookie #f)

      (define help-desk
        (case-lambda
          [()
           (unless hd-cookie
             (set! hd-cookie (start-help-server)))
           (help-desk-browser hd-cookie)]
          [(key) (help-desk key #t)]
          [(key lucky?) (help-desk key lucky? 'keyword+index)]
          [(key lucky? type) (help-desk key lucky? type 'exact)]
          [(key lucky? type mode)
           (unless hd-cookie
             (set! hd-cookie (start-help-server)))
           (search-for-docs
            hd-cookie
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
