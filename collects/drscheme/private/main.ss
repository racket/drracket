
(module main mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
           "drsig.ss"
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "unitsig.ss")
           (prefix pretty-print: (lib "pretty.ss"))
           (prefix print-convert: (lib "pconvert.ss"))
           (lib "list.ss")
           (lib "file.ss")
           (lib "plt-installer.ss" "setup"))
  
  (define argv (namespace-variable-binding 'argv))
  
  (provide main@)
  
  (define main@
    (unit/sig drscheme:main^
      (import (drscheme:app : drscheme:app^)
              (drscheme:unit : drscheme:unit^)
              (drscheme:get/extend : drscheme:get/extend^)
              (drscheme:language-configuration : drscheme:language-configuration/internal^)
              [drscheme:language : drscheme:language^]
              [drscheme:snip : drscheme:snip^])
      
      ;; no more extension after this point
      (drscheme:get/extend:get-interactions-canvas%)
      (drscheme:get/extend:get-definitions-canvas%)
      (drscheme:get/extend:get-unit-frame%)
      (drscheme:get/extend:get-interactions-text%)
      (drscheme:get/extend:get-definitions-text%)
      (drscheme:language-configuration:get-languages)
      
      (scheme:set-sexp-snip-class
       (class* (scheme:get-sexp-snip-class) (drscheme:snip:special<%>)
         (inherit-field saved-snips)
         (define/public (read-special file line col pos)
           (let ([text (make-object text:basic%)])
             (for-each
              (lambda (s) (send text insert (send s copy)
                                (send text last-position)
                                (send text last-position)))
              saved-snips)
             (values (datum->syntax-object
                      #f
                      (read (drscheme:language:open-input-text 
                             text
                             0
                             (send text last-position)))
                      (list file line col pos 1))
                     1)))
         (super-instantiate ())))
      
      ;; this default can only be set *after* the
      ;; languages have all be registered by tools
      (preferences:set-default
       drscheme:language-configuration:settings-preferences-symbol
       (drscheme:language-configuration:get-default-language-settings)
       drscheme:language-configuration:language-settings?)

      ;; if the unmarshaller returns #f, that will fail the
      ;; test for this preference, reverting back to the default.
      ;; In that case, the default is specified in the pref.ss file
      ;; of the default collection and may not be the default
      ;; specified below.
      (preferences:set-un/marshall
       drscheme:language-configuration:settings-preferences-symbol
       (lambda (x)
	 (let ([lang (drscheme:language-configuration:language-settings-language x)]
	       [settings (drscheme:language-configuration:language-settings-settings x)])
	   (list (send lang get-language-position)
		 (send lang marshall-settings settings))))
       (lambda (x)
	 (and (list? x)
	      (= 2 (length x))
	      (let* ([lang-position (first x)]
		     [marshalled-settings (second x)]
		     [lang (ormap
			    (lambda (x)
			      (and (equal? lang-position
					   (send x get-language-position))
				   x))
			    (drscheme:language-configuration:get-languages))])
		(and lang
		     (let ([settings (send lang unmarshall-settings marshalled-settings)])
		       (drscheme:language-configuration:make-language-settings
			lang
			(or settings (send lang default-settings)))))))))
      
      ;;
      ;; Show expanded language dialog when version changes
      ;; 
      (preferences:set-default 'drscheme:last-version #f (lambda (x) (or (string? x) (not x))))
      (preferences:set-default 'drscheme:last-language #f (lambda (x) (or (symbol? x) (not x))))
      (drscheme:app:check-new-version)
      
      ;; the initial window doesn't set the 
      ;; unit object's state correctly, yet.
      (define (make-basic)
	(let* ([frame (drscheme:unit:open-drscheme-window)]
	       [interactions-edit (send frame get-interactions-text)]
	       [definitions-edit (send frame get-interactions-text)]
	       [filename (send definitions-edit get-filename)])
	  (unless filename
	    (send frame update-shown)
	    (send (send frame get-interactions-canvas) focus))
          (send frame show #t)))
      
      (define (remove-duplicates files)
        (let loop ([files files])
          (cond
            [(null? files) null]
            [else (if (member (car files) (cdr files))
                      (loop (cdr files))
                      (cons (car files) (loop (cdr files))))])))
      
      (define get-dropped-files (dynamic-require '(lib "splash.ss" "framework") 'get-dropped-files))
      (let* ([files-to-open (append (reverse (get-dropped-files))
				    (reverse (vector->list argv)))]
             [normalized/filtered
              (let loop ([files files-to-open])
                (cond
                  [(null? files) null]
                  [else (let ([file (car files)])
                          (if (file-exists? file)
                              (cons (normalize-path file) (loop (cdr files)))
                              (begin
                                (message-box
                                 (string-constant drscheme)
                                 (format (string-constant cannot-open-because-dne) file))
                                (loop (cdr files)))))]))]
             [no-dups (remove-duplicates normalized/filtered)])
        (if (null? no-dups)
            (make-basic)
            (for-each (lambda (f) (handler:edit-file
                                   f
                                   (lambda () (drscheme:unit:open-drscheme-window f))))
                      no-dups))))))
