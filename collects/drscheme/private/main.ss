
(module main mzscheme
  (require (lib "unitsig.ss")
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
  (define get-dropped-files (namespace-variable-binding 'get-dropped-files))
  
  (provide main@)
  
  (define main@
    (unit/sig drscheme:main^
      (import (drscheme:app : drscheme:app^)
              (drscheme:unit : drscheme:unit^)
              (drscheme:get/extend : drscheme:get/extend^)
              (drscheme:language : drscheme:language/internal^))
      
      
  ;; no more extension after this point
      (drscheme:get/extend:get-interactions-canvas%)
      (drscheme:get/extend:get-definitions-canvas%)
      (drscheme:get/extend:get-unit-frame%)
      (drscheme:get/extend:get-interactions-text%)
      (drscheme:get/extend:get-definitions-text%)
      (drscheme:language:get-languages)
      
      ;; this default can only be set *after* the
      ;; languages have all be registered by tools
      (preferences:set-default
       drscheme:language:settings-preferences-symbol
       (drscheme:language:get-default-language-settings)
       drscheme:language:language-settings?)

      ;; if the unmarshaller returns #f, that will fail the
      ;; test for this preference, reverting back to the default.
      ;; In that case, the default is specified in the pref.ss file
      ;; of the default collection and may not be the default
      ;; specified below.
      (preferences:set-un/marshall
       drscheme:language:settings-preferences-symbol
       (lambda (x)
	 (let ([lang (drscheme:language:language-settings-language x)]
	       [settings (drscheme:language:language-settings-settings x)])
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
			    (drscheme:language:get-languages))])
		(and lang
		     (let ([settings (send lang unmarshall-settings marshalled-settings)])
		       (drscheme:language:make-language-settings
			lang
			(or settings (send lang default-settings)))))))))
      
      
  ;; the initial window doesn't set the 
  ;; unit object's state correctly, yet.
      (define (make-basic)
        (let* ([frame (drscheme:unit:open-drscheme-window)])
          
          (let* ([interactions-edit (send frame get-interactions-text)]
                 [definitions-edit (send frame get-interactions-text)]
                 [filename (send definitions-edit get-filename)])
            (unless filename
              (send interactions-edit reset-console)
              (send interactions-edit insert-prompt)
              (send frame update-shown)
              (send (send frame get-interactions-canvas) focus)))
          (send frame show #t)))
      
      (define (remove-duplicates files)
        (let loop ([files files])
          (cond
            [(null? files) null]
            [else (if (member (car files) (cdr files))
                      (loop (cdr files))
                      (cons (car files) (loop (cdr files))))])))
      
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
                                 "DrScheme"
                                 (format "Cannot open ~a becuase it does not exist" file))
                                (loop (cdr files)))))]))]
             [no-dups (remove-duplicates normalized/filtered)])
        (if (null? no-dups)
            (make-basic)
            (for-each (lambda (f) (handler:edit-file
                                   f
                                   (lambda () (drscheme:unit:open-drscheme-window f))))
                      no-dups)))
      
  ;;
  ;; Show about box when version changes
  ;; 
      
      (preferences:set-default 'drscheme:last-version #f
                                  (lambda (x)
                                    (or (string? x)
                                        (not x))))
      (drscheme:app:check-new-version))))
