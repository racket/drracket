
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
  (define program (namespace-variable-binding 'program))
  (define get-dropped-files (namespace-variable-binding 'get-dropped-files))
  
  (provide main@)
  
  (define main@
    (unit/sig drscheme:main^
      (import (drscheme:app : drscheme:app^)
              (drscheme:unit : drscheme:unit^)
              (drscheme:get/extend : drscheme:get/extend^)
              (drscheme:language : drscheme:language^)
              (basis : userspace:basis^))
      
      
  ;; no more extension after this point
      (drscheme:get/extend:get-interactions-canvas%)
      (drscheme:get/extend:get-definitions-canvas%)
      (drscheme:get/extend:get-unit-frame%)
      (drscheme:get/extend:get-interactions-text%)
      (drscheme:get/extend:get-definitions-text%)
      
      
  ;; the initial window doesn't set the 
  ;; unit object's state correctly, yet.
      (define (make-basic)
        (let* ([frame (drscheme:unit:open-drscheme-window)])
          
          (let* ([interactions-edit (ivar frame interactions-text)]
                 [definitions-edit (ivar frame interactions-text)]
                 [filename (send definitions-edit get-filename)])
            (unless filename
              (send interactions-edit reset-console)
              (send interactions-edit insert-prompt)
              (send frame update-shown)
              (send (ivar frame interactions-canvas) focus)))
          (send frame show #t)))
      
      (define (remove-duplicates files)
        (let loop ([files files])
          (cond
            [(null? files) null]
            [else (if (member (car files) (cdr files))
                      (loop (cdr files))
                      (cons (car files) (loop (cdr files))))])))
      
      (let* ([files-to-open (append (reverse (top-level:get-dropped-files))
                                    (reverse (vector->list top-level:argv)))]
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
            (for-each (lambda (f) (fw:handler:edit-file
                                   f
                                   (lambda () (drscheme:unit:open-drscheme-window f))))
                      no-dups)))
      
      
  ;;
  ;; Show about box when version changes
  ;; 
      
      (fw:preferences:set-default 'drscheme:last-version #f
                                  (lambda (x)
                                    (or (string? x)
                                        (not x))))
      (drscheme:app:check-new-version))))
