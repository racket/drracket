(module modes mzscheme
  (require (lib "unitsig.ss")
           (lib "string-constant.ss" "string-constants")
           "drsig.ss")
  
  (provide mode@)
  
  (define mode@
    (unit/sig drscheme:modes^
      (import)
      
      (define-struct mode (name surrogate repl-submit? matches-filename?))
      (define modes (list 
                     (make-mode 
                      (string-constant scheme-mode)
                      (new scheme:text-mode%)
                      (lambda (x) #f))
                     (make-mode 
                      (string-constant text-mode)
                      #f
                      (lambda (x) #t))))
      
      (define (get-modes) modes)
      
      (define (add-mode name surrogate repl-submit?)
        (let ([new-mode (make-mode name 
                                   surrogate
                                   repl-submit?)])
          (set! modes (append modes (list new-mode)))
          new-mode)))))