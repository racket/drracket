(module debug-tool mzscheme
  (require (lib "unitsig.ss")
           (lib "stacktrace.ss" "errortrace")
           (lib "class.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred"))
  
  (provide tool@)
  (define tool@
    (unit/sig () 
      (import [drscheme:frame^ : drscheme:frame^]
              [drscheme:unit^ : drscheme:unit^]
              [drscheme:rep : drscheme:rep^]
              [drscheme:get/extend : drscheme:get/extend^]
              [drscheme:language-tower : drscheme:language-tower^]
              [drscheme:language : drscheme:language^])
      
      ;; debug-lang-mixin : (implements language<%>) -> (implements language<%>)
      (define (debug-lang-mixin %)
        (class %
          (override on-execute)
          (rename [super-on-execute on-execute])
          (define (on-execute settings run-in-user-thread)
            (run-in-user-thread
             (lambda ()
               (current-eval (add-debugging (current-eval)))
               (error-display-handler debug-tool-error-display-handler)))
            (super-on-execute settings run-in-user-thread))))

      ;; add-debugging : (sexp -> value) -> sexp -> value
      ;; adds debugging information to `sexp' and calls `oe'
      (define (add-debugging oe)
        (let ([debug-tool-eval-handler
               (lambda (exp)
                 (let* ([annotated
                         (if (compiled-expression? 
                              (if (syntax? exp) (syntax-e exp) exp))
                             e
                             (annotate-top (expand exp) null #f))])
                   (oe annotated)))])
          debug-tool-eval-handler))
      
      ;; debug-tool-error-display-handler : string exn -> void
      ;; displays the errortrace debugging output
      (define (debug-tool-error-display-handler msg exn)
        (display (exn-message exn) (current-error-port))
        (when (exn? exn)
          (newline (current-error-port))
          (print-error-trace (current-error-port) exn)))
      
      ;; print-error-trace : port exn -> void
      ;; prints the error trace output to `p'
      (define (print-error-trace p x)
        (let loop ([n (error-context-display-depth)]
                   [l (continuation-mark-set->list (exn-continuation-marks x) key)])
          (cond
            [(or (zero? n) (null? l)) (void)]
            [(pair? l)
             (let ([m (car l)])
               (fprintf p "~a~n"
                        (let ([file (car m)]
                              [line (cadr m)]
                              [col (cddr m)])
                          (format "~a, line ~a, char ~a." 
                                  (or file "UNKNOWN")
                                  (or line "???")
                                  (or col "???")))))
             (loop (- n 1)
                   (cdr l))]
            [else (void)])))
      
      
      
      ;; cm-key : symbol
      ;; the key used to put information on the continuation
      (define cm-key (gensym "debug-tool-continuation-mark-key"))
      
      ;; wrap : syntax syntax -> syntax
      ;; a member of stacktrace-imports^
      (define (with-mark mark expr)
        (with-syntax ([expr expr]
                      [source (if (string? (syntax-source mark))
                                  (string->symbol (syntax-source mark))
                                  (string->symbol (format "~a" (syntax-source mark))))]
                      [line (syntax-line mark)]
                      [col (syntax-column mark)]
                      [cm-key cm-key])
          (syntax
           (with-continuation-mark
            'cm-key
            '(source line . col)
            expr))))

      ;; an unused stacktrace-import^
      (define (profile-point x) x)
      
      (define-values/invoke-unit/sig stacktrace^ stacktrace@ #f stacktrace-imports^)
      
      (let ([make-debug-lang
	     (lambda (module position)
	       (instantiate (debug-lang-mixin 
                             (drscheme:language-tower:module-based-language->language-mixin
                              (drscheme:language-tower:simple-module-based-language->module-based-language-mixin
                               drscheme:language-tower:simple-module-based-language%)))
                            ()
                            (module module)
                            (language-position position)
                            (teachpack-names null)))])
	(drscheme:language:add-language
	 (make-debug-lang '(lib "full-mzscheme.ss" "lang") '("Full" "Textual (MzScheme)")))
	(drscheme:language:add-language
	 (make-debug-lang '(lib "full-mred.ss" "lang") '("Full" "Graphical (MrEd)")))))))