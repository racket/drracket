(module syncheck-debug mzscheme
  (require (lib "pretty.ss")
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (provide debug-origin)  ;; : syntax [syntax] -> void
                          ;; creates a frame for examining the 
                          ;; origin and source fields of an expanded sexp
  
  (define debug-origin
    (case-lambda 
     [(original-object) (debug-origin original-object (expand original-object))]
     [(original-object expanded-object)
      (define-values (expanded-datum stx-ht) (syntax-object->datum/ht expanded-object))
      
      (define output-text (make-object text%))
      (define output-port (make-text-port output-text))
      (define info-text (make-object text%))
      (define info-port (make-text-port info-text))
      
      (define range-ht (make-hash-table))
      (define (range-pretty-print-pre-hook x v)
        (hash-table-put! range-ht x (send output-text last-position)))
      (define (range-pretty-print-post-hook x v)
        (hash-table-put! range-ht x 
                         (cons
                          (hash-table-get range-ht x)
                          (send output-text last-position))))
      
      (define (make-modern text)
        (send text change-style
              (make-object style-delta% 'change-family 'modern)
              0
              (send text last-position)))
      
      (define dummy
        (begin (pretty-print (syntax-object->datum original-object) output-port)
               (newline output-port)
               (parameterize ([current-output-port output-port]
                              [pretty-print-pre-print-hook range-pretty-print-pre-hook]
                              [pretty-print-post-print-hook range-pretty-print-post-hook]
                              [pretty-print-columns 30])
                 (pretty-print expanded-datum))
               (make-modern output-text)))
      
      (define ranges 
        (quicksort 
         (hash-table-map range-ht cons)
         (lambda (x y)
           (<= (- (car (cdr x)) (cdr (cdr x)))
               (- (car (cdr y)) (cdr (cdr y)))))))
      
      (define (show-info stx)
        (fprintf info-port "source: ~a\nposition: ~s\noffset: ~s\n\n"
                 (syntax-source stx)
                 (syntax-position stx)
                 (syntax-span stx))
        (let loop ([origin (syntax-property stx 'origin)])
          (cond
            [(pair? origin)
             (loop (car origin))
             (loop (cdr origin))]
            [(syntax? origin) 
             (display origin info-port)
             (newline info-port)
             (fprintf info-port
                      "original? ~a\ndatum:\n~a\n\n"
                      (and (syntax? origin) (syntax-original? origin))
                      (and (syntax? origin) (syntax-object->datum origin)))]
            [else (void)])))
      
      (for-each
       (lambda (range)
         (let* ([obj (car range)]
                [stx (hash-table-get stx-ht obj)]
                [start (cadr range)]
                [end (cddr range)])
           (when (syntax? stx)
             (send output-text set-clickback start end 
                   (lambda _ 
                     (send info-text begin-edit-sequence)
                     (send info-text erase)
                     (show-info stx)
                     (make-modern info-text)
                     (send info-text end-edit-sequence))))))
       ranges)
      
      (newline output-port)
      (newline output-port)
      (let ([before (send output-text last-position)])
        (display "all" output-port)
        (send output-text set-clickback
              before
              (send output-text last-position)
              (lambda _
                (send info-text begin-edit-sequence)
                (send info-text erase)
                (for-each (lambda (x) 
                            (let ([stx (hash-table-get stx-ht (car x))])
                              (when (syntax? stx)
                                (show-info stx))))
                          ranges)
                (make-modern info-text)
                (send info-text end-edit-sequence))))
      
      (let ()
        (define f (make-object frame% "Syntax 'origin Browser" #f 600 300))
        (define p (make-object horizontal-panel% f))
        (make-object editor-canvas% p output-text)
        (make-object editor-canvas% p info-text)
        (send f show #t))]))
  
  ;; build-ht : stx -> hash-table
  ;; the resulting hash-table maps from the each sub-object's to it's syntax.
  (define (syntax-object->datum/ht stx)
    (let ([ht (make-hash-table)])
      (values (let loop ([stx stx])
                (let ([obj (syntax-e stx)])
                  (cond
                    [(list? obj) 
                     (let ([res (map loop obj)])
                       (hash-table-put! ht res stx)
                       res)]
                    [(pair? obj) 
                     (let ([res (cons (loop (car obj))
                                      (loop (cdr obj)))])
                       (hash-table-put! ht res stx)
                       res)]
                    [(vector? obj) 
                     (let ([res (list->vector (map loop (vector->list obj)))])
                       (hash-table-put! ht res stx)
                       res)]
                    [else 
                     (let ([res (syntax-object->datum stx)])
                       (hash-table-put! ht obj res)
                       res)])))
              ht)))
  
  ;; make-text-port : text -> port
  ;; builds a port from a text object.  
  (define (make-text-port text)
    (make-output-port (lambda (c) (send text insert c 
                                        (send text last-position)
                                        (send text last-position)))
                      void)))
