(module tools mzscheme
  (require (lib "unitsig.ss")
           (lib "getinfo.ss" "setup")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           "drsig.ss"
           (lib "string-constant.ss" "string-constants"))
  
  (provide tools@)

  (define tools@
    (unit/sig drscheme:tools^
      (import [drscheme:frame : drscheme:frame^]
              [drscheme:unit : drscheme:unit^]
              [drscheme:rep : drscheme:rep^]
              [drscheme:get/extend : drscheme:get/extend^]
              [drscheme:language-tower : drscheme:language-tower^]
              [drscheme:language : drscheme:language^]
              [drscheme:init : drscheme:init^])
      
      ;; successful-tool = (make-successful-tool module-spec (union #f (instanceof bitmap%)) (union #f string))
      (define-struct successful-tool (spec bitmap name))
                     
      ;; successful-tools : (listof successful-tool)
      ;; this list contains the tools that sucessfully were loaded
      ;; it is updated in load/invoke-tool
      (define successful-tools null)
      
      ;; get-successful-tools : -> (listof sucessful-tool)
      (define (get-successful-tools) successful-tools)

      ;; collections-hash-table : (hash-table symbol string)
      ;; contains a list of the available collections.
      ;; use a hash table to cancel out duplicate collections
      (define collections-hash-table (make-hash-table))
      
      ;; add-collections-in-path : path -> void
      ;; adds each collection in the given path
      ;; to collections-hash-table
      (define (add-collections-in-path path)
        (for-each 
         (lambda (d) 
           (when (and (directory-exists? (build-path path d))
                      (not (string-ci=? d "CVS")))
             (hash-table-put! collections-hash-table (string->symbol d) d)))
         (with-handlers ([not-break-exn?
			  (lambda (x) null)])
	   (directory-list path))))
      
      ;; load/invoke-tools : string[collection-name] -> void
      ;; loads each tool in a collection
      (define (load/invoke-tools coll)
        (let ([table (with-handlers ([not-break-exn? 
                                      (lambda (x)
                                        (show-error
                                         (format (string-constant error-getting-info-tool)
                                                 coll)
                                         x)
                                        #f)])
                       (get-info (list coll)))])
          (when table
            (let* ([tools (table 'tools (lambda () null))]
                   [tool-icons (table 'tool-icons (lambda () (map (lambda (x) #f) tools)))]
                   [tool-names (table 'tool-names (lambda () (map (lambda (x) #f) tools)))])
              (unless (= (length tools) (length tool-icons))
                (message-box (string-constant drscheme)
                             (format (string-constant tool-tool-icons-same-length)
                                     tools tool-icons))
                (set! tool-icons (map (lambda (x) #f) tools)))
              (unless (= (length tools) (length tool-names))
                (message-box (string-constant drscheme)
                             (format (string-constant tool-tool-names-same-length)
                                     tools tool-names))
                (set! tool-names (map (lambda (x) #f) tools)))
              (for-each (load/invoke-tool coll) tools tool-icons tool-names)))))
      
      ;; load/invoke-tool :    string[collection-name] 
      ;;                    -> (listof string[sub-collection-name]) 
      ;;                       (union #f (cons string[filename] (listof string[collection-name])))
      ;;                       (union #f string)
      ;;                    -> void
      ;; `coll' is a collection to load the tool from
      ;; `in-path' is the `coll'-relative collection-path spec for the tool module file
      ;; `icon-spec' is the collection-path spec for the tool's icon, if there is one.
      ;; `name' is the name of the tool (only used in about box)
      (define (load/invoke-tool coll)
        (lambda (in-path icon-spec name)
          (let ([tool-bitmap
                 (and icon-spec
                      (install-tool-bitmap icon-spec))])
            (let/ec k
              (unless (or (string? in-path)
                          (and (list? in-path)
                               (not (null? in-path))
                               (andmap string? in-path)))
                (message-box (string-constant drscheme)
                             (string-constant invalid-tool-spec)
                             coll in-path)
                (k (void)))
              (let* ([tool-path
                      (if (string? in-path) 
                          `(lib ,in-path ,coll)
                          `(lib ,(car in-path) ,coll ,@(cdr in-path)))]
                     [unit 
                       (with-handlers ([not-break-exn? 
                                        (lambda (x)
                                          (show-error
                                           (format (string-constant error-invoking-tool-title)
                                                   coll in-path)
                                           x)
                                          (k (void)))])
                         (dynamic-require tool-path 'tool@))])
                (with-handlers ([not-break-exn? 
                                 (lambda (x)
                                   (show-error 
                                    (format (string-constant error-invoking-tool-title)
                                            coll in-path)
                                    x))])
                  (invoke-unit/sig unit
                                   ((unit drscheme:frame : drscheme:frame^)
                                    (unit drscheme:unit : drscheme:unit^)
                                    (unit drscheme:rep : drscheme:rep^)
                                    (unit drscheme:get/extend : drscheme:get/extend^)
                                    (unit drscheme:language-tower : drscheme:language-tower^)
                                    (unit drscheme:language : drscheme:language^)))
                  
                  (set! successful-tools 
                        (cons (make-successful-tool tool-path tool-bitmap name)
                              successful-tools))))))))

      ;; show-error : string exn -> void
      (define (show-error title x)
        (parameterize ([drscheme:init:error-display-handler-message-box-title
                        title])
          ((error-display-handler)
           (if (exn? x)
               (exn-message x)
               (format "uncaught exception: ~s" x))
           x)))


      ;; install-tool-bitmap : module-path -> bitmap
      ;; adds the tool's bitmap to the splash screen
      (define (install-tool-bitmap bitmap-path)
        (let/ec k
          (let ([bitmap
                 (with-handlers ([not-break-exn? (lambda (x) (k (void)))])
                   (make-object bitmap%
                     (build-path (build-path (apply collection-path (cdr bitmap-path)) (car bitmap-path)))))])
            (unless (and (is-a? bitmap bitmap%)
                         (send bitmap ok?))
              (k #f))
            (let-values ([(splash-eventspace splash-bitmap splash-canvas)
                          (with-handlers ([not-break-exn? (lambda (x) (k (void)))])
                            (values
                             (namespace-variable-binding 'splash-eventspace)
                             (namespace-variable-binding 'splash-bitmap)
                             (namespace-variable-binding 'splash-canvas)))])
              
              (unless (and (eventspace? splash-eventspace)
                           (is-a? splash-bitmap bitmap%)
                           (send splash-bitmap ok?)
                           (is-a? splash-canvas canvas%))
                (k (void)))
              
              (let ([bdc (make-object bitmap-dc%)])
                
                ;; truncate the bitmap, if necessary
                (unless (and (= tool-bitmap-size (send bitmap get-width))
                             (= tool-bitmap-size (send bitmap get-height)))
                  (let ([new-b (make-object bitmap% tool-bitmap-size tool-bitmap-size #f)])
                    (send bdc set-bitmap new-b)
                    (send bdc clear)
                    (send bdc draw-bitmap bitmap 
                          (max 0 (- (/ tool-bitmap-size 2)
                                    (/ (send bitmap get-width) 2)))
                          (max 0 (- (/ tool-bitmap-size 2)
                                    (/ (send bitmap get-height) 2))))
                    (send bdc set-bitmap #f)
                    (set! bitmap new-b)))
                
                (parameterize ([current-eventspace splash-eventspace])
                  (queue-callback
                   (lambda ()
                     (send bdc set-bitmap splash-bitmap)
                     (send bdc draw-bitmap bitmap 
                           tool-bitmap-x 
                           (max 0 (- (send splash-bitmap get-height) tool-bitmap-y tool-bitmap-size)))
                     (send bdc set-bitmap #f)
                     (send splash-canvas on-paint)
                     (if ((+ tool-bitmap-x tool-bitmap-gap tool-bitmap-size) . > . (send splash-bitmap get-width))
                         (begin
                           (set! tool-bitmap-y (+ tool-bitmap-y tool-bitmap-size tool-bitmap-gap))
                           (set! tool-bitmap-x tool-bitmap-gap))
                         (set! tool-bitmap-x (+ tool-bitmap-x tool-bitmap-size tool-bitmap-gap)))
                     (when (tool-bitmap-y . > . (send splash-bitmap get-width))
                       (set! tool-bitmap-y tool-bitmap-gap)))))))
            bitmap)))
      
      (define tool-bitmap-gap 4)
      (define tool-bitmap-x tool-bitmap-gap)
      (define tool-bitmap-y tool-bitmap-gap)
      (define tool-bitmap-size 32)
      
      ;; initializes the collection hash-table
      (for-each add-collections-in-path (current-library-collection-paths))
      
      ;; loads the the tools in each collection
      (for-each load/invoke-tools
                (quicksort
                 (hash-table-map collections-hash-table (lambda (sym str) str))
                 string<=?)))))
