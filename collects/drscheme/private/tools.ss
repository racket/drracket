(module tools mzscheme
  (require (lib "unitsig.ss")
           (lib "getinfo.ss" "setup")
           (lib "mred.ss" "mred")
           "drsig.ss")
  
  (provide tools@)
  (define tools@
    (unit/sig ()
      (import [drscheme:frame^ : drscheme:frame^]
              [drscheme:unit^ : drscheme:unit^]
              [drscheme:rep : drscheme:rep^]
              [drscheme:get/extend : drscheme:get/extend^])
      
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
        (let ([table (with-handlers ([not-break-exn? (lambda (x) #f)])
                       (get-info (list coll)))])
          (when table
            (let ([tools (table 'tools (lambda () null))])
              (for-each (load/invoke-tool coll) tools)))))
      
      ;; load/invoke-tool : string[collection-name] -> (listof string[sub-collection-names]) -> void
      (define (load/invoke-tool coll)
        (lambda (in-path)
          (let/ec k
            (unless (or (string? in-path)
                        (and (list? in-path)
                             ((length in-path) . >= . 1)
                             (andmap string? in-path)))
              (message-box "tool in collection ~a; invalid tool spec ~e"
                           coll in-path))
              (k (void))
            (let* ([path (if (string? in-path) (list in-path) in-path)]
                   [unit 
                    (with-handlers ([not-break-exn?
                                     (lambda (x)
                                       (message-box 
                                        (format "Error loading tool ~s; ~s" coll path)
                                        (if (exn? x)
                                            (exn-message x)
                                            (format "~s" x)))
                                       (k (void)))])
                      (dynamic-require `(lib ,(car path) ,coll ,@(cdr path)) 'tool@))])
              (with-handlers ([not-break-exn?
                               (lambda (x)
                                 (message-box "Error invoking tool ~s;~s" coll path)
                                 (if (exn? x)
                                     (exn-message x)
                                     (format "~s" x)))])
                (invoke-unit/sig unit
                                 [drscheme:frame^ : drscheme:frame^]
                                 [drscheme:unit^ : drscheme:unit^]
                                 [drscheme:rep : drscheme:rep^]
                                 [drscheme:get/extend : drscheme:get/extend^]))))))              
      
      ;; initializes the collection hash-table
      (for-each add-collections-in-path (current-library-collection-paths))
      
      ;; loads the the tools in each collection
      (hash-table-for-each
       collections-hash-table
       (lambda (sym str) (load/invoke-tools str))))))
