
(module tools mzscheme
  (require (lib "unitsig.ss")
           (lib "getinfo.ss" "setup")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           "drsig.ss"
           (lib "contract.ss")
           "tool-contracts.ss"
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants"))
  
  (provide tools@)  

  (define tools@
    (unit/sig drscheme:tools^
      (import [drscheme:frame : drscheme:frame^]
              [drscheme:unit : drscheme:unit^]
              [drscheme:rep : drscheme:rep^]
              [drscheme:get/extend : drscheme:get/extend^]
              [drscheme:language : drscheme:language^]
              [drscheme:language-configuration : drscheme:language-configuration^]
	      [drscheme:help-desk : drscheme:help-desk^]
              [drscheme:init : drscheme:init^]
              [drscheme:number-snip : drscheme:number-snip^]
	      [drscheme:debug : drscheme:debug^]
              [drscheme:eval : drscheme:eval^]
              [drscheme:teachpack : drscheme:teachpack^]
              [drscheme:modes : drscheme:modes^])
      
      ;; successful-tool = (make-successful-tool module-spec 
      ;;                                         (union #f (instanceof bitmap%))
      ;;                                         (union #f string)
      ;;                                         (union #f string))
      (define-struct successful-tool (spec bitmap name url))
      
      ;; successful-tools : (listof successful-tool)
      (define successful-tools null)

      ;; get-successful-tools : -> (listof successful-tool)
      (define (get-successful-tools) successful-tools)

      ;; successfully-loaded-tool = 
      ;; (make-successfully-loaded-tool 
      ;;    module-spec (union #f (instanceof bitmap%)) (union #f string) (union #f string)
      ;;    (-> void) (-> void))
      (define-struct successfully-loaded-tool (spec bitmap name url phase1 phase2))
                     
      ;; successfully-loaded-tools : (listof successfully-loaded-tool)
      ;; this list contains the tools that successfully were loaded
      ;; it is updated in load/invoke-tool. 
      (define successfully-loaded-tools null)

      ;; load/invoke-all-tools : -> void
      (define (load/invoke-all-tools phase1-extras phase2-extras)
        (set! current-phase 'loading-tools)
        (load/invoke-all-tools/collections
         (drscheme:init:all-toplevel-collections)
         phase1-extras
         phase2-extras))

      ;; loads the the tools in each collection
      ;; unless PLTNOTOOLS is set, in which case it
      ;; just runs the phases. If PLTONLYTOOL is set,
      ;; it only loads tools in those collections
      (define (load/invoke-all-tools/collections collections phase1-extras phase2-extras)
        (cond
          [(getenv "PLTNOTOOLS") (printf "PLTNOTOOLS: skipping tools\n")]
          [else
           (let ([onlys (getenv "PLTONLYTOOL")])
             (if onlys
                 (let* ([allowed (let ([exp (read (open-input-string onlys))])
                                   (cond 
                                     [(symbol? exp) (list exp)]
                                     [(pair? exp) exp]
                                     [else '()]))]
                        [filtered (filter (lambda (x) (memq (string->symbol x) allowed)) collections)])
                   (printf "PLTONLYTOOL: only loading ~s\n" filtered)
                   (for-each load/invoke-tools filtered))
                 (for-each load/invoke-tools collections)))])
        (run-phases phase1-extras phase2-extras))

      
                                                                             
 ;;;                     ;;       ;   ;                        ;;            
   ;                      ;      ;                              ;            
   ;                      ;      ;                              ;            
   ;     ;;;   ;;;;    ;;;;     ;   ;;;   ; ;;;  ;;; ;;;  ;;;   ;  ;;   ;;;  
   ;    ;   ;      ;  ;   ;     ;     ;    ;;  ;  ;   ;  ;   ;  ; ;    ;   ; 
   ;    ;   ;   ;;;;  ;   ;    ;      ;    ;   ;  ;   ;  ;   ;  ;;     ;;;;; 
   ;    ;   ;  ;   ;  ;   ;    ;      ;    ;   ;   ; ;   ;   ;  ; ;    ;     
   ;    ;   ;  ;   ;  ;   ;   ;       ;    ;   ;   ;;;   ;   ;  ;  ;   ;   ; 
 ;;;;;;  ;;;    ;;; ;  ;;; ;  ;     ;;;;; ;;;  ;;   ;     ;;;  ;;   ;;  ;;;  
                             ;                                               
                                                                             
                                                                             

      
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
                   [tool-names (table 'tool-names (lambda () (map (lambda (x) #f) tools)))]
                   [tool-urls (table 'tool-urls (lambda () (map (lambda (x) #f) tools)))])
              (unless (= (length tools) (length tool-icons))
                (message-box (string-constant drscheme)
                             (format (string-constant tool-tool-icons-same-length)
                                     coll tools tool-icons)
                             #f
                             '(ok stop))
                (set! tool-icons (map (lambda (x) #f) tools)))
              (unless (= (length tools) (length tool-names))
                (message-box (string-constant drscheme)
                             (format (string-constant tool-tool-names-same-length)
                                     coll tools tool-names)
                             #f
                             '(ok stop))
                (set! tool-names (map (lambda (x) #f) tools)))
              (unless (= (length tools) (length tool-urls))
                (message-box (string-constant drscheme)
                             (format (string-constant tool-tool-urls-same-length)
                                     coll tools tool-urls)
                             #f
                             '(ok stop))
                (set! tool-urls (map (lambda (x) #f) tools)))
              (for-each (load/invoke-tool coll) tools tool-icons tool-names tool-urls)))))
      
      ;; load/invoke-tool :    string[collection-name] 
      ;;                    -> (listof string[sub-collection-name]) 
      ;;                       (union #f (cons string[filename] (listof string[collection-name])))
      ;;                       (union #f string)
      ;;                       (union #f string)
      ;;                    -> void
      ;; `coll' is a collection to load the tool from
      ;; `in-path' is the `coll'-relative collection-path spec for the tool module file
      ;; `icon-spec' is the collection-path spec for the tool's icon, if there is one.
      ;; `name' is the name of the tool (only used in about box)
      (define (load/invoke-tool coll)
        (lambda (in-path icon-spec name tool-url)
          (let ([tool-bitmap
                 (and icon-spec
                      (install-tool-bitmap name icon-spec))])
            (let/ec k
              (unless (or (string? in-path)
                          (and (list? in-path)
                               (not (null? in-path))
                               (andmap string? in-path)))
                (message-box (string-constant drscheme)
                             (format (string-constant invalid-tool-spec)
                                     coll in-path)
                             #f
                             '(ok stop))
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
                  (let-values ([(phase1-thunk phase2-thunk) 
                                (invoke-tool unit (string->symbol (or name coll)))])
                    (set! successfully-loaded-tools 
                          (cons (make-successfully-loaded-tool
                                 tool-path
                                 tool-bitmap
                                 name
                                 tool-url 
                                 phase1-thunk
                                 phase2-thunk)
                                successfully-loaded-tools)))))))))

      ;; invoke-tool : unit/sig string -> (values (-> void) (-> void))
      ;; invokes the tools and returns the two phase thunks.
      (define (invoke-tool unit tool-name)
	(wrap-tool-inputs 
         (let ()
           (define-values/invoke-unit/sig drscheme:tool-exports^ unit #f drscheme:tool^)
           (values phase1 phase2))
         tool-name))

      ;; show-error : string (union exn TST) -> void
      (define (show-error title x)
        (parameterize ([drscheme:init:error-display-handler-message-box-title
                        title])
          ((error-display-handler)
           (if (exn? x)
               (format "~a" (exn-message x))
               (format "uncaught exception: ~s" x))
           x)))


      ;; install-tool-bitmap : string module-path -> bitmap
      ;; adds the tool's bitmap to the splash screen
      (define (install-tool-bitmap name bitmap-path)
        (let/ec k
          (let ([bitmap
                 (with-handlers ([not-break-exn? (lambda (x) (k (void)))])
                   (make-object bitmap%
                     (build-path (build-path (apply collection-path (cdr bitmap-path)) (car bitmap-path)))
		     'unknown/mask))])
            (unless (and (is-a? bitmap bitmap%)
                         (send bitmap ok?))
              (k #f))
            (let ([splash-eventspace ((dynamic-require '(lib "splash.ss" "framework") 'get-splash-eventspace))]
                  [splash-bitmap ((dynamic-require '(lib "splash.ss" "framework") 'get-splash-bitmap))]
                  [splash-canvas ((dynamic-require '(lib "splash.ss" "framework") 'get-splash-canvas))])
              
              (unless (and (eventspace? splash-eventspace)
                           (is-a? splash-bitmap bitmap%)
                           (send splash-bitmap ok?)
                           (is-a? splash-canvas canvas%))
                (k (void)))
              
                (parameterize ([current-eventspace splash-eventspace])
                  (queue-callback
                   (lambda ()
                     (let ([bdc (make-object bitmap-dc%)]
                           [translated-tool-bitmap-y (max 0 (- (send splash-bitmap get-height) tool-bitmap-y tool-bitmap-size))])
                       
                       ;; truncate/expand the bitmap, if necessary
                       (unless (and (= tool-bitmap-size (send bitmap get-width))
                                    (= tool-bitmap-size (send bitmap get-height)))
                         (let ([new-b (make-object bitmap% tool-bitmap-size tool-bitmap-size #f)])
                           (send bdc set-bitmap new-b)
                           (send bdc clear)
                           (send bdc draw-bitmap-section splash-bitmap 
                                 0 0 
                                 tool-bitmap-x translated-tool-bitmap-y
                                 tool-bitmap-size tool-bitmap-size)
                           (send bdc draw-bitmap bitmap 
                                 (max 0 (- (/ tool-bitmap-size 2)
                                           (/ (send bitmap get-width) 2)))
                                 (max 0 (- (/ tool-bitmap-size 2)
                                           (/ (send bitmap get-height) 2)))
				 'solid
				 (make-object color% "black")
				 (send bitmap get-loaded-mask))
                           (send bdc set-bitmap #f)
                           (set! bitmap new-b)))
                       
                       ((dynamic-require '(lib "splash.ss" "framework") 'add-splash-icon)
                        bitmap tool-bitmap-x translated-tool-bitmap-y)
                       (set! tool-bitmap-x (+ tool-bitmap-x tool-bitmap-size tool-bitmap-gap))
                       (when ((+ tool-bitmap-x tool-bitmap-gap tool-bitmap-size) . > . (send splash-bitmap get-width))
                         (set! tool-bitmap-y (+ tool-bitmap-y tool-bitmap-size tool-bitmap-gap))
                         (set! tool-bitmap-x tool-bitmap-gap))
                       (when ((+ tool-bitmap-y tool-bitmap-gap tool-bitmap-size) . > . (send splash-bitmap get-width))
                         (set! tool-bitmap-y tool-bitmap-gap)))))))
            bitmap)))
      
      (define tool-bitmap-gap 3)
      (define tool-bitmap-x tool-bitmap-gap)
      (define tool-bitmap-y tool-bitmap-gap)
      (define tool-bitmap-size 32)

      
                                                                      
       ;;                             ;                          ;;;  
        ;                           ;;;            ;;;          ;   ; 
        ;                             ;           ;  ;          ;   ; 
; ;;;   ; ;;   ;;;;    ;;;    ;;;     ;           ;             ;   ; 
 ;   ;  ;;  ;      ;  ;   ;  ;   ;    ;            ;   ;           ;  
 ;   ;  ;   ;   ;;;;   ;;;   ;;;;;    ;           ;;; ;           ;   
 ;   ;  ;   ;  ;   ;      ;  ;        ;          ;   ;           ;    
 ;   ;  ;   ;  ;   ;  ;   ;  ;   ;    ;          ;   ;;         ;   ; 
 ;;;;  ;;; ;;;  ;;; ;  ;;;    ;;;   ;;;;;         ;;;  ;        ;;;;; 
 ;                                                                    
 ;                                                                    
;;;                                                                   


      ;; run-phases : -> void
      (define (run-phases phase1-extras phase2-extras)
        (let* ([after-phase1 (run-one-phase 'phase1
                                            (string-constant tool-error-phase1)
                                            successfully-loaded-tool-phase1
                                            successfully-loaded-tools
                                            phase1-extras)]
               [after-phase2 (run-one-phase 'phase2
                                            (string-constant tool-error-phase2)
                                            successfully-loaded-tool-phase2
                                            after-phase1
                                            phase2-extras)])
          (set! current-phase 'init-complete)
          (set! successful-tools
                (map (lambda (x) (make-successful-tool
                                  (successfully-loaded-tool-spec x)
                                  (successfully-loaded-tool-bitmap x)
                                  (successfully-loaded-tool-name x)
                                  (successfully-loaded-tool-url x)))
                     after-phase2))))
      
      ;; run-one-phase : string 
      ;;                 (successfully-loaded-tool -> (-> void))
      ;;                 (listof successfully-loaded-tool)
      ;;                 (-> void)
      ;;              -> (listof successfully-loaded-tool)
      ;; filters out the tools that raise exceptions during the phase.
      ;; extras is the thunk for DrScheme init stuff on this phase.
      (define (run-one-phase _the-phase err-fmt selector tools extras)
        (set! current-phase _the-phase)
        (extras)
        (let loop ([tools tools])
          (cond
            [(null? tools) null]
            [else 
             (let ([tool (car tools)])
               (let ([phase-thunk (selector tool)])
                 (with-handlers ([not-break-exn?
                                  (lambda (exn) 
                                    (show-error
                                     (format err-fmt 
                                             (successfully-loaded-tool-spec tool)
                                             (successfully-loaded-tool-name tool))
                                     exn)
                                    (loop (cdr tools)))])
                   (phase-thunk)
                   (cons tool (loop (cdr tools))))))])))
      
      ;; current-phase : (union #f 'loading-tools 'phase1 'phase2 'init-complete)
      (define current-phase #f)
      (define (get-current-phase) current-phase)
      
      ;; only-in-phase : sym (union #f 'loading-tools 'phase1 'phase2 'init-complete) ... -> void
      ;; raises an error unless one of `phases' is the current phase
      (define (only-in-phase func . phases)
        (unless (memq current-phase phases)
          (error func "can only be called in phase: ~a"
                 (apply string-append 
                        (map (lambda (x) (format "~e " x))
                             (filter (lambda (x) x) phases)))))))))
