
(module module-language mzscheme
  (provide module-language@)
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "embed.ss" "compiler")
           (lib "launcher.ss" "launcher")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants")
           "drsig.ss"
           (lib "contract.ss"))
  
  (define op (current-output-port))
  (define (oprintf . args) (apply fprintf op args))
  
  (define module-language@
    (unit/sig drscheme:module-language^
      (import [drscheme:language-configuration : drscheme:language-configuration/internal^]
              [drscheme:language : drscheme:language^]
              [drscheme:unit : drscheme:unit^]
              [drscheme:rep : drscheme:rep^])

      (define module-language<%> 
        (interface ()
          ))
      
      ;; add-module-language : -> void
      ;; adds the special module-only language to drscheme
      (define (add-module-language)
        (define module-language%
          (module-mixin
           ((drscheme:language:get-default-mixin)
            (drscheme:language:module-based-language->language-mixin
             (drscheme:language:simple-module-based-language->module-based-language-mixin
              drscheme:language:simple-module-based-language%)))))
        (drscheme:language-configuration:add-language
         (instantiate module-language% ())))
      
      ;; collection-paths : (listof (union 'default string))
      ;; command-line-args : (vectorof string)
      (define-struct (module-language-settings drscheme:language:simple-settings)
                     (collection-paths command-line-args))
      
      ;; module-mixin : (implements drscheme:language:language<%>)
      ;;             -> (implements drscheme:language:language<%>)
      (define (module-mixin %)
        (class* % (module-language<%>)
          (define/override (use-namespace-require/copy?) #t)
          (rename [super-on-execute on-execute]
                  [super-front-end/complete-program front-end/complete-program])
          (field [iteration-number 0])
          
          ;; config-panel : as in super class
          ;; uses drscheme:language:simple-module-based-language-config-panel
          ;; and adds a collection paths configuration to it.
          (define/override (config-panel parent)
            (module-language-config-panel parent))
          
          (rename [super-default-settings default-settings])
          (define/override (default-settings)
            (let ([super-defaults (super-default-settings)])
              (apply make-module-language-settings
                     (append
                      (vector->list (drscheme:language:simple-settings->vector super-defaults))
                      (list '(default)
                            #())))))
          
          ;; default-settings? : -> boolean
          (rename [super-default-settings? default-settings?])
          (define/override (default-settings? settings)
            (and (super-default-settings? settings)
                 (and (equal? (module-language-settings-collection-paths settings)
                              '(default))
                      (eq? (module-language-settings-command-line-args settings)
                           #()))))
          
          (rename [super-marshall-settings marshall-settings])
          (define/override (marshall-settings settings)
            (let ([super-marshalled (super-marshall-settings settings)])
              (list super-marshalled
                    (module-language-settings-collection-paths settings)
                    (module-language-settings-command-line-args settings))))
          
          (rename [super-unmarshall-settings unmarshall-settings])
          (define/override (unmarshall-settings marshalled)
            (and (pair? marshalled)
                 (pair? (cdr marshalled))
                 (pair? (cddr marshalled))
                 (null? (cdddr marshalled))
                 (list? (cadr marshalled))
                 (vector? (caddr marshalled))
                 (andmap string? (vector->list (caddr marshalled)))
                 (andmap (lambda (x) (or (string? x) (symbol? x)))
                         (cadr marshalled))
                 (let ([super (super-unmarshall-settings (car marshalled))])
                   (and super
                        (apply make-module-language-settings
                               (append 
                                (vector->list (drscheme:language:simple-settings->vector super))
                                (list (cadr marshalled)
                                      (caddr marshalled))))))))
          
          (define/override (on-execute settings run-in-user-thread)
            (set! iteration-number 0)
            (super-on-execute settings run-in-user-thread)
            (run-in-user-thread
             (lambda ()
               (current-command-line-arguments (module-language-settings-command-line-args settings))
               (let ([default (current-library-collection-paths)])
                 (current-library-collection-paths
                  (apply 
                   append
                   (map (lambda (x) (if (symbol? x)
                                        default
                                        (list x)))
                        (module-language-settings-collection-paths settings))))))))

          (define/override (get-one-line-summary)
            (string-constant module-language-one-line-summary))
          
          (define/override (get-style-delta) module-language-style-delta)
          
          (define/override (front-end/complete-program input settings teachpack-cache)
            (let ([super-thunk (super-front-end/complete-program input settings teachpack-cache)]
                  [filename (get-definitions-filename (drscheme:language:text/pos-text input))]
                  [module-name #f])
              (lambda ()
                (set! iteration-number (+ iteration-number 1))
                (let ([super-result (super-thunk)])
                  (cond
                    [(= iteration-number 1)
                     (if (eof-object? super-result)
                         (raise-syntax-error
                          'module-language
                          "the definitions window must contain a module")
                         (let-values ([(name new-module)
                                       (transform-module-to-export-everything
                                        filename
                                        (expand super-result)
                                        super-result)])
                           (set! module-name name)
                           new-module))]
                    [(= 2 iteration-number)
                     (if (eof-object? super-result)
                         (with-syntax ([name module-name])
                           (syntax (require name)))
                         (raise-syntax-error
                          'module-language
                          "there can only be one expression in the definitions window"
                          super-result))]
                    [else eof])))))
          
          ;; printer settings are just ignored here.
          (define/override (create-executable setting parent program-filename)
            (let* ([executable-specs (drscheme:language:create-executable-gui
                                      parent 
                                      program-filename
                                      #t
                                      #t)])
              (when executable-specs
                (let ([stand-alone? (eq? 'stand-alone (car executable-specs))]
                      [gui? (eq? 'mred (cadr executable-specs))]
                      [executable-filename (caddr executable-specs)])
                  (with-handlers ([not-break-exn?
                                   (lambda (x)
                                     (message-box
                                      (string-constant drscheme)
                                      (if (exn? x)
                                          (format "~a" (exn-message x))
                                          (format "uncaught exception: ~s" x))))])
                    (if stand-alone?
                        (let ([short-program-name (let-values ([(base name dir) (split-path program-filename)])
                                                    (cond
                                                      [(regexp-match #rx"(.*)\\...." name)
                                                       =>
                                                       cadr]
                                                      [(regexp-match #rx"(.*)\\..." name)
                                                       =>
                                                       cadr]
                                                      [(regexp-match #rx"(.*)\\.." name)
                                                       =>
                                                       cadr]
                                                      [else name]))])
                          (make-embedding-executable
                           executable-filename
                           gui?
                           #f ;; verbose?
                           (list (list #f `(file ,program-filename)))
                           null
                           null
                           (list (if gui? "-Zmvqe-" "-mvqe-")
                                 (format "~s" `(require ,(string->symbol short-program-name))))))
                        ((if gui? make-mred-launcher make-mzscheme-launcher)
                         (list "-mvqt-" program-filename)
                         executable-filename)))))))
          
          (super-instantiate ()
            (module '(lib "plt-mred.ss" "lang"))
            (language-position (list (string-constant professional-languages) "(module ...)"))
            (language-numbers (list -1000 1000)))))
      
      ;; module-language-config-panel : panel -> (case-> (-> settings) (settings -> void))
      (define (module-language-config-panel parent)
        (define new-parent 
          (instantiate vertical-panel% ()
            (parent parent)
            (alignment '(center center))
            (stretchable-height #f)
            (stretchable-width #f)))
        (define simple-case-lambda (drscheme:language:simple-module-based-language-config-panel new-parent))
        (define cp-panel (instantiate group-box-panel% ()
                           (parent new-parent)
                           (label (string-constant ml-cp-collection-paths))))
        
        (define args-panel (instantiate group-box-panel% ()
                             (parent new-parent)
                             (label (string-constant ml-command-line-arguments))))
        (define args-text-box (new text-field%
                                   (parent args-panel)
                                   (label #f)
                                   (init-value "#()")
                                   (callback void)))
        
        ;; data associated with each item in listbox : boolean
        ;; indicates if the entry is the default paths.
        (define lb (instantiate list-box% ()
                     (parent cp-panel)
                     (choices '("a" "b" "c"))
                     (label #f)
                     (callback (lambda (x y) (update-buttons)))))
        (define button-panel (instantiate horizontal-panel% ()
                               (parent cp-panel)
                               (alignment '(center center))
                               (stretchable-height #f)))
        (define add-button (make-object button% (string-constant ml-cp-add) button-panel
                             (lambda (x y) (add-callback))))
        (define add-default-button (make-object button% (string-constant ml-cp-add-default) button-panel
                                     (lambda (x y) (add-default-callback))))
        (define remove-button (make-object button% (string-constant ml-cp-remove) button-panel
                                (lambda (x y) (remove-callback))))
        (define raise-button (make-object button% (string-constant ml-cp-raise) button-panel
                               (lambda (x y) (raise-callback))))
        (define lower-button (make-object button% (string-constant ml-cp-lower) button-panel
                               (lambda (x y) (lower-callback))))
        
        (define (update-buttons)
          (let ([lb-selection (send lb get-selection)]
                [lb-tot (send lb get-number)])
            (send remove-button enable lb-selection)
            (send raise-button enable 
                  (and lb-selection
                       (not (= lb-selection 0))))
            (send lower-button enable 
                  (and lb-selection
                       (not (= lb-selection (- lb-tot 1)))))))
                
        (define (add-callback)
          (let ([dir (get-directory
                      (string-constant ml-cp-choose-a-collection-path)
                      (send parent get-top-level-window))])
            (when dir
              (send lb append dir #f)
              (update-buttons))))
        
        (define (add-default-callback)
          (cond
            [(has-default?)
             (message-box (string-constant drscheme)
                          (string-constant ml-cp-default-already-present)
                          (send parent get-top-level-window))]
            [else
             (send lb append (string-constant ml-cp-default-collection-path) #t)
             (update-buttons)]))
        
        ;; has-default? : -> boolean
        ;; returns #t if the `default' entry has already been added
        (define (has-default?)
          (let loop ([n (send lb get-number)])
            (cond
              [(= n 0) #f]
              [(send lb get-data (- n 1)) #t]
              [else (loop (- n 1))])))
        
        (define (remove-callback)
          (let ([to-delete (send lb get-selection)])
            (send lb delete to-delete)
            (unless (zero? (send lb get-number))
              (send lb set-selection (min to-delete
                                          (- (send lb get-number) 1))))
            (update-buttons)))
        
        (define (lower-callback)
          (let* ([sel (send lb get-selection)]
                 [vec (get-lb-vector)]
                 [below (vector-ref vec (+ sel 1))])
            (vector-set! vec (+ sel 1) (vector-ref vec sel))
            (vector-set! vec sel below)
            (set-lb-vector vec)
            (send lb set-selection (+ sel 1))
            (update-buttons)))
        
        (define (raise-callback) 
          (let* ([sel (send lb get-selection)]
                 [vec (get-lb-vector)]
                 [above (vector-ref vec (- sel 1))])
            (vector-set! vec (- sel 1) (vector-ref vec sel))
            (vector-set! vec sel above)
            (set-lb-vector vec)
            (send lb set-selection (- sel 1))
            (update-buttons)))
        
        (define (get-lb-vector)
          (list->vector
           (let loop ([n 0])
             (cond
               [(= n (send lb get-number)) null]
               [else (cons (cons (send lb get-string n)
                                 (send lb get-data n))
                           (loop (+ n 1)))]))))
        
        (define (set-lb-vector vec)
          (send lb clear)
          (let loop ([n 0])
            (cond
              [(= n (vector-length vec)) (void)]
              [else (send lb append (car (vector-ref vec n)))
                    (send lb set-data n (cdr (vector-ref vec n)))
                    (loop (+ n 1))])))
        
        (define (get-collection-paths)
          (let loop ([n 0])
            (cond
              [(= n (send lb get-number)) null]
              [else
               (let ([data (send lb get-data n)])
                 (cons (if data
                           'default
                           (send lb get-string n))
                       (loop (+ n 1))))])))
        
        (define (install-collection-paths paths)
          (send lb clear)
          (for-each (lambda (cp)
                      (if (symbol? cp)
                          (send lb append
                                (string-constant ml-cp-default-collection-path)
                                #t)
                          (send lb append cp #f)))
                    paths))
        
        (define (get-command-line-args)
          (let ([str (send args-text-box get-value)])
            (let ([read-res (parameterize ([read-accept-graph #f])
                              (with-handlers ([not-break-exn? 
                                               (lambda (x) #())])
                                (read (open-input-string str))))])
              (cond
                [(and (vector? read-res)
                      (andmap string? (vector->list read-res)))
                 read-res]
                [else #()]))))
        
        (define (install-command-line-args vec)
          (send args-text-box set-value 
                (parameterize ([print-vector-length #f])
                  (format "~s" vec))))
        
        (send lb set '())
        (update-buttons)
        
        (case-lambda
          [() 
           (let ([simple-settings (simple-case-lambda)])
             (apply make-module-language-settings
                    (append
                     (vector->list (drscheme:language:simple-settings->vector simple-settings))
                     (list (get-collection-paths)
                           (get-command-line-args)))))]
          [(settings) 
           (simple-case-lambda settings)
           (install-collection-paths (module-language-settings-collection-paths settings))
           (install-command-line-args (module-language-settings-command-line-args settings))
           (update-buttons)]))
      
      ;; module-language-style-delta : (instanceof style-delta%)
      (define module-language-style-delta (make-object style-delta% 'change-family 'modern))
      
      ;; transform-module-to-export-everything : (union #f string) syntax syntax -> syntax
      ;; in addition to exporting everything, the result module's name
      ;; is the fully expanded name, with a directory prefix, 
      ;; if the file has been saved
      (define (transform-module-to-export-everything filename stx unexpanded-stx)
        (syntax-case stx (module #%plain-module-begin)
          [(module name lang (#%plain-module-begin bodies ...))
           (let ([v-name (syntax name)])
             (when filename
               (check-filename-matches filename
                                       (syntax-object->datum (syntax name)) 
                                       unexpanded-stx))
             (with-syntax ([(to-provide-specs ...)
                            (get-provide-specs
                             (syntax module)
                             (cons
                              (syntax (require lang))
                              (syntax->list
                               (syntax (bodies ...)))))]
                           [(no-provide-bodies ...)
                            (filter
                             not-provide?
                             (syntax->list
                              (syntax (bodies ...))))])
               (values
                v-name
                (syntax (module name lang
                          (#%plain-module-begin 
                           (provide to-provide-specs ...)
                           no-provide-bodies ...))))))]
          [else
           (raise-syntax-error 'module-language
                               "only module expressions are allowed"
                               unexpanded-stx)]))
      
      ;; build-prefixed-module-name : string syntax -> symbol
      ;; builds the fully prefixed name of the module from the 
      ;; filename where the file is saved and name is what
      ;; the programmer put in the module definition.
      (define (build-prefixed-module-name filename module-name)
        (let-values ([(base name dir?) (split-path filename)])
          (string->symbol
           (format 
            ",~a" 
            (build-path
             base
             (symbol->string
              (syntax-object->datum module-name)))))))

      ;; get-definitions-filename : (union text% #f) -> (union string #f)
      ;; extracts the file the definitions window is being saved in, if any.
      (define (get-definitions-filename definitions-text)
        (and (is-a? definitions-text text%)
             (let ([canvas (send definitions-text get-canvas)])
               (and canvas
                    (let ([frame (send canvas get-top-level-window)])
                      (and (is-a? frame drscheme:unit:frame%)
                           (let* ([b (box #f)]
                                  [filename (send (send frame get-definitions-text)
                                                  get-filename
                                                  b)])
                             (if (unbox b)
                                 #f
                                 filename))))))))
      
      ;; check-filename-matches : string datum syntax -> void
      (define re:check-filename-matches (regexp "^(.*)\\.[^.]*$"))
      (define (check-filename-matches filename datum unexpanded-stx)
        (unless (symbol? datum)
          (raise-syntax-error 'module-language "unexpected object in name position of module" 
                              unexpanded-stx))
        (let-values ([(base name dir?) (split-path filename)])
          (let* ([m (regexp-match re:check-filename-matches name)]
                 [matches?
                  (if m
                      (equal? (string->symbol (cadr m)) datum)
                      (equal? (string->symbol name) datum))])
            (unless matches?
              (raise-syntax-error
               'module-language
               (format "module name doesn't match saved filename, ~s and ~e"
                       datum
                       filename)
               unexpanded-stx)))))
      
      ;; get-provide-specs : syntax (listof syntax) -> (listof syntax) 
      (define (get-provide-specs orig-stx bodies)
        (let ([required-specs (make-hash-table 'equal)])
          (let loop ([bodies bodies]
                     [vars null])
            (cond
              [(null? bodies) 
               (append vars
                       (hash-table-map
                        required-specs
                        (lambda (key value)
                          (with-syntax ([x value])
                            (syntax (all-from x))))))]
              [else
               (let ([body (car bodies)]
                     [loop-with-ids
                      (lambda (ids)
                        (loop (cdr bodies)
                              (append (filter 
				       ;; When a `define-values' or `define-syntax' declaration
				       ;; is macro-generated, if the defined name also originates
				       ;; from a macro, then the name is hidden to anything
				       ;; that wasn't generated by the same macro invocation. This
				       ;; hiding relies on renaming at the symbol level, and it's
				       ;; exposed by the fact that `syntax-e' of the identifier 
				       ;; returns a different name than `identifier-binding'.
				       (lambda (id)
					 (let ([ib (identifier-binding id)])
					   ;; ib should always be a 4-elem list, but
					   ;; check, just in case:
					   (or (not (pair? ib)) 
					       (eq? (syntax-e id)
						    (cadr ib)))))
				       (syntax->list ids))
                                      vars)))])
                 (syntax-case body (define-values define-syntaxes require)
                   [(define-values (new-vars ...) body)
                    (loop-with-ids (syntax (new-vars ...)))]
                   [(define-syntaxes (new-vars ...) body)
                    (loop-with-ids (syntax (new-vars ...)))]
                   [(require specs ...)
                    (for-each (lambda (spec)
                                (syntax-case spec (prefix all-except rename)
                                  [(prefix identifier module-name) 
                                   (hash-table-put! required-specs
                                                    (syntax-object->datum (syntax module-name)) 
                                                    (syntax module-name))]
                                  [(all-except module-name identifer ...) 
                                   (hash-table-put! required-specs
                                                    (syntax-object->datum (syntax module-name))
                                                    (syntax module-name))]
                                  [(rename module-name local-identifer exported-identifer)
                                   (hash-table-put! required-specs 
                                                    (syntax-object->datum (syntax module-name))
                                                    (syntax module-name))]
                                  [module-name
                                   (hash-table-put! required-specs 
                                                    (syntax-object->datum (syntax module-name))
                                                    (syntax module-name))]))
                              (syntax->list (syntax (specs ...))))
                    (loop (cdr bodies) vars)]
                   [else 
                    (loop (cdr bodies)
                          vars)]))]))))
      
      ;; maybe-add : syntax (listof module-spec-sym) -> (listof module-spec-sym)
      (define (maybe-add syntax-module-spec other-specs)
        (let ([module-spec-sym ((current-module-name-resolver) 
                                (syntax-object->datum syntax-module-spec)
                                #f
                                #f)])
          (if (memq module-spec-sym other-specs)
              other-specs
              (cons module-spec-sym other-specs))))
      
      ;; not-provide? : syntax -> boolean
      ;; returns #t if the expression is not a provide expression, #f otherwise.
      (define (not-provide? body)
        (syntax-case body (provide)
          [(provide provide-specs ...)
           #f]
          [_ 
           #t]))
      
      ;; extract-provided-vars : (listof syntax) -> (listof syntax)
      ;; extracts the names of the variables mentioned in the provide specification
      (define (extract-provided-vars provide-specs)
        (let loop ([provide-specs provide-specs])
          (cond
            [(null? provide-specs) null]
            [else
             (syntax-case (car provide-specs) (rename struct all-from all-from-except all-defined all-defined-except)
               [(rename local-identifier export-identifier)
                (cons (syntax local-identifier) (loop (cdr provide-specs)))]
               [(struct struct-identifier (field-identifier ...)) (loop (cdr provide-specs))]
               [(all-from module-name) (loop (cdr provide-specs))]
               [(all-from-except module-name identifer ...) (loop (cdr provide-specs))]
               [(all-defined) (loop (cdr provide-specs))]
               [(all-defined-except identifier ...) (loop (cdr provide-specs))]
               [id (identifier? (syntax id)) (cons (syntax id) (loop (cdr provide-specs)))]
               [_ (loop (cdr provide-specs))])])))

      (define module-language-put-file-mixin
        (mixin (text:basic<%>) ()
          (inherit get-text last-position get-character get-top-level-window)
          (rename [super-put-file put-file])
          (define/override (put-file directory default-name)
            (let ([tlw (get-top-level-window)])
              (if (and tlw 
                       (is-a? tlw drscheme:unit:frame<%>))
                  (let* ([definitions-text (send tlw get-definitions-text)]
                         [module-language? 
                          (is-a? (drscheme:language-configuration:language-settings-language
                                  (send definitions-text get-next-settings))
                                 module-language<%>)]
                         [module-default-filename
                          (and module-language? (get-module-filename))])
                    (super-put-file directory module-default-filename))
                  (super-put-file directory default-name))))
      
          ;; returns the name after "(module " suffixed with .scm
          ;; in the beginning of the editor
          ;; or #f if the beginning doesn't match "(module "
          (define/contract get-module-filename
            (-> (union false? string?))
            (lambda ()
              (let ([open-paren (skip-whitespace 0)])
                (or (match-paren open-paren "(")
                    (match-paren open-paren "[")
                    (match-paren open-paren "{")))))
          
          (define/contract match-paren
            (number? string? . -> . (union false? string?))
            (lambda (open-paren paren)
              (and (matches open-paren paren)
                   (let ([module (skip-whitespace (+ open-paren 1))])
                     (and (matches module "module")
                          (let* ([end-module (+ module (string-length "module"))]
                                 [filename-start (skip-whitespace end-module)]
                                 [filename-end (skip-to-whitespace filename-start)])
                            (and (not (= filename-start end-module))
                                 (string-append (get-text filename-start filename-end)
                                                ".scm"))))))))
          

          (define/contract matches
            (number? string? . -> . boolean?)
            (lambda (start string)
              (let ([last-pos (last-position)])
                (let loop ([i 0])
                  (cond
                    [(and (i . < . (string-length string))
                          ((+ i start) . < . last-pos))
                     (and (char=? (string-ref string i)
                                  (get-character (+ i start)))
                          (loop (+ i 1)))]
                    [(= i (string-length string)) #t]
                    [else #f])))))
          
          (define/contract skip-whitespace
            (number? . -> . number?)
            (lambda (start) 
              (let ([last-pos (last-position)])
                (let loop ([pos start]
                           [comment-depth 0])
                  (cond
                    [(pos . >= . last-pos) last-pos]
                    [else 
                     (let ([char (get-character pos)])
                       (cond
                         [(char-whitespace? char)
                          (loop (+ pos 1))]
                         [else pos]))])))))

          (define/contract skip-to-whitespace
            (number? . -> . number?)
            (lambda (start) 
              (let ([last-pos (last-position)])
                (let loop ([pos start])
                  (cond
                    [(pos . >= . last-pos)
                     last-pos]
                    [(char-whitespace? (get-character pos))
                     pos]
                    [else
                     (loop (+ pos 1))])))))
          
          (super-instantiate ()))))))