
(module module-language mzscheme
  (provide module-language@)
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           "drsig.ss")
  
  (define module-language@
    (unit/sig drscheme:module-language^
      (import [drscheme:language-configuration : drscheme:language-configuration/internal^]
              [drscheme:language : drscheme:language^]
              [drscheme:unit : drscheme:unit^]
              [drscheme:rep : drscheme:rep^])
      
      ;; add-module-language : -> void
      ;; adds the special module-only language to drscheme
      (define (add-module-language)
        (drscheme:language-configuration:add-language
         (instantiate module-language% ())))
      
      ;; module-mixin : (implements drscheme:language:language<%>)
      ;;             -> (implements drscheme:language:language<%>)
      (define (module-mixin %)
        (class %
          (define/override (use-namespace-require/copy?) #t)
          (rename [super-on-execute on-execute]
                  [super-front-end front-end])
          (field [iteration-number 0])
          
          (define/override (on-execute settings run-in-user-thread)
            (set! iteration-number 0)
            (super-on-execute settings run-in-user-thread))
          
          (define/override (get-style-delta) module-language-style-delta)
          
          (define/override (front-end input settings)
            (if (and (drscheme:language:text/pos? input)
                     (is-a? (drscheme:language:text/pos-text input)
                            drscheme:unit:definitions-text%))
                (let ([super-thunk (super-front-end input settings)]
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
                        [else eof]))))
                (super-front-end input settings)))
          (super-instantiate ()
            (module '(lib "plt-mred.ss" "lang"))
            (language-position (list "module")))))
      
      ;; module-language-style-delta : (instanceof style-delta%)
      (define module-language-style-delta (make-object style-delta% 'change-family 'modern))
      
      ;; transform-module-to-export-everything : syntax syntax -> syntax
      ;; in addition to exporting everything, the result module's name
      ;; is the fully expanded name, with a directory prefix, 
      ;; if the file has been saved
      (define (transform-module-to-export-everything stx unexpanded-stx)
        (syntax-case stx (module #%plain-module-begin)
          [(module name lang (#%plain-module-begin bodies ...))
           (let ([filename (get-definitions-filename)])
             (when filename
               (check-filename-matches filename
                                       (syntax-object->datum (syntax name)) 
                                       unexpanded-stx))
             (let ([prefixed-name (if filename
                                      (build-prefixed-module-name filename (syntax name))
                                      (syntax name))])
               (with-syntax ([s-prefixed-name prefixed-name]
                             [(to-provide-specs ...)
                              (cons
                               (syntax (all-from lang))
                               (get-provide-specs
                                (syntax->list
                                 (syntax (bodies ...)))))]
                             [(no-provide-bodies ...)
                              (filter
                               not-provide?
                               (syntax->list
                                (syntax (bodies ...))))])
                 (values
                  prefixed-name
                  (syntax (module s-prefixed-name lang
                            (#%plain-module-begin 
                             (provide to-provide-specs ...)
                             no-provide-bodies ...)))))))]
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

      ;; get-definitions-filename : -> (union string #f)
      ;; extracts the file the definitions window is being saved in, if any.
      (define (get-definitions-filename)
        (let ([rep (drscheme:rep:current-rep)])
          (and rep
               (let ([canvas (send rep get-canvas)])
                 (and canvas
                      (let ([frame (send canvas get-top-level-window)])
                        (and (is-a? frame drscheme:unit:frame%)
                             (let* ([b (box #f)]
                                    [filename (send (send frame get-definitions-text)
                                                    get-filename
                                                    b)])
                               (if (unbox b)
                                   #f
                                   filename)))))))))
      
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
      
      ;; get-provide-spec : syntax -> (union (listof syntax) #f)
      ;; given a top-level module expression, returns #f if it
      ;; doesn't indtrouce any identifiers to the top-level scope
      ;; of the modules, or, if it does, returns a list of syntax
      ;; corresponding to the argument to `provide' to export those
      ;; definitions.
      (define (get-provide-specs bodies)
        (let loop ([bodies bodies]
                   [vars null]
                   [module-specs null]
                   [module-syms null])
          (let ([module-loop
                 (lambda (syntax-module-spec specs)
                   (let* ([module-spec (syntax-object->datum syntax-module-spec)]
                          [module-sym 
                           (if (symbol? module-spec)
                               module-spec
                               ((current-module-name-resolver) module-spec #f #f))]
                          [next-bodies
                           (cons (with-syntax ([(specs ...) specs])
                                   (syntax (require specs ...)))
                                 (cdr bodies))])
                     (if (memq module-sym module-syms)
                         (loop next-bodies
                               vars
                               module-specs
                               module-syms)
                         (loop next-bodies
                               vars
                               (cons syntax-module-spec module-specs)
                               (cons module-sym module-syms)))))])
            (cond
              [(null? bodies) 
               (append vars
                       (map (lambda (x)
                              (with-syntax ([x x])
                                (syntax (all-from x))))
                            module-specs))]
              [else
               (let ([body (car bodies)])
                 (syntax-case body (define-values define-syntaxes require prefix all-except rename)
                   [(define-values (new-vars ...) body)
                    (loop (cdr bodies)
                          (append (syntax->list (syntax (new-vars ...)))
                                  vars)
                          module-specs
                          module-syms)]
                   [(define-syntaxes (new-vars ...) body)
                    (loop (cdr bodies)
                          (append (syntax->list (syntax (new-vars ...)))
                                  vars)
                          module-specs
                          module-syms)]
                   [(require (prefix identifier module-name) specs ...) 
                    (module-loop (syntax module-name) (syntax (specs ...)))]
                   [(require (all-except module-name identifer ...) specs ...) 
                    (module-loop (syntax module-name) (syntax (specs ...)))]
                   [(require (rename module-name local-identifer exported-identifer) specs ...)
                    (module-loop (syntax module-name) (syntax (specs ...)))]
                   [(require module-name specs ...)
                    (module-loop (syntax module-name) (syntax (specs ...)))]
                   [(require)
                    (loop (cdr bodies)
                          vars
                          module-specs
                          module-syms)]
                   [else 
                    (loop (cdr bodies)
                          vars
                          module-specs
                          module-syms)]))]))))
      
      ;; maybe-add : syntax (listof module-spec-sym) -> (listof module-spec-sym)
      (define (maybe-add syntax-module-spec other-specs)
        (let ([module-spec-sym ((current-module-name-resolver) 
                                (syntax-object->datum syntax-module-spec)
                                #f
                                #f)])
          (if (memq module-spec-sym other-specs)
              other-specs
              (cons module-spec-sym other-specs))))
      
      ;; extract-provided-vars : (listof syntax) -> (listof syntax[identifier])
      (define (not-provide? body)
        (syntax-case body (provide)
          [(provide vars ...)
           #f]
          [_ #t]))
      
      ;; module-language : (implements drscheme:language:language<%>)
      (define module-language%
        (module-mixin
         (drscheme:language:module-based-language->language-mixin
          (drscheme:language:simple-module-based-language->module-based-language-mixin
           drscheme:language:simple-module-based-language%)))))))
