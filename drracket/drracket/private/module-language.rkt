#lang racket/base

(provide module-language@)
(require racket/unit
         racket/class
         racket/list
         racket/contract
         racket/sandbox
         racket/runtime-path
         racket/math
         racket/match
         racket/set
         racket/place
         racket/gui/base
         compiler/embed
         compiler/cm
         launcher
         framework
         framework/private/srcloc-panel
         string-constants
         racket/place
         mrlib/close-icon
         mrlib/name-message
         "tooltip.rkt"
         drracket/private/drsig
         "rep.rkt"
         "eval-helpers-and-pref-init.rkt"
         "local-member-names.rkt"
         (prefix-in lmn: "local-member-names.rkt")
         "insulated-read-language.rkt"
         drracket/private/rectangle-intersect
         pkg/lib
         pkg/gui
         mrlib/panel-wob
         framework/private/logging-timer
         (submod "frame.rkt" install-pkg)
         (for-syntax racket/base)
         (prefix-in 2: (combine-in 2htdp/image
                                   (only-in mrlib/image-core
                                            render-image))))

(struct exn-info (str full-str src-vecs exn-stack missing-mods) #:prefab)

;; submodule to make these accessible to the test suite
(module oc-status-structs racket/base
  ;; the online compilation state for individual tabs
  ;; oc-state is either:
  #; 
  (clean (or/c symbol? #f)
         (or/c (non-empty-listof
                (exn-info string?
                          string?
                          (listof (vector number? number?))
                          (listof string?)
                          (or/c #f module-path?)))
               #f)
         ;; the vector is the results of `transform-module` from
         ;; eval-helpers-and-pref-init.rkt in the case that online
         ;; expansion was able to compile the file (except
         ;; that instead of a syntax object, we have the bytes
         ;; for a compiled object). The `module-path?` is
         ;; probably never a `path?`.
         (or/c (vector/c bytes? symbol? module-path?)
               #f))
  #;
  (dirty boolean?)
  #;
  (running symbol? string?)

  (struct clean (error-type error-messages+locs compiled-code) #:transparent)
  (struct dirty (timer-pending?) #:transparent)
  (struct running (sym str) #:transparent)
  
  (provide (struct-out clean)
           (struct-out dirty)
           (struct-out running)))

(require (submod "." oc-status-structs))

#|
;; this code tracks which lines have been executed 
;; for use while (manually) testing the oc state machine
(require (for-syntax racket/base) racket/set)
(define candidate-lines (set))
(define touched-lines (set))
(define-syntax (line-of-interest stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-local-lift-expression #'(set! candidate-lines (set-add candidate-lines line)))
    #'(visited line)))
(define (visited line)
  (unless (set-member? touched-lines line)
    (set! touched-lines (set-add touched-lines line))
    (printf "~s\n" (sort (set->list (set-subtract candidate-lines touched-lines)) >))))
|#
(define-syntax-rule (line-of-interest) (void))

(define-runtime-path expanding-place.rkt "expanding-place.rkt")

(define sc-online-expansion-running (string-constant online-expansion-running))
(define sc-only-raw-text-files-supported 
  (string-constant online-expansion-only-raw-text-files-supported))
(define sc-abnormal-termination (string-constant online-expansion-abnormal-termination))
(define sc-abnormal-termination-out-of-memory 
  (string-constant online-expansion-abnormal-termination-out-of-memory))
(define sc-jump-to-error (string-constant jump-to-error))
(define sc-finished-successfully (string-constant online-expansion-finished-successfully))

(define op (current-output-port))
(define (oprintf . args) (apply fprintf op args))

(define-unit module-language@
  (import [prefix drracket:language-configuration: drracket:language-configuration/internal^]
          [prefix drracket:language: drracket:language/int^]
          [prefix drracket:unit: drracket:unit^]
          [prefix drracket:rep: drracket:rep/int^]
          [prefix drracket:init: drracket:init/int^]
          [prefix drracket:module-language-tools: drracket:module-language-tools/int^]
          [prefix drracket:modes: drracket:modes^]
          [prefix drracket: drracket:interface^])
  (export drracket:module-language/int^)
  
  (define module-language<%>
    (interface ()
      get-users-language-name))
  
  ;; add-module-language : -> void
  ;; adds the special module-only language to drracket
  (define (add-module-language)
    (define module-language%
      (module-mixin
       ((drracket:language:get-default-mixin)
        (drracket:language:module-based-language->language-mixin
         (drracket:language:simple-module-based-language->module-based-language-mixin
          drracket:language:simple-module-based-language%)))))
    (drracket:language-configuration:add-language
     (new module-language%)
     #:allow-executable-creation? #t))
  
  ;; collection-paths : (listof (union 'default string))
  ;; command-line-args : (vectorof string)
  ;; auto-text : string
  (define-struct (module-language-settings lmn:drracket:language:simple-settings)
    (collection-paths command-line-args auto-text compilation-on? full-trace? submodules-to-run
                      enforce-module-constants))
  
  (define (module-language-settings->prefab-module-settings settings)
    (prefab-module-settings (module-language-settings-command-line-args settings)
                            (module-language-settings-collection-paths settings)
                            (module-language-settings-compilation-on? settings)
                            (module-language-settings-full-trace? settings)
                            (drracket:language:simple-settings-annotations settings)
                            (module-language-settings-enforce-module-constants settings)))
  
  (define default-compilation-on? #t)
  (define default-full-trace? #t)
  (define default-submodules-to-run (list '(test) '(main)))
  (define default-enforce-module-constants #t)
  (define (get-default-auto-text) (preferences:get 'drracket:module-language:auto-text))

  (define (disable-debugging-et-al language-settings)
    (define lang
      (drracket:language-configuration:language-settings-language language-settings))
    (cond
      [(is-a? lang module-language<%>)
       (drracket:language-configuration:make-language-settings
        lang
        (struct-copy
         module-language-settings
         (drracket:language-configuration:language-settings-settings language-settings)
         [annotations #:parent lmn:drracket:language:simple-settings 'none]))]
      [else language-settings]))
  
  (define drracket-determined-width (make-parameter 'infinity))
  
  ;; module-mixin : (implements drracket:language:language<%>)
  ;;             -> (implements drracket:language:language<%>)
  (define (module-mixin %)
    (class* % (drracket:language:language<%> module-language<%>)

      (define/override (first-opened settings)
        (define ns (with-handlers ([exn:fail? 
                                    (λ (x) 
                                      (log-error 
                                       "DrRacket:module-language.rkt:first-opened exn: ~a" 
                                       (exn-message x))
                                      (for ([x (in-list (continuation-mark-set->context
                                                         (exn-continuation-marks x)))])
                                        (log-error (format "  ~s" x)))
                                      #f)])
                     ;; get-ns can fail in all kinds of strange ways;
                     ;; just give up if it does, since an error here
                     ;; means drracket won't start up.
                     (get-ns (get-auto-text settings))))
        (when ns (current-namespace ns)))
      
      (define/private (get-ns str)
        (define ev (parameterize ([sandbox-path-permissions (list (list 'exists #rx#""))])
                     (make-evaluator 'racket/base)))
        (ev `(current-inspector ,(current-inspector)))
        (ev `(parameterize ([read-accept-reader #t])
               (define stx (read-syntax "here" (open-input-string ,str)))
               (define modname
                 (syntax-case stx ()
                   [(module name . stuff)
                    `',(syntax->datum #'name)]
                   [_ #f]))
               (and modname
                    (eval stx)
                    (namespace-require modname)
                    (module->namespace modname)))))
      
      (inherit get-language-name)
      (define/public (get-users-language-name defs-text settings)
        (define str (send defs-text irl-get-read-language-name))
        (define pos (and str (regexp-match-positions #rx"#(?:!|lang )" str)))
        (cond
          [pos
           (define language-name-without-runtime-stuff
             (substring str (cdr (car pos)) (string-length str)))
           (format "~a~a"
                   language-name-without-runtime-stuff
                   (case (drracket:language:simple-settings-annotations settings)
                     [(none) (string-constant module-language-repl-no-annotations)]
                     [(debug) (string-constant module-language-repl-debug-annotations)]
                     [(debug/profile)
                      (string-constant module-language-repl-debug/profile-annotations)]
                     [(test-coverage)
                      (string-constant module-language-repl-test-annotations)]))]
          [else
           (get-language-name)]))
      
      (define/override (use-namespace-require/copy?) #f)
      
      (define/augment (capability-value key)
        (cond
          [(eq? key 'drscheme:autocomplete-words)
           (drracket:language-configuration:get-all-manual-keywords)]
          [(eq? key 'drscheme:define-popup)
           (define the-irl (drracket:module-language-tools:capability-value-irl))
           (call-read-language the-irl
                               'drracket:define-popup
                               '(("(define" "(define ...)" "δ")
                                 ("(module" "(module ...)" "ρ")))]
          [else (drracket:language:get-capability-default key)]))
      
      ;; config-panel : as in super class
      ;; uses drracket:language:simple-module-based-language-config-panel
      ;; and adds a collection paths configuration to it.
      (define/override (config-panel parent)
        (module-language-config-panel parent))
      
      ;; NOTE: this method is also used in the super class's implementation
      ;; of default-settings?, which is why the super call is appropriate
      ;; there, even tho these settings are not the same as the defaults
      ;; in other languages (here 'none is the default annotations,
      ;; there you get errortrace annotations).
      (define/override (default-settings)
        (let ([super-defaults (super default-settings)])
          (make-module-language-settings
           #t 'print 'mixed-fraction-e #f #t 'debug;; simple settings defaults 
           
           '(default)
           #()
           (get-default-auto-text)
           default-compilation-on?
           default-full-trace?
           default-submodules-to-run
           default-enforce-module-constants)))
      
      ;; default-settings? : -> boolean
      (define/override (default-settings? settings)
        
        (and (super default-settings? settings)
             
             (equal? (module-language-settings-collection-paths settings)
                     '(default))
             (equal? (module-language-settings-command-line-args settings)
                     #())
             ;; Never show that this is a "custom" language because of the
             ;; auto-text
             ;; (equal? (module-language-settings-auto-text settings)
             ;;         default-auto-text)
             (equal? (module-language-settings-compilation-on? settings)
                     default-compilation-on?)
             (equal? (module-language-settings-full-trace? settings)
                     default-full-trace?)
             (equal? (module-language-settings-submodules-to-run settings)
                     default-submodules-to-run)
             (equal? (module-language-settings-enforce-module-constants settings)
                     default-enforce-module-constants)))
      
      (define/override (marshall-settings settings)
        (let ([super-marshalled (super marshall-settings settings)])
          (list super-marshalled
                (module-language-settings-collection-paths settings)
                (module-language-settings-command-line-args settings)
                (module-language-settings-auto-text settings)
                (module-language-settings-compilation-on? settings)
                (module-language-settings-full-trace? settings)
                (module-language-settings-submodules-to-run settings)
                (module-language-settings-enforce-module-constants settings))))
      
      (define/override (unmarshall-settings marshalled)
        (and (list? marshalled)
             (let ([marshalled-len (length marshalled)])
               ;; older formats had no auto-text or compilation disabling
               (and (<= 3 marshalled-len)
                    (let ([collection-paths (list-ref marshalled 1)]
                          [command-line-args (list-ref marshalled 2)]
                          [auto-text (if (<= marshalled-len 3)
                                         (get-default-auto-text)
                                         (list-ref marshalled 3))]
                          [compilation-on? (if (<= marshalled-len 4)
                                               default-compilation-on?
                                               (list-ref marshalled 4))]
                          [full-trace? (if (<= marshalled-len 5)
                                           default-full-trace?
                                           (list-ref marshalled 5))]
                          [submodules-to-run (if (<= marshalled-len 6)
                                                 default-submodules-to-run
                                                 (list-ref marshalled 6))]
                          [enforce-module-constants (if (<= marshalled-len 7)
                                                        default-enforce-module-constants
                                                        (list-ref marshalled 7))])
                      (and (list? collection-paths)
                           (andmap (λ (x) (or (string? x) (symbol? x)))
                                   collection-paths)
                           (vector? command-line-args)
                           (andmap string? (vector->list command-line-args))
                           (or (string? auto-text) (not auto-text))
                           (boolean? compilation-on?)
                           ((listof (listof symbol?)) submodules-to-run)
                           (boolean? enforce-module-constants)
                           (let ([super (super unmarshall-settings 
                                               (let ([p (car marshalled)])
                                                 ;; Convert 'write to 'print:
                                                 (if (eq? (vector-ref p 1) 'write)
                                                     (list->vector 
                                                      (list* (vector-ref p 0)
                                                             'print
                                                             (cddr (vector->list p))))
                                                     p)))])
                             (and super
                                  (apply 
                                   make-module-language-settings
                                   (append
                                    (vector->list (drracket:language:simple-settings->vector super))
                                    (list collection-paths
                                          command-line-args
                                          auto-text
                                          
                                          ;; current versions of drracket do not allow this 
                                          ;; combination in the first place (compilation is only
                                          ;; allowed in 'none and 'debug mode), but older
                                          ;; versions might.
                                          (and (memq 
                                                (drracket:language:simple-settings-annotations super) 
                                                '(none debug))
                                               compilation-on?)
                                          
                                          full-trace?
                                          submodules-to-run
                                          enforce-module-constants)))))))))))
      
      (define/override (on-execute settings run-in-user-thread)
        (super on-execute settings run-in-user-thread)
        
        (let ([currently-open-files (get-currently-open-files)])
          (run-in-user-thread
           (λ ()
             (set-module-language-parameters 
              (module-language-settings->prefab-module-settings settings)
              module-language-parallel-lock-client
              currently-open-files)))))
      
      (define/override (get-one-line-summary)
        (string-constant module-language-one-line-summary))
      
      (define/public (get-auto-text settings)
        (or (module-language-settings-auto-text settings)
            (preferences:get 'drracket:most-recent-lang-line)))
      
      ;; utility for the front-end method: return a function that will return
      ;; each of the given syntax values on each call, executing thunks when
      ;; included; when done with the list, send eof.
      (define (expr-getter . exprs/thunks)
        (define (loop)
          (if (null? exprs/thunks)
            eof
            (let ([x (car exprs/thunks)])
              (set! exprs/thunks (cdr exprs/thunks))
              (if (procedure? x) (begin (x) (loop)) x))))
        loop)
      
      (inherit get-reader)
      (define repl-init-thunk (make-thread-cell #f))

      ;; drracket will always supply `the-irl`, but some tools might call this,
      ;; and they might not supply it
      (define/override (front-end/complete-program port settings [the-irl #f])
        (define (super-thunk) 
          (define reader (get-reader))
          (reader (object-name port) port))
        (define path
          (cond [(get-filename port) => (compose simplify-path cleanse-path)]
                [else #f]))
        (define resolved-modpath (and path (module-path-index-resolve
                                            (module-path-index-join
                                             path
                                             #f))))

        (define-values (name lang module-expr)
          (cond
            [(drracket:rep:current-pre-compiled-transform-module-results)
             =>
             (λ (transform-module-results)
               (define compiled-expression
                 (parameterize ([read-accept-compiled #t])
                   (read (open-input-bytes (vector-ref transform-module-results 2)))))
               (values
                (vector-ref transform-module-results 0)
                (vector-ref transform-module-results 1)
                (with-syntax ([x compiled-expression]) #'x)))]
            [else
             (define expr
               ;; just reading the definitions might be a syntax error,
               ;; possibly due to bad language (eg, no foo/lang/reader)
               (with-handlers ([exn:fail? (λ (e) (raise-hopeless-exception e))])
                 (super-thunk)))
             (when (eof-object? expr)
               (raise-hopeless-syntax-error (string-append
                                             "There must be a valid module in the\n"
                                             "definitions window.  Try starting your program with\n"
                                             "\n"
                                             "  #lang racket\n"
                                             "or\n"
                                             "  #lang htdp/bsl\n"
                                             "\n"
                                             "and clicking ‘Run’.")))
             (let ([more (super-thunk)])
               (unless (eof-object? more)
                 (raise-hopeless-syntax-error
                  "there can only be one expression in the definitions window"
                  more)))
             (transform-module path expr raise-hopeless-syntax-error)]))

        (define modspec (or path `',name))
        (define (check-interactive-language)
          (unless (memq '#%top-interaction (namespace-mapped-symbols))
            (raise-hopeless-exception
             #f ; no error message, just a suffix
             (format "~s does not support a REPL (no #%top-interaction)"
                     lang))))
        ;; We're about to send the module expression to drracket now, the rest
        ;; of the setup is done in `front-end/finished-complete-program' below,
        ;; so use `repl-init-thunk' to store an appropriate continuation for
        ;; this setup.  Once we send the expression, we'll be called again only
        ;; if it was evaluated (or expanded) with no errors, so begin with a
        ;; continuation that deals with an error, and if we're called again,
        ;; change it to a continuation that initializes the repl for the
        ;; module.  So the code is split among several thunks that follow.
        (define (*pre)
          (thread-cell-set! repl-init-thunk *error)
          (current-module-declare-name resolved-modpath)
          (current-module-declare-source path))
        (define (*post)
          (current-module-declare-name #f)
          (current-module-declare-source #f)
          (when path ((current-module-name-resolver) resolved-modpath #f))
          (thread-cell-set! repl-init-thunk *init))
        (define (*error)
          (current-module-declare-name #f)
          (current-module-declare-source #f)
          ;; syntax error => try to require the language to get a working repl
          (with-handlers ([void (λ (e)
                                  (raise-hopeless-syntax-error
                                   "invalid language specification"
                                   lang))])
            (namespace-require lang))
          (check-interactive-language))
        (define (*init)
          (parameterize ([current-namespace (current-namespace)])
            ;; the prompt makes it continue after an error
            (call-with-continuation-prompt
             (λ () (with-stack-checkpoint 
                    (begin
                      (*do-module-specified-configuration)
                      (namespace-require modspec)
                      (for ([submod (in-list (module-language-settings-submodules-to-run settings))])
                        (define submod-spec `(submod ,modspec ,@submod))
                        (when (module-declared? submod-spec)
                          (dynamic-require submod-spec #f))))))))
          (current-namespace (module->namespace modspec))
          (check-interactive-language))
        (define (*do-module-specified-configuration)
          (define info (module->language-info modspec #t))
          (unless (mcli? info) (set! info #f))
          (when the-irl
            (parameterize ([current-eventspace drracket:init:system-eventspace])
              (queue-callback
               (λ () (set-irl-mcli-vec! the-irl info)))))
          (when info
            (let ([get-info
                   ((dynamic-require (vector-ref info 0)
                                     (vector-ref info 1))
                    (vector-ref info 2))])
              (let ([configs (get-info 'configure-runtime '())])
                (for ([config (in-list configs)])
                  ((dynamic-require (vector-ref config 0)
                                    (vector-ref config 1))
                   (vector-ref config 2))))))
          (define cr-submod `(submod ,modspec configure-runtime))
          (when (module-declared? cr-submod)
            (dynamic-require cr-submod #f)))
        ;; here's where they're all combined with the module expression
        (expr-getter *pre module-expr *post))
      
      (define/override (front-end/finished-complete-program settings)
        (cond [(thread-cell-ref repl-init-thunk)
               => (λ (t) (thread-cell-set! repl-init-thunk #f) (t))]))
      
      (define/override (front-end/interaction port settings)
        (λ ()
          (let ([v (parameterize ([read-accept-reader #t]
                                  [read-accept-lang #f])
                     (with-stack-checkpoint
                      ((current-read-interaction) 
                       (object-name port)
                       port)))])
            (if (eof-object? v)
                v
                (let ([w (cons '#%top-interaction v)])
                  (if (syntax? v)
                      (namespace-syntax-introduce
                       (datum->syntax #f w v))
                      v))))))

      (define/override (render-value/format value settings port width)
        (do-print value settings port width))
      (define/override (render-value value settings port)
        (do-print value settings port 'infinity))
      (define/private (do-print value settings port width)
        (parameterize ([drracket-determined-width width])
          (print value port))
        (newline port))
    

      
      ;; printer settings are just ignored here.
      (define/override (create-executable setting parent program-filename)
        (let* ([executable-specs (drracket:language:create-executable-gui
                                  parent program-filename #t #t)])
          (when executable-specs
            (let ([executable-type (list-ref executable-specs 0)]
                  [gui? (eq? 'mred (list-ref executable-specs 1))]
                  [executable-filename (list-ref executable-specs 2)]
                  [aux (list-ref executable-specs 3)])
              (with-handlers ([(λ (x) #f) ;exn:fail?
                               (λ (x)
                                 (message-box
                                  (string-constant drscheme)
                                  (if (exn? x)
                                      (format "~a" (exn-message x))
                                      (format "uncaught exception: ~s" x))))])
                (let ([call-create-embedding-executable
                       (λ (exe-name)
                         (let ([short-program-name
                                (let-values ([(base name dir) (split-path program-filename)])
                                  (path-replace-suffix name #""))])
                           (create-embedding-executable
                            exe-name
                            #:gracket? gui?
                            #:aux aux
                            #:verbose? #f
                            #:expand-namespace (make-base-namespace)
                            #:modules (list (list #f program-filename))
                            #:configure-via-first-module? #t
                            #:literal-expression
                            (parameterize ([current-namespace (make-base-empty-namespace)])
                              (namespace-require 'racket/base)
                              (compile
                               `(namespace-require
                                 '',(string->symbol (path->string short-program-name)))))
                            #:cmdline '("-U" "--"))))])
                  
                  (case executable-type
                    [(launcher)
                     (let ([make-launcher (if gui? make-mred-launcher make-mzscheme-launcher)])
                       (make-launcher (list "-qt-" (path->string program-filename))
                                      executable-filename))]
                    [(distribution)
                     (drracket:language:create-distribution-for-executable
                      executable-filename
                      gui?
                      call-create-embedding-executable)]
                    [(stand-alone)
                     (define c (make-custodian))
                     (define d (new dialog% 
                                    [parent parent] 
                                    [label (string-constant create-executable-title)]))
                     (new message% 
                          [parent d]
                          [label (string-constant creating-executable-progress-status)])
                     (new button%
                          [parent d]
                          [label (string-constant abort)]
                          [callback (lambda (_1 _2)
                                      (custodian-shutdown-all c))])
                     (define thd
                       (parameterize ([current-custodian c])
                         (thread
                          (λ ()
                            (call-create-embedding-executable executable-filename)))))
                     (thread
                      (λ ()
                        (thread-wait thd)
                        (queue-callback (λ () (send d show #f)))))
                     (send d show #t)])))))))
      
      (super-new
       [module #f]
       [language-position (list (string-constant module-language-name))]
       [language-numbers (list -32768)]
       [reader (λ (src port)
                 (parameterize ([read-accept-reader #t])
                   (with-stack-checkpoint
                    (read-syntax src port))))])))
  
  ;; can be called with #f to just kill the repl (in case we want to kill it
  ;; but keep the highlighting of a previous error)
  (define (raise-hopeless-exception exn [suffix #f])
    (define rep (drracket:rep:current-rep))
    ;; Throw an error as usual if we don't have the drracket rep, then we just
    ;; raise the exception as normal.  (It can happen in some rare cases like
    ;; having a single empty scheme box in the definitions.)
    (unless rep (if exn (raise exn) (error "\nInteractions disabled")))
    (when exn ((error-display-handler) (exn-message exn) exn))
    ;; these are needed, otherwise the warning can appear before the output
    (flush-output (current-output-port))
    (flush-output (current-error-port))
    ;; do the rep-related work carefully -- using drracket's eventspace, and
    ;; wait for it to finish before we continue.
    (let ([s (make-semaphore 0)]
          [msg (string-append "\nInteractions disabled"
                              (if suffix (string-append ": " suffix) "."))])
      (parameterize ([current-eventspace drracket:init:system-eventspace])
        (queue-callback
         (λ ()
           (send rep call-without-reset-highlighting
             (λ ()
               (send* rep (insert-warning msg)
                          (set-show-no-user-evaluation-message? #f))))
           (semaphore-post s))))
      (semaphore-wait s))
    (custodian-shutdown-all (send rep get-user-custodian)))
  
  (define (raise-hopeless-syntax-error . error-args)
    (with-handlers ([exn:fail? raise-hopeless-exception])
      (apply raise-syntax-error '|Module Language|
             error-args)))
    
  ;; module-language-config-panel : panel -> (case-> (-> settings) (settings -> void))
  (define (module-language-config-panel parent)
    (define new-parent
      (new-vertical-panel%
           [parent parent]
           [alignment '(center center)]
           [stretchable-height #f]
           [stretchable-width #f]))
    (define compilation-on-check-box #f)
    (define enforce-module-constants-checkbox #f)
    (define compilation-on? #t)
    (define save-stacktrace-on-check-box #f)
    (define run-submodules-choice #f)
    (define left-debugging-radio-box #f)
    (define right-debugging-radio-box #f)
    (define submodules-to-run #f)
    
    (define (set-submodules-to-run l)
      (define ht (make-hash))
      (for ([submod (in-list (preferences:get 'drracket:submodules-to-choose-from))]
            [x (in-naturals)])
        (hash-set! ht submod x))
      
      ;; there may be a submodule in this list that is no longer choosable from
      ;; the preferences (if the user edits their prefs or the defaults change)
      ;; make sure the sort below doesn't crash by putting in an +inf.0
      ;; (also, rely on sort's stability)
      (set! submodules-to-run (sort l < 
                                    #:key (λ (x) (hash-ref ht x +inf.0)))))
    
    (define simple-case-lambda
      (drracket:language:simple-module-based-language-config-panel
       new-parent
       #:case-sensitive #t
       
       #:get-debugging-radio-box (λ (rb-l rb-r) 
                                   (set! left-debugging-radio-box rb-l)
                                   (set! right-debugging-radio-box rb-r))
       
       #:debugging-radio-box-callback
       (λ (debugging-radio-box evt)
         (update-compilation-checkbox left-debugging-radio-box right-debugging-radio-box))

       #:dynamic-panel-extras
       (λ (dynamic-panel)
         (set! compilation-on-check-box
               (new check-box%
                    [label (string-constant automatically-compile)]
                    [parent dynamic-panel]
                    [callback
                     (λ (_1 _2) (set! compilation-on? (send compilation-on-check-box get-value)))]))
         (set! save-stacktrace-on-check-box 
               (new check-box%
                    [label (string-constant preserve-stacktrace-information)]
                    [parent dynamic-panel]))
         (set! enforce-module-constants-checkbox
               (new check-box%
                    [label (string-constant enforce-module-constants-checkbox-label)]
                    [parent dynamic-panel]))
         (set! run-submodules-choice 
               (new (class name-message%
                      (define/override (fill-popup menu reset)
                        (for ([item (in-list (preferences:get 'drracket:submodules-to-choose-from))]
                              [x (in-naturals)])
                          (new checkable-menu-item%
                               [label 
                                (apply string-append (add-between (map symbol->string item) " "))]
                               [checked (member item submodules-to-run)]
                               [callback 
                                (λ (a b)
                                  (if (member item submodules-to-run)
                                      (set-submodules-to-run (remove item submodules-to-run))
                                      (set-submodules-to-run (cons item submodules-to-run))))]
                               [parent menu]))
                        (new separator-menu-item% [parent menu])
                        (new menu-item% 
                             [parent menu]
                             [callback 
                              (λ (a b)
                                (define new-submod (add-another-possible-submodule parent))
                                (when new-submod
                                  (set-submodules-to-run (cons new-submod submodules-to-run))))]
                             [label (string-constant add-submodule)]))
                      (super-new
                       [font normal-control-font]
                       [parent dynamic-panel]
                       [label (string-constant submodules-to-run)])))))))
    (define (update-compilation-checkbox left-debugging-radio-box right-debugging-radio-box)
      (case (send left-debugging-radio-box get-selection)
        [(0 1)
         (send compilation-on-check-box enable #t)
         (send compilation-on-check-box set-value compilation-on?)]
        [(#f)
         (send compilation-on-check-box enable #f)
         (send compilation-on-check-box set-value #f)]))
    
    (define cp-panel (new group-box-panel%
                          [parent new-parent]
                          [label (string-constant ml-cp-collection-paths)]))
    
    (define args-panel (new group-box-panel%
                            [parent new-parent]
                            [label (string-constant ml-command-line-arguments)]))
    (define args-text-box (new text-field%
                               [parent args-panel]
                               [label #f]
                               [init-value "#()"]
                               [callback void]))
    (define auto-text-panel (new group-box-panel%
                                 [parent new-parent]
                                 [label (string-constant module-language-auto-text)]))
    (define hp (new horizontal-panel%
                    [parent auto-text-panel]
                    [alignment '(center bottom)]))
    (define auto-text-rb (new radio-box%
                              [parent hp]
                              [label #f]
                              [choices
                               (list
                                (string-constant module-language-auto-text-most-recent)
                                (string-constant module-language-auto-text-always-same))]
                              [callback
                               (λ (rb evt)
                                 (preferences:set
                                  'drracket:module-language:auto-text
                                  (case (send rb get-selection)
                                    [(0)
                                     (turn-on/off-auto-text-text-box #f)
                                     #f]
                                    [(1)
                                     (turn-on/off-auto-text-text-box #t)
                                     "#lang racket\n"])))]))
    (define auto-text-text-box (new text-field%
                                    [parent hp]
                                    [label #f]
                                    [init-value ""]
                                    [callback 
                                     (λ (tf evt)
                                       (preferences:set 'drracket:module-language:auto-text
                                                        (get-auto-text)))]))
    (define (turn-on/off-auto-text-text-box on?)
      (send auto-text-text-box enable on?)
      (send auto-text-text-box set-field-background
            (color-prefs:lookup-in-color-scheme
             (if on?
                 'framework:basic-canvas-background
                 'framework:disabled-background-color))))
    
    ;; data associated with each item in listbox : boolean
    ;; indicates if the entry is the default paths.
    (define collection-paths-lb (new list-box%
                                     [parent cp-panel]
                                     [choices '("a" "b" "c")]
                                     [label #f]
                                     [callback (λ (x y) (update-buttons))]))
    (define button-panel (new-horizontal-panel%
                              [parent cp-panel]
                              [alignment '(center center)]
                              [stretchable-height #f]))
    (define add-button
      (make-object button% (string-constant ml-cp-add) button-panel
        (λ (x y) (add-callback))))
    (define add-default-button
      (make-object button% (string-constant ml-cp-add-default) button-panel
        (λ (x y) (add-default-callback))))
    (define remove-button
      (make-object button% (string-constant ml-cp-remove) button-panel
        (λ (x y) (remove-callback))))
    (define raise-button
      (make-object button% (string-constant ml-cp-raise) button-panel
        (λ (x y) (move-callback -1))))
    (define lower-button
      (make-object button% (string-constant ml-cp-lower) button-panel
        (λ (x y) (move-callback +1))))
    
    (define (update-buttons)
      (let ([lb-selection (send collection-paths-lb get-selection)]
            [lb-tot (send collection-paths-lb get-number)])
        (send remove-button enable lb-selection)
        (send raise-button enable (and lb-selection (not (= lb-selection 0))))
        (send lower-button enable
              (and lb-selection (not (= lb-selection (- lb-tot 1)))))))
    
    (define (add-callback)
      (let ([dir (get-directory (string-constant ml-cp-choose-a-collection-path)
                                (send parent get-top-level-window))])
        (when dir
          (send collection-paths-lb append (path->string dir) #f)
          (update-buttons))))
    
    (define (add-default-callback)
      (cond [(has-default?)
             (message-box (string-constant drscheme)
                          (string-constant ml-cp-default-already-present)
                          (send parent get-top-level-window))]
            [else
             (send collection-paths-lb append (string-constant ml-cp-default-collection-path) #t)
             (update-buttons)]))
    
    ;; has-default? : -> boolean
    ;; returns #t if the `default' entry has already been added
    (define (has-default?)
      (let loop ([n (send collection-paths-lb get-number)])
        (cond [(= n 0) #f]
              [(send collection-paths-lb get-data (- n 1)) #t]
              [else (loop (- n 1))])))
    
    (define (remove-callback)
      (let ([to-delete (send collection-paths-lb get-selection)])
        (send collection-paths-lb delete to-delete)
        (unless (zero? (send collection-paths-lb get-number))
          (send collection-paths-lb set-selection
                (min to-delete (- (send collection-paths-lb get-number) 1))))
        (update-buttons)))
    
    (define (move-callback d)
      (let* ([sel (send collection-paths-lb get-selection)]
             [vec (get-lb-vector)]
             [new (+ sel d)]
             [other (vector-ref vec new)])
        (vector-set! vec new (vector-ref vec sel))
        (vector-set! vec sel other)
        (set-lb-vector vec)
        (send collection-paths-lb set-selection new)
        (update-buttons)))
    
    (define (get-lb-vector)
      (list->vector (for/list ([n (in-range (send collection-paths-lb get-number))])
                              (cons (send collection-paths-lb get-string n)
                                    (send collection-paths-lb get-data n)))))
    
    (define (set-lb-vector vec)
      (send collection-paths-lb clear)
      (for ([x (in-vector vec)] [n (in-naturals)])
           (send collection-paths-lb append (car x))
           (send collection-paths-lb set-data n (cdr x))))
    
    (define (get-collection-paths)
      (for/list ([n (in-range (send collection-paths-lb get-number))])
                (let ([data (send collection-paths-lb get-data n)])
                  (if data 'default (send collection-paths-lb get-string n)))))
    
    (define (install-collection-paths paths)
      (send collection-paths-lb clear)
      (for ([cp paths])
           (if (symbol? cp)
               (send collection-paths-lb append (string-constant ml-cp-default-collection-path) #t)
               (send collection-paths-lb append cp #f))))
    
    (define (get-command-line-args)
      (let* ([str (send args-text-box get-value)]
             [read-res (parameterize ([read-accept-graph #f])
                         (with-handlers ([exn:fail:read? (λ (x) #())])
                           (read (open-input-string str))))])
        (if (and (vector? read-res) (andmap string? (vector->list read-res)))
            read-res
            #())))
    
    (define (install-command-line-args vec)
      (send args-text-box set-value
            (parameterize ([print-vector-length #f])
              (format "~s" vec))))
    
    (define (get-auto-text)
      (case (send auto-text-rb get-selection)
        [(0) #f]
        [(1)
         (define str (send auto-text-text-box get-value))
         (cond
           [(equal? str "") ""]
           [else (string-append str "\n")])]))
    
    (define (install-auto-text str)
      (cond
        [(string? str)
         (send auto-text-rb set-selection 1)
         (send auto-text-text-box set-value (regexp-replace #rx"\n$" str ""))
         (turn-on/off-auto-text-text-box #t)]
        [else
         (send auto-text-rb set-selection 0)
         (turn-on/off-auto-text-text-box #f)]))
    
    (install-collection-paths '(default))
    (update-buttons)
    (install-auto-text (get-default-auto-text))
    (update-compilation-checkbox left-debugging-radio-box right-debugging-radio-box)

    (case-lambda
      [()
       (let ([simple-settings (simple-case-lambda)])
         (apply make-module-language-settings
                (append 
                 (vector->list (drracket:language:simple-settings->vector simple-settings))
                 (list (get-collection-paths)
                       (get-command-line-args)
                       (get-auto-text)
                       (case (send left-debugging-radio-box get-selection)
                         [(0 1) compilation-on?]
                         [(#f) #f])
                       (send save-stacktrace-on-check-box get-value)
                       submodules-to-run
                       (send enforce-module-constants-checkbox get-value)))))]
      [(settings)
       (simple-case-lambda settings)
       (install-collection-paths (module-language-settings-collection-paths settings))
       (install-command-line-args (module-language-settings-command-line-args settings))
       (install-auto-text (module-language-settings-auto-text settings))
       (set! compilation-on? (module-language-settings-compilation-on? settings))
       (send compilation-on-check-box set-value (module-language-settings-compilation-on? settings))
       (update-compilation-checkbox left-debugging-radio-box right-debugging-radio-box)
       (send save-stacktrace-on-check-box set-value (module-language-settings-full-trace? settings))
       (set-submodules-to-run (module-language-settings-submodules-to-run settings))
       (send enforce-module-constants-checkbox set-value
             (module-language-settings-enforce-module-constants settings))
       (update-buttons)]))
  
  (define (add-another-possible-submodule parent)
    (define (get-sexp x)
      (with-handlers ((exn:fail:read? (λ (x) #f)))
        (define p (open-input-string (string-append "(" x ")")))
        (define sexp (read p))
        (and (eof-object? (read-char p))
             (list? sexp)
             (andmap symbol? sexp)
             sexp)))
    (define msg (get-text-from-user (string-constant add-submodule-title) 
                                    "submodule"
                                    (let loop ([parent parent])
                                      (define p (send parent get-parent))
                                      (if p
                                          (loop p)
                                          parent))
                                    #:validate (λ (x) (get-sexp x))))
    (define submods
      (and msg
           (get-sexp msg)))
    (cond
      [submods
       (preferences:set 'drracket:submodules-to-choose-from
                        (append (preferences:get 'drracket:submodules-to-choose-from)
                                (list submods)))
       submods]
      [else #f]))
  
  ;; get-filename : port -> (union string #f)
  ;; extracts the file the definitions window is being saved in, if any.
  (define (get-filename port)
    (let ([source (object-name port)])
      (cond
        [(path? source) source]
        [(is-a? source text%)
         (let ([canvas (send source get-canvas)])
           (and canvas
                (let ([frame (send canvas get-top-level-window)])
                  (and (is-a? frame drracket:unit:frame%)
                       (let* ([b (box #f)]
                              [filename (send (send frame get-definitions-text)
                                              get-filename
                                              b)])
                         (if (unbox b)
                             #f
                             filename))))))]
        [else #f])))
  
  (define online-expansion-logger (make-logger 'online-expansion-state-machine (current-logger)))
  (define-syntax-rule
    (define/oc-log (id . args) . body)
    (define (id . args)
      (log-oel 'id (list . args))
      (begin0 (let () . body)
              (log-oel 'id #f))))
  
  (define-syntax-rule
    (log-oel id args)
    (when (log-level? online-expansion-logger 'info)
      (log-oel/proc id args)))
  
  (define (log-oel/proc id args)
    (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
    (define (val->str t)
      (cond
        [(is-a? t drracket:unit:tab<%>)
         (define fn (send (send t get-defs) get-filename))
         (if fn
             (let-values ([(base name path?) (split-path fn)])
               (path->string name))
             "untitled")]
        [(vector? t) (format "~s" (vector (vector-ref t 0) '...))]
        [(not t) "#f"]
        [else "?"]))
    (log-message online-expansion-logger
                 'info
                 (format "~a: ~a\n     running ~a dirty/pending ~a dirty-tabs ~a clean-tabs ~a" 
                         id
                         (if args
                             (map val->str args)
                             (if oc-timer-running?
                                 "return: timer running"
                                 "return: timer stopped"))
                         (val->str running-tab)
                         (val->str dirty/pending-tab)
                         (map val->str dirty-tabs)
                         (map val->str clean-tabs))
                 (current-continuation-marks))
    (unless args
      ;; the invariant is not always true when the call is made
      ;; but it should be true when the call returns
      (oc-check-invariant id)))
  
  (define module-language-online-expand-tab-mixin
    (mixin (drracket:unit:tab<%>) ()
      (inherit get-frame get-defs get-ints)
      
      (define/augment (on-close) 
        (oc-remove-tab this)
        (inner (void) on-close))
      
      ;; (or/c clean? dirty? running?)
      (define running-status (clean #f #f #f))
      (define our-turn? #f)

      (define/public (get-pre-compiled-transform-module-results)
        (and (preferences:get 'drracket:online-compilation-default-on)
             (clean? running-status)
             (clean-compiled-code running-status)))
      
      (define/public (set-oc-status s) 
        (unless (equal? running-status s)
          (set! running-status s)
          (update-gui)))
      (define/public (get-oc-status) running-status)
      
      (define/private (update-gui)
        (update-little-dot)
        (update-bottom-bar)
        (update-error-in-defs))
      
      (define/private (update-error-in-defs)
        (send (get-defs) begin-edit-sequence #f #f)
        (send (get-defs) clear-old-error)
        (when (clean? running-status)
          (match-define (clean error-type error-messages+locs compiled-code) running-status)
          (when error-messages+locs
            (define pref-key
              (case error-type
                [(exn access-violation abnormal-termination) 'drracket:online-expansion:other-errors]
                [(exn:variable) 'drracket:online-expansion:variable-errors]
                [(reader-in-defs-error) 'drracket:online-expansion:read-in-defs-errors]
                [else (error 'module-language.rkt "unknown clean status: ~s" running-status)]))
            (case (preferences:get pref-key)
              [(margin) 
               (send (get-defs) set-margin-error-ranges ;; FIX HERE
                     (set->list
                      (for*/set ([error-message+loc (in-list error-messages+locs)]
                                 [range (in-list (exn-info-src-vecs error-message+loc))])
                        (define pos (vector-ref range 0))
                        (define span (vector-ref range 1))
                        (error-range (- pos 1) (+ pos span -1) #f))))]
              [(gold) 
               (send (get-defs) set-gold-highlighted-errors 
                     (remove-duplicates
                      (apply append (map exn-info-src-vecs error-messages+locs))))])
            (send (get-ints) set-error-ranges 
                  (set->list
                   (for*/set ([error-message+loc (in-list error-messages+locs)]
                              [range (in-list (exn-info-src-vecs error-message+loc))])
                     (define pos (vector-ref range 0))
                     (define span (vector-ref range 1))
                     (srcloc (get-defs) #f #f pos span))))))
        (send (get-defs) end-edit-sequence))

      (define hide-bottom-bar-after-delay-timer (new timer%
                                                     [notify-callback
                                                      (lambda ()
                                                        (when (and (clean? running-status)
                                                                   (not (clean-error-messages+locs running-status)))
                                                          (send (get-defs) hide-module-language-error-panel)))]))

      (define/private (hide-bottom-bar-after-delay)
        (send hide-bottom-bar-after-delay-timer stop)
        (send hide-bottom-bar-after-delay-timer start 1000 #t))
      
      (define/private (update-bottom-bar)
        (cond
          [(running? running-status)
           (set-bottom-bar-status/blank)]
          [(and (dirty? running-status) our-turn?)
           (set-bottom-bar-status/blank)]
          [(and (dirty? running-status) (not our-turn?))
           (send (get-defs) set-bottom-bar-status (list (exn-info "" "" '() '() #f)) #f #f)]
          [(clean? running-status)
           (if (clean-error-messages+locs running-status)
               (send (get-defs) set-bottom-bar-status 
                     (clean-error-messages+locs running-status)
                     #t #t)
               (begin
                 (set-bottom-bar-status/blank)
                 (hide-bottom-bar-after-delay)))]))
      
      (define/private (set-bottom-bar-status/blank)
        (send (get-defs) set-bottom-bar-status
              (list (exn-info "" "" '() '() #f))
              #f
              #f))
      
      (define/public (update-little-dot)
        (when (eq? this (send (get-frame) get-current-tab))
          (define star?
            (and (clean? running-status)
                 (clean-compiled-code running-status)))
          (send (get-frame) frame-show-bkg-running (get-colors) (get-label) #:star? star?)))
      
      (define/private (get-colors)
        (cond
          [(running? running-status)
           (case (running-sym running-status)
             [(running) (list "blue")]
             [(finished-expansion) (list "purple")])]
          [(dirty? running-status)
           (if (null? bkg-colors)
               #f
               (map (λ (x) (list-ref x 1)) bkg-colors))]
          [(clean? running-status)
           (if (clean-error-messages+locs running-status)
               (list "red")
               (if (null? bkg-colors)
                   (list "forestgreen")
                   (map (λ (x) (list-ref x 1)) bkg-colors)))]))
      
      (define/private (get-label)
        (cond
          [(running? running-status)
           (list (running-str running-status))]
          [(dirty? running-status)
           (if (null? bkg-colors)
               #f
               (map (λ (x) (list-ref x 2)) bkg-colors))]
          [(clean? running-status)
           (if (clean-error-messages+locs running-status)
               (for/list ([pr (clean-error-messages+locs running-status)])
                 (exn-info-str pr))
               (list sc-finished-successfully))]))

      (define bkg-colors '())
      
      (define/public (add-bkg-running-color id color label)
        (set! bkg-colors
              (sort
               (cons (list id color label) bkg-colors)
               string<=? #:key (compose symbol->string car)))
        (update-little-dot))
      
      (define/public (remove-bkg-running-color id)
        (set! bkg-colors (filter (λ (x) (not (eq? (car x) id))) bkg-colors))
        (update-little-dot))
      
      (define dep-paths (set))
      (define error-dep-paths (set))
      (define/public (set-dep-paths d error?)
        (cond
          [error?
           (set! error-dep-paths d)]
          [else
           (set! dep-paths d)
           (set! error-dep-paths (set))]))
      (define/public (set-dirty-if-dep path)
        (when (or (set-member? dep-paths path)
                  (set-member? error-dep-paths path))
          (oc-set-dirty this)))

      (super-new)))
  
  (define module-language-online-expand-text-mixin
    (mixin (text:basic<%> 
            drracket:unit:definitions-text<%>
            drracket:module-language-tools:definitions-text<%>
            text:all-string-snips<%>) ()
      (inherit last-position get-top-level-window get-filename
               get-tab get-canvas invalidate-bitmap-cache 
               set-position get-start-position get-end-position
               highlight-range dc-location-to-editor-location
               begin-edit-sequence end-edit-sequence in-edit-sequence?
               save-port all-string-snips? find-first-snip
               get-port-name)

      (define/public (fetch-data-to-send)
        (define fn (get-port-name))
        (cond
          [(all-string-snips?)
           (define str (make-string (last-position) #\space))
           (fetch-string str)
           (values str fn)]
          [else
           (define bp (open-output-bytes))
           (save-port bp 'standard)
           (values (get-output-bytes bp) fn)]))

      ;; fetch-string : string -> void
      ;; EFFECT: fills in str
      ;; str's length must be the length of the buffer
      ;; all snips in the buffer must be string snips
      (define/private (fetch-string str)
        (let loop ([s (find-first-snip)]
                   [i 0])
          (cond
            [(not s) #f]
            [else
             (define size (send s get-count))
             (when str (send s get-text! str 0 size i))
             (loop (send s next) (+ i size))])))


      ;; the state of the bottom bar (when this definitions text is
      ;; in the current tab)
      ; if the bar is hidden entirely 
      (define error/status-message-hidden? #t) 
      ; a list of triples (in a vector of size 3) of strings, srclocs, and stackframes (as strings)
      ; that show up in the bar and control the "jump to error" / next prev buttons
      (define error/status-message-strs+srclocs (list (exn-info "" "" '() '() #f)))
      (define error/status-index 0)
      
      ; if the string should be red/italic or just normal font
      (define error/status-message-err? #f)
      
      (define bottom-bar-most-recent-jumped-to-loc #f)
      (define/public (set-bottom-bar-most-recent-jumped-to-loc loc)
        (set! bottom-bar-most-recent-jumped-to-loc loc)
        (update-frame-expand-error))
      
      (define/public (set-bottom-bar-status new-error/status-message-strs+srclocs
                                            message-err?
                                            force-visible?)
        (define new-error/status-message-str 
          (exn-info-str (car new-error/status-message-strs+srclocs)))
        (unless (string? new-error/status-message-str)
          (error 'set-bottom-bar-status "expected a string, got ~s" new-error/status-message-str))
        (when (or (not (and (equal? error/status-message-strs+srclocs 
                                    new-error/status-message-strs+srclocs)
                            (equal? error/status-message-err? message-err?)))
                  (and force-visible?
                       error/status-message-hidden?))
          (set! error/status-message-strs+srclocs new-error/status-message-strs+srclocs)
          (unless (< error/status-index (length new-error/status-message-strs+srclocs))
            ;; try to preserve the error/status-index in the case
            ;; that the error messages didn't change much.
            ;; not sure this is a good idea (or if the test above is the right test)
            (set! error/status-index 0))
          (set! error/status-message-err? message-err?)
          (when force-visible? 
            (set! error/status-message-hidden? #f))
          (update-frame-expand-error)))
      (define/public (update-frame-expand-error)
        (when (eq? (get-tab) (send (send (get-tab) get-frame) get-current-tab))
          (define (matching-srcloc error/status-message-str+srcloc)
            (for/or ([pos+span-vec (exn-info-src-vecs error/status-message-str+srcloc)])
              (define pos (vector-ref pos+span-vec 0))
              (define span (vector-ref pos+span-vec 1))
              (and (equal? (send (get-tab) get-defs)
                           (srcloc-source bottom-bar-most-recent-jumped-to-loc))
                   (equal? (srcloc-position bottom-bar-most-recent-jumped-to-loc)
                           pos)
                   (equal? (srcloc-span bottom-bar-most-recent-jumped-to-loc)
                           span))))
          (define msgs
            (cond
              [bottom-bar-most-recent-jumped-to-loc
               (for/list ([error/status-message-str+srcloc
                           (in-list error/status-message-strs+srclocs)]
                          #:when (matching-srcloc error/status-message-str+srcloc))
                 (exn-info-str error/status-message-str+srcloc))]
              [else
               (list (exn-info-str (list-ref error/status-message-strs+srclocs
                                             error/status-index)))]))
          (define install-suggestions
            (apply 
             append
             (for/list ([error/status-message-str+srcloc (in-list error/status-message-strs+srclocs)]
                        #:when (exn-info-missing-mods error/status-message-str+srcloc))
               (define missing-mods (exn-info-missing-mods error/status-message-str+srcloc))
               (with-handlers ([exn:fail? (λ (x) '())])
                 (pkg-catalog-suggestions-for-module missing-mods)))))
          (define has-missing-mods?
            (for/or ([error/status-message-str+srcloc (in-list error/status-message-strs+srclocs)]
                     #:when (exn-info-missing-mods error/status-message-str+srcloc))
              (define missing-mods (exn-info-missing-mods error/status-message-str+srcloc))
              (not (null? missing-mods))))
            
          (define (combine-msg vec)
            (define msg (exn-info-full-str vec))
            (define stack (exn-info-src-vecs vec))
            (apply
             string-append
             (cons
              msg
              (for/list ([stack stack])
                (format "\n  ~a" stack)))))
          (define copy-msg
            (cond
              [bottom-bar-most-recent-jumped-to-loc
               (apply
                string-append
                (add-between
                 (for/list ([error/status-message-str+srcloc
                             (in-list error/status-message-strs+srclocs)]
                            #:when (matching-srcloc error/status-message-str+srcloc))
                   (combine-msg error/status-message-str+srcloc))
                 "\n"))]
              [else
               (combine-msg (list-ref error/status-message-strs+srclocs error/status-index))]))
          (send (send (get-tab) get-frame) set-expand-error/status
                error/status-message-hidden?
                msgs
                copy-msg
                error/status-message-err?
                (cond
                  [(null? error/status-message-strs+srclocs) 0]
                  [(null? (cdr error/status-message-strs+srclocs))
                   (length (exn-info-src-vecs (car error/status-message-strs+srclocs)))]
                  [else
                   (for/sum ([error/status-message-str+srcloc
                              (in-list error/status-message-strs+srclocs)])
                     (max 1 (length (exn-info-src-vecs error/status-message-str+srcloc))))])
                (and has-missing-mods? install-suggestions))))
      (define/public (hide-module-language-error-panel)
        (set! error/status-message-hidden? #t)
        (update-frame-expand-error))

      (define/public (expand-error-next) 
        (define current-srclocs 
          (exn-info-src-vecs (list-ref error/status-message-strs+srclocs error/status-index)))
        (define candidates (filter (λ (error-message-srcloc) 
                                     (> (- (vector-ref error-message-srcloc 0) 1) 
                                        (get-end-position)))
                                   current-srclocs))
        (cond
          [(null? candidates)
           (jump-to-new-index (modulo (+ error/status-index 1) 
                                      (length error/status-message-strs+srclocs))
                              first)]
          [else
           (jump-to (car candidates))]))
      
      (define/public (expand-error-prev) 
        (define current-srclocs 
          (exn-info-src-vecs (list-ref error/status-message-strs+srclocs error/status-index)))
        (define candidates (filter (λ (error-message-srcloc) 
                                     (< (+ (vector-ref error-message-srcloc 0)
                                           (vector-ref error-message-srcloc 1)
                                           -1)
                                        (get-start-position)))
                                   current-srclocs))
        (cond
          [(null? candidates)
           (jump-to-new-index (modulo (- error/status-index 1) 
                                      (length error/status-message-strs+srclocs))
                              last)]
          [else
           (jump-to (last candidates))]))
      
      (define/private (jump-to-new-index new-error/status-index which)
        (set! error/status-index new-error/status-index)
        (define current-srclocs 
          (exn-info-src-vecs (list-ref error/status-message-strs+srclocs error/status-index)))
        (unless (null? current-srclocs) (jump-to (which current-srclocs)))
        (update-frame-expand-error))
      
      (define/private (jump-to vec)
        ;; when we get here, the only errors we see are from online expansion
        ;; and those errors' source locs are always in the definitions text
        (send (send (get-tab) get-ints) highlight-a-single-error
              (srcloc this
                      #f #f
                      (vector-ref vec 0)
                      (vector-ref vec 1)))
        (set-position (- (vector-ref vec 0) 1))
        (define cnvs (get-canvas))
        (when cnvs (send cnvs focus)))
      
      (define online-error-ranges '())
      (define online-highlighted-errors '())
      
      (define/public (set-gold-highlighted-errors ranges)
        (set! online-highlighted-errors 
              (for/list ([range (in-list ranges)]) 
                (define pos (vector-ref range 0))
                (define span (vector-ref range 1))
                (highlight-range (- pos 1) (+ pos span -1)
                                 'drracket:gold-error-background-highlighting))))
      
      (define/public (set-margin-error-ranges rngs)
        (unless (equal? online-error-ranges rngs)
          (invalidate-online-error-ranges)
          (set! online-error-ranges rngs)
          (invalidate-online-error-ranges)))
      
      (define/public (clear-old-error)
        (begin-edit-sequence #f #f)
        (for ([cleanup-thunk (in-list online-highlighted-errors)])
          (cleanup-thunk))
        (for ([an-error-range (in-list online-error-ranges)])
          (when (error-range-clear-highlight an-error-range)
            ((error-range-clear-highlight an-error-range))
            (set-error-range-clear-highlight! an-error-range #f)))
        (invalidate-online-error-ranges)
        (set-margin-error-ranges '())        
        (end-edit-sequence))
      
      (define/private (invalidate-online-error-ranges)
        (when (get-admin)
          ;; invalidate-online-error-ranges can be called at strange times
          ;; because it is invoked via a queue-callback thunk; specifically
          ;; the tab may have changed in drracket, which means that there is
          ;; no admin and thus there is no reason to invalidate any drawing
          (for ([an-error-range (in-list online-error-ranges)])
            (define-values (x y w h) (get-box an-error-range))
            (invalidate-bitmap-cache x y 'display-end h))))
      
      (define byt (box 0.0))
      (define byb (box 0.0))
      (define vbx (box 0.0))
      (define vby (box 0.0))
      (define vbw (box 0.0))
      (define vbh (box 0.0))
    
      (inherit position-location get-admin)
      
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (unless (null? online-error-ranges)
          (unless before?
            
            (define path (new dc-path%))
            (define found-something-to-draw? #f)
            
            (for ([an-error-range (in-list online-error-ranges)])
              (define-values (x y w h) (get-box an-error-range))
              (when (rectangles-intersect? x y (+ x w) (+ y h)
                                           left top right bottom)
                (set! found-something-to-draw? #t)
                (send path move-to (+ dx x) (+ dy y))
                (send path line-to (+ dx x w) (+ dy y))
                (send path line-to (+ dx x w) (+ dy y h))
                (send path arc (+ dx x) (+ dy y) (* w 2/3) h (* pi 3/2) (* pi 1/2))
                (send path close)))
            
            (when found-something-to-draw?
              (define saved-brush (send dc get-brush))
              (define saved-pen (send dc get-pen))
              (define smoothing (send dc get-smoothing))
              (send dc set-smoothing 'smoothed)
              
              (send dc set-brush "red" 'solid)
              (send dc set-pen "red" 1 'transparent)
              (send dc set-alpha 
                    (if (preferences:get 'framework:white-on-black?)
                        .5
                        .25))
              
              (send dc draw-path path)
              (send dc set-alpha 1)
              (send dc set-brush saved-brush)
              (send dc set-pen saved-pen)
              (send dc set-smoothing smoothing))))
        (super on-paint before? dc left top right bottom dx dy draw-caret))
      
      (define/override (on-event evt)
        (define-values (mx my) 
          (dc-location-to-editor-location 
           (send evt get-x)
           (send evt get-y)))
        (cond
          [(or (send evt moving?)
               (send evt entering?))
           (for ([an-error-range (in-list online-error-ranges)])
             (define-values (x y w h) (get-box an-error-range))
             (cond
               [(and (<= x mx (+ x w))
                     (<= y my (+ y h)))
                (unless (error-range-clear-highlight an-error-range)
                  (set-error-range-clear-highlight! 
                   an-error-range
                   (highlight-range (error-range-start an-error-range)
                                    (error-range-end an-error-range)
                                    "pink")))]
               [else
                (when (error-range-clear-highlight an-error-range)
                  ((error-range-clear-highlight an-error-range))
                  (set-error-range-clear-highlight! an-error-range #f))]))
           (super on-event evt)]
          [(send evt leaving?)
           (for ([an-error-range (in-list online-error-ranges)])
             (when (error-range-clear-highlight an-error-range)
               ((error-range-clear-highlight an-error-range))
               (set-error-range-clear-highlight! an-error-range #f)))
           (super on-event evt)]
          [(send evt button-down? 'left)
           (define used-click? #f)
           (for ([an-error-range (in-list online-error-ranges)])
             (define-values (x y w h) (get-box an-error-range))
             (when (and (<= x mx (+ x w))
                        (<= y my (+ y h)))
               (set! used-click? #t)
               (set-position (error-range-start an-error-range))))
           (unless used-click?
             (super on-event evt))]
          [else
           (super on-event evt)]))
      
      ;; pre: get-admin does not return #f
      (define/private (get-box an-error-range)
        (define start-pos (error-range-start an-error-range))
        (define end-pos (error-range-end an-error-range))
        (position-location start-pos #f byt)
        (position-location end-pos #f byb #f)
        (send (get-admin) get-view vbx vby vbw vbh)
        
        (define x (+ (unbox vbx)
                     (unbox vbw)
                     (- online-compilation-error-pen-width)
                     (- online-compilation-error-pen-width)))
        (define y (unbox byt))
        (define w (* online-compilation-error-pen-width 2))
        (define h (- (unbox byb) (unbox byt)))
        
        (values x y w h))
        
      (define need-to-dirty? #f)

      
      (define/augment (after-insert start end) 
        (if (in-edit-sequence?)
            (set! need-to-dirty? #t)
            (oc-set-dirty (get-tab)))
        (inner (void) after-insert start end))
      
      (define/augment (after-delete start end) 
        (if (in-edit-sequence?)
            (set! need-to-dirty? #t)
            (oc-set-dirty (get-tab)))
        (inner (void) after-delete start end))
      
      (define/augment (after-edit-sequence)
        (when need-to-dirty?
          (set! need-to-dirty? #f)
          (oc-set-dirty (get-tab)))
        (inner (void) after-edit-sequence))
      
      (define/augment (after-load-file success?)
        (when success? 
          (oc-set-dirty (get-tab)))
        (inner (void) after-load-file success?))
      
      (define/augment (after-set-next-settings new-settings)
        (oc-language-change (get-tab))
        (inner (void) after-set-next-settings new-settings))

      (define/augment (on-save-file filename format)
        (unless (equal? filename (get-filename))
          (unless (editor:doing-autosave?)
            (if (in-edit-sequence?)
                (set! need-to-dirty? #t)
                (oc-set-dirty (get-tab)))))
        (inner (void) on-save-file filename format))
      
      (define/augment (after-save-file success?)
        (when success?
          (define bx (box #f))
          (define path (get-filename bx))
          (when (and path
                     (not (unbox bx)))
            (for ([frame (in-list (send (group:get-the-frame-group) get-frames))])
              (when (is-a? frame drracket:unit:frame%)
                (for ([tab (in-list (send frame get-tabs))])
                  (send tab set-dirty-if-dep path))))))
        (inner (void) after-save-file success?))
      
      (super-new)))
  
  (define module-language-online-expand-rep-mixin
    (mixin (drracket:rep:text<%>) ()
      (inherit get-definitions-text)
      (define/override (on-highlighted-errors loc/s)
        (send (get-definitions-text) set-bottom-bar-most-recent-jumped-to-loc
              (and (not (list? loc/s)) loc/s))
        (super on-highlighted-errors loc/s))
      (super-new)))
  
  (define module-language-online-expand-frame-mixin
    (mixin (frame:basic<%> frame:info<%> drracket:unit:frame<%>) ()
      (inherit get-info-panel get-current-tab)
      
      (define expand-error-parent-panel #f)
      (define expand-error-panel #f)
      (define expand-error-message #f)
      (define expand-error-button-parent-panel #f)
      (define expand-error-install-suggestions-panel #f)
      (define expand-error-single-child #f)
      (define expand-error-multiple-child #f)
      (define expand-error-zero-child #f)
      
      (define expand-error-msgs #f)
      (define expand-error-msgs+stack "")
      (define expand-error-msg-is-err? #f)
      (define expand-error-srcloc-count 0)
      (define expand-error-hidden? #f)
      (define expand-error-install-suggestions '())

      ;; colors : (or/c #f (listof string?))
      (define colors #f)
      (define star? #f)
      (define tooltip-labels #f)
      (define/public (get-online-expansion-colors) colors)
      
      (super-new)
      
      (define/override (make-root-area-container cls parent)
        (set! expand-error-parent-panel
              (super make-root-area-container vertical-pane% parent))
        (define root (make-object cls expand-error-parent-panel))
        (set! expand-error-panel
              (new-horizontal-panel% 
                   [stretchable-height #f]
                   [parent expand-error-parent-panel]))
        
        (set! expand-error-message (new error-message%
                                        [parent expand-error-panel]
                                        [stretchable-width #t]
                                        [msgs '("hi")]
                                        [err? #f]))
        (set! expand-error-button-parent-panel
              (new-horizontal-panel%
                   [stretchable-width #f]
                   [stretchable-height #f]
                   [parent expand-error-panel]))
        (set! expand-error-install-suggestions-panel
              (new-horizontal-panel%
                   [stretchable-width #f]
                   [stretchable-height #f]
                   [parent expand-error-panel]))
        (set! expand-error-single-child
              (new button% 
                   [parent expand-error-button-parent-panel]
                   [stretchable-width #t]
                   [label (string-append
                           sc-jump-to-error
                           (case (car (get-default-shortcut-prefix))
                             [(cmd)
                              (if (equal? (system-type) 'macosx)
                                  " (⌘.)"
                                  "")]
                             [(control) " (Ctrl+.)"]
                             [else ""]))]
                   [font small-control-font]
                   [callback (λ (b evt) (send (send (get-current-tab) get-defs) expand-error-next))]))
        (set! expand-error-multiple-child
              (new-horizontal-panel% [parent expand-error-button-parent-panel]))
        (set! expand-error-zero-child
              (new-horizontal-panel% [parent expand-error-button-parent-panel]))
        (new button% 
             [label "<"]
             [font small-control-font]
             [callback (λ (b evt) (send (send (get-current-tab) get-defs) expand-error-prev))]
             [parent expand-error-multiple-child])
        (new message% 
             [parent expand-error-multiple-child]
             [label sc-jump-to-error])
        (new button% 
             [label ">"]
             [font small-control-font]
             [callback (λ (b evt) (send (send (get-current-tab) get-defs) expand-error-next))]
             [parent expand-error-multiple-child])
        (new close-icon% 
             [parent expand-error-panel]
             [callback (λ () (send (send (get-current-tab) get-defs)
                                   hide-module-language-error-panel))])
        
        ;; this canvas makes sure that the expand-error-panel always has the same height,
        ;; even when the contents of expand-error-button-parent-panel change. At this
        ;; point in the construction of the frame, everything is in there so we can
        ;; see how tall that is and then the spacer will insist it stays that high.
        (send expand-error-panel reflow-container)
        (define the-height (send expand-error-panel get-height))
        (define spacer-canvas (new (class panel%
                                     (define/override (container-size info)
                                       (values 0 the-height))
                                     (super-new 
                                      [stretchable-width #f]
                                      [stretchable-height #f]
                                      [parent expand-error-panel]))))
        
        (send expand-error-parent-panel change-children (λ (l) (remq expand-error-panel l)))
        root)

      (define/public (set-expand-error/status hidden? msgs msgs+stacks err?
                                              srcloc-count install-suggestions)
        (unless (and (equal? expand-error-hidden? hidden?)
                     (equal? expand-error-msgs msgs)
                     (equal? expand-error-msgs+stack msgs+stacks)
                     (equal? expand-error-msg-is-err? err?)
                     (equal? expand-error-srcloc-count srcloc-count))
          (set! expand-error-hidden? hidden?)
          (set! expand-error-msgs msgs)
          (set! expand-error-msgs+stack msgs+stacks)
          (set! expand-error-msg-is-err? err?)
          (set! expand-error-srcloc-count srcloc-count)
          (set! expand-error-install-suggestions install-suggestions)
          (when expand-error-message
            (send expand-error-parent-panel change-children
                  (λ (l) (if hidden?
                             (if (memq expand-error-panel l)
                                 (remq expand-error-panel l)
                                 l)
                             (if (memq expand-error-panel l)
                                 l
                                 (append l (list expand-error-panel))))))
            (send expand-error-message set-msgs 
                  expand-error-msgs expand-error-msg-is-err? expand-error-msgs+stack
                  (cond
                    [(not err?) '()]
                    [(= srcloc-count 0) '()]
                    [(= srcloc-count 1)
                     (list (list sc-jump-to-error
                                 (λ () (send (send (get-current-tab) get-defs) expand-error-next))))]
                    [else
                     (list
                      (list (string-constant jump-to-next-error-highlight-menu-item-label)
                                 (λ () (send (send (get-current-tab) get-defs) expand-error-next)))
                      (list (string-constant jump-to-prev-error-highlight-menu-item-label)
                            (λ () (send (send (get-current-tab) get-defs) expand-error-prev))))]))

            (send expand-error-install-suggestions-panel change-children (λ (l) '()))
            (when expand-error-install-suggestions
              (cond
                [(null? expand-error-install-suggestions) 
                 (new button% 
                      [parent expand-error-install-suggestions-panel]
                      [callback 
                       (λ (_1 _2)
                         (pkg-catalog-update-local/simple-status-dialog 
                          #:parent
                          (send expand-error-install-suggestions-panel get-top-level-window)))]
                      [font small-control-font]
                      [label (string-constant update-catalog)])]
                [else
                 (for ([suggestion-pkg (in-list expand-error-install-suggestions)])
                   (new button% 
                        [parent expand-error-install-suggestions-panel]
                        [callback 
                         (λ (_1 _2)
                           (install-pkg
                            (send expand-error-install-suggestions-panel get-top-level-window)
                            (lambda (thunk)
                              (parameterize ([error-display-handler 
                                              drracket:init:original-error-display-handler])
                                (thunk)))
                            #:package-to-offer suggestion-pkg))]
                        [font small-control-font]
                        [label (format (string-constant install-package-button) 
                                       suggestion-pkg)]))]))
            (send expand-error-button-parent-panel change-children
                  (λ (l)
                    (list (cond
                            [(not err?) expand-error-zero-child]
                            [(= srcloc-count 0) expand-error-zero-child]
                            [(= srcloc-count 1) expand-error-single-child]
                            [else expand-error-multiple-child])))))))
      
      (define/augment (on-tab-change from-tab to-tab)
        (oc-new-active)
        (send (send to-tab get-defs) update-frame-expand-error)
        (send to-tab update-little-dot)
        (inner (void) on-tab-change from-tab to-tab))

      (define/override (on-activate active?)
        (oc-new-active)
        (super on-activate active?))
      
      (define/public (frame-show-bkg-running new-colors labels #:star? [_new-star? #f])
        (define new-star? (and _new-star? #t))
        (unless (equal? tooltip-labels labels)
          (set! tooltip-labels labels)
          (update-tooltip))
        (unless (and (equal? new-colors colors)
                     (equal? new-star? star?))
          (set! colors new-colors)
          (set! star? new-star?)
          (send running-canvas refresh)))
     
      (define tooltip-frame #f)
      (define/private (show-tooltip)
        (define tooltip-labels-to-show
          (if (preferences:get 'drracket:online-compilation-default-on)
              tooltip-labels
              (list (string-constant online-expansion-is-disabled))))
        (cond
          [tooltip-labels-to-show
           (unless tooltip-frame
             (set! tooltip-frame (new tooltip-frame% [frame-to-track this])))
           (send tooltip-frame set-tooltip tooltip-labels-to-show)
           (define-values (rx ry) (send running-canvas client->screen 0 0))
           (define-values (cw ch) (send running-canvas get-client-size))
           (send tooltip-frame show-over rx ry cw ch #:prefer-upper-left? #t)]
          [else
           (when tooltip-frame
             (send tooltip-frame show #f))]))
      
      (define/private (update-tooltip)
        (when tooltip-frame
          (cond
            [(or tooltip-labels
                 (not (preferences:get 'drracket:online-compilation-default-on)))
             (when (send tooltip-frame is-shown?)
               ;; just call this, as it updates the tooltip label already
               (show-tooltip))]
            [else
             (send tooltip-frame show #f)])))
      
      (define/private (hide-tooltip)
        (when tooltip-frame
          (send tooltip-frame show #f)))

      (define ball-size 10)
              
      (define running-canvas
        (let ([tlw this])
          (new (class canvas%
                 (inherit get-dc popup-menu refresh get-client-size)
                 (define star-table (make-hash))
                 (define/private (get-star color)
                   (cond
                     [(hash-ref star-table color #f) => values]
                     [else
                      (define i (2:star-polygon 6 9 4 "solid" color))
                      (define bmp (make-bitmap (2:image-width i) (2:image-height i)))
                      (define bdc (make-object bitmap-dc% bmp))
                      (2:render-image i bdc 0 0)
                      (send bdc set-bitmap #f)
                      (hash-set! star-table color bmp)
                      bmp]))
                 (define/override (on-paint)
                   (let ([dc (get-dc)])
                     (define-values (colors-to-draw star?-to-draw)
                       (cond
                         [(not (in-module-language tlw)) (values #f #f)]
                         [(preferences:get 'drracket:online-compilation-default-on)
                          (values colors star?)]
                         [else (values (list "red") #f)]))
                     (when colors-to-draw
                       (when (list? colors-to-draw)
                         (define-values (cw ch) (get-client-size))
                         (define len (length colors-to-draw))
                         (send dc set-smoothing 'aligned)
                         (send dc set-pen "black" 1 'transparent)
                         (define old-clip (send dc get-clipping-region))
                         (define clipping-diameter (if star? (max cw ch) ball-size))
                         (for ([color (in-list colors-to-draw)]
                               [i (in-naturals)])
                           (define region (new region% [dc dc]))
                           (send region set-arc
                                 (- (/ cw 2) (/ clipping-diameter 2))
                                 (- (/ ch 2) (/ clipping-diameter 2))
                                 clipping-diameter clipping-diameter
                                 (+ (* pi 1/2) (* 2 pi (/ i len)))
                                 (+ (* pi 1/2) (* 2 pi (/ (+ i 1) len))))
                           (send dc set-clipping-region region)
                           (cond
                             [star?
                              (when color
                                (define the-star (get-star color))
                                (send dc draw-bitmap the-star
                                      (/ (- cw (send the-star get-width)) 2)
                                      (/ (- ch (send the-star get-height)) 2)
                                      'solid
                                      (send the-color-database find-color color)))]
                             [else
                              (if color
                                  (send dc set-brush color 'solid)
                                  (send dc set-brush "black" 'transparent))
                              (send dc draw-rectangle 0 0 cw ch)])
                           (send dc set-clipping-region old-clip))))))
                 (define cb-proc (λ (sym new-val)
                                   (set! colors #f)
                                   (refresh)))
                 (preferences:add-callback 'drracket:online-compilation-default-on cb-proc #t)
                 (define/override (on-event evt) 
                   (cond
                     [(not (in-module-language tlw)) (void)]
                     [(send evt button-down?)
                      (define menu (new popup-menu%))
                      (define on? (preferences:get 'drracket:online-compilation-default-on))
                      (new menu-item% 
                           [parent menu]
                           [label (if on?
                                      (string-constant disable-online-expansion)
                                      (string-constant enable-online-expansion))]
                           [callback
                            (λ args
                              (preferences:set 'drracket:online-compilation-default-on (not on?)))])
                      (popup-menu menu (send evt get-x) (send evt get-y))]
                     [(send evt entering?)
                      (show-tooltip)]
                     [(send evt leaving?)
                      (hide-tooltip)]))
                 (super-new [style '(transparent)]
                            [parent (get-info-panel)]
                            [stretchable-width #f]
                            [stretchable-height #f]
                            [min-width 10]
                            [min-height 10])
                 
                 (inherit min-width min-height)
                 (let ([dc (get-dc)])
                   (min-width ball-size)
                   (min-height ball-size))))))))
  
  (define error-message%
    (class canvas%
      (init-field msgs err? [copy-msg ""])
      (define menu-items '())
      (inherit refresh get-dc get-client-size popup-menu show is-shown?)
      (define/public (set-msgs _msgs _err? _copy-msg _menu-items)
        (unless (string? _copy-msg)
          (raise-argument-error 'error-message%::set-msgs
                                "string?"
                                2
                                _msgs _err? _copy-msg _menu-items))
        (set! msgs _msgs)
        (set! err? _err?)
        (set! copy-msg _copy-msg)
        (set! menu-items _menu-items)
        (define no-content-in-msgs?
          (or (equal? msgs '()) (equal? msgs '(""))))
        (cond
          [(and no-content-in-msgs? (not err?))
           (show #f)]
          [else
           (unless (is-shown?) (show #t))
           (set-the-height/dc-font (editor:get-current-preferred-font-size))
           (refresh)]))
      (define/override (on-event evt)
        (cond
          [(and (send evt button-down?) err?)
           (define m (new popup-menu%))
           (define itm (new menu-item% 
                            [label (string-constant copy-error-message)]
                            [parent m]
                            [callback
                             (λ (itm evt)
                               (send the-clipboard set-clipboard-string 
                                     copy-msg
                                     (send evt get-time-stamp)))]))
           (for ([item (in-list menu-items)])
             (new menu-item%
                  [label (list-ref item 0)]
                  [parent m]
                  [callback (λ (itm evt) ((list-ref item 1)))]))
           (popup-menu m 
                       (+ (send evt get-x) 1)
                       (+ (send evt get-y) 1))]
          [else
           (super on-event evt)]))
      (define/override (on-paint)
        (define dc (get-dc))
        (define-values (cw ch) (get-client-size))
        (send dc set-text-foreground (if (white-on-black-panel-scheme?)
                                         (if err? "pink" "white")
                                         (if err? "firebrick" "black")))
        (define-values (tot-th gap-space) (height/gap-space dc))
        (for/fold ([y (- (/ ch 2) (/ tot-th 2))]) ([msg (in-list msgs)])
          (define-values (tw th td ta) (send dc get-text-extent msg #f 'grapheme))
          (send dc draw-text msg 2 y 'grapheme)
          (+ y th gap-space)))
      (super-new [style '(transparent)])
      
      ;; need object to hold onto this function, so this is
      ;; intentionally a private field, not a method
      (define (font-size-changed-callback _ new-prefs)
        (define new-size (editor:font-size-pref->current-font-size new-prefs))
        (set-the-height/dc-font new-size)
        (refresh))
      (preferences:add-callback
       'framework:standard-style-list:font-size
       font-size-changed-callback
       #t)
      
      (define/private (set-the-height/dc-font font-size)
        (define dc (get-dc))
        (send dc set-font 
              (send the-font-list find-or-create-font
                    font-size
                    (send normal-control-font get-family)
                    (if err? 
                        'italic
                        (send normal-control-font get-style))
                    (send normal-control-font get-weight)
                    (send normal-control-font get-underlined)
                    (send normal-control-font get-smoothing)))
        (define top/bottom-padding 4)
        (define-values (tot-th gap-space) (height/gap-space dc))
        (min-height (+ top/bottom-padding (inexact->exact (ceiling tot-th)))))

      (define/private (height/gap-space dc)
        (define gap-space 2)
        (define tot-th-without-gap-space
          (for/sum ([msg (in-list msgs)])
            (define-values (tw th td ta) (send dc get-text-extent msg))
            th))
        (values (+ tot-th-without-gap-space
                   (* (max 0 (- (length msgs) 1)) gap-space))
                gap-space))
      
      (inherit min-height)
      (set-the-height/dc-font
       (editor:get-current-preferred-font-size))))
  
  ;; get-current-oc-state : -> (or/c tab #f) (or/c tab #f) (listof tab) (listof tab)
  ;; the tabs in the results are only those that are in the module language
  (define (get-current-oc-state)
    (define running-tab #f)
    (define dirty/pending-tab #f)
    (define dirty-tabs '())
    (define clean-tabs '())
    (for ([frame (in-list (send (group:get-the-frame-group) get-frames))])
      (when (is-a? frame drracket:unit:frame<%>)
        (for ([tab (in-list (send frame get-tabs))])
          (define ts (send tab get-oc-status))
          (when (tab-in-module-language tab)
            (cond
              [(dirty? ts)
               (cond
                 [(dirty-timer-pending? ts)
                  (when dirty/pending-tab 
                    (error 'get-current-oc-state "found multiple dirty/pending tabs"))
                  (set! dirty/pending-tab tab)]
                 [else
                  (set! dirty-tabs (cons tab dirty-tabs))])]
              [(running? ts)
               (when running-tab (error 'get-current-oc-state "found multiple running tabs"))
               (set! running-tab tab)]
              [(clean? ts)
               (set! clean-tabs (cons tab clean-tabs))])))))
    (values running-tab dirty/pending-tab dirty-tabs clean-tabs))
  
  (define (oc-check-invariant id)
    (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
    
    (define (tab->fn tab/f)
      (and (is-a? tab/f drracket:unit:tab%)
           (let ([fn (send (send tab/f get-defs) get-filename)])
             (if fn
                 (let-values ([(base name dir?) (split-path fn)])
                   (path->string name))
                 "untitled"))))
    (define (die str)
      (error 'oc-check-invariant "just after call to ~a: ~a"
             id
             str))
    
    (for ([frame (in-list (send (group:get-the-frame-group) get-frames))])
      (when (is-a? frame drracket:unit:frame<%>)
        (for ([tab (in-list (send frame get-tabs))])
          (define ts (send tab get-oc-status))
          (cond
            [(running? ts)
             (unless (eq? running-tab tab)
               (die (format "running-tab (~a) not the same as a tab with running status (~a)"
                            (tab->fn running-tab)
                            (tab->fn tab))))]
            [(and (dirty? ts)
                  (dirty-timer-pending? ts))
             (unless (eq? dirty/pending-tab tab)
               (die
                (format "dirty/pending-tab (~a) not the same as a tab with timer-pending status (~a)"
                        (tab->fn running-tab)
                        (tab->fn tab))))])))))
          

  
  (define (oc-maybe-start-something)
    (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
    (cond
      [running-tab
       (line-of-interest)
       (void)]
      [dirty/pending-tab 
       (line-of-interest)
       (void)]
      [else
       (define focus-tab (get-focus-tab))
       (define tab-to-start
         (cond
           [(member focus-tab dirty-tabs)
            (line-of-interest)
            focus-tab]
           [(null? dirty-tabs)
            (line-of-interest)
            #f]
           [else
            (line-of-interest)
            (argmax (λ (tab) (send tab get-last-touched)) 
                    dirty-tabs)]))
       (when tab-to-start
         (send tab-to-start set-oc-status (dirty #t))
         (postpone-oc-timer))]))
  
  ;; oc-stop-improper-tabs : -> void
  ;; if the focused tab is dirty, then stop anything that might
  ;; be pending or running in preparation to run the focused tab
  (define (oc-stop-improper-tabs)
    (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
    (define focus-tab (get-focus-tab))
    (line-of-interest)
    (when (and focus-tab (dirty? (send focus-tab get-oc-status)))
      (cond
        [running-tab
         (line-of-interest)
         (unless (eq? running-tab focus-tab)
           (line-of-interest)
           (stop-place-running)
           (send running-tab set-oc-status (dirty #f)))]
        [dirty/pending-tab 
         (line-of-interest)
         (unless (eq? dirty/pending-tab focus-tab)
           (line-of-interest)
           (send dirty/pending-tab set-oc-status (dirty #f))
           (send oc-timer stop))])))
    
  (define/oc-log (oc-remove-tab tab)
    (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
    (cond
      [(eq? tab running-tab)
       (line-of-interest)
       (stop-place-running)
       (send tab set-oc-status (clean #f #f #f))]
      [(eq? tab dirty/pending-tab)
       (line-of-interest)
       (send oc-timer stop)
       (send tab set-oc-status (clean #f #f #f))]
      [else
       (line-of-interest)
       (void)])
    (oc-maybe-start-something))
  
  (define/oc-log (oc-pref-set new-val)
    (cond
      [new-val
       (line-of-interest)
       (oc-maybe-start-something)]
      [else
       (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
       (when running-tab
         (line-of-interest)
         (stop-place-running)
         (send running-tab set-oc-status (dirty #f)))
       (when dirty/pending-tab
         (line-of-interest)
         (send dirty/pending-tab set-oc-status (dirty #f))
         (send oc-timer stop))]))
  
  (preferences:add-callback 
   'drracket:online-compilation-default-on
   (λ (_ new-val) (queue-callback (λ () (oc-pref-set new-val)))))
  
  (define/oc-log (oc-set-dirty tab)
    (when (preferences:get 'drracket:online-compilation-default-on)
      (when (tab-in-module-language tab)
        (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
        (cond
          [(eq? running-tab tab)
           (line-of-interest)
           (stop-place-running)
           (send tab set-oc-status (dirty #f))
           (oc-maybe-start-something)]
          [(eq? dirty/pending-tab tab)
           (line-of-interest)
           (postpone-oc-timer)]
          [else
           (line-of-interest)
           (send tab set-oc-status (dirty #f))
           (oc-stop-improper-tabs)
           (oc-maybe-start-something)]))))
  
  (define/oc-log (oc-new-active)
    (when (preferences:get 'drracket:online-compilation-default-on)
      (line-of-interest)
      (oc-stop-improper-tabs)
      (oc-maybe-start-something)))
  
  (define/oc-log (oc-language-change tab)
    (when (preferences:get 'drracket:online-compilation-default-on)
      (cond
        [(tab-in-module-language tab) 
         (line-of-interest)
         (send tab set-oc-status (dirty #f))
         (oc-stop-improper-tabs)
         (oc-maybe-start-something)]
        [else
         (line-of-interest)
         (define ts (send tab get-oc-status)) 
         (cond
           [(running? ts)
            (line-of-interest)
            (stop-place-running)
            (send tab set-oc-status (dirty #f))]
           [(and (dirty? ts)
                 (dirty-timer-pending? ts))
            (line-of-interest)
            (send tab set-oc-status (dirty #f))
            (send oc-timer stop)]
           [else
            (send tab set-oc-status (dirty #f))])
         (oc-maybe-start-something)])))

  (define/oc-log (oc-timer-expired)
    (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
    (when dirty/pending-tab
      (define-values (editor-contents filename/loc) 
        (send (send dirty/pending-tab get-defs) fetch-data-to-send))
      (cond
        [editor-contents
         (line-of-interest)
         (when running-tab
           (line-of-interest)
           (stop-place-running)
           (send running-tab set-oc-status (dirty #f)))
         (send dirty/pending-tab set-oc-status (running 'running sc-online-expansion-running))
         (define settings (tab-in-module-language dirty/pending-tab))
         (send-to-place editor-contents
                        filename/loc
                        (module-language-settings->prefab-module-settings settings)
                        (λ (res) (oc-finished res))
                        (λ (a b) (oc-status-message a b))
                        (λ (key val) (oc-monitor-value key val))
                        (get-currently-open-files)
                        (send dirty/pending-tab get-defs))]
        [else
         (line-of-interest)
         (send dirty/pending-tab set-oc-status
               (clean 'exn
                      (list (exn-info sc-only-raw-text-files-supported
                                      sc-only-raw-text-files-supported
                                      (list (vector (+ filename/loc 1) 1))
                                      '()
                                      #f))
                      #f))
         (oc-maybe-start-something)])))

  (define/oc-log (oc-finished res)    
    (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
    (when running-tab
      (cond
        [(eq? (vector-ref res 0) 'handler-results)
         (line-of-interest)
         ;; inform the installed handlers that something has come back
         (for ([key-val (in-list (vector-ref res 1))])
           (define that-key (list-ref key-val 0))
           (define val (list-ref key-val 1))
           (for ([o-e-h (in-list (drracket:module-language-tools:get-online-expansion-handlers))])
             (define this-key 
               (list (drracket:module-language-tools:online-expansion-handler-mod-path o-e-h)
                     (drracket:module-language-tools:online-expansion-handler-id o-e-h)))
             (when (equal? this-key that-key)
               (cond
                 [(drracket:module-language-tools:online-expansion-handler-monitor? o-e-h)
                  ((drracket:module-language-tools:online-expansion-handler-local-handler o-e-h)
                   (send running-tab get-defs)
                   drracket:module-language-tools:done)]
                 [else
                  ((drracket:module-language-tools:online-expansion-handler-local-handler o-e-h) 
                   (send running-tab get-defs)
                   val)]))))
         
         (send running-tab set-oc-status (clean #f #f (vector-ref res 3)))
         (send running-tab set-dep-paths (list->set (vector-ref res 2)) #f)]
        [else
         (line-of-interest)
         (send running-tab set-oc-status
               (clean (vector-ref res 0)
                      (if (equal? (vector-ref res 0) 'abnormal-termination)
                          (let ([msg
                                 (if (regexp-match #rx"memory" (vector-ref res 1))
                                     sc-abnormal-termination-out-of-memory
                                     sc-abnormal-termination)])
                            (list (exn-info msg msg '() '() #f)))
                          (vector-ref res 1))
                      #f))
         (send running-tab set-dep-paths (list->set (vector-ref res 2)) #t)])
      (oc-maybe-start-something)))
  
  (define/oc-log (oc-status-message sym str)
    (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
    (when running-tab
      (line-of-interest)
      (send running-tab set-oc-status (running sym str))))
  
  (define/oc-log (oc-monitor-value key val)
    (define-values (running-tab dirty/pending-tab dirty-tabs clean-tabs) (get-current-oc-state))
    (when running-tab
      (line-of-interest)
      (for ([handler (in-list (drracket:module-language-tools:get-online-expansion-handlers))])
        (when (equal? (list (drracket:module-language-tools:online-expansion-handler-mod-path handler)
                            (drracket:module-language-tools:online-expansion-handler-id handler))
                      key)
          (line-of-interest)
          ((drracket:module-language-tools:online-expansion-handler-local-handler handler)
           (send running-tab get-defs)
           val)))))
  
  ;; get-focus-tab : -> (or/c tab #f)
  (define (get-focus-tab)
    (define tlw (get-top-level-focus-window))
    (and (is-a? tlw drracket:unit:frame<%>)
         (send tlw get-current-tab)))

  (define oc-timer-running? #f)
  (define oc-timer (new (class timer%
                          (define/override (start msec just-once?)
                            (set! oc-timer-running? #t)
                            (super start msec just-once?))
                          (define/override (stop)
                            (set! oc-timer-running? #f)
                            (super stop))
                          (super-new [notify-callback (lambda () 
                                                        (set! oc-timer-running? #f)
                                                        (oc-timer-expired))]))))
  (define (postpone-oc-timer)
    (send oc-timer stop)
    (send oc-timer start 250 #t))
  
  (define expanding-place #f)
  (define pending-thread #f)
  (define pending-tell-the-tab-show-bkg-running #f)
  (define (set-pending-thread tttsbr pt) 
    (set! pending-thread pt)
    (set! pending-tell-the-tab-show-bkg-running tttsbr))

  ;; editor-contents : (or/c bytes? string?) -- if bytes?, then this is in wxme editor format
  ;;                     if string? then it is the contents of the editor as a string
  (define (send-to-place editor-contents 
                         port-name
                         prefab-module-settings 
                         show-results 
                         tell-the-tab-show-bkg-running
                         monitor-status
                         currently-open-files
                         defs)
    (unless expanding-place
      (set! expanding-place (dynamic-place expanding-place.rkt 'start))
      (place-channel-put expanding-place module-language-compile-lock)
      (place-channel-put
       expanding-place 
       (for/list ([o-e-h (in-list (drracket:module-language-tools:get-online-expansion-handlers))])
         (list (drracket:module-language-tools:online-expansion-handler-mod-path o-e-h)
               (drracket:module-language-tools:online-expansion-handler-id o-e-h)
               (drracket:module-language-tools:online-expansion-handler-monitor? o-e-h)))))
    
    (for ([o-e-h (in-list (drracket:module-language-tools:get-online-expansion-handlers))])
      (when (drracket:module-language-tools:online-expansion-handler-monitor? o-e-h)
        ((drracket:module-language-tools:online-expansion-handler-local-handler o-e-h)
         defs
         drracket:module-language-tools:start)))
    
    (set-pending-thread
     tell-the-tab-show-bkg-running
     (thread (λ () 
               (define-values (pc-in pc-out) (place-channel))
               (define-values (pc-status-drracket-place pc-status-expanding-place) (place-channel))
               (define to-send
                 (vector-immutable editor-contents
                                   port-name
                                   pc-in 
                                   prefab-module-settings
                                   pc-status-expanding-place
                                   currently-open-files))
               (place-channel-put expanding-place to-send)
               (define us (current-thread))
               (thread (λ () 
                         (define got-status-update (place-channel-get pc-status-drracket-place))
                         ;; when got-status-update isn't 'finished-expansion, then
                         ;; that means that expansion won't ever finish (due to
                         ;; an error or an aborted job)
                         (when (equal? got-status-update 'finished-expansion)
                           (queue-callback
                            (λ ()
                              (when (and (eq? us pending-thread)
                                         pending-tell-the-tab-show-bkg-running)
                                (pending-tell-the-tab-show-bkg-running
                                 'finished-expansion
                                 sc-online-expansion-running)))))))
               
               ;; this loop catches all responses but handles the monitor response
               ;; response here and keeps waiting for the actual result in that case.
               (let loop ()
                 (define res (place-channel-get pc-out))
                 (cond
                   [(equal? (vector-ref res 0) 'monitor-message)
                    (queue-callback
                     (λ ()
                       (when (getenv "PLTDRPLACEPRINT")
                         (printf "PLTDRPLACEPRINT: got monitor result back from the place\n"))
                       (monitor-status (vector-ref res 1) (vector-ref res 2))))
                    (loop)]
                   [else
                    (queue-callback
                     (λ ()
                       (when (eq? us pending-thread)
                         (set-pending-thread #f #f))
                       (when (getenv "PLTDRPLACEPRINT")
                         (printf "PLTDRPLACEPRINT: got results back from the place\n"))
                       (show-results res)))]))))))
  
  (define (stop-place-running)
    (when expanding-place
      (when pending-thread
        (place-channel-put expanding-place 'abort)
        (set-pending-thread #f #f))))
  
  (struct error-range (start end [clear-highlight #:mutable]))
  
  (define online-compilation-error-pen-width 8)
    
  (define module-language-put-file-mixin
    (mixin (text:basic<%>) ()
      (inherit get-text last-position get-character get-top-level-window)

      (define/override (put-file directory default-name)
        (let ([tlw (get-top-level-window)])
          (if (in-module-language tlw)
              (let ([module-default-filename (get-module-filename)])
                (super put-file directory module-default-filename))
              (super put-file directory default-name))))
      
      ;; returns the name after "(module " suffixed with .scm
      ;; in the beginning of the editor
      ;; or #f if the beginning doesn't match "(module "
      (define/private (get-module-filename)
        (let ([open-paren (skip-whitespace 0)])
          (or (match-paren open-paren "(")
              (match-paren open-paren "[")
              (match-paren open-paren "{"))))
      
      (define/private (match-paren open-paren paren)
        (and (matches open-paren paren)
             (let ([module (skip-whitespace (+ open-paren 1))])
               (and (matches module "module")
                    (let* ([end-module (+ module (string-length "module"))]
                           [filename-start (skip-whitespace end-module)]
                           [filename-end (skip-to-whitespace filename-start)])
                      (and (not (= filename-start end-module))
                           (string-append (get-text filename-start filename-end)
                                          ".rkt")))))))
      
      
      (define/private (matches start string)
        (let ([last-pos (last-position)])
          (let loop ([i 0])
            (cond
              [(and (i . < . (string-length string))
                    ((+ i start) . < . last-pos))
               (and (char=? (string-ref string i)
                            (get-character (+ i start)))
                    (loop (+ i 1)))]
              [(= i (string-length string)) #t]
              [else #f]))))
      
      (define/private (skip-whitespace start)
        (let ([last-pos (last-position)])
          (let loop ([pos start])
            (cond
              [(pos . >= . last-pos) last-pos]
              [else
               (let ([char (get-character pos)])
                 (cond
                   [(char-whitespace? char)
                    (loop (+ pos 1))]
                   [else pos]))]))))
      
      (define/private (skip-to-whitespace start)
        (let ([last-pos (last-position)])
          (let loop ([pos start])
            (cond 
              [(pos . >= . last-pos)
               last-pos]
              [(char-whitespace? (get-character pos))
               pos]
              [else
               (loop (+ pos 1))]))))
      
      (super-new)))
  
  (define defs/ints-font 
    (send the-font-list find-or-create-font 72 'swiss 'normal 'normal))
  
  (define big-defs/ints-label<%>
    (interface ()
      set-lang-wants-big-defs/ints-labels?))
  
  (define (mk-module-language-text-mixin id)
    (mixin (editor<%>) (big-defs/ints-label<%>)
      (inherit get-admin invalidate-bitmap-cache get-dc
               dc-location-to-editor-location)
      (define inside? #f)
      (define recently-typed? #f)
      (define fade-amount 1)
      (define lang-wants-big-defs/ints-labels? #f)

      (define recently-typed-timer 
        (new logging-timer%
             [notify-callback
              (λ ()
                (update-recently-typed #f)
                (unless (equal? fade-amount 1)
                  (cond
                    [inside? 
                     (set! fade-amount (+ fade-amount 1/10))
                     (send recently-typed-timer start 100 #t)]
                    [else
                     (set! fade-amount 1)])
                  (invalidate-bitmap-cache 0 0 'display-end 'display-end)))]))
      
      (define/public (set-lang-wants-big-defs/ints-labels? w?) 
        (unless (equal? lang-wants-big-defs/ints-labels? w?)
          (set! lang-wants-big-defs/ints-labels? w?)

          (send recently-typed-timer stop)  ;; reset the recently-typed timer so
          (set! fade-amount 1)              ;; that changing the language makes the
          (set! recently-typed? #f)         ;; labels appear immediately
          
          (invalidate-bitmap-cache 0 0 'display-end 'display-end)))
      
      (define/override (on-char evt)
        (when inside?
          (update-recently-typed #t)
          (set! fade-amount 0)
          (send recently-typed-timer stop)
          (when (and lang-wants-big-defs/ints-labels?
                     (preferences:get 'drracket:defs/ints-labels))
            (send recently-typed-timer start 10000 #t)))
        (super on-char evt))
      
      (define/private (update-recently-typed nv)
        (unless (equal? recently-typed? nv)
          (set! recently-typed? nv)
          (invalidate-bitmap-cache 0 0 'display-end 'display-end)))
      
      (define/override (on-event evt)
        (define new-inside?
          (cond
            [(send evt leaving?) #f]
            [else (preferences:get 'drracket:defs/ints-labels)]))
        (unless (equal? new-inside? inside?)
          (set! inside? new-inside?)
          (when lang-wants-big-defs/ints-labels?
            (invalidate-bitmap-cache 0 0 'display-end 'display-end)))
        (cond
          [(and lang-wants-big-defs/ints-labels?
                (preferences:get 'drracket:defs/ints-labels)
                (send evt button-down?)
                (get-admin))
           (define admin (get-admin))
           (define dc (get-dc))
           (define-values (tw th _1 _2) (send dc get-text-extent id defs/ints-font 'grapheme))
           (define-values (mx my) (dc-location-to-editor-location 
                                   (send evt get-x) (send evt get-y)))
           (send admin get-view bx by bw bh)
           (cond
             [(and (<= (- (unbox bw) tw) mx (unbox bw))
                   (<= (- (unbox bh) th) my (unbox bh)))
              (define menu (new popup-menu%))
              (new menu-item% 
                   [label (string-constant hide-defs/ints-label)]
                   [parent menu]
                   [callback (λ (x y)
                               (preferences:set 'drracket:defs/ints-labels #f))])
              (send admin popup-menu menu (+ (send evt get-x) 1) (+ (send evt get-y) 1))]
             [else
              (super on-event evt)])]
          [else (super on-event evt)]))
      
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (unless before?
          (when (and inside?
                     (not recently-typed?)
                     lang-wants-big-defs/ints-labels?)
            (define admin (get-admin))
            (when admin
              (define-values (tx ty tw th) (get-drawing-region admin dc))
              (define α (send dc get-alpha))
              (define fore (send dc get-text-foreground))
              (send dc set-font defs/ints-font)
              (when (rectangles-intersect?
                     left top right bottom
                     tx ty (+ tx tw) (+ ty th))
                (send dc set-text-foreground (if (white-on-black-panel-scheme?) "white" "black"))
                (send dc set-alpha (* fade-amount .5))
                (send dc draw-text id (+ dx tx) (+ dy ty) 'grapheme)
                (send dc set-alpha α)
                (send dc set-text-foreground fore))
              (send dc set-font defs/ints-font)))))

      (define/private (get-drawing-region admin dc)
        (send admin get-view bx by bw bh)
        (define-values (tw th _1 _2) (send dc get-text-extent id defs/ints-font))
        (define tx (+ (unbox bx) (- (unbox bw) tw)))
        (define ty (+ (unbox by) (- (unbox bh) th)))
        (values tx ty tw th))

      (define to-invalidate #f)
      (define/override (on-scroll-to)
        (super on-scroll-to)
        (define admin (get-admin))
        (when admin
          (define dc (get-dc))
          (define-values (tx ty tw th) (get-drawing-region admin dc))
          (set! to-invalidate (vector tx ty tw th))))
      (define/override (after-scroll-to)
        (super after-scroll-to)
        (when to-invalidate
          (invalidate-bitmap-cache
           (vector-ref to-invalidate 0)
           (vector-ref to-invalidate 1)
           (vector-ref to-invalidate 2)
           (vector-ref to-invalidate 3))
          (set! to-invalidate #f))
        (define admin (get-admin))
        (when admin
          (define dc (get-dc))
          (define-values (tx ty tw th) (get-drawing-region admin dc))
          (invalidate-bitmap-cache tx ty tw th)))
      
      (super-new)))
  
  (define bx (box 0))
  (define by (box 0))
  (define bw (box 0))
  (define bh (box 0))
  
  (define module-language-big-defs/ints-interactions-text-mixin
    (mk-module-language-text-mixin (string-constant interactions-window-label)))
  (define module-language-big-defs/ints-definitions-text-mixin
    (mk-module-language-text-mixin (string-constant definitions-window-label)))
  
  (define module-language-compile-lock (make-compile-lock))
  
  (define module-language-parallel-lock-client
    (compile-lock->parallel-lock-client
     module-language-compile-lock
     (current-custodian)
     current-parallel-lock-shutdown-evt))
  
  ;; in-module-language : (or/c top-level-window<%> #f) -> module-language-settings or #f
  (define (in-module-language tlw)
    (and tlw
         (is-a? tlw drracket:unit:frame<%>)
         (tab-in-module-language (send tlw get-current-tab))))
  
  ;; in-module-language : tab -> module-language-settings or #f
  (define (tab-in-module-language tab)
    (define defs (send tab get-defs))
    (define settings (send defs get-next-settings))
    (define mode (send defs get-current-mode))
    (and (is-a? (drracket:language-configuration:language-settings-language settings)
                module-language<%>)
         (drracket:modes:mode-intended-to-edit-programs? mode)
         (drracket:language-configuration:language-settings-settings settings)))
  
  (define (initialize-prefs-panel)
    (preferences:add-panel
     (string-constant online-expansion)
     (λ (parent)
       (define parent-vp (new-vertical-panel% 
                              [parent parent]
                              [alignment '(center top)]))
       (new-vertical-panel% [parent parent-vp])
       (define vp (new-vertical-panel% 
                       [parent parent-vp]
                       [stretchable-width #f]
                       [stretchable-height #f]
                       [alignment '(left center)]))
       (new-vertical-panel% [parent parent-vp])
       
       (define ((make-callback pref-sym) choice evt)
         (preferences:set pref-sym (index->pref (send choice get-selection))))
       (preferences:add-check (new-horizontal-panel% 
                                   [parent vp]
                                   [stretchable-height #f]
                                   [alignment '(center center)])
                              'drracket:online-compilation-default-on
                              (string-constant enable-online-expansion))
       (new-vertical-panel% [parent vp] [stretchable-height #f] [min-height 20])
       (define read-choice
         (new choice% 
              [parent vp]
              [label (string-constant online-expansion-show-read-errors-as)]
              [callback (make-callback 'drracket:online-expansion:read-in-defs-errors)]
              [choices (list (string-constant online-expansion-error-margin)
                             (string-constant online-expansion-error-gold-highlight))]))
       (define var-choice
         (new choice% 
              [parent vp]
              [label (string-constant online-expansion-show-variable-errors-as)]
              [callback (make-callback 'drracket:online-expansion:variable-errors)]
              [choices (list (string-constant online-expansion-error-margin)
                             (string-constant online-expansion-error-gold-highlight))]))
       (define other-choice
         (new choice% 
              [parent vp]
              [label (string-constant online-expansion-show-other-errors-as)]
              [callback (make-callback 'drracket:online-expansion:other-errors)]
              [choices (list (string-constant online-expansion-error-margin)
                             (string-constant online-expansion-error-gold-highlight))]))
       (define (connect-to-prefs choice pref-sym)
         (preferences:add-callback 
          pref-sym
          (λ (pref-sym nv) (send choice set-selection (pref->index nv))))
         (send choice set-selection (pref->index (preferences:get pref-sym))))
       (define (index->pref n)
         (case n
           [(0) 'margin]
           [(1) 'gold]))
       (define (pref->index p)
         (case p
           [(margin) 0]
           [(gold) 1]))
       (connect-to-prefs read-choice 'drracket:online-expansion:read-in-defs-errors)
       (connect-to-prefs var-choice 'drracket:online-expansion:variable-errors)
       (connect-to-prefs other-choice 'drracket:online-expansion:other-errors)
       (for ([f (in-list (drracket:module-language-tools:get-online-expansion-pref-funcs))])
         (f vp))
       parent-vp)))
  
  (define (get-currently-open-files)
    (for*/list ([frame (in-list
                        (send (group:get-the-frame-group) get-frames))]
                #:when (frame . is-a? . drracket:unit:frame%)
                [tab (in-list (send frame get-tabs))]
                [v (in-value (send (send tab get-defs) get-filename))]
                #:when v)
      v))
  
  (define modes<%> (interface () 
                     change-mode-to-match))
  
  (define modes-mixin
    (mixin ((class->interface text%)
            mode:host-text<%>
            drracket:unit:definitions-text<%>)
      (modes<%>)
          
      (inherit get-surrogate set-surrogate 
               get-next-settings get-tab)
      
      (define/augment (after-set-next-settings next-settings)
        (change-mode-to-match)
        (inner (void) after-set-next-settings next-settings))
      
      (define current-mode #f)
      
      (define/public (get-current-mode) current-mode)
      (define/public (set-current-mode mode)
        (set! current-mode mode)
        (define surrogate (drracket:modes:mode-surrogate mode))
        (set-surrogate surrogate)
        (define interactions-text (send (get-tab) get-ints))
        (when interactions-text
          (send interactions-text set-surrogate surrogate)
          (send interactions-text set-submit-predicate
                (drracket:modes:mode-repl-submit mode))))
      
      (define/public (is-current-mode? mode)
        (and current-mode
             (equal? (drracket:modes:mode-name current-mode) 
                     (drracket:modes:mode-name mode))))
      
      (define/public (change-mode-to-match)
        (let* ([language-settings (get-next-settings)]
               [language-name 
                (and language-settings
                     (send (drracket:language-configuration:language-settings-language
                            language-settings)
                           get-language-position))])
          (let loop ([modes (drracket:modes:get-modes)])
            (cond
              [(null? modes) (error 'change-mode-to-match
                                    "didn't find a matching mode")]
              [else (let ([mode (car modes)])
                      (if ((drracket:modes:mode-matches-language mode) language-name)
                          (unless (is-current-mode? mode)
                            (set-current-mode mode))
                          (loop (cdr modes))))]))))
      (super-new))))
