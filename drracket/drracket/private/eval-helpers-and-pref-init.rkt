#lang racket/base
(require racket/class
         racket/draw
         racket/list
         racket/unit
         racket/set
         compiler/cm
         setup/dirs
         planet/config
         pkg/lib
         framework/preferences
         errortrace/stacktrace
         errortrace/errortrace-key
         (prefix-in *** '#%foreign) ;; just to make sure it is here
         "compiled-dir.rkt")

(provide set-basic-parameters/no-gui
         set-module-language-parameters
         (struct-out prefab-module-settings)
         transform-module
         get-init-dir
         should-annotate?
         make-with-mark
         make-debug-compile-handler/errortrace-annotate
         current-parallel-lock-shutdown-evt)

(preferences:set-default 'drracket:child-only-memory-limit
                         (* 1024 1024 128)
                         (λ (x) (or (not x)
                                    (and (exact-integer? x)
                                         (x . >= . (* 1024 1024 8))))))

;; get-init-dir : (or/c path? #f) -> path?
;; returns the initial directory for a program
;; that is saved in 'path/f' (with #f indicating
;; an unsaved file)
(define (get-init-dir path/f)
  (cond
    [path/f
     (let-values ([(base name dir?) (split-path path/f)])
       base)]
    [else
     (find-system-path 'home-dir)]))

(struct prefab-module-settings
  (command-line-args
   collection-paths
   compilation-on?
   full-trace?
   annotations
   enforce-module-constants)
  #:prefab)

(define orig-namespace (current-namespace))

(define (set-basic-parameters/no-gui)
  (let ([cust (current-custodian)])
    (define (drracket-plain-exit-handler arg)
      (custodian-shutdown-all cust))
    (exit-handler drracket-plain-exit-handler))
  (read-accept-reader #f)
  (read-accept-lang #t)
  (read-accept-compiled #f)
  (current-thread-group (make-thread-group))
  (current-command-line-arguments #())
  (current-pseudo-random-generator (make-pseudo-random-generator))
  (current-evt-pseudo-random-generator (make-pseudo-random-generator))
  (read-curly-brace-as-paren #t)
  (read-square-bracket-as-paren #t)
  (error-print-width 250)
  (current-ps-setup (make-object ps-setup%))
  (current-namespace (make-base-empty-namespace))
  ;; is this wise?
  #;(namespace-attach-module orig-namespace ''#%foreign))

;; Use this parameter when creating a parallel-lock client,
;; so locks can be released when the user custodian is shut down.
;; The `set-module-language-parameters` function sets this
;; parameter on the assumption at the current custodian at that
;; point is the user custodian.
(define current-parallel-lock-shutdown-evt (make-parameter never-evt))

(define-logger drracket/cm)
(define (set-module-language-parameters settings 
                                        module-language-parallel-lock-client
                                        currently-open-files
                                        #:use-use-current-security-guard? [use-current-security-guard? #f])
  (current-command-line-arguments (prefab-module-settings-command-line-args settings))
  (let* ([default (current-library-collection-paths)]
         [cpaths (append-map (λ (x) (if (symbol? x) default (list x)))
                             (prefab-module-settings-collection-paths settings))])
    (when (null? cpaths)
      (eprintf "WARNING: your collection paths are empty!\n"))
    (current-library-collection-paths cpaths))
  
  (compile-context-preservation-enabled (prefab-module-settings-full-trace? settings))
  (compile-enforce-module-constants (prefab-module-settings-enforce-module-constants settings))
  (define path->pkg-cache (make-hash))
  (when (prefab-module-settings-compilation-on? settings)
    (define pkg-directory-cache (make-hash))
    (define (pkg-directory/use-cache pkg)
      (cond
        [(hash-ref pkg-directory-cache pkg #f)
         =>
         values]
        [else
         (define ans (pkg-directory pkg))
         (hash-set! pkg-directory-cache pkg ans)
         ans]))
    
    (define open-pkgs
      (for/fold ([s (set)]) ([path (in-list currently-open-files)])
        (define pkg (path->pkg path #:cache path->pkg-cache))
        (if (and pkg
                 (memq 'write
                       (file-or-directory-permissions (pkg-directory/use-cache pkg))))
            (set-add s pkg)
            s)))
    (for ([pkg (in-set open-pkgs)])
      (log-info "DrRacket: enabling bytecode-file compilation for package ~s" pkg))
    
    (define skip-path?
      (let* ([cd (find-collects-dir)]
             [sd (find-share-dir)]
             [no-dirs (append
                       (list (CACHE-DIR))
                       (if cd (list cd) null)
                       (if sd (list sd) null))])
        (λ (p)
          (define skip-in-paths? (file-stamp-in-paths p no-dirs))
          (define skip-pkgs?
            (let ([pkg (path->pkg p #:cache path->pkg-cache)])
              (and pkg
                   (not (set-member? open-pkgs pkg))
                   (file-stamp-in-paths p (list (pkg-directory/use-cache pkg))))))
          (log-drracket/cm-info "~a; skip? ~a ~a thd ~a"
                                p
                                (and skip-in-paths? #t)
                                (and skip-pkgs? #t)
                                (eq-hash-code (current-thread)))
          (or skip-in-paths?
              skip-pkgs?))))
    
    (define extra-compiled-file-path
      (case (prefab-module-settings-annotations settings)
        [(none) (build-path compiled-dir "drracket")]
        [(debug) (build-path compiled-dir "drracket" "errortrace")]
        [else #f]))
    (when extra-compiled-file-path
      ;; Add extra compiled-file path:
      (use-compiled-file-paths
       (cons extra-compiled-file-path
             (use-compiled-file-paths)))
      ;; If we ever skip a file, then don't use the extra compiled-file
      ;; path for the skipped file's dependencies (because modules
      ;; compiled against the non-DrRacket-generated bytecode might not
      ;; work with any DrRacket-generated bytecode that is sitting around):
      (current-load/use-compiled
       (let ([orig (current-load/use-compiled)])
         (lambda (path mod-name)
           (if (and (member extra-compiled-file-path (use-compiled-file-paths))
                    (skip-path? path))
               (parameterize ([use-compiled-file-paths
                               (remove extra-compiled-file-path 
                                       (use-compiled-file-paths))])
                 (orig path mod-name))
               (orig path mod-name))))))
    ;; Install the compilation manager:
    (current-parallel-lock-shutdown-evt (make-custodian-box (current-custodian) #t))
    (parallel-lock-client module-language-parallel-lock-client)
    (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler 
                                #t
                                #:security-guard (and use-current-security-guard?
                                                      (current-security-guard))))
    (manager-skip-file-handler skip-path?)))

(define (transform-module filename stx raise-hopeless-syntax-error)
  (define-values (mod name lang body)
    (syntax-case stx ()
      [(module name lang . body)
       (eq? 'module (syntax-e #'module))
       (values #'module #'name #'lang #'body)]
      [_ (raise-hopeless-syntax-error
          (string-append "only a module expression is allowed, either\n"
                         "    #lang <language-name>\n or\n"
                         "    (module <name> <language> ...)\n")
          stx)]))
  (define name* (syntax-e name))
  (unless (symbol? name*)
    (raise-hopeless-syntax-error "bad syntax in name position of module"
                                 stx name))
  (let* (;; rewrite the module to use the racket/base version of `module'
         [mod  (datum->syntax #'here 'module mod)]
         [expr (datum->syntax stx `(,mod ,name ,lang . ,body) stx stx)])
    (values name* (syntax->datum lang) expr)))


  ;; inputs to stacktrace/errortrace-annotate@ that are used both
  ;; in debug.rkt and also for online expansion

;; make-with-mark : (any/c -> any/c) -> mark-stx syntax natural -> syntax
;; the result of the first application should be bound to `with-mark`,
;; a member of stacktrace-imports^
(define ((make-with-mark special-source-handling) src-stx expr phase)
  (cond
    [(should-annotate? expr phase)
     (define source
       (cond
         [(path? (syntax-source src-stx))
          (syntax-source src-stx)]
         [(special-source-handling (syntax-source src-stx)) => values]
         [else #f]))
     (define position (or (syntax-position src-stx) 0))
     (define span (or (syntax-span src-stx) 0))
     (define line (or (syntax-line src-stx) 0))
     (define column (or (syntax-column src-stx) 0))
     (with-syntax ([expr expr]
                   [mark (vector source line column position span)]
                   [et-key (syntax-shift-phase-level #'errortrace-key phase)]
                   [wcm (syntax-shift-phase-level #'with-continuation-mark phase)]
                   [qte (syntax-shift-phase-level #'quote phase)])
       (syntax
        (wcm et-key
             (qte mark)
             expr)))]
    [else expr]))

(define (should-annotate? s phase)
  (and (syntax-source s)
       (syntax-property s 'errortrace:annotate)))


(define (make-debug-compile-handler/errortrace-annotate orig errortrace-annotate)
  (define reg (namespace-module-registry (current-namespace)))
  (define phase (namespace-base-phase (current-namespace)))
  (define (drracket-debug-compile-handler e immediate-eval?)
    (orig
     (if (and (eq? reg
                   (namespace-module-registry (current-namespace)))
              (or (equal? phase
                          (namespace-base-phase (current-namespace)))
                  ;; annotate a module at any phase
                  (syntax-case e ()
                    [(mod . _)
                     (and (identifier? #'mod)
                          (free-identifier=? #'mod #'module (namespace-base-phase (current-namespace)) phase))]
                    [_ #f]))
              (not (compiled-expression? (if (syntax? e)
                                             (syntax-e e)
                                             e))))
         (errortrace-annotate
          (if (syntax? e)
              e
              (namespace-syntax-introduce
               (datum->syntax #f e))))
         e)
     immediate-eval?))
  drracket-debug-compile-handler)
