#lang racket/base
(require racket/class
         racket/draw
         racket/list
         racket/set
         compiler/cm
         setup/dirs
         planet/config
         pkg/lib
         framework/preferences
         (prefix-in *** '#%foreign) ;; just to make sure it is here
         (prefix-in el: errortrace/errortrace-lib))

(provide set-basic-parameters/no-gui
         set-module-language-parameters
         (struct-out prefab-module-settings)
         transform-module
         get-init-dir)

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
  

(define-logger drracket/cm)
(define (set-module-language-parameters settings 
                                        module-language-parallel-lock-client
                                        currently-open-files
                                        #:use-use-current-security-guard?
                                        [use-current-security-guard? #f])
  (current-command-line-arguments (prefab-module-settings-command-line-args settings))
  (let* ([default (current-library-collection-paths)]
         [cpaths (append-map (λ (x) (if (symbol? x) default (list x)))
                             (prefab-module-settings-collection-paths settings))])
    (when (null? cpaths)
      (eprintf "WARNING: your collection paths are empty!\n"))
    (current-library-collection-paths cpaths))
  
  (compile-context-preservation-enabled (prefab-module-settings-full-trace? settings))
  (compile-enforce-module-constants (prefab-module-settings-enforce-module-constants settings))
  (cond
    [(prefab-module-settings-compilation-on? settings)
     (define path->pkg-cache (make-hash))
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
     
     (define dirs-to-avoid-compiling-for-sure
       (let ([cd (find-collects-dir)])
         (append
          (list (CACHE-DIR))
          (if cd (list cd) null))))
     
     (define exploded-simplified-dirs-to-avoid-compiling-for-sure
       (for/list ([dir-to-avoid-compiling-for-sure
                   (in-list dirs-to-avoid-compiling-for-sure)])
         (explode-path (simplify-path dir-to-avoid-compiling-for-sure))))
     
     (define (use-errortrace-to-compile? path)
       (cond
         [(equal? (prefab-module-settings-annotations settings) 'debug)
          (define dir (explode-path (simplify-path path)))
          (cond
            [(for/or ([dir-to-avoid (in-list exploded-simplified-dirs-to-avoid-compiling-for-sure)])
               (dir-is-below? dir dir-to-avoid))
             #f]
            [else
             (define pkg (path->pkg path #:cache path->pkg-cache))
             (cond
               [(not pkg) #t] ;; compile files outside of the pkg system
               [(set-member? open-pkgs pkg) #t]
               [else #f])])]
         [else #f]))
     
     (define (dir-is-below? dir-eles dir-eles-to-avoid)
       (let loop ([dir-eles dir-eles]
                  [dir-eles-to-avoid dir-eles-to-avoid])
         (cond
           [(null? dir-eles-to-avoid) #t]
           [(null? dir-eles) #f]
           [else
            (and (equal? (car dir-eles) (car dir-eles-to-avoid))
                 (loop (cdr dir-eles) (cdr dir-eles-to-avoid)))])))
     
     (define errortrace-compiled-file-path (build-path "compiled" "drracket" "errortrace"))
     (define raw-compiled-file-path (build-path "compiled" "drracket"))
     
     (case (prefab-module-settings-annotations settings)
       [(debug)
        (define raw-errortrace-handler (el:make-errortrace-compile-handler))
        (define original-handler (current-compile))
        (define is-outermost? (make-parameter #t))
        (current-compile
         (λ (e immediate-eval?)
           (define clrd (current-load-relative-directory))
           (define use-et? (or (is-outermost?) (and clrd (use-errortrace-to-compile? clrd))))
           (parameterize ([is-outermost? #f])
             (cond
               [use-et?
                (raw-errortrace-handler e immediate-eval?)]
               [else
                (original-handler e immediate-eval?)]))))
        (current-path->mode
         (λ (path)
           (define use-et? (use-errortrace-to-compile? path))
           (cond
             [use-et? errortrace-compiled-file-path]
             [else raw-compiled-file-path])))
        (use-compiled-file-paths
         (list* errortrace-compiled-file-path
                raw-compiled-file-path
                (use-compiled-file-paths)))]
       [else
        (use-compiled-file-paths
         (cons raw-compiled-file-path (use-compiled-file-paths)))])
     
     ;; don't mess with .zo files in the installation or
     ;; in the PLaneT cache. If there are .zo files there
     ;; that are out of date or somehow problematic then
     ;; we will just let errors happen. Hopefully 'raco setup'
     ;; will clean things up in that case.
     (manager-skip-file-handler
      (let* ([cd (find-collects-dir)]
             [no-dirs (append
                       (list (CACHE-DIR))
                       (if cd (list cd) null))])
        (λ (p)
          (file-stamp-in-paths p no-dirs))))
     
     (when (and (pair? (use-compiled-file-paths))
                (equal? (car (use-compiled-file-paths))
                        errortrace-compiled-file-path))
       (current-load/use-compiled
        (let ([orig (current-load/use-compiled)]
              [the-security-guard (and use-current-security-guard? (current-security-guard))])
          (λ (path mod-name)
            (unless (use-errortrace-to-compile? path)
              (define-values (base name dir?) (split-path path))
              (define dont-want-zo
                (path-add-suffix (build-path base errortrace-compiled-file-path name) #".zo"))
              (parameterize ([current-security-guard (or the-security-guard
                                                         (current-security-guard))])
                (when (file-exists? dont-want-zo)
                  (delete-file dont-want-zo))))
            (orig path mod-name)))))
     
     ;; Install the compilation manager:
     (parallel-lock-client module-language-parallel-lock-client)
     (current-load/use-compiled
      (make-compilation-manager-load/use-compiled-handler
       #t
       #:security-guard (and use-current-security-guard? (current-security-guard))))]

    [else
     (case (prefab-module-settings-annotations settings)
       [(debug)
        (define is-outermost? (make-parameter #t))
        (define raw-errortrace-handler (el:make-errortrace-compile-handler))
        (define original-handler (current-compile))
        (current-compile
         (λ (e immediate-eval?)
           (define use-et? (is-outermost?))
           (parameterize ([is-outermost? #f])
             (cond
               [use-et?
                (raw-errortrace-handler e immediate-eval?)]
               [else
                (original-handler e immediate-eval?)]))))
        (use-compiled-file-paths
         (cons (build-path "compiled" "errortrace")
               (use-compiled-file-paths)))]
       [else
        (void)])]))

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
    (values name lang expr)))
