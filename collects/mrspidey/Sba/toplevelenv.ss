;; toplevelenv.ss
;; Handles the default top level environment
;;
;; ======================================================================
;; Primitive environments: symbol -> priminfo
;; First a helper fn

(define-structure (priminfo sym type domain-filters predicate-fn))

;; filter-domains is a list of filters for various args,
;; if no error raised

;; inj-predicate is a fn : list-AVS x list-AVS x AVS x bool -> AVS 
;; used for (if (pred ... x ...) ...)

(define (primitive->priminfo name type . attrs)
  (let ([type (expand-type type)])
    (make-priminfo
     name 
     type

     ;; Calculate filter-domains
     (match type
       [(domain '->* _)
        ;; Now walk domain and args
        (recur loop ([domain domain])
          (match domain
            [('cons domain domain-rest)
             (cons
              (let ([templates (con-exp->templates domain)])
                (if templates
                    (make-filter #t templates)
                    #f))
              (loop domain-rest))]
            [_ '()]))]
       [_ '()])

     ;; Calculate predicate-fn
     (foldl
      (lambda (attr predicate-fn)
        (let* 
            ([mk-predicate-fn
              (lambda (filter-then filter-else)
                (lambda (b a AVS sign)
                  (let ([filter (if sign filter-then filter-else)])
                    (if filter
                        (let ([nu-AVS (mk-AVS-tmp 'filter)])
                          (new-con! AVS
                                    (create-con-filter filter
                                                       nu-AVS))
                          nu-AVS)
                        AVS))))]
             [get-templates
              (lambda (C*) (map lookup-template C*))]
             [nu-fn
              (match attr
                [('predicate-fn exp) exp]
                [('predicate . C*)
                 (let ([templates (get-templates C*)])
                   (mk-predicate-fn (make-filter #t templates)
                                    (make-filter #f templates)))]
                [('predicate* true-info false-info)
                 (apply 
                  mk-predicate-fn
                  (map
                   (match-lambda
                    ['_ #f]
                    [(bool . C*)
                     (let ([templates (get-templates C*)])
                       (make-filter bool templates))]
                    [_ (error 'predicate* "Bad syntax")])
                   (list true-info false-info)))]
                [_ #f])])
          (if nu-fn
              ;; compose
              (lambda (b a AVS sign)
                (nu-fn b a
                       (predicate-fn b a AVS sign)
                       sign))
              predicate-fn)))
      (lambda (b a AVS bool) AVS)                 
      attrs))))

;; ----------------------------------------------------------------------
;; Now the default environment

(define default-primitive-env (void))
;; type: symbol -> priminfo

;; ======================================================================

(define output-type-expander (void))
(define (init-output-type-expander!)
  (set! output-type-expander (lambda (x) x)))

(define (install-output-type-expander! expander)
  (let ([old-expander output-type-expander])
    (set! output-type-expander
        (lambda (type) 
          (old-expander (recur loop ([type type])
                          (let ([t (expander type)])
                            (if (eq? t type) type (loop t)))))))))

;; ======================================================================
;; For input-type-expander, recursion is done in expand-type

(define default-input-type-expander (void))
(define input-type-expander (void))

(define (init-input-type-expander!)
  (set! input-type-expander default-input-type-expander))

(define (init-empty-input-type-expander!)
  (set! input-type-expander (lambda (x) x)))

(define (capture-input-type-expander!)
  (set! default-input-type-expander input-type-expander))

(define (install-input-type-expander! expander)
  (let ([old-expander input-type-expander])
    (set! input-type-expander
        (lambda (type) 
          ;;(pretty-print `(input-type-expander ,type))
          (old-expander (expander type))))))

;; ======================================================================

(define (extend-env-w/-define-structure env tag . args)
  ;; env: sym -> priminfo
  (let ([add! 
         (lambda (name type . attrs)
           (set! env
               (extend-env env name
                           (apply primitive->priminfo name type attrs))))]
        [type (symbol-append 'structure: tag)]
        [gen-args (map (lambda (_) (gensym)) args)]
        [gen-arg (gensym)])
    (apply add-constructor! type (map (lambda (a) #t) args))
    (add! (symbol-append 'make- tag) `(,@gen-args -> (,type ,@gen-args)))
    (add! (symbol-append tag '?) `(_ -> bool) `(predicate ,type))
    (for-each-with-n
     (lambda (arg n)
       (let ([type-ref
              `((,type ,@(map (lambda (a)
                                (if (eq? a arg) gen-arg '_))
                              args))
                -> ,gen-arg)]
             [type-bang 
              `((,type ,@(map (lambda (a)
                                (if (eq? a arg) `(! ,gen-arg) '_))
                              args))
                ,gen-arg
                -> void)])
         (add! (symbol-append tag '- arg) type-ref)
         (add! (symbol-append tag '- (add1 n)) type-ref)
         (add! (symbol-append 'set- tag '- arg '!) type-bang)
         (add! (symbol-append 'set- tag '- (add1 n) '!) type-bang)))
     args)
    env))

;; ======================================================================

(define (add-default-primitive! name type . attrs)
  ;; working w/ default primitive env => use default constructor env
  (init-current-constructor-env!)
  (set! default-primitive-env
      (extend-env default-primitive-env 
                  name
                  (apply primitive->priminfo name type attrs))))

(define (add-default-primitives! l)
  (for-each 
   (lambda (args) (apply add-default-primitive! args))
   l))

(define
  select-language
  (make-parameter-list
   'none
   '((DrScheme  "DrScheme")
     (MzScheme  "MzScheme")
     (R4RS      "R4RS")
     (Chez      "Chez Scheme")
     (Rice      "Rice Scheme")
     (none      "None"))
   (lambda (scheme)
     (unless (eq? scheme 'none)
       (init-default-constructor-env!)
       (init-empty-input-type-expander!)
       (init-output-type-expander!)
       (set! default-primitive-env empty-env)
       (init-R4RS!)
       (case scheme
         [MzScheme (init-MzScheme-on-R4RS!)]
         [DrScheme (init-MzScheme-on-R4RS!) 
                   (init-DrScheme-on-MzScheme!)]
         [R4RS     (void)]
         [Chez     (init-Chez-on-R4RS!)]
         [Rice     (init-Chez-on-R4RS!)
                   (init-Rice-on-Chez!)]
         [none     (void)])
       (when (st:numops) (init-smart-numops!))
       ;; Load .spideyrc file and .spidey-system-rc files
       (when (file-exists? "~/.spideyrc") (load "~/.spideyrc"))
       (let ([systemrc (string-append "~/.spidey-" 
                                      (symbol->string scheme)
                                      "-rc")])
         (when (file-exists? systemrc) (load systemrc)))
       (capture-input-type-expander!)))))

;; ======================================================================

