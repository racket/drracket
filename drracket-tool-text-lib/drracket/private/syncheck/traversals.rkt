#lang racket/base

(require "colors.rkt"
         "syncheck-intf.rkt"
         "syncheck-local-member-names.rkt"
         "annotate.rkt"
         "contract-traversal.rkt"
         "xref.rkt"
         string-constants
         racket/match
         racket/set
         racket/class
         racket/list
         racket/contract
         racket/pretty
         racket/path
         racket/dict
         syntax/id-table
         scribble/manual-struct
         (for-syntax racket/base))

(define-logger check-syntax)

(provide make-traversal
         current-max-to-send-at-once
         build-trace%)

(define current-max-to-send-at-once (make-parameter +inf.0))

;; make-traversal : namespace string[directory] -> (values (syntax (union #f syntax) -> void)
;;                                                         (-> void))
;; returns a pair of functions that close over some state that
;; represents the top-level of a single program. The first value
;; is called once for each top-level expression and the second
;; value is called once, after all expansion is complete.
(define (make-traversal user-namespace user-directory [print-extra-info? #f])
  (let* ([tl-phase-to-binders (make-hash)]
         [tl-phase-to-varrefs (make-hash)]
         [tl-phase-to-varsets (make-hash)]
         [tl-phase-to-tops (make-hash)]
         [tl-binding-inits (make-hash)]
         [tl-templrefs (make-id-set (syntax-local-phase-level))]
         [tl-module-lang-requires (make-hash)]
         [tl-phase-to-requires (make-hash)]
         [tl-sub-identifier-binding-directives (make-hash)]
         [user-directory (and user-directory (simple-form-path user-directory))]
         [expanded-expression
          (λ (sexp [ignored void])
            (parameterize ([current-directory (or user-directory (current-directory))]
                           [current-load-relative-directory user-directory])
              (define is-module?
                (syntax-case sexp (module)
                  [(module . rest) #t]
                  [_ #f]))
              (cond
                [is-module?
                 (define phase-to-binders (make-hash))
                 (define phase-to-varrefs (make-hash))
                 (define phase-to-varsets (make-hash))
                 (define phase-to-tops (make-hash))
                 (define phase-to-requires (make-hash))
                 (define binding-inits (make-hash))
                 (define templrefs (make-id-set 0))
                 (define module-lang-requires (make-hash))
                 (define requires (make-hash))
                 (define require-for-syntaxes (make-hash))
                 (define require-for-templates (make-hash))
                 (define require-for-labels (make-hash))
                 (define sub-identifier-binding-directives (make-hash))
                 (annotate-basic sexp
                                 user-namespace
                                 user-directory
                                 phase-to-binders
                                 phase-to-varrefs
                                 phase-to-varsets
                                 phase-to-tops
                                 binding-inits
                                 templrefs
                                 module-lang-requires
                                 phase-to-requires
                                 sub-identifier-binding-directives)
                 (annotate-variables user-namespace
                                     user-directory
                                     phase-to-binders
                                     phase-to-varrefs
                                     phase-to-varsets
                                     phase-to-tops
                                     templrefs
                                     module-lang-requires
                                     phase-to-requires
                                     sub-identifier-binding-directives)
                 (annotate-contracts sexp
                                     (hash-ref phase-to-binders 0 (λ () (make-id-set 0)))
                                     (hash-ref binding-inits 0 (λ () (make-id-set 0)))
                                     binder+mods-binder)
                 (when print-extra-info?
                   (print-extra-info (list (list 'phase-to-binders phase-to-binders)
                                           (list 'phase-to-varrefs phase-to-varrefs)
                                           (list 'phase-to-varsets phase-to-varsets)
                                           (list 'phase-to-tops phase-to-tops)
                                           (list 'phase-to-requires phase-to-requires)
                                           (list 'binding-inits binding-inits)
                                           (list 'templrefs templrefs)
                                           (list 'module-lang-requires module-lang-requires)
                                           (list 'requires requires)
                                           (list 'require-for-syntaxes require-for-syntaxes)
                                           (list 'require-for-templates require-for-templates)
                                           (list 'require-for-labels require-for-labels)
                                           (list 'sub-identifier-binding-directives
                                                 sub-identifier-binding-directives))))]
                [else
                 (annotate-basic sexp
                                 user-namespace
                                 user-directory
                                 tl-phase-to-binders
                                 tl-phase-to-varrefs
                                 tl-phase-to-varsets
                                 tl-phase-to-tops
                                 tl-binding-inits
                                 tl-templrefs
                                 tl-module-lang-requires
                                 tl-phase-to-requires
                                 tl-sub-identifier-binding-directives)])))]
         [expansion-completed
          (λ ()
            (parameterize ([current-directory (or user-directory (current-directory))]
                           [current-load-relative-directory user-directory])
              (when print-extra-info?
                (print-extra-info (list (list 'tl-phase-to-binders tl-phase-to-binders)
                                        (list 'tl-phase-to-varrefs tl-phase-to-varrefs)
                                        (list 'tl-phase-to-varsets tl-phase-to-varsets)
                                        (list 'tl-phase-to-tops tl-phase-to-tops)
                                        (list 'tl-templrefs tl-templrefs)
                                        (list 'tl-module-lang-requires tl-module-lang-requires)
                                        (list 'tl-phase-to-requires tl-phase-to-requires)
                                        (list 'tl-sub-identifier-binding-directives
                                              tl-sub-identifier-binding-directives))))
              (annotate-variables user-namespace
                                  user-directory
                                  tl-phase-to-binders
                                  tl-phase-to-varrefs
                                  tl-phase-to-varsets
                                  tl-phase-to-tops
                                  tl-templrefs
                                  tl-module-lang-requires
                                  tl-phase-to-requires
                                  tl-sub-identifier-binding-directives)))])
    (values expanded-expression expansion-completed)))


(define (print-extra-info stuff)
  (for ([info (in-list stuff)])
    (printf "~s\n" (car info))
    (pretty-print
     (let loop ([info (cadr info)])
       (cond
         [(hash? info)
          (for/hash ([(k v) (in-hash info)])
            (values (loop k) (loop v)))]
         [(free-id-table? info)
          (for/list ([(k v) (in-dict info)])
            (list (loop k) '=> (loop v)))]
         [(pair? info) (cons (loop (car info)) (loop (cdr info)))]
         [else info])))
    (newline)))

;; type req/tag = (make-req/tag syntax sexp boolean)
(define-struct req/tag (req-stx req-sexp used?))

;; enclosing-bindings-visible? : boolean -- indicates of the bindings of
;;    the containing module are visible inside this module (ie this
;;    module was declared with a #f for the language (or via module+)
;; name : symbol -- the name of the module
(struct submodule (enclosing-bindings-visible? name) #:transparent)

;; binder : syntax?
;; mods : same contract as `mods` in level+tail+mod-loop
(struct binder+mods (binder mods) #:transparent)

;; ids : (listof syntax?)
;; in? : boolean? -- indicates if `ids` are the only ones included (#t) or if they are excluded (#f)
;; prefix : (or/c #f syntax?)
;; space : (or/c #f symbol)
;; b+m : binder+mods?
;;   -- INVARIANT: if prefix is syntax?, then in? must be #f
(struct require-context (ids in? prefix space b+m) #:transparent)

;; annotate-basic :
;;   stx-obj: syntax?
;;   user-namespace: namespace?
;;   user-directory: (or/c #f (and/c path? complete-path?))
;;   phase-to-binders:
;;      hash[(list/c (or/c integer? #f) -- level
;;                   mods) -- same contract as `mods` in level+tail+mod-loop
;;           -o>
;;           free-id-table[identifier -o> (listof binder+mods?)]]
;;   phase-to-varrefs, phase-to-varsets, phase-to-tops, binding-inits, templrefs:
;;      hash[(list/c (or/c integer? #f) -- level
;;                   mods) -- same contract as `mods` in level+tail+mod-loop
;;           -o>
;;           free-id-table[identifier -o> (listof syntax?)]]
;;    module-lang-requires:
;;      hash-table[(list/c src pos span) -o> #t]
;;    phase-to-requires:
;;      hash[(list/c (or/c integer? #f) -- level
;;                   mods) -- same contract as `mods` in level+tail+mod-loop
;;           -o>
;;           hash[sexp -o> (listof binder+mods?)]]
;;    sub-identifier-binding-directives: ?
;; -> void
(define (annotate-basic stx-obj
                        user-namespace user-directory
                        phase-to-binders
                        phase-to-varrefs
                        phase-to-varsets
                        phase-to-tops
                        binding-inits
                        templrefs
                        module-lang-requires
                        phase-to-requires
                        sub-identifier-binding-directives)

  (let level+tail+mod-loop ([stx-obj stx-obj]
                            [level 0]
                            [level-of-enclosing-module 0]
                            [tail-parent-src #f]
                            [tail-parent-pos #f]
                            ;; mods: (or/c #f   ; => outside a module
                            ;;             '()  ; => inside the main module in this file
                            ;;             (non-emptylistof submodule?)
                            ;;                  ; => inside some submodules
                            [mods #f])
    (define-values (next-tail-parent-src next-tail-parent-pos)
      (let ([child-src (find-source-editor stx-obj)]
            [child-pos (syntax-position stx-obj)]
            [defs-text (current-annotations)])
        (cond
          [(and child-src child-pos defs-text)
           (when (and tail-parent-src tail-parent-pos)
             (unless (and (eq? tail-parent-src child-src)
                          (equal? tail-parent-pos child-pos))
               (send defs-text syncheck:add-tail-arrow
                     tail-parent-src (- tail-parent-pos 1)
                     child-src (- child-pos 1))))
           (values child-src child-pos)]
          [else
           (values tail-parent-src tail-parent-pos)])))
    (define (sub-mods mod) (if mods (cons mod mods) '()))
    (let* ([level-loop (λ (sexp level) (level+tail+mod-loop sexp level level-of-enclosing-module
                                                            #f #f
                                                            mods))]
           [tail-loop (λ (sexp) (level+tail+mod-loop sexp level level-of-enclosing-module
                                                     next-tail-parent-src next-tail-parent-pos
                                                     mods))]
           [mod-loop (λ (sexp mod) (level+tail+mod-loop sexp 0
                                                        (+ level level-of-enclosing-module)
                                                        #f #f
                                                        (sub-mods mod)))]
           [loop (λ (sexp) (level+tail+mod-loop sexp level level-of-enclosing-module
                                                #f #f mods))]
           [varrefs (lookup-phase-to-mapping phase-to-varrefs
                                             (list (+ level level-of-enclosing-module) mods)
                                             (+ level level-of-enclosing-module))]
           [varsets (lookup-phase-to-mapping phase-to-varsets
                                             (+ level level-of-enclosing-module))]
           [binders (lookup-phase-to-mapping phase-to-binders
                                             (+ level level-of-enclosing-module))]
           [tops (lookup-phase-to-mapping phase-to-tops (+ level level-of-enclosing-module))])

      (define (collect-general-info stx)
        (add-origins stx varrefs level-of-enclosing-module collect-general-info)
        (add-disappeared-bindings stx binders sub-identifier-binding-directives varrefs
                                  level level-of-enclosing-module mods
                                  collect-general-info)
        (add-disappeared-uses stx varrefs sub-identifier-binding-directives
                              level level-of-enclosing-module mods
                              collect-general-info)
        (add-mouse-over-tooltips stx)
        (add-sub-range-binders stx
                               sub-identifier-binding-directives
                               level
                               level-of-enclosing-module
                               mods))

      (define (collect-nested-general-info stx)
        (let loop ([stx stx])
          (cond
            [(syntax? stx)
             (collect-general-info stx)
             (loop (syntax-e stx))]
            [(pair? stx)
             (loop (car stx))
             (loop (cdr stx))])))

      (collect-general-info stx-obj)

      (define (list-loop/tail-last bodies)
        (unless (null? bodies)
          (let body-loop ([fst (car bodies)]
                          [bodies (cdr bodies)])
            (cond
              [(null? bodies)
               (tail-loop fst)]
              [else
               (loop fst)
               (body-loop (car bodies) (cdr bodies))]))))

      (define (handle-quoted-syntax datum)
        (let loop ([stx datum]
                   [identifiers-as-disappeared-uses?
                    (syntax-property stx-obj 'identifiers-as-disappeared-uses?)])
          (cond
            [(and (not identifiers-as-disappeared-uses?)
                  (syntax? stx)
                  (syntax-property stx 'identifiers-as-disappeared-uses?))
             (loop stx #t)]
            [(identifier? stx)
             (add-id (if identifiers-as-disappeared-uses? varrefs templrefs)
                     stx level-of-enclosing-module)]
            [(syntax? stx)
             (loop (syntax-e stx) identifiers-as-disappeared-uses?)]
            [(pair? stx)
             (loop (car stx) identifiers-as-disappeared-uses?)
             (loop (cdr stx) identifiers-as-disappeared-uses?)]
            [(vector? stx)
             (for ([e (in-vector stx)])
               (loop e identifiers-as-disappeared-uses?))]
            [(box? stx)
             (loop (unbox stx) identifiers-as-disappeared-uses?)])))

      (syntax-case* stx-obj (#%plain-lambda case-lambda if begin begin0 let-values letrec-values
                                            set! quote quote-syntax with-continuation-mark
                                            #%plain-app #%top
                                            define-values define-syntaxes begin-for-syntax
                                            module module*
                                            #%require #%provide #%declare #%expression)
          (λ (x y) (free-identifier=? x y level 0))
        [(#%plain-lambda args bodies ...)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (add-binders (syntax args) binders #f #f level level-of-enclosing-module
                        sub-identifier-binding-directives mods)
           (list-loop/tail-last (syntax->list (syntax (bodies ...)))))]
        [(case-lambda [argss bodiess ...]...)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (for-each
            (λ (args bodies)
              (add-binders args binders #f #f level level-of-enclosing-module
                           sub-identifier-binding-directives mods)
              (list-loop/tail-last (syntax->list bodies)))
            (syntax->list (syntax (argss ...)))
            (syntax->list (syntax ((bodiess ...) ...)))))]
        [(if test then else)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (loop (syntax test))
           (tail-loop (syntax then))
           (tail-loop (syntax else)))]
        [(begin bodies ...)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (list-loop/tail-last (syntax->list (syntax (bodies ...)))))]

        ;; treat a single body expression specially, since this has
        ;; different tail behavior.
        [(begin0 body)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (tail-loop (syntax body)))]

        [(begin0 bodies ...)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (for-each loop (syntax->list (syntax (bodies ...)))))]

        [(let-values (bindings ...) bs ...)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (for-each collect-general-info (syntax->list (syntax (bindings ...))))
           (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
             (for-each (λ (x es) (add-binders x binders binding-inits es
                                              level level-of-enclosing-module
                                              sub-identifier-binding-directives mods))
                       (syntax->list (syntax ((xss ...) ...)))
                       (syntax->list (syntax (es ...))))
             (for-each loop (syntax->list (syntax (es ...))))
             (list-loop/tail-last (syntax->list (syntax (bs ...))))))]
        [(letrec-values (bindings ...) bs ...)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (for-each collect-general-info (syntax->list (syntax (bindings ...))))
           (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
             (for-each (λ (x es) (add-binders x binders binding-inits es
                                              level level-of-enclosing-module
                                              sub-identifier-binding-directives mods))
                       (syntax->list (syntax ((xss ...) ...)))
                       (syntax->list (syntax (es ...))))
             (for-each loop (syntax->list (syntax (es ...))))
             (list-loop/tail-last (syntax->list (syntax (bs ...))))))]
        [(set! var e)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (add-origins (list-ref (syntax->list stx-obj) 1) varrefs level-of-enclosing-module collect-general-info)
           ;; tops are used here because a binding free use of a set!'d variable
           ;; is treated just the same as (#%top . x).
           (add-id varsets (syntax var) level-of-enclosing-module)
           (if (identifier-binding (syntax var) 0)
               (add-id varrefs (syntax var) level-of-enclosing-module)
               (add-id tops (syntax var) level-of-enclosing-module))

           (loop (syntax e)))]
        [(quote datum)
         (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)]
        [(quote-syntax datum . maybe-hash-colon-local)
         (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
         (handle-quoted-syntax #'datum)]
        [(with-continuation-mark a b c)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (loop (syntax a))
           (loop (syntax b))
           (tail-loop (syntax c)))]
        [(#%plain-app pieces ...)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (for-each loop (syntax->list (syntax (pieces ...)))))]
        [(#%top . var)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (add-id tops (syntax var) level-of-enclosing-module))]
        [(define-values vars b)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (add-binders (syntax vars) binders binding-inits #'b
                        level level-of-enclosing-module
                        sub-identifier-binding-directives mods)
           (add-definition-target (syntax vars) mods level)
           (loop (syntax b)))]
        [(define-syntaxes names exp)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (add-binders (syntax names) binders binding-inits #'exp
                        level level-of-enclosing-module
                        sub-identifier-binding-directives mods)
           (add-definition-target (syntax names) mods level)
           (level-loop (syntax exp) (+ level 1)))]
        [(begin-for-syntax exp ...)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (for ([e (in-list (syntax->list (syntax (exp ...))))])
             (level-loop e (+ level 1))))]
        [(module m-name lang (mb bodies ...))
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (add-module-lang-require module-lang-requires (syntax lang))
           (annotate-require-open user-namespace user-directory (syntax lang) level #f)
           (define this-submodule (submodule #f (syntax-e #'m-name)))
           (define next-level-mods (sub-mods this-submodule))
           (define sub-requires
             (hash-ref! phase-to-requires
                        (list (+ level level-of-enclosing-module) next-level-mods)
                        (λ () (make-hash))))
           (hash-cons! sub-requires (syntax->datum #'lang)
                       (require-context '() #f #f #f (binder+mods #'lang this-submodule)))
           (for ([body (in-list (syntax->list (syntax (bodies ...))))])
             (mod-loop body this-submodule)))]
        [(module* m-name lang (mb bodies ...))
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (define this-submodule (submodule (not (syntax-e #'lang)) (syntax-e #'m-name)))
           (define next-level-mods (sub-mods this-submodule))
           (when (syntax-e #'lang)
             (add-module-lang-require module-lang-requires (syntax lang))
             (annotate-require-open user-namespace user-directory (syntax lang) level #f)
             (define sub-requires
               (hash-ref! phase-to-requires
                          (list (+ level level-of-enclosing-module) next-level-mods)
                          (λ () (make-hash))))
             (hash-cons! sub-requires (syntax->datum #'lang)
                         (require-context '() #f #f #f (binder+mods #'lang this-submodule))))

           (for ([body (in-list (syntax->list (syntax (bodies ...))))])
             (if (syntax-e #'lang)
                 (mod-loop body this-submodule)
                 (mod-loop (syntax-shift-phase-level body (- level)) this-submodule))))]


        ; top level or module top level only:
        [(#%require raw-require-specs ...)
         (let ()
           (collect-nested-general-info #'(raw-require-specs ...))
           (define (handle-raw-require-spec spec)
             (let loop ([spec spec]
                        [level level]
                        [space #f])
               (define (add-to-level n) (and n level (+ n level)))
               (syntax-case* spec (for-meta for-syntax for-template for-label just-meta for-space just-space portal)
                   symbolic-compare?
                 [(for-meta phase specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec (add-to-level (syntax-e #'phase)) space))]
                 [(for-syntax specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec (add-to-level 1) space))]
                 [(for-template specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec (add-to-level -1) space))]
                 [(for-label specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec #f space))]
                 [(just-meta phase specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec level space))]
                 [(for-space the-space specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec level (syntax-e #'the-space)))]
                 [(just-space ignored specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec level space))]
                 [(portal id content)
                  (begin
                    (handle-quoted-syntax #'content)
                    (add-definition-target (list #'id) mods level)
                    (add-binders (list #'id) binders binding-inits #'content
                                 level level-of-enclosing-module
                                 sub-identifier-binding-directives mods))]
                 [_
                  (handle-phaseless-spec spec level space)])))
           (define (handle-phaseless-spec stx level space)
             (define adjusted-level (and level (+ level level-of-enclosing-module)))
             (define require-ht (hash-ref! phase-to-requires
                                           (list adjusted-level mods)
                                           (λ () (make-hash))))
             (define require-context
               (phaseless-spec->require-context
                mods
                stx
                space
                (λ (local-id)
                  (add-binders (list local-id) binders binding-inits #'b
                               level level-of-enclosing-module
                               sub-identifier-binding-directives mods))))
             (define raw-module-path (binder+mods-binder (require-context-b+m require-context)))
             (annotate-require-open user-namespace user-directory raw-module-path level stx)
             (when (original-enough? raw-module-path)
               (define key
                 (match (syntax->datum raw-module-path)
                   [`',m
                    (define we-have-seen-this-sumodule?
                      (hash-has-key? phase-to-requires (list adjusted-level (cons (submodule #f m) mods))))
                    (if we-have-seen-this-sumodule?
                        `(submod "." ,m)
                        `',m)]
                   [rmp rmp]))
               (hash-cons! require-ht key require-context)))

           (for ([spec (in-list (syntax->list #'(raw-require-specs ...)))])
             (handle-raw-require-spec spec)))]

        ; module top level only:
        [(#%provide raw-provide-specs ...)
         (let ()
           (collect-nested-general-info #'(raw-provide-specs ...))
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (define (handle-raw-provide-spec spec)
             (let loop ([spec spec]
                        [level level])
               (syntax-case* spec (for-meta for-syntax for-label protect)
                   symbolic-compare?
                 [(protect specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec level))]
                 [(for-meta n specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec (+/f level (syntax-e #'n))))]
                 [(for-syntax specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec (and level (add1 level))))]
                 [(for-label specs ...)
                  (for ([spec (in-list (syntax->list #'(specs ...)))])
                    (loop spec #f))]
                 [_
                  (handle-phaseless-spec spec level)])))
           (define (handle-phaseless-spec spec level)
             (let ([varrefs (lookup-phase-to-mapping
                             phase-to-varrefs
                             (list (+/f level level-of-enclosing-module) mods)
                             (+/f level level-of-enclosing-module))]
                   [provided-vars (extract-provided-vars spec)])
               (for ([provided-var (in-list provided-vars)])
                 (add-id varrefs provided-var level-of-enclosing-module))))
           (define (+/f x y) (and x y (+ x y)))
           (for ([spec (in-list (syntax->list #'(raw-provide-specs ...)))])
             (handle-raw-provide-spec spec)))]

        ; module top level only:
        [(#%declare declare-specs ...)
         (void)]

        [(#%expression arg)
         (begin
           (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
           (tail-loop #'arg))]
        [id
         (identifier? (syntax id))
         (add-id varrefs stx-obj level-of-enclosing-module)]
        [_
         (begin
           #;
           (printf "unknown stx: ~.s datum: ~e source: ~e\n"
                   stx-obj
                   (and (syntax? stx-obj)
                        (syntax->datum stx-obj))
                   (and (syntax? stx-obj)
                        (syntax-source stx-obj)))
           (void))]))))

(define (add-module-lang-require module-lang-requires stx)
  (hash-set! module-lang-requires (module-lang-require-ht-key stx) #t))
(define (is-module-lang-require? module-lang-requires stx)
  (hash-ref module-lang-requires (module-lang-require-ht-key stx) #f))
(define (module-lang-require-ht-key stx)
  (list (syntax-source stx)
        (syntax-position stx)
        (syntax-span stx)))

(define (hash-cons! ht k v)
  (hash-set! ht k (cons v (hash-ref ht k '()))))

(define ((char-coordinates-are-right-thing? index) v)
  (define pred?
    (if (syntax? (vector-ref v index))
        exact-nonnegative-integer?
        (and/c inexact? (real-in 0 1))))
  (and (pred? (vector-ref v (+ index 1)))
       (pred? (vector-ref v (+ index 2)))))

(define ok-coordinate? (or/c exact-nonnegative-integer? (and/c (real-in 0 1) inexact?)))
(define sub-range-binder-prop?
  (or/c (vector/c #:flat? #t
                  identifier? exact-nonnegative-integer? exact-nonnegative-integer?
                  syntax? exact-nonnegative-integer? exact-nonnegative-integer?)
        (vector/c #:flat? #t
                  identifier?
                  exact-nonnegative-integer? exact-nonnegative-integer?
                  (real-in 0 1) (real-in 0 1)
                  syntax?
                  exact-nonnegative-integer? exact-nonnegative-integer?
                  (real-in 0 1) (real-in 0 1))))

(define (add-sub-range-binders stx
                               sub-identifier-binding-directives
                               level
                               level-of-enclosing-module
                               mods)
  (let loop ([prop (syntax-property stx 'sub-range-binders)])
    (cond
      [(pair? prop)
       (loop (car prop))
       (loop (cdr prop))]
      [(sub-range-binder-prop? prop)
       (define the-vec
         (if (= (vector-length prop) 6)
             (vector (vector-ref prop 0)
                     (vector-ref prop 1)
                     (vector-ref prop 2)
                     .5 .5
                     (vector-ref prop 3)
                     (vector-ref prop 4)
                     (vector-ref prop 5)
                     .5 .5)
             prop))
       (define (shift-it i)
         (syntax-shift-phase-level (vector-ref the-vec i)
                                   level-of-enclosing-module))
       (define new-entry
         (vector (shift-it 0)
                 (vector-ref the-vec 1)
                 (vector-ref the-vec 2)
                 (vector-ref the-vec 3)
                 (vector-ref the-vec 4)
                 (shift-it 5)
                 (vector-ref the-vec 6)
                 (vector-ref the-vec 7)
                 (vector-ref the-vec 8)
                 (vector-ref the-vec 9)))
       (define key (list level mods))
       (hash-update! sub-identifier-binding-directives key (λ (v) (cons new-entry v)) '())]
      [(vector? prop)
       (log-check-syntax-debug
        "found a vector in a 'sub-range-binders property that is ill-formed ~s"
        prop)])))

(define mouse-over-tooltip-prop?
  (vector/c #:flat? #t syntax? exact-nonnegative-integer? exact-nonnegative-integer?
            (or/c string?
                  ;; don't check the range here, since we want a predicate, not really a contract
                  (procedure-arity-includes/c 0))))
(define (add-mouse-over-tooltips stx)
  (let loop ([prop (syntax-property stx 'mouse-over-tooltips)])
    (cond
      [(pair? prop)
       (loop (car prop))
       (loop (cdr prop))]
      [(mouse-over-tooltip-prop? prop)
       (add-mouse-over/loc (find-source-editor (vector-ref prop 0))
                           (vector-ref prop 1)
                           (vector-ref prop 2)
                           (vector-ref prop 3))])))

;; add-disappeared-bindings : syntax id-set integer -> void
(define (add-disappeared-bindings stx
                                  binders
                                  sub-identifier-binding-directives
                                  disappeared-uses
                                  level
                                  level-of-enclosing-module
                                  mods
                                  collect-general-info)
  (define prop (syntax-property stx 'disappeared-binding))
  (when prop
    (let loop ([prop prop])
      (cond
        [(pair? prop)
         (loop (car prop))
         (loop (cdr prop))]
        [(identifier? prop)
         (collect-general-info prop)
         (add-binders prop
                      binders
                      #f
                      #f
                      level
                      level-of-enclosing-module
                      sub-identifier-binding-directives
                      mods)]))))

;; add-disappeared-uses : syntax id-set integer -> void
(define (add-disappeared-uses stx
                              id-set
                              sub-identifier-binding-directives
                              level
                              level-of-enclosing-module
                              mods
                              collect-general-info)
  (define prop (syntax-property stx 'disappeared-use))
  (when prop
    (let loop ([prop prop])
      (cond
        [(pair? prop)
         (loop (car prop))
         (loop (cdr prop))]
        [(identifier? prop)
         (collect-general-info prop)
         (add-sub-range-binders prop
                                sub-identifier-binding-directives
                                level
                                level-of-enclosing-module
                                mods)
         (add-id id-set prop level-of-enclosing-module)]))))

;; annotate-variables : namespace directory string id-set[four of them]
;;                      (listof syntax) (listof syntax)
;;                      hash[phase -o> sub-identifier-binding-directive]
;;                   -> void
;; colors in and draws arrows for variables, according to their classifications
;; in the various id-sets
(define (annotate-variables user-namespace
                            user-directory
                            phase-to-binders
                            phase-to-varrefs
                            phase-to-varsets
                            phase-to-tops
                            templrefs
                            module-lang-requires
                            phase-to-requires
                            sub-identifier-binding-directives)

  (define unused/phases (make-hash))

  ;; hash[(list (list src pos pos dx dy) (list src pos pos dx dy)) -o> #t
  ;;       above indicates if this arrow has been recorded
  ;;       below indicates the number of defs and uses at this spot
  ;;      (list src pos pos) -o> (cons number number)]
  (define connections (make-hash))

  (for ([(level+mods requires) (in-hash phase-to-requires)])
    (define new-hash (make-hash))
    (hash-set! unused/phases level+mods new-hash)
    (for ([k (in-hash-keys requires)])
      (hash-set! new-hash k #t)))

  (for* ([(level binders) (in-hash phase-to-binders)]
         [(_ binder+modss) (in-dict binders)]
         [binder+mods (in-list binder+modss)])
    (define var (binder+mods-binder binder+mods))
    (define varset (lookup-phase-to-mapping phase-to-varsets level))
    (color-variable var level varset)
    (document-variable var level))

  (for ([(level+mods varrefs) (in-hash phase-to-varrefs)])
    (define level (list-ref level+mods 0))
    (define mods (list-ref level+mods 1))
    (define binders (lookup-phase-to-mapping phase-to-binders level))
    (define varsets (lookup-phase-to-mapping phase-to-varsets level))
    (initialize-binder-connections binders connections)
    (for* ([vars (in-list (get-idss varrefs))]
           [var (in-list vars)])
      (color-variable var level varsets)
      (document-variable var level)
      (connect-identifier var
                          mods
                          binders
                          unused/phases
                          phase-to-requires
                          level
                          user-namespace
                          user-directory
                          #t
                          connections
                          module-lang-requires)))


  ;; build a set of all of the known phases
  (define phases (set))
  (define all-mods (set))
  (for ([phase (in-hash-keys phase-to-binders)])
    (set! phases (set-add phases phase)))
  (for ([phase+mod (in-hash-keys phase-to-requires)])
    (define phase (list-ref phase+mod 0))
    (define mod (list-ref phase+mod 1))
    (set! phases (set-add phases phase))
    (set! all-mods (set-add all-mods mod)))

  (for ([vars (in-list (get-idss templrefs))])
    (for ([var (in-list vars)])

      ;; connect every identifier inside a quote-syntax to
      ;; each binder, at any phase, in any submodule
      (for* ([phase (in-set phases)]
             [mod (in-set all-mods)])
        (document-variable var phase)
        (connect-identifier var
                            mod
                            (lookup-phase-to-mapping phase-to-binders phase)
                            unused/phases
                            phase-to-requires
                            phase
                            user-namespace
                            user-directory
                            #f
                            connections
                            module-lang-requires))))

  (for ([(level tops) (in-hash phase-to-tops)])
    (define binders (lookup-phase-to-mapping phase-to-binders level))
    (for* ([vars (in-list (get-idss tops))]
           [var (in-list vars)])
      (color/connect-top user-namespace user-directory binders var connections module-lang-requires)))

  (for ([(phase+mods require-hash) (in-hash phase-to-requires)])
    ;; don't mark for-label requires as unused until we can properly handle them
    (when (car phase+mods)
      (define unused-hash (hash-ref unused/phases phase+mods))
      (color-unused require-hash unused-hash module-lang-requires)))

  (for ([(level+mods directives) (in-hash sub-identifier-binding-directives)])
    (define phase-level (list-ref level+mods 0))
    (define mods (list-ref level+mods 1))
    (for ([directive (in-list directives)])
      (match-define (vector binding-id to-start to-span to-dx to-dy
                            new-binding-id from-start from-span from-dx from-dy)
        directive)
      (define all-varrefs (lookup-phase-to-mapping phase-to-varrefs (list phase-level mods) phase-level))
      (define all-binders (lookup-phase-to-mapping phase-to-binders phase-level))
      (define varrefs (get-ids all-varrefs binding-id))
      (when varrefs
        (for ([varref (in-list varrefs)])
          (connect-syntaxes new-binding-id varref #t all-binders
                            phase-level
                            connections #f
                            #:from-start from-start #:from-width from-span
                            #:from-dx from-dx #:from-dy from-dy
                            #:to-start to-start #:to-width to-span
                            #:to-dx to-dx #:to-dy to-dy)))))

  (annotate-counts connections)
  (flush-index-entry-cache))

;; color-unused : hash-table[sexp -o> syntax] hash-table[sexp -o> #f] hash-table[syntax -o> #t]
;;             -> void
(define (color-unused requires unused module-lang-requires)
  (for ([k (in-hash-keys unused)])
    (define require-contexts
      (hash-ref requires
                k
                (λ () (error 'syncheck/traversals.rkt "requires doesn't have a mapping for ~s" k))))
    (for ([require-context (in-list require-contexts)])
      (define binder+mods (require-context-b+m require-context))
      (define stx (binder+mods-binder binder+mods))
      (unless (hash-ref module-lang-requires
                        (list (syntax-source stx) (syntax-position stx) (syntax-span stx))
                        #f)
        (define defs-text (current-annotations))
        (define source-editor (find-source-editor stx))
        (when (and defs-text source-editor)
          (define pos (syntax-position stx))
          (define span (syntax-span stx))
          (when (and pos span)
            (define start (- pos 1))
            (define fin (+ start span))
            (send defs-text syncheck:add-unused-require source-editor start fin)
            (send defs-text syncheck:add-text-type source-editor start fin 'unused-identifier)))
        (color stx unused-require-style-name)))))

;; color-unused-binder : source integer integer -> void
(define (color-unused-binder source start end)
  (define defs-text (current-annotations))
  (when (and defs-text source)
    (send defs-text syncheck:add-text-type source start end 'unused-identifier))
  (color-range source start end unused-require-style-name))

(define (self-module? mpi)
  (define-values (a b) (module-path-index-split mpi))
  (and (not a) (not b)))

;; connect-identifier : syntax
;;                      (or/c #f (listof symbol)) -- name of enclosing sub-modules
;;                      id-set
;;                      (union #f hash-table)
;;                      (union #f hash-table)
;;                      (union identifier-binding identifier-transformer-binding)
;;                      boolean
;;                      connections-table (see its defn)
;;                      hash[(list source position span) -o> #t]
;;                   -> void
;; adds the arrows that correspond to binders/bindings
(define (connect-identifier var mods-where-var-is all-binders unused/phases phase-to-requires
                            phase-level user-namespace user-directory actual?
                            connections module-lang-requires)
  (define binders (get-ids all-binders var))
  (when binders
    (for ([binder+mods (in-list binders)])
      (define binder (binder+mods-binder binder+mods))
      (when (equal? (syntax->datum binder) (syntax->datum var))
        (define binder-is-outside-reference?
          (or (not mods-where-var-is)
              (not (binder+mods-mods binder+mods))
              (let loop ([mods-where-var-is (reverse mods-where-var-is)]
                         [mods-where-binder-is (reverse (binder+mods-mods binder+mods))])
                (cond
                  [(null? mods-where-binder-is)
                   (for/and ([mod (in-list mods-where-var-is)])
                     (submodule-enclosing-bindings-visible? mod))]
                  [(null? mods-where-var-is) #f]
                  [else
                   (and (equal? (car mods-where-var-is)
                                (car mods-where-binder-is))
                        (loop (cdr mods-where-var-is)
                              (cdr mods-where-binder-is)))]))))
        (when binder-is-outside-reference?
          (connect-syntaxes binder var actual? all-binders phase-level connections #f)))))

  (when (and unused/phases phase-to-requires)
    (define req-path/pr (get-module-req-path var phase-level))
    (define source-req-path/pr (get-module-req-path var phase-level #:nominal? #f))
    (when (and req-path/pr source-req-path/pr)
      (let loop ([mods mods-where-var-is])
        (connect-identifier-to-a-require var mods all-binders unused/phases phase-to-requires
                                         phase-level user-namespace user-directory actual?
                                         connections module-lang-requires
                                         req-path/pr source-req-path/pr)
        (when (and (pair? mods)
                   (submodule-enclosing-bindings-visible? (car mods)))
          (loop (cdr mods)))))))

;; accepts arguments given to `connect-identifier` and those have
;; the same contracts as the matching names but also accepts:
;; req-path/pr and source-req-path/pr, which are results from get-module-req-path
(define (connect-identifier-to-a-require var mods all-binders unused/phases phase-to-requires
                                         phase-level user-namespace user-directory actual?
                                         connections module-lang-requires
                                         req-path/pr source-req-path/pr)
  (define req-path (list-ref req-path/pr 0))
  (define id (list-ref req-path/pr 1))
  (define source-req-path (list-ref source-req-path/pr 2))
  (define source-id (list-ref source-req-path/pr 1))
  (define req-phase+space-shift (list-ref req-path/pr 3))
  (define req-phase-level (if (pair? req-phase+space-shift) (car req-phase+space-shift) req-phase+space-shift))
  (define req-space (and (pair? req-phase+space-shift) (cdr req-phase+space-shift)))
  (define require-hash-key (list req-phase-level mods))
  (define require-ht (hash-ref phase-to-requires require-hash-key #f))
  (when id
    (define-values (filename submods)
      (get-require-filename source-req-path user-namespace user-directory))
    (when filename
      (add-jump-to-definition
       var
       source-id
       filename
       submods
       req-phase+space-shift)))
  (when require-ht
    (define req-binder+modss (hash-ref require-ht req-path #f))
    (when req-binder+modss
      (define unused (hash-ref unused/phases require-hash-key #f))
      (when unused (hash-remove! unused req-path))
      (for ([require-context (in-list req-binder+modss)])
        (define binder+mods (require-context-b+m require-context))
        (define req-stx (binder+mods-binder binder+mods))
        (define match/prefix
          (id/require-match (syntax->datum var) id req-space require-context))
        (when match/prefix
          (define prefix-length
            (cond
              [(and (identifier? match/prefix)
                    (syntax-span match/prefix))
               (syntax-span match/prefix)]
              [else 0]))
          (add-mouse-over var
                          (format
                           (string-constant cs-mouse-over-import/library-only)
                           req-path))
          ;; connect the require to the identifier
          (connect-syntaxes req-stx
                            var actual? all-binders
                            #:to-start prefix-length
                            #:to-width (if (syntax-span var)
                                           (- (syntax-span var) prefix-length)
                                           0)
                            phase-level
                            connections
                            (if (is-module-lang-require? module-lang-requires req-stx)
                                'module-lang
                                #t))
          ;; connect the prefix
          (when (and (identifier? match/prefix)
                     (syntax-source match/prefix)
                     (syntax-position match/prefix)
                     (syntax-span match/prefix))
            (connect-syntaxes match/prefix var actual? all-binders
                              #:to-start 0
                              #:to-width prefix-length
                              phase-level
                              connections
                              (if (is-module-lang-require? module-lang-requires req-stx)
                                  'module-lang
                                  #t))))))))

(define (id/require-match var id req-space require-context)
  (define prefix (require-context-prefix require-context))
  (cond
    [(not (equal? (require-context-space require-context)
                  req-space))
     #f]
    [prefix
     (and (equal? (format "~a~a" (syntax->datum prefix) id) (symbol->string var))
          (not (member var (map syntax-e (require-context-ids require-context))))
          prefix)]
    [(require-context-in? require-context)
     (member var (map syntax-e (require-context-ids require-context)))]
    [else
     (and (not (member var (map syntax-e (require-context-ids require-context))))
          (equal? var id))]))

(define (phaseless-spec->require-context mods stx space [found-local-id void])
  (syntax-case* stx (only prefix all-except prefix-all-except rename) symbolic-compare?
    [(only raw-module-path id ...)
     (require-context (syntax->list #'(id ...)) #t #f space (binder+mods #'raw-module-path mods))]
    [(prefix prefix-id raw-module-path)
     (require-context '() #f #'prefix-id space (binder+mods #'raw-module-path mods))]
    [(all-except raw-module-path id ...)
     (require-context (syntax->list #'(id ...)) #f #f space (binder+mods #'raw-module-path mods))]
    [(prefix-all-except prefix-id raw-module-path id ...)
     (require-context (syntax->list #'(id ...)) #t #'prefix-id space (binder+mods #'raw-module-path mods))]
    [(rename raw-module-path local-id exported-id)
     (found-local-id #'local-id)
     (require-context (list #'local-id) #t #f space (binder+mods #'raw-module-path mods))]
    [_
     (require-context '() #f #f space (binder+mods stx mods))]))

;; get-module-req-path : identifier number [#:nominal? boolean]
;;                    -> (union #f (list require-sexp sym ?? module-path-index? phase+space?))
(define (get-module-req-path var phase-level #:nominal? [nominal-source-path? #t])
  (define binding (identifier-binding var phase-level))
  (let/ec return
    (unless (pair? binding) (return #f))
    (define phase+space-shift (list-ref binding 5))
    (define phase-shift (if (pair? phase+space-shift) (car phase+space-shift) phase+space-shift))
    (define phase+space (list-ref binding 6))
    (define phase (if (pair? phase+space) (car phase+space) phase+space))
    (define space (and (pair? phase+space) (cdr phase+space)))
    (when (and (number? phase-level)
               (not (= phase-level
                       (+ phase-shift
                          phase))))
      (return #f))
    (define mod-path (if nominal-source-path? (list-ref binding 2) (list-ref binding 0)))
    (cond
      [(module-path-index? mod-path)
       (define-values (base offset) (module-path-index-split mod-path))
       (list base
             (if nominal-source-path? (list-ref binding 3) (list-ref binding 1))
             mod-path
             phase+space-shift)]
      [(symbol? mod-path)
       (list mod-path
             (if nominal-source-path? (list-ref binding 3) (list-ref binding 1))
             mod-path
             phase+space-shift)]
      [else #f])))

;; color/connect-top : namespace directory id-set syntax connections[see defn for ctc] -> void
(define (color/connect-top user-namespace user-directory binders var connections module-lang-requires)
  (define top-bound?
    (or (get-ids binders var)
        (parameterize ([current-namespace user-namespace])
          (let/ec k
            (namespace-variable-value (syntax-e var) #t (λ () (k #f)))
            #t))))
  (cond
    [top-bound? (color var lexically-bound-variable-style-name)]
    [else
     (add-mouse-over var (format "~s is a free variable" (syntax-e var)))
     (color var free-variable-style-name)])
  (connect-identifier var
                      #f
                      binders
                      #f
                      #f
                      0
                      user-namespace
                      user-directory
                      #t
                      connections
                      module-lang-requires))

;; annotate-counts : connections[see defn] -> void
;; this function doesn't try to show the number of uses at
;; a use site, as it is not obvious how to compute that.
;; in particular, you could think of following arrows from
;; the use site back to the definition and then counting
;; the number of arrows originating there, but consider this example:
;; (define-syntax-rule (m x y z)
;;   (list (let ([y 1]) x x)
;;         (let ([z 1]) x)))
;; (m w w w)
;; if you do that here, then which def site do you pick?
;; and note that picking both of them leads to double counting
;; it seems possible to have a different datastructure (one that
;; records the src locs of each 'end' position of each arrow)
;; to do this, but maybe lets leave that for another day.
(define (annotate-counts connections)
  (for ([(key val) (in-hash connections)])
    (when (list? val)
      (define start (first val))
      (define end (second val))
      (define color? (third val))
      (define (show-starts)
        (when (zero? start)
          (define defs-text (current-annotations))
          (when defs-text
            (send defs-text syncheck:unused-binder
                  (list-ref key 0) (list-ref key 1) (list-ref key 2))))
        (add-mouse-over/loc (list-ref key 0) (list-ref key 1) (list-ref key 2)
                            (cond
                              [(zero? start)
                               (string-constant cs-zero-varrefs)]
                              [(= 1 start)
                               (string-constant cs-one-varref)]
                              [else
                               (format (string-constant cs-n-varrefs) start)])))
      (define (show-ends)
        (unless (= 1 end)
          (add-mouse-over/loc (list-ref key 0) (list-ref key 1) (list-ref key 2)
                              (format (string-constant cs-binder-count) end))))
      (cond
        [(zero? end)   ;; assume this is a binder, show uses
         #;(when (and color? (zero? start))
             (color-unused-binder (list-ref key 0) (list-ref key 1) (list-ref key 2)))
         (show-starts)]
        [(zero? start) ;; assume this is a use, show bindings (usually just one, so do nothing)
         (show-ends)]
        [else          ;; crazyness, show both
         (show-starts)
         (show-ends)]))))

;; color-variable : syntax phase-level identifier-mapping -> void
(define (color-variable var phase-level varsets)
  (define b (identifier-binding var phase-level))
  (define lexical? (is-lexical? b))
  (cond
    [(get-ids varsets var)
     (add-mouse-over var (string-constant cs-set!d-variable))
     (color var set!d-variable-style-name)]
    [lexical? (color var lexically-bound-variable-style-name)]
    [(pair? b) (color var imported-variable-style-name)]))

    (define (is-lexical? b)
      (or (not b)
          (eq? b 'lexical)
          (and (pair? b)
               (let ([path (caddr b)])
                 (and (module-path-index? path)
                      (self-module? path))))))

;; initialize-binder-connections : id-set connections -> void
(define (initialize-binder-connections binders connections)
  (for ([binder (in-list (free-id-table-keys binders))])
    (define source (find-source-editor binder))
    (define pos (syntax-position binder))
    (define span (syntax-span binder))
    (when (and pos span)
      (define pos-left (sub1 pos))
      (define pos-right (+ pos-left span))
      (define connections-start
        (list source pos-left pos-right))
      (unless (hash-ref connections connections-start #f)
        ;; before we find any references we should consider this binder to be colored red
        ;; only flip to #f when we find a reference
        (hash-set! connections connections-start (list 0 0 #t))))))

;; connect-syntaxes : syntax[original] syntax[original] boolean symbol connections -> void
;; adds an arrow from `from' to `to', unless they have the same source loc.
(define (connect-syntaxes from to actual? all-binders level connections require-arrow?
                          #:from-start [from-start 0]
                          #:from-width [from-width (if (exact? from-start)
                                                       (syntax-span from)
                                                       from-start)]
                          #:from-dx [from-dx .5]
                          #:from-dy [from-dy .5]
                          #:to-start [to-start 0]
                          #:to-width [to-width (if (exact? to-start)
                                                   (syntax-span to)
                                                   to-start)]
                          #:to-dx [to-dx .5]
                          #:to-dy [to-dy .5])
  (define from-source (find-source-editor from))
  (define to-source (find-source-editor to))
  (define defs-text (current-annotations))
  (when (and from-source to-source defs-text)
    (define pos-from (syntax-position from))
    (define span-from (syntax-span from))
    (define pos-to (syntax-position to))
    (define span-to (syntax-span to))
    (when (and pos-from span-from pos-to span-to)
      (define from-pos-left (+ (syntax-position from) -1 from-start))
      (define from-pos-right (+ from-pos-left from-width))
      (define to-pos-left (+ (syntax-position to) -1 to-start))
      (define to-pos-right (+ to-pos-left to-width))
      ;; just an approximation; could be tightened if this is problematic
      (define arrow-points-to-itself? (= from-pos-left to-pos-left))
      (define connections-start/no-pxpy
        (list from-source from-pos-left from-pos-right))
      (define connections-end/no-pxpy
        (list to-source to-pos-left to-pos-right))
      (define connections-key
        (list (list from-source from-pos-left from-pos-right from-dx from-dy)
              (list to-source to-pos-left to-pos-right to-dx to-dy)))
      (cond
        [(not arrow-points-to-itself?)
         (unless (hash-ref connections connections-key #f)
           (hash-set! connections connections-key #t)
           (define start-before (or (hash-ref connections connections-start/no-pxpy #f)
                                    (list 0 0 #t)))
           (define end-before (or (hash-ref connections connections-end/no-pxpy #f)
                                  (list 0 0 #t)))
           (hash-set! connections connections-start/no-pxpy
                      (list (+ (first start-before) 1) (second start-before) (third start-before)))
           (hash-set! connections connections-end/no-pxpy
                      (list (first end-before) (+ 1 (second end-before)) (third end-before))))
         (define (name-dup? str)
           (define sym (string->symbol str))
           (define id1 (datum->syntax from sym))
           (define id2 (datum->syntax to sym)) ;; do I need both?
           (for*/or ([(_ binder+modss) (in-dict all-binders)]
                     [binder+mods (in-list binder+modss)])
             (define id (binder+mods-binder binder+mods))
             (or (free-identifier=? id1 id level)
                 (free-identifier=? id2 id level))))
         (when (and (<= from-pos-left from-pos-right)
                    (<= to-pos-left to-pos-right))
           (send defs-text syncheck:add-arrow/name-dup/pxpy
                 from-source from-pos-left from-pos-right from-dx from-dy
                 to-source to-pos-left to-pos-right to-dx to-dy
                 actual? level require-arrow? name-dup?))]
        [else
         (unless (hash-ref connections connections-key #f)
           (hash-set! connections connections-key #t)
           (define start-before (or (hash-ref connections connections-start/no-pxpy #f)
                                    (list 0 0 #t)))
           (hash-set! connections connections-start/no-pxpy
                      (list (first start-before) (second start-before) #f)))]))))

;; add-jump-to-definition : syntax symbol path -> void
;; registers the range in the editor so that the
;; popup menu in this area allows the programmer to jump
;; to the definition of the id.
(define (add-jump-to-definition stx id filename submods phase-level+space)
  (define source (find-source-editor stx))
  (define defs-text (current-annotations))
  (when (and source defs-text (syntax-position stx) (syntax-span stx))
    (let* ([pos-left (- (syntax-position stx) 1)]
           [pos-right (+ pos-left (syntax-span stx))])
      (send defs-text syncheck:add-jump-to-definition/phase-level+space
            source
            pos-left
            pos-right
            id
            filename
            submods
            phase-level+space))))

;; annotate-require-open : namespace string -> (stx -> void)
;; relies on current-module-name-resolver, which in turn depends on
;; current-directory and current-namespace
;;  phaseless-require-spec/module-lang-require is #f => module-lang require
;;    otherwise, it is the phaseless part of the require-spec
(define (annotate-require-open user-namespace user-directory raw-module-path
                               phase-level phaseless-require-spec/module-lang-require)
  (when (original-enough? raw-module-path)
    (define req-source (find-source-editor raw-module-path))
    (when (and req-source
               (syntax-position raw-module-path)
               (syntax-span raw-module-path))
      (define defs-text (current-annotations))
      (when defs-text
        (define start (- (syntax-position raw-module-path) 1))
        (define end (+ start (syntax-span raw-module-path)))
        (define-values (file submods)
          (get-require-filename (syntax->datum raw-module-path)
                                user-namespace
                                user-directory))
        (when file
          (send defs-text syncheck:add-require-open-menu
                req-source start end file))
        (when phaseless-require-spec/module-lang-require
          (define prefix
            (syntax-case* phaseless-require-spec/module-lang-require (prefix prefix-all-except)
                symbolic-compare?
              [(prefix pfx . _) #'pfx]
              [(prefix-all-except pfx . _) #'pfx]
              [_ #f]))
          (when prefix
            (define prefix-source (find-source-editor prefix))
            (define prefix-start (and prefix-source
                                      (syntax-position prefix)
                                      (- (syntax-position prefix) 1)))
            (define prefix-end (and prefix-start
                                    (syntax-span prefix)
                                    (+ prefix-start (syntax-span prefix))))
            (send defs-text syncheck:add-prefixed-require-reference req-source start end
                  (syntax-e prefix) prefix-source prefix-start prefix-end)))))))

;; get-require-filename : sexp-or-module-path-index namespace string[directory] -> filename or #f
;; finds the filename corresponding to the require in stx
(define (get-require-filename datum user-namespace user-directory)
  (parameterize ([current-namespace user-namespace]
                 [current-directory (or user-directory (current-directory))]
                 [current-load-relative-directory user-directory])
    (define mpi
      (with-handlers ([exn:fail? (λ (x) #f)])
        (cond
          [(module-path-index? datum)
           (module-path-index-resolve datum)]
          [else
           ((current-module-name-resolver) datum #f #f #t)])))
    (define rkt-path/mod-path (and mpi (resolved-module-path-name mpi)))
    (define rkt-path/f (cond
                         [(path? rkt-path/mod-path) rkt-path/mod-path]
                         [(and (pair? rkt-path/mod-path)
                               (path? (car rkt-path/mod-path)))
                          (car rkt-path/mod-path)]
                         [else #f]))
    (define rkt-submods (cond
                          [(not rkt-path/mod-path) #f]
                          [(or (symbol? rkt-path/mod-path) (path? rkt-path/mod-path)) '()]
                          [(pair? rkt-path/mod-path) (cdr rkt-path/mod-path)]))
    (define cleaned-up-path
      (let/ec k
        (unless (path? rkt-path/f) (k rkt-path/f))
        (when (file-exists? rkt-path/f) (k rkt-path/f))
        (let* ([bts (path->bytes rkt-path/f)]
               [len (bytes-length bts)])
          (unless (and (len . >= . 4)
                       (bytes=? #".rkt" (subbytes bts (- len 4))))
            (k rkt-path/f))
          (define ss-path (bytes->path (bytes-append (subbytes bts 0 (- len 4)) #".ss")))
          (unless (file-exists? ss-path)
            (k rkt-path/f))
          ss-path)))
    (values cleaned-up-path rkt-submods)))

;; add-origins : syntax? id-set exact-integer? -> void
(define (add-origins stx id-set level-of-enclosing-module collect-general-info)
  (define did-already (make-hasheq))
  (let loop ([ct (syntax-property stx 'origin)])
    (match ct
      [(cons hd tl)
       (unless (hash-ref did-already ct #f)
         (hash-set! did-already ct #t)
         (loop hd)
         (loop tl))]
      [(? identifier?)
       (collect-general-info ct)
       (add-id id-set ct level-of-enclosing-module)]
      [_ (void)])))

;; FIXME: handle for-template and for-label
;; extract-provided-vars : syntax -> (listof syntax[identifier])
(define (extract-provided-vars stx)
  (syntax-case* stx (rename struct all-from all-from-except all-defined-except) symbolic-compare?
    [identifier
     (identifier? (syntax identifier))
     (list (syntax identifier))]

    [(rename local-identifier export-identifier)
     (list (syntax local-identifier))]

    ;; why do I even see this?!?
    [(struct struct-identifier (field-identifier ...))
     null]

    [(all-from module-name) null]
    [(all-from-except module-name identifier ...)
     null]
    [(all-defined-except identifier ...)
     (syntax->list #'(identifier ...))]
    [_
     null]))

(define (symbolic-compare? x y) (eq? (syntax-e x) (syntax-e y)))

;; add-binders : syntax id-set (or/c #f id-set) (or/c #f syntax) integer -> void
;; transforms an argument list into a bunch of symbols/symbols
;; and puts them into the id-set
;; effect: colors the identifiers
(define (add-binders stx id-set binding-to-init init-exp level level-of-enclosing-module
                     sub-identifier-binding-directives mods)
  (define (add-id&init&sub-range-binders stx)
    (add-sub-range-binders stx
                           sub-identifier-binding-directives
                           level
                           level-of-enclosing-module
                           mods)
    (when binding-to-init
      (add-init-exp binding-to-init stx init-exp level-of-enclosing-module))
    (add-id id-set stx level-of-enclosing-module #:mods mods))
  (let loop ([stx stx])
    (define e
      (if (syntax? stx)
          (syntax-e stx)
          stx))
    (cond
      [(cons? e)
       (define fst (car e))
       (define rst (cdr e))
       (cond
         [(syntax? fst)
          (add-id&init&sub-range-binders fst)
          (loop rst)]
         [else (loop rst)])]
      [(null? e) (void)]
      [else (add-id&init&sub-range-binders stx)])))

;; add-definition-target : syntax[(sequence of identifiers)] (listof symbol) -> void
(define (add-definition-target stx mods phase-level)
  (when mods
    (define defs-text (current-annotations))
    (for ([id (in-list (if (list? stx) stx (syntax->list stx)))])
      (define source (syntax-source id))
      (define ib (identifier-binding id phase-level))
      (when (and (list? ib) source defs-text (syntax-position id) (syntax-span id))
        (define pos-left (- (syntax-position id) 1))
        (define pos-right (+ pos-left (syntax-span id)))
        (send defs-text syncheck:add-definition-target/phase-level+space
              source
              pos-left
              pos-right
              (list-ref ib 1)
              (map submodule-name mods)
              phase-level)))))

;; annotate-raw-keyword : syntax id-map integer -> void
;; annotates keywords when they were never expanded. eg.
;; if someone just types `(λ (x) x)' it has no 'origin
;; field, but there still are keywords.
(define (annotate-raw-keyword stx id-map level-of-enclosing-module)
  (define lst (syntax-e stx))
  (when (pair? lst)
    (let ([f-stx (car lst)])
      (when (identifier? f-stx)
        (add-id id-map f-stx level-of-enclosing-module)))))

;
;
;       ;
;       ;                                                                   ;
;       ;                                             ;             ;
;    ;; ;   ;;;    ;;;;  ;   ; ; ;; ;;  ;;;   ; ;;   ;;;;;  ;;;;   ;;;;;  ;;;     ;;;   ; ;;
;   ;  ;;  ;   ;  ;      ;   ; ;; ;; ; ;   ;  ;;  ;   ;         ;   ;       ;    ;   ;  ;;  ;
;   ;   ;  ;   ;  ;      ;   ; ;  ;  ; ;   ;  ;   ;   ;         ;   ;       ;    ;   ;  ;   ;
;   ;   ;  ;   ;  ;      ;   ; ;  ;  ; ;;;;;  ;   ;   ;      ;;;;   ;       ;    ;   ;  ;   ;
;   ;   ;  ;   ;  ;      ;   ; ;  ;  ; ;      ;   ;   ;     ;   ;   ;       ;    ;   ;  ;   ;
;   ;  ;;  ;   ;  ;      ;  ;; ;  ;  ; ;      ;   ;   ;     ;  ;;   ;       ;    ;   ;  ;   ;
;    ;; ;   ;;;    ;;;;   ;; ; ;  ;  ;  ;;;;  ;   ;    ;;;   ;;  ;   ;;;    ;     ;;;   ;   ;
;
;
;

;; document-variable : stx[identifier,original] phase-level -> void
(define (document-variable stx phase-level)
  (define defs-text (current-annotations))
  (when defs-text
    (define binding-info (identifier-binding stx phase-level))
    (when (and (pair? binding-info)
               (syntax-position stx)
               (syntax-span stx))
      (define start (- (syntax-position stx) 1))
      (define fin (+ start (syntax-span stx)))
      (define source-editor (find-source-editor stx))
      (when source-editor
        (define info (get-index-entry-info binding-info))
        (when info
          (define-values (entry-desc path definition-tag tag) (apply values info))
          (send defs-text syncheck:add-text-type
                source-editor start fin
                'document-identifier)
          (send defs-text syncheck:add-docs-menu
                source-editor
                start
                fin
                (syntax-e stx)
                (build-docs-label entry-desc)
                path
                definition-tag
                tag))))))

(define (build-docs-label entry-desc)
  (define libs (exported-index-desc-from-libs entry-desc))
  (cond
    [(null? libs) (format (string-constant cs-view-docs) (exported-index-desc-name entry-desc))]
    [else
     (format (string-constant cs-view-docs-from)
             (format (string-constant cs-view-docs) (exported-index-desc-name entry-desc))
             (apply string-append (add-between (map (λ (x) (format "~s" x)) libs) ", ")))]))

;
;
;
;   ;       ;
;           ;
;           ;                     ;
;   ;    ;; ;        ;;;    ;;;  ;;;;   ;;;
;   ;   ;  ;;       ;      ;   ;  ;    ;
;   ;  ;    ;       ;;    ;    ;  ;    ;;
;   ;  ;    ;        ;;   ;;;;;;  ;     ;;
;   ;  ;    ;          ;  ;       ;       ;
;   ;   ;  ;;          ;   ;      ;       ;
;   ;    ;; ;       ;;;     ;;;;   ;;  ;;;
;
;
;

(define (lookup-phase-to-mapping phase-to key [n key])
  (hash-ref! phase-to key (λ () (make-id-set n))))

;; make-id-set : -> id-set
(define (make-id-set n) (make-free-id-table #:phase n))

(define (add-init-exp phase-to id init-exp phase)
  (when (original-enough? id)
    (define mapping (lookup-phase-to-mapping phase-to phase))
    (define old (free-id-table-ref mapping id '()))
    (free-id-table-set! mapping id (cons init-exp old))))

(define dont-add-binder+mods (gensym 'dont-add-binder+mods))

;; add-id : id-set identifier -> void
(define (add-id mapping id level-of-enclosing-module #:mods [mods dont-add-binder+mods])
  (when (original-enough? id)
    (unless (syntax-property id 'identifier-as-keyword)
      (define shifted-id (syntax-shift-phase-level id level-of-enclosing-module))
      (define old (free-id-table-ref mapping shifted-id '()))
      (define no-span-id
        (if (syntax-property shifted-id 'implicit-made-explicit)
            (datum->syntax shifted-id
                           (syntax-e shifted-id)
                           (vector (syntax-source shifted-id)
                                   (syntax-line shifted-id)
                                   (syntax-column shifted-id)
                                   (syntax-position shifted-id)
                                   0)
                           shifted-id)
            shifted-id))
      (define with-binder+mods
        (if (equal? mods dont-add-binder+mods)
            no-span-id
            (binder+mods no-span-id mods)))
      (free-id-table-set! mapping shifted-id (cons with-binder+mods old)))))

(define (original-enough? x)
  (or (syntax-original? x)
      (syntax-property x 'original-for-check-syntax)))

;; get-idss : id-set -> (listof (listof identifier))
(define (get-idss mapping)
  (for/list ([(x y) (in-dict mapping)])
    y))

;; get-ids : id-set identifier -> (union (listof identifier) #f)
(define (get-ids mapping var)
  (free-id-table-ref mapping var #f))

(define build-trace%
  (class (annotations-mixin object%)
    (init-field src)
    (define trace '())

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))

    ;; send over the non _ variables in the message to the main drracket place
    (define-syntax (log stx)
      (syntax-case stx ()
        [(_ name args ...)
         (with-syntax ([(wanted-args ...)
                        (filter (λ (x) (not (regexp-match #rx"^_" (symbol->string (syntax-e x)))))
                                (syntax->list #'(args ...)))])
           #'(define/override (name args ...)
               (add-to-trace (vector 'name wanted-args ...))))]))

    (log syncheck:add-tail-arrow _from-text from-pos _to-text to-pos)
    (log syncheck:add-arrow/name-dup/pxpy
         _start-text start-pos-left start-pos-right start-px start-py
         _end-text end-pos-left end-pos-right end-px end-py
         actual? level require-arrow? name-dup?)
    (log syncheck:add-mouse-over-status _text pos-left pos-right str)
    (log syncheck:add-text-type _text start fin text-type)
    (log syncheck:add-background-color _text start fin color)
    (log syncheck:add-jump-to-definition/phase-level+space _text start end id filename submods phase-level)
    (log syncheck:add-definition-target/phase-level+space _text start-pos end-pos id mods phase-level)
    (log syncheck:add-require-open-menu _text start-pos end-pos file)
    (log syncheck:add-docs-menu _text start-pos end-pos key the-label path definition-tag tag)
    (log syncheck:add-id-set to-be-renamed/poss dup-name?)
    (log syncheck:add-prefixed-require-reference
         _req-src req-pos-left req-pos-right
         prefix _prefix-src prefix-start prefix-end)
    (log syncheck:unused-binder _src left right)
    (log syncheck:add-unused-require _req-src req-pos-left req-pos-right)

    (define/public (get-trace) (reverse trace))
    (define/public (add-to-trace thing)
      (set! trace (cons thing trace)))
    (super-new)))
