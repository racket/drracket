;; atlunit.ss
;; Handles annotated lazy units - units, cmpd-units and reference-units
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------
;; The structures ...

(define-const-typed-structure
  (atlunit-unit struct:atlunit)
  ((: env (listof (cons zodiac:binding FlowType)))
   (: exp zodiac:unit-form)))

(define (create-atlunit-unit env exp)
  (make-atlunit-unit #f env exp))

;; -----

(define-const-typed-structure
  (atlunit-cmpd struct:atlunit)
  ((: cmpd-unit zodiac:compound-unit-form)
   (: times (listof num))
   (: ftype* (listof FlowType))))

(define (create-atlunit-cmpd cmpd-unit time* ftype*)
  (make-atlunit-cmpd #f cmpd-unit time* ftype*))

;; -----

(define-const-typed-structure
  (atlunit-reference struct:atlunit)
  ((: exp zodiac:sexp)))

(define (create-atlunit-reference exp)
  (assert (zodiac:reference-unit-form? exp))
  (make-atlunit-reference #f exp))

;; ----------------------------------------------------------------------
;; (: partial-import-list 
;;    (union false (listof (cons (union FlowType false) time))))

(define (pretty-partial-import-list pil)
  (and pil
    (map (match-lambda
           [(ftype . itime)
             (cons (and ftype (FlowType->pretty ftype)) itime)])
      pil)))

;; ----------------------------------------------------------------------

(define (apply-unit unit partial-import-list)
  (: unit FlowType)
  ;; returns (union atunit Tvar)
  (match (FlowType->Atype unit)
    [(? atlunit? atlunit) (apply-atlunit atlunit partial-import-list)]
    [($ atunit imports exports result expr)
      (let ([imports
              (cond
                [(eq? partial-import-list #f) imports]
                [(= (length imports) (length partial-import-list))
                  (map (match-lambda*
                         [((sym . tvars) (ftype . _))
                           (if ftype
                             (begin
                               (for-each 
                                 (lambda (tvar)
                                   (new-edge! (FlowType->Tvar ftype) tvar)) 
                                 tvars)
                               (list sym))
                             (cons sym tvars))])
                    imports partial-import-list)]
                [else
                  (let ([msg (format "Unit takes ~s imports, given ~s"
                               (length imports) (length partial-import-list))])
                    (if (zodiac:parsed? expr)
                      (mrspidey:warning msg expr 0)
                      (mrspidey:warning msg)))
                  imports])])
        (make-atunit imports exports result expr))]
    [(? Tvar? tvar-u)
      (for-each-with-n
        (match-lambda*
          [((ftype . _) n)
            (new-con! tvar-u
              (create-con (get-unit-import-template n)
                0 (FlowType->Tvar ftype) #f))])
        partial-import-list)
      tvar-u]))

;; ----------------------------------------------------------------------

(define (atlunit->atunit atlunit) 
  ;; returns atunit
  (apply-atlunit atlunit #f))

(define gatlunit (void))

(define (apply-atlunit atlunit partial-import-list)
  (: partial-import-list (listof (cons (union FlowType false) num)))
  ;; (returns atunit)
  (set! gatlunit atlunit)
  (match atlunit
    [(and atlunit ($ atlunit ui))
      (assert (or (eq? ui #f) (atunit? ui)))
      (when (and ui partial-import-list)
        (pretty-debug-unit `(ui ,ui partial-import-list ,partial-import-list))
        (mrspidey:error 
          "Annotated lazy unit invoked more than once"
          (match atlunit
            [($ atlunit-unit _ _ exp) exp]
            [($ atlunit-cmpd _ exp) exp]
            [($ atlunit-reference _ exp) exp])))
      (or ui
        (let* ( [fn
                  (cond
                    [(atlunit-unit? atlunit)  apply-atlunit-unit]
                    [(atlunit-cmpd? atlunit)  apply-atlunit-cmpd]
                    [(atlunit-reference? atlunit) apply-atlunit-reference]
                    [else (mrspidey:internal-error
                            'apply-atlunit->atunit "Not a lazy unit")])]
                [ui (fn atlunit partial-import-list)])
          (set-atlunit-ui! atlunit ui)
          ui))]))

;; ----------------------------------------------------------------------

(define (apply-atlunit-unit atlunit partial-import-list)
  (match atlunit
    [($ atlunit-unit _ env 
        (and unit-exp ($ zodiac:unit-form _ s _ _ imports exports body)))
     (pretty-debug-unit
      `(apply-atlunit-unit
        ,(zodiac:stripper unit-exp)
        ,(and partial-import-list
              (map FlowType->pretty (map car partial-import-list))))
      7)

     (let*-vals
       ( [free-names (zodiac:free-vars-defs body)]
         [free-names2 (setdiff2 free-names imports)]
         [init-env (get-default-bindings free-names2)]
         [env1 (cond
                 [(eq? partial-import-list #f) init-env]
                 [(not (= (length imports) (length partial-import-list)))
                   (mrspidey:error 
                     (format 
                       "Bad number of imports for unit, expected ~s, given ~s"
                       (length imports)
                       (length partial-import-list)))]
                 [else (foldr2
                         (lambda (name ftype env)
                           (if ftype
                             (atenv:extend env name ftype)
                             env))
                         init-env
                         imports
                         (map car partial-import-list))])]
         [(env2 refs result) (traverse-defs body env1)]
         ;;[_ (pretty-print `(env2 ,(atenv->pretty env2)))]
         [env3 (atenv:unflush (atenv:flush! env2))]
         ;;[_ (pretty-print `(env3 ,(atenv->pretty env2)))]
         [exports 
           (map 
             (match-lambda
               [(lvr . z-sym)
                 (let* ( [binding (zodiac:varref-binding lvr)]
                         [ftype (atenv:lookup env3 binding)]
                         [_ (assert (FlowType? ftype) 'atlunit-unit->atunit 
                              binding)]
                         [ftype (link-parsed-ftype! lvr ftype)])
                   (cons (zodiac:read-object z-sym) ftype))])
             exports)]
         [imports
           (map (lambda (name)
                  (cons (zodiac:binding-var name)
                    (filter-map
                      (match-lambda
                        [(name2 . tvar)
                          (and (eq? name name2) tvar)])
                      refs)))
             imports)])
       (make-atunit imports exports result unit-exp))]))

;; ----------------------------------------------------------------------

(define (apply-atlunit-cmpd atlunit partial-import-list)
  (match atlunit
    [($ atlunit-cmpd _
       (and cmpd-unit
         ($ zodiac:compound-unit-form _ start _ _ 
           import-bindings links exports))
       times ftype*)
      (: import-bindings (listof zodiac:lexical-binding))
      (: links
        (listof (list sym zodiac:parsed (listof (cons (union sym #f) sym)))))
      (: times (listof num))
      (: ftype* (listof FlowType))
      (: exports (listof (list sym sym sym)))
    
      (pretty-debug-unit
        `(apply-atlunit-cmpd
           ,(map zodiac:binding-orig-name import-bindings)
           ,(zodiac:stripper cmpd-unit)
           ,(map FlowType->pretty ftype*)
           ,exports
           ,(and partial-import-list
              (map FlowType->pretty (map car partial-import-list)))))

      (letrec*
        ( [time-N (zodiac-time cmpd-unit)]
          [saved-refs '()]              ; forward refs and refs to some imports
          [imports (map zodiac:binding-orig-name import-bindings)]
          [import-env 
            (cond
              [(eq? partial-import-list #f) '()]
              [(not (= (length imports) (length partial-import-list)))
                (mrspidey:warning 
                  (format "Compound-unit requires ~s imports, given ~s"
                    (length imports) (length partial-import-list))
                  start 0)
                '()]
              [else 
                (map
                  (lambda (import-binding import ftype-time)
                    (let* ( [ftype (car ftype-time)]
                            [_ (assert (FlowType? ftype) 'atlunit-3 ftype)]
                            [ftype (link-parsed-ftype! import-binding ftype)]
                            [ftype-time (cons ftype (cdr ftype-time))])
                      (cons import ftype-time)))
                  import-bindings
                  imports
                  partial-import-list)])]
          [import-refs (make-vector (length imports) '())]
          [access-import
            (lambda (sym)
              (match (lookup-or-fail import-env sym (lambda () (cons #f 0)))
                [(ftype . itime)
                  (values
                    (or ftype
                      (let ([tvar (mk-Tvar 'get-export)]
                             [n (index imports sym)])
                        (if n 
                          (vector-set! import-refs n
                            (cons tvar (vector-ref import-refs n)))
                          (mrspidey:warning
                            (format
                              "Symbol ~s not in import list of compound-unit" 
                              sym)
                            (zodiac:zodiac-start cmpd-unit) 7))
                        tvar))
                    (max time-N itime))]))]
          ;; alist of tags and function to access exports
          [tag->access (list (cons #f access-import))]
          [tag.sym->ftype.time
            (lambda (tag sym)
              (assert (or (not tag) (symbol? tag)) tag 'tag.sym->ftype.time2)
              (assert (symbol? sym) sym 'tag.sym->ftype.time2)
              (match (lookup-or-#f tag->access tag)
                [(? procedure? access) (access sym)]
                [#f (let ([tvar (mk-Tvar 'forward-get-export)])
                      (set! saved-refs (cons (list tag sym tvar) saved-refs))
                      (values tvar time-N))]))]
          [last-invoked-unit #f]
          [_ 
            (for-each
              (lambda (link time-U ftype)
                (match-let*
                  ( [(tag _ . i*) link]
                    ;; [_ (pretty-print `(i* ,i* tag ,tag link ,link))]
                    ;; tag is not a zodiac:parsed, so cant hang type off it
                    ;; [ftype (link-parsed-ftype! tag ftype)]
                    [tag (zodiac:read-object tag)]
                    ;; Figure out imports
                    [import-ftype.times
                      (map 
                        (lambda (i)
                          (let*-vals
                            ([(tag sym)
                               (match i
                                 [(tag . sym) 
                                   (values
                                     (zodiac:read-object tag)
                                     (zodiac:read-object sym))]
                                 [($ zodiac:lexical-varref)
                                   (values #f 
                                     (zodiac:binding-orig-name
                                       (zodiac:varref-binding i)))])]
                              [_ (assert (or (not tag) (symbol? tag)) 1)]
                              [(ftype itime) (tag.sym->ftype.time tag sym)]
                              ;;#[ftype (link-parsed-ftype! i ftype)]
                              )
                            (cons ftype itime)))
                        i*)]
                    [time-export (apply max time-N time-U
                                   (map cdr import-ftype.times))]
                    [_ (pretty-debug-unit
                         `(apply-atlunit-cmpd 
                            invoking ,tag ,ftype ,import-ftype.times))]
                    [invoked-unit (apply-unit ftype import-ftype.times)]
                    [_ (pretty-debug-unit
                         `(apply-atlunit-cmpd result tag ,tag ,invoked-unit))]
                    [_ (set! last-invoked-unit invoked-unit)]
                    [access-exports
                      (match invoked-unit
                        [($ atunit imports exports)
                          (lambda (sym)
                            (match (lookup-or-#f exports sym)
                              [#f (mrspidey:warning
                                    (format 
                                      "Exported var ~s not in unit tagged ~s in compound-unit" 
                                      sym tag)
                                    cmpd-unit 9)
                                (values (mk-tvar-void) 0)]
                              [ftype (values ftype time-export)]))]
                        [(? Tvar? tvar-u)
                          (lambda (sym)
                            (let ([tvar (mk-Tvar 'get-export)])
                              (new-con! tvar-u 
                                (create-con (get-unit-export-template sym)
                                  0 tvar #t))
                              (values tvar time-export)))])])

                  (set! tag->access (cons (cons tag access-exports) tag->access))
                  (set! saved-refs
                    (filter
                      (match-lambda
                        [(tag2 sym tvar)
                          (if (eq? tag tag2)
                            (let-values ([(ftype _) (access-exports sym)])
                              (new-edge! (FlowType->Tvar ftype) tvar)
                              #f)
                            #t)])
                      saved-refs))))
              links times ftype*)]

          [unit-imports (map cons imports (vector->list import-refs))]
          [unit-exports 
            (map 
              (match-lambda
                [(and export (tag id . e-id))
                  (let*-vals
                    ([(ftype _) 
                       (tag.sym->ftype.time 
                         (zodiac:read-object tag)
                         (zodiac:read-object id))]
                      ;;#[ftype (link-parsed-ftype! export ftype)]
                      )
                    (cons (zodiac:read-object e-id) ftype))])
              exports)]
          [result
            (match last-invoked-unit
              [#f (wrap-value (mk-tvar-void))]
              [($ atunit _ _ result) result]
              [(? Tvar? tvar-u)
                (let ([tvar (mk-Tvar 'cmpd-unit-result)])
                  (new-con! tvar-u (create-con template-unit 0 tvar #t))
                  tvar)])])

        (make-atunit unit-imports
          unit-exports
          result
          cmpd-unit))]))

;; ----------------------------------------------------------------------
(define regenerating-ftype (void))

(define (apply-atlunit-reference atlunit partial-import-list)
  (match atlunit
    [($ atlunit-reference _ 
       (and N ($ zodiac:reference-unit-form _ _ _ _ file kind)))

      (pretty-debug-unit 
        `(apply-atlunit-reference
           ,(zodiac:location-line (zodiac:zodiac-start N))
           pil ,(pretty-partial-import-list partial-import-list)))

        (let*-vals
          ( [_ (unless (zodiac:string? file)
                 (mrspidey:error
                   "reference-unit requires a string argument, given ~s"
                   file))]
            [file (zodiac:read-object file)]
            [path+file
              (if (relative-path? file)
                (build-path (current-directory) file)
                file)]
            [file-directory (path-only path+file)]
            [(_ file _) (split-path path+file)]
            [za (regexp-replace ".ss$" file ".za")]
            [_ (when (eq? za file) 
                 (mrspidey:error
                   (format "Invalid extension on ~s, requires .ss"
                     file)))]
            [t-N (zodiac-time* N)]
            [za (case (st:save-za-in)
                  [(source-directory)
                    (build-path file-directory za)]
                  [(tmp-directory) 
                    (build-path 
                      (wx:find-path 'temp-dir)
                      (file-name-from-path za))])]
            [t-za (file-modify-seconds za)]
            
            ;; restrict imports to prims, closed schemas, and atstructs
            [partial-import-list-restricted
              (and partial-import-list
                (map
                  (match-lambda
                    [(ftype . itime)
                      (cons
                        (and (fo-FlowType? ftype)
                          (match (fo-FlowType-def ftype)
                            [(or ($ schema _ _ ()) (? atprim?) (? atstruct?))
                              ftype]
                            [_ #f]))
                        itime)])
                  partial-import-list))]
            [_ (pretty-debug-unit 
                 `(pil-restricted 
                    ,(pretty-partial-import-list
                       partial-import-list-restricted)))]

            [port-for-included-unit
              (lambda ()
                (dynamic-let ([current-directory file-directory])
                  (open-code-file file)))]

            [traverse-included-unit
              ;; (zodiac:parsed -> (union atunit atlunit))
              (lambda ()
                (let*-vals
                  ([_ (mrspidey:progress 
                        (format "Analyzing referenced unit ~a" file))]
                    [_ (extend-file-time-cache! path+file t-N)]
                    [exps (zodiac:read* (port-for-included-unit) path+file)]
                    [(parsed-exps free-names) 
                      (dynamic-let
                        ([current-directory file-directory])
                        (my-scheme-expand-program exps))]
                    [_ (unless (= (length parsed-exps) 1)
                         (mrspidey:error 
                           (format
                             "reference-unit file ~s not a single exp" 
                             path+file)))]
                    [parsed-exp (car parsed-exps)]
                    [_ (pretty-debug
                         `(traverse-included-unit
                            ,(zodiac:stripper parsed-exp)))]
                    [gtr global-tref-env]
                    [gtd global-tdef-env]
                    [gtb global-tbang-env]
                    [init-env (get-default-bindings free-names)]
                    [(ftype env refs) (traverse-exp parsed-exp init-env)]
                    [ftype (extract-1st-value ftype)]
                    [ftype 
                      (case kind
                        [(exp) ftype]
                        [(imp) (create-fo-FlowType
                                 (apply-unit ftype
                                   partial-import-list-restricted))])]
                    [savable-ftype (make-savable-ftype ftype)]
                    [atunit (FlowType->Atype savable-ftype)]
                    [_ (unless (atunit? atunit)
                         (mrspidey:error 
                           (format 
                             "reference-unit file did not produce an annotated unit ~s"
                             atunit)
                           N))]
                    [nu-tref (get-prefix global-tref-env gtr)]
                    [nu-tdef (get-prefix global-tdef-env gtd)]
                    [nu-tbang (get-prefix global-tbang-env gtb)]
                    )
                  (init-global-tenv! gtr gtd gtb)
                  (pretty-debug-unit `(nu-tbang ,nu-tbang))
                  (for-each 
                    (match-lambda 
                      [(binding . _)
                        (mrspidey:warning 
                          (format
                            "Unit refs imported var ~s of enclosing unit"
                            (zodiac:binding-orig-name binding)))])
                    refs) 
                  (values
                    exps parsed-exp
                    savable-ftype nu-tref nu-tdef nu-tbang)))]

            [l-start list-ftype]
            [(ftype tref tdef tbang regenerate)
              (if (and (st:unit-read-za) t-za (< t-N t-za))

                ;; ------
                ;; Just load from the file for delta-min
                (let*-vals
                  ( [s (format "Loading ~a" (file-name-from-path za))]
                    [_ (mrspidey:progress s '...)]
                    [(delta-min tref tdef tbang) (read-za za)]
                    [_ (mrspidey:progress s 'done)])

                  (values delta-min tref tdef tbang traverse-included-unit))

                ;; ------
                ;; Regenerate
                (let*-vals 
                  ( [separate-S
                      (and 
                        (st:unit-separate-S)
                        (not (memq (st:unit-simplify) '(none nonempty))))]
                    [s "Saving kernel state:"]
                    [kernel-state 
                      (when separate-S 
                        (begin
                          (mrspidey:progress s '...)
                          (begin0
                            (save-kernel-state)
                            (init-kernel!)
                            (mrspidey:progress s 'done))))]
                    [l2 list-ftype]
                    [(exps parsed-exp ftype nu-tref nu-tdef nu-tbang)
                      ;; we don't consider indirectly included files
                      ;; to be included
                      (parameterize ([record-analyzed-file-hook
                                       (lambda (filename . _)
                                         (void))])
                        (traverse-included-unit))]
                    [l1 list-ftype]
                    ;;[_ (close-constraints (filter Tvar? (get-prefix l1 l2)))]
                    [E (append 
                         (savable-ftype-external-vars ftype)
                         (map cdr nu-tref)
                         (map cdr nu-tdef)
                         (map cdr nu-tbang))]
                    [_ (pretty-debug-unit
                         `(external vars 
                            ,(map FlowType-name E)
                            ,(FlowType-name ftype)
                            ,(map FlowType-name 
                               (savable-ftype-external-vars ftype))
                            ,(map FlowType-name (map cdr nu-tref))
                            ,(map FlowType-name (map cdr nu-tdef))
                            ,(map FlowType-name (map cdr nu-tbang))))]

                    ;;[_ (show-stat-small)]
                    ;; Restore state
                    [s "Restoring kernel state"]
                    [new-kernel-state
                      (when separate-S 
                        (mrspidey:progress s '...)
                        (begin0
                          (save-kernel-state)
                          (restore-kernel-state! kernel-state)
                          (mrspidey:progress s 'done)))]

                    [s "Simplifying constraints"]
                    [_ (mrspidey:progress s '...)]
                    [l3 list-ftype]
                    [(list-tvar tvar->nu) 
                      (minimize-constraints-&-compare
                        (st:unit-simplify) E E l1 l2)]
                    [_ (mrspidey:progress s 'done)]

                    ;; debugging test
                    [_ '(mrspidey:progress "debugging-test" '...)]
                    [_ '(check-unreachable
                         (get-prefix l1 l2)
                         (get-prefix list-ftype l3))]
                    [_ '(really-check-kernel-ok)]
                    [_ '(mrspidey:progress "debugging-test" 'done)]


                    [ftype2 (update-ftype ftype tvar->nu)]
                    [upd-binding 
                      (match-lambda
                        [(sym . tvar) (cons sym (tvar->nu tvar))])]
                    [upd-tref (map upd-binding nu-tref)]
                    [upd-tdef (map upd-binding nu-tdef)]
                    [upd-tbang (map upd-binding nu-tbang)])

                  (when separate-S
                    (when (st:zero-old-constraint-sets)
                      (free-kernel-state! new-kernel-state))
                    (when (st:zero-old-asts)
                      (zodiac:zero! exps)
                      (zodiac:zero! parsed-exp)))
                  ;;(show-stat-small)

                  ;; Stuff to save in list-tvar, ftype2, 
                  ;; upd-tref, upd-tdef, upd-tbang

                  (when (st:unit-write-za)
                    (write-za
                      za list-tvar ftype2
                      upd-tref upd-tdef upd-tbang))

                  (values ftype2 upd-tref upd-tdef upd-tbang
                    (if separate-S
                      traverse-included-unit
                      (lambda () 
                        (values 
                          exps parsed-exp
                          ftype nu-tref nu-tdef nu-tbang))))))]
            [_ (ok-ftype ftype)]
            [l-end list-ftype]
            [_
              ;; Mark new ftypes as from .za file
              (for-each
                (lambda (ftype)
                  (set-FlowType-type-annotation! ftype path+file))
                (get-prefix l-end l-start))]
            [atunit (apply-unit ftype partial-import-list)])

          (pretty-debug-unit `(tbang ,tbang))
                  
          (begin
            (for-each (match-lambda [(s . t) (add-global-tref! s t)]) tref)
            (for-each (match-lambda [(s . t) (add-global-tdef! s t)]) tdef)
            (for-each (match-lambda [(s . t) (add-global-tbang! s t)]) tbang))

          (ok-ftype ftype)

          (record-analyzed-file 
            path+file
            (lambda () (port-for-included-unit))
            (lambda ()
              (set! regenerating-ftype ftype)
              (ok-ftype ftype)
              
              (let*-vals 
                ( [s (format "Regenerating included unit ~a" 
                       (file-name-from-path file))]
                  [_ (mrspidey:progress s)]
                  [(exps parsed-exp ftype2 tref2 tdef2 tbang2) 
                    ((lambda () (regenerate)))])
                (ok-ftype ftype)
                (flow-d! ftype2 ftype)
                (flow-e! tdef2 tdef)
                (flow-e! tbang2 tbang)
                (flow-e! tref tref2)
                (list parsed-exp))))

          (unless (atunit? atunit)
            (mrspidey:error 
              (format
                "reference-unit file did not produce an annotated unit ~s" 
                atunit)))

          atunit)]))
 
;; ----------------------------------------------------------------------

(define (make-savable-ftype ftype)
  ;; Converts an ftype to a "savable" ftype
  ;; ie a tvar, a closed schema, a primitive, an atstruct
  ;; or an atunit with savable exports and result
  ;; atvalues could also be made savable, if there is any need
  (if (Tvar? ftype)
      ftype
      (match (fo-FlowType-def ftype)
        [($ schema tvar tvar* '()) ftype]
        [($ atunit imports exports result exp)
         (create-fo-FlowType
          (make-atunit imports
                       (map (match-lambda
                             [(sym . ftype) 
                              (cons sym (make-savable-ftype ftype))])
                            exports)
                       (make-savable-ftype result)
                       exp))]
        [(and atlunit ($ atlunit)) 
         (make-savable-ftype (create-fo-FlowType (atlunit->atunit atlunit)))]
        [(and pi ($ atprim name type)) ftype]
        [(? atstruct?) ftype]
        [_ (FlowType->Tvar ftype)])))

;; --------------------
        
(define (savable-ftype-external-vars ftype)
  (if (Tvar? ftype)
      (list ftype)
      (match (fo-FlowType-def ftype)
        [($ schema tvar tvar* '()) (list tvar)]
        [($ atprim) '()]
        [($ atunit imports exports result exp)
         (apply append 
                (append (map cdr imports)
                        (map savable-ftype-external-vars (map cdr exports))
                        (list (savable-ftype-external-vars result))))]
        [($ atstruct) '()]
        [x (mrspidey:internal-error 'savable-ftype-external-vars
                                    "Bad Atype ~s" x)])))

;; --------------------

(define (update-ftype ftype tvar->nu)
  ;; Creates a copy of ftype with new type variables
  (if (Tvar? ftype)
      (tvar->nu ftype)
      (create-fo-FlowType
       (match (fo-FlowType-def ftype)
         [($ schema tvar tvar* '())
          (make-schema (tvar->nu tvar) (filter-map tvar->nu tvar*) '())]
         [(? atprim? atprim) atprim]
         [($ atunit imports exports result exp)
          (make-atunit
           (map (match-lambda
                 [(sym . tvar*) (cons sym (map tvar->nu tvar*))])
                imports)
           (map (match-lambda
                 [(sym . ftype) 
                  (cons sym (update-ftype ftype tvar->nu))])
                exports)
            (update-ftype result tvar->nu)
            exp)]
         [(? atstruct? atstruct) atstruct]))))

;; --------------------
        
(define (flow-d! ftype2 ftype)
  (let ([E2 (savable-ftype-external-vars ftype2)]
        [E  (savable-ftype-external-vars ftype)])
    (pretty-debug-unit
      `(E ,(map FlowType-name E) E2 ,(map FlowType-name E2)))
    (unless (= (length E) (length E2))
      (mrspidey:error
        (format "The .za file is incompatible, and may be of date (~s ~s)"
          (length E) (length E2))))
    (for-each new-bidir-edge! E E2)))

;; --------------------

(define (ok-ftype ftype)
  (pretty-debug-unit `(ok-ftype ,(FlowType-name ftype)))
  (let ([E  (savable-ftype-external-vars ftype)])
    (pretty-debug-unit `(E ,(map FlowType-name E)))
    (for-each
      (lambda (tvar)
        (assert (list? (FlowType-arrowto tvar)) 
          'ok-ftype (FlowType-num ftype)))
      E)))

;; --------------------
        
(define (flow-e! env env2)
    (assert (= (length env) (length env2)) 'flow-e! env env2)
    (for-each new-bidir-edge! (map cdr env) (map cdr env2)))

;; ----------------------------------------------------------------------

;(trace apply-unit)
;(trace apply-atlunit)
;(trace apply-atlunit-unit)
;(trace apply-atlunit-cmpd)
;(trace apply-atlunit-reference)
;(trace atlunit->atunit)
