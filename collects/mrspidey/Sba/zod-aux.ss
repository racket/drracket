; zodiac-aux.ss
; Helper functions for zodiac structures
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
; ----------------------------------------------------------------------

(define compat
  (lambda (fn)
    (letrec
        ([cl-fn
          (lambda (exp)
            ;;(pretty-print-debug `(compat ,(stripper exp)))
            (let ([r (fn exp cl-fn)])
              (if r
                  r
                  (match exp
                    [($ zodiac:if-form o s f b test then else)
                     (zodiac:make-if-form o s f b
                      (cl-fn test) (cl-fn then) (cl-fn else))]
                    ;;[($ zodiac:lambda-form  o s f b args body)
                    ;; (let* ([args (map-ilist cl-fn args)])
                    ;;   (zodiac:make-lambda-form o s f b args (cl-fn body)))]

                    [($ zodiac:case-lambda-form  o s f b arglists bodies)
                     (zodiac:make-case-lambda-form 
                      o s f b
                       (map cl-fn arglists)
                       (map cl-fn bodies))]
                    [($ zodiac:arglist vars)
                      (zodiac:make-arglist (map cl-fn vars))]
                    [($ zodiac:set!-form o s f b  var val)
                     (zodiac:make-set!-form o s f b (cl-fn var) (cl-fn val))]
                    [($ zodiac:begin-form o s f b bodies)
                     (zodiac:make-begin-form o s f b (map cl-fn bodies))]
                    [($ zodiac:let-values-form o s f b  varss vals body)
                     (let ([varss (map (lambda (vars) (map cl-fn vars))
                                       varss)])
                       (zodiac:make-let-values-form 
                        o s f b
                        varss (map cl-fn vals) (cl-fn body)))]
                    [($ zodiac:letrec*-values-form o s f b  varss vals body)
                     (let ([varss (map (lambda (vars) (map cl-fn vars))
                                       varss)])
                       (zodiac:make-letrec*-values-form 
                        o s f b
                        varss (map cl-fn vals) (cl-fn body)))]
                    [($ zodiac:define-values-form o s f b vars value)
                     (zodiac:make-define-values-form o s f b 
                                                     (map cl-fn vars)
                                                     (cl-fn value))]
                    [($ zodiac:poly-form o s f b exp)
                     (zodiac:make-poly-form o s f b (cl-fn exp))]

                    [($ zodiac:app o s f b fun args)
                     (zodiac:make-app o s f b
                                      (cl-fn fun) (map cl-fn args))]
                    ;;[($ zodiac:delay-form o s f b expr)
                    ;; (zodiac:make-delay-form o s f b (cl-fn expr))]
                    
                    [($ zodiac::-form o s f b exp type)
                     (zodiac:make-:-form o s f b (cl-fn exp) type)]

                    [($ zodiac:type:-form o s f b type attrs) 
                      exp]

                    [($ zodiac:unit-form o s f b imports exports body)
                     (zodiac:make-unit-form o s f b 
                       (map cl-fn imports)
                       (map 
                         (match-lambda
                           [(varref . sym)
                             (cons (cl-fn varref) sym)])
                         exports)
                       (map cl-fn body))]
                    [($ zodiac:compound-unit-form o s f b
                        imports links exports)
                     ;;(pretty-print-debug `compound-unit)
                     (zodiac:make-compound-unit-form
                      o s f b 
                       (map cl-fn imports)
                      (map (match-lambda
                            [(tag exp . imports)
                              ;;(pretty-print-debug `compound-unit-link)
                              (list*
                                tag
                                (cl-fn exp)
                                (map 
                                  (lambda (import)
                                    (if (zodiac:lexical-varref? import)
                                      (cl-fn import)
                                      import))
                                  imports))])
                           links)
                      exports)]
                    [($ zodiac:reference-unit-form o s f b file kind signed?)
                      (zodiac:make-reference-unit-form o s f b 
                        file kind signed?)]

                    [($ zodiac:invoke-unit-form o s f b exp vars)
                     (zodiac:make-invoke-unit-form 
                      o s f b 
                      (cl-fn exp) (map cl-fn vars))]

                    ;; ---------- objects! ----------
                    [($ zodiac:class*/names-form o s f b
                       this super-init super-expr interfaces init-vars 
                       inst-clauses)
                      (zodiac:make-class*/names-form o s f b
                        (cl-fn this)
                        (cl-fn super-init)
                        (cl-fn super-expr)
                        interfaces
                        (cl-fn init-vars)
                        (map cl-fn inst-clauses))]
                    [($ zodiac:public-clause exports internals exprs)
                      (zodiac:make-public-clause 
                        exports
                        (map cl-fn internals)
                        (map cl-fn exprs))]
                    [($ zodiac:private-clause internals exprs)
                      (zodiac:make-private-clause 
                        (map cl-fn internals)
                        (map cl-fn exprs))]
                    [($ zodiac:inherit-clause internals imports)
                      (zodiac:make-inherit-clause 
                        (map cl-fn internals) imports)]
                    [($ zodiac:rename-clause internals inheriteds)
                      (zodiac:make-rename-clause 
                        (map cl-fn internals)
                        (map cl-fn inheriteds))]
                    [($ zodiac:sequence-clause exprs)
                      (zodiac:make-sequence-clause (map cl-fn exprs))]

                    ;; ---------- sexps ----------
                    [($ zodiac:list o s f l len marks)
                     (zodiac:make-list o s f (map cl-fn l) len marks)]

                    [_ exp]))))])
      cl-fn)))

(define compat*
  (lambda (fn)
    (let ([cl-fn (compat fn)])
      (lambda (exp*) (map cl-fn exp*)))))

; ----------------------------------------------------------------------

(define (ast-size defs)
  (let* ([size 0]
         [fn (lambda (x f)
               (set! size
                   (+ size
                      (match x
                        [($ zodiac:quote-form 0 s f b expr)
                         (const-size expr)]
                        [_ 1])))
               #f)])
    ((compat* fn) defs)
    size))

; ----------------------------------------------------------------------
;; const-size
;; Gives the size of a zodiac constant

(define const-size
  (match-lambda
   [(a . d) (+ 1 (const-size a) (const-size d))]
   [(or ($ zodiac:list _ _ _ l) ($ zodiac:improper-list _ _ _ l))
    (const-size l)]
   [(or (? vector? v) ($ zodiac:vector _ _ _ v))
    (+ 1 (apply + (map const-size v)))]
   [else 1]))

; ----------------------------------------------------------------------

(define unparse-dynamic-letd
  (lambda (fn)
    (letrec
        ([cl-fn
          (lambda  (exp)
            (let ([r (fn exp cl-fn)])
              (if r
                  r
                  (match exp
                    [($ zodiac:varref _ _ _ _ sym) sym]
                    [($ zodiac:binding _ _ _ _ sym) sym]
                    [($ zodiac:if-form _ _ _ _ test then else)
                     `(if ,(cl-fn test) ,(cl-fn then) ,(cl-fn else))]
                    ;;[($ zodiac:lambda-form _ _ _ _ args body)
                    ;; (let* ([args (map-ilist cl-fn args)])
                    ;;   `(lambda ,args ,(cl-fn body)))]
                    [($ zodiac:case-lambda-form _ _ _ _ arglists bodies)
                     `(case-lambda
                       ,@(map
                          (lambda (arglist body)
                            (list (cl-fn arglist) (cl-fn body)))
                           arglists bodies))]
                    [($ zodiac:sym-arglist (var)) (cl-fn var)]
                    [($ zodiac:list-arglist vars) (map cl-fn vars)]
                    [($ zodiac:ilist-arglist vars) 
                      (cons (map cl-fn (rdc vars)) (cl-fn (rac vars)))]
                    [($ zodiac:set!-form _ _ _ _ var val)
                     `(set! ,(cl-fn var) ,(cl-fn val))]
                    [($ zodiac:begin-form _ _ _ _ bodies)
                      `(begin ,@(map cl-fn bodies))]
                    [($ zodiac:let-values-form _ _ _ _ varss vals body)
                     (let ([varss (map (lambda (vars) (map cl-fn vars))
                                       varss)])
                       `(let-values ,(map list varss (map cl-fn vals))
                          ,(cl-fn body)))]
                    [($ zodiac:letrec*-values-form _ _ _ _ varss vals body)
                     (let ([varss (map (lambda (vars) (map cl-fn vars))
                                       varss)])
                       `(letrec*-values ,(map list varss (map cl-fn vals))
                          ,(cl-fn body)))]
                    [($ zodiac:define-values-form _ _ _ _ vars value)
                     `(define-values ,(map cl-fn vars) ,(cl-fn value))]
                    [($ zodiac:poly-form _ _ _ _ exp)
                     `(poly ,(cl-fn exp))]
                    [($ zodiac:app _ _ _ _ fun args)
                     `(,(cl-fn fun) ,@(map cl-fn args))]
                    [($ zodiac:quote-form _ _ _ _ expr)
                     (list 'quote (cl-fn expr))]
                    [($ zodiac::-form _ _ _ _ exp type)
                     (list ': (cl-fn exp) type)]
                    [($ zodiac:st:control-form o s f b para val)
                     (list 'st:control para val)]

                    ;; [($ zodiac:unchanged _ _ _ _ expr) (cl-fn expr)]
                    [($ zodiac:unit-form o s f b imports exports body)
                     `(unit (import ,@(map cl-fn imports))
                            (export 
                             ,@(map (match-lambda 
                                     [(i . e) (list (cl-fn i) (zodiac:read-object e))])
                                    exports))
                            ,@(map cl-fn body))]
                    [($ zodiac:compound-unit-form _ _ _ _ 
                        imports links exports)
                     `(compound-unit
                       (import ,@(map cl-fn imports))
                       (link 
                        ,@(map 
                            (match-lambda
                              [(tag exp . imports)
                                (list tag
                                  (cons 
                                    (cl-fn exp) 
                                    (map (match-lambda
                                           [(tag . sym) (list tag sym)]
                                           [(? zodiac:varref? lvr)
                                             (zodiac:varref-var lvr)])
                                      imports)))])
                               links))
                       (export ,@(map
                                  (match-lambda
                                   [(tag i . e) (list tag (list i e))])
                                  exports)))]
                    [($ zodiac:invoke-unit-form _ _ _ _ exp)
                     `(invoke-unit ,(cl-fn exp))]
                    [($ zodiac:reference-unit-form _ _ _ _ file kind signed?)
                      (list 
                        (if signed?
                          'reference-unit
                          'reference-unit/sig)
                        file)]
                    [($ zodiac:struct-form _ _ _ _ 
                       ($ zodiac:symbol _ _ _ tag)
                       parent 
                       (($ zodiac:symbol _ _ _ fields) ...))
                     `(struct
                        ,(if parent (list tag (cl-fn parent)) tag)
                        ,fields)]
                    [($ zodiac:string  _ _ _ text) text]
                    [($ zodiac:boolean _ _ _ text) text]
                    [($ zodiac:char    _ _ _ text) text]
                    [($ zodiac:symbol  _ _ _ text) text]
                    [($ zodiac:number  _ _ _ text) text]
                    [($ zodiac:list    _ _ _ contents) 
                     (map cl-fn contents)]
                    [($ zodiac:improper-list _ _ _ contents)
                     (map-ilist cl-fn contents)]
                    [($ zodiac:vector  _ _ _ contents)
                     (list->vector (map cl-fn contents))]
                    [($ zodiac:define-type-form _ _ _ _ sym type)
                     `(define-type ,sym ,type)]
                    [($ zodiac:define-constructor-form _ _ _ _ sym modes)
                     `(define-constructor ,sym ,@modes)]
                    [x x]
                    ;;[_ (error 'unparse "Bad exp ~s" exp)]
                    ))))])
      cl-fn)))

'(define (stripper exp)
   ((unparse-dynamic-letd (lambda (x cl-fn) #f))
     exp))

(define (stripper exp)
  (if (zodiac:parsed? exp)
    (zodiac:parsed->raw exp)
    'stripper:not-a-parsed))

; ======================================================================
;; Routines to add fields to each zodiac structure
;; mutated and refs fields are only for binding structures

(define (myzodiac:register-client name init-fn)
  (let*-vals
    ([(getter setter) (zodiac:register-client name init-fn)])
    (values
      (lambda (parsed) (getter (zodiac:parsed-back parsed)))
      (lambda (parsed val) (setter (zodiac:parsed-back parsed) val)))))

(define-values
  (parsed-ftype set-parsed-ftype!)
  (myzodiac:register-client 'ftype (lambda () #f)))

(define-values
  (parsed-check set-parsed-check!)
  (myzodiac:register-client 'check (lambda () #f)))

(define-values
  (parsed-atprim set-parsed-atprim!)
  (myzodiac:register-client 'atprim (lambda () #f)))

(define-values
  (app-tvar-args set-app-tvar-args!)
  (myzodiac:register-client 'app-args (lambda () #f)))

(define-values
  (binding-refs set-binding-refs!)
  (myzodiac:register-client 'refs (lambda () 0)))

(define-values
  (binding-mutated set-binding-mutated!)
  (myzodiac:register-client 'mutated (lambda () #f)))

;;(trace binding-mutated)
;;(trace set-binding-mutated!)

;; ----------------------------------------------------------------------
;; Info on varrefs

(define (get-top-level-varref-binding x) 
  (let* ( [b (zodiac:top-level-varref/bind-slot x)]
          [u (unbox b)])
    (if (zodiac:binding? u)
      u
      (let ([u (my-create-binding 
                 (zodiac:varref-var x)
                 (zodiac:zodiac-start x)
                 (zodiac:zodiac-finish x))])
        (set-box! b u)
        u))))

(define (varref-binding varref)
  (match varref
    [($ zodiac:bound-varref) (zodiac:bound-varref-binding varref)]
    [($ zodiac:top-level-varref/bind) (get-top-level-varref-binding varref)]
    [x (error 'varref-backinfo "Bad varref ~s" x)]))

(define my-create-binding
  (case-lambda
    [(name) (my-create-binding name (no-location) (no-location))]
    [(name open close)
      (zodiac:make-binding
        (zodiac:make-origin 'non-source 'spidey)
        open close (zodiac:make-empty-back-box) name name)]))

;; ----------------------------------------------------------------------

(define lambda-flatten-arglist
  (match-lambda
   [($ zodiac:list _ _ _ l) l]
   [($ zodiac:improper-list _ _ _ l x) (append l (list x))]
   [(? zodiac:binding? b) (list b)]
   [() '()]
   [(a . b) (cons a (lambda-flatten-arglist b))]))

; ----------------------------------------------------------------------

(define (no-location) (zodiac:make-location 0 0 0 "no-file"))

(define (location-inc loc)
  (zodiac:make-location 
   0 0
   (add1 (zodiac:location-offset loc))
   (zodiac:location-file loc)))

(define determine-end-first-token
  (lambda (object)
    (cond
      [(or (zodiac:scalar? object)
         (zodiac:varref? object)
         (zodiac:binding?  object)
         (and (zodiac:quote-form? object)
           (or
             (zodiac:boolean? (zodiac:quote-form-expr object))
             (zodiac:char? (zodiac:quote-form-expr object))
             (zodiac:symbol? (zodiac:quote-form-expr object))
             (zodiac:number? (zodiac:quote-form-expr object)))))
        (location-inc (zodiac:zodiac-finish object))]
      [(or (zodiac:sequence? object)
         (zodiac:parsed?     object)
         (zodiac:app?      object))
        (location-inc (zodiac:zodiac-start object))]
      [else (error 'zodiac:determine-end-first-token
              "shouldn't be here ~s" object)])))

; ----------------------------------------------------------------------

(define parsed-value?
  (match-lambda
    [(or
       ($ zodiac:quote-form)
       ;;($ zodiac:lambda-form)
       ($ zodiac:case-lambda-form)
       ($ zodiac:lexical-varref)) #t]
    [($ zodiac:letrec*-values-form 
       _ _ _ _ vars 
       ((? parsed-value?) ...)
       (? parsed-value?)) #t]
    [_ #t]))
      
; ----------------------------------------------------------------------

(define free-refs
  (lambda (exp bindings)
    (let* ( [hash-bindings (make-hash-table)]
            [_ (for-each
                 (lambda (bind) (hash-table-put! hash-bindings bind #t))
                 bindings)]
            [refs '()]
            [fn
              (lambda (exp cl-fn)
                (match exp
                  [($ zodiac:varref)
                    (let ([bind (varref-binding exp)])
                      (when 
                        (hash-table-get hash-bindings bind (lambda () #f))
                        (set! refs (cons bind refs))))
                    #f]
                  [($ zodiac:unit-form) 'do-not-traverse]
                  [_ #f]))])
      ((compat fn) exp)
      refs)))
               
(define (free-vars exp bindings) (list->set (free-refs exp bindings)))

;; ----------------------------------------------------------------------

(define (initialize-mutated defs)
  (let* (;; set binding of mutated variables
          [_ (for-each
               (match-lambda
                 [($ zodiac:define-values-form _ _ _ _ varrefs expr) 
                   (match expr
                     ;; kludge - don't consider define-struct variables
                     ;; to be mutable
                     [($ zodiac:struct-form) (void)]
                     [_
                       (for-each
                         (lambda (varref) 
                           (set-binding-mutated!
                             (get-top-level-varref-binding varref)
                             #t))
                         varrefs)])]
                 [_ (void)])
               defs)]
          [free '()]
          [fn
            (lambda (def cl-fn)
              ;;(display `(initialize-mutated ,(stripper def))) (newline)
              (match def
                [($ zodiac:letrec*-values-form _ _ _ _ varss)
                  (for-each 
                    (lambda (vars)
                      (for-each
                        (lambda (var) (set-binding-mutated! var #t))
                        vars))
                    varss)
                  #f]
                [($ zodiac:set!-form _ _ _ _ var) 
                  ;;(display `(initialize-mutated! ,(stripper var))) (newline)
                  (set-binding-mutated! (varref-binding var) #t)
                  #f]
                [_ #f]))])
    ((compat* fn) defs)
    free))

(define (free-vars-defs defs)
  (let* ( ;; Put binding on slot field of variables that are defined
          ;; and set to mutated
          [_ (for-each
               (match-lambda
                 [($ zodiac:define-values-form _ _ _ _ varrefs expr) 
                   (match expr
                     ;; kludge - don't consider define-struct variables
                     ;; to be mutable
                     [($ zodiac:struct-form) 
                       (for-each
                         (lambda (varref) 
                           (get-top-level-varref-binding varref))
                         varrefs)]
                     [_
                       (for-each
                         (lambda (varref) 
                           (set-binding-mutated!
                             (get-top-level-varref-binding varref)
                             #t))
                         varrefs)])]
                 [_ (void)])
               defs)]
          [free '()]
          [fn
            (lambda (def cl-fn)
              ;;(pretty-debug `(get-tlvr ,(stripper def)))
              (match def
                [($ zodiac:top-level-varref/bind)
                  (when
                    (not 
                      (zodiac:binding?
                        (unbox (zodiac:top-level-varref/bind-slot def))))
                    (set! free 
                      (cons (get-top-level-varref-binding def) free)))
                  #f]
                [($ zodiac:unit-form) 'do-not-traverse]
                [($ zodiac:letrec*-values-form _ _ _ _ varss)
                  (for-each 
                    (lambda (vars)
                      (for-each
                        (lambda (var) (set-binding-mutated! var #t))
                        vars))
                    varss)
                  #f]
                ;; struct-ref in function posn is not a free variable
                [($ zodiac:app _ _ _ _ 
                   ($ zodiac:top-level-varref/bind _ _ _ _ 'struct-ref)
                   args)
                  (for-each cl-fn args)
                  #t]
                [_ #f]))])
    ((compat* fn) defs)
    free))

;; ----------------------------------------------------------------------

(define (zero! p)
  '(let-macro 
     zerostruct
     (lambda (name . fields)
       `(begin
          ;;(display ',(symbol-append 'zodiac: name))
          (when (,(symbol-append 'zodiac: name '?) p)
            ,@(map
                (lambda (field)
                  `(begin
                     ;;(display ',(symbol-append 'zodiac: name '- field))
                     (zero! 
                       (begin0
                         (,(symbol-append 'zodiac: name '- field) p)
                         (,(symbol-append 'zodiac:set- name '- field '!) p 'zro)))))
                fields))))

     (zerostruct parsed back)
     (zerostruct app fun args)
     (zerostruct varref var)
     (zerostruct bound-varref binding)
     (zerostruct binding var orig-name)
     (zerostruct top-level-varref/bind slot)
     (zerostruct arglist vars)
     (zerostruct paroptarglist vars)
     (zerostruct set!-form var val)
     (zerostruct begin-form bodies)
     (zerostruct begin0-form bodies)
     (zerostruct define-values-form vars val)
     (zerostruct let-values-form vars vals body)
     (zerostruct letrec*-values-form vars vals body)
     (zerostruct if-form test then else)
     (zerostruct quote-form expr)
     (zerostruct case-lambda-form args bodies)
     (zerostruct struct-form type super fields)
     (zerostruct unit-form imports exports clauses)
     (zerostruct compound-unit-form imports links exports)
     (zerostruct invoke-unit-form unit variables)
     (zerostruct invoke-open-unit-form unit name-specifier variables)
     (zerostruct class*-form this super-names super-exprs init-vars inst-clauses)
     (zerostruct public-clause exports internals exprs)
     (zerostruct private-clause internals exprs)
     (zerostruct inherit-clause internals imports)
     (zerostruct rename-clause internals imports)
     (zerostruct sequence-clause exprs)
     (when (pair? p)
       (zero! (begin0 (car p) (set-car! p 'zro)))
       (zero! (begin0 (cdr p) (set-cdr! p 'zro))))

     ))

;; ----------------------------------------------------------------------

(define (inline-begins defs)
  (recur loop ([defs defs])
    (match defs
      [() '()]
      [(($ zodiac:begin-form _ _ _ _ bodies) . rest)
        (append (loop bodies) (loop rest))]
      [(def . rest)
        (cons def (loop rest))])))

