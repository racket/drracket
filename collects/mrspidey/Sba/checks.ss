;; smart-checks.ss - identifies unsafe operations
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


;;-------------------------------------------

(define-struct annotation (loc))
(define-struct (check-annotation struct:annotation) (text num rest))
(define-struct (uncheck-annotation struct:annotation) (text))
(define-struct (type-annotation struct:annotation) (end-first finish FlowType))

;; ------------------------------

(define check-annotations   '())        ; store away checks for mred
(define uncheck-annotations '())        ; store away unchecked prims

;;-------------------------------
;; Counters

(define prim-count  (void))
(define lam-count   (void))
(define prim-apps   (void))
(define user-apps   (void))
(define ivar-count  (void))
(define type-assert-count (void))
(define type-assert-failed-total (void))

(define prim-checks-def (void))
(define lam-checks-def  (void))
(define ap-checks-def   (void))
(define ivar-checks-def (void))
(define type-assert-failed-def (void))

(define total-checks (void))

;; --------------------

(define init-counts
  (lambda ()
    (set! prim-count (make-counter))
    (set! lam-count  (make-counter))
    (set! prim-apps  (make-counter))
    (set! user-apps  (make-counter))
    (set! ivar-count  (make-counter))
    (set! type-assert-count (make-counter))
    (set! type-assert-failed-total (make-counter))
    (set! total-checks (make-counter))
    (reset-counts)))

;; These three count per expression 
;; need to be reset with every definition traversed.

(define reset-counts
  (lambda ()
    (set! prim-checks-def (make-counter))
    (set! lam-checks-def  (make-counter))
    (set! ap-checks-def   (make-counter))
    (set! ivar-checks-def (make-counter))
    (set! type-assert-failed-def (make-counter))))

 (define make-counter
   (lambda ()
     (let ([num 0])
       (lambda (n)
         (set! num (+ n num))
         num))))

;; ----------------------------------------

(define calc-checks
  (lambda (defs)
    (mrspidey:add-summary (format "CHECKS:"))
    (init-counts)
    (set! check-annotations   '()) 
    (set! uncheck-annotations '()) 
    (for-each-with-n
     (lambda (def n)
       (mrspidey:zprogress "Checking" (zodiac:zodiac-start def))
       (match def
         [($ zodiac:define-values-form _ open _ _ 
            ($ zodiac:list _ _ _ ($ zodiac:varref _ _ _ _ sym))
            _)
          (reset-counts)
          ((zodiac:compat check-fn) def)
           ;;(make-check-summary-line sym open)
           ]
         [_
          (reset-counts)
          ((zodiac:compat check-fn) def)
          '(make-check-summary-line 
             (format "<expr> at line ~s" 
               (zodiac:location-line 
                 (zodiac:zodiac-start def)))
             (zodiac:zodiac-start def))
           ]))
     defs)
    (unless (null? defs)
      (mrspidey:zprogress "Checking" (zodiac:zodiac-finish (rac defs))))
    (make-check-summary "")
    (list check-annotations uncheck-annotations)))

;; ----------

(define check-fn
  (lambda (M unp)
    (pretty-debug-check `(check-fn ,(zodiac:stripper M)))
    (match M
      ;; ivars
      [($ zodiac:app _ _ _ _
         ($ zodiac:varref _ open _ _ (and ivar-sym (or '#%uq-ivar 'uq-ivar)))
         (obj-exp ivar-arg))
        (=> fail)
        (pretty-debug-check `(ivar ,ivar-sym))

        ;; Check the ivar is ok
        (let*-vals
          ( [ftype (zodiac:parsed-ftype obj-exp)]
            [ftype (and ftype (extract-1st-value ftype))]
            [(tvars-ivarset-ok? set-tvar-ivarset-ok!) 
              (alloc-Tvar-field)])
          (letrec
            ([check-ivarset-ok?
               (lambda (tvar sym)
                 (pretty-debug-check `(check-ivarset-ok? ,(Tvar-name tvar)))
                 (set-tvar-ivarset-ok! tvar #t)
                 (andmap
                   (match-lambda
                     [($ AV _ (? (lambda (t) (eq? t template-ivarset)))
                        misc fields+)
                       (assert (list? misc) 'ivar-check)
                       (or 
                         (memq sym misc)
                         (let ([parent (vector-ref fields+ 0)])
                           (and (not (null? (get-Tvar-objs parent)))
                             (check-ivarset-ok? parent sym)))
                         (begin
                           (pretty-debug-check
                             `(ivar-failure ,sym ,misc ,(Tvar-name tvar)))
                           #f)
                         )]
                     [_ #t])
                   (get-Tvar-objs tvar)))])
            (if 
              (match ivar-arg
                [($ zodiac:quote-form _ _ _ _ ($ zodiac:symbol _ _ _ sym))
                  (and (Tvar? ftype) (check-ivarset-ok? ftype sym))]
                [_ #f])
              (begin
                (ivar-count 1)
                (add-uncheck! open (symbol->string ivar-sym)))
              (begin
                (ivar-count 1)
                (ivar-checks-def 1)
                (mrspidey:add-summary "ivar check" open 0)
                (add-check! open (symbol->string ivar-sym))))))

        ;; Check the primitive application - NOT!
        '(match ivar-arg
           [($ zodiac:quote-form _ _ _ _ ($ zodiac:symbol _ open _ sym))
             (let* ([ftype (zodiac:parsed-ftype M)])
               (when ftype
                 (let ([ftype (extract-1st-value ftype)])
                   (when (Tvar? ftype)
                     (pretty-debug-check '(ivar-tvar))
                     (match (get-Tvar-objs ftype)
                       [(($ AV _ (? (lambda (t) (eq? t template-lam)))
                           (and pi ($ atprim name type))))
                         (pretty-debug-check '(ivar-lam-AV))
                         (case (check-prim pi (zodiac:parsed-ftype M))
                           [(#t)
                             (prim-count 1)
                             (add-uncheck! open (symbol->string sym))]
                           [(#f)
                             (prim-count 1)
                             (mrspidey:add-summary "Method check" open 0)
                             (prim-checks-def 1)
                             (add-check! open (symbol->string sym) ftype)
                             (pretty-debug `(CHECK-fo-prim ,(zodiac:stripper M)))
                             (zodiac:set-parsed-check! ivar-arg #t)]
                           [(not-function-type) 
                             (pretty-debug-check '(ivar-not-fn-type))]
                           [(not-function-AV) (printf "ivar:not-function-AV~n")])]
                       [_ (void)])))))]
           [_ (void)])
        (fail)]
      [($ zodiac:app _ _ _ _ fn args)
        (unless (atprim? (zodiac:parsed-atprim fn))
          (check-ap M))
        #f]
      [($ zodiac:case-lambda-form) 
       (check-lambda M)
       #f]
      [($ zodiac:top-level-varref)
       (if (zodiac:parsed-atprim M)
           (check-ho-prim M)
           ;; Otherwise should do bound? check, but don't
           (void))
       #f]
      [($ zodiac::-form)
       (check-type-assertion M)
       #f]
      [_ #f])))

;;-------------------------------------------

(define (parsed-Tvar x) 
  ;;(pretty-print `(parsed-Tvar ,(zodiac:stripper x)))
  (if (zodiac:parsed-ftype x)
    (let* ( [ftype (zodiac:parsed-ftype x)]
            [tvar (FlowType->Tvar ftype)])
      (pretty-debug-check
        `(parsed-Tvar ,(zodiac:stripper x) ,(FlowType-name ftype) 
           ,(Tvar-name tvar)))
      tvar)
    (mk-tvar-empty)))

(define check-ap
  (match-lambda
    [(and app ($ zodiac:app _ open _ _ fun args))
      (pretty-debug-check
        `(check-ap,(zodiac:stripper fun) ,(zodiac:parsed-ftype fun)))
      (let ([ftype-fn (zodiac:parsed-ftype fun)]) 
        (match (and (FlowType? ftype-fn) (FlowType->Atype ftype-fn))
          [($ atprim) (prim-apps 1)]
          [_ (user-apps 1)]))
      (let ([tvar (parsed-Tvar fun)])
        (unless 
          (or
            (st:all-checks)
            (Tvar-in-type? tvar 
              '(mvalues (cons (lambda _ _) (nil)))
              '()
              (lambda (a)
                (mrspidey:error 'check-ap 
                  "Reference to unbound type var ~s" a))))
          ;; Check the application
          (ap-checks-def 1)
          (mrspidey:add-summary "Application check" open 0)
          (add-check! open "(" tvar)
          (pretty-debug `(CHECK-ap ,(zodiac:stripper app)))
          (zodiac:set-parsed-check! app #t)))]))

(define check-lambda
  (match-lambda
    [(and lam ($ zodiac:case-lambda-form _ open _ _ arglists bodies))
      (pretty-debug-check `(check-lam ,(zodiac:stripper lam)))
      ;; put arity check on lambda 
      (lam-count 1)
      (let* ( [tvar (parsed-Tvar lam)]
              [tvar (extract-1st-value tvar)])
        (match (get-Tvar-objs tvar)
          [(($ AV _ (? (lambda (t) (eq? t template-lam))) misc _ #(domain))
             . _) 
            ;; rest may be other AV-lam's for this case-lambda
            (let* ([arglist->ilist
                     (match-lambda
                       [($ zodiac:sym-arglist)
                         '_]
                       [($ zodiac:list-arglist vars) 
                         (map (lambda (_) '_) vars)]
                       [($ zodiac:ilist-arglist vars)
                         (recur loop ([vars vars])
                           (match vars
                             [(x) '_]
                             [(a . b) `(_ . ,(loop b))]))])]
                    [type
                      (recur loop ([ilists (map arglist->ilist arglists)])
                        (pretty-debug-check `(ilists ,ilists))
                        (if (memq '_ ilists)
                          '_
                          (let ( [nils 
                                   (filter null? ilists)]
                                 [non-nils
                                   (filter 
                                     (lambda (x) (not (null? x)))
                                     ilists)])
                            (cond
                              [(null? non-nils)
                                'nil]
                              [(null? nils) 
                                `(cons _ ,(loop (map cdr non-nils)))]
                              [else
                                `(union
                                   nil
                                   (cons _ ,(loop (map cdr non-nils))))]))))])
              (pretty-debug-check `(type ,type))
                    
              (when
                (or
                  (st:all-checks)
                  (not (Tvar-in-type? 
                         domain (expand-input-type type) '()
                         (lambda (a)
                           (mrspidey:internal-error 'check-lambda
                             "unbound tvar")))))
                ;; Check it
                (lam-checks-def 1)
                (mrspidey:add-summary "Arity check" open 0)
                (add-check! open "lambda" domain)
                (zodiac:set-parsed-check! lam #t)))]
          [_;; never analyzed
            (void)]))]))

(define (check-ho-prim M)
  ;; name : name-structure of prim
  ;; open: source location for primitive
  ;; returns correct primitive and either 1 or 0

  (pretty-debug-check `(check-ho-prim ,(zodiac:stripper M)))

  (match-let*
    ( [($ zodiac:varref _ open _ _ sym) M]
      [(and atprim ($ atprim sym tschema _ _ _)) (zodiac:parsed-atprim M)])

    (match (parsed-Tvar M)
      [(? Tvar? tvar)

        (case (check-prim atprim tvar)
          [(#t)
            (prim-count 1)
            (add-uncheck! open (symbol->string sym))]
          [(#f)
            (prim-count 1)
            (mrspidey:add-summary (format "~s check" sym) open 0)
            (prim-checks-def 1)
            (add-check! open (symbol->string sym) tvar)
            (pretty-debug `(CHECK-fo-prim ,(zodiac:stripper M)))
            (zodiac:set-parsed-check! M #t)]
          [(not-function-type) (void)]
          [(not-function-AV) 
            ;(printf "check-hoo-prim: not-function-AV~n")
            (void)])])))

;; ----------------------------------------------------------------------

(define (check-prim atprim tvar)

  (pretty-debug-check `(check-prim ,atprim ,(Tvar-name tvar) ))

  (match (get-Tvar-objs (extract-1st-value tvar))
    [(($ AV _ template misc _ #(domain))) 
      (check-prim-domain atprim domain)]
    [_ 'not-function-AV]))

(define (check-prim-domain atprim domain)
  (pretty-debug-check `(check-prim-domain ,atprim ,(Tvar-name domain) ))
  (match-let*
    ( [($ atprim sym tschema _ _ _) atprim]
      [schemas (match tschema
                 [('case-> . schemas) schemas]
                 [schema (list schema)])])

    (ormap
      (lambda (schema)

        (let-values ([(forall type) (split-schema schema)])
          (match type
            [('lambda expected-domain _)
              (pretty-debug-check `(check-prim domain ,(Tvar-name domain)))
              (if (and 
                    (Tvar-in-type? 
                      domain expected-domain forall
                      (lambda (a)
                        (mrspidey:warning
                          (format
                            "Reference to unbound type var ~s in domain of ho primitive ~s" 
                            a sym))
                        (lambda (AV) #f)))
                    (not (st:all-checks)))
                #t
                (begin
                  (pretty-debug-check 
                    `(check-prim-domain failed on ,sym
                       ,(FlowType-name domain)
                       ,expected-domain))
                  #f))]
            [_
              ;; Not a primitive function
              (pretty-debug-check `(Not a prim fn ,type))
              'not-function-type])))

      schemas)))

;; ----------------------------------------------------------------------

(define check-type-assertion
  (match-lambda
   [(and M ($ zodiac::-form _ open _ _ exp type))
    (pretty-debug-check `(check-type-assertion ,(zodiac:stripper M)))
    (type-assert-count 1)
    (let* ([tvar (extract-1st-value (parsed-Tvar exp))]
           [in-type?
            (lambda (type) 
              (Tvar-in-type? 
               tvar (expand-input-type type) '()
               (lambda (a)
                   (mrspidey:warning
                     (format "Unbound type variable ~s" a)
                     open 3)
                 (lambda (AV) #f))))])
      (unless 
          (match type
            [('exact type) 
             ;; Can't do exact yet - best is nonempty
             (and (not (null? (get-Tvar-objs tvar))) (in-type? type))]
            [_ (in-type? type)])
        ;; Is bad
        (type-assert-failed-def 1)
        (type-assert-failed-total 1)
        (add-check! open "(")
        (mrspidey:warning
         (format "Type assertion ~s failed"
                 (list ': '... ;;(zodiac:stripper exp)
                       type))
         (zodiac:zodiac-start M) 
         2)))]))

;; ----------------------------------------------------------------------

'(define (Tvar-in-global-type? ftype type where)
  (Tvar-in-type? 
   Tvar 
   (expand-input-type type)
   (lambda (fv)
     (lookup-or-fail 
      global-tdef-env fv
      (lambda ()
        (mrspidey:error
         (format 
          "Reference to unbound type var ~s in ~a ~s"
          fv where type)))))))

;; ----------------------------------------------------------------------

(define (add-check! open text . rest)
  (set! check-annotations 
      (cons (make-check-annotation open text (total-checks 0) rest)
            check-annotations))
  (total-checks 1))

(define (add-uncheck! open text)
  (set! uncheck-annotations 
      (cons (make-uncheck-annotation open text) uncheck-annotations)))

(define (show-checks)
  (for-each
   (match-lambda
    [($ annotation ($ zodiac:location l c o f) text)
     (printf "File: ~s offset ~s text ~s~n"
             (file-name-from-path f) o text)])
   check-annotations))

;; ----------------------------------------------------------------------

(define (file-wrapper-start outport)
  ;; Writes prefix to file
  ;(fprintf outport ";; Generated by SBA Soft Scheme ~a~%" st:version)
  (fprintf outport ";; Control string ")
  (for-each (lambda (x) (fprintf outport" '~a" x)) (mrspidey:control-fn))
  (fprintf outport "~n~n"))

(define (file-wrapper-end outport)
  (newline outport)
  '(for-each (lambda (s) (fprintf outport ";; ~a" s))
             (reverse summary))
  (void)
  )

;; ----------------------------------------------------------------------

'(define (write-soft-file outfile)
   ;; Writes soft typed file - macro expanded with checks - executable
   (when (file-exists? outfile) (delete-file outfile))
   (let ([outport (open-output-file outfile)])
    
     (file-wrapper-start outport)
     (for-each
      (lambda (def)
        (pretty-print 
         ((zodiac:unparse-dynamic-letd
           (lambda (exp cl-fn)
             (if (zodiac:parsed-check exp)
                 (match exp
                   [($ zodiac:app _ open close back fun args)
                    `(CHECK-ap ,(cl-fn fun) ,@(map cl-fn args))]
                   [($ zodiac:lambda-form _  open close back 
                       args body level)
                    (let* ([args (map-ilist cl-fn args)])
                      `(CHECK-lambda ,args ,(cl-fn body)))]
                   [($ zodiac:lexical-varref
                       _ open close _
                       ($ zodiac:bound _ _ _ _ sym) 
                       Tvar-box)
                    (symbol-append 'CHECK- sym)])
                 #f)))
          def)
         outport)
        (newline outport))
      defs-bind)
     (file-wrapper-end outport)))      

;; ----------------------------------------------------------------------

'(define (write-annotated-file outfile source-thunk)
   ;; As source file, but with checks
   (when (file-exists? outfile) (delete-file outfile))
   (let* ([inport (source-thunk)]
          [outport (open-output-file outfile)]
          [checks (map
                   (match-lambda
                    [($ annotation loc)
                     (cons (zodiac:location-offset loc) rest)])
                   check-annotations)]
          [checks (sort
                   (match-lambda*
                    [((ofs1 . _) (ofs2 . _)) (< ofs1 ofs2)])
                   checks)])
    
     (file-wrapper-start outport)
     (recur loop ([pos 0][checks checks])
       (let ([c (read-char inport)])
         (unless (eof-object? c)
           (match checks
             [(( (? (lambda (p) (= p pos)))
                 num name . _) 
               . _) 
              (let ([to-drop
                     (match name
                       ["("
                        ;; Is an application check
                        (assert (char=? c #\())
                        (display (format "(CHECK-ap ~s " num)
                                 outport)
                        ""]
                       ["lambda"
                           ;; Is a lambda check
                           (assert (char=? c #\())
                         (display (format "(CHECK-lambda ~s " num)
                                  outport)
                         "lambda"]
                       [prim
                        ;; Is a primitive check
                        (assert (char=? c (string-ref prim 0)))
                        (display (format "(CHECK-~a ~s)" name num)
                                 outport)
                        (substring prim 1 (string-length prim))])])
                (recur loop2 ([pos (add1 pos)]
                              [s (string->list to-drop)])
                  (cond
                   [(null? s) (loop pos (cdr checks))]
                   [(char=? (read-char inport) (car s))
                    (loop2 (add1 pos) (cdr s))]
                   [else
                    (error 'write-annotated-file
                           "File not as expected")])))]
             [_                       
              (write-char c outport)
              (loop (add1 pos) checks)]))))
    
     (close-input-port inport)
     (file-wrapper-end outport)
     (close-output-port outport)))

;; --------------------

(define make-check-summary-line
  (lambda (name src)
    (let ([total (+ (prim-checks-def 0) (lam-checks-def 0) 
                    (ap-checks-def 0) (type-assert-failed-def 0))])
      (unless (= 0 total)
        (let* ((s (format "~a~a " (padr name 19) (padl total 3)))
               (s (if (< 0 (prim-checks-def 0))
                      (format "~a (~a prim)" s (prim-checks-def 0))
                      s))
               (s (if (< 0 (lam-checks-def 0))
                      (format "~a (~a lambda)" s (lam-checks-def 0))
                      s))
               (s (if (< 0 (ap-checks-def 0))
                      (format "~a (~a ap)" s (ap-checks-def 0))
                      s))
               (s (if (< 0 (type-assert-failed-def 0))
                      (format "~a (~a type assertions)" 
                              s (type-assert-failed-def 0))
                      s)))
          (mrspidey:add-summary s src 0))))))

;; --------------------

(define make-check-summary
  (lambda (hdr)
    (let* ([f (lambda (s) (mrspidey:add-summary s))]
           [total-possible 
            (+ (user-apps 0) (prim-count 0) (lam-count 0)
               (type-assert-count 0))]
           [percentage
            (if (= 0 total-possible)
                0
                (string->number
                 (chop-number
                  (exact->inexact (* (/ (total-checks 0)
                                        total-possible) 100))
                  4)))])
      
      (f (format "~a~a~a~a" 
                 hdr
                 (padr "TOTAL CHECKS:" 19)
                 (padl (total-checks 0) 3)
                 (format "  (of ~s possible checks is ~s%)"
                         total-possible percentage)))
      
      (unless (zero? (type-assert-failed-total 0))
        (f (format "~a~a~a~a" 
                   hdr
                   (padr "FAILED ASSERTIONS:" 19)
                   (padl (type-assert-failed-total 0) 3)
                   (format "  (of ~s total type assertions)" 
                           (type-assert-count 0)))))
      
      )))

;;----------------------------------------------------------------------

(define (calc-type-annotations defs)
  (let* ( [type-annotations '()]
          [locs-done-table-size 2048]
          [locs-done-mask 2047]
          [loc->ndx (lambda (loc)
                      (bitwise-and (zodiac:location-offset loc)
                        locs-done-mask))]
          [locs-done-table (make-vector locs-done-table-size '())]
          [fn 
            (lambda (exp cl-fn)
              (match exp
                [($ zodiac:parsed origin start finish)
                  (let* ([ftype (zodiac:parsed-ftype exp)]
                          [end-first (zodiac:determine-end-first-token exp)])

                    (pretty-debug 
                      `(ftype ,(and (FlowType? ftype) (FlowType-name ftype))
                         start ,(zodiac:location-offset start)))

                    (when 
                      (and
                        (FlowType? ftype)
                        ;;(memq (zodiac:origin-who origin) '(source reader))
                        )

                      ;; Don't add type-annotation if something else has this
                      ;; start location
                      (let ([ndx (loc->ndx start)])
                        (when (not (find
                                     (lambda (l) 
                                       (= (zodiac:location-offset l)
                                         (zodiac:location-offset start)))
                                     (vector-ref locs-done-table ndx)))
                          ;; Check FlowType points to exp
                          (assert (or (eq? (FlowType-expr ftype) exp)
                                    (eq? (FlowType-expr ftype) #t))
                            'calc-type-annotation ftype
                            (FlowType-expr ftype) exp)
                          '(set-FlowType-expr! ftype exp)
                          (set! type-annotations 
                            (cons 
                              (make-type-annotation 
                                start end-first finish ftype) 
                              type-annotations))
                          (vector-set! 
                            locs-done-table ndx
                            (cons start (vector-ref locs-done-table ndx)))))))
                  #f]
                [_ #f]))])

    (map-with-n
     (lambda (exp n)
       (begin
         (mrspidey:zprogress "Typing" (zodiac:zodiac-start exp))
         ((zodiac:compat fn) exp)))
     defs)
    (unless (null? defs)
      (mrspidey:zprogress "Typing" (zodiac:zodiac-finish (rac defs))))

    type-annotations))

;; ----------------------------------------------------------------------

















