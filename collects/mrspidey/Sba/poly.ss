;; poly.ss
;; ----------------------------------------------------------------------
;; Preprocesses the program


;; ----------------------------------------------------------------------

(define debugging-topo #f)
(defmacro pretty-debug-topo args
  `(when debugging-topo (pretty-print ,@args)))

; ======================================================================

(define (st:poly file)
  (st: (topo-file file)))
  
; ======================================================================

(define-struct def-info (def out in to from color))

(define (topo-file file)
  (let ([file (normalize-path file)])
    (let*-vals 
      ( [sexps (zodiac:read* (open-code-file file) file)]
        [(defs free-names)
          (dynamic-let ([current-directory (path-only file)])
            (top-level-parse-defs sexps))]
        [prims-refd (map zodiac:bound-var free-names)]
        [_ (pretty-debug-topo `(prims-refd ,prims-refd))]
        [prim-sexps
          (filter-map
            (lambda (name)
              (or
                (ormap
                  (match-lambda
                    [(and d ('define (? (lambda (n) (eq? n name))) _))
                      (let* ( [p (open-output-string)]
                              [_ (pretty-print d p)]
                              [_ (close-output-port p)]
                              [p (open-input-string (get-output-string p))]
                              [p (system-expand-if-necy p)]
                              [d (zodiac:read p 
                                   (zodiac:make-location 1 1 0 "prim-file"))])
                        (d))]
                    [_ #f])
                  r4rs-prims)
                (begin
                  (unless (memq name real-primitives)
                    (printf "Warning: Bad primitive ~s~n" name))
                  #f)))
            prims-refd)]
        [_ (pretty-debug-topo `(prim-sexps ,(map zodiac:stripper prim-sexps)))]
        [all-sexps (append prim-sexps sexps)]
        [(defs free-names)
          (dynamic-let ([current-directory (path-only file)])
            (top-level-parse-defs all-sexps))]
        [def->di
          (lambda (def)
            (let ([di (make-def-info def '() '() '() '() #f)])
              (match def
                [($ zodiac:define-values-form _ _ _ _ lvrs exp)
                  (set-def-info-out! di 
                    (map zodiac:lexical-varref-binding lvrs))
                  (set-def-info-in! di (zodiac:free-refs exp))]
                [exp (set-def-info-in! di (zodiac:free-refs exp))])
              di))]
        [di* (mapLR def->di defs)]
        [_ (for-each
             (lambda (di)
               (pretty-debug-topo
                 `(DEF ,(zodiac:stripper (def-info-def di))
                    ,(map zodiac:bound-var (def-info-out di))
                    ,(map zodiac:bound-var (def-info-in di)))))
             di*)]

        [_ (for-each
             (lambda (di1)
               (for-each
                 (lambda (sym)
                   (for-each
                     (lambda (di2)
                       (when (memq sym (def-info-out di2))
                         (set-def-info-to! di1 
                           (cons di2 (def-info-to di1)))
                         (set-def-info-from! di2
                           (cons di1 (def-info-from di2)))))
                     di*))
                 (def-info-in di1)))
             di*)]
           [_ (pretty-debug-topo "to/from fields filled")]
                      
           [dfs (lambda (di* field-sel)
                  (for-each (lambda (di) (set-def-info-color! di 'white)) di*)
                  (let ([done '()])
                    (letrec ([visit 
                              (lambda (di)
                                (when (eq? (def-info-color di) 'white)
                                  (set-def-info-color! di 'black)
                                  (for-each visit (field-sel di))
                                  (set! done (cons di done))))])
                      (for-each visit di*)
                      done)))]

           ;; do topological sort. See "Intro to Algs", p. 489

           [di*1 (dfs di* def-info-to)]
           ;; di*1 contains definitions before references
           [show-di
            (lambda (di)
              `(DEF
                 ,(map zodiac:bound-var (def-info-out di))
                 ,(map zodiac:bound-var (def-info-in di))))]
           [show (lambda (str di*) 
                   (pretty-debug-topo str)
                   (pretty-debug-topo (map show-di di*)))]
           [_ (show "di*1" di*1)]
           [di*2 (dfs di*1 def-info-from)]
           [_ (show "di*2" di*2)]
           
           ;; Calculate list of strongly connected components

           [_ (for-each (lambda (di) (set-def-info-color! di 'white)) di*)]
           [sccs
            (mapLR
             (lambda (di)
               (let ([scc '()])
                 (recur loop ([di di])
                   (when (eq? (def-info-color di) 'white)
                     (set-def-info-color! di 'black)                     
                     (set! scc (cons di scc))
                     (for-each loop (def-info-to di))))
                 scc))
             di*2)]
           [sccs (filter (lambda (x) (not (null? x))) sccs)]

           [_ (begin
                (pretty-debug-topo 'SCCS)
                (for-each (lambda (scc) (show "scc" scc)) sccs))]

           [my-begin 
            (lambda (first rest)
              (make-zodiac:begin-form 
               first rest
               (zodiac:no-location)(zodiac:no-location) (box #f)))]
         
           [value-def?
            (match-lambda
             [($ zodiac:define-values-form _ _ _ _ _ exp)
              (zodiac:value? exp)]
             [_ #f])]
           
           ;; --- Figure out which are polymorphic
           ;; Go from end up to know if multiple refs for each var
           ;; A ref inside a polymorphic def counts as multiple refs

           [vars-single-ref '()]
           [vars-multi-ref '()]
           [add-multi-ref!
            (lambda (var)
                (set! vars-multi-ref (set-add var vars-multi-ref)))]
           [add-single-ref!
            (lambda (var)
              (if (element-of? var vars-single-ref)
                  (add-multi-ref! var)
                  (set! vars-single-ref (set-add var vars-single-ref))))]

           [sccs2 ;; include polymorphism flag
            (mapRL
             (lambda (scc)
               (let* ( [values?
                         (andmap 
                           (match-lambda 
                             [($ def-info def out) (value-def? def)])
                           scc)]
                       [mutable? 
                         '(ormap 
                            (match-lambda
                              [($ def-info def out)
                                (ormap (lambda (o) (memq o mutable-syms)) out)])
                            scc)]
                       [mutable? #f]
                       [defs (apply append (map def-info-out scc))]
                       [refs (apply append (map def-info-in scc))]
                       [multi-refs
                         (match defs
                           [(def) (element-of? def vars-multi-ref)]
                           [() #f]
                           [defs #t])]
                       [poly-flag 
                         (if (and values? (not mutable?))
                           (if multi-refs 'poly 'poly1ref)
                           #f)])

                 (pretty-debug-topo
                   `( defs  ,(map zodiac:bound-var defs) 
                      refs ,(map zodiac:bound-var refs)
                      values? ,values? mutable? ,mutable?
                      multi-refs ,multi-refs poly-flag ,poly-flag))

                 (if (eq? poly-flag 'poly)
                   (for-each add-multi-ref! refs)
                   (for-each add-single-ref! refs))

                 (cons poly-flag scc)))
             sccs)]

        [_ (pretty-print
             `(SCCS
                ,(map
                   (match-lambda
                     [(poly . di*)
                       (let* ( [o (apply append (map def-info-out di*))]
                               [ov (map zodiac:bound-var o)])
                         (case poly
                           [poly (cons '-->POLY ov)]
                           [poly1ref (cons '-->POLY1REF ov)]
                           [#f ov]))])
                   sccs2)))]

        [nu-program
          ;; Return new program
          (apply append
            (map
              (match-lambda
                [('poly .
                   (($ def-info ($ zodiac:define-values-form _ _ _ _ 
                                  (lvrs) exps))
                     ...))
                  (let ([syms
                          (map (lambda (lvr)
                                 (zodiac:bound-var 
                                   (zodiac:lexical-varref-binding lvr)))
                            lvrs)])
                    (if (= (length syms) 1)
                    `((define ,(car syms)
                        (poly (letrec
                                ([,(car syms) ,(zodiac:stripper (car exps))])
                                ,(car syms)))))
                      `((define-values ,syms
                          (poly (letrec*-values 
                                  ([,syms
                                     (values 
                                       ,@(map zodiac:stripper exps))])
                                (values ,@syms)))))))]
                [(_ . di*)
                  (map zodiac:stripper (map def-info-def di*))])
              sccs2))]

        [outfile (regexp-replace ".ss$" file ".poly.ss")]
        [_ (when (eq? outfile file)  
             (error 'topo-file "Bad suffix on ~s" file))]
        [_ (delete-file outfile)]
        [p3 (open-output-file outfile)])

      (printf "----------PROGRAM----------~n")
      (for-each
        (lambda (def)
          (pretty-debug-topo def)
          (pretty-print def p3))
        nu-program)
      
      (close-output-port p3)
      outfile)))

; ======================================================================

(define zodiac:value?
  (match-lambda
    [(or
       ($ zodiac:quote-form)
       ($ zodiac:lambda-form)
       ($ zodiac:case-lambda-form)
       ($ zodiac:lexical-varref)) #t]
    [($ zodiac:letrec-values-form 
       _ _ _ _ vars 
       ((? zodiac:value?) ...)
       (? zodiac:value?)) #t]
    [_ #f]))

;; ----------------------------------------------------------------------

(define r4rs-prims
  `((define caar (lambda (x) (car (car x))))
    (define cadr (lambda (x) (car (cdr x))))
    (define cdar (lambda (x) (cdr (car x))))
    (define cddr (lambda (x) (cdr (cdr x))))
    (define caaar (lambda (x) (car (car (car x)))))
    (define caadr (lambda (x) (car (car (cdr x)))))
    (define cadar (lambda (x) (car (cdr (car x)))))
    (define caddr (lambda (x) (car (cdr (cdr x)))))
    (define cdaar (lambda (x) (cdr (car (car x)))))
    (define cdadr (lambda (x) (cdr (car (cdr x)))))
    (define cddar (lambda (x) (cdr (cdr (car x)))))
    (define cdddr (lambda (x) (cdr (cdr (cdr x)))))
    (define caaaar (lambda (x) (car (car (car (car x))))))
    (define caaadr (lambda (x) (car (car (car (cdr x))))))
    (define caadar (lambda (x) (car (car (cdr (car x))))))
    (define caaddr (lambda (x) (car (car (cdr (cdr x))))))
    (define cadaar (lambda (x) (car (cdr (car (car x))))))
    (define cadadr (lambda (x) (car (cdr (car (cdr x))))))
    (define caddar (lambda (x) (car (cdr (cdr (car x))))))
    (define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
    (define cdaaar (lambda (x) (cdr (car (car (car x))))))
    (define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
    (define cdadar (lambda (x) (cdr (car (cdr (car x))))))
    (define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
    (define cddaar (lambda (x) (cdr (cdr (car (car x))))))
    (define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
    (define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
    (define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))
    (define list (lambda a a))
    (define length
      (lambda (a)
        (recur loop ((a a) (len 0))
          (if (null? a)
              len
              (loop (cdr a) (+ 1 len))))))
    (define append
      (lambda a
        (letrec ((app2 (lambda (a b)
                         (if (null? a)
                             b
                             (cons (car a) (app2 (cdr a) b))))))
          (recur loop ((a a))
            (cond ((null? a) '())
                  ((null? (cdr a)) (car a))
                  (else (app2 (car a) (loop (cdr a)))))))))
    (define reverse
      (lambda (a)
        (recur loop ((a a) (acc '()))
          (if (null? a)
              acc
              (loop (cdr a) (cons (car a) acc))))))
    (define list-tail
      (lambda (a n)
        (if (zero? n)
            a
            (list-tail (cdr a) (- n 1)))))
    (define list-ref
      (lambda (a n)
        (if (zero? n)
            (car a)
            (list-ref (cdr a) (- n 1)))))
    (define memq
      (lambda (x a)
        (cond ((null? a) #f)
              ((eq? x (car a)) a)
              (else (memq x (cdr a))))))
    (define memv
      (lambda (x a)
        (cond ((null? a) #f)
              ((eqv? x (car a)) a)
              (else (memv x (cdr a))))))
    (define member
      (lambda (x a)
        (cond ((null? a) #f)
              ((equal? x (car a)) a)
              (else (member x (cdr a))))))
    (define assq
      (lambda (x a)
        (cond ((null? a) #f)
              ((eq? x (car (car a))) (car a))
              (else (assq x (cdr a))))))
    (define assv
      (lambda (x a)
        (cond ((null? a) #f)
              ((eqv? x (car (car a))) (car a))
              (else (assv x (cdr a))))))
    (define assoc
      (lambda (x a)
        (cond ((null? a) #f)
              ((equal? x (car (car a))) (car a))
              (else (assoc x (cdr a))))))
    (define string->list
      (lambda (s)
        (recur loop ((n (- (string-length s) 1)) (acc '()))
          (if (negative? n)
              acc
              (loop (- n 1) (cons (string-ref s n) acc))))))
;    (define list->string
;      (lambda (a)
;        (apply string a)))
    (define list->string
      (lambda (a)
        (letrec ([length
                  (lambda (a)
                    (recur loop ((a a) (len 0))
                      (if (null? a)
                          len
                          (loop (cdr a) (+ 1 len)))))])
          (let ((s (make-string (length a))))
            (recur loop ((i 0) (a a))
              (if (null? a)
                  s
                  (begin
                    (string-set! s i (car a))
                    (loop (+ 1 i) (cdr a)))))))))
    (define vector->list
      (lambda (v)
        (recur loop ((n (- (vector-length v) 1)) (acc '()))
          (if (negative? n)
              acc
              (loop (- n 1) (cons (vector-ref v n) acc))))))
;    (define list->vector
;      (lambda (a)
;        (apply vector a)))
    (define list->vector
      (lambda (a)
        (letrec ([length
                  (lambda (a)
                    (recur loop ((a a) (len 0))
                      (if (null? a)
                          len
                          (loop (cdr a) (+ 1 len)))))])
          (if (null? a)
              (vector)
              (let ((v (make-vector (length a) (car a))))
                (recur loop ((i 1) (a (cdr a)))
                  (if (null? a)
                      v
                      (begin
                        (vector-set! v i (car a))
                        (loop (+ 1 i) (cdr a))))))))))
    (define map
      (lambda (f a . args)
        (letrec ((map1 (lambda (f l)
                         (if (null? l)
                             '()
                             (cons (f (car l))
                                   (map1 f (cdr l))))))
                 (map2 (lambda (f l1 l2)
                         (cond ((null? l1)
                                '())
                               ((null? l2)
                                (error 'map "lists differ in length"))
                               (else
                                 (cons (f (car l1) (car l2))
                                       (map2 f (cdr l1) (cdr l2)))))))
                 (map* (lambda (f l*)
                         (if (null? (car l*))
                             '()
                             (cons (let ((l (map1 car l*)))
                                     (if (null? l)
                                         (f)
                                         (apply f l)))
                                   (map* f (map1 cdr l*)))))))
          (cond ((null? args)
                 (map1 f a))
                ((null? (cdr args))
                 (map2 f a (car args)))
                (else
                  (map* f (cons a args)))))))
    (define for-each
      (lambda (f a . args)
        (letrec ((map (lambda (f l)
                         (if (null? l)
                             '()
                             (cons (f (car l))
                                   (map f (cdr l)))))))
          (letrec ((for-each1 (lambda (f l)
                                (if (null? l)
                                    (void)
                                    (begin
                                      (f (car l))
                                      (for-each1 f (cdr l))))))
                   (for-each2 (lambda (f l1 l2)
                                (cond ((null? l1)
                                       (void))
                                      ((null? l2)
                                       (error 'for-each "lists differ in length"))
                                      (else
                                        (f (car l1) (car l2))
                                        (for-each2 f (cdr l1) (cdr l2))))))
                   (for-each* (lambda (f l*)
                                (if (null? (car l*))
                                    (void)
                                    (begin
                                      (let ((l (map car l*)))
                                        (if (null? l)
                                            (f)
                                            (apply f l)))
                                      (for-each* f (map cdr l*)))))))
            (cond ((null? args)
                   (for-each1 f a))
                  ((null? (cdr args))
                   (for-each2 f a (car args)))
                  (else
                    (for-each* f (cons a args))))))))
    (define call-with-input-file
      (lambda (s f)
        (let* ((p (open-input-file s))
               (v (f p)))
          (close-input-port p)
          v)))
    (define call-with-output-file
      (lambda (s f)
        (let* ((p (open-output-file s))
               (v (f p)))
          (close-output-port p)
          v)))
    (define with-input-from-file
      (lambda (s f)
        ; no way to switch current input in R4RS Scheme
        (error 'with-input-from-file "not supported")
        (f)))
    (define with-output-to-file
      (lambda (s f)
        ; no way to switch current output in R4RS Scheme
        (error 'with-output-to-file "not supported")
        (f)))
    
    (define make-promise
      (lambda (thunk)
        (let ([first #t]
              [val #f])
          (lambda ()
            (cond
             [(eq? first 'forcing)
              (error 'force "recursive force")]
             [(eq? first #t)
              (set! first 'forcing)
              (set! val (thunk))
              (set! first #f)
              val]
             [else val])))))
    (define force (lambda (promise) (promise)))
    (define make-list
      (lambda (n val)
        (recur loop ((n n))
          (if (< n 1)
              '()
              (cons val (loop (- n 1)))))))
    (define andmap
      (lambda (f list0 . lists)
        (if (null? list0)
            (and)
            (recur loop ((lists (cons list0 lists)))
              (if (null? (cdr (car lists)))
                  (apply f (map car lists))
                  (and (apply f (map car lists))
                       (loop (map cdr lists))))))))
    (define ormap
      (lambda (f list0 . lists)
        (if (null? list0)
            (or)
            (recur loop ((lists (cons list0 lists)))
              (if (null? (cdr (car lists)))
                  (apply f (map car lists))
                  (or (apply f (map car lists))
                      (loop (map cdr lists))))))))
    (define dynamic-wind
      (lambda (in doit out)
        (let* ([a (in)]
               [b (doit)]
               [c (out)])
          b)))
    (define not (lambda (x) (if x #f #t)))
    (define add1 (lambda (x) (+ x 1)))
    (define sub1 (lambda (x) (- x 1)))
    (define equal? 
      (lambda (x y)
        (or (eqv? x y)
            (and (pair? x) (pair? y) 
                 (equal? (car x) (car y))
                 (equal? (cdr x) (cdr y)))
            (and (vector? x) (vector? y)
                 (= (vector-length x) (vector-length y))
                 (recur loop ([i (vector-length x)])
                   (or (zero? i)
                       (let ([i (- i 1)])
                         (and (equal? (vector-ref x i) (vector-ref y i))
                              (loop i)))))))))
    (define negative? (lambda (x) (< x 0)))
    (define list? (lambda (x) (or (null? x)
                                  (and (pair? x) (list? (cdr x))))))
    (define reverse!
      (lambda (x)
        (recur loop ([x x][p '()])
          (let ([q (cdr x)])
            (set-cdr! x p)
            (if (null? q)
                x
                (loop q x))))))
  (define even? (lambda (x) (or (zero? x) (odd? (sub1 x)))))
  (define odd? (lambda (x) (even? (sub1 x))))
))

(define real-primitives
  '(car cdr zero? null? nil null + - * / apply
        number? cons pair? 
        < > <= >= = <>
        vector make-vector vector-length vector-ref vector-set!
        void error display newline remainder quotient modulo abs expt min 
        real-part even sqrt cos integer? exact?
        eq? eqv?
        call-with-values 
        boolean? procedure? symbol? gensym
        set-cdr! set-car! printf random 
        string-ref symbol->string number->string symbol? char=? 
        char? string? vector?
     #%eqv #%void 
     string->symbol string-append string<? #%global-defined-value
))

;; ----------------------------------------------------------------------

(define (do-topo-files files)
  (let* ([file-thunk* (files->file-thunk* files)]
         [defs1 (load-parse-expand file-thunk*)]
         [_ (set! defs-expanded defs1)]
         [defs2 (map bind-def defs1)]
         [_ (set! defs-bind defs2)]
         [def (topological-sort defs2)])
    (zodiac:expr-stripper def)))

(define (test-topo files out)
  (when (file-exists? out) (delete-file out))
  (let* ([file-thunk* (files->file-thunk* files)]
         [defs1 (load-parse-expand file-thunk*)]
         [_ (set! defs-expanded defs1)]
         [defs2 (map bind-def defs1)]
         [_ (set! defs-bind defs2)]
         [def (topological-sort defs2)])
    (pretty-print (zodiac:expr-stripper def))))

;; ----------------------------------------------------------------------
