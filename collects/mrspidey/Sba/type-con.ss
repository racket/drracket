;; type-con.ss
;; 
;; Parses a type to produce a set of constraints
;; Also tests an AVS for membership in a type
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
;; ----------------------------------------------------------------------

(define typevar?
  (lambda (v)
    (and (symbol? v)
         (not (type-constructor? v))
         (not (memq v '(_ union rec ->*))))))

(define (compat-type-once type cl-fn)
  (match type
    [(and c (or (? number?) (? char?) ('quote (? symbol?)))) c]
    [('rec bind t2)
     `(rec ,(map (match-lambda [(a t) (list a (cl-fn t))]) bind) 
           ,(cl-fn t2))]
    [('forall vars type)
     `(forall ,vars ,(cl-fn type))]
    [('case-> . (and types (? list?) (_ . _))) 
     `(case-> ,@(map cl-fn types))]
    [('union . (? list? union)) `(union ,@(map cl-fn union))]
    [(? symbol? a) a]
    [(arg '->* res) (list (cl-fn arg) '->* (cl-fn res))]
    ;; check constructors
    [((? type-constructor? C) . args)
     `(,C ,@(map (match-lambda
                  [('! arg) (list '! (cl-fn arg))]
                  [arg (cl-fn arg)])
                 args))]
    [_ #f]))

(define (expand-input-type type)
  ;; Do rewriting transformations
  ;;(pretty-print `(expand-type ,type))
  (let ([t2 (input-type-expander type)])
    (if (eq? type t2)
        (or (compat-type-once type expand-input-type)
            (mrspidey:error 
             (format "Bad input type ~s" type)))
        (expand-input-type t2))))

(define (expand-output-type type)
  ;; Do rewriting transformations
  ;;(pretty-print `(expand-type ,type))
  (let* ([type (or (compat-type-once type expand-output-type)
                   type
                   (mrspidey:error (format "Bad output type ~s" type)))])
    (recur fn ([type type])
      (let ([t2 (output-type-expander type)])
        (if (eq? type t2)
            type 
            (fn t2))))))

;; ----------------------------------------------------------------------

(define (con-exp->templates con-exp)
  ;; Assumes con-exp already expanded
  (match con-exp
    [('union . E*)
     (let ([T* (map con-exp->templates E*)])
       (and (andmap (lambda (x) x) T*)
            (apply append T*)))]
    [('forall _ type) (con-exp->templates type)]
    [(_ '->* _) (list template-lam)]
    [(or (C . _) C)
     (if (type-constructor? C)
         (list (lookup-template C))
         #f)]))

;; ----------------------------------------------------------------------

(define (generic-type->con type prefix forall-env sign)
  ;; Assumes type->con is already expanded
  (letrec
      ([mk-AVS-tmp (lambda () (mk-AVS (symbol-append prefix ':tc)))]
       [parse-type-generic
        (lambda (t env sign)
          ;; sign is #t for +ive, #f for -ive
          (let ([new-sign-edge!
                 (lambda (from to)
                   (if sign
                       (new-edge! from to)
                       (new-edge! to from)))]
                [parse-type 
                 (lambda (t) 
                   (parse-type-generic t env sign))])
            (match t

              ;; Recursive definitions
              [('rec bind t2)
               (let* ([a* (map car bind)]
                      [AVS* (map 
                             (lambda (a) 
                               (mk-AVS (symbol-append prefix ': a)))
                             a*)]
                      [env  (extend-env* env a* AVS*)])
                 (for-each
                  (match-lambda
                   [(a t)
                    (new-sign-edge! (parse-type-generic t env sign)
                                    (lookup env a))])
                  bind)
                 (parse-type-generic t2 env sign))]

              ;; Unions
              [('union . (? list? union))
               (let ([AVS (mk-AVS-tmp)])
                 (for-each
                  (lambda (t)
                    (new-sign-edge! (parse-type-generic t env sign)
                                    AVS))
                  union)
                 AVS)]

              ;; case->
              [('case-> . (? list? types)) 
               (parse-type-generic (rac types) env sign)]

              ;; type variables
              ['_ (mk-AVS-tmp)]
              [(? symbol? a)
               (lookup-or-fail 
                env a
                (lambda () 
                  (lookup-or-fail
                   forall-env a
                   (lambda ()
                     ;; reference type named a
                     ;; if sign +ive, use this type
                     ;; if sign -ive, return dummy AVS
                     (let ([AVS (mk-AVS (symbol-append prefix ': a))])
                       (set! global-in-type-env 
                           (cons (list a AVS (box #f)) global-in-type-env))
                       (if sign
                           AVS
                           (mk-AVS (symbol-append prefix ': a))))))))]

              [(arg '->* res)
               (let ([AVS (mk-AVS-tmp)])
                 (if sign
                     ;; make the AV
                     (new-AV! AVS
                              (make-AV-lam 
                               (parse-type-generic arg env #f)
                               (parse-type-generic res env #t)
                               0 #t))
                     ;; extract the various components
                     (begin
                       (new-con! AVS 
                                 (create-con 
                                  template-lam 0 
                                  (parse-type-generic arg env #t)))
                       (new-con! AVS 
                                 (create-con
                                  template-lam 1 
                                  (parse-type-generic res env #f)))))
                 AVS)]

              ;; constants
              [(or (? number? c) (? char? c) ('quote (? symbol? c)))
               (let ([AVS (mk-AVS-tmp)])
                 (if sign (new-AV! AVS (traverse-const c)))
                 AVS)]

              ;; check constructors
              [((? type-constructor? C) . args)
               (match-let*
                   ([(and template ($ template type signs ref assign))
                     (lookup-template C)]
                    [AVS (mk-AVS-tmp)])
                 (unless (= (vector-length ref) (length args))
                   (mrspidey:error
                    (format "Constructor ~s given ~s arg(s), expected ~s" 
                            C (length args) (vector-length ref))))
                 (if sign
                     ;; make the AV, unless void and (st:see-void) is false
                     (unless (and (eq? C 'void) (not (st:see-void)))
                       (new-AV! 
                        AVS
                        (apply
                         make-constructed-AV C 
                         (map 
                          (lambda (a)
                            (parse-type-generic a env sign)) 
                          args)
                         ;; DO SOMETHING FOR NULL ARGS
                         )))

                     ;; extract the various components
                     ;; need to do unification on mutable fields,
                     ;; unless pattern matching arg is (! a)
                     (for-each-with-n
                      (lambda (arg n)
                        (match arg
                          ['_ (void)]
                          [('! a)
                           (unless (vector-ref assign n)
                             (mrspidey:error
                              "! used on immutable field in type"))
                           ;; Inject only
                           (new-con!
                            AVS
                            (create-con template (vector-ref assign n)
                                        (parse-type-generic a env #t)))]
                          [arg
                           ;; Extract 
                           (new-con!
                            AVS
                            (create-con template (vector-ref ref n)
                                        (parse-type-generic arg env #f)))]))
                      args))
                 AVS)]
              [_
               ;; Didn't match any type
               (mrspidey:error (format "invalid type syntax ~s" t))])))])

    (let ([AVS (parse-type-generic type empty-env sign)])
      AVS)))

;; ----------------------------------------------------------------------

(define (type->con type AVS prefix top-misc)
  ;; Assumes type is already expanded
  (new-edge! (generic-type->con type prefix empty-env #t) AVS)

  ;; Put top-misc in misc field of each AV
  (for-each
   (lambda (AV) (set-AV-misc! AV top-misc))
   (get-AVS-objs AVS)))

;; ----------------------------------------------------------------------

(define (split-schema type)
  (match type
    [('forall vars type)
     (list
      (map (lambda (v) (cons v (mk-AVS-tmp v))) vars)
      type)]
    [type (list '() type)]))

(define (type-schema->con type AVS prefix top-misc)
  ;; Assumes type is already expanded
  (match-let ([(forall-env type) (split-schema type)])
    (new-edge! (generic-type->con type prefix forall-env #t) AVS)

    ;; Put top-misc in misc field of each AV
    (for-each
     (lambda (AV) (set-AV-misc! AV top-misc))
     (get-AVS-objs AVS))))

;; ----------------------------------------------------------------------

(define (type-schema->con-for-nargs type prefix n)
  ;; Assumes type is already expanded
  ;; Puts top-misc in misc field of top AV
  ;; returns ((AVS-para ...) . AVS-result) if possible
  ;; otherwise returns #f

  (match-let ([(forall-env type) (split-schema type)])
    (match (dom+rng-for-nargs type n)
      [#f #f]
      [(forall-env para* rng)
       (recur loop ([para* para*]
                    [AVS-para* '()])
         (match para*
           [()
            (cons (reverse AVS-para*) 
                  (generic-type->con rng prefix forall-env #t))]
           [(para . rest)
            (let* ([AVS-para (generic-type->con para prefix forall-env #f)])
              (loop rest (cons AVS-para AVS-para*)))]))])))

;; ----------------------------------------------------------------------

(define (dom+rng-for-nargs type nargs)
  ;; extracts appropriate type from top-level case->.
  ;; returns (forall-env (para ...) result) if matching case
  ;; o/w returns #f

  (let* ([try1 
          (lambda (type forall)
            ;; See if type has the right # args
            (match type
              [(dom '->* rng)
               (recur loop ([dom dom][para* '()][n nargs])
                 (match dom
                   [('nil) 
                    (if (zero? n)
                        (list (map (lambda (v) (cons v (mk-AVS-tmp 'dr)))
                                   forall)
                              (reverse para*)
                              rng)
                        #f)]
                   [('cons para rest) (loop rest (cons para para*) (sub1 n))]
                   [_ #f]))])
            [_ #f])]
         [try2
          (lambda (type forall)
            (match type
              [('case-> . types)
               (ormap (lambda (t) (try1 t forall)) types)]
              [type (try1 type forall)]))])
    (match type
      [('forall vars type) (try2 type vars)]
      [type (try2 type '())])))

;; ----------------------------------------------------------------------

(define AVS-in-type?
  (lambda (AVS type)
    ;; Checks if AVS is contained in the type
    ;; Assumes type is already expanded
    
    (letrec
        ([visit-alloc (lambda () (make-vector num-AV #f))]
         [add-visit! 
          (lambda (v AV)
            (vector-set! v (AV-num AV) #t))]
         [visited?
          (lambda (v AV) 
            (vector-ref v (AV-num AV)))]
         [AVS-in-type?
          (lambda (AVS type env)
            ;; env maps type vars to AV-membership test function
            (andmap (lambda (AV) (AV-in-type? AV type env))
                    (get-AVS-objs AVS)))]
         [AV-in-type?
          (lambda (AV type env)
            ;; env maps type vars to AV-membership test function
            (match type

              ;; Recursive definitions
              [('rec bind t2)
               (let* 
                   ([a* (map car bind)]
                    [t* (map cadr bind)]
                    [envbox (box #f)]
                    [nuenv
                     (extend-env*
                      env a*
                      (map (lambda (t)
                             (let ([v (visit-alloc)])
                               (lambda (AV)
                                 (or (visited? v AV)
                                     (begin
                                       (add-visit! v AV)
                                       (AV-in-type? AV t (unbox envbox)))))))
                           t*))])
                 (set-box! envbox nuenv)
                 (AV-in-type? AV t2 nuenv))]

              [('case-> . types) (AV-in-type? AV (rac types) env)]

              [('union . (? list? union))
               (ormap (lambda (type) (AV-in-type? AV type env))
                      union)]
              ['_ #t]
              [(? symbol? a) 
               ((lookup-or-fail env a (lambda () (lambda (AV) #t)))
                AV)]
              [(arg '->* res) (eq? (AV-template AV) template-lam)]

              [(? number? n)
               (and (eq? (template-type (AV-template AV)) 'num)
                    (= (AV-misc AV) n))]
              [(? char? c)
               (and (eq? (template-type (AV-template AV)) 'char)
                    (char=? (AV-misc AV) c))]
              [('quote (? symbol? s))
               (and (eq? (template-type (AV-template AV)) 'sym)
                    (eq? (AV-misc AV) s))]

              [((? type-constructor? C) . args)
               (match-let 
                   ([(and template ($ template type signs ref assign))
                     (lookup-template C)])
                 (cond
                  [(eq? (AV-template AV) template)
                   (recur loop ([args args][n 0])
                     (if (null? args)
                         #t
                         (and 
                          (AVS-in-type? 
                           (vector-ref (AV-fields AV) (vector-ref ref n))
                           (match args
                             [(('! a) . _) a]
                             [(a . _) a])
                           env)
                          (loop (cdr args) (add1 n)))))]
                  [(memq (AV-template AV) (template-sub-templates template))
                   #t]
                  [else #f]))]
              [_ 
               ;; Didn't match any type
               (mrspidey:error (format "Invalid type syntax ~s" type))]))])
      (AVS-in-type? AVS type empty-env))))

;(trace AVS-in-type?)



