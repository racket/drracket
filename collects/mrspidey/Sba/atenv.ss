;; atenv.ss
;; Section for handling environment
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

(define-const-typed-structure
  atenv ( (: immut     (listof (cons zodiac:binding FlowType)))
          (: notcap    (listof (cons zodiac:binding mutable-binding)))
          (: cap       (listof (cons zodiac:binding mutable-binding)))
          (: flushed   (listof (cons zodiac:binding mutable-binding)))
          (: unflushed (listof (cons zodiac:binding mutable-binding)))
          (: both      (listof (cons zodiac:binding mutable-binding)))))

(define-const-typed-structure 
  mutable-binding ( (: current FlowType)
                    (: at-transfer Tvar)
                    (: flushed bool)))

;; ----------------------------------------------------------------------

(define atenv:empty (make-atenv '() '() '() '() '() '()))

(define (atenv:extend-mutated env name ftype tvar)
  (pretty-debug-atenv `(atenv:extend-mutated ,(atenv->pretty env)))
  (assert (zodiac:binding? name) 'atenv:extend name)
  (let ([ftype (link-parsed-ftype! name ftype)])
    (match env
      [($ atenv i n c f u b)
        (make-atenv 
          i
          (extend-env n name 
            (make-mutable-binding 
              ftype tvar
              #f))
          c f u b)])))

(define (atenv:extend env name ftype)
  (pretty-debug-atenv `(atenv:extend ,(atenv->pretty env)))
  (assert (zodiac:binding? name) 'atenv:extend name)
  (let ([ftype (link-parsed-ftype! name ftype)])
    (match env
      [($ atenv i n c f u b)
        (if (zodiac:binding-mutated name)
          (make-atenv 
            i
            (extend-env n name 
              (make-mutable-binding 
                ftype 
                (mk-Tvar 'mut-var
                  ;;(symbol-append 'mut-var- (zodiac:binding-var name))
                  )
                #f))
            c f u b)
          (make-atenv
            (extend-env i name ftype)
            n c f u b))])))

(define (atenv:extend* env names ftypes)
  (foldr2 (lambda (name ftype env) (atenv:extend env name ftype))
          env names ftypes))

(define (atenv:extend-voids env names)
  (atenv:extend* env names (map (lambda (x) (mk-tvar-void)) names)))

(define (atenv:extend-undefineds env names)
  (atenv:extend* env names (map (lambda (x) (mk-tvar-undefined)) names)))

(define (atenv:lookup env name)
  (pretty-debug-atenv `(atenv:lookup ,(atenv->pretty env)))
  (if (zodiac:binding-mutated name)
    (match (or 
             (lookup-or-#f (atenv-notcap env)    name)
             (lookup-or-#f (atenv-cap env)       name)
             (lookup-or-#f (atenv-flushed env)   name)
             (lookup-or-#f (atenv-unflushed env) name)
             (lookup-or-#f (atenv-both env)      name))
      [($ mutable-binding cur) cur]
      [x x])
    (lookup-or-#f (atenv-immut env) name)))

(define (atenv:change-binding env name ftype)
  (pretty-debug-atenv
    `(->atenv:change-binding
       ,(zodiac:binding-var name) ,(FlowType->pretty ftype)
       ,(atenv->pretty env)))
  (match env
    [($ atenv i n c f u b)
      (let/cc k
        (let* 
          ([err
             (lambda ()
               (pretty-debug-atenv
                 `(atenv:change-binding
                    ,(zodiac:binding-var name)
                    ,(map zodiac:binding-var (map car env))))
               (mrspidey:warning
                 (format "Unbound variable ~s" (zodiac:binding-var name))
                 (zodiac:zodiac-start name)
                 2)
               (k env))]
            [result-env
              (if (zodiac:binding-mutated name)
                (let ([chg-fn
                        (match-lambda
                          [($ mutable-binding cur at-transfer)
                            (make-mutable-binding ftype at-transfer #f)])])
                  (if (lookup-or-#f n name)
                    (make-atenv
                      i
                      (env:change-binding n name chg-fn err)
                      c f u b)
                    ;; Must be in one of c f u b
                    ;; Lift out to captured
                    (if (lookup-or-#f c name)
                      (make-atenv
                        i n
                        (env:change-binding c name chg-fn err)
                        f u b)
                      (let*-vals
                        ([(bind f u b)
                           (cond
                             [(lookup-or-#f f name)
                               (let*-vals ([(bind f) (env:remove f name)])
                                 (values bind f u b))]
                             [(lookup-or-#f u name)
                               (let*-vals ([(bind u) (env:remove u name)])
                                 (values bind f u b))]
                             [(lookup-or-#f b name)
                               (let*-vals ([(bind b) (env:remove b name)])
                                 (values bind f u b))]
                             [else (err)])])
                        (make-atenv
                          i n
                          (extend-env c name (chg-fn bind))
                          f u b)))))
                (make-atenv 
                  (env:change-binding i name (lambda (old) ftype) err)
                  n c f u b))])
          (pretty-debug-atenv
            `(->atenv:change-binding-returns ,(atenv->pretty result-env)))
          result-env))]))

(define (atenv:change-bindings env bindings ftypes)
  (foldr2
   (lambda (binding ftype env)
     (atenv:change-binding env binding ftype))
   env bindings ftypes))

;; ------------------------------

(define (atenv:capture-locs env bindings)
  (pretty-debug-atenv 
    `(atenv:capture-locs ,(atenv->pretty env) ,(map zodiac:binding-var bindings)))
  (match env
    [($ atenv i n c f u b)
      (recur loop ([n n][n-ok '()][c c])
        (if (null? n)
          (make-atenv i n-ok c f u b)
          (if (and 
                (memq (caar n) bindings)
                ;; ### KLUDGE FOR DECENT, BUT POSSIBLY WRONG, POLYMORPHISM
                (not (poly-atype? (FlowType->Atype 
                                    (mutable-binding-current (cdar n))))))
            (loop (cdr n) n-ok (cons (car n) c))
            (loop (cdr n) (cons (car n) n-ok) c))))]))

(define (atenv:unflush env)
  (pretty-debug-atenv `(atenv:unflush ,(atenv->pretty env)))
  (match env
    [($ atenv i n c f u b)
      (let ([upd
              (match-lambda
                [(name . ($ mutable-binding cur trans))
                  (cons name (make-mutable-binding trans trans #t))])])
        (make-atenv
          i n '() '()
          (append (map upd c) u)
          (append (map upd f) b)))]))

(define (atenv:flush! env)
  (pretty-debug-atenv `(atenv:flush! ,(atenv->pretty env)))
  (match env
    [($ atenv i n c f u b)
      (let ([upd
              (match-lambda
                [(name . (and bind ($ mutable-binding cur trans flushed)))
                  (unless flushed
                    (new-edge! (FlowType->Tvar cur) trans)
                    (set-mutable-binding-flushed! bind #t))])])
        (for-each upd c)
        (for-each upd u)
        (make-atenv i n '() (append c f) '() (append u b)))]))

(define (atenv->pretty env)
  (match env
    [($ atenv i n c f u b)
      (let ([p (match-lambda
                 [(name . ($ mutable-binding cur trans))
                   (list (zodiac:binding-var name) 
                     (list
                       (FlowType->pretty cur)
                       (FlowType->pretty trans)))])])
              
        (list
          (map (match-lambda
                 [(name . ftype)
                   (list 
                     (zodiac:binding-var name) 
                     (FlowType->pretty ftype))])
            i)
          (map p n) 
          (map p c)
          (map p f)
          (map p u)
          (map p b)))]))

(define (atenv:domain env)
  (match env
    [($ atenv i n c f u b)
      (map car (append i n c f u b))]))

(define (atenv:ok? e)
  (and
    (atenv? e)
    (match e
      [($ atenv i n c f u b)
        (and
          (list? i)
          (andmap
            (match-lambda
              [(($ zodiac:binding) . ($ FlowType)) #t]
              [_ #f])
            i)
          (andmap
            (lambda (n)
              (and
                (list? n)
                (andmap
                  (match-lambda
                    [(($ zodiac:binding) . ($ mutable-binding)) #t]
                    [_ #f])
                  n)))
            (list n c f u b)))])))

'(defmacro check-atenv-fn-ok (fn)
  (let ( [old-fn (gensym)])
    `(begin
       (define ,old-fn ,fn)
       (define ,fn
         (let ([env-ok atenv:ok?])
           (lambda (env . rest)
             ;;(printf "Entering ~s~n" (quote ,fn))
             (unless (env-ok env)
               (pretty-print env)
               (error (quote ,fn) "Bad env on entry"))
             (let ([r (apply ,old-fn env rest)])
               ;;(printf "exiting ~s~n" (quote ,fn))
               (unless (env-ok env)
                 (error (quote ,fn) "Bad env on exit"))
               r)))))))

;(check-atenv-fn-ok atenv:extend)
;(check-atenv-fn-ok atenv:extend*)
;(check-atenv-fn-ok atenv:extend-voids)
;(check-atenv-fn-ok atenv:change-binding)
;(check-atenv-fn-ok atenv:change-bindings)
;(check-atenv-fn-ok atenv:capture-locs)
;(check-atenv-fn-ok atenv:unflush)
;(check-atenv-fn-ok atenv:flush!)

;; ======================================================================

(define (link-parsed-ftype! parsed ftype)
  (assert (FlowType? ftype) 'link-parsed-ftype! ftype)
  (if need-label-types
    (let ([nu-ftype
            (if (and need-explanation (FlowType-expr ftype))
              (copy-ftype ftype)
              ftype)])
      (pretty-debug-object 
        `(link-parsed-ftype! 
           ,(zodiac:stripper parsed)
           ,(zodiac:location-offset (zodiac:zodiac-start parsed))
           ,(FlowType-name ftype)))
      (zodiac:set-parsed-ftype! parsed nu-ftype)
      (set-FlowType-expr! nu-ftype #t) ;parsed
      nu-ftype)
    ftype))

;; ======================================================================



