;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Matching Syntactic Extensions for Scheme
;;
;; Specialized for MzScheme; works with define-struct
;;
;; Report bugs to wright@research.nj.nec.com.  The most recent version of
;; this software can be obtained by anonymous FTP from ftp.nj.nec.com
;; in file pub/wright/match.tar.Z.  Be sure to set "type binary" when
;; transferring this file.
;;
;; Written by Andrew K. Wright, 1993 (wright@research.nj.nec.com).
;; Adapted from code originally written by Bruce F. Duba, 1991.
;; This package also includes a modified version of Kent Dybvig's
;; define-structure (see Dybvig, R.K., The Scheme Programming Language,
;; Prentice-Hall, NJ, 1987).
;;
;; This software is in the public domain.  Feel free to copy,
;; distribute, and modify this software as desired.  No warranties
;; nor guarantees of any kind apply.  Please return any improvements
;; or bug fixes to wright@research.nj.nec.com so that they may be included
;; in future releases.
;;
;; This macro package extends Scheme with several new expression forms.
;; Following is a brief summary of the new forms.  See the associated
;; LaTeX documentation for a full description of their functionality.
;;
;;
;;         match expressions:
;;
;; exp ::= ...
;;       | (match exp clause ...)
;;       | (match-lambda clause ...)
;;       | (match-lambda* clause ...)
;;       | (match-let ((pat exp) ...) body)
;;       | (match-let* ((pat exp) ...) body)
;;       | (match-letrec ((pat exp) ...) body)
;;       | (match-define> pat exp)
;;
;; clause ::= (pat body) | (pat => exp)
;;
;;         patterns:                       matches:
;;
;; pat ::= identifier                      anything, and binds identifier
;;       | _                               anything
;;       | ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | 'sexp                           an s-expression
;;       | 'symbol                         a symbol (special case of s-expr)
;;       | (pat_1 ... pat_n)               list of n elements
;;       | (pat_1 ... pat_n . pat_{n+1})   list of n or more
;;       | (pat_1 ... pat_n pat_n+1 ooo)   list of n or more, each element
;;                                           of remainder must match pat_n+1
;;       | #(pat_1 ... pat_n)              vector of n elements
;;       | #(pat_1 ... pat_n pat_n+1 ooo)  vector of n or more, each element
;;                                           of remainder must match pat_n+1
;;       | #&pat                           box
;;       | ($ struct-name pat_1 ... pat_n) a structure
;;       | (and pat_1 ... pat_n)           if all of pat_1 thru pat_n match
;;       | (or pat_1 ... pat_n)            if any of pat_1 thru pat_n match
;;       | (not pat_1 ... pat_n)           if all pat_1 thru pat_n don't match
;;       | (? predicate pat_1 ... pat_n)   if predicate true and all of
;;                                           pat_1 thru pat_n match
;;       | (set! identifier)               anything, and binds setter
;;       | (get! identifier)               anything, and binds getter
;;       | `qp                             a quasi-pattern
;;
;; ooo ::= ...                             zero or more
;;       | ___                             zero or more
;;       | ..k                             k or more
;;       | __k                             k or more
;;
;;         quasi-patterns:                 matches:
;;
;; qp  ::= ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | identifier                      a symbol
;;       | (qp_1 ... qp_n)                 list of n elements
;;       | (qp_1 ... qp_n . qp_{n+1})      list of n or more
;;       | (qp_1 ... qp_n qp_n+1 ooo)      list of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #(qp_1 ... qp_n)                vector of n elements
;;       | #(qp_1 ... qp_n qp_n+1 ooo)     vector of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #&qp                            box
;;       | ,pat                            a pattern
;;       | ,@pat                           a pattern
;;
;; The names (quote, quasiquote, unquote, unquote-splicing, ?, _, $,
;; and, or, not, set!, get!, ..., ___) cannot be used as pattern variables.
;;
;;
;;         structure expressions:
;;
;; exp ::= ...
;;       | (define-structure (id_0 id_1 ... id_n))
;;       | (define-structure (id_0 id_1 ... id_n)
;;                           ((id_{n+1} exp_1) ... (id_{n+m} exp_m)))
;;       | (define-const-structure (id_0 arg_1 ... arg_n))
;;       | (define-const-structure (id_0 arg_1 ... arg_n)
;;                                 ((arg_{n+1} exp_1) ... (arg_{n+m} exp_m)))
;;
;; arg ::= id | (! id)
;;
;;
;; match:error-control controls what code is generated for failed matches.
;; Possible values:
;;  'unspecified - do nothing, ie., evaluate (cond [#f #f])
;;  'fail - call match:error, or die at car or cdr
;;  'error - call match:error with the unmatched value
;;  'match - call match:error with the unmatched value _and_
;;             the quoted match expression
;; match:error-control is set by calling match:set-error-control with
;; the new value.
;; Added by Matthew:
;;  match:error-control-param is a system parameter for this value
;;
;; match:error is called for a failed match.
;; match:error is set by calling match:set-error with the new value.
;;
;; match:structure-control controls the uniqueness of structures
;; (does not exist for Scheme 48 version).
;; Possible values:
;;  'vector - (default) structures are vectors with a symbol in position 0
;;  'disjoint - structures are fully disjoint from all other values
;; match:structure-control is set by calling match:set-structure-control
;; with the new value.
;;
;; match:runtime-structures controls whether local structure declarations
;; generate new structures each time they are reached
;; (does not exist for Scheme 48 version).
;; Possible values:
;;  #t - (default) each runtime occurrence generates a new structure
;;  #f - each lexical occurrence generates a new structure
;;
;; End of user visible/modifiable stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unit
(import)
(export match:set-error match:set-error-control match:error-control-param
match:error match match-lambda match-lambda* 
match-letrec match-let match-let*)
		   
(define match:version "Version 1.10mz, Feb 5, 1996")

(define-struct (exn:misc:match struct:exn:misc) (value))

(define match:error
(case-lambda
[(val) (raise
(make-exn:misc:match 
(format "match: no matching clause for ~s" val)
(current-continuation-marks)
val))]
[(val expr) (raise
(make-exn:misc:match 
(format "match: no matching clause for ~s: ~s" val expr)
(current-continuation-marks)
val))]))

(define match:syntax-err (lambda (obj msg) (error 'match
"~a ~e" msg obj))) (define match:set-error (lambda (v)
(set! match:error v))) (define match:error-control-param (case-lambda
[() match:error-control] [(v) (match:set-error-control v)])) (define
match:error-control 'error) (define match:set-error-control (lambda
(v) (if (memq v '(unspecified fail error match)) (set!
match:error-control v) (error 'match:set-error-control "invalid setting: ~s" v))))
(define match:disjoint-predicates (cons 'null
'(pair?  symbol?  boolean?  number?  string?  char?  procedure?
vector?  box?)))  (define match:vector-structures '()) (define
match:expanders (letrec ((genmatch (lambda (x clauses match-expr)
(let* ((length>= (gensym)) (eb-errf (error-maker match-expr)) (blist
(car eb-errf)) (plist (map (lambda (c) (let* ((x (bound
(validate-pattern (car c)))) (p (car x)) (bv (cadr x)) (bindings
(caddr x)) (code (gensym)) (fail (and (pair?  (cdr c)) (pair?  (cadr
c)) (eq? (caadr c) '=>) (symbol?  (cadadr c)) (pair?  (cdadr c))
(null?  (cddadr c)) (pair?  (cddr c)) (cadadr c))) (bv2 (if fail (cons
fail bv) bv)) (body (if fail (cddr c) (cdr c)))) (set! blist (cons
`(,code (lambda ,bv2 ,@body)) (append bindings blist))) (list p code
bv (and fail (gensym)) #f))) clauses)) (code (gen x '() plist (cdr
eb-errf) length>= (gensym)))) (unreachable plist match-expr)
(inline-let `(let ((,length>= (lambda (n) (lambda (l) (>= (length l)
n)))) ,@blist) ,code))))) (genletrec (lambda (pat exp body match-expr)
(let* ((length>= (gensym)) (eb-errf (error-maker match-expr)) (x
(bound (validate-pattern pat))) (p (car x)) (bv (cadr x)) (bindings
(caddr x)) (code (gensym)) (plist (list (list p code bv #f #f))) (x
(gensym)) (m (gen x '() plist (cdr eb-errf) length>= (gensym))) (gs
(map (lambda (_) (gensym)) bv))) (unreachable plist match-expr)
`(letrec ((,length>= (lambda (n) (lambda (l) (>= (length l) n))))
,@(map (lambda (v) `(,v #f)) bv) (,x ,exp) (,code (lambda ,gs ,@(map
(lambda (v g) `(set! ,v ,g)) bv gs) ,@body)) ,@bindings ,@(car
eb-errf)) ,m)))) (gendefine (lambda (pat exp match-expr) (let*
((length>= (gensym)) (eb-errf (error-maker match-expr)) (x (bound
(validate-pattern pat))) (p (car x)) (bv (cadr x)) (bindings (caddr
x)) (code (gensym)) (plist (list (list p code bv #f #f))) (x (gensym))
(m (gen x '() plist (cdr eb-errf) length>= (gensym))) (gs (map (lambda
(_) (gensym)) bv))) (unreachable plist match-expr) `(begin ,@(map
(lambda (v) `(define ,v #f)) bv) ,(inline-let `(let ((,length>=
(lambda (n) (lambda (l) (>= (length l) n)))) (,x ,exp) (,code (lambda
,gs ,@(map (lambda (v g) `(set! ,v ,g)) bv gs) (cond (#f #f))))
,@bindings ,@(car eb-errf)) ,m)))))) (pattern-var? (lambda (x) (and
(symbol? x) (not (dot-dot-k? x)) (not (memq x '(quasiquote quote
unquote unquote-splicing ?  _ $ and or not set!  get!  ...  ___))))))
(dot-dot-k? (lambda (s) (and (symbol? s) (if (memq s '(... ___)) 0
(let* ((s (symbol->string s)) (n (string-length s))) (and (<= 3 n)
(memq (string-ref s 0) '(#\. #\_)) (memq (string-ref s 1) '(#\. #\_))
(andmap char-numeric?  (string->list (substring s 2 n)))
(string->number (substring s 2 n)))))))) (error-maker (lambda
(match-expr) (cond ((eq? match:error-control 'unspecified) (cons '()
(lambda (x) `(cond (#f #f))))) ((memq match:error-control '(error
fail)) (cons '() (lambda (x) `((#%global-defined-value 'match:error)
,x)))) ((eq? match:error-control 'match) (let ((errf (gensym)) (arg
(gensym))) (cons `((,errf (lambda (,arg) ((#%global-defined-value
'match:error) ,arg ',match-expr)))) (lambda (x) `(,errf ,x))))) (else
(match:syntax-err '(unspecified error fail match) "invalid value for match:error-control, legal values are"))))) (unreachable (lambda
(plist match-expr) (for-each (lambda (x) (if (not (car (cddddr x)))
(begin (display "Warning: unreachable pattern ") (display (car x))
(display " in ") (display match-expr) (newline)))) plist)))
(validate-pattern (lambda (pattern) (letrec ((simple? (lambda (x) (or
(string? x) (boolean? x) (char? x) (number? x) (null? x)))) (ordinary
(lambda (p) (let ((g204 (lambda (x y) (cons (ordinary x) (ordinary
y))))) (if (simple? p) ((lambda (p) p) p) (if (equal?  p '_) ((lambda
() '_)) (if (pattern-var?  p) ((lambda (p) p) p) (if (pair?  p) (if
(equal?  (car p) 'quasiquote) (if (and (pair?  (cdr p)) (null?  (cddr
p))) ((lambda (p) (quasi p)) (cadr p)) (g204 (car p) (cdr p))) (if
(equal?  (car p) 'quote) (if (and (pair?  (cdr p)) (null?  (cddr p)))
((lambda (p) p) p) (g204 (car p) (cdr p))) (if (equal?  (car p) '?)
(if (and (pair?  (cdr p)) (list?  (cddr p))) ((lambda (pred ps) `(?
,pred ,@(map ordinary ps))) (cadr p) (cddr p)) (g204 (car p) (cdr p)))
(if (equal?  (car p) 'and) (if (and (list?  (cdr p)) (pair?  (cdr p)))
((lambda (ps) `(and ,@(map ordinary ps))) (cdr p)) (g204 (car p) (cdr
p))) (if (equal?  (car p) 'or) (if (and (list?  (cdr p)) (pair?  (cdr
p))) ((lambda (ps) `(or ,@(map ordinary ps))) (cdr p)) (g204 (car p)
(cdr p))) (if (equal?  (car p) 'not) (if (and (list?  (cdr p)) (pair?
(cdr p))) ((lambda (ps) `(not ,@(map ordinary ps))) (cdr p)) (g204
(car p) (cdr p))) (if (equal?  (car p) '$) (if (and (pair?  (cdr p))
(symbol?  (cadr p)) (list?  (cddr p))) ((lambda (r ps) `($ ,r ,@(map
ordinary ps))) (cadr p) (cddr p)) (g204 (car p) (cdr p))) (if (equal?
(car p) 'set!)  (if (and (pair?  (cdr p)) (pattern-var?  (cadr p))
(null?  (cddr p))) ((lambda (p) p) p) (g204 (car p) (cdr p))) (if
(equal?  (car p) 'get!)  (if (and (pair?  (cdr p)) (pattern-var?
(cadr p)) (null?  (cddr p))) ((lambda (p) p) p) (g204 (car p) (cdr
p))) (if (equal?  (car p) 'unquote) (g204 (car p) (cdr p)) (if (equal?
(car p) 'unquote-splicing) (g204 (car p) (cdr p)) (if (and (pair?
(cdr p)) (dot-dot-k?  (cadr p)) (null?  (cddr p))) ((lambda (p ddk)
`(,(ordinary p) ,ddk)) (car p) (cadr p)) (g204 (car p) (cdr
p)))))))))))))) (if (vector?  p) ((lambda (p) (let* ((pl (vector->list
p)) (rpl (reverse pl))) (apply vector (if (and (not (null? rpl))
(dot-dot-k?  (car rpl))) (reverse (cons (car rpl) (map ordinary (cdr
rpl)))) (map ordinary pl))))) p) (if (box? p) ((lambda (p) (box
(ordinary (unbox p)))) p)

((lambda ()
(match:syntax-err
pattern
"syntax error in pattern"))))))))))))
(quasi (lambda (p)
(let ((g193 (lambda (x
y)
(cons (quasi
x)
(quasi
y)))))
(if (simple? p)
((lambda (p) p)
p)
(if (symbol? p)
((lambda (p)
`',p)
p)
(if (pair?
p)
(if (equal?
(car p)
'unquote)
(if (and (pair?
(cdr p))
(null?
(cddr p)))
((lambda (p)
(ordinary
p))
(cadr p))
(g193 (car p)
(cdr p)))
(if (and (pair?
(car p))
(equal?
(caar p)
'unquote-splicing)
(pair?
(cdar p))
(null?
(cddar
p)))
(if (null?
(cdr p))
((lambda (p)
(ordinary
p))
(cadar
p))
((lambda (p
y)
(append
(ordlist
p)
(quasi
y)))
(cadar
p)
(cdr p)))
(if (and (pair?
(cdr p))
(dot-dot-k?
(cadr p))
(null?
(cddr p)))
((lambda (p
ddk)
`(,(quasi
p)
,ddk))
(car p)
(cadr p))
(g193 (car p)
(cdr p)))))
(if (vector?
p)
((lambda (p)
(let* ((pl (vector->list
p))
(rpl (reverse
pl)))
(apply
vector
(if (dot-dot-k?
(car rpl))
(reverse
(cons (car rpl)
(map quasi
(cdr rpl))))
(map ordinary
pl)))))
p)
(if (box? p)
((lambda (p)
(box (quasi
(unbox
p))))
p)
((lambda ()
(match:syntax-err
pattern
"syntax error in pattern")))))))))))
(ordlist (lambda (p)
(cond
((null? p) '())
((pair? p) (cons (ordinary
(car p))
(ordlist
(cdr p))))
(else (match:syntax-err
pattern
"invalid use of unquote-splicing in pattern"))))))
(ordinary pattern))))
(bound (lambda (pattern)
(letrec ((pred-bodies '())
(bound (lambda (p a k)
(cond
((eq? '_ p) (k p a))
((symbol? p) (if (memq p a)
(match:syntax-err
pattern
"duplicate variable in pattern"))
(k p (cons p a)))
((and (pair? p)
(eq? 'quote (car p))) (k p
a))
((and (pair? p)
(eq? '? (car p))) (cond
((not (null?
(cddr p))) (bound
`(and (? ,(cadr p))
,@(cddr p))
a
k))
((or (not (symbol?
(cadr p)))
(memq (cadr p)
a)) (let ((g (gensym)))
(set! pred-bodies
(cons `(,g ,(cadr p))
pred-bodies))
(k `(? ,g)
a)))
(else (k p
a))))
((and (pair? p)
(eq? 'and (car p))) (bound*
(cdr p)
a
(lambda (p
a)
(k `(and ,@p)
a))))
((and (pair? p)
(eq? 'or (car p))) (bound
(cadr p)
a
(lambda (first-p
first-a)
(let or* ((plist (cddr p))
(k (lambda (plist)
(k `(or ,first-p
,@plist)
first-a))))
(if (null?
plist)
(k plist)
(bound
(car plist)
a
(lambda (car-p
car-a)
(if (not (permutation
car-a
first-a))
(match:syntax-err
pattern
"variables of or-pattern differ in"))
(or* (cdr plist)
(lambda (cdr-p)
(k (cons car-p
cdr-p)))))))))))
((and (pair? p)
(eq? 'not (car p))) (cond
((not (null?
(cddr p))) (bound
`(not (or ,@(cdr p)))
a
k))
(else (bound
(cadr p)
a
(lambda (p2
a2)
(if (not (permutation
a
a2))
(match:syntax-err
p
"no variables allowed in"))
(k `(not ,p2)
a))))))
((and (pair? p)
(pair? (cdr p))
(dot-dot-k? (cadr p))) (bound
(car p)
a
(lambda (q
b)
(let ((bvars (find-prefix
b
a)))
(k `(,q ,(cadr p)
,bvars
,(gensym)
,(gensym)
,(map (lambda (_)
(gensym))
bvars))
b)))))
((and (pair? p)
(eq? '$ (car p))) (bound*
(cddr p)
a
(lambda (p1
a)
(k `($ ,(cadr p)
,@p1)
a))))
((and (pair? p)
(eq? 'set! (car p))) (if (memq (cadr p)
a)
(k p
a)
(k p
(cons (cadr p)
a))))
((and (pair? p)
(eq? 'get! (car p))) (if (memq (cadr p)
a)
(k p
a)
(k p
(cons (cadr p)
a))))
((pair? p) (bound
(car p)
a
(lambda (car-p a)
(bound
(cdr p)
a
(lambda (cdr-p
a)
(k (cons car-p
cdr-p)
a))))))
((vector? p) (boundv
(vector->list
p)
a
(lambda (pl a)
(k (list->vector
pl)
a))))
((box? p) (bound
(unbox p)
a
(lambda (p a)
(k (box p)
a))))
(else (k p a)))))
(boundv (lambda (plist a k)
(let ((g187 (lambda ()
(k plist a))))
(if (pair? plist)
(if (and (pair?
(cdr plist))
(dot-dot-k?
(cadr plist))
(null?
(cddr plist)))
((lambda ()
(bound
plist
a
k)))
(if (null? plist)
(g187)
((lambda (x y)
(bound
x
a
(lambda (car-p
a)
(boundv
y
a
(lambda (cdr-p
a)
(k (cons car-p
cdr-p)
a))))))
(car plist)
(cdr plist))))
(if (null? plist)
(g187)
((#%global-defined-value 'match:error)
plist))))))
(bound* (lambda (plist a k)
(if (null? plist)
(k plist a)
(bound
(car plist)
a
(lambda (car-p a)
(bound*
(cdr plist)
a
(lambda (cdr-p a)
(k (cons car-p
cdr-p)
a))))))))
(find-prefix (lambda (b a)
(if (eq? b a)
'()
(cons (car b)
(find-prefix
(cdr b)
a)))))
(permutation (lambda (p1 p2)
(and (= (length p1)
(length p2))
(andmap
(lambda (x1)
(memq x1 p2))
p1)))))
(bound
pattern
'()
(lambda (p a)
(list p (reverse a) pred-bodies))))))
(inline-let (lambda (let-exp)
(letrec ((occ (lambda (x e)
(let loop ((e e))
(cond
((pair? e) (+ (loop (car e))
(loop (cdr e))))
((eq? x e) 1)
(else 0)))))
(subst (lambda (e old new)
(let loop ((e e))
(cond
((pair? e) (cons (loop (car e))
(loop (cdr e))))
((eq? old e) new)
(else e)))))
(const? (lambda (sexp)
(or (symbol? sexp)
(boolean? sexp)
(string? sexp)
(char? sexp)
(number? sexp)
(null? sexp)
(and (pair? sexp)
(eq? (car sexp)
'quote)
(pair? (cdr sexp))
(symbol?
(cadr sexp))
(null?
(cddr sexp))))))
(isval? (lambda (sexp)
(or (const? sexp)
(and (pair? sexp)
(memq (car sexp)
'(lambda quote
match-lambda
match-lambda*))))))
(small? (lambda (sexp)
(or (const? sexp)
(and (pair? sexp)
(eq? (car sexp)
'lambda)
(pair? (cdr sexp))
(pair? (cddr sexp))
(const?
(caddr sexp))
(null?
(cdddr sexp)))))))
(let loop ((b (cadr let-exp))
(new-b '())
(e (caddr let-exp)))
(cond
((null? b) (if (null? new-b)
e
`(let ,(reverse new-b)
,e)))
((isval? (cadr (car b))) (let* ((x (caar b))
(n (occ x
e)))
(cond
((= 0 n) (loop (cdr b)
new-b
e))
((or (= 1
n)
(small?
(cadr (car b)))) (loop (cdr b)
new-b
(subst
e
x
(cadr (car b)))))
(else (loop (cdr b)
(cons (car b)
new-b)
e)))))
(else (loop (cdr b)
(cons (car b) new-b)
e)))))))
(gen (lambda (x sf plist erract length>= eta)
(if (null? plist)
(erract x)
(let* ((v '())
(val (lambda (x) (cdr (assq x v))))
(fail (lambda (sf)
(gen x
sf
(cdr plist)
erract
length>=
eta)))
(success (lambda (sf)
(set-car!
(cddddr (car plist))
#t)
(let* ((code (cadr (car plist)))
(bv (caddr (car plist)))
(fail-sym (cadddr
(car plist))))
(if fail-sym
(let ((ap `(,code
,fail-sym
,@(map val
bv))))
`(call/ec
(lambda (,fail-sym)
(let ((,fail-sym (lambda ()
;; Changed CF 9.24.96 for multiple value returns
(call-with-values
(lambda () ,(fail sf))
,fail-sym))))
,ap))))
`(,code
,@(map val bv)))))))
(let next ((p (caar plist))
(e x)
(sf sf)
(kf fail)
(ks success))
(cond
((eq? '_ p) (ks sf))
((symbol? p) (set! v (cons (cons p e) v))
(ks sf))
((null? p) (emit `(null? ,e) sf kf ks))
((string? p) (emit `(equal? ,e ,p)
sf
kf
ks))
((boolean? p) (emit `(equal? ,e ,p)
sf
kf
ks))
((char? p) (emit `(equal? ,e ,p) sf kf ks))
((number? p) (emit `(equal? ,e ,p)
sf
kf
ks))
((and (pair? p) (eq? 'quote (car p))) (emit `(equal?
,e
,p)
sf
kf
ks))
((and (pair? p) (eq? '? (car p))) (let ((tst `(,(cadr p)
,e)))
(emit tst
sf
kf
ks)))
((and (pair? p) (eq? 'and (car p))) (let loop ((p (cdr p))
(sf sf))
(if (null?
p)
(ks sf)
(next (car p)
e
sf
kf
(lambda (sf)
(loop (cdr p)
sf))))))
((and (pair? p) (eq? 'or (car p))) (let ((or-v v))
(let loop ((p (cdr p))
(sf sf))
(if (null?
p)
(kf sf)
(begin (set! v
or-v)
(next (car p)
e
sf
(lambda (sf)
(loop (cdr p)
sf))
ks))))))
((and (pair? p) (eq? 'not (car p))) (next (cadr p)
e
sf
ks
kf))
((and (pair? p) (eq? '$ (car p))) (let* ((tag (cadr p))
(fields (cdr p))
(rlen (length
fields))
(tst `(,(symbol-append
tag
'?)
,e)))
(emit tst
sf
kf
(let rloop ((n 1))
(lambda (sf)
(if (= n
rlen)
(ks sf)
(next (list-ref
fields
n)
`(struct-ref
,e
,(sub1 n))
sf
kf
(rloop
(+ 1
n)))))))))
((and (pair? p) (eq? 'set! (car p))) (set! v
(cons (cons (cadr p)
(setter
e
p))
v))
(ks sf))
((and (pair? p) (eq? 'get! (car p))) (set! v
(cons (cons (cadr p)
(getter
e
p))
v))
(ks sf))
((and (pair? p)
(pair? (cdr p))
(dot-dot-k? (cadr p))) (emit `(list?
,e)
sf
kf
(lambda (sf)
(let* ((k (dot-dot-k?
(cadr p)))
(ks (lambda (sf)
(let ((bound (list-ref
p
2)))
(cond
((eq? (car p)
'_) (ks sf))
((null?
bound) (let* ((ptst (next (car p)
eta
sf
(lambda (sf)
#f)
(lambda (sf)
#t)))
(tst (if (and (pair?
ptst)
(symbol?
(car ptst))
(pair?
(cdr ptst))
(eq? eta
(cadr ptst))
(null?
(cddr ptst)))
(car ptst)
`(lambda (,eta)
,ptst))))
(assm `(andmap
,tst
,e)
(kf sf)
(ks sf))))
((and (symbol?
(car p))
(equal?
(list (car p))
bound)) (next (car p)
e
sf
kf
ks))
(else (let* ((gloop (list-ref
p
3))
(ge (list-ref
p
4))
(fresh (list-ref
p
5))
(p1 (next (car p)
`(car ,ge)
sf
kf
(lambda (sf)
`(,gloop
(cdr ,ge)
,@(map (lambda (b
f)
`(cons ,(val b)
,f))
bound
fresh))))))
(set! v
(append
(map cons
bound
(map (lambda (x)
`(reverse
,x))
fresh))
v))
`(let ,gloop
((,ge ,e)
,@(map (lambda (x)
`(,x '()))
fresh))
(if (null?
,ge)
,(ks sf)
,p1)))))))))
(case k
((0) (ks sf))
((1) (emit `(pair?
,e)
sf
kf
ks))
(else (emit `((,length>=
,k)
,e)
sf
kf
ks)))))))
((pair? p) (emit `(pair? ,e)
sf
kf
(lambda (sf)
(next (car p)
(add-a e)
sf
kf
(lambda (sf)
(next (cdr p)
(add-d
e)
sf
kf
ks))))))
((and (vector? p)
(>= (vector-length p) 6)
(dot-dot-k?
(vector-ref
p
(- (vector-length p) 5)))) (let* ((vlen (- (vector-length
p)
6))
(k (dot-dot-k?
(vector-ref
p
(+ vlen
1))))
(minlen (+ vlen
k))
(bound (vector-ref
p
(+ vlen
2))))
(emit `(vector?
,e)
sf
kf
(lambda (sf)
(assm `(>= (vector-length
,e)
,minlen)
(kf sf)
((let vloop ((n 0))
(lambda (sf)
(cond
((not (= n
vlen)) (next (vector-ref
p
n)
`(vector-ref
,e
,n)
sf
kf
(vloop
(+ 1
n))))
((eq? (vector-ref
p
vlen)
'_) (ks sf))
(else (let* ((gloop (vector-ref
p
(+ vlen
3)))
(ind (vector-ref
p
(+ vlen
4)))
(fresh (vector-ref
p
(+ vlen
5)))
(p1 (next (vector-ref
p
vlen)
`(vector-ref
,e
,ind)
sf
kf
(lambda (sf)
`(,gloop
(- ,ind
1)
,@(map (lambda (b
f)
`(cons ,(val b)
,f))
bound
fresh))))))
(set! v
(append
(map cons
bound
fresh)
v))
`(let ,gloop
((,ind (- (vector-length
,e)
1))
,@(map (lambda (x)
`(,x '()))
fresh))
(if (> ,minlen
,ind)
,(ks sf)
,p1)))))))
sf))))))
((vector? p) (let ((vlen (vector-length p)))
(emit `(vector? ,e)
sf
kf
(lambda (sf)
(emit `(equal?
(vector-length
,e)
,vlen)
sf
kf
(let vloop ((n 0))
(lambda (sf)
(if (= n
vlen)
(ks sf)
(next (vector-ref
p
n)
`(vector-ref
,e
,n)
sf
kf
(vloop
(+ 1
n)))))))))))
((box? p) (emit `(box? ,e)
sf
kf
(lambda (sf)
(next (unbox p)
`(unbox ,e)
sf
kf
ks))))
(else (display
"FATAL ERROR IN PATTERN MATCHER")
(newline)
(error #f "THIS NEVER HAPPENS"))))))))
(emit (lambda (tst sf kf ks)
(cond
((in tst sf) (ks sf))
((in `(not ,tst) sf) (kf sf))
(else (let* ((e (cadr tst))
(implied (cond
((eq? (car tst) 'equal?) (let ((p (caddr
tst)))
(cond
((string?
p) `((string?
,e)))
((boolean?
p) `((boolean?
,e)))
((char?
p) `((char?
,e)))
((number?
p) `((number?
,e)))
((and (pair?
p)
(eq? 'quote
(car p))) `((symbol?
,e)))
(else '()))))
((eq? (car tst) 'null?) `((list?
,e)))
((vec-structure? tst) `((vector?
,e)))
(else '())))
(not-imp (case (car tst)
((list?) `((not (null?
,e))))
(else '())))
(s (ks (cons tst (append implied sf))))
(k (kf (cons `(not ,tst)
(append not-imp sf)))))
(assm tst k s))))))
(assm (lambda (tst f s)
(cond
((equal? s f) s)
((and (eq? s #t) (eq? f #f)) tst)
((and (eq? (car tst) 'pair?)
(memq match:error-control
'(unspecified fail))
(memq (car f) '(cond match:error))
(guarantees s (cadr tst))) s)
((and (pair? s)
(eq? (car s) 'if)
(equal? (cadddr s) f)) (if (eq? (car (cadr s))
'and)
`(if (and ,tst
,@(cdr (cadr s)))
,(caddr s)
,f)
`(if (and ,tst
,(cadr s))
,(caddr s)
,f)))

;; ### OLD call-with-current-continuation test was here

((and #f
(pair? s)
(equal? (car s) 'let)
(pair? (cdr s))
(pair? (cadr s))
(pair? (caadr s))
(pair? (cdaadr s))
(pair? (car (cdaadr s)))
(equal? (caar (cdaadr s)) 'lambda)
(pair? (cdar (cdaadr s)))
(null? (cadar (cdaadr s)))
(pair? (cddar (cdaadr s)))
(null? (cdddar (cdaadr s)))
(null? (cdr (cdaadr s)))
(null? (cdadr s))
(pair? (cddr s))
(null? (cdddr s))
(equal? (caddar (cdaadr s)) f)) (let ((fail (caaadr
s))
(s2 (caddr
s)))
`(let ((,fail (lambda ()
,f)))
,(assm tst
`(,fail)
s2))))
(else `(if ,tst ,s ,f)))))
(guarantees (lambda (code x)
(let ((a (add-a x)) (d (add-d x)))
(let loop ((code code))
(cond
((not (pair? code)) #f)
((memq (car code) '(cond match:error)) #t)
((or (equal? code a) (equal? code d)) #t)
((eq? (car code) 'if) (or (loop (cadr code))
(and (loop (caddr
code))
(loop (cadddr
code)))))
((eq? (car code) 'lambda) #f)
((and (eq? (car code) 'let)
(symbol? (cadr code))) #f)
(else (or (loop (car code))
(loop (cdr code)))))))))
(in (lambda (e l)
(or (member e l)
(and (eq? (car e) 'list?)
(or (member `(null? ,(cadr e)) l)
(member `(pair? ,(cadr e)) l)))
(and (eq? (car e) 'not)
(let* ((srch (cadr e))
(const-class (equal-test? srch)))
(cond
(const-class (let mem ((l l))
(if (null? l)
#f
(let ((x (car l)))
(or (and (equal?
(cadr x)
(cadr srch))
(disjoint?
x)
(not (equal?
const-class
(car x))))
(equal?
x
`(not (,const-class
,(cadr srch))))
(and (equal?
(cadr x)
(cadr srch))
(equal-test?
x)
(not (equal?
(caddr
srch)
(caddr
x))))
(mem (cdr l)))))))
((disjoint? srch) (let mem ((l l))
(if (null? l)
#f
(let ((x (car l)))
(or (and (equal?
(cadr x)
(cadr srch))
(disjoint?
x)
(not (equal?
(car x)
(car srch))))
(mem (cdr l)))))))
((eq? (car srch) 'list?) (let mem ((l l))
(if (null? l)
#f
(let ((x (car l)))
(or (and (equal?
(cadr x)
(cadr srch))
(disjoint?
x)
(not (memq (car x)
'(list?
pair?
null?))))
(mem (cdr l)))))))
((vec-structure? srch) (let mem ((l l))
(if (null? l)
#f
(let ((x (car l)))
(or (and (equal?
(cadr x)
(cadr srch))
(or (disjoint?
x)
(vec-structure?
x))
(not (equal?
(car x)
'vector?))
(not (equal?
(car x)
(car srch))))
(equal?
x
`(not (vector?
,(cadr srch))))
(mem (cdr l)))))))
(else #f)))))))
(equal-test? (lambda (tst)
(and (eq? (car tst) 'equal?)
(let ((p (caddr tst)))
(cond
((string? p) 'string?)
((boolean? p) 'boolean?)
((char? p) 'char?)
((number? p) 'number?)
((and (pair? p)
(pair? (cdr p))
(null? (cddr p))
(eq? 'quote (car p))
(symbol? (cadr p))) 'symbol?)
(else #f))))))
(disjoint? (lambda (tst)
(memq (car tst) match:disjoint-predicates)))
(vec-structure? (lambda (tst)
(memq (car tst) match:vector-structures)))
(add-a (lambda (a)
(let ((new (and (pair? a) (assq (car a) c---rs))))
(if new (cons (cadr new) (cdr a)) `(car ,a)))))
(add-d (lambda (a)
(let ((new (and (pair? a) (assq (car a) c---rs))))
(if new (cons (cddr new) (cdr a)) `(cdr ,a)))))
(c---rs '((car caar . cdar)
(cdr cadr . cddr)
(caar caaar . cdaar)
(cadr caadr . cdadr)
(cdar cadar . cddar)
(cddr caddr . cdddr)
(caaar caaaar . cdaaar)
(caadr caaadr . cdaadr)
(cadar caadar . cdadar)
(caddr caaddr . cdaddr)
(cdaar cadaar . cddaar)
(cdadr cadadr . cddadr)
(cddar caddar . cdddar)
(cdddr cadddr . cddddr)))
(setter (lambda (e p)
(let ((mk-setter (lambda (s)
(symbol-append 'set- s '!))))
(cond
((not (pair? e)) (match:syntax-err
p
"unnested set! pattern"))
((eq? (car e) 'vector-ref) `(let ((x ,(cadr e)))
(lambda (y)
(vector-set!
x
,(caddr
e)
y))))
((eq? (car e) 'unbox) `(let ((x ,(cadr e)))
(lambda (y)
(set-box! x y))))
((eq? (car e) 'car) `(let ((x ,(cadr e)))
(lambda (y)
(set-car! x y))))
((eq? (car e) 'cdr) `(let ((x ,(cadr e)))
(lambda (y)
(set-cdr! x y))))
((let ((a (assq (car e) get-c---rs)))
(and a
`(let ((x (,(cadr a) ,(cadr e))))
(lambda (y)
(,(mk-setter (cddr a))
x
y))))))
(else `(let ((x ,(cadr e)))
(lambda (y)
(,(mk-setter (car e)) x y))))))))
(getter (lambda (e p)
(cond
((not (pair? e)) (match:syntax-err
p
"unnested get! pattern"))
((eq? (car e) 'vector-ref) `(let ((x ,(cadr e)))
(lambda ()
(vector-ref
x
,(caddr
e)))))
((eq? (car e) 'unbox) `(let ((x ,(cadr e)))
(lambda () (unbox x))))
((eq? (car e) 'car) `(let ((x ,(cadr e)))
(lambda () (car x))))
((eq? (car e) 'cdr) `(let ((x ,(cadr e)))
(lambda () (cdr x))))
((let ((a (assq (car e) get-c---rs)))
(and a
`(let ((x (,(cadr a) ,(cadr e))))
(lambda () (,(cddr a) x))))))
(else `(let ((x ,(cadr e)))
(lambda () (,(car e) x)))))))
(get-c---rs '((caar car . car)
(cadr cdr . car)
(cdar car . cdr)
(cddr cdr . cdr)
(caaar caar . car)
(caadr cadr . car)
(cadar cdar . car)
(caddr cddr . car)
(cdaar caar . cdr)
(cdadr cadr . cdr)
(cddar cdar . cdr)
(cdddr cddr . cdr)
(caaaar caaar . car)
(caaadr caadr . car)
(caadar cadar . car)
(caaddr caddr . car)
(cadaar cdaar . car)
(cadadr cdadr . car)
(caddar cddar . car)
(cadddr cdddr . car)
(cdaaar caaar . cdr)
(cdaadr caadr . cdr)
(cdadar cadar . cdr)
(cdaddr caddr . cdr)
(cddaar cdaar . cdr)
(cddadr cdadr . cdr)
(cdddar cddar . cdr)
(cddddr cdddr . cdr)))
(symbol-append (lambda l
(string->symbol
(apply
string-append
(map (lambda (x)
(cond
((symbol? x) (symbol->string
x))
((number? x) (number->string
x))
(else x)))
l)))))
(rac (lambda (l) (if (null? (cdr l)) (car l) (rac (cdr l)))))
(rdc (lambda (l)
(if (null? (cdr l)) '() (cons (car l) (rdc (cdr l)))))))
(list genmatch genletrec gendefine pattern-var?)))
(define match
(lambda args
(cond
((and (list? args)
(<= 1 (length args))
(andmap
(lambda (y) (and (list? y) (<= 2 (length y))))
(cdr args))) (let* ((exp (car args))
(clauses (cdr args))
(e (if (symbol? exp) exp (gensym))))
(if (symbol? exp)
((car match:expanders)
e
clauses
`(match ,@args))
`(let ((,e ,exp))
,((car match:expanders)
e
clauses
`(match ,@args))))))
(else (match:syntax-err `(match ,@args) "syntax error in")))))
(define match-lambda
(lambda args
(if (and (list? args)
(andmap
(lambda (g184)
(if (and (pair? g184) (list? (cdr g184)))
(pair? (cdr g184))
#f))
args))
((lambda ()
(let ((e (gensym))) `(lambda (,e) (match ,e ,@args)))))
((lambda ()
(match:syntax-err
`(match-lambda ,@args)
"syntax error in"))))))
(define match-lambda*
(lambda args
(if (and (list? args)
(andmap
(lambda (g176)
(if (and (pair? g176) (list? (cdr g176)))
(pair? (cdr g176))
#f))
args))
((lambda ()
(let ((e (gensym))) `(lambda ,e (match ,e ,@args)))))
((lambda ()
(match:syntax-err
`(match-lambda* ,@args)
"syntax error in"))))))
(define match-let
(lambda args
(let ((g154 (cadddr match:expanders))
(g153 (lambda (pat exp body) `(match ,exp (,pat ,@body))))
(g149 (lambda (p1 e1 p2 e2 body)
(let ((g1 (gensym)) (g2 (gensym)))
`(let ((,g1 ,e1) (,g2 ,e2))
(match (cons ,g1 ,g2) ((,p1 . ,p2) ,@body))))))
(g145 (lambda (pat exp body)
(let ((g (map (lambda (x) (gensym)) pat))
(vpattern (list->vector pat)))
`(let ,(map list g exp)
(match (vector ,@g) (,vpattern ,@body))))))
(g137 (lambda ()
(match:syntax-err `(match-let ,@args) "syntax error in"))))
(if (pair? args)
(if (symbol? (car args))
(if (and (pair? (cdr args)) (list? (cadr args)))
(let g163 ((g162 (cadr args)) (g161 '()) (g160 '()))
(if (null? g162)
(if (and (list? (cddr args)) (pair? (cddr args)))
((lambda (name pat exp body)
(if (andmap
(cadddr match:expanders)
pat)
`(let ,@args)
`(letrec ((,name (match-lambda*
(,pat ,@body))))
(,name ,@exp))))
(car args)
(reverse g160)
(reverse g161)
(cddr args))
(g137))
(if (and (pair? (car g162))
(pair? (cdar g162))
(null? (cddar g162)))
(g163 (cdr g162)
(cons (cadar g162) g161)
(cons (caar g162) g160))
(g137))))
(g137))
(if (list? (car args))
(if (andmap
(lambda (g168)
(if (and (pair? g168)
(g154 (car g168))
(pair? (cdr g168)))
(null? (cddr g168))
#f))
(car args))
(if (and (list? (cdr args)) (pair? (cdr args)))
((lambda () `(let ,@args)))
(let g141 ((g140 (car args))
(g139 '())
(g138 '()))
(if (null? g140)
(g137)
(if (and (pair? (car g140))
(pair? (cdar g140))
(null? (cddar g140)))
(g141 (cdr g140)
(cons (cadar g140) g139)
(cons (caar g140) g138))
(g137)))))
(if (and (pair? (car args))
(pair? (caar args))
(pair? (cdaar args))
(null? (cddaar args)))
(if (null? (cdar args))
(if (and (list? (cdr args))
(pair? (cdr args)))
(g153 (caaar args)
(cadaar args)
(cdr args))
(let g141 ((g140 (car args))
(g139 '())
(g138 '()))
(if (null? g140)
(g137)
(if (and (pair? (car g140))
(pair? (cdar g140))
(null? (cddar g140)))
(g141 (cdr g140)
(cons (cadar g140) g139)
(cons (caar g140) g138))
(g137)))))
(if (and (pair? (cdar args))
(pair? (cadar args))
(pair? (cdadar args))
(null? (cdr (cdadar args)))
(null? (cddar args)))
(if (and (list? (cdr args))
(pair? (cdr args)))
(g149 (caaar args)
(cadaar args)
(caadar args)
(car (cdadar args))
(cdr args))
(let g141 ((g140 (car args))
(g139 '())
(g138 '()))
(if (null? g140)
(g137)
(if (and (pair? (car g140))
(pair? (cdar g140))
(null? (cddar g140)))
(g141 (cdr g140)
(cons (cadar g140)
g139)
(cons (caar g140)
g138))
(g137)))))
(let g141 ((g140 (car args))
(g139 '())
(g138 '()))
(if (null? g140)
(if (and (list? (cdr args))
(pair? (cdr args)))
(g145 (reverse g138)
(reverse g139)
(cdr args))
(g137))
(if (and (pair? (car g140))
(pair? (cdar g140))
(null? (cddar g140)))
(g141 (cdr g140)
(cons (cadar g140) g139)
(cons (caar g140) g138))
(g137))))))
(let g141 ((g140 (car args))
(g139 '())
(g138 '()))
(if (null? g140)
(if (and (list? (cdr args))
(pair? (cdr args)))
(g145 (reverse g138)
(reverse g139)
(cdr args))
(g137))
(if (and (pair? (car g140))
(pair? (cdar g140))
(null? (cddar g140)))
(g141 (cdr g140)
(cons (cadar g140) g139)
(cons (caar g140) g138))
(g137))))))
(if (pair? (car args))
(if (and (pair? (caar args))
(pair? (cdaar args))
(null? (cddaar args)))
(if (null? (cdar args))
(if (and (list? (cdr args))
(pair? (cdr args)))
(g153 (caaar args)
(cadaar args)
(cdr args))
(let g141 ((g140 (car args))
(g139 '())
(g138 '()))
(if (null? g140)
(g137)
(if (and (pair? (car g140))
(pair? (cdar g140))
(null? (cddar g140)))
(g141 (cdr g140)
(cons (cadar g140) g139)
(cons (caar g140) g138))
(g137)))))
(if (and (pair? (cdar args))
(pair? (cadar args))
(pair? (cdadar args))
(null? (cdr (cdadar args)))
(null? (cddar args)))
(if (and (list? (cdr args))
(pair? (cdr args)))
(g149 (caaar args)
(cadaar args)
(caadar args)
(car (cdadar args))
(cdr args))
(let g141 ((g140 (car args))
(g139 '())
(g138 '()))
(if (null? g140)
(g137)
(if (and (pair? (car g140))
(pair? (cdar g140))
(null? (cddar g140)))
(g141 (cdr g140)
(cons (cadar g140)
g139)
(cons (caar g140)
g138))
(g137)))))
(let g141 ((g140 (car args))
(g139 '())
(g138 '()))
(if (null? g140)
(if (and (list? (cdr args))
(pair? (cdr args)))
(g145 (reverse g138)
(reverse g139)
(cdr args))
(g137))
(if (and (pair? (car g140))
(pair? (cdar g140))
(null? (cddar g140)))
(g141 (cdr g140)
(cons (cadar g140) g139)
(cons (caar g140) g138))
(g137))))))
(let g141 ((g140 (car args))
(g139 '())
(g138 '()))
(if (null? g140)
(if (and (list? (cdr args))
(pair? (cdr args)))
(g145 (reverse g138)
(reverse g139)
(cdr args))
(g137))
(if (and (pair? (car g140))
(pair? (cdar g140))
(null? (cddar g140)))
(g141 (cdr g140)
(cons (cadar g140) g139)
(cons (caar g140) g138))
(g137)))))
(g137))))
(g137)))))
(define match-let*
(lambda args
(let ((g123 (lambda ()
(match:syntax-err
`(match-let* ,@args)
"syntax error in"))))
(if (pair? args)
(if (null? (car args))
(if (and (list? (cdr args)) (pair? (cdr args)))
((lambda (body) `(let* ,@args)) (cdr args))
(g123))
(if (and (pair? (car args))
(pair? (caar args))
(pair? (cdaar args))
(null? (cddaar args))
(list? (cdar args))
(list? (cdr args))
(pair? (cdr args)))
((lambda (pat exp rest body)
(if ((cadddr match:expanders) pat)
`(let ((,pat ,exp)) (match-let* ,rest ,@body))
`(match ,exp (,pat (match-let* ,rest ,@body)))))
(caaar args)
(cadaar args)
(cdar args)
(cdr args))
(g123)))
(g123)))))
(define match-letrec
(lambda args
(let ((g115 (cadddr match:expanders))
(g114 (lambda (pat exp body)
((cadr match:expanders)
pat
exp
body
`(match-letrec ((,pat ,exp)) ,@body))))
(g110 (lambda (p1 e1 p2 e2 body)
`(match-letrec (((p1 . p2) (cons e1 e2))) ,@body)))
(g106 (lambda (pat exp body)
`(match-letrec
((,(list->vector pat) (vector ,@exp)))
,@body)))
(g98 (lambda ()
(match:syntax-err
`(match-letrec ,@args)
"syntax error in"))))
(if (pair? args)
(if (list? (car args))
(if (andmap
(lambda (g121)
(if (and (pair? g121)
(g115 (car g121))
(pair? (cdr g121)))
(null? (cddr g121))
#f))
(car args))
(if (and (list? (cdr args)) (pair? (cdr args)))
((lambda () `(letrec ,@args)))
(let g102 ((g101 (car args)) (g100 '()) (g99 '()))
(if (null? g101)
(g98)
(if (and (pair? (car g101))
(pair? (cdar g101))
(null? (cddar g101)))
(g102 (cdr g101)
(cons (cadar g101) g100)
(cons (caar g101) g99))
(g98)))))
(if (and (pair? (car args))
(pair? (caar args))
(pair? (cdaar args))
(null? (cddaar args)))
(if (null? (cdar args))
(if (and (list? (cdr args)) (pair? (cdr args)))
(g114 (caaar args) (cadaar args) (cdr args))
(let g102 ((g101 (car args))
(g100 '())
(g99 '()))
(if (null? g101)
(g98)
(if (and (pair? (car g101))
(pair? (cdar g101))
(null? (cddar g101)))
(g102 (cdr g101)
(cons (cadar g101) g100)
(cons (caar g101) g99))
(g98)))))
(if (and (pair? (cdar args))
(pair? (cadar args))
(pair? (cdadar args))
(null? (cdr (cdadar args)))
(null? (cddar args)))
(if (and (list? (cdr args))
(pair? (cdr args)))
(g110 (caaar args)
(cadaar args)
(caadar args)
(car (cdadar args))
(cdr args))
(let g102 ((g101 (car args))
(g100 '())
(g99 '()))
(if (null? g101)
(g98)
(if (and (pair? (car g101))
(pair? (cdar g101))
(null? (cddar g101)))
(g102 (cdr g101)
(cons (cadar g101) g100)
(cons (caar g101) g99))
(g98)))))
(let g102 ((g101 (car args))
(g100 '())
(g99 '()))
(if (null? g101)
(if (and (list? (cdr args))
(pair? (cdr args)))
(g106 (reverse g99)
(reverse g100)
(cdr args))
(g98))
(if (and (pair? (car g101))
(pair? (cdar g101))
(null? (cddar g101)))
(g102 (cdr g101)
(cons (cadar g101) g100)
(cons (caar g101) g99))
(g98))))))
(let g102 ((g101 (car args)) (g100 '()) (g99 '()))
(if (null? g101)
(if (and (list? (cdr args))
(pair? (cdr args)))
(g106 (reverse g99)
(reverse g100)
(cdr args))
(g98))
(if (and (pair? (car g101))
(pair? (cdar g101))
(null? (cddar g101)))
(g102 (cdr g101)
(cons (cadar g101) g100)
(cons (caar g101) g99))
(g98))))))
(if (pair? (car args))
(if (and (pair? (caar args))
(pair? (cdaar args))
(null? (cddaar args)))
(if (null? (cdar args))
(if (and (list? (cdr args)) (pair? (cdr args)))
(g114 (caaar args) (cadaar args) (cdr args))
(let g102 ((g101 (car args))
(g100 '())
(g99 '()))
(if (null? g101)
(g98)
(if (and (pair? (car g101))
(pair? (cdar g101))
(null? (cddar g101)))
(g102 (cdr g101)
(cons (cadar g101) g100)
(cons (caar g101) g99))
(g98)))))
(if (and (pair? (cdar args))
(pair? (cadar args))
(pair? (cdadar args))
(null? (cdr (cdadar args)))
(null? (cddar args)))
(if (and (list? (cdr args))
(pair? (cdr args)))
(g110 (caaar args)
(cadaar args)
(caadar args)
(car (cdadar args))
(cdr args))
(let g102 ((g101 (car args))
(g100 '())
(g99 '()))
(if (null? g101)
(g98)
(if (and (pair? (car g101))
(pair? (cdar g101))
(null? (cddar g101)))
(g102 (cdr g101)
(cons (cadar g101) g100)
(cons (caar g101) g99))
(g98)))))
(let g102 ((g101 (car args))
(g100 '())
(g99 '()))
(if (null? g101)
(if (and (list? (cdr args))
(pair? (cdr args)))
(g106 (reverse g99)
(reverse g100)
(cdr args))
(g98))
(if (and (pair? (car g101))
(pair? (cdar g101))
(null? (cddar g101)))
(g102 (cdr g101)
(cons (cadar g101) g100)
(cons (caar g101) g99))
(g98))))))
(let g102 ((g101 (car args)) (g100 '()) (g99 '()))
(if (null? g101)
(if (and (list? (cdr args))
(pair? (cdr args)))
(g106 (reverse g99)
(reverse g100)
(cdr args))
(g98))
(if (and (pair? (car g101))
(pair? (cdar g101))
(null? (cddar g101)))
(g102 (cdr g101)
(cons (cadar g101) g100)
(cons (caar g101) g99))
(g98)))))
(g98)))
(g98)))))
(define match-define
(lambda args
(let ((g94 (cadddr match:expanders))
(g92 (lambda ()
(match:syntax-err
`(match-define ,@args)
"syntax error in"))))
(if (pair? args)
(if (g94 (car args))
(if (and (pair? (cdr args)) (null? (cddr args)))
((lambda () `(begin (define ,@args))))
(g92))
(if (and (pair? (cdr args)) (null? (cddr args)))
((lambda (pat exp)
((caddr match:expanders)
pat
exp
`(match-define ,@args)))
(car args)
(cadr args))
(g92)))
(g92))))))