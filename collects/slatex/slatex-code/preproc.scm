;preproc.scm
;Macro preprocessor for SLaTeX
;(c) Dorai Sitaram, Rice U., 1991, 1994

;property lists

(define preproc:*properties* '())

(define preproc:get
  (lambda (sym prop . default)
    (let ((sym-props (assoc sym preproc:*properties*)))
      (cond (sym-props
	     (let ((prop-val (assoc prop (cdr sym-props))))
	       (cond (prop-val (cdr prop-val))
		     ((pair? default) (car default))
		     (else #f))))
	    ((pair? default) (car default))
	    (else #f)))))

(define preproc:put
  (lambda (sym prop val)
    (let ((sym-props (assoc sym preproc:*properties*)))
      (if sym-props
	  (let* ((props (cdr sym-props))
		 (prop-val (assoc prop props)))
	    (if prop-val
		(set-cdr! prop-val val)
		(set-cdr! sym-props
			  (cons (cons prop val) props))))
	  (set! preproc:*properties*
	    (cons (cons sym (list (cons prop val)))
		  preproc:*properties*))))))

;define-macro

(define defmacro-preproc
  (lambda (kw xfmr)
    (preproc:put #f kw kw)
    (preproc:put kw 'defmacro-preproc xfmr)))

(define preproc:macro?
  (lambda (s)
    (and (symbol? s)
	 (preproc:get s 'defmacro-preproc))))

(define expand-macrocalls
  (lambda (e)
    (if (not (pair? e)) e
        (let* ((a (car e)) (xfmr (preproc:macro? a)))
          (if xfmr
              (expand-macrocalls (apply xfmr (cdr e)))
              (case a
                ;;something that looks like a macro call
                ;;within quote shouldn't be expanded
                ((quote) e)
                ;;lambda-arg can contain dotted list -- so
                ;;we avoid letting else-clause map across it
                ((lambda)
                 `(lambda ,(cadr e)
                    ,@(map expand-macrocalls (cddr e))))
                ;;case-tags can look like macro calls -- these
                ;;shouldn't be expanded
                ((case)
                 `(case ,(expand-macrocalls (cadr e))
                    ,@(map (lambda (clause)
                             `(,(car clause)
                               ,@(map expand-macrocalls (cdr clause))))
                           (cddr e))))
                ;;expand-macrocalls can be mapped across the rest --
                ;;it isn't likely that we can have an expression
                ;;that looks like a macro call but isn't
                (else (map expand-macrocalls e))))))))

;some macros

;package

(define make-slatex-alias
  (lambda (zz)
    (if (not (null? zz))
        (begin
          (preproc:put 'slatex (car zz) (cadr zz))
          (make-slatex-alias (cddr zz))))))

(load "aliases.scm")

(define preproc:string-index
  (lambda (s c)
    (let ((n (string-length s)))
      (let loop ((i 0))
        (cond ((>= i n) #f)
              ((char=? (string-ref s i) c) i)
              (else (loop (+ i 1))))))))

(defmacro-preproc 'in-package
  (lambda (p) #f))

(defmacro-preproc 'shadow
  (lambda (xx) #f))

(define *current-package* #f)

(defmacro-preproc 'eval-within
  (lambda (p . ee)
    (let ((ee
           (let insert-qualifieds ((e ee))
             (cond ((pair? e)
                    (set-car! e (insert-qualifieds (car e)))
                    (set-cdr! e (insert-qualifieds (cdr e)))
                    e)
                   ((symbol? e)
                    (%eval-within-get-qualified-symbol p e))
                   (else e)))))
      (case (length ee)
        ((0) #f)
        ((1) (car ee))
        (else (cons 'begin ee))))))

(define %eval-within-get-qualified-symbol
  (lambda (curr-p px)
    (let* ((px-s (symbol->string px))
           (i (%eval-within-dblcolon-index px-s)))
      (cond (i (let ((p (string->symbol (substring px-s 0 i)))
                     (x (string->symbol (substring px-s (+ i 2)
                                                   (string-length px-s)))))
                 (if (eq? p curr-p) (preproc:put p x px))
                 px))
            (else (cond ((preproc:get curr-p px))
                        ((preproc:get #f px))
                        (else px)))))))

(define %eval-within-dblcolon-index
  (lambda (s)
    (let ((i (preproc:string-index s #\:)))
      (if (or (not i)
              (= i (- (string-length s) 1))) #f
              (let ((i+1 (+ i 1)))
                (if (char=? (string-ref s i+1) #\:)
                    i #f))))))

;defvar

(defmacro-preproc 'defvar
  (lambda (x e)
    `(define ,x ,e)))

;fluid-let

(define gentemp
  (let ((n -1))
    (lambda ()
      ;;generates an allegedly new symbol.  This is a
      ;;gross hack since there is no standardized way
      ;;of getting uninterned symbols
      (set! n (+ n 1))
      (string->symbol (string-append "%:g" (number->string n) "%")))))

(defmacro-preproc 'fluid-let
  (lambda (let-pairs . body)
    (let ((x-s (map car let-pairs))
    	  (i-s (map cadr let-pairs))
    	  (old-x-s (map (lambda (p) (gentemp)) let-pairs)))
      `(let ,(map (lambda (old-x x) `(,old-x ,x)) old-x-s x-s)
         ,@(map (lambda (x i) `(set! ,x ,i)) x-s i-s)
         (let ((%temp% (begin ,@body)))
    	   ,@(map (lambda (x old-x) `(set! ,x ,old-x)) x-s old-x-s)
    	   %temp%)))))

;defenum

(defmacro-preproc 'defenum
  (lambda z
    (let loop ((z z) (n 0) (r '()))
      (if (null? z) `(begin ,@r)
          (loop (cdr z) (+ n 1)
                (cons `(define ,(car z) (integer->char ,n)) r))))))

;defrecord

(defmacro-preproc 'defrecord
  (lambda (name . fields)
    (let loop ((fields fields) (i 0) (r '()))
      (if (null? fields)
	  `(begin (define ,name (lambda () (make-vector ,i)))
             ,@r)
	  (loop (cdr fields) (+ i 1)
                (cons `(define ,(car fields) ,i) r))))))

;of

(defmacro-preproc 'of
  (lambda (r i . z)
    (cond ((null? z) `(vector-ref ,r ,i))
          ((and (eq? i '/) (= (length z) 1))
           `(string-ref ,r ,(car z)))
          (else `(of (vector-ref ,r ,i) ,@z)))))

;setf

(defmacro-preproc 'setf
  (lambda (l r)
    (if (symbol? l) `(set! ,l ,r)
        (let ((a (car l)))
	  (if (eq? a 'list-ref)
	      `(set-car! (list-tail ,@(cdr l)) ,r)
	      `(,(cond ((eq? a 'list-ref) 'list-set!)
		       ((eq? a 'string-ref) 'string-set!)
		       ((eq? a 'vector-ref) 'vector-set!)
		       ((eq? a 'of) 'the-setter-for-of)
		       (else
			(error "(setf ~s ~s) is ill-formed." l r)))
		,@(cdr l) ,r))))))

;the-setter-for-of

(defmacro-preproc 'the-setter-for-of
  (lambda (r i j . z)
    (cond ((null? z) `(vector-set! ,r ,i ,j))
          ((and (eq? i '/) (= (length z) 1))
           `(string-set! ,r ,j ,(car z)))
          (else `(the-setter-for-of (vector-ref ,r ,i) ,j ,@z)))))

;eval-{if,unless}

(defmacro-preproc 'eval-if
  (lambda (dialects . body)
    (if (memq dialect dialects)
	(if (= (length body) 1) (car body)
            `(begin ,@body))
	`#f)))

(defmacro-preproc 'eval-unless
  (lambda (dialects . body)
    (if (not (memq dialect dialects))
	(if (= (length body) 1) (car body)
            `(begin ,@body))
	`#f)))

;func{tion, all}

(defmacro-preproc 'function
  (lambda (x)
    `,x))

(defmacro-preproc 'funcall
  (lambda (f . args)
    `(,f ,@args)))
