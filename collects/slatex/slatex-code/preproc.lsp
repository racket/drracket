;preproc.lsp
;Preprocessor to allow CL interpret the brand of Scheme
;used in SLaTeX.
;(c) Dorai Sitaram, Nov. 1992

#+gcl
(make-package :slatex)

#-gcl
(defpackage slatex
  (:use cl))

;print lower-case

(setq *print-case* :downcase)

;defmacro-slatex

(defmacro defmacro-slatex (m vv &rest ee)
  `(progn
     (setf (get nil ',m) ',m)
     (setf (get ',m 'defmacro-slatex)
	   #'(lambda ,vv ,@ee))))

(defun slatex-macro-p (s)
  (and (symbolp s) (get s 'defmacro-slatex)))

(defun expand-macrocalls (e)
  (if (not (consp e)) e
    (let* ((a (car e)) (xfmr (slatex-macro-p a)))
      (if xfmr
	  (expand-macrocalls (apply xfmr (cdr e)))
	(case a
	  ((quote) e)
	  ((lambda)
	   `(lambda ,(cadr e)
	      ,@(mapcar #'expand-macrocalls (cddr e))))
	  ((case)
	   `(case ,(expand-macrocalls (cadr e))
	      ,@(mapcar #'(lambda (clause)
			    `(,(car clause)
			      ,@(mapcar #'expand-macrocalls (cdr clause))))
			(cddr e))))
	  (t (mapcar #'expand-macrocalls e)))))))

;some macros

;package

(defvar *alias-alist* '())

(defun make-slatex-alias (zz)
  (loop
   (when (null zz) (return))
   (push (cons (car zz) (cadr zz)) *alias-alist*)
   (setq zz (cddr zz))))

(load "aliases.scm")

(defmacro-slatex eval-within (p &rest ee)
  (let ((ee (nsublis *alias-alist* ee)))
    (case (length ee)
      ((0) nil)
      ((1) (car ee))
      (t (cons 'progn ee)))))

(defmacro-slatex slatex::%lambda (parms &rest body)
  `(function
    (lambda ,(dot-to-and-rest parms) ; cl::lambda
      ,@body)))

(defun dot-to-and-rest (vv)
  ;Change the . z format of Scheme lambdalists to
  ;CL's &rest z format
  (cond ((null vv) nil)
	((symbolp vv) `(&rest ,vv))
	(t (let* ((last-vv (last vv))
		  (cdr-last-vv (cdr last-vv)))
	     (if cdr-last-vv
		 (progn
		   (setf (cdr last-vv) `(&rest ,cdr-last-vv))
		   vv)
	       vv)))))

(defmacro-slatex define (x e)
  (unless (and x (symbolp x) (consp e))
    (error "define ~s ~s" x e))
  (let ((a (car e)))
    (case a
      ((slatex::%let let*)
       `(,a ,(cadr e)
	    (define ,x ,(caddr e))))
      ((slatex::%lambda)
       `(defun ,x ,(dot-to-and-rest (cadr e))
	  ,@(cddr e)))
      (t (error "define ~s ~s" x e)))))

(defmacro-slatex slatex::%let (n &rest ee)
  ;Named let with name containing the string "loop"
  ;is considered to be iterative and is transformed
  ;into CL loop.
  (if (and n (symbolp n))
      (let ((tail-recursive-p
	     (search "LOOP" (symbol-name n))))
	(if (and tail-recursive-p (eq n 'loop))
	    (setf n '%%%loop%%%
		  ee (nsublis `((loop . ,n)) ee)))
	`(,(if tail-recursive-p 'named-let-tail-recursive
	     'named-let-non-tail-recursive) ,n ,@ee))
    `(let ,n ,@ee))) ; cl::let?

(defmacro-slatex named-let-non-tail-recursive (n xvxv &rest ee)
  `(labels ((,n ,(mapcar 'car xvxv) ,@ee))
	   (,n ,@(mapcar 'cadr xvxv))))

(defmacro-slatex named-let-tail-recursive (n xvxv &rest ee)
  (let ((xx (mapcar 'car xvxv)))
    `(let ,xvxv
       (flet ((,n ,xx
		  (throw ',n (values ,@xx))))
	 (loop
	  (multiple-value-setq ,xx
	    (let ,(mapcar #'(lambda (x) `(,x ,x)) xx)
	      (catch ',n
		(return ,(if (= (length ee) 1) (car ee)
			   (cons 'progn ee)))))))))))

(defmacro-slatex defenum (&rest z)
  (do ((z z (cdr z))
       (n 0 (1+ n))
       (r '() (cons `(defvar ,(car z) (code-char ,n)) r)))
      ((null z) `(progn ,@r))))

(defmacro-slatex defrecord (name &rest fields)
  (do ((fields fields (cdr fields))
       (i 0 (1+ i))
       (r '() (cons `(defvar ,(car fields) ,i) r)))
      ((null fields)
       `(progn
	  (defun ,name () (make-array ,i))
	  ,@r))))

(defmacro-slatex of (r i &rest z)
  (cond ((null z) `(elt ,r ,i))
	((and (eq i '/) (= (length z) 1))
	 `(char ,r ,(car z)))
	(t `(of (elt ,r ,i) ,@z))))

(defmacro-slatex eval-if (dialects &rest body)
  (if (member 'cl dialects)
      (if (= (length body) 1) (car body)
	`(progn ,@body))))

(defmacro-slatex eval-unless (dialects &rest body)
  (if (not (member 'cl dialects))
      (if (= (length body) 1) (car body)
	`(progn ,@body))))
