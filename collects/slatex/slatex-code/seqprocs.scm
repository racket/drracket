;seqprocs.scm
;SLaTeX v. 2.3
;Sequence routines
;(c) Dorai Sitaram, Rice U., 1991, 1994

(eval-if (cscheme)
  (eval-within slatex
    (define slatex::some
      (lambda (f l) (there-exists? l f)))))

(eval-unless (chez cl cscheme mzscheme)
  (eval-within slatex
      (define slatex::some
	(lambda (f l)
	  ;returns nonfalse iff f is true of at least one element in l;
	  ;this nonfalse value is that given by the first such element in l;
	  ;only one argument list supported
	  (let loop ((l l))
	    (if (null? l) #f
		(or (f (car l)) (loop (cdr l)))))))))

(eval-within slatex

  (define slatex::ormapcdr
    (lambda (f l)
      ;apply f to successive cdrs of l, returning
      ;immediately when an application is true.
      ;only one argument list supported
      (let loop ((l l))
	(if (null? l) #f
	    (or (funcall f l) (loop (cdr l)))))))

  (define slatex::list-prefix?
    (lambda (pfx l)
      ;tests if list pfx is a prefix of list l
      (cond ((null? pfx) #t)
	    ((null? l) #f)
	    ((eqv? (car pfx) (car l)) (list-prefix? (cdr pfx) (cdr l)))
	    (else #f))))

  (define slatex::string-suffix?
    (lambda (sfx s)
      ;tests if string sfx is a suffix of string s
      (let ((sfx-len (string-length sfx)) (s-len (string-length s)))
	(if (> sfx-len s-len) #f
	    (let loop ((i (- sfx-len 1)) (j (- s-len 1)))
	      (if (< i 0) #t
		  (and (char=? (string-ref sfx i) (string-ref s j))
		       (loop (- i 1) (- j 1)))))))))

  )



(eval-unless (bigloo chez cl cscheme elk guile mzscheme pcsge stk scm)
  (eval-within slatex
    (define slatex::append!
      (lambda (l1 l2)
        ;destructively appends lists l1 and l2;
        ;only two argument lists supported
        (cond ((null? l1) l2)
              ((null? l2) l1)
              (else (let loop ((l1 l1))
                      (if (null? (cdr l1))
                          (set-cdr! l1 l2)
                          (loop (cdr l1))))
                    l1))))))

(eval-unless (cl cscheme)
  (eval-within slatex
      (define slatex::mapcan
	(lambda (f l)
	  ;maps f on l but splices (destructively) the results;
	  ;only one argument list supported
	  (let loop ((l l))
	    (if (null? l) '()
		(append! (f (car l)) (loop (cdr l)))))))))

(eval-unless (bigloo chez cl cscheme elk mzscheme pcsge)
  (eval-within slatex
      (define slatex::reverse!
	(lambda (s)
	  ;reverses list s inplace (i.e., destructively)
	  (let loop ((s s) (r '()))
	    (if (null? s) r
		(let ((d (cdr s)))
		  (set-cdr! s r)
		  (loop d s))))))))

(eval-unless (cl)
  (eval-within slatex

    (define slatex::lassoc
      (lambda (x al eq)
	(let loop ((al al))
	  (if (null? al) #f
	      (let ((c (car al)))
		(if (eq (car c) x) c
		    (loop (cdr al))))))))

    (define slatex::lmember
      (lambda (x l eq)
	(let loop ((l l))
	  (if (null? l) #f
	      (if (eq (car l) x) l
		  (loop (cdr l)))))))

    (define slatex::delete
      (lambda (x l eq)
	(let loop ((l l))
	  (cond ((null? l) l)
		((eq (car l) x) (loop (cdr l)))
		(else (set-cdr! l (loop (cdr l)))
		      l)))))

    (define slatex::adjoin
      (lambda (x l eq)
	(if (lmember x l eq) l
	    (cons x l))))

    (define slatex::delete-if
      (lambda (p s)
	(let loop ((s s))
	  (cond ((null? s) s)
		((p (car s)) (loop (cdr s)))
		(else (set-cdr! s (loop (cdr s)))
		      s)))))

    (define slatex::string-prefix?
      (lambda (s1 s2 i)
	;Tests if s1 and s2 have the same first i chars.
	;Both s1 and s2 must be at least i long.
	(let loop ((j 0))
	  (if (= j i) #t
	      (and (char=? (string-ref s1 j) (string-ref s2 j))
		   (loop (+ j 1)))))))

    (define slatex::sublist
      (lambda (l i f)
	;finds the sublist of l from index i inclusive to index f exclusive
	(let loop ((l (list-tail l i)) (k i) (r '()))
	  (cond ((>= k f) (reverse! r))
		((null? l)
		 (slatex::error "sublist: List too small."))
		(else (loop (cdr l) (+ k 1) (cons (car l) r)))))))

    (define slatex::position-char
      (lambda (c l)
	;finds the leftmost index of character-list l where character c occurs
	(let loop ((l l) (i 0))
	  (cond ((null? l) #f)
		((char=? (car l) c) i)
		(else (loop (cdr l) (+ i 1)))))))

    (define slatex::string-position-right
      (lambda (c s)
	;finds the rightmost index of string s where character c occurs
	(let ((n (string-length s)))
	  (let loop ((i (- n 1)))
	    (cond ((< i 0) #f)
		  ((char=? (string-ref s i) c) i)
		  (else (loop (- i 1))))))))

    ))

(eval-if (cl)
  (eval-within slatex

    (defun lassoc (x l eq)
      (declare (list l))
      (global-assoc x l :test eq))

    (defun lmember (x l eq)
      (declare (list l))
      (global-member x l :test eq))

    (defun delete (x l eq)
      (declare (list l))
      (global-delete x l :test eq))

    (defun adjoin (x l eq)
      (declare (list l))
      (global-adjoin x l :test eq))

    (defun string-prefix? (s1 s2 i)
      (declare (global-string s1 s2) (integer i))
      (string= s1 s2 :end1 i :end2 i))

    (defun string-position-right (c s)
      (declare (character c) (global-string s))
      (position c s :test (function char=) :from-end t))

    ))
