;s4.scm
;SLaTeX v. 2.3
;Making dialect meet R5RS spec
;(includes optimizing for Chez 4.0a+)
;(c) Dorai Sitaram, Rice U., 1991, 1994

(eval-if (chez)
  (eval-when (compile load eval)
    (if (not (bound? 'optimize-level))  ;do only for old Chezs
        (let ((cwif call-with-input-file)
              (cwof call-with-output-file))
          (set! call-with-input-file
            (lambda (f p)
              (cwif f (lambda (pt)
                        (p pt)
                        (close-input-port pt)))))
          (set! call-with-output-file
            (lambda (f p)
              (cwof f (lambda (pt)
                        (p pt)
                        (close-output-port pt)))))))))

(eval-if (chez)
  (if (bound? 'optimize-level) (optimize-level 3)))

(eval-if (cl)
  (eval-within slatex

    (defun member (x s)
      (declare (list s))
      (global-member x s :test (function equal)))

    (defun assoc (x s)
      (declare (list s))
      (global-assoc x s :test (function equal)))

    (defun number->string (n &optional (b 10))
      (declare (number n))
      (write-to-string n :base b))

    (defun string->number (s &optional (b 10))
      (declare (global-string s))
      (let ((*read-base* b))
	(let ((n (read-from-string s)))
	  (if (numberp n) n nil))))

    (defun char-whitespace? (c)
      (declare (character c))
      (or (char= c #\space) (char= c #\tab)
	  (not (graphic-char-p c))))

    (defun make-string (n &optional (c #\space))
      (declare (number n))
      (global-make-string n :initial-element c))

    (defun string (&rest z)
      (concatenate 'global-string z))

    (defun string-append (&rest z)
      (apply (function concatenate) 'global-string z))

    (defun string->list (s)
      (declare (global-string s))
      (concatenate 'list s))

    (defun list->string (l)
      (declare (list l))
      (concatenate 'global-string l))

    (defun make-vector (n &optional x)
      (declare (number n))
      (make-array (list n) :initial-element x))

    (defun vector->list (v)
      (declare (vector v))
      (concatenate 'vector v))

    (defun list->vector (l)
      (declare (list l))
      (concatenate 'vector l))

    (defun call-with-input-file (f p)
      (with-open-file (i f :direction :input)
	(funcall p i)))

    (defun call-with-output-file (f p)
      (with-open-file (o f :direction :output)
	(funcall p o)))

    (defun read (&optional p)
      (global-read p nil :eof-object))

    (defun read-char (&optional p)
      (global-read-char p nil :eof-object))

    (defun peek-char (&optional p)
      (global-peek-char nil p nil :eof-object))

    (defun eof-object? (v)
      (eq v :eof-object))

    ))
