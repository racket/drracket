(define (make-directory* dir)
  (let-values ([(base name dir?) (split-path dir)])
    (when (and (string? base)
	       (not (directory-exists? base)))
      (make-directory* base))
    (unless (make-directory dir)
      (error 'make-directory* "couldn't make directory: ~s" dir))))

(define (append-c-suffix s)
  (string-append s ".c"))

(define (append-constant-pool-suffix s)
  (string-append s ".kp"))

(define (append-object-suffix s)
  (string-append
   s
   (case (system-type)
     [(unix macos) ".o"]
     [(windows) ".obj"])))

(define (append-extension-suffix s)
  (string-append
   s
   (case (system-type)
     [(unix macos) ".so"]
     [(windows) ".dll"])))

(define-values (extract-base-filename/ss
		extract-base-filename/c
		extract-base-filename/kp
		extract-base-filename/o)
  (let ([mk
	 (lambda (pat kind)
	   (letrec ([extract-base-filename
		     (case-lambda
		      [(s p)
		       (let ([m (regexp-match (format "^(.*)\\.(~a)$" pat) s)])
			 (cond
			  [m (cadr m)]
			  [p (error p "not a ~a file: ~a" kind s)]
			  [else #f]))]
		      [(s) (extract-base-filename s #f)])])
	     extract-base-filename))])
    (values
     (mk "ss|scm" "Scheme")
     (mk "c" "C")
     (mk "kp" "constant pool")
     (mk "o|obj" "compiled object"))))
