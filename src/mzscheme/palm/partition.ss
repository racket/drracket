
(require-library "cmdline.ss")

(define to-main-seg #f)
(define fatal-error? #f)

(define files
  (command-line
   "partition.ss"
   argv
   (once-each
    [("--main") file "put a file in the main segment"
		(set! to-main-seg file)])
   (args files
	 files)))

(read-case-sensitive #t)

;;;;;; Load decls

(define source (make-hash-table))

(define limit 100)

(define multiple-ok '(malloc free div))

(for-each
 (lambda (f)
   (with-input-from-file f
     (lambda ()
       (let ([fname (let-values ([(base name dir?) (split-path f)])
		      (let* ([m (regexp-match "^(.*)[.]map$" name)]
			     [n (cadr m)])
			(if (equal? n to-main-seg)
			    #f
			    (if (> (string-length n) 6)
				(substring n 0 6)
				n))))]
	     [cnt 0])
	 (let loop ()
	   (let ([r (read)])
	     (unless (eof-object? r)
	       (case (car r)
		 [(impl simpl) 
		  (set! cnt (add1 cnt))
		  (let ([old-f (hash-table-get source (cadr r) (lambda () #f))])
		    (when old-f
		      (unless (memq (cadr r) multiple-ok)
			(fprintf (current-error-port) "Warning: multiple declarations: ~a, in ~a and ~a~n" 
				 (cadr r) old-f fname))))
		  (when (and (eq? (car r) 'impl)
			     (eq? 'undeclared (hash-table-get source (cadr r) (lambda () 'undeclared)))
			     (not (memq (cadr r) multiple-ok)))
		    (fprintf (current-error-port) "~a: global ~a in ~a was not declared~n" 
			     (if fname "Error" "Warning")
			     (cadr r) fname)
		    (when fname (set! fatal-error? #t)))
		  (hash-table-put! source (cadr r) 
				   (if (or (< cnt limit)
					   (not fname))
				       fname
				       (format "~a~c" fname 
					       (integer->char
						(+ 65 (quotient cnt limit))))))]
		 [(decl) (hash-table-get source (cadr r)
					 (lambda ()
					   (hash-table-put! source (cadr r) #f)
					   #f))]
		 [else (void)])
	       (loop))))))))
 files)

(for-each
 (lambda (l)
   (hash-table-put! source l #f))
 multiple-ok)


(when fatal-error?
  (exit 1))

;;;;;; Output labels

(with-output-to-file "segmap.h"
  (lambda ()
    (hash-table-for-each
     source
     (lambda (k v)
       (printf "#define SEGOF_~a ~a~n" k
	       (if v
		   (format "__attribute__ ((section (~s)))" v)
		   "/* default */")))))
  'truncate)

(with-output-to-file "mz.def"
  (lambda ()
    (printf "app { \"MzScheme\" MzSc }~n")
    (printf "multiple code { ")
    (let ([t (make-hash-table)])
      (hash-table-for-each
       source
       (lambda (k v)
	 (when v
	   (let ([s (string->symbol v)])
	     (hash-table-get t s
			     (lambda ()
			       (hash-table-put! t s #t)
			       (printf "~s " s))))))))
    (printf "}~n"))
  'truncate)
