
;; Utilities for creating a .plt package, relies on gzip and mmencode

(define pack
  (case-lambda
   [(dest name paths collections)
    (pack dest name paths collections std-filter #t 'file)]
   [(dest name paths collections filter)
    (pack dest name paths collections filter #t 'file)]
   [(dest name paths collections filter encode?)
    (pack dest name paths collections filter encode? 'file)]
   [(dest name paths collections filter encode? file-mode)
    (let* ([p (if encode?
		  (process (format "gzip -c | mmencode > ~s" dest))
		  #f)]
	   [stdin (if p 
		      (cadr p)
		      (open-output-file dest 'truncate/replace))]
	   [echo (lambda (p)
		   (thread
		    (lambda ()
		      (let loop ()
			(let ([l (read-line p 'any)])
			  (unless (eof-object? l)
			    (printf "~a~n" l)
			    (loop)))))))]
	   [t1 (and p (echo (car p)))]
	   [t2 (and p (echo (list-ref p 3)))])
      (fprintf stdin "PLT~n")
      (write
       `(lambda (request failure)
	  (case request
	    [(name) ,name]
	    [(unpacker) 'mzscheme]))
       stdin)
      (newline stdin)
      (write
       `(unit 
	 (import plthome mzuntar)
	 (export)
	 (mzuntar void)
	 (quote ,collections))
       stdin)
      (newline stdin)
      (for-each
       (lambda (path)
	 (mztar path stdin filter file-mode))
       paths)
      (close-output-port stdin)
      (when p
	(thread-wait t1)
	(thread-wait t2)))]))

(define (mztar path output filter file-mode)
  (define (path->list p)
    (let-values ([(base name dir?) (split-path p)])
	(if (string? base)
	    (append (path->list base) (list name))
	    (list name))))
  (define-values (init-dir init-files)
    (if (file-exists? path)
	(let-values ([(base name dir?) (split-path path)])
	  (values base (list name)))
	(values path #f)))

  (let loop ([dir init-dir][dpath (path->list init-dir)][files init-files])
    (printf "MzTarring ~a~a...~n" dir
	    (if files (car files) ""))
    (fprintf output "~s~n~s~n" 'dir dpath)
    (for-each
     (lambda (f)
       (let* ([p (build-path dir f)]
	      [filter-val (filter p)])
	 (when filter-val
	   (if (directory-exists? p)
	       (loop p (append dpath (list f)) #f)
	       (let ([len (file-size p)])
		 ; (printf "MzTarring ~a~n" p)
		 (fprintf output "~s~n~s~n~s~n*"
			  (case filter-val
			    [(file) 'file]
			    [(file-replace) 'file-replace]
			    [else file-mode])
			  (append dpath (list f))
			  len)
		 (with-input-from-file p
		   (lambda ()
		     (let loop ()
		       (let ([c (read-char)])
			 (unless (eof-object? c)
			   (write-char c output)
			   (loop)))))))))))
     (or files (directory-list dir)))))

(define (std-filter path)
  (not (or (regexp-match "CVS$" path)
	   (regexp-match "compiled$" path)
	   (regexp-match "~$" path)
	   (regexp-match "^#.*#$" path))))

