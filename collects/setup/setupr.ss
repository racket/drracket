
; Expects parameters to be set before invocation.
; Calls `exit' when done.

(unit/sig ()
  (import setup-option^
	  mzlib:file^
	  compiler^
	  (compiler:option : compiler:option^)
	  launcher-maker^)
  
  (define plthome
    (or (getenv "PLTHOME")
	(let ([dir (collection-path "mzlib")])
	  (and dir
	       (let-values ([(base name dir?) (split-path dir)])
		 (and (string? base)
		      (let-values ([(base name dir?) (split-path base)])
			(and (string? base)
			     (complete-path? base)
			     base))))))))

  (define setup-fprintf
    (lambda (p s . args)
      (apply fprintf p (string-append "setup-plt: " s "~n") args)))

  (define setup-printf
    (lambda (s . args)
      (apply setup-fprintf (current-output-port) s args)))

  (setup-printf "Setup version is ~a" (version))
  (setup-printf "PLT home directory is ~a" plthome)
  (setup-printf "Collection Paths are: ~a" (current-library-collection-paths))

  (exit-handler
   (let ([oh (exit-handler)])
     (lambda (num)
       (let ([error-log (build-path (collection-path "setup") "errors")])
	 (if (zero? num)
	     (when (file-exists? error-log)
	       (delete-file error-log))
	     (call-with-output-file error-log
	       (lambda (port)
		 (show-errors port))
	       'truncate))
	 (oh num)))))

  (define (warning s x)
    (setup-printf s
		  (if (exn? x)
		      (exn-message x)
		      x)))

  (define (pretty-name f)
    (with-handlers ([void (lambda (x) f)])
      (let-values ([(base name dir?) (split-path f)])
	(format "~a in ~a" name base))))

  (define (call-info info flag default test)
    (with-handlers ([void (lambda (x) 
			    (warning
			     (format "Warning: error getting ~a info: ~~a"
				     flag)
			     x)
			    default)])
      (let ([v (info flag (lambda () default))])
	(test v)
	v)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Archive Unpacking              ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (port64->port p)
    (let* ([waiting 0]
	   [waiting-bits 0]
	   [at-eof? #f]
	   [push
	    (lambda (v)
	      (set! waiting (+ (arithmetic-shift waiting 6) v))
	      (set! waiting-bits (+ waiting-bits 6)))])
      (make-input-port
       (lambda ()
	 (let loop ()
	   (if at-eof?
	       eof
	       (if (>= waiting-bits 8)
		   (begin0
		    (integer->char (arithmetic-shift waiting (- 8 waiting-bits)))
		    (set! waiting-bits (- waiting-bits 8))
		    (set! waiting (bitwise-and waiting (sub1 (arithmetic-shift 1 waiting-bits)))))
		   (let* ([c (read-char p)]
			  [n (if (eof-object? c)
				 (#%char->integer #\=)
				 (char->integer c))])
		     (cond
		      [(<= (#%char->integer #\A) n (#%char->integer #\Z)) (push (- n (#%char->integer #\A)))]
		      [(<= (#%char->integer #\a) n (#%char->integer #\z)) (push (+ 26 (- n (#%char->integer #\a))))]
		      [(<= (#%char->integer #\0) n (#%char->integer #\9)) (push (+ 52 (- n (#%char->integer #\0))))]
		      [(= (#%char->integer #\+) n) (push 62)]
		      [(= (#%char->integer #\/) n) (push 63)]
		      [(= (#%char->integer #\=) n) (set! at-eof? #t)])
		     (loop))))))
       (lambda ()
	 (or at-eof? (char-ready? p)))
       void)))

  (define (port64gz->port p64gz)
    (let ([gunzip-through-ports
	   (invoke-unit/sig
	    (compound-unit/sig
	     (import)
	     (link [I : (gunzip-through-ports) ((require-library "inflater.ss"))]
		   [X : () ((unit/sig () (import (gunzip-through-ports)) gunzip-through-ports) I)])
	     (export)))])
      ; Inflate in a thread so the whole input isn't read at once
      (let*-values ([(pgz) (port64->port p64gz)]
		    [(waiting?) #f]
		    [(ready) (make-semaphore)]
		    [(read-pipe write-pipe) (make-pipe)]
		    [(out) (make-output-port
			    (lambda (s)
			      (set! waiting? #t)
			      (semaphore-wait ready)
			      (set! waiting? #f)
			      (display s write-pipe))
			    (lambda ()
			      (close-output-port write-pipe)))]
		    [(get) (make-input-port
			    (lambda ()
			      (if (char-ready? read-pipe)
				  (read-char read-pipe)
				  (begin
				    (semaphore-post ready)
				    (read-char read-pipe))))
			    (lambda ()
			      (or (char-ready? read-pipe) waiting?))
			    (lambda ()
			      (close-input-port read-pipe)))])
	(thread (lambda () 
		  (with-handlers ([void (lambda (x)
					  (warning "Warning: unpacking error: ~a" x))])
		    (gunzip-through-ports pgz out))
		  (close-output-port out)))
	get)))

  (define (unmztar p filter)
    (let loop ()
      (let ([kind (read p)])
	(unless (eof-object? kind)
	  (case kind
	    [(dir) (let ([s (apply build-path (read p))])
		     (unless (relative-path? s)
		       (error "expected a directory name relative path string, got" s))
		     (when (filter 'dir s plthome)
		       (let ([d (build-path plthome s)])
			 (unless (directory-exists? d)
			   (when (verbose)
			     (setup-printf "  making directory ~a" (pretty-name d)))
			   (make-directory* d)))))]
	    [(file file-replace) 
	     (let ([s (apply build-path (read p))])
	       (unless (relative-path? s)
		 (error "expected a file name relative path string, got" s))
	       (let ([len (read p)])
		 (unless (and (number? len) (integer? len))
		   (error "expected a file name size, got" len))
		 (let* ([write? (filter kind s plthome)]
			[path (build-path plthome s)])
		   (let ([out (and write?
				   (if (file-exists? path)
				       (if (eq? kind 'file)
					   #f
					   (open-output-file path 'truncate))
				       (open-output-file path)))])
		     (when (and write? (not out))
		       (setup-printf "  skipping ~a; already exists" (pretty-name path)))
		     (when (and out (or #t (verbose)))
		       (setup-printf "  unpacking ~a" (pretty-name path)))
		     ; Find starting *
		     (let loop ()
		       (let ([c (read-char p)])
			 (cond
			  [(char=? c #\*) (void)] ; found it
			  [(char-whitespace? c) (loop)]
			  [(eof-object? c) (void)] ; signal the error below
			  [else (error 
				 (format
				  "unexpected character setting up ~a, looking for #\*"
				  path)
				 c)])))
		     ; Copy file data
		     (let loop ([n len])
		       (unless (zero? n)
			 (let ([c (read-char p)])
			   (when (eof-object? c)
			     (error (format 
				     "unexpected end-of-file while ~a ~a (at ~a of ~a)"
				     (if out "unpacking" "skipping")
				     path
				     (- len n -1) len)))
			   (when out
			     (write-char c out)))
			 (loop (sub1 n))))
		     (when out
		       (close-output-port out))))))]
	    [else (error "unknown file tag" kind)])
	  (loop)))))

  (define (unpack-archive archive)
    (with-handlers ([void
		     (lambda (x)
		       (warning (format "Warning: error unpacking ~a: ~~a"
					archive)
				x)
		       null)])
      (call-with-input-file archive
	(lambda (p64)
	  (let* ([p (port64gz->port p64)])
	    (unless (and (eq? #\P (read-char p))
			 (eq? #\L (read-char p))
			 (eq? #\T (read-char p)))
	      (error "not an unpackable distribution archive"))
	    (let* ([n (make-namespace)]
		   [info (eval (read p) n)])
	      (unless (and (procedure? info)
			   (procedure-arity-includes? info 2))
		(error "expected a procedure of arity 2, got" info))
	      (let ([name (call-info info 'name #f
				     (lambda (n) 
				       (unless (string? n)
					 (if n
					     (error "couldn't find the package name")
					     (error "expected a string")))))]
		    [unpacker (call-info info 'unpacker #f
					 (lambda (n) 
					   (unless (eq? n 'mzscheme)
					     (error "unpacker isn't mzscheme:" n))))])
		(unless (and name unpacker)
		  (error "bad name or unpacker"))
		(setup-printf "Unpacking ~a from ~a" name archive)
		(let ([u (eval (read p) n)])
		  (unless (unit? u)
		    (error "expected a unit, got" u))
		  (let ([plthome plthome]
			[unmztar (lambda (filter)
				   (unmztar p filter))])
		    (invoke-unit u plthome unmztar))))))))))

  (define x-specific-collections
    (apply 
     append
     (specific-collections)
     (map unpack-archive (archives))))

  (define (done)
    (setup-printf "Done setting up"))

  (unless (null? (archives))
    (when (null? x-specific-collections)
      (done)
      (exit 0))) ; done

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;           Collection Compilation             ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-struct cc (collection path name info))

  (define collection->cc
    (lambda (collection-p)
      (with-handlers ([void (lambda (x) #f)])
	(let ([dir (apply collection-path collection-p)])
	  (with-handlers ([(lambda (x)
			     (and (exn:i/o:filesystem? x)
				  (string=? (exn:i/o:filesystem-pathname x)
					    (build-path dir "info.ss"))))
			   (lambda (x) #f)]
			  [void
			   (lambda (x)
			     (warning "Warning: error loading info.ss: ~a" x)
			     #f)])
	    (let* ([info (parameterize ([use-compiled-file-kinds 'none])
			   (apply require-library/proc "info.ss" collection-p))]
		   [name (call-info info 'name #f
				    (lambda (x)
				      (unless (string? x)
					(error "result is not a string:" x))))])
	      (and
	       name
	       ;(call-info info 'compile-prefix #f #t)
	       (make-cc
		collection-p
		(apply collection-path collection-p)
		name
		info))))))))

  (define (cannot-compile c)
    (error 'setup-plt "don't know how to compile collection: ~a" 
	   (if (= (length c) 1)
	       (car c)
	       c)))

  (define collections-to-compile
    (if (null? x-specific-collections)
	(let ([ht (make-hash-table)])
	  (let loop ([collection-paths (current-library-collection-paths)])
	    (cond
	     [(null? collection-paths) 
	      (hash-table-map ht (lambda (k v) v))]
	     [else (let ([cp (car collection-paths)])
		     (let loop ([collections (if (directory-exists? cp)
						 (directory-list cp)
						 null)])
		       (cond
			[(null? collections) (void)]
			[else (let* ([collection (car collections)]
				     [coll-sym (string->symbol collection)])
				(hash-table-get
				 ht
				 coll-sym
				 (lambda ()
				   (let ([cc (collection->cc (list collection))])
				     (when cc
				       (hash-table-put! 
					ht
					coll-sym
					cc))))))
			      (loop (cdr collections))])))
		   (loop (cdr collection-paths))])))
	(map
	 (lambda (c)
	   (or (collection->cc c)
	       (cannot-compile c)))
	 x-specific-collections)))

  (define control-io-apply
    (lambda (print-doing f args)
      (if (make-verbose)
	  (begin
	    (apply f args)
	    #t)
	  (let* ([oop (current-output-port)]
		 [printed? #f]
		 [on? #f]
		 [op (make-output-port 
		      (lambda (s)
			(let loop ([s s])
			  (if on?
			      (let ([m (regexp-match-positions (string #\newline) s)])
				(if m
				    (begin
				      (set! on? #f)
				      (when (verbose)
					(display (substring s 0 (add1 (caar m))) oop)
					(flush-output oop))
				      (loop (substring s (add1 (caar m)) (string-length s))))
				    (when (verbose)
				      (display s oop)
				      (flush-output oop))))
			      (let ([m (or (regexp-match-positions "making" s)
					   (regexp-match-positions "compiling" s))])
				(when m
				  (unless printed?
				    (set! printed? #t)
				    (print-doing oop))
				  (set! on? #t)
				  (when (verbose)
				    (display "  " oop)) ; indentation 
				  (loop (substring s (caar m) (string-length s))))))))
		      void)])
	    (parameterize ([current-output-port op])
	      (apply f args)
	      printed?)))))

  ; Close over sub-collections
  (set! collections-to-compile
    (let loop ([l collections-to-compile])
      (if (null? l)
	  null
	  (let* ([cc (car l)]
		 [info (cc-info cc)])
	    (append
	     (list cc)
	     (map
	      (lambda (subcol)
		(or
		 (collection->cc subcol)
		 (cannot-compile subcol)))
	      (call-info info 'compile-subcollections null
			 (lambda (x)
			   (unless (and (list? x)
					(andmap
					 (lambda (x)
					   (list? x)
					   (andmap
					    (lambda (x)
					      (and (string? x)
						   (relative-path? x)))
					    x))
					 x))
			     (error "result is not a list of relative path string lists:" x)))))
	     (loop (cdr l)))))))

  (define (delete-files-in-directory path printout)
    (for-each
     (lambda (end-path)
       (let ([path (build-path path end-path)])
	 (cond
	  [(directory-exists? path)
	   (void)]
	  [(file-exists? path)
	   (printout)
	   (unless (delete-file path)
	     (error 'delete-files-in-directory
		    "unable to delete file: ~a" path))]
	  [else (error 'delete-files-in-directory
		       "encountered ~a, neither a file nor a directory"
		       path)])))
     (directory-list path)))

  (define (is-subcollection? collection sub-coll)
    (cond
     [(null? collection) #t]
     [(null? sub-coll) #f]
     [else (and (string=? (car collection) (car sub-coll))
		(is-subcollection? (cdr collection) (cdr sub-coll)))]))

  (define (clean-collection cc)
    (let* ([info (cc-info cc)]
	   [default (box 'default)]
	   [paths (call-info
		   info
		   'clean
		   (list "compiled" (build-path "compiled" "native" (system-library-subpath)))
		   (lambda (x)
		     (unless (or (eq? x default)
				 (and (list? x)
				      (andmap string? x)))
		       (error 'setup-plt "expected a list of strings for 'clean, got: ~s"
			      x))))]
	   [printed? #f]
	   [print-message
	    (lambda ()
	      (unless printed?
		(set! printed? #t)
		(setup-printf "Deleting files for ~a." (cc-name cc))))])
      (for-each (lambda (path)
		  (let ([full-path (build-path (cc-path cc) path)])
		    (cond
		     [(directory-exists? full-path)
		      (delete-files-in-directory
		       full-path
		       print-message)]
		     [(file-exists? full-path)
		      (delete-file full-path)
		      (print-message)]
		     [else (void)])))
		paths)))

  (when (clean)
    (for-each clean-collection collections-to-compile))

  (when (or (make-zo) (make-so))
    (compiler:option:verbose (compiler-verbose))
    (compiler:option:compile-subcollections #f))

  (define errors null)
  (define (record-error cc desc go)
    (with-handlers ([(lambda (x) (not (exn:misc:user-break? x)))
		     (lambda (x)
		       (if (exn? x)
			   (begin
			     (fprintf (current-error-port) "~a~n" (exn-message x))
			     (when (defined? 'print-error-trace)
				   ((global-defined-value 'print-error-trace)
				    (current-error-port)
				    x)))
			   (fprintf (current-error-port) "~s~n" x))
		       (set! errors (cons (list cc desc x) errors)))])
      (go)))
  (define (show-errors port)
    (for-each
     (lambda (e)
       (let ([cc (car e)]
	     [desc (cadr e)]
	     [x (caddr e)])
	 (setup-fprintf port
			" Error during ~a for ~a (~a)"
			desc (cc-name cc) (cc-path cc))
	 (if (exn? x)
	     (setup-fprintf port "  ~a" (exn-message x))
	     (setup-fprintf port "  ~s" x))))
     errors))

  (define (make-it desc compile-collection)
    (for-each (lambda (cc)
		(record-error
		 cc
		 (format "Making ~a" desc)
		 (lambda ()
		   (unless (let ([b (box 1)]) (eq? b ((cc-info cc) 'compile-prefix (lambda () b))))
		     (unless (control-io-apply 
			      (lambda (p) (setup-fprintf p "Making ~a for ~a at ~a" desc (cc-name cc) (cc-path cc)))
			      compile-collection
			      (cc-collection cc))
		       (setup-printf "No need to make ~a for ~a at ~a" desc (cc-name cc) (cc-path cc)))))))
	      collections-to-compile))

  (when (make-zo) (make-it ".zos" compile-collection-zos))
  (when (make-so) (make-it "extension" compile-collection-extension))

  (when (make-launchers)
    (let ([name-list 
	   (lambda (l)
	     (unless (and (list? l)
			  (andmap (lambda (x)
				    (and (string? x)
					 (relative-path? x)))
				  l))
	       (error "result is not a list of relative path strings:" l)))])
      (for-each (lambda (cc)
		  (record-error
		   cc
		   "Launcher Setup"
		   (lambda ()
		     (when (= 1 (length (cc-collection cc)))
		       (let ([info (cc-info cc)])
			 (map
			  (lambda (kind
				   mzscheme-launcher-libraries
				   mzscheme-launcher-names
				   mzscheme-program-launcher-path
				   install-mzscheme-program-launcher)
			    (let ([mzlls (call-info info mzscheme-launcher-libraries null
						    name-list)]
				  [mzlns (call-info info mzscheme-launcher-names null
						    name-list)])
			      (if (= (length mzlls) (length mzlns))
				  (map
				   (lambda (mzll mzln)
				     (let ([p (mzscheme-program-launcher-path mzln)])
				       (unless (file-exists? p)
					 (setup-printf "Installing ~a launcher ~a" kind p)
					 (install-mzscheme-program-launcher 
					  mzll
					  (car (cc-collection cc))
					  mzln))))
				   mzlls mzlns)
				  (setup-printf "Warning: ~a launcher library list ~s doesn't match name list ~s"
						kind mzlls mzlns))))
			  '("MzScheme" "MrEd")
			  '(mzscheme-launcher-libraries mred-launcher-libraries)
			  '(mzscheme-launcher-names mred-launcher-names)
			  (list mzscheme-program-launcher-path mred-program-launcher-path)
			  (list install-mzscheme-program-launcher install-mred-program-launcher)))))))
		collections-to-compile)))

  (when (call-install)
    (for-each (lambda (cc)
		(let/ec k
		  (record-error
		   cc
		   "General Install"
		   (lambda ()
		     (let ([t ((cc-info cc) 'install-collection (lambda () (k #f)))])
		       (unless (and (procedure? t)
				    (procedure-arity-includes? t 1))
			 (error 'setup-plt
				"install-collection: result is not a procedure of arity 1 for ~a"
				(cc-name cc)))
		       (setup-printf "Installing ~a" (cc-name cc))
		       (t plthome))))))
	      collections-to-compile))

  (done)

  (unless (null? errors)
    (setup-printf "")
    (show-errors (current-error-port))
    (when (pause-on-errors)
      (fprintf (current-error-port)
	       "INSTALLATION FAILED.~nPress Enter to continue...~n")
      (read-line))
    (exit 1))

  (exit 0))
