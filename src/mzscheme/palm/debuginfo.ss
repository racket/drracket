
(require-library "cmdline.ss")

(define disasm-file #f)
(define result-file #f)

(define objs
  (command-line
   "debuginfo.ss"
   argv
   (once-each
    [("-d") file "disassembly file"
	    (set! disasm-file file)]
    [("-o") file "result file"
	    (set! result-file file)])
   (args objs
	 objs)))

(unless disasm-file
  (error 'debuginfo.ss "disassembly file not provided"))

(define asms (map (lambda (s) (regexp-replace ".o$" s ".s")) objs))

(define asm-funcs (make-hash-table))
(define disasm-funcs (make-hash-table))

(define re:dot-cmd (regexp (string #\^ #\tab #\[ #\. #\])))
(define re:dot-label (regexp (string #\^ #\[ #\. #\])))
(define re:func (regexp "^([a-zA-Z0-9_]*):"))

(define (parse-asm f)
  (define (finish f p)
    (when f
      (hash-table-put! asm-funcs
		       (string->symbol f)
		       (reverse! p))))

  (with-input-from-file f
    (lambda ()
      (let loop ([name #f][prog null])
	(let ([l (read-line)])
	  (cond
	   [(eof-object? l) (finish name prog)]
	   [(or (regexp-match re:dot-cmd l)
		(regexp-match re:dot-label l))
	    (loop name prog)]
	   [(regexp-match re:func l)
	    => (lambda (m)
		 (finish name prog)
		 (loop (cadr m) null))]
	   [else
	    (loop name (cons l prog))]))))))

(define re:dfunc (regexp "^[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f] <([a-zA-Z0-9_]*)>:"))
(define re:dline (regexp (format "^(    |)[ 0-9a-f][ 0-9a-f][ 0-9a-f][0-9a-f]:~c[0-9a-f]... .... .... ~c(.*)$" #\tab #\tab)))

(define (parse-disasm f)
  (define (finish f p)
    (when f
      (hash-table-put! disasm-funcs
		       (string->symbol f)
		       (reverse! p))))

  (with-input-from-file f
    (lambda ()
      (let loop ([name #f][prog null])
	(let ([l (read-line)])
	  (cond
	   [(eof-object? l) (finish name prog)]
	   [(regexp-match re:dfunc l)
	    => (lambda (m)
		 (finish name prog)
		 (loop (cadr m) null))]
	   [(regexp-match re:dline l)
	    => (lambda (m)
		 (loop name (cons (caddr m) prog)))]
	   [else
	    (loop name prog)]))))))

(for-each parse-asm asms)
(parse-disasm disasm-file)

(define re:segment (regexp "lea __text__([a-z]*)@END[.]w[(]%a5[)],%a0"))
(define re:dsegment (regexp "lea %a5@[(]([0-9-]*)[)],%a0"))

(define re:func (regexp "jsr ([a-zA-Z0-9_]+)[(]%a2[)]"))
(define re:dfunc (regexp "jsr %a2@[(]([0-9-]+)[)]"))

(define segments (make-hash-table))
(define functions (make-hash-table))

(define (expect s re d)
  (let ([m (regexp-match re d)])
    (if m
	(cadr m)
	(begin
	  (printf "Warning: expected ~a match: ~a~n" s d)
	  #f))))

(define (infer a d)
  (cond
   [(regexp-match re:segment a)
    => (lambda (m)
	 (let ([seg (string->symbol (let ([s (cadr m)])
				      (if (string=? s "")
					  "palm"
					  s)))]
	       [delta (expect "segment" re:dsegment d)])
	   (when delta
	     (let ([d (string->number delta)]
		   [e (hash-table-get segments seg (lambda () #f))])
	       (if e
		   (unless (= e d)
		     (printf "Warning: ~s mapped to ~a and ~a~n" seg d e))
		   (hash-table-put! segments seg d))))))]
   [(regexp-match re:func a)
    => (lambda (m)
	 (let ([func (string->symbol (cadr m))]
	       [delta (expect "function" re:dfunc d)])
	   (when delta
	     (let ([d (string->number delta)]
		   [e (hash-table-get functions func (lambda () #f))])
	       (if e
		   (unless (= e d)
		     (printf "Warning: ~s mapped to ~a and ~a~n" func d e))
		   (begin
		     (when (negative? d)
		       (printf "Warning: ~s has negative offset: ~a~n" func d))
		     (hash-table-put! functions func d)))))))]))
	   

(hash-table-for-each
 asm-funcs
 (lambda (f a)
   (let ([d (hash-table-get disasm-funcs f (lambda () #f))])
     (when d
       (if (= (length a) (length d))
	   (for-each
	    (lambda (al dl)
	      (infer al dl))
	    a d)
	   '(printf "mismatch: ~a: ~a ~a~n"
		    f (length a) (length d)))))))

;;----------------------------------------

(when result-file
  (current-output-port (open-output-file result-file 'truncate/replace)))

(printf "(~n")
(hash-table-for-each
 segments
 (lambda (k v)
   (printf " (~a ~a)~n" k v)))
(printf ")~n~n")

(printf "(~n")
(hash-table-for-each
 functions
 (lambda (k v)
   (printf "(~a ~a)~n" k v)))
(printf ")~n")

