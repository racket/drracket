
;; Assumptions:
;;  No calls of the form (f)(...)
;;  For arrays, records, and non-pointers, pass by address only
;;  No gc-triggering code in .h files
;;  No instance vars declared as function pointers without a typedef
;;    for the func ptr type
;;
;; BUGS: doesn't check for pointer comparisons where one of the comparees
;;       is a function call

(define cmd-line (vector->list argv))

(define (filter-false s)
  (if (string=? s "#f")
      #f
      s))

(define cpp (filter-false (car cmd-line)))
(define file-in (cadr cmd-line))
(define file-out (filter-false (caddr cmd-line)))

(require-library "function.ss")
(require-library "errortrace.ss" "errortrace")
(error-print-width 100)

(define cpp-process
  (process (format "~a -DMZ_PRECISE_GC ~a ~a"
		   cpp
		   (if (null? (cdddr cmd-line))
		       ""
		       (cadddr cmd-line))
		   file-in)))
(close-output-port (cadr cpp-process))

(define ctok-process
  (process (build-path (current-load-relative-directory) "ctok")))

(define (mk-error-thread proc)
  (thread (lambda ()
	    (let loop ()
	      (let ([l (read-line (list-ref proc 3) 'any)])
		(unless (eof-object? l)
		  (fprintf (current-error-port) "~a~n" l)
		  (loop)))))))

(define cpp-error-thread (mk-error-thread cpp-process))
(define ctok-error-thread (mk-error-thread ctok-process))

;; cpp output to ctok input:
(thread (lambda ()
	  (let ([s (make-string 4096)])
	    (let loop ()
	      (let ([l (read-string! s (car cpp-process))])
		(unless (eof-object? l)
		  (display (if (< l 4096) (substring s 0 l) s)
			   (cadr ctok-process))
		  (loop))))
	    (close-input-port (car cpp-process))
	    (close-output-port (cadr ctok-process)))))

(define e-raw #f)

(define read-thread
  (thread
   (lambda ()
     (with-handlers ([void (lambda (x)
			     (set! e-raw x))])
       (parameterize ([read-case-sensitive #t])
	 (set! e-raw (read (car ctok-process))))))))

((list-ref cpp-process 4) 'wait)
(thread-wait cpp-error-thread)
(when (eq? ((list-ref cpp-process 4) 'status) 'done-error)
  (error 'xform "cpp failed"))

((list-ref ctok-process 4) 'wait)
(thread-wait ctok-error-thread)
(when (eq? ((list-ref ctok-process 4) 'status) 'done-error)
  (error 'xform "ctok failed"))

(thread-wait read-thread)
(when (exn? e-raw)
  (raise e-raw))

(current-output-port (if file-out
			 (open-output-file file-out 'truncate)
			 (make-output-port void void)))
(let ([eh (error-escape-handler)])
  (error-escape-handler
   (lambda ()
     (close-output-port (current-output-port))
     (current-output-port (current-error-port))
     (when file-out
       '(delete-file file-out))
     (eh))))

(define exit-with-error? #f)

(define (log-error format . args)
  (fprintf (current-error-port) "Error ")
  (apply fprintf (current-error-port) format args)
  (newline (current-error-port))
  (set! exit-with-error? #t))

(define per-block-push? #t)

;; Header:
(printf "#define FUNCCALL(setup, x) (setup, x)~n")
(printf "#define FUNCCALL_EMPTY(x) FUNCCALL(SETUP(0), x)~n")
(printf "#define PREPARE_VAR_STACK(size) void *__gc_var_stack__[size+2]; __gc_var_stack__[0] = GC_variable_stack;~n")
(printf "#define SETUP(x) (GC_variable_stack = __gc_var_stack__, __gc_var_stack__[1] = (void *)x)~n")
(printf "#define PUSH(v, x) (__gc_var_stack__[x+2] = (void *)&(v))~n")
(printf "#define PUSHARRAY(v, l, x) (__gc_var_stack__[x+2] = (void *)0, __gc_var_stack__[x+3] = (void *)&(v), __gc_var_stack__[x+4] = (void *)l)~n")
(printf "#define BLOCK_SETUP(x) ~a~n" (if per-block-push? "x" "/* skipped */"))
(printf "~n")

;; C++ cupport:
(printf "#define NEW_OBJ(t) (new t,  (t *)GC_pop_current_new())~n")
(printf "#define NEW_OBJECT(t, args) (new t args,  (t *)GC_pop_current_new())~n")
(printf "#define NEW_ARRAY(t, array) (new t array)~n")
(printf "#define NEW_ATOM(t) (new (AtomicGC) t)~n")
(printf "#define NEW_ATOM_ARRAY(t, array) (new (AtomicGC) t array)~n")
(printf "#define DELETE(x) (delete x)~n")
(printf "#define DELETE_ARRAY(x) (delete[] x)~n")
(printf "#define CURRENT_NEW_THIS GC_get_current_new()~n")
(printf "~n")

(define-struct tok (n line col file))
(define-struct (seq struct:tok) (close in))
(define-struct (parens struct:seq) ())
(define-struct (brackets struct:seq) ())
(define-struct (braces struct:seq) ())
(define-struct (call struct:tok) (func args live tag))
(define-struct (block-push struct:tok) (vars tag super-tag))
(define-struct (note struct:tok) (s))

(define-struct vtype ())
(define-struct (pointer-type struct:vtype) (base stars))
(define-struct (array-type struct:vtype) (count))
(define-struct (struct-type struct:vtype) (struct))
(define-struct (struct-array-type struct:struct-type) (count))
(define-struct (union-type struct:vtype) ())
(define-struct (non-pointer-type struct:vtype) (base))

(define-struct live-var-info (tag maxlive maxpush vars new-vars pushed-vars num-calls))

(define-struct prototype (type static? pointer? pointer?-determined?))

(define-struct c++-class (parent prototyped top-vars))

(define c++-classes null)

(define label? #t)

(define semi '|;|)
(define START_XFORM_SKIP (string->symbol "START_XFORM_SKIP"))
(define END_XFORM_SKIP (string->symbol "END_XFORM_SKIP"))
(define Scheme_Object (string->symbol "Scheme_Object"))
(define sElF (string->symbol "sElF"))
(define gcMark (string->symbol "gcMark"))
(define gcMARK_TYPED (string->symbol "gcMARK_TYPED"))
(define Mark_Proc (string->symbol "Mark_Proc"))
(define gcBYTES_TO_WORDS (string->symbol "gcBYTES_TO_WORDS"))
(define NEW_OBJ (string->symbol "NEW_OBJ"))
(define NEW_OBJECT (string->symbol "NEW_OBJECT"))
(define NEW_ARRAY (string->symbol "NEW_ARRAY"))
(define NEW_ATOM (string->symbol "NEW_ATOM"))
(define NEW_ATOM_ARRAY (string->symbol "NEW_ATOM_ARRAY"))
(define DELETE (string->symbol "DELETE"))
(define DELETE_ARRAY (string->symbol "DELETE_ARRAY"))
(define CURRENT_NEW_THIS (string->symbol "CURRENT_NEW_THIS"))

(define non-functions
  '(<= < > >= == != !
       \| \|\| & && : ? % + - * / ^ >> << 
       = >>= <<= ^= += *= /= -= %= \|= &= ++ --
       return sizeof if for while else switch case
       asm __asm __asm__ __volatile __volatile__ volatile __extension__
       __typeof
       
       ;; The following are functions, but they don't trigger GC, and
       ;; they either take one argument or no pointer arguments.
       ;; So we can ignore them:

       strlen cos sin exp pow log sqrt atan2 isnan isinf
       floor ceil round fmod fabs __maskrune
       isalpha isdigit isspace tolower toupper
       fread fwrite socket fcntl setsockopt connect send recv close
       __builtin_next_arg __error __errno_location __toupper __tolower
       scheme_get_env
       scheme_get_milliseconds scheme_get_process_milliseconds
       scheme_rational_to_double scheme_bignum_to_double
       scheme_rational_to_float scheme_bignum_to_float

       scheme_make_small_bignum scheme_make_small_rational scheme_make_small_complex))

(define non-gcing-functions
  ;; The following don't need wrappers, but we need to check for
  ;;  nested function calls because it takes more than one argument:
  '(memcpy
    strcmp strcpy strcat memset
    printf sprintf vsprintf vprintf
    strncmp scheme_strncmp
    read write))

(define non-returning-functions
  ;; The following functions never return, so the wrappers
  ;; don't need to push any variables:
  '(exit
    scheme_wrong_type scheme_wrong_number scheme_wrong_syntax
    scheme_raise_exn scheme_signal_error
    scheme_raise_out_of_memory
    ))

(define (get-constructor v)
  (cond
   [(parens? v) make-parens]
   [(brackets? v) make-brackets]
   [(braces? v) make-braces]))

(define (get-variable-size vtype)
  (cond
   [(array-type? vtype)
    3]
   [(struct-type? vtype)
    (let ([size (let ([m (assq (struct-type-struct vtype) struct-defs)])
		  (apply + (map get-variable-size
				(cdr m))))])
      (if (struct-array-type? vtype)
	  (* size (struct-array-type-count vtype))
	  size))]
   [else 1]))

(define (replace-live-vars live-vars new-live-vars)
  (make-live-var-info (live-var-info-tag live-vars)
		      (live-var-info-maxlive live-vars)
		      (live-var-info-maxpush live-vars)
		      new-live-vars
		      (live-var-info-new-vars live-vars)
		      (live-var-info-pushed-vars live-vars)
		      (live-var-info-num-calls live-vars)))

(define gentag
  (let ([count 0])
    (lambda ()
      (set! count (add1 count))
      (format "XfOrM~a" count))))

(define next-indent #f)

(define (newline/indent i)
  (newline)
  (set! next-indent i))

(define (display/indent v s)
  (when next-indent
    ;; can't get pre-processor line directive to work
    '(when (and v (tok-file v) (tok-line v))
      (printf "# ~a ~s~n" (max 1 (- (tok-line v) 1)) (tok-file v)))
    (display (make-string next-indent #\space))
    (set! next-indent #f))
  (display s))

(define re:quote-or-backslash (regexp "[\\\"]"))

(define (push-vars l plus comma)
  (let loop ([l l][n 0][comma comma])
    (unless (null? l)
      (loop (cdr l)
	    (let push-var ([full-name (caar l)][vtype (cdar l)][n n][comma comma])
	      (cond
	       [(union-type? vtype)
		(log-error "[UNION]: Can't push union onto mark stack: ~a." full-name)
		(printf "~aPUSHUNION(~a, ~a~a)" comma full-name n plus)
		(add1 n)]
	       [(array-type? vtype)
		(printf "~aPUSHARRAY(~a, ~a, ~a~a)" comma full-name (array-type-count vtype) n plus)
		(+ 3 n)]
	       [(struct-type? vtype)
		(let aloop ([array-index 0][n n][comma comma])
		  ;; Push each struct in array (or only struct if not an array)
		  (let loop ([n n][l (cdr (assq (struct-type-struct vtype) struct-defs))][comma comma])
		    (if (null? l)
			(if (and (struct-array-type? vtype)
				 (< (add1 array-index) (struct-array-type-count vtype)))
			    ;; Next in array
			    (aloop (add1 array-index) n comma)
			    ;; All done
			    n)
			(loop (push-var (format "~a~a.~a"
						full-name 
						(if (struct-array-type? vtype)
						    (format "[~a]" array-index)
						    "")
						(caar l))
					(cdar l)
					n
					comma)
			      (cdr l)
			      ", "))))]
	       [else
		(printf "~aPUSH(~a, ~a~a)" comma full-name n plus)
		(+ n 1)]))
	    ", "))))

(define (total-push-size vars)
  (apply + (map (lambda (x)
		  (get-variable-size (cdr x)))
		vars)))

(define (print-it e indent semi-newlines?)
  (let loop ([e e][prev #f])
    (unless (null? e)
      (let ([v (car e)])
	(cond
	 [(seq? v)
	  (display/indent v (tok-n v))
	  (let ([subindent (if (braces? v)
			       (begin
				 (newline/indent (+ indent 2))
				 (+ indent 2))
			       indent)])
	    (print-it (seq-in v) subindent
		      (not (and (parens? v)
				prev
				(memq (tok-n prev) '(for)))))
	    (when (and next-indent (= next-indent subindent))
	      (set! next-indent indent)))
	  (display/indent #f (seq-close v))
	  (cond
	   [(braces? v)
	    (newline/indent indent)]
	   [(brackets? v)
	    (display/indent v " ")]
	   [(parens? v)
	    (if (and prev 
		     (memq (tok-n prev) '(if))
		     (or (null? (cdr e))
			 (not (braces? (cadr e)))))
		(newline/indent (+ indent 2))
		(display/indent v " "))]
	   [else (error 'xform "unknown brace: ~a" (caar v))])]
	 [(note? v)
	  (display/indent v (note-s v))
	  (newline/indent indent)]
	 [(call? v)
	  (if (null? (call-live v))
	      (display/indent v "FUNCCALL_EMPTY(")
	      (begin
		(display/indent v (format "FUNCCALL(SETUP_~a((SETUP(~a)" 
					  (call-tag v)
					  (total-push-size (call-live v))))
		(push-vars (call-live v) "" ", ")
		(display/indent v ")), ")))
	  (print-it (append (call-func v) (list (call-args v))) indent #f)
	  (display/indent v ")")]
	 [(block-push? v)
	  (let ([size (total-push-size (block-push-vars v))]
		[prev-add (if (block-push-super-tag v)
			      (format "+~a_COUNT" (block-push-super-tag v))
			      "")]
		[tag (block-push-tag v)]
		[tabbing (if (zero? indent)
			     ""
			     (make-string (sub1 indent) #\space))])
	    (unless (zero? size)
	      (display/indent v "BLOCK_SETUP((")
	      (push-vars (block-push-vars v) prev-add "")
	      (display/indent v "));")
	      (newline))
	    (printf "#~adefine ~a_COUNT (~a~a)~n" tabbing tag size prev-add)
	    (printf "#~adefine SETUP_~a(x) " tabbing tag)
	    (cond
	     [(and (zero? size) (block-push-super-tag v)) 
	      (printf "SETUP_~a(x)" (block-push-super-tag v))]
	     [per-block-push? (printf "SETUP(~a_COUNT)" tag)]
	     [else (printf "x")])
	    (newline/indent indent))]
	 [else
	  (if (string? (tok-n v))
	      (begin
		(display/indent v "\"")
		(display (tok-n v))
		(display/indent v "\""))
	      (display/indent v (tok-n v)))
	  (display/indent v " ")
	  (when (and (eq? semi (tok-n v))
		     semi-newlines?)
	    (newline/indent indent))])
	(loop (cdr e) v)))))

(define skipping? #f)

(define (top-level e where)
  (cond
   [(end-skip? e)
    (set! skipping? #f)
    null]
   [(start-skip? e)
    (set! skipping? #t)
    null]
   [skipping?
    e]
   [(access-modifier? e)
    (list* (car e) (cadr e) (top-level (cddr e) where))]
   [(friend? e)
    e]
   [(prototype? e) 
    (let ([name (register-proto-information e)])
      (when label? (printf "/* PROTO ~a */~n" name)))
    e]
   [(typedef? e)
    (when label? (printf "/* TYPEDEF */~n"))
    (check-pointer-type e)
    e]
   [(struct-decl? e)
    (if (braces? (caddr e))
	(begin
	  (register-struct e)
	  (when label? (printf "/* STRUCT ~a */~n" (tok-n (cadr e)))))
	(when label? (printf "/* STRUCT DECL */~n")))
    e]
   [(class-decl? e)
    (if (or (braces? (caddr e))
	    (eq? ': (tok-n (caddr e))))
	(begin
	  (when label? (printf "/* CLASS ~a */~n" (tok-n (cadr e))))
	  (register-class e))
	(begin
	  (when label? (printf "/* CLASS DECL */~n"))
	  e))]
   [(function? e)
    (let ([name (register-proto-information e)])
      (when label? (printf "/* FUNCTION ~a */~n" name)))
    (if (and where (regexp-match "[.]h$" where))
	;; Still in headers; probably an inlined function
	e
	(convert-function e))]
   [(var-decl? e)
    (when label? (printf "/* VAR */~n"))
    (unless (eq? (tok-n (car e)) 'static)
      (let-values ([(pointers non-pointers) (get-vars e "TOPVAR" #f)])
	(top-vars (append pointers non-pointers (top-vars)))))
    e]

   [(and (>= (length e) 3)
	 (eq? (tok-n (car e)) 'extern)
	 (equal? (tok-n (cadr e)) "C")
	 (braces? (caddr e)))
    (list* (car e)
	   (cadr e)
	   (let ([body-v (caddr e)])
	     (make-braces
	      (tok-n body-v)
	      (tok-line body-v)
	      (tok-col body-v)
	      (tok-file body-v)
	      (seq-close body-v)
	      (process-top-level (seq-in body-v))))
	   (cdddr e))]
   
   [else (print-struct #t)
	 (error 'xform "unknown form: ~s" e)]))

(define (start-skip? e)
  (and (pair? e)
       (eq? START_XFORM_SKIP (tok-n (car e)))))

(define (end-skip? e)
  (and (pair? e)
       (eq? END_XFORM_SKIP (tok-n (car e)))))

(define (access-modifier? e)
  (and (memq (tok-n (car e)) '(public private protected))
       (eq? (tok-n (cadr e)) ':)))

(define (friend? e)
  (memq (tok-n (car e)) '(friend)))

(define (prototype? e)
  (let ([l (length e)])
    (and (> l 2)
	 ; Ends in semicolon
	 (eq? semi (tok-n (list-ref e (sub1 l))))
	 ; next-to-last is parens
	 (let ([v (list-ref e (- l 2))])
	   (and (parens? v)))
	 ; Symbol before parens
	 (symbol? (tok-n (list-ref e (- l 3)))))))

(define (typedef? e)
  (eq? 'typedef (tok-n (car e))))

(define (struct-decl? e)
  (memq (tok-n (car e)) '(struct enum)))

(define (class-decl? e)
  (memq (tok-n (car e)) '(class)))

(define (function? e)
  (let ([l (length e)])
    (and (> l 2)
	 (let* ([_n (tok-n (list-ref e (sub1 l)))]
		[ll (if (eq? _n semi)
			(- l 2)
			(sub1 l))])
	   (let ([v (list-ref e ll)])
	     (and (braces? v)
		  (let ([v (list-ref e (sub1 ll))])
		    (and (parens? v)))))))))

(define (var-decl? e)
  (let ([l (length e)])
    (and (> l 2)
	 (eq? semi (tok-n (list-ref e (sub1 l)))))))

(define prototyped (make-parameter null))
(define top-vars (make-parameter null))

(define (register-proto-information e)
  (let loop ([e e][type null])
    (if (parens? (cadr e))
	(let ([name (tok-n (car e))]
	      [type (let loop ([t (reverse type)])
		      (if (and (pair? t)
			       (memq (tok-n (car t)) '(extern static inline virtual)))
			  (loop (cdr t))
			  t))]
	      [static? (ormap (lambda (t) (eq? (tok-n t) 'static)) type)])
	  (unless (assq name (prototyped))
	    (prototyped (cons (cons name (make-prototype type static? #f #f))
			      (prototyped))))
	  name)
	(loop (cdr e) (cons (car e) type)))))

(define (prototype-for-pointer? m)
  (let ([name (car m)]
	[proto (cdr m)])
    (unless (prototype-pointer?-determined? proto)
      (let ([e (append (prototype-type proto)
		       (list (make-tok name #f #f #f)
			     (make-tok semi #f #f #f)))])
	(let ([vars (get-pointer-vars e "PROTODEF" #f)])
	  (set-prototype-pointer?! proto (not (null? vars)))
	  (set-prototype-pointer?-determined?! proto #t))))
    (prototype-pointer? proto)))

(define pointer-types '())
(define non-pointer-types '(int char long unsigned ulong uint void float double))
(define struct-defs '())

(define (check-pointer-type e)
  (let-values ([(pointers non-pointers)
		(get-vars (cdr e) "PTRDEF" #t)])
    (set! pointer-types (append pointers pointer-types))
    (set! non-pointer-types (append (map car non-pointers) non-pointer-types))))

(define (get-vars e comment union-ok?)
  (let* ([e (filter (lambda (x) (not (eq? 'volatile (tok-n x)))) e)]
	 [base (tok-n (car e))]
	 [base-is-ptr?
	  (assq base pointer-types)]
	 [base-struct
	  (and (eq? base 'struct)
	       (if (or (braces? (cadr e)) (braces? (caddr e)))
		   (register-struct e)
		   (let ([m (assq (tok-n (cadr e)) struct-defs)])
		     (and m (car m)))))]
	 [minpos (if (or (eq? base 'struct)
			 (eq? base 'union))
		     1
		     0)]
	 [non-ptr-base (case (tok-n (car e))
			 [(int long short double float char) (list (tok-n (car e)))]
			 [(unsigned)
			  (if (memq (tok-n (cadr e)) '(int long char))
			      (list 'unsigned (tok-n (cadr e))))]
			 [else #f])])
    (let loop ([l (- (length e) 2)][array-size #f][pointers null][non-pointers null])
      (if (<= l minpos)
	  (values pointers non-pointers)
	  ;; Look back for "=" before comma:
	  (let ([skip (let loop ([l (sub1 l)])
			(cond
			 [(or (<= l minpos) 
			      (eq? '|,| (tok-n (list-ref e l))))
			  #f]
			 [(eq? '= (tok-n (list-ref e l)))
			  (sub1 l)]
			 [else (loop (sub1 l))]))])
	    (if skip
		;; Skip assignment RHS:
		(loop skip #f pointers non-pointers)
		;; Not assignment RHS:
		(let ([v (list-ref e l)])
		  (cond
		   [(seq? v)
		    ;; Array? Struct?
		    (cond
		     [(brackets? v)
		      ;; Array decl:
		      (loop (sub1 l)
			    (let ([inner (seq-in (list-ref e l))])
			      (if (null? inner)
				  'pointer
				  (tok-n (car inner))))
			    pointers non-pointers)]
		     [(braces? v) 
		      ;; No more variable declarations
		      (values pointers non-pointers)]
		     [else
		      ;; End of function ptr
		      ;; (and we don't care about func ptrs)
		      (values pointers non-pointers)])]
		   [(memq (tok-n v) '(int long char unsigned void ulong uint))
		    ;; No more variable declarations
		    (values pointers non-pointers)]
		   [(memq (tok-n v) '(|,| * : 1))
		    (loop (sub1 l) #f pointers non-pointers)]
		   [else (let* ([name (tok-n v)]
				[pointer? (or (eq? 'pointer array-size)
					      (eq? '* (tok-n (list-ref e (sub1 l)))))]
				[star-count (+ (if (eq? 'pointer array-size)
						   1 
						   0)
					       (let loop ([l (sub1 l)])
						 (if (eq? '* (tok-n (list-ref e l)))
						     (add1 (loop (sub1 l)))
						     0)))]
				[base-struct (or base-struct
						 (and base-is-ptr?
						      (struct-type? (cdr base-is-ptr?))
						      (struct-type-struct (cdr base-is-ptr?))))]
				[union? (eq? base 'union)]
				[struct-array? (and base-struct (not pointer?) (number? array-size))])
			   (when (and struct-array?
				      (> array-size 5))
			     (log-error "[SIZE] ~a in ~a: Large array of structures at ~a."
					(tok-line v) (tok-file v) name))
			   (when (and (not union-ok?)
				      (not pointer?)
				      (or union?
					  (and base-struct
					       (let has-union? ([base base-struct])
						 (let ([v (cdr (assq base struct-defs))])
						   (ormap
						    (lambda (v)
						      (or (union-type? v)
							  (and (struct-type? v)
							       (has-union? (struct-type-struct v)))))
						    v))))))
			     (fprintf (current-error-port)
				      "Warning [UNION] ~a in ~a: Can't handle union or record with union, ~a.~n"
				      (tok-line v) (tok-file v) name))
			   (if (and (or pointer?
					base-is-ptr?
					base-struct
					union?)
				    ; Ignore these variables, for one reason or another:
				    (not (memq name '(tcp_connect_dest_addr tcp_listen_addr
						      tcp_accept_addr))))
			       (begin
				 (when label?
				   (printf "/* ~a: ~a ~a*/~n" 
					   comment name
					   (cond
					    [struct-array?
					     (format "struct ~a[~a] " base-struct array-size)]
					    [(number? array-size)
					     (format "[~a] " array-size)]
					    [(and base-struct (not pointer?))
					     (format "struct ~a " base-struct)]
					    [(and union? (not pointer?)) "union "]
					    [else (format "~a ~a* " (or (and base (list base))
									non-ptr-base)
							  star-count)])))
				 (loop (sub1 l) #f 
				       (cons (cons name
						   (cond
						    [struct-array?
						     (make-struct-array-type base-struct array-size)]
						    [(number? array-size)
						     (make-array-type array-size)]
						    [pointer? (make-pointer-type (or (and base (list base))
										     non-ptr-base)
										 star-count)]
						    [base-struct
						     (make-struct-type base-struct)]
						    [union?
						     (make-union-type)]
						    [else
						     (make-pointer-type (or (and base (list base))
									    non-ptr-base)
									star-count)]))
					     pointers)
				       non-pointers))
			       (begin
				 (when label?
				   (printf "/* NP ~a: ~a */~n" 
					   comment name))
				 (loop (sub1 l) #f pointers (cons (cons name 
									(make-non-pointer-type non-ptr-base)) 
								  non-pointers)))))]))))))))

(define (get-pointer-vars e comment union-ok?)
  (let-values ([(pointers non-pointers)
		(get-vars e comment union-ok?)])
    pointers))

(define (register-struct e)
  (let ([body (seq-in (if (braces? (cadr e))
			  (cadr e)
			  (caddr e)))]
	[name (if (braces? (cadr e))
		  (gensym 'Anonymous)
		  (tok-n (cadr e)))])
    (let ([l (let ([el (body->lines body #f)])
	       (apply
		append
		(map (lambda (e)
		       (get-pointer-vars e "PTRFIELD" #t))
		     el)))])
      (and (not (null? l))
	   (begin
	     (set! struct-defs (cons (cons name l) struct-defs))
	     name)))))

(define (body->lines e comma-sep?)
  (reverse!
   (foldl-statement
    e
    comma-sep?
    (lambda (sube l)
      (cons sube l))
    null)))

(define (type->decl x)
  (cond
   [(non-pointer-type? x)
    (map (lambda (x) (make-tok x #f #f #f)) (non-pointer-type-base x))]
   [(and (pointer-type? x) (pointer-type-base x))
    (append (map (lambda (x) (make-tok x #f #f #f)) (pointer-type-base x))
	    (let loop ([n (pointer-type-stars x)])
	      (if (zero? n)
		  null
		  (cons (make-tok '* #f #f #f) (loop (sub1 n))))))]
   [else (log-error "[TYPE]: Can't render type declaration for ~a"
		    x)
	 (list (make-tok '??? #f #f #f))]))
   

(define (register-class e)
  (let ([name (tok-n (cadr e))]
	[body-pos (if (eq? ': (tok-n (caddr e)))
		      (if (memq (tok-n (cadddr e)) '(public private))
			  5
			  4)
		      2)])
    (unless (braces? (list-ref e body-pos))
      (error 'xform "Confused by form of class declaration at line ~a in ~a"
	     (tok-line (car e))
	     (tok-file (car e))))
    (let* ([super (if (> body-pos 2)
		      (tok-n (list-ref e (sub1 body-pos)))
		      #f)]
	   [cl (make-c++-class super
			       null
			       null)]
	   [pt (prototyped)]
	   [vs (top-vars)])
      (set! c++-classes (cons (cons name cl) c++-classes))
      (prototyped null)
      (top-vars null)
      (let* ([body-v (list-ref e body-pos)]
	     [body-e (process-top-level (seq-in body-v))])
	;; Save prototype list, but remove constructor and statics:
	(set-c++-class-prototyped! cl (filter (lambda (x)
						(not (or (eq? (car x) name)
							 (prototype-static? (cdr x)))))
					      (prototyped)))
	(set-c++-class-top-vars! cl (top-vars))
	(prototyped pt)
	(top-vars vs)
	(let loop ([e e][p body-pos])
	  (if (zero? p)
	      (append
	       (if (or super (eq? name 'gc_marking))
		   null
		   (list
		    (make-tok ': #f #f #f)
		    (make-tok 'public #f #f #f)
		    (make-tok 'gc_marking #f #f #f)))
	       (cons (make-braces
		      (tok-n body-v)
		      (tok-line body-v)
		      (tok-col body-v)
		      (tok-file body-v)
		      (seq-close body-v)
		      (append
		       body-e
		       (if (eq? name 'gc_marking)
			   null
			   (list
			    (make-tok 'public #f #f #f)
			    (make-tok ': #f #f #f)
			    (make-tok 'int #f #f #f)
			    (make-tok gcMark #f #f #f)
			    (make-parens
			     "(" #f #f #f ")"
			     (list (make-tok Mark_Proc #f #f #f)
				   (make-tok 'mark #f #f #f)))
			    (make-braces
			     "{" #f #f #f "}"
			     (make-mark-body (or super 'gc_marking)
					     (c++-class-top-vars cl)))))))
		     (cdr e)))
	      (cons (car e) (loop (cdr e) (sub1 p)))))))))

(define (make-mark-body super vars)
  (let ([pointers (filter (lambda (x)
			    (not (non-pointer-type? (cdr x))))
			  vars)])
    (append
     (list
      (make-tok super #f #f #f)
      (make-tok ':: #f #f #f)
      (make-tok gcMark #f #f #f)
      (make-parens
       "(" #f #f #f ")"
       (list (make-tok 'mark #f #f #f)))
      (make-tok semi #f #f #f))
     (if (null? pointers)
	 null
	 (list
	  (make-tok 'if #f #f #f)
	  (make-parens
	   "(" #f #f #f ")"
	   (list (make-tok 'mark #f #f #f)))
	  (make-braces
	   "{" #f #f #f "}"
	   (apply
	    append
	    (map (lambda (x)
		   (list
		    (make-tok gcMARK_TYPED #f #f #f)
		    (make-parens
		     "(" #f #f #f ")"
		     (append
		      (type->decl (cdr x))
		      (list (make-tok '|,| #f #f #f)
			    (make-tok (car x) #f #f #f))))
		    (make-tok semi #f #f #f)))
		 pointers)))))
     (list
      (make-tok 'return #f #f #f)
      (make-tok gcBYTES_TO_WORDS #f #f #f)
      (make-parens
       "(" #f #f #f ")"
       (list
	(make-tok 'sizeof #f #f #f)
	(make-parens
	 "(" #f #f #f ")"
	 (list (make-tok '*this #f #f #f)))))
      (make-tok semi #f #f #f)))))

(define (find-c++-class class-name report-err?)
  (and class-name
       (let ([m (assoc class-name c++-classes)])
	 (if m
	     (cdr m)
	     (begin
	       (when report-err?
		 (log-error "[CLASS]: Unknown class ~a."
			    class-name))
	       #f)))))

(define (get-c++-class-member var c++-class c++-class-members)
  (and c++-class
       (let ([m (assoc var (c++-class-members c++-class))])
	 (or m
	     (let ([parent (c++-class-parent c++-class)])
	       (and parent
		    (if (c++-class? parent)
			(get-c++-class-member var parent c++-class-members)
			(let ([parent (find-c++-class parent #t)])
			  (set-c++-class-parent! c++-class parent)
			  (get-c++-class-member var parent c++-class-members)))))))))

(define (get-c++-class-var var c++-class)
  (get-c++-class-member var c++-class c++-class-top-vars))

(define (get-c++-class-method var c++-class)
  (get-c++-class-member var c++-class c++-class-prototyped))

(define used-self? #f)

(define (convert-function e)
  (let*-values ([(body-v len) (let* ([len (sub1 (length e))]
				     [v (list-ref e len)])
				;; Function may have trailing semicolon:
				(if (eq? semi (tok-n v))
				    (values (list-ref e (sub1 len)) (sub1 len))
				    (values v len)))]
		[(body-e) (seq-in body-v)]
		[(args-e) (seq-in (list-ref e (sub1 len)))]
		[(arg-vars all-arg-vars) 
		 (let ([arg-decls (body->lines
				   (append
				    (map 
				     (lambda (v) (if (eq? '|,| (tok-n v))
						     (make-tok semi (tok-line v) (tok-col v) (tok-file v))
						     v))
				     args-e)
				    (list (make-tok semi #f #f #f)))
				   #f)])
		   (let loop ([l arg-decls][arg-vars null][all-arg-vars null])
		     (if (null? l)
			 (values arg-vars all-arg-vars) 
			 (let-values ([(ptrs non-ptrs) (get-vars (car l) "PTRARG" #f)])
			   (loop (cdr l) (append arg-vars ptrs) (append all-arg-vars ptrs non-ptrs))))))]
		[(class-name function-name) 
		 (let loop ([e e])
		   (cond
		    [(null? e) (values #f #f)]
		    [(null? (cdr e)) (values #f #f)]
		    [(eq? ':: (tok-n (cadr e)))
		     (values (tok-n (car e))
			     (tok-n (caddr e)))]
		    [else (loop (cdr e))]))]
		[(c++-class) (let ([c++-class (find-c++-class class-name #t)])
			       (and c++-class
				    (or (get-c++-class-method function-name c++-class)
					(eq? function-name class-name)
					(eq? function-name '~))
				    c++-class))])
     (append
      (let loop ([e e][len len])
	(if (zero? len)
	    null
	    (cons (car e) (loop (cdr e) (sub1 len)))))
      (list
       (make-braces
	(tok-n body-v)
	(tok-line body-v)
	(tok-col body-v)
	(tok-file body-v)
	(seq-close body-v)
	(let-values ([(body-e live-vars)
		      (convert-body (if c++-class
					(let ([e (begin
						   (set! used-self? #f)
						   (convert-class-vars body-e all-arg-vars c++-class))])
					  (append
					   (if used-self?
					       (list
						(make-tok class-name #f #f #f)
						(make-tok '* #f #f #f)
						(make-tok sElF #f #f #f)
						(make-tok '= #f #f #f)
						(if (eq? class-name function-name)
						    (make-parens
						     "(" #f #f #f")"
						     (list
						      (make-parens
						       "(" #f #f #f")"
						       (list (make-tok class-name #f #f #f)
							     (make-tok '* #f #f #f)))
						      (make-tok CURRENT_NEW_THIS #f #f #f)))
						    (make-tok 'this #f #f #f))
						(make-tok semi #f #f #f))
					       null)
					   e))
					body-e)
				    arg-vars arg-vars 
				    c++-class
				    (make-live-var-info #f -1 0 null null null 0) #t)])
	  body-e))))))

(define (convert-class-vars body-e arg-vars c++-class)
  (let ([el (body->lines body-e #f)])
    (let-values ([(decls body) (split-decls el)])
      (for-each (lambda (e) 
		  (let-values ([(pointers non-pointers) (get-vars e "CVTLOCAL" #f)])
		    (for-each
		     (lambda (var)
		       (when (get-c++-class-var (car var) c++-class)
			 (log-error "[SHADOW++] ~a in ~a: Class variable ~a shadowed in decls."
				    (tok-line (caar decls)) (tok-file (caar decls))
				    (car var))))
		     (append! pointers non-pointers))))
		decls))
    (let loop ([e body-e][can-convert? #t][paren-arrows? #t])
      (cond
       [(null? e) null]
       [(and can-convert?
	     (pair? (cdr e))
	     (eq? (tok-n (cadr e)) '::)
	     (find-c++-class (tok-n (car e)) #f))
	;; Maybe class-qualified method invocation. See
	;;  what happens if we remove the qualification
	(let ([rest (loop (cddr e) #t paren-arrows?)])
	  (if (eq? sElF (tok-n (car rest)))
	      (list* (car rest)
		     (cadr rest)
		     (car e)
		     (cadr e)
		     (cddr rest))
	      (list* (car e)
		     (cadr e)
		     rest)))]
       [else
	(let ([v (car e)])
	  (cond
	   [(memq (tok-n v) '(|.| -> ::))
	    ;; Don't check next as class member
	    (cons v (loop (cdr e) #f paren-arrows?))]
	   [(eq? (tok-n v) 'delete)
	    ;; Make `delete' expression look like a function call
	    (let ([arr? (seq? (cadr e))])
	      (loop (list*
		     (make-tok (if arr? DELETE_ARRAY DELETE)
			       (tok-line v) (tok-file v) (tok-col v))
		     (make-parens
		      "(" (tok-line v) (tok-col v) (tok-file v) ")"
		      (list ((if arr? caddr cadr) e)))
		     ((if arr? cdddr cddr) e))
		    #t
		    paren-arrows?))]
	   [(and (eq? (tok-n v) 'new)
		 (not (parens? (cadr e))))
	    ;; Make `new' expression look like a function call
	    (let* ([t (cadr e)]
		   [obj? (find-c++-class (tok-n t) #f)]
		   [atom? (memq (tok-n t) non-pointer-types)])
	      (unless (or obj? atom?)
		(log-error "[NEW] ~a in ~a: New used on non-class"
			   (tok-line (car e)) (tok-file (car e))))
	      
	      (if (and (pair? (cddr e))
		       (seq? (caddr e)))
		  (loop (list*
			 (make-tok (if (brackets? (caddr e)) 
				       (if atom? NEW_ATOM_ARRAY NEW_ARRAY)
				       (if atom? NEW_ATOM NEW_OBJECT))
				   (tok-line v) (tok-file v) (tok-col v))
			 (make-parens
			  "(" (tok-line v) (tok-col v) (tok-file v) ")"
			  (list (cadr e) (make-tok '|,| #f #f #f) (caddr e)))
			 (cdddr e))
			#t
			paren-arrows?)
		  (loop (list*
			 (make-tok (if atom? NEW_ATOM NEW_OBJ) (tok-line v) (tok-file v) (tok-col v))
			 (make-parens
			  "(" (tok-line v) (tok-col v) (tok-file v) ")"
			  (list (cadr e)))
			 (cddr e))
			#t
			paren-arrows?)))]
	   [(and can-convert?
		 (pair? (cdr e))
		 (parens? (cadr e))
		 (get-c++-class-method (tok-n v) c++-class))
	    ;; method call:
	    (set! used-self? #t)
	    (list*
	     (make-tok sElF (tok-line v) (tok-col v) (tok-file v))
	     (make-tok '-> (tok-line v) (tok-col v) (tok-file v))
	     v
	     (loop (cdr e) #t paren-arrows?))]
	   [(and paren-arrows?
		 (>= (length e) 3)
		 (eq? '-> (tok-n (cadr e)))
		 (or (null? (cdddr e))
		     (not (parens? (cadddr e)))))
	    (loop (cons (make-parens
			 "(" #f #f #f ")"
			 (list (car e) (cadr e) (caddr e)))
			(cdddr e))
		  can-convert?
		  #t)]
	   [else
	    ;; look for conversion
	    (cons
	     (cond
	      [(braces? v)
	       (make-braces
		"{" (tok-line v) (tok-col v) (tok-file v) "}"
		(convert-class-vars (seq-in v) arg-vars c++-class))]
	      [(seq? v)
	       ((get-constructor v)
		(tok-n v) (tok-line v) (tok-col v) (tok-file v) (seq-close v)
		(loop (seq-in v) #t #f))]
	      [(and can-convert? (eq? (tok-n v) 'this))
	       (set! used-self? #t)
	       (make-tok sElF (tok-line v) (tok-col v) (tok-file v))]
	      [(and can-convert?
		    (not (assq (tok-n v) arg-vars))
		    (get-c++-class-var (tok-n v) c++-class))
	       (set! used-self? #t)
	       (make-parens
		"(" (tok-line v) (tok-col v) (tok-file v) ")"
		(list (make-tok sElF (tok-line v) (tok-col v) (tok-file v))
		      (make-tok '-> (tok-line v) (tok-col v) (tok-file v))
		      v))]
	      [else v])
	     (loop (cdr e) #t paren-arrows?))]))]))))
		
(define re:funcarg (regexp "^__funcarg"))
(define (is-generated? x)
  (regexp-match re:funcarg (symbol->string (car x))))

(define (convert-body body-e extra-vars pushable-vars c++-class live-vars setup-stack?)
  (let ([el (body->lines body-e #f)])
    (let-values ([(decls body) (split-decls el)])
      (let* ([local-vars 
	      (apply
	       append
	       (map (lambda (e) (get-pointer-vars e "PTRLOCAL" #f)) decls))]
	     [vars (begin
		       (ormap (lambda (var)
				(when (assq (car var) extra-vars)
				  (log-error "[SHADOW] ~a in ~a: Pointerful variable ~a shadowed in decls."
					     (tok-line (caar decls)) (tok-file (caar decls))
					     (car var))))
			      
			      local-vars)
		       (append extra-vars local-vars))])
	  ;; Convert calls and body (recusively)
	  (let-values ([(orig-maxlive) (live-var-info-maxlive live-vars)]
		       [(orig-maxpush) (live-var-info-maxpush live-vars)]
		       [(orig-tag) (live-var-info-tag live-vars)]
		       [(body-x live-vars)
			(let loop ([body body])
			  (cond
			   [(null? body)
			    ;; Starting live-vars record for this block:
			    ;;  Create new tag
			    ;;  Locally-defined arrays and records are always live.
			    ;;  Start with -1 maxlive in case we want to check whether anything
			    ;;   was pushed in the block.
			    (values null (make-live-var-info (gentag)
							     -1
							     0
							     (append
							      (let loop ([vars local-vars])
								(cond
								 [(null? vars) null]
								 [(or (array-type? (cdar vars))
								      (struct-type? (cdar vars)))
								  (cons (car vars) (loop (cdr vars)))]
								 [else (loop (cdr vars))]))
							      (live-var-info-vars live-vars))
							     (live-var-info-new-vars live-vars)
							     (live-var-info-pushed-vars live-vars)
							     (live-var-info-num-calls live-vars)))]
			   [(eq? (tok-n (caar body)) START_XFORM_SKIP)
			    (let skip-loop ([body (cdr body)])
			      (let*-values ([(end?) (eq? (tok-n (caar body)) END_XFORM_SKIP)]
					    [(rest live-vars) ((if end?
								   loop
								   skip-loop)
							       (cdr body))])
				(values (if end? rest (cons (car body) rest)) live-vars)))]
			   [else
			    (when (body-var-decl? (car body))
			      (let ([type (tok-n (caar body))]
				    [var (let loop ([e (car body)])
					   (if (or (null? (cdr e))
						   (eq? semi (tok-n (cadr e))))
					       (tok-n (car e))
					       (loop (cdr e))))])
				(unless (eq? ':: (tok-n (cadar body)))
				  (log-error "[DECL] ~a in ~a: Variable declaration (~a ~a) not at the beginning of a block."
					     (tok-line (caar body)) (tok-file (caar body))
					     type var))))
			    (let*-values ([(rest live-vars) (loop (cdr body))]
					  [(e live-vars)
					   (convert-function-calls (car body)
								   vars
								   c++-class
								   live-vars
								   #f)])
			      (values (cons e rest) live-vars))]))])
	    ;; Collect live vars and look for function calls in decl section.
	    (let ([live-vars
		   (let loop ([decls decls][live-vars live-vars])
		     (if (null? decls)
			 live-vars
			 (let dloop ([el (body->lines (car decls) #t)]
				     [live-vars live-vars])
			   (if (null? el)
			       (loop (cdr decls) live-vars)
			       (let-values ([(_ live-vars)
					     ;; We're not really interested in the conversion.
					     ;; We just want to get live vars and
					     ;; complain about function calls:
					     (convert-function-calls (car el) extra-vars c++-class live-vars #t)])
				 (dloop (cdr el) live-vars))))))])
	      ;; Calculate vars to push in this block
	      (let ([newly-pushed (filter (lambda (x)
					    (or (assq (car x) local-vars)
						(assq (car x) pushable-vars)
						(and setup-stack?
						     (is-generated? x))))
					  (live-var-info-pushed-vars live-vars))])
		(values (apply
			 append
			 (append
			  decls
			  (list (append (if label?
					    (list (make-note 'note #f #f #f (format "/* PTRVARS: ~a */" (map car vars))))
					    null)
					(if setup-stack?
					    (apply append (live-var-info-new-vars live-vars))
					    null)
					(if (and setup-stack? (not (negative? (live-var-info-maxlive live-vars))))
					    (list (make-note 'note #f #f #f (format "PREPARE_VAR_STACK(~a);" 
										    (if per-block-push?
											(+ (total-push-size newly-pushed)
											   (live-var-info-maxpush live-vars))
											(live-var-info-maxlive live-vars)))))
					    null)
					(if (negative? (live-var-info-maxlive live-vars))
					    null
					    (list (make-block-push
						   "block push"
						   #f #f #f
						   newly-pushed (live-var-info-tag live-vars) orig-tag)))))
			  body-x))
			;; Restore original tag and union max live vars:
			(make-live-var-info orig-tag
					    (max orig-maxlive
						 (live-var-info-maxlive live-vars))
					    (max orig-maxpush
						 (+ (total-push-size newly-pushed)
						    (live-var-info-maxpush live-vars)))
					    (live-var-info-vars live-vars)
					    (live-var-info-new-vars live-vars)
					    (live-var-info-pushed-vars live-vars)
					    (live-var-info-num-calls live-vars))))))))))

(define (body-var-decl? e)
  (and (pair? e)
       (or (memq (tok-n (car e)) non-pointer-types)
	   (assq (tok-n (car e)) pointer-types)
	   (assq (tok-n (car e)) c++-classes))))

(define (looks-like-call? e-)
  ;; e- is a reversed expression
  (and (pair? e-)
       (parens? (car e-))
       ;; Something precedes
       (not (null? (cdr e-)))
       ;; Not an assignment, sizeof, if, string
       (not (memq (tok-n (cadr e-)) non-functions))
       (not (string? (tok-n (cadr e-))))
       ;; Look back one more for if, etc. if preceding is paren
       (not (and (parens? (cadr e-))
		 (not (null? (cddr e-)))
		 (memq (tok-n (caddr e-)) '(if while for))))))

(define (cast-or-call e- cast-k call-k)
  ;; Looks like a function call, although we don't know the
  ;; function yet.  (The parens may be preceded by an
  ;; unparenthesized expression.) And it could be a cast (which
  ;; requires parens).
  (let ([pre (cadr e-)])
    ;; Look for cast:
    (if (and (parens? pre)
	     (let ([prel (seq-in pre)])
	       (or 
		;; Assume we never have (func)(args, ...)
		(= 1 (length prel))
		;; trailing * is a give-away
		(eq? '* (tok-n (list-ref prel (sub1 (length prel)))))
		;; leading `struct' is a giveaway:
		(eq? 'struct (tok-n (car prel))))))
	;; Cast
	(cast-k)
	;; Call
	(call-k))))

(define (resolve-indirection v get-c++-class-member c++-class locals)
  (and (parens? v)
       (= 3 (length (seq-in v)))
       (eq? '-> (tok-n (cadr (seq-in v))))
       (let ([lhs (car (seq-in v))])
	 (cond
	  [(eq? sElF (tok-n lhs))
	   (get-c++-class-member (tok-n (caddr (seq-in v))) c++-class)]
	  [(or (resolve-indirection lhs get-c++-class-var c++-class locals)
	       (assq (tok-n lhs) locals))
	   => (lambda (m)
		(let ([type (cdr m)])
		  (and (pointer-type? type)
		       (= 1 (pointer-type-stars type))
		       (= 1 (length (pointer-type-base type))))
		  (let ([c++-class (find-c++-class (car (pointer-type-base type)) #f)])
		    (and c++-class
			 (get-c++-class-member (tok-n (caddr (seq-in v))) c++-class)))))]
	  [else #f]))))

(define (lift-out-calls args live-vars c++-class)
  (let ([e (seq-in args)])
    (if (null? e)
	(values null args null null live-vars)
	(let ([el (body->lines e #t)])
	  (let loop ([el el]
		     [new-args null][setups null][new-vars null]
		     [ok-calls null][must-convert? #t][live-vars live-vars])
	    (letrec ([lift-one?
		      (lambda (e)
			(let ([e- (let ([e- (reverse e)])
				    (if (null? (cdr el))
					e-
					(cdr e-)))]) ; skip comma
			  (and (looks-like-call? e-)
			       (cast-or-call e- 
					     (lambda () #f) 
					     (lambda () 
					       (lambda (wrap)
						 (lift-one (cons e
								 (cons (or (and (null? (cddr e-)) 
										(cadr e-))
									   (and (= 3 (length (cdr e-)))
										(eq? '-> (tok-n (caddr e-)))
										(make-parens
										 "(" #f #f #f ")"
										 (reverse (cdr e-)))))
								       (car e-)))
							   wrap)))))))]
		     [lift-one
		      (lambda (call-form wrap)
			(let* ([call (car call-form)]
			       [call-func (cadr call-form)]
			       [call-args (cddr call-form)]
			       [p-m (and must-convert?
					 call-func
					 (if (parens? call-func)
					     (resolve-indirection call-func get-c++-class-method c++-class null)
					     (assq (tok-n call-func) (prototyped))))])
			  (if p-m
			      (let ([new-var (gensym '__funcarg)])
				(loop (cdr el)
				      (cons (append
					     (wrap (list (make-tok new-var #f #f #f)))
					     (if (null? (cdr el))
						 null
						 (list (make-tok '|,| #f #f #f))))
					    new-args)
				      (cons (if (null? (cdr el))
						;; Add comma
						(append call (list (make-tok '|,| #f #f #f)))
						call)
					    setups)
				      (cons (cons new-var (prototype-for-pointer? p-m))
					    new-vars)
				      ok-calls
				      #t
				      (make-live-var-info
				       (live-var-info-tag live-vars)
				       (live-var-info-maxlive live-vars)
				       (live-var-info-maxpush live-vars)
				       (live-var-info-vars live-vars)
				       ;; Add newly-created vars for lifting to declaration set
				       (cons (append (prototype-type (cdr p-m))
						     (list
						      (make-tok new-var #f #f #f)
						      (make-tok semi #f #f #f)))
					     (live-var-info-new-vars live-vars))
				       (live-var-info-pushed-vars live-vars)
				       (live-var-info-num-calls live-vars))))
			      (loop (cdr el) (cons (wrap e) new-args) setups new-vars 
				    (if must-convert?
					ok-calls
					(cons call-args ok-calls))
				    #t
				    live-vars))))]
		     [lift-in-arithmetic?
		      (lambda (e)
			(and (pair? e)
			     (cond
			      ;; look for: ! <liftable>
			      [(eq? '! (tok-n (car e)))
			       (let ([k (lift-in-arithmetic? (cdr e))])
				 (and k
				      (lambda (wrap)
					(k (lambda (x) 
					     (wrap
					      (cons (car e) x)))))))]
			      ;; look for: (<liftable>)
			      [(and (parens? (car e))
				    (null? (cdr e)))
			       (let ([k (lift-in-arithmetic? (seq-in (car e)))])
				 (and k
				      (lambda (wrap)
					(k (lambda (x) 
					     (wrap (list
						    (make-parens
						     "(" #f #f #f ")"
						     x))))))))]
			      ;; look for: n op <liftable>
			      [(and (>= (length e) 3)
				    (let ([n (tok-n (car e))])
				      (or (number? n) (symbol? n)))
				    (memq (tok-n (cadr e)) '(+ - * /)))
			       (let ([k (lift-in-arithmetic? (cddr e))])
				 (and k
				      (lambda (wrap)
					(k (lambda (x) 
					     (wrap
					      (list* (car e) (cadr e) x)))))))]
			      ;; look for: <liftable> op n
			      [(let ([len (if (null? el)
					      (length e)
					      (sub1 (length e)))]) ; skip comma
				 (and (>= len 3)
				      (let ([n (tok-n (list-ref e (sub1 len)))])
					(or (number? n) (symbol? n)))
				      (memq (tok-n (list-ref e (- len 2))) '(+ - * /))))
			       (let* ([last? (null? el)]
				      [len (if last?
					       (length e)
					       (sub1 (length e)))])
				 (printf "/* this far ~a */~n" (tok-line (car e)))
				 (let ([k (lift-in-arithmetic? (let loop ([e e])
								 (if (null? ((if last?
										 cddr 
										 cdddr)
									     e))
								     (if last?
									 null
									 (cddr e))
								     (cons (car e) (loop (cdr e))))))])
				   (and k
					(lambda (wrap)
					  (k (lambda (x) 
					       (wrap
						(append x 
							(list
							 (list-ref e (- len 2))
							 (list-ref e (- len 1)))
							(if last?
							    (list (list-ref e len))
							    null)))))))))]
			      [(lift-one? e) => values]
			      [else #f])))])
	      (cond
	       [(null? el)
		(if (null? new-vars)
		    (values null args null ok-calls live-vars)
		    (values
		     setups
		     (make-parens
		      "(" (tok-line args) (tok-col args) (tok-file args) ")"
		      (apply append (reverse! new-args)))
		     new-vars
		     ok-calls
		     live-vars))]
	       [(lift-in-arithmetic? (car el)) => (lambda (k) (k values))]
	       [(and (= (length (car el)) 2)
		     (or (string? (tok-n (caar el)))
			 (number? (tok-n (caar el)))))
		;; Constant => still no need to lift other args..
		(loop (cdr el) (cons (car el) new-args) setups new-vars ok-calls must-convert? live-vars)]
	       [else
		(loop (cdr el) (cons (car el) new-args) setups new-vars ok-calls #t live-vars)])))))))

(define (convert-function-calls e vars c++-class live-vars complain-not-in)
  ;; e is a single statement
  ;; Reverse to calculate live vars as we go.
  ;; Also, it's easier to look for parens and then inspect preceeding
  ;;  to find function calls.
  (let ([e- (reverse e)]
	[orig-num-calls (live-var-info-num-calls live-vars)])
    (let loop ([e- e-][result null][live-vars live-vars])
      (cond
       [(null? e-) (values result live-vars)]
       [(looks-like-call? e-)
	;; Looks like a function call, maybe a cast:
	(cast-or-call
	 e-
	 (lambda ()
	   ;; It's a cast:
	   (let-values ([(v live-vars)
			 (convert-paren-interior (car e-) vars c++-class live-vars complain-not-in)])
	     (loop (cddr e-)
		   (list* (cadr e-) v result)
		   live-vars)))
	 (lambda ()
	   ;; It's a function call; find the start
	   (let-values ([(args) (car e-)]
			[(func rest-)
			 (let loop ([e- (cdr e-)])
			   (cond
			    [(null? e-)
			     (values null null)]
			    [(null? (cdr e-))
			     (values e- null)]
			    [(parens? (car e-))
			     (values (list (car e-)) (cdr e-))]
			    [(brackets? (car e-))
			     ;; Array access
			     (let-values ([(func rest-) (loop (cdr e-))])
			       (values (cons (car e-) func) rest-))]
			    ;; Struct reference, class-specified:
			    [(memq (tok-n (cadr e-)) '(-> |.| ::))
			     (let-values ([(func rest-) (loop (cddr e-))])
			       (values (list* (car e-) (cadr e-) func) rest-))]
			    [else (values (list (car e-)) (cdr e-))]))])
	     (when (and complain-not-in
			(or (not (pair? complain-not-in))
			    (not (memq args complain-not-in))))
	       (log-error "[CALL] ~a in ~a: Bad place for function call, starting tok is ~s."
			  (tok-line (car func)) (tok-file (car func))
			  (tok-n (car func))))
	     ;; Lift out function calls as arguments. (Can re-order code.
	     ;; MzScheme source code must live with this change to C's semantics.)
	     ;; Calls are replaced by varaibles, and setup code generated that
	     ;; assigns to the variables.
	     (let*-values ([(orig-live-vars) live-vars]
			   [(setups args new-vars ok-calls live-vars)
			    ;; Split args into setup (calls) and args.
			    ;; List newly-created vars (in order) in new-vars.
			    ;; Make sure each setup ends with a comma.
			    (lift-out-calls args live-vars c++-class)]
			   [(args live-vars)
			    (convert-paren-interior args vars 
						    c++-class
						    (replace-live-vars 
						     live-vars
						     (append (map (lambda (x)
								    (cons (car x) (make-vtype)))
								  (filter (lambda (x)
									    (cdr x))
									  new-vars))
							     (live-var-info-vars live-vars)))
						    ok-calls)]
			   [(func live-vars)
			    (convert-function-calls (reverse func) vars c++-class live-vars #t)]
			   ;; Process lifted-out function calls:
			   [(setups live-vars)
			    (let loop ([setups setups][new-vars new-vars][result null][live-vars live-vars])
			      (if (null? setups)
				  (values result live-vars)
				  (let-values ([(setup live-vars)
						(convert-function-calls (car setups) vars 
									c++-class
									;; Remove var for this one:
									(replace-live-vars
									 live-vars
									 (remove (caar new-vars)
										 (live-var-info-vars live-vars)
										 (lambda (a b)
										   (eq? a (car b)))))
									#f)])
				    (loop (cdr setups)
					  (cdr new-vars)
					  (cons (list* (make-tok (caar new-vars) #f #f #f)
						       (make-tok '= #f #f #f)
						       setup)
						result)
					  live-vars))))])
	       ;; Put everything back together. Lifted out calls go into a sequence
	       ;;  before the main function call.
	       (let ([pushed-vars (if (and (null? (cdr func))
					   (memq (tok-n (car func)) non-returning-functions))
				      ;; non-returning -> don't need to push vars
				      null
				      (live-var-info-vars orig-live-vars))])
		 (loop rest-
		       (let ([call (if (and (null? (cdr func))
					    (memq (tok-n (car func)) non-gcing-functions))
				       ;; Call without pointer pushes
				       (make-parens
					"(" #f #f #f ")"
					(append func (list args)))
				       ;; Call with pointer pushes
				       (make-call
					"func call"
					#f
					#f
					#f
					func
					args
					pushed-vars
					(live-var-info-tag orig-live-vars)))])
			 (cons (if (null? setups)
				   call
				   (make-parens
				    "(" #f #f #f ")"
				    (append
				     (apply append setups)
				     (list call))))
			       result))
		       (make-live-var-info (live-var-info-tag live-vars)
					   ;; maxlive is either size for this push or old maxlive:
					   (max (total-push-size (live-var-info-vars orig-live-vars))
						(live-var-info-maxlive live-vars))
					   ;; note: maxpush calculated at block level
					   (live-var-info-maxpush live-vars)
					   (live-var-info-vars live-vars)
					   (live-var-info-new-vars live-vars)
					   ;; Add newly-pushed variable to pushed set:
					   (let* ([old-pushed (live-var-info-pushed-vars live-vars)]
						  [new-pushed (filter (lambda (x) (not (assq (car x) old-pushed))) pushed-vars)])
					     (append new-pushed old-pushed))
					   (add1 (live-var-info-num-calls live-vars)))))))))]
       [(eq? 'goto (tok-n (car e-)))
	;; Goto - assume all vars are live
	(loop (cdr e-) (cons (car e-) result) 
	      (replace-live-vars live-vars vars))]
       [(eq? '= (tok-n (car e-)))
	;; Check for assignments where the LHS can move due to
	;; a function call on the RHS
	(if (> (live-var-info-num-calls live-vars) orig-num-calls)
	    (let ([assignee (cdr e-)])
	      ;; Special case: (YYY -> ivar) = XXX;
	      (let ([special-case-type (and (not (null? assignee))
					    (null? (cdr assignee))
					    (= 2 (length result))
					    (call? (car result))
					    (eq? semi (tok-n (cadr result)))
					    (let ([m (resolve-indirection (car assignee) get-c++-class-var c++-class vars)])
					      (and m (cdr m))))])
		(if (and special-case-type
			 (or (non-pointer-type? special-case-type)
			     (pointer-type? special-case-type)))
		    ;; Change to (newvar = XXX, (YYY -> ivar) = newvar)
		    (let ([new-var (gensym '__assign)]
			  [v (car e-)])
		      (loop null
			    (list
			     (make-parens
			      "(" (tok-file v) (tok-line v) (tok-col v) ")"
			      (list (make-tok new-var #f #f #f)
				    (make-tok '= #f #f #f)
				    (car result)
				    (make-tok '|,| #f #f #f)
				    (car assignee)
				    v
				    (make-tok new-var (tok-file v) (tok-line v) (tok-col v))))
			     (cadr result)) ; semicolon
			    ;; Add new variable to the list:
			    (make-live-var-info
			     (live-var-info-tag live-vars)
			     (live-var-info-maxlive live-vars)
			     (live-var-info-maxpush live-vars)
			     (live-var-info-vars live-vars)
			     ;; Add newly-created vars for lifting to declaration set
			     (cons (append (type->decl special-case-type)
					   (list
					    (make-tok new-var #f #f #f)
					    (make-tok semi #f #f #f)))
				   (live-var-info-new-vars live-vars))
			     (live-var-info-pushed-vars live-vars)
			     (live-var-info-num-calls live-vars))))
		    (begin
		      (when (and (not (null? assignee))
				 (or (if (brackets? (car assignee))
					 (or (not (or (null? (cddr assignee))
						      (eq? ': (tok-n (caddr assignee)))))
					     (let ([v (cadr assignee)])
					       (or (not (symbol? (tok-n v)))
						   ;; Assignment to locally-declared array is fine:
						   (let ([m (assq (tok-n v) vars)])
						     (and m
							  (not (or (array-type? (cdr m))
								   (struct-array-type? (cdr m)))))))))
					 (not (symbol? (tok-n (car assignee)))))
				     (and (symbol? (tok-n (car assignee)))
					  (not (null? (cdr assignee)))
					  ;; ok if preceeding is else or label terminator
					  (not (memq (tok-n (cadr assignee)) '(else :)))
					  ;; assignment to field in record is ok
					  (not (and (eq? (tok-n (cadr assignee)) '|.|)
						    (pair? (cddr assignee))
						    (symbol? (tok-n (caddr assignee)))
						    (null? (cdddr assignee))))
					  ;; ok if preceeding is `if', `until', etc.
					  (not (and (parens? (cadr assignee))
						    (pair? (cddr assignee))
						    (memq (tok-n (caddr assignee)) '(if while for until))))))
				 (not (eq? 'exn_table (tok-n (car (last-pair e-))))))
			(fprintf (current-error-port)
				 "Warning [ASSIGN] ~a in ~a: suspicious assignment with a function call, LHS ends ~s.~n"
				 (tok-line (car e-)) (tok-file (car e-))
				 (tok-n (cadr e-))))
		      (loop (cdr e-) (cons (car e-) result) live-vars)))))
	    (loop (cdr e-) (cons (car e-) result) live-vars))]
       [(braces? (car e-))
	(let*-values ([(v) (car e-)]
		      ;; do/while/for: we'll need a fixpoint for live-vars
		      ;;  (We'll get the fixpoint by poing things twice)
		      [(do?) (and (not (null? (cdr e-)))
				  (memq (tok-n (cadr e-)) '(do)))]
		      [(while?) (and (not (null? (cdr e-)))
				     (parens? (cadr e-))
				     (not (null? (cddr e-)))
				     (memq (tok-n (caddr e-)) '(for while)))]
		      [(orig-new-vars) (live-var-info-new-vars live-vars)]
		      [(orig-pushed-vars) (live-var-info-pushed-vars live-vars)]
		      ;; Proc to convert body once
		      [(convert-brace-body) 
		       (lambda (live-vars)
			 (convert-body (seq-in v) vars null c++-class live-vars #f))]
		      ;; First conversion
		      [(e live-vars) (convert-brace-body live-vars)]
		      ;; Proc to filter live and pushed vars, dropping vars no longer in scope:
		      [(filter-live-vars)
		       (lambda (live-vars)
			 (let* ([not-declared (lambda (x) (assq (car x) vars))]
				[new-live-vars (filter
						not-declared
						(live-var-info-vars live-vars))]
				[new-pushed-vars (filter
						  (lambda (x) (or (not-declared x)
								  (is-generated? x)))
						  (live-var-info-pushed-vars live-vars))])
			   (make-live-var-info (live-var-info-tag live-vars)
					       (live-var-info-maxlive live-vars)
					       (live-var-info-maxpush live-vars)
					       new-live-vars
					       (live-var-info-new-vars live-vars)
					       new-pushed-vars
					       (live-var-info-num-calls live-vars))))]
		      [(restore-new-vars)
		       (lambda (live-vars)
			 (make-live-var-info (live-var-info-tag live-vars)
					     (live-var-info-maxlive live-vars)
					     (live-var-info-maxpush live-vars)
					     (live-var-info-vars live-vars)
					     orig-new-vars
					     orig-pushed-vars
					     (live-var-info-num-calls live-vars)))]
		      [(e live-vars rest extra)
		       (cond
			[do?
			 (let-values ([(e live-vars)
				       (convert-brace-body (restore-new-vars live-vars))])
			   (values e live-vars (cdr e-) #f))]
			[while?
			 ;; Run test part. We don't filter live-vars, but maybe we should:
			 (let-values ([(v live-vars)
				       (convert-seq-interior (cadr e-) #t vars 
							     c++-class
							     (restore-new-vars live-vars)
							     #f)])
			   ;; Now run body again:
			   (let-values ([(e live-vars)
					 (convert-brace-body (restore-new-vars live-vars))])
			     ;; Finally, run test again:
			     (let-values ([(v live-vars)
					   (convert-seq-interior (cadr e-) #t vars 
								 c++-class
								 live-vars
								 #f)])
			       (values e live-vars (cddr e-) v))))]
			[else
			 (values e live-vars (cdr e-) #f)])])
	  (loop rest
		(append
		 (if extra
		     (list extra)
		     null)
		 (list (make-braces
			(tok-n v)
			(tok-line v)
			(tok-col v)
			(tok-file v)
			(seq-close v)
			e))
		 result)
		(filter-live-vars live-vars)))]
       [(seq? (car e-))
	;; Do nested body:
	(let-values ([(v live-vars)
		      (convert-seq-interior (car e-) (parens? (car e-)) 
					    vars c++-class live-vars 
					    (or complain-not-in 
						(brackets? (car e-))))])
	  (loop (cdr e-) (cons v result) live-vars))]
       [(and (assq (tok-n (car e-)) vars)
	     (not (assq (tok-n (car e-)) (live-var-info-vars live-vars))))
	;; Add a live variable:
	(loop (cdr e-)
	      (cons (car e-) result)
	      (replace-live-vars live-vars 
				 (cons (assq (tok-n (car e-)) vars)
				       (live-var-info-vars live-vars))))]
       [(and (memq (tok-n (car e-)) '(while do for))
	     (case (tok-n (car e-))
	       [(do)
		(not (braces? (car result)))]
	       [(for)
		(not (braces? (cadr result)))]
	       [(while)
		(not (or (eq? semi (tok-n (cadr result)))
			 (braces? (cadr result))))]))
	(log-error "[LOOP] ~a in ~a: while/do/for with body not in braces."
		   (tok-line (car e-)) (tok-file (car e-)))
	(loop (cdr e-) (cons (car e-) result) live-vars)]
       [else (loop (cdr e-) (cons (car e-) result) live-vars)]))))

(define (convert-seq-interior v comma-sep? vars c++-class live-vars complain-not-in)
  (let ([e (seq-in v)])
    (let ([el (body->lines e comma-sep?)])
      (let-values ([(el live-vars)
		    (let loop ([el el])
		      (if (null? el)
			  (values null live-vars)
			  (let-values ([(rest live-vars) (loop (cdr el))])
			    (let-values ([(e live-vars)
					  (convert-function-calls (car el) vars c++-class live-vars complain-not-in)])
			      (values (cons e rest) live-vars)))))])
	(values ((get-constructor v)
		 (tok-n v)
		 (tok-line v)
		 (tok-col v)
		 (tok-file v)
		 (seq-close v)
		 (apply append el))
		live-vars)))))

(define (convert-paren-interior v vars c++-class live-vars complain-not-in)
  (convert-seq-interior v #t vars c++-class live-vars complain-not-in))

(define (split-decls el)
  (let loop ([el el][decls null])
    (if (null? el)
	(values (reverse! decls) null)
	(let ([e (car el)])
	  (if (or 
	       ;; These keywords appear only in decls:
	       (memq (tok-n (car e)) '(union struct static))
	       ;; Otherwise try harder:
	       (and
		;; Decl needs at least three parts:
		(< 2 (length e))
		;; Decl ends in seimicolon
		(eq? semi (tok-n (list-ref e (sub1 (length e)))))
		;; Doesn't start with a star, decrement, or increment
		(not (memq (tok-n (car e)) '(* -- ++)))
		;; Not an assignemnt
		(not (memq (tok-n (cadr e)) '(= += -=)))
		;; Not a return, case, new, or delete
		(not (memq (tok-n (car e)) '(return case new delete)))
		;; Not a label, field lookup, pointer deref, class-specific
		(not (memq (tok-n (cadr e)) '(: |.| -> ::)))
		;; No parens/braces in first two parts, except __typeof
		(not (seq? (car e)))
		(or (not (seq? (cadr e)))
		    (eq? '__typeof (tok-n (car e))))))
	      ;; Looks like a decl
	      (loop (cdr el) (cons e decls))
	      ;; Not a decl
	      (values (reverse! decls) el))))))

(define (get-one e comma-sep?)
  (let loop ([e e][result null][first #f])
    (cond
     [(null? e) (values (reverse! result) null)]
     [(eq? semi (tok-n (car e)))
      (values (reverse! (cons (car e) result)) (cdr e))]
     [(and (eq? '|,| (tok-n (car e))) comma-sep?)
      (values (reverse! (cons (car e) result)) (cdr e))]
     [(and (braces? (car e))
	   (not (memq first '(typedef struct union enum))))
      (let ([rest (cdr e)])
	(if (or (null? rest)
		(not (eq? semi (tok-n (car rest)))))
	    (values (reverse! (cons (car e) result)) rest)
	    (values (reverse! (list* (car rest) (car e) result)) (cdr rest))))]
     [else (loop (cdr e) (cons (car e) result) (or first (tok-n (car e))))])))

(define (foldl-statement e comma-sep? f a-init)
  (let loop ([e e][a a-init])
    (if (null? e)
	a
	(let-values ([(sube e) (get-one e comma-sep?)])
	  (loop e (f sube a))))))

; (print-it e 0 #t) (exit)

(define (process-top-level e)
  (foldl-statement
   e
   #f
   (lambda (sube l)
     (let* ([sube (top-level sube ".h")])
       (append l sube)))
   null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define e (let ([source #f])
	    (letrec ([translate
		      (lambda (v)
			(when (cadr v)
			  (set! source (cadr v)))
			(if (pair? (car v))
			    (let ([body (map translate (cddddr v))])
			      ((cond
				[(string=? "(" (caar v)) make-parens]
				[(string=? "[" (caar v)) make-brackets]
				[(string=? "{" (caar v)) make-braces])
			       (caar v) (caddr v) (cadddr v)
			       source
			       (cond
				[(string=? "(" (caar v)) ")"]
				[(string=? "[" (caar v)) "]"]
				[(string=? "{" (caar v)) "}"])
			       body))
			    (make-tok (car v) (caddr v) (cadddr v) source)))])
	      (map translate e-raw))))

(foldl-statement
 e
 #f
 (lambda (sube where)
   (let* ([where (or (tok-file (car sube))
		     where)]
	  [sube (top-level sube where)])
     (print-it sube 0 #t)
     where))
 #f)

(when exit-with-error?
  (error 'xform "Errors converting"))
