;;
;;  zodiac:scanner-code@
;;  $Id: scanner.ss,v 1.13 1999/10/26 18:47:02 shriram Exp $
;;
;;  Zodiac Scanner  July 96.
;;  mwk, plt group, Rice university.
;;
;;  The Scanner returns one of three struct types:
;;
;;    scalar  (symbol, number, string, boolean, char)
;;    token   (anything else)
;;    eof
;;
;;  In case of error, we invoke static-error (or internal-error)
;;  with args: token (type 'error) message (string).
;;

;;
;; Imports: make- constructors and parameters.
;; Exports: scan.
;;

(unit/sig  zodiac:scanner-code^
  (import zodiac:structures^
	  zodiac:scanner-structs^
	  (zodiac : zodiac:reader-structs^)
	  zodiac:scanner-parameters^
	  (report : zodiac:interface^))
  
  ;;
  ;;  Insert elements into table of ascii chars (plus eof).
  ;;  Indices can be either chars or ints.
  ;;  Elts can be either single char/int or list of char/int.
  ;;
  
  (define  fill
    (letrec ([loop
	      (lambda  (table  value  char-list)
		(if  (null?  char-list)  'done
		     (let* ([elt  (car char-list)]
			    [num  (if (integer? elt) elt (char->integer elt))])
		       (vector-set!  table  num  value)
		       (loop  table  value  (cdr char-list)))))])
      (case-lambda
       [(table  value)
	(vector-fill!  table  value)]
       [(table  value  elts)
	(if  (list?  elts)
	     (loop  table  value  elts)
	     (loop  table  value  (list elts)))])))
  
  
  
  ;; Internal definitions for the scanner.
  
  (define z:void   (void))
  (define z:location  make-location)
  
  (define z:origin  make-origin)
  (define source   (lambda () (z:origin 'source 'source)))
  
  (define z:scalar
    (lambda (maker)
      (lambda (obj  st  fin)
	(maker  (source)  st  fin  obj))))
  
  ;;;;; Moved from here

  (define z:token
    (lambda  (tag  obj  st  fin)
      (make-token  (source)  st  fin  obj  tag)))
  
  (define z:eof  (lambda (loc) (make-eof loc)))
  
  
  ;; Codes for the tokens returned by the scanner.
  ;; This is the "type" field in token.
  ;; (it's safe to change these here, if needed.)
  
  (define open-tag       'list)
  (define close-tag      'endseq)
  (define dot-tag        'period)
  (define quote-tag      'quote)
  (define quasi-tag      'quasiquote)
  (define unquote-tag    'unquote)
  (define splicing-tag   'unquote-splicing)
  (define string-tag     'string)
  (define box-tag        'box)
  (define boolean-tag    'boolean)
  (define char-tag       'char)
  (define circ-obj-tag   'circular-obj)
  (define circ-ref-tag   'circular-ref)
  (define vector-tag     'vector)
  (define size-vec-tag   'sized-vector)
  (define number-tag     'number)
  (define symbol-tag     'symbol)
  (define eof-tag        'eof)
  (define error-tag      'error)
  (define snip-tag       'snip)
  
  ;; Other codes for char classes.
  
  (define delim-tag      'delim)
  (define space-tag      'space)
  (define tab-tag        'tab)
  (define newline-tag    'newline)
  (define letter-tag     'letter)
  (define octal-tag      'octal)
  (define digit-tag      'digit)
  
  ;; The scanner's alphabet.
  
  (define dot-char     #\. )
  (define dot-int      (char->integer  dot-char))
  (define quote-char   #\' )
  (define quote-int    (char->integer  quote-char))
  (define quasi-char   #\` )
  (define quasi-int    (char->integer  quasi-char))
  (define unquote-char #\, )
  (define unquote-int  (char->integer  unquote-char))
  (define comment-char #\; )
  (define comment-int  (char->integer  comment-char))
  (define string-char  #\" )
  (define string-int   (char->integer  string-char))
  (define hash-char    #\# )
  (define hash-int     (char->integer  hash-char))
  (define box-char     #\& )
  (define bslash-char  #\\ )
  (define bslash-int   (char->integer  bslash-char))
  (define stick-char   #\| )
  (define stick-int    (char->integer  stick-char))
  (define bang-char    #\! )
  (define zero-int     (char->integer  #\0))
  (define space-int    (char->integer  #\space))
  (define rangle-int   (char->integer  #\> ))
  (define langle-int   (char->integer  #\< ))
  
  (define splicing-int (char->integer  #\@ ))
  (define eq-sign-int  (char->integer  #\= ))
  (define eof-int      256)
  (define snip-int     257)
  (define ascii-size   258)
  
  (define open-list     (map car  scan:paren-relation))
  (define close-list    (map cadr scan:paren-relation))
  
  (define delim-list    (cons eof-int (cons snip-int scan:delim-list)))
  
  ;;;;; Moved to here

  (define z:symbol   (z:scalar  (lambda (so st fi obj)
				  (zodiac:make-symbol so st fi obj obj '()))))
  (define z:number   (z:scalar  zodiac:make-number))
  (define z:string   (z:scalar  zodiac:make-string))
  (define z:boolean  (z:scalar  zodiac:make-boolean))
  (define z:char     (z:scalar  zodiac:make-char))
  (define z:snip     (z:scalar  zodiac:make-external))
  (define z:type-sym  (z:scalar  zodiac:make-type-symbol))

 ;;;;;;;;;;;;;;;;

  ;; letters and octals are used in #\space and #\012.
  ;; digits are used in #3(.
  ;; (nothing to do with the chars allowed in symbols.)
  
  (define letter-list
    ((lambda (l) (append  (map char-upcase l)  l))
     (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	   #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
  
  (define digit-list  (list  #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define octal-list  (list  #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
  
  ;; Ambig = chars that can begin symbols or numbers.
  ;; Dot and hash are also ambiguous, but in different ways.
  
  (define ambig-list   (cons  #\+  (cons  #\-  digit-list)))
  
  ;; Chars that come after #.
  
  (define prim-char  #\% )
  (define prim-int   (char->integer  prim-char))
  (define boolean-list  (list #\t #\f #\T #\F))
  (define hash-num-list
    (list #\i #\e #\b #\o #\d #\x  #\I #\E #\B #\O #\D #\X))
  
  (define special-char-list
    (map
     (lambda (l)
       (let ([str  (car l)]  [elt  (cadr l)])
	 (list  (reverse  (string->list str))
		(if  (char? elt)  elt
		     (integer->char elt)))))
     scan:special-char-list))
  
  ;; Number of columns that tab rounds up to.
  
  (define tab-mult   8)
  
  (define text->string
    (lambda (text) (string->immutable-string (list->string (reverse text)))))
  
  (define text->number
    (lambda (text) (string->number (text->string text))))
  
  ;; Convert symbols to single case (if case-sensitive? = #f),
  ;; so *make sure* all calls to z:symbol come through here.
  
  (define text->symbol
    (lambda (text) 
      (let ([obj (if (read-case-sensitive)
		     text 
		     (map char-downcase text))])
	(string->symbol (text->string obj)))))
  
  (define text->char
    (lambda (text)
      (let  ([low-case  (map  char-downcase  text)])
	(let  loop  ([l  special-char-list])
	  (cond
	    [(null? l)  #f]
	    [(equal? low-case (caar l))  (cadar l)]
	    [else  (loop  (cdr l))])))))
  
  (define char->digit  (lambda (c) (- (char->integer c) zero-int)))
  
  
  ;;
  ;;  The Scanner.  Optional args (in order):
  ;;    port = input port to read from.
  ;;    init-loc = location of 1st (next) char from port.
  ;;    skip-script = #t if skip #!... at start of file.
  ;;    first-col = number (usually 1) of 1st column of each line.
  ;;
  ;;  Basic Idea:  States in DFA are implemented as procedures.
  ;;    Calling a procedure means you're in that state (history).
  ;;
  ;;  Main Invariant:
  ;;    char = "current" char
  ;;    (line, col, offset) = loc of (current) char
  ;;    start-loc = global
  ;;
  
  (define  scan
    (opt-lambda 
	([port   (current-input-port)]
	 [init-loc      default-initial-location]
	 [skip-script?  #t]
	 [first-col     scan:def-first-col])
      
      
      ;; The Scanner's State.
      ;;  char = the "current" character.
      ;;  int = ascii code for char, or else 256 for eof.
      ;;  (line, col, offset) = (right) location of current char.
      ;;  (prev-line, prev-col) = (right) location of previous char.
      ;;  file = copied from location of init-loc.
      
      (let*
	  ([char  #\a]   ;; Dummy values, int must != newline.
	   [int    65] 
	   [line    (location-line  init-loc)]
	   [col     (- (location-column  init-loc) 1)]
	   [offset  (- (location-offset  init-loc) 1)]
	   [file    (location-file  init-loc)]
	   [prev-line  -99]
	   [prev-col   -99]
	   [start-loc  'error]
	   [first-offset  (location-offset init-loc)]
	   
	   [hash?      (lambda () (= int hash-int))]
	   [bslash?    (lambda () (= int bslash-int))]
	   [dquote?    (lambda () (= int string-int))]
	   [eq-sign?   (lambda () (= int eq-sign-int))]
	   [splicing?  (lambda () (= int splicing-int))]
	   [stick?     (lambda () (= int stick-int))]
	   [prim?      (lambda () (= int prim-int))]
	   [eof?       (lambda () (= int eof-int))]
	   [snip?      (lambda () (= int snip-int))]
	   
	   [main-table   (make-vector  ascii-size  (lambda () #f))]
	   [hash-table   (make-vector  ascii-size  (lambda () #f))]
	   [delim-table  (make-vector  ascii-size  #f)]
	   [char-table   (make-vector  ascii-size  #f)]
	   
	   [delim?    (lambda () (vector-ref delim-table int))]
	   [tab?      (lambda () (eq? (vector-ref delim-table int) tab-tag))]
	   [newline?  (lambda () (eq? (vector-ref delim-table int) newline-tag))]
	   [open?     (lambda () (eq? (vector-ref delim-table int) open-tag))]
	   
	   [space?    (lambda () (= int space-int))]
	   [rangle?   (lambda () (= int rangle-int))]
	   [type-sym-delim?
	    (lambda ()
	      (cond [(space?)  #f]
		    [(rangle?)  #t]
		    [else  (delim?)]))]
	   
	   [letter?   (lambda () (eq? (vector-ref char-table int) letter-tag))]
	   [octal?    (lambda () (eq? (vector-ref char-table int) octal-tag))]
	   [digit?     
	    (lambda ()
	      (let ([tag  (vector-ref char-table int)])
		(or  (eq? tag octal-tag)  (eq? tag digit-tag))))]
	   [fetch-char
	      (if (procedure? port)
		  port
		  (lambda () (read-char port)))]
	   [hash-char-list (list hash-char)])
	
	(letrec
	    
	    ;; For now, a naive treatment of location.
	    ;; We always use the *right* side of a char, so:
	    ;;   <newline> affects the following char,
	    ;;   <tab> affects itself.
	    
	    ([get-char
	      (lambda ()
		(set!  prev-line  line) 
		(set!  prev-col   col)
		(if  (newline?)
		     (begin  (set!  line  (+ line 1))
			     (set!  col  first-col))
		     (set!  col  (+  col  1)))
		(set!  char  (fetch-char))
		(set!  int 
		       (cond [(char? char)      (char->integer char)]
			     [(eof-object? char)  eof-int]
			     [else                snip-int]))
		(if  (tab?)
		     (set!  col  (* tab-mult (ceiling (/ col tab-mult)))))
		(set!  offset  (+  offset  1)))]
	     
	     [this-loc
	      (lambda ()
		(z:location  line  col  offset  file))]
	     
	     [prev-loc
	      (lambda ()
		(z:location  prev-line  prev-col  (- offset 1)  file))]
	     
	     [start-here
	      (lambda ()
		(set!  start-loc  (this-loc)))]
	     
	     [z:error
	      (case-lambda 
	       [(str)
		(report:static-error
		 (z:token  error-tag  z:void  start-loc  (prev-loc))
		 str)]
	       [(str  text)
		(report:static-error
		 (z:token  error-tag  z:void  start-loc  (prev-loc))
		 (format  str  (text->string  text)))])]
	     
	     [z:eof-error
	      (lambda  (str)
		(report:static-error
		 (z:token  error-tag  z:void  start-loc  (prev-loc))
		 (format  "unexpected end of file inside ~a"  str)))]
	     
	     ;;
	     ;; States in the scanner.
	     ;; Inv: When a state is called, (char, int) is the 
	     ;;   1st char of that token.
	     ;; get-token = the thunk returned to the reader.
	     ;;
	     
	     [get-token
	      (lambda ()
		(start-here)
		((vector-ref main-table int)))]
	     
	     [scan-wspace
	      (lambda ()
		(get-char)
		(get-token))]
	     
	     [scan-open
	      (lambda ()
		(let ([c  char])
		  (get-char)
		  (z:token  open-tag  c  start-loc  start-loc)))]
	     
	     [scan-close
	      (lambda ()
		(let ([c  char])
		  (get-char)
		  (z:token  close-tag  c  start-loc  start-loc)))]
	     
	     ;; Should we check for case-sensitive? here?
	     ;; You *really* don't want a letter being a delim!
	     
	     [scan-delim-sym
	      (lambda ()
		(let ([sym  (text->symbol (list char))])
		  (get-char)
		  (z:symbol  sym  start-loc  start-loc)))]
	     
	     [scan-quote 
	      (lambda ()
		(get-char)
		(z:token  quote-tag  z:void  start-loc  start-loc))]
	     
	     [scan-quasi
	      (lambda ()
		(get-char)
		(z:token  quasi-tag  z:void  start-loc  start-loc))]
	     
	     [scan-unquote
	      (lambda ()
		(get-char)
		(if  (or  (eof?)  (not (splicing?)))
		     (z:token  unquote-tag  z:void  start-loc  start-loc)
		     (let ([end-loc  (this-loc)])
		       (get-char)
		       (z:token  splicing-tag  z:void  start-loc  end-loc))))]
	     
	     [scan-comment
	      (lambda ()
		(cond
		  [(newline?)  (begin (get-char) (get-token))]
		  [(eof?)      (get-token)]
		  [else        (begin (get-char) (scan-comment))]))]
	     
	     [scan-string
	      (lambda ()
		(let  loop  ([l  null])
		  (get-char)
		  (cond
		    [(eof?)  (z:error  "missing close quote in string")]
		    [(dquote?)
		     (let ([end-loc  (this-loc)])
		       (get-char)
		       (z:string  (text->string l)  start-loc  end-loc))]
		    [(bslash?)
		     (begin 
		       (get-char)
		       (cond
			 [(eof?)   (z:error "missing close quote in string")]
			 [(snip?)  (get-char)
			  (z:error "objects in string must be chars")]
			 [else     (loop  (cons char l))]))]
		    [(snip?)  (get-char)
		     (z:error  "objects in string must be chars")]
		    [else  (loop  (cons char l))])))]
	     
	     [scan-dot
	      (lambda ()
		(get-char)
		(if  (delim?)
		     (z:token  dot-tag  z:void  start-loc  start-loc)
		     (sym-or-num  (list  dot-char))))]
	     
	     [scan-hash
	      (lambda ()
		(get-char)
		((vector-ref  hash-table  int)))] 
	     
	     [scan-box
	      (lambda ()
		(get-char)
		(z:token  box-tag  z:void  start-loc  (prev-loc)))]
	     
	     [scan-boolean
	      (lambda ()
		(let ([val  (char-ci=?  char  #\t)])
		  (get-char)
		  (z:boolean  val  start-loc  (prev-loc))))]
	     
	     [scan-char
	      (lambda ()
		(get-char)  ; skip the \ char.
		(cond
		  [(eof?)   (z:error "missing character after #\\")]
		  [(snip?)  (get-char)
		   (z:error "must put character after #\\")]
		  [(letter?)
		   (let  loop  ([l  (list char)])
		     (get-char)
		     (if  (letter?)
			  (loop  (cons char l))
			  (if (null? (cdr l))
			      (z:char  (car l)  start-loc  (prev-loc))
			      (let ([ch  (text->char l)])
				(if  ch
				     (z:char  ch  start-loc  (prev-loc))
				     (z:error "`~a' is not a valid character" 
					      (append l (list bslash-char hash-char))))))))]
		  [(octal?)
		   (let ([c1 char]  [d1 (char->digit char)])
		     (get-char)
		     (if (octal?)
			 (let ([c2 char] [d2 (char->digit char)])
			   (get-char)
			   (if (octal?)
			       (let ([c3 char] [d3 (char->digit char)])
				 (get-char)
				 (let ([num  (+ (* 64 d1) (* 8 d2) d3)])
				   (if  (<= 0 num 255)
					(z:char  (integer->char num)
						 start-loc  (prev-loc))
					(z:error  "`#\\~a' is not a valid octal character"
						  (list  c3  c2  c1)))))
			       (z:char  (integer->char  (+ (* 8 d1) d2))
					start-loc  (prev-loc))))
			 (z:char  c1  start-loc  (prev-loc))))]
		  [else
		   (let ([c  char])
		     (get-char)
		     (z:char  c  start-loc  (prev-loc)))]))]
	     
	     [scan-vector
	      (lambda ()
		(let ([c  char])
		  (get-char)
		  (z:token  vector-tag  c  start-loc  (prev-loc))))]
	     
	     [scan-hash-digit
	      (lambda ()
		(let  loop  ([l  (list char)])
		  (get-char)
		  (cond
		    [(digit?)  (loop  (cons char l))]
		    [(eof?)    (z:eof-error  "# syntax")]
		    [(open?)
		     (let ([c  char]
			   [num  (text->number l)])
		       (get-char)
		       ; The vector-constant-size test is now to let mzscheme
		       ; try the malloc and see if it succeeds or raises exn.
		       (if (with-handlers
			       (((lambda (x) #t) (lambda (x) #f)))
			     (make-vector num 0)
			     #t)
			   (z:token  size-vec-tag  (list  num  c)
				     start-loc  (prev-loc))
			   (z:error  "vector constant size too large")))]
		    [(hash?)
		     (let ([num  (text->number l)])
		       (get-char)
		       (z:token  circ-ref-tag  num  start-loc  (prev-loc)))]
		    [(eq-sign?)
		     (let ([num  (text->number l)])
		       (get-char)
		       (z:token  circ-obj-tag  num  start-loc  (prev-loc)))]
		    [(prim?)
		     (let ([text  (append  l  (list hash-char))])
		       (symbol-only  text))]
		    [(snip?)  (get-char)
		     (z:error "invalid # syntax")]
		    [else  
		     (let ([c  char])
		       (get-char)
		       (z:error "invalid # syntax"))])))]
	     
	     [scan-hash-stick
	      (lambda ()
		(let  loop  ([nest  1])
		  (get-char)
		  (cond
		    [(= nest 0)  (get-token)]
		    [(eof?)  (z:eof-error "#| comment")]
		    [(hash?)
		     (begin
		       (get-char)
		       (cond
			 [(eof?)  (get-token)]
			 [(stick?)  (loop  (+ nest 1))]
			 [else  (loop  nest)]))]
		    [(stick?)
		     (begin
		       (get-char)
		       (cond
			 [(eof?)  (get-token)]
			 [(hash?)  (loop  (- nest 1))]
			 [else  (loop  nest)]))]
		    [else  (loop  nest)])))]
	     
	     [scan-primitive
	      (lambda () (symbol-only  hash-char-list))]
	     
	     [scan-hash-other
	      (lambda ()
		(let ([c  char])
		  (get-char)
		  (z:error  "invalid # syntax")))]
	     
	     [scan-hash-eof
	      (lambda () (z:eof-error  "# syntax"))]
	     
	     [scan-to-delim
	      (lambda (delim?  text  esc)
		(cond 
		  [(delim?)  (values  text  esc)]
		  [(bslash?) 
		   (begin
		     (get-char)
		     (cond
		       [(eof?)   (z:error "missing character inside escape")]
		       [(snip?)  (get-char)
			(z:error "invalid object inside escape")]
		       [else  (let ([c  char])
				(get-char)
				(scan-to-delim delim? (cons c text) #t))]))]
		  [(stick?)
		   (let  loop  ([l  text])
		     (get-char)
		     (cond
		       [(eof?)   (z:error "missing close stick")]
		       [(snip?)  (get-char)
			(z:error "invalid object inside stick")]
		       [(stick?)  
			(begin
			  (get-char)
			  (scan-to-delim delim? l #t))]
		       [else  (loop  (cons  char  l))]))]
		  [else 
		   (let ([c  char])
		     (get-char)
		     (scan-to-delim delim? (cons c text) esc))]))]
	     
	     [scan-sym-num  (lambda () (sym-or-num   null))]
	     [scan-symbol   (lambda () (symbol-only  null))]
	     [scan-number
	      (lambda () (number-only  hash-char-list))]
	     
	     [sym-or-num
	      (lambda (text)
		(let-values ([(text  used-stick?)
			      (scan-to-delim  delim?  text  #f)])
		  (if  used-stick?
		       (z:symbol (text->symbol text)
				 start-loc (prev-loc))
		       (with-handlers
			   ([exn:read?
			     (lambda (x) (z:error "`~a' is not a valid number"
						  text))])
			 (let*
			     ([str  (text->string text)]
			      [num  (read (open-input-string str))])
			   (if (number? num)
			       (z:number (if (and (inexact? num) 
						  (disallow-untagged-inexact-numbers))
					     (z:error (format "`~~a' is not a valid number; try ~a"
							      (read (open-input-string (string-append "#e" str))))
						      text)
					     num)
					 start-loc (prev-loc))
			       (z:symbol (text->symbol text)
					 start-loc  (prev-loc))))))))]
	     
	     [symbol-only
	      (lambda (text)
		(let-values ([(text  foo) (scan-to-delim  delim?  text  #t)])
		  (z:symbol  (text->symbol text)
			     start-loc  (prev-loc))))]
	     
	     [number-only
	      (lambda (text)
		(let-values ([(text  foo) (scan-to-delim  delim?  text  #f)])
		  (with-handlers
		      ([(lambda (x) #t)
			(lambda (x) (z:error "`~a' starts out like a number, but isn't one" text))])
		    (let* ([str  (text->string text)]
			   [num  (read (open-input-string str))])
		      (if (number? num)
			  (z:number  num  start-loc  (prev-loc))
			  (z:error "`~a' starts out like a number, but isn't one" text))))))]
	     
	     [scan-eof
	      (lambda () (z:eof (this-loc)))]
	     
	     [scan-snip
	      (lambda () 
		(let ([obj  char])
		  (get-char)
		  (z:snip  obj  start-loc  start-loc)))]
	     
	     ;; #! is treated as a comment if first two bytes of file.
	     [scan-hash-script
	      (lambda ()
		(if (and skip-script?
			 (= offset (add1 first-offset)))
		    (begin (skip-hash-script)
			   (get-token))
		    (scan-hash-other)))]
	     
	     [skip-hash-script
	      (lambda ()
		(get-char)
		(cond
		  [(eof?)      'return]
		  [(newline?)  (get-char)]
		  [(bslash?)   
		   (get-char)
		   (if (eof?) 'return (skip-hash-script))]
		  [else  (skip-hash-script)]))]
	     )
	  
	  (fill  main-table  scan-symbol)
	  (fill  main-table  scan-wspace   scan:whitespace-list)
	  (fill  main-table  scan-sym-num  ambig-list)
	  (fill  main-table  scan-dot      dot-char)
	  (fill  main-table  scan-open     open-list)
	  (fill  main-table  scan-close    close-list)
	  (fill  main-table  scan-delim-sym scan:self-delim-symbols)
	  (fill  main-table  scan-quote    quote-char)
	  (fill  main-table  scan-quasi    quasi-char)
	  (fill  main-table  scan-unquote  unquote-char)
	  (fill  main-table  scan-comment  comment-char)
	  (fill  main-table  scan-string   string-char)
	  (fill  main-table  scan-hash     hash-char)
	  (fill  main-table  scan-eof      eof-int)
	  (fill  main-table  scan-snip     snip-int)
	  
	  (fill  hash-table  scan-hash-other)
	  (fill  hash-table  scan-box      box-char)
	  (fill  hash-table  scan-boolean  boolean-list)
	  (fill  hash-table  scan-char     bslash-char)
	  (fill  hash-table  scan-number   hash-num-list)
	  (fill  hash-table  scan-vector   open-list)
	  (fill  hash-table  scan-hash-digit  digit-list)
	  (fill  hash-table  scan-hash-stick  stick-char)
	  (fill  hash-table  scan-primitive   prim-char)
	  (fill  hash-table  scan-hash-eof  eof-int)
	  (fill  hash-table  scan-hash-script  bang-char)
	  
	  (fill  delim-table  #f)
	  (fill  delim-table  delim-tag    delim-list)
	  (fill  delim-table  space-tag    scan:whitespace-list)
	  (fill  delim-table  tab-tag      scan:tab-list)
	  (fill  delim-table  newline-tag  scan:newline-list)
	  (fill  delim-table  open-tag     open-list)
	  (fill  delim-table  eof-tag      eof-int)
	  (fill  delim-table  snip-tag     snip-int)
	  
	  (fill  char-table  #f)
	  (fill  char-table  letter-tag  letter-list)
	  (fill  char-table  digit-tag   digit-list)
	  (fill  char-table  octal-tag   octal-list)
	  
	  (get-char)	  
	  get-token))))) 

