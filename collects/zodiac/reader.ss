;;
;;  zodiac:reader-code@
;;  $Id: reader.ss,v 1.6 1999/02/04 14:32:54 mflatt Exp $
;;
;;  Zodiac Reader  July 96
;;  mwk, plt group, Rice university.
;;
;;  The Reader returns one of three struct types:
;;
;;    scalar    (symbol, number, string, boolean, char)
;;    sequence  (list, vector, improper-list)
;;    eof
;;
;;  In case of error, we invoke static-error (or internal-error)
;;  with args:  zodiac-obj  fmt-string . args
;;

(unit/sig  zodiac:reader-code^

    (import
      zodiac:structures^
      zodiac:scanner-structs^
      (zodiac : zodiac:reader-structs^)
      zodiac:scanner-parameters^
      (report : zodiac:interface^)
      zodiac:scanner-code^)

   ;; rename some things for now.  will clean this up.

   (define paren-relation scan:paren-relation)
   (define def-init-loc default-initial-location)
   (define def-first-col scan:def-first-col)

   
   (define default-vector-object 
     (lambda (start  finish)
       (zodiac:make-number 
        (make-origin  'reader  'vector)
        start  finish  0))) 
   
   (define my-make-list
     (lambda (len  elt)
       (let  loop  ([n  0]  [l  null])
         (if  (= n len)  l
              (loop  (+ n 1)  (cons elt l))))))
   
   (define paren-rel  paren-relation)
   
   ;; Need to subdivide z:token with vector and sized-vector.
   ;; Then object only has the paren, and size is separate.
   
   (define match? 
     (lambda (t1 t2)
       (let ([c1  (token-object t1)]
             [c2  (token-object t2)])
         (member (list (if (char? c1) c1 (cadr c1))
                       c2)
                 paren-rel))))
   
   (define z:endseq? 
     (lambda (obj) 
       (and (token? obj) 
            (eq? (token-type obj) 'endseq))))
   
   (define read-origin
     (lambda (how)
       (make-origin  'reader  how)))
   
   (define z:r-s-e (lambda x (apply report:static-error x)))
   (define z:int-error (lambda x (apply report:internal-error x)))

   ;; pack-quote into zodiac structure.
   ;;   ,@ <obj>  -->  ( unquote-splicing  <obj> )
   ;;   12 3   4       1 1              2  3   4 4
   
   (define pack-quote
     (lambda (type  token  obj)
       (let ([one   (zodiac-start   token)]
             [two   (zodiac-finish  token)]
             [four  (zodiac-finish  obj)])
         (zodiac:make-list
          (read-origin  type)  one  four
          (list 
           (zodiac:make-symbol
            (read-origin  type)  one  two
            type type '(-1))
           obj)
          2 '()))))
   
   ;; pack-box into zodiac structure.
   ;;   #& <obj>  -->  (z:box  origin  start  finish  <obj>)
   ;;   12 3   4                       1      4

   (define pack-box
     (lambda (box  obj)
       (let ([one   (zodiac-start  box)]
             [four  (zodiac-finish obj)])
         (zodiac:make-box  (read-origin 'box)  one  four  obj))))
   
   ;; pack-seqn combines pack-list, -vector 
   
   (define pack-seqn
     (lambda (z:maker)
       (lambda (open-token  close-token  head  len)
         (z:maker
          (zodiac-origin  open-token)
          (zodiac-start   open-token)
          (zodiac-finish  close-token)
          head
          len))))
   
   (define pack-list      (pack-seqn  (lambda (orig open close head len)
					(zodiac:make-list orig open close
					  head len '()))))
   (define pack-vector    (pack-seqn  zodiac:make-vector))

   (define allow-improper-lists (make-parameter #t))
   (define allow-reader-quasiquote (make-parameter #t))

   (define (dot-err s)
     (if (allow-improper-lists)
	 s
	 "misuse of `.' (improper lists are not allowed)"))

   (define pack-imp-list 
     (lambda (open-token  close-token  head  len  dot)
       (let ([obj  (zodiac:make-improper-list
                     (zodiac-origin  open-token)
                     (zodiac-start   open-token)
                     (zodiac-finish  close-token)
                     head  len  dot '())])
         (if (allow-improper-lists)
             obj 
             (z:r-s-e  obj  (dot-err ""))))))

   ;; convert  (a . (b . ())) ==> (a b)  if parameter set
   ;; and obj after dot is list or imp-list.
   ;; REPLACE #f in cond with (not compact-imp-list-parameter).

   (define compact-imp-list
     (lambda (open-token close-token head len dot before-dot after-dot)
       (cond
         [#f  (pack-imp-list open-token close-token head len dot)]
         [(zodiac:list? after-dot)
           (set-cdr! before-dot (zodiac:read-object after-dot))
           (pack-list 
             open-token 
             close-token 
             head 
             (+ len (zodiac:sequence-length after-dot) -1))]
         [(zodiac:improper-list? after-dot)
           (set-cdr! before-dot (zodiac:read-object after-dot))
           (pack-imp-list
             open-token
             close-token
             head
             (+ len (zodiac:sequence-length after-dot) -1)
             (zodiac:improper-list-period after-dot))]
         [else (pack-imp-list open-token close-token head len dot)])))


   (define  read
     (opt-lambda 
       ([port   (current-input-port)]
        [init-loc      def-init-loc]
        [skip-script   #t]
        [first-col     def-first-col])

       (let*
           ([get-token  (scan port init-loc skip-script first-col)])
         
         ;; read-obj returns one of:
         ;;  z:read   zodiac object (scalar or sequence)
         ;;  z:token  (type 'endseq) for close paren
         ;;  z:period
         ;;  z:eof
         
         (letrec 
             ([read-obj
               (lambda ()
                 (let ([token  (get-token)])
                   (cond
                     [(zodiac:scalar?  token)  token]
                     [(token?  token) 
                      (let ([type  (token-type  token)])
                        (cond
                          [(eq?  type  'endseq)  token]
                          [(eq?  type  'list) 
                           (read-seqn  type  token  pack-list)]
                          [(eq?  type  'vector)
                           (read-seqn  type  token  pack-vector)]
                          [(eq?  type  'sized-vector)
                           (read-seqn  type  token  finish-vector)]
                          [(or (eq?  type  'quote) 
                               (eq?  type  'quasiquote)
                               (eq?  type  'unquote) 
                               (eq?  type  'unquote-splicing))
			   (unless (or (eq?  type  'quote) 
				       (allow-reader-quasiquote))
			     (z:r-s-e  token 
				       (format "illegal use of \"~a\""
					       (case type
						 [(quasiquote) "`"]
						 [(unquote) ","]
						 [else ",@"]))))
                           (read-quote  type  token)]
                          [(eq?  type  'period) 
                           (make-period  (zodiac-start  token))]
                          [(eq?  type  'box)  (read-box  token)]
                          [(or (eq?  type  'circular-obj)
                               (eq?  type  'circular-ref))
                           (z:r-s-e  token
                                     "circular objects are not implemented")]
                          [else  
                           (z:int-error  token 
                              "unknown scanner token type: ~s"  type)]))]
                     [(eof?  token)  token]
                     [else  
                      (z:int-error token "unknown scanner object")])))]
              
              
              ;; read-seqn combines read-list, -vector, -imp-list
              ;;  type = 'list, 'vector, 'improper-list
              ;;  token = z:token for (, #(, #nn(  ;; )))
              ;;  end-fcn = fcn to call when get close paren.
              ;; sent = sentinel to simplify making list.
              ;; p = last item in list, before obj.
              
              [read-seqn
               (lambda (type  token  end-fcn)
                 (let ([sent  (cons  #f  null)])
                   (let  loop  ([p sent] [len 0])
                     (let ([obj  (read-obj)])
                       (cond
                         [(zodiac:read?  obj)
                          (set-cdr!  p  (cons obj null))
                          (loop  (cdr p)  (+ len 1))]
                         [(z:endseq?  obj)
                          (if (match? token obj)
                              (if (eq?  type  'sized-vector)
                                  (finish-vector  token  obj  (cdr sent)  p  len)
                                  (end-fcn  token  obj  (cdr sent)  len))
                              (z:r-s-e  obj
                                        "close paren does not match open paren"))]
                         [(period?  obj)
                          (if (eq?  type  'list)
                              (if (= len 0)
                                  (z:r-s-e  obj
                                            (dot-err "can't put `.' as first item in list"))
                                  (finish-imp-list token obj (cdr sent) p len))
                              (z:r-s-e obj (dot-err "can't use `.' inside vector")))]
                         [(eof?  obj)
                          (z:r-s-e  token  "missing close paren")]
                         [else 
                          (z:int-error obj "unknown reader object")])))))]
              
              
              ;; read-improper-list
              ;; exactly one object after dot, then close paren.
              ;; p = item before dot.
              ;; obj = item after dot (or else error).
              ;; obj2 = close paren (or else error).
              
              [finish-imp-list
               (lambda (token  dot  head  p  len)
		 (unless (allow-improper-lists)
		   (z:r-s-e  dot (dot-err "")))
                 (let ([obj  (read-obj)])
                   (cond 
                     [(zodiac:read?  obj)
                      (let ([obj2  (read-obj)])
                        (cond 
                          [(z:endseq?  obj2)
                           (if (match? token obj2)
                               (begin
                                 (set-cdr! p (cons obj null))
                                 (compact-imp-list token obj2 head (+ len 1) dot p obj))
                               (z:r-s-e  obj2
                                  "close paren does not match open paren"))] 
                          [(zodiac:read?  obj2)
                           (z:r-s-e  obj2  "too many elements after `.'")]
                          [(period?  obj2)
                           (z:r-s-e  obj2  "can't have two `.'s in a list")]
                          [(eof?  obj2)
                           (z:r-s-e  obj2  "missing close paren")]
                          [else
                           (z:int-error obj2 "Unknown reader object")]))]
                     [(period?  obj)
                      (z:r-s-e  obj  "can't have two `.'s in a list")]
                     [(z:endseq?  obj)
                      (z:r-s-e  obj  "must put one object after `.' in list")]
                     [(eof?  obj)
                      (z:r-s-e  token  "missing close paren")]
                     [else
                      (z:int-error obj "unknown reader object")])))]
              
              
              ;; finish sized-vectors
              ;; compare size with actual number of elements,
              ;; and pad or complain as necessary.
              ;; object in open-token = (size char)
              
              [finish-vector
               (lambda (open-token  close-token  head  tail  len)
                 (let ([size  (car (token-object open-token))])
                   (cond
                     [(= len size)
                      (pack-vector  open-token  close-token  head  len)]
                     [(= len 0) 
                      (let ([obj  (default-vector-object
                                   (zodiac-finish  open-token)
                                   (zodiac-finish  close-token))])
                        (pack-vector  open-token  close-token
                                      (my-make-list  size  obj)  size))]
                     [(< len size)
                      (let* ([last-obj  (car tail)]
                             [p  (my-make-list  (- size len)  last-obj)])
                        (set-cdr!  tail  p)
                        (pack-vector  open-token  close-token  head  size))]
                     [else 
                      (z:r-s-e  open-token
                                "too many elements in vector constant")])))]
              
              
              ;; read-quote  '<obj> --> (quote <obj>)
              ;; can only quote reader-objs, not dot, close paren, eof.
              ;; quote-type is the symbol quote, unquote, ... (kludge!)
              
              [read-quote
               (lambda (quote-type  quote-token)
                 (let ([obj  (read-obj)])
                   (if (zodiac:read?  obj)
                       (pack-quote  quote-type  quote-token  obj)
                       (box/quote-error  quote-type  quote-token  obj))))]
              
              ;; read-box  #& <obj> --> (box (quote <obj>))
              ;; can only box reader-objs, not dot, close paren, eof.
              
              [read-box
               (lambda (box-token)
                 (let ([obj  (read-obj)])
                   (if (zodiac:read?  obj)
                       (pack-box  box-token  obj)
                       (box/quote-error  'box  box-token  obj))))]
              
              ;; Can't put dot, close paren, eof after box or quote.
              ;; type = symbol box, quote, unquote, ...
              ;; token = box or quote z:token.
              ;; obj = bad object after box/quote.
              
              [box/quote-error
               (lambda (type  token  obj)
                 (cond
                   [(eof?  obj)
                    (z:r-s-e  token  "missing object after ~a"  type)]
                   [(period?  obj)
                    (z:r-s-e  obj  "can't put `.' after ~a"  type)]
                   [(z:endseq?  obj)
                    (z:r-s-e  obj  "can't put close paren after ~a"  type)]
                   [else
                    (z:int-error obj "unknown reader object")]))]
              
              ;; read-top-level returns the next scheme object and
              ;; complains if dot or close paren is outside a list.
              ;; close paren is in z:token with type 'endseq.
              
              [read-top-level
               (lambda () 
                 (let  ([obj  (read-obj)])
                   (cond
                     [(or (zodiac:read? obj) (eof? obj))  obj]
                     [(period?  obj)
                      (z:r-s-e  obj  (dot-err "can't use `.' outside list"))]
                     [(z:endseq?  obj)
                      (z:r-s-e  obj  "too many close parens")]
                     [else
                      (z:int-error obj "Unknown reader object")])))] 
              )
           read-top-level))))
   
  )

