(unit/sig reader^
  (import xml-structs^ mzlib:function^)
  
  ;; Start-tag ::= (make-start-tag Location Location Symbol (listof Attribute))
  (define-struct (start-tag struct:source) (name attrs))
  
  ;; End-tag ::= (make-end-tag Location Location Symbol)
  (define-struct (end-tag struct:source) (name))
  
  ;; Token ::= Contents | Start-tag | End-tag | Eof
  
  (define read-comments (make-parameter #f))
  (define trim-whitespace (make-parameter #f))
  
  ;; read-xml : [Input-port] -> Document
  (define read-xml
    (case-lambda
     [(in) (read-from-port in)]
     [() (read-from-port (current-input-port))]))
  
  ;; read-from-port : Input-port -> Document
  (define (read-from-port in)
    (let-values ([(misc0 start) (read-misc in)])
      (make-document (make-prolog misc0 #f)
                     (cond
                       [(start-tag? start) (read-element start in)]
                       [(element? start) start]
                       [else (error 'xml-read "expected root element - received ~a" start)])
                     (let-values ([(misc1 end-of-file) (read-misc in)])
                       (unless (eof-object? end-of-file)
                         (error 'xml-read "extra stuff at end of document ~a" end-of-file))
                       misc1))))
  
  ;; read-misc : Input-port -> (listof Misc) Token
  (define (read-misc in)
    (let read-more ()
      (let ([x (lex in)])
        (cond
          [(or (pi? x) (comment? x))
           (let-values ([(lst next) (read-more)])
             (values (cons x lst) next))]
          [(and (pcdata? x) (andmap char-whitespace? (string->list (pcdata-string x))))
           (read-more)]
          [else (values null x)]))))
  
  ;; read-element : Start-tag Input-port -> Element
  (define (read-element start in)
    (let ([name (start-tag-name start)]
          [a (source-start start)]
          [b (source-stop start)])
      (make-element
       a b name (start-tag-attrs start)
       (let read-content ()
         (let ([x (lex in)])
           (cond
             [(eof-object? x)
              (error 'xml-read "unclosed ~a tag at [~a ~a]" name a b)]
             [(start-tag? x) (cons (read-element x in) (read-content))]
             [(end-tag? x)
              (unless (eq? name (end-tag-name x))
                (error 'xml-read "start tag ~a at [~a ~a] doesn't match end tag ~a at [~a ~a]"
                       name a b (end-tag-name x) (source-start x) (source-stop x)))
              null]
             [(entity? x) (cons (expand-entity x) (read-content))]
             [(comment? x) (if (read-comments)
                               (cons x (read-content))
                               (read-content))]
             [else (cons x (read-content))]))))))
  
  ;; expand-entity : Entity -> (U Entity Pcdata)
  ;; more here - allow expansion of user defined entities
  (define (expand-entity x)
    (let ([expanded (default-entity-table (entity-text x))])
      (if expanded
          (make-pcdata (source-start x) (source-stop x) expanded)
          x)))
  
  ;; default-entity-table : Symbol -> (U #f String)
  (define (default-entity-table name)
    (case name
      [(amp) "&"]
      [(lt) "<"]
      [(gt) ">"]
      [(quot) "\""]
      [(apos) "'"]
      [else #f]))
  
  ;; lex : Input-port -> Token
  (define (lex in)
    (when (trim-whitespace)
      (skip-space in))
    (let ([c (peek-char in)])
      (cond
        [(eof-object? c) c]
        [(eq? c #\&) (lex-entity in)]
        [(eq? c #\<) (lex-tag-cdata-pi-comment in)]
        [else (lex-pcdata in)])))
  
  ;; lex-entity : Input-port -> Entity
  (define (lex-entity in)
    (let ([start (file-position in)])
      (read-char in)
      (let ([data (case (peek-char in)
                    [(#\#)
                     (read-char in)
                     (let ([n (case (peek-char in)
                                [(#\x) (read-char in)
                                 (string->number (read-until #\; in) 16)]
                                [else (string->number (read-until #\; in))])])
                       (unless (number? n)
                         (lex-error in "malformed numeric entity"))
                       n)]
                    [else
                     (begin0
                       (lex-name in)
                       (unless (eq? (read-char in) #\;)
                         (lex-error in "expected ; at the end of an entity")))])])
        (make-entity start
                     (file-position in)
                     data))))
  
  ;; lex-tag-cdata-pi-comment : Input-port -> Start-tag | Element | End-tag | Pcdata | Pi | Comment
  (define (lex-tag-cdata-pi-comment in)
    (let ([start (file-position in)])
      (read-char in)
      (case (non-eof peek-char in)
        [(#\!)
         (read-char in)
         (case (non-eof peek-char in)
           [(#\-) (read-char in)
            (unless (eq? (read-char in) #\-)
              (lex-error in "expected second - after <!-"))
            (let ([data (lex-comment-contents in)])
              (unless (eq? (read-char in) #\>)
                (lex-error in "expected > to end comment (\"--\" can't appear in comments)"))
              ;(make-comment start (file-position in) data)
              (make-comment data))]
           [(#\[) (read-char in)
            (unless (string=? (read-string 6 in) "CDATA[")
              (lex-error in "expected CDATA following <["))
            (let ([data (lex-cdata-contents in)])
              (make-pcdata start (file-position in) data))]
           [else (skip-dtd in)
                 (skip-space in)
                 (unless (eq? (peek-char in) #\<)
                   (lex-error in "expected pi, comment, or element after doctype"))
                 (lex-tag-cdata-pi-comment in)])]
        [(#\?) (read-char in)
         (let ([name (lex-name in)])
           (skip-space in)
           (let ([data (lex-pi-data in)])
             (make-pi start (file-position in) name data)))]
        [(#\/) (read-char in)
         (let ([name (lex-name in)])
           (skip-space in)
           (unless (eq? (read-char in) #\>)
             (lex-error in "expected > to close ~a's end tag" name))
           (make-end-tag start (file-position in) name))]
        [else
         (let ([name (lex-name in)]
               [attrs (lex-attributes in)])
           (skip-space in)
           (case (read-char in)
             [(#\/)
              (unless (eq? (read-char in) #\>)
                (lex-error in "expected > to close empty element ~a" name))
              (make-element start (file-position in) name attrs null)]
             [(#\>) (make-start-tag start (file-position in) name attrs)]
             [else (lex-error in "expected / or > to close tag ~a" name)]))])))
  
  
  ;; lex-attributes : Input-port -> (listof Attribute)
  (define (lex-attributes in)
    (quicksort (let loop ()
                 (skip-space in)
                 (cond
                   [(name-start? (peek-char in))
                    (cons (lex-attribute in) (loop))]
                   [else null]))
               (lambda (a b)
                 (let ([na (attribute-name a)]
                       [nb (attribute-name b)])
                   (cond
                     [(eq? na nb) (lex-error in "duplicated attribute name ~a" na)]
                     [else (string<? (symbol->string na) (symbol->string nb))])))))
  
  ;; lex-attribute : Input-port -> Attribute
  (define (lex-attribute in)
    (let ([start (file-position in)]
          [name (lex-name in)])
      (skip-space in)
      (unless (eq? (read-char in) #\=)
        (lex-error in "expected = in attribute ~a" name))
      (skip-space in)
      ;; more here - handle entites and disallow "<"
      (let* ([delimiter (read-char in)]
             [value (case delimiter
                      [(#\' #\")
                       (list->string
                        (let read-more ()
                          (let ([c (non-eof peek-char in)])
                            (cond
                              [(eq? c delimiter) (read-char in) null]
                              [(eq? c #\&)
                               (let ([entity (expand-entity (lex-entity in))])
                                 (if (pcdata? entity)
                                     (append (string->list (pcdata-string entity)) (read-more))
                                     ;; more here - do something with user defined entites
                                     (read-more)))]
                              [else (read-char in) (cons c (read-more))]))))]
                      [else (lex-error in "attribute values must be in ''s or in \"\"s")])])
        (make-attribute start (file-position in) name value))))
  
  ;; skip-space : Input-port -> Void
  ;; deviation - should sometimes insist on at least one space
  (define (skip-space in)
    (let loop ()
      (let ([c (peek-char in)])
        (when (and (not (eof-object? c)) (char-whitespace? c))
          (read-char in)
          (loop)))))
  
  ;; lex-pcdata : Input-port -> Pcdata
  ;; deviation - disallow ]]> "for compatability" with SGML, sec 2.4 XML spec 
  (define (lex-pcdata in)
    (let ([start (file-position in)]
          [data (let loop ([c (read-char in)])
                  (let ([next (peek-char in)])
                    (cond
                      [(or (eof-object? next) (eq? next #\&) (eq? next #\<))
                       (list c)]
                      [(and (char-whitespace? next) (trim-whitespace))
                       (skip-space in)
                       (let ([lst (loop #\space)])
                         (cond
                           [(null? (cdr lst)) (list c)]
                           [else (cons c lst)]))]
                      [else (cons c (loop (read-char in)))])))])
      (make-pcdata start
                   (file-position in)
                   (list->string data))))
    
  ;; lex-name : Input-port -> Symbol
  (define (lex-name in)
    (let ([c (read-char in)])
      (unless (name-start? c)
        (lex-error in "expected name, received ~a" c))
      (string->symbol
       (list->string
        (cons c (let lex-rest ()
                  (cond
                    [(name-char? (peek-char in))
                     (cons (read-char in) (lex-rest))]
                    [else null])))))))
  
  ;; skip-dtd : Input-port -> Void
  (define (skip-dtd in)
    (let skip ()
      (case (non-eof read-char in)
        [(#\') (read-until #\' in) (skip)]
        [(#\") (read-until #\" in) (skip)]
        [(#\<)
         (case (non-eof read-char in)
           [(#\!) (case (non-eof read-char in)
                    [(#\-) (read-char in) (lex-comment-contents in) (read-char in) (skip)]
                    [else (skip) (skip)])]
           [(#\?) (lex-pi-data in) (skip)]
           [else (skip) (skip)])]
        [(#\>) (void)]
        [else (skip)])))
  
  ;; name-start? : Char -> Bool
  (define (name-start? ch)
    (or (char-alphabetic? ch) 
        (eq? ch #\_)
        (eq? ch #\:)))
  
  ;; name-char? : Char -> Bool
  (define (name-char? ch)
    (or (name-start? ch)
        (char-numeric? ch)
        (eq? ch #\.)
        (eq? ch #\-)))
  
  ;; read-until : Char Input-port -> String
  ;; discards the stop character, too
  (define (read-until char in)
    (list->string
     (let read-more ()
       (let ([c (non-eof read-char in)])
         (cond
           [(eq? c char) null]
           [else (cons c (read-more))])))))
  
  ;; non-eof : (Input-port -> (U Char Eof)) Input-port -> Char
  (define (non-eof f in)
    (let ([c (f in)])
      (cond
        [(eof-object? c) (lex-error in "unexpected eof")]
        [else c])))
  
  ;; gen-read-until-string : String -> Input-port -> String
  ;; uses Knuth-Morris-Pratt from
  ;; Introduction to Algorithms, Cormen, Leiserson, and Rivest, pages 869-876
  ;; discards stop from input
  (define (gen-read-until-string stop)
    (let* ([len (string-length stop)]
           [prefix (make-vector len 0)]
           [fall-back
            (lambda (k c)
              (let ([k (let loop ([k k])
                         (cond
                           [(and (> k 0) (not (eq? (string-ref stop k) c)))
                            (loop (vector-ref prefix (sub1 k)))]
                           [else k]))])
                (if (eq? (string-ref stop k) c)
                    (add1 k)
                    k)))])
      (let init ([k 0] [q 1])
        (when (< q len)
          (let ([k (fall-back k (string-ref stop q))])
            (vector-set! prefix q k)
            (init k (add1 q)))))
      ;; (vector-ref prefix x) = the longest suffix that matches a prefix of stop
      (lambda (in)
        (list->string
         (let/ec out
           (let loop ([matched 0] [out out])
             (let* ([c (read-char in)]
                    [matched (fall-back matched c)])
               (cond
                 [(= matched len) (out null)]
                 [(zero? matched) (cons c (let/ec out (loop matched out)))]
                 [else (cons c (loop matched out))]))))))))
  
  ;; "-->" makes more sense, but "--" follows the spec.
  (define lex-comment-contents (gen-read-until-string "--"))
  (define lex-pi-data (gen-read-until-string "?>"))
  (define lex-cdata-contents (gen-read-until-string "]]>"))
  
  ;; lex-error : Input-port String TST* -> alpha
  (define (lex-error in str . rest)
    (error 'lex-error " at positon ~a: ~a" (file-position in)
           (apply format str rest))))