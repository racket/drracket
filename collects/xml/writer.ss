(unit/sig writer^
  (import xml-structs^ mzlib:function^)
  
  (define empty-tag-shorthand (make-parameter #t))
  
  ;; var-argify : (a Output-port -> b) -> (a [Output-port] -> b)
  (define (var-argify f)
    (case-lambda
     [(x out) (f x out)]
     [(x) (f x (current-output-port))]))
  
  ;; gen-write/display-xml/content : (Nat Output-port -> Void) -> Content [Output-Port]-> Void
  (define (gen-write/display-xml/content dent)
    (var-argify (lambda (c out) (write-xml-content c 0 dent out))))
  
  ;; indent : Nat Output-port -> Void
  (define (indent n out)
    (newline out)
    (let loop ([n n])
      (unless (zero? n)
        (display #\space out)
        (loop (sub1 n)))))
  
  ;; write-xml/content : Content [Output-port] -> Void
  (define write-xml/content (gen-write/display-xml/content void))
  
  ;; display-xml/content : Content [Output-port] -> Void
  (define display-xml/content (gen-write/display-xml/content indent))
  
  ;; gen-write/display-xml : (Content [Output-port] -> Void) -> Document [Output-port] -> Void
  (define (gen-write/display-xml output-content)
    (var-argify (lambda (doc out)
                  (display-outside-misc (prolog-misc (document-prolog doc)) out)
                  (output-content (document-element doc))
                  (display-outside-misc (document-misc doc) out))))
  
  ;; write-xml : Document [Output-port] -> Void
  (define write-xml (gen-write/display-xml write-xml/content))
  
  ;; display-xml : Document [Output-port] -> Void
  (define display-xml (gen-write/display-xml display-xml/content))

  ;; display-outside-misc : (listof Misc) Output-port -> Void
  (define (display-outside-misc misc out)
    (for-each (lambda (x)
                ((cond
                   [(comment? x) write-xml-comment]
                   [(pi? x) write-xml-pi]) x 0 void out)
                (newline out))
              misc))
    
  ;; write-xml-content : Content Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-content el over dent out)
    ((cond
       [(element? el) write-xml-element]
       [(pcdata? el) write-xml-pcdata]
       [(entity? el) write-xml-entity]
       [(comment? el) write-xml-comment]
       [(pi? el) write-xml-pi]
       [else (error 'write-xml-content "received ~a" el)])
     el over dent out))
  
  ;; write-xml-element : Element Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-element el over dent out)
    (let ([start (lambda (f) (write-xml-base (format f (element-name el)) over dent out))]
          [content (element-content el)])
      (start "<~a")
      (for-each (lambda (att)
                  (fprintf out " ~s=~s" (attribute-name att)
                           (escape (attribute-value att) escape-attribute-table)))
                (element-attributes el))
      (if (and (null? content) (empty-tag-shorthand))
          (fprintf out "/>")
          (begin
            (fprintf out ">")
            (for-each (lambda (c) (write-xml-content c (incr over) dent out)) content)
            (start "</~a")
            (fprintf out ">")))))
  
  ;; write-xml-base : (U String Char Symbol) Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-base el over dent out)
    (dent over out)
    (display el out))
  
  ;; write-xml-pcdata : Pcdata Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-pcdata str over dent out)
    (write-xml-base (escape (pcdata-string str) escape-table) over dent out))
  
  ;; write-xml-pi : Processing-instruction Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-pi pi over dent out)
    (write-xml-base (format "<?~a ~a?>" (pi-target-name pi) (pi-instruction pi)) over dent out))
  
  ;; write-xml-comment : Comment Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-comment comment over dent out)
    (write-xml-base (format "<!--~a-->" (comment-text comment)) over dent out))
  
  ;; write-xml-entity : Entity Nat (Nat Output-stream -> Void) Output-stream -> Void
  (define (write-xml-entity entity over dent out)
    (let ([n (entity-text entity)])
      (fprintf out (if (number? n) "&#~a;" "&~a;") n)))
    
  (define escape-table
    (map (lambda (x y) (cons (regexp (symbol->string x)) y))
         '(< > &)
         '("\\&lt;" "\\&gt;" "\\&amp;")))
  
  (define escape-attribute-table
    (list* (cons (regexp "'") "\\&apos;") (cons (regexp "\"") "\\&quot;") escape-table))
  
  ;; escape : String -> String
  ;; more here - this could be much more efficient
  (define (escape x table)
    (foldr (lambda (esc str) (regexp-replace* (car esc) str (cdr esc)))
           x
           table))

  ;; incr : Nat -> Nat
  (define (incr n) (+ n 2)))
