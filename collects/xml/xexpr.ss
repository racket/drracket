(unit/sig extra-xexpr^
  (import xml-structs^ writer^ mzlib:function^)
  ;; Xexpr ::= String
  ;;        |  (list* Symbol (listof Attribute-srep) (listof Xexpr))
  ;;        |  (cons Symbol (listof Xexpr))
  ;;        |  Symbol
  ;;        |  Nat
  ;;        |  Comment
  ;;        |  Processing-instruction
  ;; Attribute-srep ::= (list Symbol String)
  
  ;; sorting is no longer necessary, since xt3d uses xml->zxexpr, which sorts.
  
  ;; assoc-sort : (listof (list Symbol a)) -> (listof (list Symbol a))
  (define (assoc-sort to-sort)
    (quicksort to-sort (bcompose string<? (compose symbol->string car))))
  
  (define xexpr-drop-empty-attributes (make-parameter #f))
  
  ;; xml->xexpr : Content -> Xexpr
  ;; The contract is loosely enforced.
  (define (xml->xexpr x)
    (let* ([non-dropping-combine
            (lambda (atts body)
              (cons (assoc-sort (map attribute->srep atts))
                    body))]
           [combine (if (xexpr-drop-empty-attributes)
                        (lambda (atts body)
                          (if (null? atts)
                              body
                              (non-dropping-combine atts body)))
                        non-dropping-combine)])
      (let loop ([x x])
        (cond
          [(element? x)
           (let ([body (map loop (element-content x))]
                 [atts (element-attributes x)])
             (cons (element-name x) (combine atts body)))]
          [(pcdata? x) (pcdata-string x)]
          [(entity? x) (entity-text x)]
          [(or (comment? x) (pi? x)) x]
          [(document? x) (error 'xml->xexpr "Expected content, given ~a~nUse document-element to extract the content." x)]
          [else (error 'xml->xexpr "Expected content, given ~a" x)]))))
  
  ;; attribute->srep : Attribute -> Attribute-srep
  (define (attribute->srep a)
    (list (attribute-name a) (attribute-value a)))
  
  ;; srep->attribute : Attribute-srep -> Attribute
  (define (srep->attribute a)
    (unless (and (pair? a) (pair? (cdr a)) (null? (cddr a)) (symbol? (car a)) (string? (cadr a)))
      (error 'srep->attribute "expected (cons Symbol String) given ~a" a))
    (make-attribute 'scheme 'scheme (car a) (cadr a)))
  
  ;; xexpr->xml : Xexpr -> Content
  ;; The contract is enforced.
  (define (xexpr->xml x)
    (cond
      [(pair? x)
       (let ([f (lambda (atts body)
                  (unless (list? body)
                    (error 'xexpr->xml "expected a list of xexprs a the body in ~a" x))
                  (make-element 'scheme 'scheme (car x)
                                atts
                                (map xexpr->xml body)))])
         (if (and (pair? (cdr x)) (or (null? (cadr x)) (and (pair? (cadr x)) (pair? (caadr x)))))
             (f (map srep->attribute (cadr x)) (cddr x))
             (f null (cdr x))))]
      [(string? x) (make-pcdata 'scheme 'scheme x)]
      [(or (symbol? x) (and (integer? x) (>= x 0))) (make-entity 'scheme 'scheme x)]
      [(or (comment? x) (pi? x)) x]
      [else (error 'xexpr->xml "malformed xexpr ~s" x)]))
  
  ;; xexpr->string : Xexpression -> String
  (define (xexpr->string xexpr)
    (let ([port (open-output-string)])
      (write-xml/content (xexpr->xml xexpr) port)
      (get-output-string port)))
  
  ;; bcompose : (a a -> c) (b -> a) -> (b b -> c)
  (define (bcompose f g)
    (lambda (x y) (f (g x) (g y)))))
