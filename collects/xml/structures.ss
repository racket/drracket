(unit/sig xml-structs^
  (import)
  
  ;; Location ::= Nat | Symbol
  ;; Source ::= (make-source Location Location)
  (define-struct source (start stop))
  
  ;; Document ::= (make-document Prolog Element (listof Misc))
  (define-struct document (prolog element misc))
  
  ;; Prolog ::= (make-prolog (listof Misc) #f)
  (define-struct prolog (misc dtd))
  
  ;; Element ::= (make-element Location Location Symbol (listof Attribute) (listof Content))
  (define-struct (element struct:source) (name attributes content))
  
  ;; Attribute ::= (make-attribute Location Location Symbol String)
  (define-struct (attribute struct:source) (name value))
  
  ;; Pcdata ::= (make-pcdata Location Location String)
  (define-struct (pcdata struct:source) (string))
  
  ;; Content ::= Pcdata  
  ;;          |  Element
  ;;          |  Entity
  ;;          |  Misc
  
  ;; Misc ::= Comment
  ;;       |  Processing-instruction
  
  ;; Entity ::= (make-entity Location Location (U Nat Symbol))
  (define-struct (entity struct:source) (text))
  
  ;; Processing-instruction ::= (make-pi Location Location String (list String))
  ;; also represents XMLDecl
  (define-struct (pi struct:source) (target-name instruction))
  
  ;; Comment ::= (make-comment String)
  (define-struct comment (text))
  
  ;; content? : TST -> Bool
  (define (content? x)
    (or (pcdata? x) (element? x) (entity? x) (comment? x) (pi? x))))