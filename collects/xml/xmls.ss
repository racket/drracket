(require-library "functios.ss")
(require-library "invoke.ss")

(define-signature xml-structs^
  ((struct document (prolog element misc))
   (struct comment (text))
   (struct prolog (misc dtd))
   (struct element (name attributes content))
   (struct attribute (name value))
   (struct pi (target-name instruction))
   (struct source (start stop))
   (struct pcdata (string))
   (struct entity (text))
   content?))

(define-signature writer^ (write-xml display-xml write-xml/content display-xml/content empty-tag-shorthand html-empty-tags))
(define-signature reader^ (read-xml read-comments collapse-whitespace))

(define-signature xexpr^ (xml->xexpr xexpr->xml xexpr->string xexpr-drop-empty-attributes))
(define-signature extra-xexpr^ ((open xexpr^) assoc-sort bcompose attribute->srep))
(define-signature space^ (eliminate-whitespace))
(define-signature xml^ ((open xml-structs^) (open reader^) (open writer^) (open xexpr^) (open space^)))

