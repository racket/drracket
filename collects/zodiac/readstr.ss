;;
;; zodiac:reader-structs@
;; $Id$
;;
;; Reader's subtree of the hierarchy.
;;
;;  zodiac (origin start finish)
;;    read (object)
;;       scalar
;;          symbol (orig-name marks)
;;          number
;;          string
;;          boolean
;;          char
;;          box
;;          type-symbol
;;          external
;;       sequence (length)
;;          list (marks)
;;          vector
;;          improper-list (period marks)
;;

(unit/sig  zodiac:reader-structs^
  (import  zodiac:structures^)

  (define-struct  (read  struct:zodiac)  (object))

  (define-struct  (scalar  struct:read) ())
  (define-struct  (symbol    struct:scalar) (orig-name marks))
  (define-struct  (number    struct:scalar) ())
  (define-struct  (string    struct:scalar) ())
  (define-struct  (boolean   struct:scalar) ())
  (define-struct  (char      struct:scalar) ())
  (define-struct  (box       struct:scalar) ())
  (define-struct  (type-symbol  struct:scalar) ())
  (define-struct  (external  struct:scalar) ())

  (define-struct  (sequence  struct:read) (length))
  (define-struct  (list           struct:sequence) (marks))
  (define-struct  (vector         struct:sequence) ())
  (define-struct  (improper-list  struct:sequence) (period marks))
  )

