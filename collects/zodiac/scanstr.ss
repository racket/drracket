;;
;; zodiac:scanner-structs@
;; $Id$
;;
;; Scanner's subtree of the hierarchy.
;;
;;  zodiac (origin start finish)
;;    scanned
;;       token (object type)
;;

(unit/sig  zodiac:scanner-structs^
  (import  zodiac:structures^)

  (define-struct  (scanned  struct:zodiac) ())
  (define-struct  (token  struct:scanned)  (object  type))
  )

