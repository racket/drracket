;;
;; zodiac:structures@
;; $Id$
;;
;; Top-level zodiac structures (outside the hierarchy)
;; and base of zodiac hierarchy.
;;

(unit/sig  zodiac:structures^
  (import)

  (define-struct  origin    (who  how))
  (define-struct  location  (line  column  offset  file))
  (define-struct  period    (location))
  (define-struct  eof       (location))

  (define-struct  zodiac    (origin  start  finish))
  )

