;;
;; $Id: zsigs.ss,v 1.9 1998/03/05 18:30:42 mflatt Exp $
;;
;; The signatures for all scanner/reader units.
;;

;;
;; Top-level zodiac structures (outside the hierarchy)
;; and base of zodiac hierarchy.
;;

(define-signature  zodiac:structures^
  ((struct  origin    (who  how))
   (struct  location  (line  column  offset  file))
   (struct  period    (location))
   (struct  eof       (location))
   (struct  zodiac    (origin  start  finish))))

;;
;; Scanner's subtree of the hierarchy.
;;
;;  zodiac (origin start finish)
;;    scanned
;;       token (object type)
;;

(define-signature  zodiac:scanner-structs^
  ((struct  scanned  ())
   (struct  token    (object  type))))

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
;;          list
;;          vector
;;          improper-list (period)
;;

(define-signature  zodiac:reader-structs^
  ((struct  read  (object))
   (struct  scalar  ())
   (struct  symbol  (orig-name marks))
   (struct  number  ())
   (struct  string  ())
   (struct  boolean ())
   (struct  char    ())
   (struct  box     ())
   (struct  type-symbol ())
   (struct  external  ())
   (struct  sequence  (length))
   (struct  list  (marks))
   (struct  vector  ())
   (struct  improper-list  (period marks))))

;;
;;  Scanner/Reader Parameters.
;;
;;  The scan values (outside make-scanner) mostly can
;;  be reset at will.  But don't use letters, digits, #, etc.
;;  The parameters inside make-scanner should not be reset.
;;
;;  The char lists can be either chars or ints.
;;

(define-signature  zodiac:scanner-parameters^
  (disallow-untagged-inexact-numbers
   scan:paren-relation
   scan:self-delim-symbols
   scan:newline-list
   scan:tab-list
   scan:whitespace-list
   scan:delim-list
   scan:special-char-list
   default-initial-location
   scan:def-first-col
   scan:def-vect-val))

;;
;; The scanner & reader units just export one function.
;;

(define-signature  zodiac:scanner-code^  (scan))
(define-signature  zodiac:reader-code^   (read allow-improper-lists allow-reader-quasiquote))

