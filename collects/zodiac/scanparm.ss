;;
;;  zodiac:scanner-parameters@
;;  $Id: scanparm.ss,v 1.5 1997/12/03 19:20:21 robby Exp $
;;
;;  Scanner/Reader Parameters.
;;
;;  The scan values (outside make-scanner) mostly can
;;  be reset at will.  But don't use letters, digits, #, etc.
;;  The parameters inside make-scanner should not be reset.
;;
;;  The char lists can be either chars or ints.
;;

(unit/sig  zodiac:scanner-parameters^
  (import  zodiac:structures^)

  (define disallow-untagged-inexact-numbers (make-parameter #f))

   ;; Only #\space and #\newline are always builtin,
   ;; so we specify the rest with ascii codes.

   (define  space    #\space)
   (define  nul       0)
   (define  backsp    8)
   (define  tab       9)
   (define  newline  10)
   (define  vtab     11)
   (define  page     12)
   (define  return   13)
   (define  rubout  127)
   
   (define scan:paren-relation
     (let ((base '((#\( #\)))))
       (let ((w/-brackets (if (read-square-bracket-as-paren)
			    (cons '(#\[ #\]) base)
			    base)))
	 (let ((w/-braces (if (read-curly-brace-as-paren)
			    (cons '(#\{ #\}) w/-brackets)
			    w/-brackets)))
	   w/-braces))))

   (define scan:self-delim-symbols
     (let ((base '()))
       (let ((w/-brackets (if (read-square-bracket-as-paren)
			    base
			    (append '(#\[ #\]) base))))
	 (let ((w/-braces (if (read-curly-brace-as-paren)
			    w/-brackets
			    (append '(#\{ #\}) w/-brackets))))
	   w/-braces))))

   (define  scan:newline-list  (list  newline  return))
   (define  scan:tab-list      (list  tab))
   (define  scan:whitespace-list
     (list  space  tab  newline  vtab  page  return))

   (define  scan:delim-list
     (append  scan:whitespace-list
              (map  car   scan:paren-relation)
              (map  cadr  scan:paren-relation)
	      scan:self-delim-symbols
	      (list  #\;  #\"  #\,  #\'  #\` )))
   
   (define  scan:special-char-list
     `(("space"      ,space)
       ("newline"    ,newline)
       ("linefeed"   ,newline)
       ("nul"        ,nul)
       ("null"       ,nul)
       ("backspace"  ,backsp)
       ("tab"        ,tab)
       ("vtab"       ,vtab)
       ("page"       ,page)
       ("return"     ,return)
       ("rubout"     ,rubout)))
   
   (define default-initial-location  (make-location 1 1 0 'nofile))
   (define scan:def-first-col  1)
   (define scan:def-vect-val  0)
 )

