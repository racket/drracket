; $Id: load.ss,v 1.20 1998/04/21 02:59:55 robby Exp $

(require-library "macro.ss")
(require-library "cores.ss")

(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")

; All this stuff needs to be disappeared.

(define zodiac:system@
  (require-library-unit/sig "link.ss" "zodiac"))
