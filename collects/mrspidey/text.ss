;; mrspidey-mred.ss
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------

(load-relative "handlers.ss")
(load-relative "macros.ss")

(load/use-compiled
  (let ([p (build-path (current-load-relative-directory) "Zodiac" "load.ss")])
    (if (file-exists? p)
      p
      (build-path (current-load-relative-directory) ".." "zodiac" "load.ss"))))
(load-relative "Sba/load.ss")
(load-relative "Sba/test-sba.ss")
;;(load-relative "Sba/devel.ss")

(printf "~nmrspidey-text loaded~n")

;; --- Pull up a default language
(st:language 'DrScheme)

(define (t) (st: "/home/cormac/Spidey/Test/t.ss"))

