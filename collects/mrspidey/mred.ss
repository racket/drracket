;; mred.ss
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
; Pulls up MrSpidey without DrScheme
; Loads all UI and analysis files into MrEd
;; ----------------------------------------------------------------------

(mred:add-version-spec 's 1)

#| MATTHEW: deleted this
(define plt-home-directory
  (let ([plt (getenv "PLTHOME")])
    (normalize-path
      (or plt
        (case (system-type)
          [(unix) "/usr/local/lib/plt/"]
          [(windows) "C:\\PLT"]
          [else (let-values ([(base name dir?)
			       (split-path (current-directory))])
		  (if (string? base)
		    base
		    (current-directory)))])))))
|#
;(reference-library "sparams.ss" "backward")

;; ------------------------------

(load-relative "pltrc-co.ss")

(load-relative "text.ss")
(load-relative "Sba/hyper.ss")
(define mrspidey:load-progress 
  (lambda (str)
    (printf "Loading ~s~n" str)
    (flush-output)))
(load-relative "Gui/load.ss")
(printf "~nAll loaded~n")

; ----------------------------------------------------------------------

(define T 
  (lambda (file)
    (send spidey run-mrspidey file)))
(define (pacwar) (T "~/Spidey/Unit/Pacwar/main.ss"))
(define (mred) (T "~/Spidey/Unit/Mred/main.ss"))
(define (zodiac) (T "~/Spidey/Unit/Zodiac/invoke.ss"))

; ----------------------------------------------------------------------



