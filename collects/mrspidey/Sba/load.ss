;; Sba/load.ss
;; Loads analysis, assumes zodiac, ../aux.ss, ../macros.ss already loaded
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

(load-relative "sigs.ss")

;; ----------------------------------------------------------------------
;; Library stuff

(load-relative (build-path "lib" "lib-para.ss"))
(load-relative (build-path "lib" "lib-list.ss"))
(load-relative (build-path "lib" "lib-vec.ss"))
(load-relative (build-path "lib" "lib-set.ss"))
(load-relative (build-path "lib" "lib-misc.ss"))
(load-relative (build-path "lib" "env.ss"))
(load-relative "config.ss")
(load-relative "debug.ss")

;; ----------------------------------------------------------------------
;; Extended Zodiac

(load-relative "zod-extra.ss")
(invoke-open-unit/sig
 mrspidey:zodiac@ 
 zodiac
 mrspidey:CDL^ mrspidey:interaction^)

;; ---------------------------------------------------------------------
;; Front End Stuff

(load-relative "ldexpand.ss")

;; ======================================================================
;; Kernel stuff

(load-relative "hash.ss")
(load-relative "kernel.ss")
(load-relative (build-path "min" "min.ss"))
(load-relative (build-path "min" "nonempty.ss"))
(load-relative (build-path "min" "min-live.ss"))
(load-relative (build-path "min" "livefewe.ss"))
(load-relative (build-path "min" "hopcroft.ss"))
(load-relative (build-path "min" "min-dfa.ss"))

;; ----------------------------------------------------------------------
;; top level stuff

(load-relative "template.ss")
(load-relative "kern-aux.ss")
(load-relative "typelang.ss")
(load-relative "contain.ss")
(load-relative "type-env.ss")
(load-relative "sdl.ss")
(load-relative "language.ss")

;; ----------------------------------------------------------------------
;; Traversal

(load-relative "atype.ss")
(load-relative "atenv.ss")
(load-relative "atlunit.ss")
(load-relative "traverse.ss")

;; ----------------------------------------------------------------------

(load-relative "za.ss")
(load-relative "program.ss")
(load-relative "checks.ss")
(load-relative "driver.ss")
(load-relative "results.ss")

;; ----------------------------------------------------------------------

(define (t) (st: "../test/t.ss"))
;(load "test-sba.ss")
;(load "devel.ss")
;(load "poly.ss")
