;; Sba/lib/main.ss
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

;; elaboration-time context:
;; mrspidey signatures

(unit/sig mrspidey:library^
  (import mrspidey:interaction^ mzlib:unprefixed-core^)
  (include "lib-para.ss")
  (include "lib-list.ss")
  (include "lib-vec.ss")
  (include "lib-set.ss")
  (include "lib-misc.ss")
  (include "env.ss"))
