;; link.ss
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

(define mrspidey:sba@
  (compound-unit/sig
    (import
      (INTERACTION : mrspidey:interaction^)
      (MZLIB : mzlib:unprefixed-core^)
      (WX : wx^)
      )
    (link
      [CDL :
        mrspidey:CDL^ 
        (mrspidey:CDL@ INTERACTION MZLIB WX)]
      [ZODIAC :
        mrspidey:zodiac^
        (mrspidey:zodiac@ CDL INTERACTION)]        
      [LOADEXPAND :
        mrspidey:loadexpand^
        (mrspidey:loadexpand@ CDL INTERACTION LANGUAGES ZODIAC MZLIB)]
      [HASH : 
        mrspidey:hash^
        (mrspidey:hash@ CDL INTERACTION MZLIB)]
      [KERNEL :
        mrspidey:kernel^ 
        (mrspidey:kernel@ CDL HASH KERNEL-AUX TEMPLATES)]
      [MIN :
        mrspidey:min^
        (mrspidey:all-min@ CDL INTERACTION KERNEL KERNEL-AUX 
          TEMPLATES MZLIB)]
      [TEMPLATES :
        mrspidey:templates^
        (mrspidey:templates@ CDL INTERACTION KERNEL)]
      [KERNEL-AUX :
        mrspidey:kernel-aux^
        (mrspidey:kernel-aux@ CDL INTERACTION KERNEL TEMPLATES ZODIAC)]
      [ATYPE :
        mrspidey:atype^
        (mrspidey:atype@ CDL INTERACTION KERNEL KERNEL-AUX
          MIN TYPELANG TEMPLATES
          TYPE-ENV SDL ATLUNIT MZLIB)]
      [TYPELANG :
        mrspidey:typelang^
        (mrspidey:typelang@ CDL INTERACTION KERNEL KERNEL-AUX
          TEMPLATES TYPE-ENV ATYPE CONTAINED MZLIB ZODIAC)]
      [CONTAINED :
        mrspidey:contained^
        (mrspidey:contained@ CDL KERNEL SDL)]
      [TYPE-ENV :
        mrspidey:type-env^
        (mrspidey:type-env@ CDL INTERACTION KERNEL KERNEL-AUX
          TEMPLATES MIN ATYPE ZODIAC)]
      [SDL :
        mrspidey:sdl^ 
        (mrspidey:sdl@ 
          CDL KERNEL MIN TYPELANG KERNEL-AUX TEMPLATES ATYPE
          MZLIB)]
      [LANGUAGES :
        mrspidey:languages^
        (mrspidey:languages@ CDL INTERACTION KERNEL
          TEMPLATES KERNEL-AUX 
          ATYPE TYPE-ENV TYPELANG ATENV LOADEXPAND TRAVERSE
          MZLIB ZODIAC )]
      [ATENV :
        mrspidey:atenv^
        (mrspidey:atenv@ CDL INTERACTION KERNEL KERNEL-AUX ATYPE
          ZODIAC MZLIB)]
      [TRAVERSE :
        mrspidey:traverse^
        (mrspidey:traverse@ CDL INTERACTION KERNEL MIN
          LOADEXPAND
          TEMPLATES KERNEL-AUX TYPELANG TYPE-ENV
          LANGUAGES ATYPE ATLUNIT ATENV
          ZODIAC MZLIB)]
      [ATLUNIT :
        mrspidey:atlunit^
        (mrspidey:atlunit@ CDL INTERACTION KERNEL KERNEL-AUX
          MIN LOADEXPAND TYPE-ENV
          TEMPLATES LANGUAGES ATYPE ATENV TRAVERSE
          ZA ZODIAC MZLIB WX)]
      [ZA :
        mrspidey:za^ 
        (mrspidey:za@ CDL INTERACTION KERNEL TEMPLATES 
          TYPE-ENV TYPELANG ATYPE MZLIB)]
      [PROGRAM :
        mrspidey:program^
        (mrspidey:program@ CDL INTERACTION 
          KERNEL KERNEL-AUX TYPE-ENV
          LOADEXPAND TRAVERSE
          TEMPLATES ATENV ATYPE LANGUAGES
          ZODIAC MZLIB)]
      [CHECKS :
        mrspidey:calc-checks^
        (mrspidey:calc-checks@ CDL INTERACTION LOADEXPAND 
          KERNEL KERNEL-AUX
          TYPELANG TEMPLATES ATYPE SDL
          ZODIAC MZLIB)]
      [DRIVER :
        mrspidey:driver^
        (mrspidey:driver@ CDL INTERACTION KERNEL
          TEMPLATES TYPELANG TYPE-ENV ATYPE ATENV
          PROGRAM CHECKS LANGUAGES ZODIAC)]
      [HYPER :
        mrspidey:hyper^
        (mrspidey:hyper@ CDL INTERACTION LOADEXPAND
          PROGRAM CHECKS TEMPLATES KERNEL KERNEL-AUX ATYPE MZLIB)]
      )
    (export 
      (open DRIVER)
      (open CDL)
      (open ATYPE)
      (open HYPER)
      (open KERNEL)
      (open CHECKS)
      (open LANGUAGES)
      (unit ZODIAC zodiac))))

