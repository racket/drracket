; zod-extra.ss
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

(define mrspidey:zodiac:parameters@
  (unit/sig plt:parameters^
            (import)
            (define case-sensitive? #t)
            (define unmatched-cond/case-is-error? #t)
            (define allow-set!-on-undefined? #t)
            (define allow-improper-lists? #t)
            (define check-syntax-level 'advanced)
            ;(define allow-internal-defines? #f)
            ))

(define mrspidey:zodiac:interface@
  (unit/sig zodiac:interface^
    (import mrspidey:interaction^)
    (define default-error-handler
      (lambda (keyword)
        (lambda (where fmt-spec . args)
          (apply mrspidey:internal-error
            keyword fmt-spec args))))
    (define internal-error
      (lambda (where fmt-spec . args)
        (let ([msg 
                (parameterize ([print-struct #t])
                  (string-append "Syntax error: " 
                    (apply format fmt-spec args)))])
          (if #t                        ;(zodiac:zodiac? where)
            (mrspidey:error msg where)                    
            (mrspidey:error msg)))))
    (define static-error
      (lambda (where fmt-spec . args)
        (let ([msg
                (parameterize ([print-struct #t])
                  (string-append "Syntax error: " 
                    (apply format fmt-spec args)))])
          (if #t                        ;(zodiac:zodiac? where)
            (mrspidey:error msg where)                    
            (mrspidey:error msg)))))
    (define dynamic-error
      (default-error-handler 'zodiac-run-time))))

;; ----------------------------------------------------------------------

(define mrspidey:zodiac-aux@
  (unit/sig
    mrspidey:zodiac-aux^
    (import
      mrspidey:CDL^
      (zodiac : zodiac:system^))
    (include "zod-aux.ss")))

;; ----------------------------------------------------------------------

(load-relative "zod-link.ss")




