;; driver.ss - driver file for text version
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

;; ----------------------------------------------------------------------

'(define st:defconstructor add-default-constructor!)
'(define st:defprim add-default-primitive!)
'(define st:type-alias default-constructor-alias!)
'(define (st:deftype name nutype) 
   (install-input-type-expander!
    (lambda (type)
      (if (eq? type name) nutype type))))

(define st:set-debug set-debug-flag)

; ----------------------------------------------------------------------

(define st:analyze
  (lambda (file)
    (analyze-program file)
    (void)))

(define st:
  (lambda (file)
    (let ([defs (analyze-program file)])
      (calc-checks defs)
      (void))))


; ----------------------------------------------------------------------

(define (st:type-fn . args)
  ;;(when (null? defs-bind)
  ;;  (error 'st:type "Checking not done yet, use st:"))
  (let* ([show
           (lambda (name)
             (list (zodiac:binding-var name)
               ':
               (FlowType->SDL (atenv:lookup global-def-env name))))]
          [show*
            (lambda (b*)
              (map show b*)
              ;;(for-each (lambda (b) (pretty-print (show b))) b*)
              )])

    (if (null? args)
      ;; Show all
      (show* (atenv:domain global-def-env))
      ;; Show selectively
      (show*
        (filter
          (lambda (name) (memq (zodiac:binding-var name) args))
          (atenv:domain global-def-env))))))

;; ----------------------------------------------------------------------

(define st:help
  (lambda ()
    ;(printf "Commands for SBA Soft Scheme Version ~a~%" st:version)
    (printf "  (st:analyze file)          analyze file~n")
    (printf "  (st:check file)                 type check file~%")
;    (printf "  (st:write file [output])       type check file and write it~%")
    (printf "  (st:type [definition ...])     show type of top level defs~n")
;    (printf "  (st:ltype var)                 show type of internal vars~n")
;    (printf "  (st:why N)                     prints cause of check N~n")
    (printf "  (st:control [param [value]])   show or change parameters~n")
    (printf "  (st:help)                      prints this message~%")))

; ----------------------------------------------------------------------
;  (st:check    file (output))    type check file
;  (st:bench    file)             execute type checked file fast
;  (st:run      file)             execute type checked file
; ----------------------------------------------------------------------

(st:language 'DrScheme)
;(st:language 'MrEd)

;(printf "DrScheme selected~n")
