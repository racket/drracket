;; library-paras.ss
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

(define make-parameter-list
  (case-lambda
   [(default list-opt)
    (make-parameter-list default list-opt 
                         (lambda (x) (void))
                         (lambda (x) #f))]
   [(default list-opt call)
    (make-parameter-list default list-opt call
                         (lambda (x) #f))]
   [(default list-opt call also-ok?)
    (let* ([list-opt
            (map
             (match-lambda
              [(tag (? string? name) (? string? help)) (list tag name help)]
              [(tag (? string? name)) (list tag name "")]
              [(or (tag) (tag "") tag) (list tag (format "~a" tag) "")])
             list-opt)]
           [list-tag (map car list-opt)])
                                        ;(assert (memq default list-tag))
      (let ([current default])
        (call current)
        (match-lambda*
         [() current]
         [('?) list-opt]
         [(x)
          (unless (or (member x list-tag) (also-ok? x))
            (error 
              'parameter
              "Parameter argument ~s is not one of ~s" x list-tag))
          (set! current x)
          (call current)])))]))

(define (make-parameter-boolean x)
  (make-parameter-list x 
    (list
      (list #f "Off" "")
      (list #t "On" ""))))

'(define (make-parameter-integer x)
  (make-parameter x
                  (lambda (x)
                    (unless (integer? x)
                      (error 'parameter "Must be an integer"))
                    x)))

(define (make-parameter-integer x)
  (unless (integer? x) (error 'parameter "Must be an integer"))
  (case-lambda
    [() x]
    [(y)
      (unless (integer? y) (error 'parameter "Must be an integer"))
      (set! x y)]))
