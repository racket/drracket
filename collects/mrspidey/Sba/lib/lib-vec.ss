;; library-vec.ss
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

(define (vector-copy v)
  (let* ( [l (vector-length v)]
          [w (make-vector l)])
    (for i 0 l (vector-set! w i (vector-ref v i)))
  w))

;; vector map
(define vector-map1
  (lambda (f v)
    (let* ([l (vector-length v)]
           [w (make-vector l)])
      (for i 0 l
           (vector-set! w i (f (vector-ref v i))))
      w)))

(define vector-map2
  (lambda (f v1 v2)
    (let* ([l (vector-length v1)]
           [w (make-vector l)])
      (assert (= l (vector-length v2)))
      (for i 0 l
           (vector-set! w i (f (vector-ref v1 i) (vector-ref v2 i))))
      w)))

(define vector-map
  (case-lambda
   [(f v) (vector-map1 f v)]
   [(f v1 v2) (vector-map2 f v1 v2)]))

;; for-each over a vector, but also pass f a 0-based index
(define vector-for-each-with-n
  (lambda (f v)
    (for i 0 (vector-length v)
         (f (vector-ref v i) i))))

(define vector-for-each
  (case-lambda
    [(f v)
      (for i 0 (vector-length v)
        (f (vector-ref v i)))]
    [(f v1 v2)
      (assert (= (vector-length v1) (vector-length v2)) 'vector-for-each)
      (for i 0 (vector-length v1)
        (f (vector-ref v1 i) (vector-ref v2 i)))]))

(define (vector-andmap1 f v)
  (let ([l (vector-length v)])
    (recur loop ([i 0])
      (or (= i l)
          (and (f (vector-ref v i)) (loop (add1 i)))))))

(define (vector-andmap2 f v1 v2)
  (let ([l (vector-length v1)])
    (assert (= l (vector-length v2)))
    (recur loop ([i 0])
      (or (= i l)
          (and (f (vector-ref v1 i) (vector-ref v2 i))
               (loop (add1 i)))))))

(define vector-andmap
  (match-lambda*
   [(f v) (vector-andmap1 f v)]
   [(f v1 v2) (vector-andmap2 f v1 v2)]))


(define (vector-ormap1 f v)
  (let ([l (vector-length v)])
    (recur loop ([i 0])
      (and (not (= i l))
           (or (f (vector-ref v i)) (loop (add1 i)))))))

(define (vector-ormap2 f v1 v2)
  (let ([l (vector-length v1)])
    (assert (= l (vector-length v2)))
    (recur loop ([i 0])
      (and (not (= i l))
           (or (f (vector-ref v1 i) (vector-ref v2 i))
               (loop (add1 i)))))))

(define vector-ormap
  (match-lambda*
   [(f v) (vector-ormap1 f v)]
   [(f v1 v2) (vector-ormap2 f v1 v2)]))


(define (vector-zero! p)
  (for i 0 (vector-length p) (vector-set! p i 'zerod!)))
