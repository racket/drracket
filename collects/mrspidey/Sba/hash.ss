;; hash.ss
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

(define-struct hash-table-state 
  (hash-table-size entries-in-table
    resize-table-size entry->hash-fn
    hash-table))

 ;;------------------------------------------------------------

 (define prime1 83)
 (define prime2 1789)
 (define default-hash-table-size (* 32 1024)) 
 (define resize-table-fraction 0.6)

 ;;------------------------------------------------------------

 (define hash-table-size   0)
 (define entries-in-table  0)
 (define resize-table-size 0)
 (define entry->hash-fn    (void))
 (define hash-table        (void))

 ;;------------------------------------------------------------

 (define (init-hash-table entry->hash size)
   (set! hash-table-size (if (zero? size) default-hash-table-size size))
   (set! entries-in-table 0)
   (set! resize-table-size 
       (inexact->exact 
         (round (* hash-table-size resize-table-fraction))))
   (set! entry->hash-fn entry->hash)
   (set! hash-table (make-vector hash-table-size '())))

 (define (capture-hash-table-state)
   (make-hash-table-state hash-table-size entries-in-table
                          resize-table-size entry->hash-fn
                          hash-table))

(define restore-hash-table-state!
   (match-lambda
    [($ hash-table-state t1 t2 t3 t4 t5)
     (set! hash-table-size t1)
     (set! entries-in-table t2)
     (set! resize-table-size t3)
     (set! entry->hash-fn t4)
     (set! hash-table t5)]))

(define free-hash-table-state!
  (match-lambda
    [($ hash-table-state t1 t2 t3 t4 t5)
      (assert (not (eq? t5 hash-table)))
      (vector-zero! t5)]))

(define (prompt-hash-table-state)
  (list (capture-hash-table-state)
    (vector-length hash-table)
    (recur loop ([i (sub1 (vector-length hash-table))])
      (if (< i 0)
        '()
        (if (null? (vector-ref hash-table i))
          (loop (sub1 i))
          (cons (cons i (vector-ref hash-table i)) (loop (sub1 i))))))))

(define unprompt-hash-table-state!
  (match-lambda
    [(state vec-size vec-entries)
      (restore-hash-table-state! state)
      (for i 0 (vector-length hash-table) (vector-set! hash-table i '()))
      (for-each
        (match-lambda
          [(i . elem) (vector-set! hash-table i elem)])
        vec-entries)]))

 ;;------------------------------------------------------------


 ;; (define (hash-fn n1 n2)
 ;;  (fxlogand (+ (* n1 prime1) (* n2 prime2)) (sub1 hash-table-size)))

 (define (hash-fn n1 n2)
   (modulo (+ (* n1 prime1) (* n2 prime2)) hash-table-size))

 (define (hash-fn* n*) (foldl hash-fn 0 n*))

 (define (add-entry h entry)
   (vector-set! hash-table h (cons entry (vector-ref hash-table h)))
   (set! entries-in-table (add1 entries-in-table))
   '(if (zero? (mod entries-in-table 10000))
        (printf "Entries ~s~n" entries-in-table))
   (when (>= entries-in-table resize-table-size)
     (resize-hash-table)))

 (define (resize-hash-table)
   (set! hash-table-size (* 2 hash-table-size))
   (let ( [old hash-table]
          [s (format "Resizing hash table to ~s" hash-table-size)])
     (mrspidey:progress s '...)
     (flush-output)
     (set! hash-table (make-vector hash-table-size '()))
     (set! resize-table-size 
         (inexact->exact 
          (round (* hash-table-size resize-table-fraction))))
     (for i 0 (vector-length old)
          (map 
           (lambda (entry)
             (let ([h (entry->hash-fn entry)])
               (vector-set! hash-table h
                            (cons entry (vector-ref hash-table h)))))
           (vector-ref old i)))
     (mrspidey:progress s 'done)
     ;;(show-stat-small)
     ))

 (define (hash-table-list h) (vector-ref hash-table h))

 (define (hash-find h ok?)
   (recur loop ([l (vector-ref hash-table h)])
     (cond [(null? l) #f]
           [(ok? (car l)) (car l)]
           [else (loop (cdr l))])))

 (define (hash-table-info)
   (list hash-table-size entries-in-table
         (recur loop ([i 0][c 0])
           (if (= i hash-table-size) c
               (let ([l (length (vector-ref hash-table i))])
                 (loop (add1 i)
                       (+ c (max 0 (- l 1)))))))))

 ;;------------------------------------------------------------

