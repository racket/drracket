;; library-set.ss
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
;;
;; Set operations implemented by lists.
;; Identity of elements is based on eq?.
;; These should probably be sped up some day.
;; ----------------------------------------------------------------------

 (define empty-set '())

 (define empty-set? null?)

 ;; construct a set
 (define set
   (lambda l
     (list->set l)))

 ;; construct a set from a list by removing duplicates
 (define list->set
   (match-lambda
    [() '()]
    [(x . y) (if (memq x y)
                 (list->set y)
                 (cons x (list->set y)))]))

 (define list->set-equal?
   (match-lambda
    [() '()]
    [(x . y) (if (member x y)
                 (list->set y)
                 (cons x (list->set y)))]))

 ;; test for membership
 (define element-of?
   (lambda (x set)
     (and (memq x set) #t)))

 (define (set-add x set)
   (if (memq x set) set (cons x set)))

 (define cardinality length)

 ;; does s2 contain s1?
 (define set<=
   (lambda (a b)
     (and (andmap (lambda (x) (memq x b)) a) #t)))

 ;; are two sets equal? (mutually containing)
 (define set-eq?
   (lambda (a b)
     (and (= (cardinality a) (cardinality b)) (set<= a b))))

 ;; unite two sets
 (define union2
   (lambda (a b)
     (if (null? b)
         a
         (foldr (lambda (x b)
                  (if (memq x b)
                      b
                      (cons x b)))
                b
                a))))

 ;; unite any number of sets
 (define union
   (lambda l
     (foldr union2 '() l)))

 (define setdiff2
   (lambda (a b)
     (if (or (null? a) (null? b))
         a
       (if (memq (car a) b)
         (setdiff2 (cdr a) b)
         (cons (car a) (setdiff2 (cdr a) b))))))

 (define setdiff
   (lambda l
     (if (null? l)
         '()
         (setdiff2 (car l) (foldr union2 '() (cdr l))))))

 (define intersect2
   (lambda (a b)
     (cond [(or (null? a) (null? b)) '()]
           [(memq (car b) a) (cons (car b) (intersect2 a (cdr b)))]
           [else (intersect2 a (cdr b))])))

 (define intersect
   (lambda l
     (if (null? l)
         '()
         (foldl intersect2 (car l) l))))


