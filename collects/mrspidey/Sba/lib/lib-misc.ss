; library-misc.ss
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

 (define symbol-append
   (lambda l
     (string->symbol
      (apply string-append (map (lambda (x) (#%format "~a" x)) l)))))

 (define (get-cpu-time str thunk)
   (match-let
       ([(t . r) (get&return-cpu-time thunk)])
     (printf "~a ~ss~n" str (/ t 1000.0))
     r))

 (define (get&return-cpu-time thunk)
   (let* ([s1 (- (current-process-milliseconds) (current-gc-milliseconds))]
          [r  (thunk)]
          [s2 (- (current-process-milliseconds) (current-gc-milliseconds))]
          [t (- s2 s1)])
     (cons t r)))

 (define (make-timer) (cons 0 0))
 (define (clear-timer! c) (set-car! c 0) (set-cdr! c 0))

 (define (record-time timer thunk)
   (match-let
       ([(t . r) (get&return-cpu-time (lambda () (call-with-values thunk list)))])
     (set-car! timer (add1 (car timer)))
     (set-cdr! timer (+ t (cdr timer)))
     (apply #%values r)))

 (define (strip-string s c)
   ;; Strips leading chars off s, up to and including c
   (recur loop ([s (string->list s)])
     (cond
      [(null? s) ""]
      [(char=? (car s) c) (list->string (cdr s))]
      [else (loop (cdr s))])))

 ;; pad on left
 (define padl
   (lambda (arg n)
     (let ((s (format "~a" arg)))
       (recur loop ((s s))
         (if (< (string-length s) n)
             (loop (string-append " " s))
             s)))))

 ;; pad on right
 (define padr
   (lambda (arg n)
     (let ((s (format "~a" arg)))
       (recur loop ((s s))
         (if (< (string-length s) n)
             (loop (string-append s " "))
             s)))))

 (define chop-number
   (lambda (x n)
     (substring (format "~s00000000000000000000" x) 0 (- n 1))))

 ;; Is the first string a substring of the second string?
 ;; 
 (define substring?
   (lambda (s1 s2)
     (let ([l1 (string-length s1)][l2 (string-length s2)])
       (let loop ([i 0])
         (cond
          [(>= (+ i l1) l2)  #f]
          [(string=? (substring s2 i (+ i l1)) s1) #t]
          [else (loop (add1 i))])))))

                                        ; Returns the part of the string after the last slash
                                        ;
 (define get-base-name
   (lambda (filename)
     (let ([len (string-length filename)])
       (let loop ([i 0][fname-start 0])
         (cond
          [(= i len) (substring filename fname-start len)]
          [(char=? (string-ref filename i) #\/)
           (loop (add1 i) (add1 i))]
          [else (loop (add1 i) fname-start)])))))

                                        ; removes an object fro a list destructively using eqv?
                                        ;
(define remv!
  (lambda (obj l)
    (let ([head l])
      (if (eqv? obj (car l))
        (if (pair? l)
          (cdr l)
          '())
        (let loop ([l l])
          (cond
            [(null? l) head]
            [(null? (cdr l)) head]
            [(eqv? obj (cadr l))
              (if (pair? (cdr l))
                (set-cdr! l (cddr l))
                (set-cdr! l '()))
              head]
            [ else (loop (cdr l))]))))))

                                        ; takes a string, a position (i) and a char and resturns a substring of the characters
                                        ; from the position to the char
                                        ;
(define (char-find str i  char)
  (apply string 
         (let loop ([i i])
           (if (< i (string-length str))
               (let ([c (string-ref str i)]) 
                 (if (char=?  c char)
                     '()
                     (cons c (loop (add1 i)))))
               '()))))
 ;;

(define (file-newer f1 f2)
  (let ([s1 (file-modify-seconds f1)]
        [s2 (file-modify-seconds f2)])
    (and (number? s1) 
         (number? s2)
         (> s1 s2))))
    

;; curried eq?

(define eqc?
  (lambda (x)
    (lambda (y)
      (eq? x y))))

;; ----------------------------------------------------------------------

