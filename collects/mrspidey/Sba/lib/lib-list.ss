;; library-list.ss
;; ----------------------------------------------------------------------

;; map from left to right
(define mapLR
  (lambda (f l)
    (match l
      [() '()]
      [(x . y) (let ([v (f x)]) (cons v (mapLR f y)))]
      [l (error 'mapLR "Bad list ~s" l)])))

;; map from right to left
(define mapRL
  (lambda (f l)
    (match l
      [() '()]
      [(x . y) (let ([v (mapRL f y)]) (cons (f x) v))])))

(define foldl-with-n
  (lambda (f i l)
    (recur loop ([l l][acc i][n 0])
      (match l
        [() acc]
        [(x . y) (loop y (f x n acc) (add1 n))]))))

;; fold for a 2-argument function
;; right operand of f is accumulator
(define foldr2
  (lambda (f i l1 l2)
    (recur loop ([l1 l1][l2 l2])
      (match (list l1 l2)
        [(() ()) i]
        [((x1 . y1) (x2 . y2)) (f x1 x2 (loop y1 y2))]))))

;; filter elements out of a list by a predicate
(define filter
  (lambda (p l)
    (match l
      [() '()]
      [(x . y) (if (p x) (cons x (filter p y)) (filter p y))])))

;; filter and map left to right
(define filter-map
  (lambda (p l)
    (match l
      [() '()]
      [(x . y)
       (match (p x)
         [#f (filter-map p y)]
         [x (cons x (filter-map p y))])])))

;; filter and map left to right, and return (filtered-list . unfiltered)

(define filter-map-split
  (lambda (p l)
    (recur loop ([done-filtered '()][done-unfiltered '()][l l])
      (match l
        [() (values done-filtered done-unfiltered)]
        [(x . y)
         (match (p x)
           [#f (loop done-filtered (cons x done-unfiltered) y)]
           [x  (loop (cons x done-filtered) done-unfiltered y)])]))))

;; last element of a list
(define rac
  (lambda (l)
    (match l
      [(last) last]
      [(_ . rest) (rac rest)])))

;; all but the last element of a list
(define rdc
  (lambda (l)
    (match l
      [(_) '()]
      [(x . rest) (cons x (rdc rest))])))

;; map left to right over a list, but also pass f a 0-based index
(define map-with-n
  (lambda (f l)
    (recur loop ([l l][n 0])
      (match l
        [() '()]
        [(x . y) (let ([v (f x n)]) (cons v (loop y (+ 1 n))))]
        [l (error 'map-with-n "Bad list ~s" l)]))))

;; for-each, but also pass f a 0-based index
(define for-each-with-n
  (lambda (f l)
    (recur loop ([l l][n 0])
      (match l
        [() '()]
        [(x . y) (f x n) (loop y (+ 1 n))]))))

;; map on a (possibly improper) list
(define map-ilist
  (lambda (f l)
    (recur loop ([l l])
      (match l
        [() '()]
        [(x . y) (cons (f x) (loop y))]
        [x (f x)]))))

;; length on a (possibly improper) list
(define length-ilist
  (match-lambda
    [(x . y) (add1 (length-ilist y))]
    [_ 0]))

(define improper?
  (match-lambda
    [(x . y) (improper? y)]
    [() #f]
    [_ #t]))

(define (flatten-ilist l)
  (cond [(null? l) '()]
        [(pair? l) (cons (car l) (flatten-ilist (cdr l)))]
        [else (list l)]))

;; map a binary function down 2 lists, left to right
(define map2
  (lambda (f a b)
    (match (cons a b)
      [(() . ())
       '()]
      [((ax . ay) . (bx . by))
       (let ([v (f ax bx)]) (cons v (map2 f ay by)))]
      [else (error 'map2 "lists differ in length")])))

; map over a list of lists

(define (mapmap f ll) (map (lambda (l) (map f l)) ll))

;; interate a binary function down 2 lists, left to right
(define for-each2
  (lambda (f a b)
    (match (cons a b)
      [(() . ())
       (void)]
      [((ax . ay) . (bx . by))
       (f ax bx)
       (for-each2 f ay by)]
      [else (error 'for-each2 "lists differ in length")])))

;; andmap for 2 lists
(define andmap2
  (lambda (f a b)
    (match (cons a b)
      [(() . ())
       #t]
      [((ax) . (bx))
       (f ax bx)]
      [((ax . ay) . (bx . by))
       (and (f ax bx) (andmap2 f ay by))]
      [else (error 'andmap2 "lists differ in length")])))

;; andmap for 2 lists, fail on inequal lengths
(define andmap2len
  (lambda (f a b)
    (match (cons a b)
      [(() . ()) 
       #t]
      [((ax) . (bx))
       (f ax bx)]
      [((ax . ay) . (bx . by))
       (and (f ax bx) (andmap2len f ay by))]
      [else #f])))
;(define andmap andmap2)

;; ormap for 2 lists
(define ormap2
  (lambda (f a b)
    (match (cons a b)
      [(() . ())
       #f]
      [((ax) . (bx))
       (f ax bx)]
      [((ax . ay) . (bx . by))
       (or (f ax bx) (ormap2 f ay by))]
      [else (error 'ormap2 "lists differ in length")])))

;; make a list containing n copies of e
(define list-n-copies
  (lambda (n e)
    (if (zero? n)
	'()
	(cons e (list-n-copies (sub1 n) e)))))

(define (count p l)
  (recur loop ([c 0][l l])
    (cond
     [(null? l) c]
     [(p (car l)) (loop (add1 c) (cdr l))]
     [else (loop c (cdr l))])))

(define (index l x)
  (recur loop ([l l][i 0])
    (cond
     [(null? l) #f]
     [(eq? x (car l)) i]
     [else (loop (cdr l) (add1 i))])))

(define (get-prefix l1 l2)
    (if (eq? l1 l2)
        '()
        (cons (car l1) (get-prefix (cdr l1) l2))))

(define (mklist n)
  (if (zero? n)
      '()
      (cons n (mklist (sub1 n)))))

(define (nth l n)
  (if (zero? n)
      (car l)
      (nth (cdr l) (sub1 n))))


; Takes an atom and a list and returns the position of the atom in the list
(define list-pos
  (lambda (a l)
    (recur loop ([l l][i 0])
      (cond                               
       [(null? l) #f]
       [(eqv? a (car l)) i]
       [else (loop (cdr l) (add1 i))]))))

; Takes an atom and a list and returns the position of the atom in the list
; uses equal?, returns #f if no match
(define list-pos-equal
  (lambda (a l)
    (recur loop ([l l][n 0])
      (cond
       [(null? l) #f]
       [(equal? a (car l)) n]
       [else (loop (cdr l) (add1 n))]))))

;; Returns first element in set satisfying a predicate, or #f
(define (find p l)
  (recur loop ([l l])
    (cond
     [(null? l) #f]
     [(p (car l)) (car l)]
     [else (loop (cdr l))])))


