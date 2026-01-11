#lang racket/base

(require racket/class
         racket/function
         racket/list
         racket/contract
         string-constants)

(provide (struct-out defn)
         get-definitions
         get-define-popup-info
         (struct-out define-popup-info))

;; defn = (make-defn number string number number)
(define-struct defn (indent name start-pos end-pos)
  #:mutable #:transparent)

(struct define-popup-info (prefix long-name short-name find-next get-name) #:transparent)

;; get-define-popup-info :
;; valid-configurations-as-specified-in-the-drscheme:defined-popup-docs
;; -> (or/c (non-empty-listof define-popup-info) #f)
(define (get-define-popup-info cap)
  (cond
    [(not cap) #f]
    [((cons/c string? string?) cap)
     (list (define-popup-info (car cap) (cdr cap) "δ" #f #f))]
    [((list/c string? string? string?) cap)
     (list (define-popup-info (list-ref cap 0) (list-ref cap 1) (list-ref cap 2) #f #f))]
    [((listof (list/c string? string? string?)) cap)
     (for/list ([cap (in-list cap)])
       (define-popup-info (list-ref cap 0) (list-ref cap 1) (list-ref cap 2) #f #f))]
    [((listof (list/c string? string? string?
                      (or/c #f (procedure-arity-includes/c 4))
                      (or/c #f (procedure-arity-includes/c 3))))
      cap)
     (for/list ([cap (in-list cap)])
       (define-popup-info (list-ref cap 0) (list-ref cap 1) (list-ref cap 2) (list-ref cap 3) (list-ref cap 4)))]
    [else #f]))
  

;; get-definitions : string boolean text -> (listof defn)
(define (get-definitions the-define-popup-infos indent? text)
  (define min-indent 0)

  ;; pos : nat
  ;; latest-positions : (listof (or/c natural +inf.0 #f))
  ;;   latest positions are where we found each of these strings in the last go;
  ;;   with a #f on the one we actually returned as the next result last time
  ;;   (or all #fs if this is the first time)
  ;;   in this go, we'll fill in the #fs and then pick the smallest to return
  (define (find-next pos latest-positions)
    (define filled-in-positions
      (for/list ([latest-position (in-list latest-positions)]
                 [a-define-popup-info (in-list the-define-popup-infos)])
        (cond
          [latest-position latest-position]
          [else
           (define tag-string (define-popup-info-prefix a-define-popup-info))
           (define (search text tag-string pos
                           #:case-sensitive? [case-sensitive? #f]
                           #:delimited? [delimited? #f])
             (let loop ([pos pos])
                (define search-pos-result (send text find-string tag-string 'forward pos 'eof #t case-sensitive?))
                (cond
                  [(and search-pos-result
                        (or (in-comment-or-text? text search-pos-result)
                            (and delimited?
                                 (not (delimited-text? text search-pos-result (+ search-pos-result (string-length tag-string)))))))
                   (and (< search-pos-result (send text last-position))
                        (loop (+ search-pos-result 1)))]
                  [else search-pos-result])))
           (or ((or (define-popup-info-find-next a-define-popup-info)
                    (lambda (text tag-string pos default) (default text tag-string pos)))
                text tag-string pos search)
               +inf.0)])))

    (define-values (smallest-i smallest-pos)
      (for/fold ([smallest-i #f] [smallest-pos #f])
                ([pos (in-list filled-in-positions)]
                 [i (in-naturals)])
        (cond
          [(not smallest-i) (values i pos)]
          [(< pos smallest-pos) (values i pos)]
          [else (values smallest-i smallest-pos)])))
    (when (and smallest-pos (= +inf.0 smallest-pos))
      (set! smallest-pos #f)
      (set! smallest-i #f))
    (define final-positions
      (for/list ([position (in-list filled-in-positions)]
                 [i (in-naturals)])
        (cond
          [(equal? i smallest-i) #f]
          [else position])))
    (values smallest-pos
            (and smallest-i
                 (string-length (define-popup-info-prefix
                                  (list-ref the-define-popup-infos smallest-i))))
            (and smallest-i
                 (let ([proc (define-popup-info-get-name
                               (list-ref the-define-popup-infos smallest-i))])
                   (if proc
                       (lambda (text pos) (proc text pos get-defn-name))
                       get-defn-name)))
            final-positions))
  
  (define defs
    (let loop ([pos 0] [find-state (map (λ (x) #f) the-define-popup-infos)])
      (define-values (defn-pos tag-length defn-get-name new-find-state) (find-next pos find-state))
      (cond
        [(not defn-pos) null]
        [else
         (define indent (get-defn-indent text defn-pos))
         (define name (defn-get-name text (+ defn-pos tag-length)))
         (set! min-indent (min indent min-indent))
         (define next-defn (make-defn indent (or name (string-constant end-of-buffer-define))
                                      defn-pos defn-pos))
         (cons next-defn
               (loop (+ defn-pos tag-length)
                     new-find-state))])))
  
  ;; update end-pos's based on the start pos of the next defn
  (unless (null? defs)
    (let loop ([first (car defs)]
               [defs (cdr defs)])
      (cond
        [(null? defs)
         (set-defn-end-pos! first (send text last-position))]
        [else (set-defn-end-pos! first (max (- (defn-start-pos (car defs)) 1)
                                            (defn-start-pos first)))
              (loop (car defs) (cdr defs))])))
  
  (when indent?
    (for ([defn (in-list defs)])
      (set-defn-name! defn
                      (string-append
                       (apply string
                              (vector->list
                               (make-vector
                                (- (defn-indent defn) min-indent) #\space)))
                       (defn-name defn)))))
  defs)

;; in-comment-or-text?: text number -> boolean
;; returns #t if `define-start-pos' is in a comment or text according
;; to coloring information, #f otherwise
(define (in-comment-or-text? text define-start-pos)
  (case (send text classify-position define-start-pos)
    [(comment text string) #t]
    [else #f]))

(define (delimited-text? text start-pos end-pos)
  (and (equal? end-pos
               (send text forward-match start-pos (send text last-position)))
       (equal? start-pos
               (send text backward-match end-pos 0))))

;; get-defn-indent : text number -> number
;; returns the amount to indent a particular definition
(define (get-defn-indent text pos)
  (define para (send text position-paragraph pos))
  (define para-start (send text paragraph-start-position para #t))
  (let loop ([c-pos para-start]
             [offset 0])
    (cond
      [(< c-pos pos)
       (define char (send text get-character c-pos))
       (cond
         [(char=? char #\tab) (loop (+ c-pos 1) (+ offset (- 8 (modulo offset 8))))]
         [else (loop (+ c-pos 1) (+ offset 1))])]
      [else offset])))

;; whitespace-or-paren?
(define (whitespace-or-paren? char)
  (or (char=? #\) char)
      (char=? #\( char)
      (char=? #\] char)
      (char=? #\[ char)
      (char-whitespace? char)))

;; skip : text number (char -> bool) -> number
;; Skips characters recognized by `skip?'
(define (skip text pos skip?)
  (let loop ([pos pos])
    (cond
      [(>= pos (send text last-position)) (send text last-position)]
      [else
       (define char (send text get-character pos))
       (cond
         [(skip? char)
          (loop (+ pos 1))]
         [else pos])])))

;; skip-to-whitespace/paren : text number -> number
;; skips to the next parenthesis or whitespace after `pos', returns that position.
(define (skip-to-whitespace/paren text pos)
  (skip text pos (negate whitespace-or-paren?)))

;; skip-whitespace/paren : text number -> number
;; skips past any parenthesis or whitespace
(define (skip-whitespace/paren text pos)
  (skip text pos whitespace-or-paren?))

;; skip-past-non-whitespace : text number -> number
;; skips to the first whitespace character after the first non-whitespace character
(define (skip-past-non-whitespace text pos)
  (skip text (skip text pos char-whitespace?) (negate char-whitespace?)))

(define (forward-to-next-token text pos)
  (send text forward-match pos (send text last-position)))

;; get-defn-name : text number -> string
;; returns the name of the definition starting at `define-pos',
;; assuming that the bound name is the first symbol to appear
;; after the one beginning at `define-pos'. This heuristic 
;; usually finds the bound name, but it breaks for Redex
;; metafunction definitions (thus the hack below).
(define (get-defn-name text define-pos)
  (define end-suffix-pos (skip-to-whitespace/paren text define-pos))
  (define suffix (send text get-text define-pos end-suffix-pos))
  (define end-header-pos
    (cond [(regexp-match #rx"^-metafunction(/extension)?$" suffix)
           => (λ (m)
                (define extension? (second m))
                (skip-past-non-whitespace
                 text
                 (if extension?
                     (skip-past-non-whitespace text end-suffix-pos)
                     end-suffix-pos)))]
          [else end-suffix-pos]))
  (define start-name-pos (skip-whitespace/paren text end-header-pos))
  (define end-name-pos (forward-to-next-token text start-name-pos))
  (cond
    [(not end-name-pos) #f]
    [(>= end-suffix-pos (send text last-position))
     (string-constant end-of-buffer-define)]
    [else (send text get-text start-name-pos end-name-pos)]))

;; more tests are in tests/drracket/get-defs-test
(module+ test
  (require rackunit racket/gui/base framework)

  (let ()
    (define t (new racket:text%))
    (send t insert "(define (f x) x)\n")
    (send t insert "  (define (g x) x)\n")
    (send t insert "(module m racket/base)\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ" #f #f)
            (define-popup-info "(module" "(module ...)" "ρ" #f #f))
      #t
      t)
     ;; the 18 in the first element of this list
     ;; might actually supposed to be 17
     ;; (and other test cases similarly)
     (list (defn 0 "f" 0 18)
           (defn 2 "  g" 19 35)
           (defn 0 "m" 36 59)))
    (check-equal?
     (get-definitions
      (list (define-popup-info "(module" "(module ...)" "ρ" #f #f))
      #t
      t)
     (list (defn 0 "m" 36 59))))

  (let ()
    (define t (new racket:text%))
    (send t insert "(dEfInE (f x) x)\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ"
              (lambda (text prefix pos default) (default text prefix pos #:case-sensitive? #t))
              #f))
      #t
      t)
     (list)))

  (let ()
    (define t (new racket:text%))
    (send t insert "(dEfInE (f x) x)\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ" #f #f))
      #t
      t)
     (list (defn 0 "f" 0 17))))

  (let ()
    (define t (new racket:text%))
    (send t insert "(define (f x) x)\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ"
              (lambda (text prefix pos default) (default text prefix pos #:delimited? #t))
              #f))
      #t
      t)
     (list)))

  (let ()
    (define t (new racket:text%))
    (send t insert "(define (f x) x)\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ" #f
              (lambda (text pos default) (string-append "found " (default text pos)))))
      #t
      t)
     (list (defn 0 "found f" 0 17))))

  (let ()
    (define t (new racket:text%))
    (send t insert "(define (f x) x)\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ"
              (lambda (text prefix pos default) (and (zero? pos) 11))
              (lambda (text pos default) "found")))
      #t
      t)
     (list (defn 11 "           found" 11 17))))

  (let ()
    (define t (new racket:text%))
    (send t insert "(define-metafunction L\n")
    (send t insert "  M : any -> any\n")
    (send t insert "  [(M any_1) any_1])\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ" #f #f))
      #t
      t)
     (list (defn 0 "M" 0 61))))

  (let ()
    (define t (new racket:text%))
    (send t insert "(define-metafunction L\n")
    (send t insert "  [(M any_1) any_1])\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ" #f #f))
      #t
      t)
     (list (defn 0 "M" 0 44))))

  (let ()
    (define t (new racket:text%))
    (send t insert "(define (|(| x) x)\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ" #f #f))
      #t
      t)
     (list (defn 0 "|(|" 0 19))))

  (let ()
    (define t (new racket:text%))
    (send t insert "(define)\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ" #f #f))
      #t
      t)
     (list (defn 0 "<< end of buffer >>" 0 9))))

  (let ()
    (define t (new racket:text%))
    (send t insert "(define)\n")
    (check-equal?
     (get-definitions
      (list (define-popup-info "(define" "(define ...)" "δ" #f #f))
      #t
      t)
     (list (defn 0 "<< end of buffer >>" 0 9))))
  )
