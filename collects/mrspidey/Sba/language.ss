;; languages.ss - defines analyzed language
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

(define-struct language-spec
  (kernel constructor-env initial-env tdef-env))
(define language-spec (void))

(define make-expander-namespace (void))

;; ======================================================================

(define
  st:language
  (make-parameter-list
    'none
    '( (DrScheme  "DrScheme")
       (MzScheme  "MzScheme")
       (MrEd      "MrEd")
       (R4RS      "R4RS")
       (Chez      "Chez Scheme")
       (none      "None"))
    (lambda (scheme)
      (unless (eq? scheme 'none)
        (init-default-constructor-env!)
        (init-input-type-expander!)
        (init-output-type-expander!)
        (init-kernel!)
        (init-common-AV!)
        (set! make-expander-namespace (lambda () (make-namespace)))
        (init-expand!)
        (set! initial-env (make-hash-table))
        ;; ---
        (init-R4RS!)
        (case scheme
          [(MzScheme)
            (init-MzScheme-on-R4RS!)]
          [(MrEd)     (init-MzScheme-on-R4RS!)
            (init-mzlib!)
            (init-MrEd-on-MzScheme!)
            (set! make-expander-namespace (lambda () (make-namespace 'wx)))]
          [(DrScheme) 
            (init-MzScheme-on-R4RS!) 
            (init-mzlib!)
            (init-DrScheme-on-MzScheme!)
            (set! make-expander-namespace (lambda () (make-namespace 'wx)))
            ]
          [(R4RS)     (void)]
          [(Chez)     (init-Chez-on-R4RS!)]
          [(none)     (void)])
        (when (st:numops) (init-smart-numops!) (init-vectors-w/-length))
        
        (when (file-exists? "~/.spideyrc") (language-add-boot-file "~/.spideyrc"))
        (let ([systemrc (string-append "~/.spidey-" 
                          (symbol->string scheme)
                          "-rc")])
          (when (file-exists? systemrc) (language-add-boot-file systemrc)))

        ;; 
        ;(printf "Saving language spec ... ")
        (set! language-spec
          (make-language-spec
            (prompt-kernel-state)
            constructor-env
            initial-env
            global-tdef-env))
        ;(printf "done~n")
        ))))

;(trace st:language)

;; ----------------------------------------------------------------------

(define (initialize-language!)
  (mrspidey:progress "Initializing language" '...)
  (match language-spec
    [($ language-spec k c i t)
      (pretty-debug '(initialize-language!))
      (unprompt-kernel-state! k)
      (set-constructor-env! c)
      (set! initial-env i)
      (init-global-tenv! '() t '())
      ])
  (mrspidey:progress "Initializing language" 'done))

;; ----------------------------------------------------------------------

(define st:numops         ;; Default false
  (let ([value #f])
    (match-lambda*
     [() value]
     [('?) `( (#f "Inaccurate" "")
              (#t "Accurate" ""))]
     [(nu)
       (unless (boolean? nu) (error 'st:numops "Bad parameter"))
       (unless (eq? value nu)
         (set! value nu)
         (when value (st:constants #t))
         ;; Reprocess the language
         (st:language (st:language)))
       value])))

;; ======================================================================

(define initial-env '())

(define (extend-initial-env! sym ftype)
  (assert (and (symbol? sym) (FlowType? ftype)) 'extend-initial-env)
  (hash-table-put! initial-env sym ftype)
  (hash-table-put! initial-env (symbol-append '#% sym) ftype))

(define list-primitives '())

(define (add-default-primitive! name type . attrs)
  (set! list-primitives (cons (list name type) list-primitives))
  ;; working w/ default primitive env => use default constructor env
  (let* ([def (apply primitive->atprim name type attrs)]
         [ftype (create-fo-FlowType def)])
    (extend-initial-env! name ftype)))

(define (show-primitives)
  (let ([l (quicksort list-primitives
             (lambda (a b)
               (string<? (symbol->string (car a)) (symbol->string (car b)))))]
         [file "~/Spidey/doc/prims.tex"])
    (when (file-exists? file) (delete-file file))
    (with-output-to-file file
      (lambda ()
        (for-each
          (match-lambda
            [(name type)
              (printf "\\scheme|~s| \\> ~n" name)
              (printf "\\begin{schemedisplay}~n")
              (parameterize ([pretty-print-columns 60])
                (pretty-print type))
              (printf "\\end{schemedisplay}~n")
              (printf "\\\\~n")])
          l)))))

(define (add-default-primitives! l)
  (for-each 
   (match-lambda
    [(name 'GLOBAL-TYPE-VARIABLE)
      '(add-default-tdef! name)]
    [args (apply add-default-primitive! args)])
   l))

(define (add-default-tdef! name) (add-global-tdef! name))

;  (let ([Tvar (mk-Tvar (symbol-append 'default-tdef: name))])
;    (set! default-tdef-env
;      (cons (cons name Tvar) default-tdef-env))))

;; ======================================================================

(define (get-default-bindings free-names)
  (recur loop ([env atenv:empty][free-names free-names])
    (match free-names
      [() env]
      [(name . rest)
        (let ([sym (zodiac:binding-var name)])
          (match (hash-table-get initial-env sym (lambda () #f))
            [#f 
              (mrspidey:warning (format "Free variable ~s" sym))
              (loop env rest)]
            [ftype (loop (atenv:extend env name ftype) rest)]))])))

;; ======================================================================

(define (language-add-boot-file filename)
  (let*-vals
    ( [filename (normalize-path filename)]
      [boot-defs (zodiac:read* (open-code-file filename) filename)]
      [(defs free-names) (my-scheme-expand-program boot-defs)]
      [env (get-default-bindings free-names)]
      [(env refs result) (top-level-traverse-defs defs env)])
    (for-each
      (match-lambda
        [($ zodiac:define-values-form _ _ _ _ refs)
          (for-each
            (lambda (ref)
              (let ([var (zodiac:varref-binding ref)])
                (pretty-debug `(Defining ,(zodiac:binding-var var)))
                (extend-initial-env!
                  (zodiac:binding-var var)
                  (atenv:lookup env var))))
            refs)]
        [_ (void)])
      defs)))

;; ======================================================================

(define (init-R4RS!)
  ;; Also extends it with void

  (add-constructor! 'vec #t)
  (add-constructor! 'iport)
  (add-constructor! 'oport)
  (add-constructor! 'eof  )
  (add-constructor! 'box  #t)

  ;; for demos
  (add-constructor! 'pair #t #t)

  (add-default-primitives!
    `(
       ;; for demos
       (nil            nil)
       (pair           (forall (a b) (a b -> (pair a b))))
       (left            (forall (a) ((pair a _) -> a)))
       (right            (forall (a) ((pair _ a) -> a)))

       ;;(nil            nil)

       ;; booleans
       (not            (_ -> bool) (predicate* (#t false) (#f false)))

       ;; equivalence predicates
       (eqv?           (_ _ -> bool))
       (eq?            (_ _ -> bool))
       (equal?         (_ _ -> bool))

       ;; pairs and lists
       (cons           (forall (a b) (a b -> (cons a b))))
       (car            (forall (a) ((cons a _) -> a)))
       (cdr            (forall (a) ((cons _ a) -> a)))
       (caar           (forall (a) ((cons (cons a _) _) -> a)))
       (cadr           (forall (a) ((cons _ (cons a _)) -> a)))
       (cdar           (forall (a) ((cons (cons _ a) _) -> a)))
       (cddr           (forall (a) ((cons _ (cons _ a)) -> a)))
       (caaar          (forall (a) ((cons (cons (cons a _) _) _) -> a)))
       (caadr          (forall (a) ((cons _ (cons (cons a _) _)) -> a)))
       (cadar          (forall (a) ((cons (cons _ (cons a _)) _) -> a)))
       (caddr          (forall (a) ((cons _ (cons _ (cons a _))) -> a)))
       (cdaar          (forall (a) ((cons (cons (cons _ a) _) _) -> a)))
       (cdadr          (forall (a) ((cons _ (cons (cons _ a) _)) -> a)))
       (cddar          (forall (a) ((cons (cons _ (cons _ a)) _) -> a)))
       (cdddr          (forall (a) ((cons _ (cons _ (cons _ a))) -> a)))
       (caaaar         (forall (a) ((cons (cons (cons (cons a _) _) _) _) -> a)))
       (caaadr         (forall (a) ((cons _ (cons (cons (cons a _) _) _)) -> a)))
       (caadar         (forall (a) ((cons (cons _ (cons (cons a _) _)) _) -> a)))
       (caaddr         (forall (a) ((cons _ (cons _ (cons (cons a _) _))) -> a)))
       (cadaar         (forall (a) ((cons (cons (cons _ (cons a _)) _) _) -> a)))
       (cadadr         (forall (a) ((cons _ (cons (cons _ (cons a _)) _)) -> a)))
       (caddar         (forall (a) ((cons (cons _ (cons _ (cons a _))) _) -> a)))
       (cadddr         (forall (a) ((cons _ (cons _ (cons _ (cons a _)))) -> a)))
       (cdaaar         (forall (a) ((cons (cons (cons (cons _ a) _) _) _) -> a)))
       (cdaadr         (forall (a) ((cons _ (cons (cons (cons _ a) _) _)) -> a)))
       (cdadar         (forall (a) ((cons (cons _ (cons (cons _ a) _)) _) -> a)))
       (cdaddr         (forall (a) ((cons _ (cons _ (cons (cons _ a) _))) -> a)))
       (cddaar         (forall (a) ((cons (cons (cons _ (cons _ a)) _) _) -> a)))
       (cddadr         (forall (a) ((cons _ (cons (cons _ (cons _ a)) _)) -> a)))
       (cdddar         (forall (a) ((cons (cons _ (cons _ (cons _ a))) _) -> a)))
       (cddddr         (forall (a) ((cons _ (cons _ (cons _ (cons _ a)))) -> a)))

       (set-car!       (forall (a) ((cons (! a) _) a -> void)))
       (set-cdr!       (forall (b) ((cons _ (! b)) b -> void)))
       (list           (case->
                         (forall (a b c d e) (a b c d e -> (list a b c d e)))
                         (forall (a b c d) (a b c d -> (list a b c d)))
                         (forall (a b c) (a b c -> (list a b c)))
                         (forall (a b) (a b -> (list a b)))
                         (forall (a) (a -> (list a)))
                         (forall (a) (a *-> a))))
       (length         ((listof _) -> num))
       (append         (case->
                         (forall (l) (l -> l))
                         (forall (a l)
                           ((listof a) l -> 
                             (MU out (union (cons a out) l))))
                         (forall (inlist a)
                           ((arglistof (union inlist (listof a))) 
                             *-> 
                             (MU out (union (cons a out) inlist))))))
       (reverse        (forall (a) ((listof a) -> (listof a))))
       (list-tail      (forall (a tail)
                         ((MU l (union nil (cons a (union l tail)))) 
                           num 
                           -> (cons a tail))))
       (list-ref       (forall (a) ((listof a) num -> a)))
       (memq           (forall (a tail)
                         (a (MU l (union nil (cons a l) tail)) 
                           -> (union false (cons a tail)))))
       (memv           (forall (a tail)
                         (a (MU l (union nil (cons a l) tail)) 
                           -> (union false (cons a tail)))))
       (member         (forall (a tail)
                         (a (MU l (union nil (cons a l) tail)) 
                           -> (union false (cons a tail)))))
       (assq           (forall (a c)
                         (a (listof (cons a c)) -> 
                           (union false (cons a c)))))
       (assv           (forall (a c)
                         (a (listof (cons a c)) ->
                           (union false (cons a c)))))
       (assoc          (forall (a c)
                         (a (listof (cons a c)) -> 
                           (union false (cons a c)))))

       ;; symbols
       (symbol->string (sym -> str))
       (string->symbol (str -> sym))

       ;; numbers
       (complex?       (_ -> bool)       (predicate* (#t num) _))
       (real?          (_ -> bool)       (predicate* (#t num) _))
       (rational?      (_ -> bool)       (predicate* (#t num) _))
       (integer?       (_ -> bool)       (predicate* (#t num) _))
       (exact?         (num -> bool))
       (inexact?       (num -> bool))
       (=              ((arg num (arg num (arglistof num))) *-> bool)         )
       (<              ((arg num (arg num (arglistof num))) *-> bool)         )
       (>              ((arg num (arg num (arglistof num))) *-> bool)         )
       (<=             ((arg num (arg num (arglistof num))) *-> bool)         )
       (>=             ((arg num (arg num (arglistof num))) *-> bool)         )
       (zero?          (num -> bool))
       (positive?      (num -> bool))
       (negative?      (num -> bool))
       (odd?           (num -> bool))
       (even?          (num -> bool))
       (max            ((arg num (arglistof num)) *-> num)                    )
       (min            ((arg num (arglistof num)) *-> num)                    )
       (+              ((arglistof num) *-> num)                              )
       (*              ((arglistof num) *-> num)                              )
       (-              ((arg num (arglistof num)) *-> num)                    )
       (/              ((arg num (arglistof num)) *-> num)                    )
       (abs            (num -> num))
       (quotient       (num num -> num))
       (remainder      (num num -> num))
       (modulo         (num num -> num))
       (gcd            ((arglistof num) *-> num)                              )
       (lcm            ((arglistof num) *-> num)                              )
       (numerator      (num -> num)                                         )
       (denominator    (num -> num)                                         )
       (floor          (num -> num)                                         )
       (ceiling        (num -> num)                                         )
       (truncate       (num -> num)                                         )
       (round          (num -> num)                                         )
       (rationalize    (num num -> num)                                     )
       (exp            (num -> num)                                         )
       (log            (num -> num)                                         )
       (sin            (num -> num)                                         )
       (cos            (num -> num)                                         )
       (tan            (num -> num)                                         )
       (asin           (num -> num)                                         )
       (acos           (num -> num)                                         )
       (atan           (num optional num -> num)                            )
       (sqrt           (num -> num)                                         )
       (expt           (num num -> num)                                     )
       (make-rectangular (num num -> num)                                   )
       (make-polar     (num num -> num)                                     )
       (real-part      (num -> num)                                         )
       (imag-part      (num -> num)                                         )
       (magnitude      (num -> num)                                         )
       (angle          (num -> num)                                         )
       (exact->inexact (num -> num)                                         )
       (inexact->exact (num -> num)                                         )
       (number->string (num optional num -> str)         )
       (string->number (str optional num -> num)         )

       ;; characters
       (char=?         (char char -> bool)                                  )
       (char<?         (char char -> bool)                                  )
       (char>?         (char char -> bool)                                  )
       (char<=?        (char char -> bool)                                  )
       (char>=?        (char char -> bool)                                  )
       (char-ci=?      (char char -> bool)                                  )
       (char-ci<?      (char char -> bool)                                  )
       (char-ci>?      (char char -> bool)                                  )
       (char-ci<=?     (char char -> bool)                                  )
       (char-ci>=?     (char char -> bool)                                  )
       (char-alphabetic? (char -> bool)                                     )
       (char-numeric?  (char -> bool)                                       )
       (char-whitespace? (char -> bool)                                     )
       (char-upper-case? (char -> bool)                                     )
       (char-lower-case? (char -> bool)                                     )
       (char->integer  (char -> num)                                        )
       (integer->char  (num -> char)                                        )
       (char-upcase    (char -> char)                                       )
       (char-downcase  (char -> char)                                       )

       ;; strings
       (make-string    (num optional char -> str)                           )
       (string         ((arglistof char) *-> str)                           )
       (string-length  (str -> num)                                         )
       (string-ref     (str num -> char)                                    )
       (string-set!    (str num char -> void)                               )
       (string=?       (str str -> bool)                                    )
       (string<?       (str str -> bool)                                    )
       (string>?       (str str -> bool)                                    )
       (string<=?      (str str -> bool)                                    )
       (string>=?      (str str -> bool)                                    )
       (string-ci=?    (str str -> bool)                                    )
       (string-ci<?    (str str -> bool)                                    )
       (string-ci>?    (str str -> bool)                                    )
       (string-ci<=?   (str str -> bool)                                    )
       (string-ci>=?   (str str -> bool)                                    )
       (substring      (str num num -> str)                                 )
       (string-append  ((arglistof str) *-> str)                            )
       (string->list   (str -> (listof char))                               )
       (list->string   ((listof char) -> str)                               )
       (string-copy    (str -> str)                                         )
       (string-fill!   (str char -> void)                                   )

       ;; vectors
       ;; make-vector -- different semantics
       (make-vector    (case->
                         (num   -> (vec num))
                         (forall (a) (num a -> (vec a)))
                         (forall (a) (num optional a -> (vec (union a num))))))
       (vector         (forall (a) ((arglistof a) *-> (vec a))))
       (vector-length  (forall (a) ((vec a) -> num)))
       (vector-ref     (forall (a) ((vec a) num -> a)))
       (vector-set!    (forall (a) ((vec (! a)) num a -> void)))
       (vector->list   (forall (a) ((vec a) -> (listof a))))
       (list->vector   (forall (a) ((listof a) -> (vec a))))
       (vector-fill!   (forall (a) ((vec (! a)) a -> void)))

       ;; control features
       (apply          (case->
                         (forall (l r)       ((l *->* r)             l ->* r))
                         (forall (a l r)     ((a l *->* r)     a     l ->* r))
                         (forall (a b l r)   ((a b l *->* r)   a b   l ->* r))
                         (forall (a b c l r) ((a b c l *->* r) a b c l ->* r))
                         (forall (a b c d l r) ((a b c d l *->* r) a b c d l ->* r))
                         (forall (x r) 
                           (((MU l (union (cons x l) x)) *->* r) 
                             x
                             (arglistof x)
                             *->* r))))
       (map            (case->
                         (forall (a r)
                           ((a     -> r) (listof a) 
                             -> (listof r)))
                         (forall (a b r)
                           ((a b   -> r) (listof a) (listof b) 
                             -> (listof r)))
                         (forall (a b c r) 
                           ((a b c -> r) (listof a) (listof b) (listof c) 
                             -> (listof r)))
                         (forall (x r)
                           (((arglistof x) *-> r) (listof x) (arglistof (listof x))
                             *-> (listof r)))))

       (for-each       (case->
                         (forall (a)
                           ((a     -> _) (listof a) 
                             -> void))
                         (forall (a b)
                           ((a b   -> _) (listof a) (listof b) 
                             -> void))
                         (forall (a b c) 
                           ((a b c -> _) (listof a) (listof b) (listof c) 
                             -> void))
                         (forall (x)
                           (((arglistof x) *-> _) (listof x) (arglistof (listof x))
                             *-> void))))

       (force                 (forall (a) ((promise a) -> a)))
       (make-promise          ((-> a) -> (promise a)))
       (promise?              (_ -> bool) (predicate promise))
       (call-with-current-continuation 
         (forall (a) (((a *-> empty) ->* (mvalues a)) ->* (mvalues a))))
       (eval           (sexp -> sexp));; --- Not quite right!!!

       ;; input and output
       (call-with-input-file  (forall (a) (str (iport -> a) -> a)))
       (call-with-output-file (forall (a) (str (oport -> a) -> a)))
       (current-input-port    (-> iport)                                    )
       (current-output-port   (-> oport)                                    )
       (with-input-from-file  (forall (a) (str (-> a) -> a)))
       (with-output-to-file   (forall (a) (str (-> a) -> a)))
       (open-input-file       (str -> iport)                                )
       (open-output-file      (str -> oport)                                )
       (close-input-port      (iport -> void)                               )
       (close-output-port     (oport -> void)                               )
    
       (read                  (optional iport -> (union eof sexp)))
       (read-char             (optional iport -> (union char eof)))
       (peek-char             (optional iport -> (union char eof)))
       (char-ready?           (optional iport -> bool))
       (write                 (optional oport -> void))
       (display               (_ optional oport -> void))
       (newline               (optional oport -> void))
       (write-char            (char optional oport -> void))

       ;; system interface
       (load           (str -> void)                                          )
       (transcript-on  (str -> void)                                          )
       (transcript-off (-> void)                                              )

       ;; predicates
       (number?        (_ -> bool)         (predicate num))
       (null?          (_ -> bool)         (predicate nil))
       (char?          (_ -> bool)         (predicate char))
       (symbol?        (_ -> bool)         (predicate sym))
       (string?        (_ -> bool)         (predicate str))
       (vector?        (_ -> bool)         (predicate vec))
       (cvector?       (_ -> bool)         (predicate vec))
       (pair?          (_ -> bool)         (predicate cons))
       (procedure?     (_ -> bool)         (predicate lambda))
       (eof-object?    (_ -> bool)         (predicate eof))
       (input-port?    (_ -> bool)         (predicate iport))
       (output-port?   (_ -> bool)         (predicate oport))
       (boolean?       (_ -> bool)         (predicate true false))
       (list?          (_ -> bool)         (predicate* (#t nil cons) (#f nil)))

       )))

;; ======================================================================

(define (language-add-boxes-etc!)
  (add-default-primitives!
   `(
     (read                  (optional iport -> (union eof sexp)))
     (box?           (_ -> bool)                       (predicate box))
     (box            (forall (a) (a -> (box a))))
     (unbox          (forall (a) ((box a) -> a)))
     (set-box!       (forall (a) ((box (! a)) a -> void)))
     (void           (-> void))

     (add1          (num -> num)                                      )
     (sub1          (num -> num)                                      )
     (ormap         (case->
                     (forall (a r)
                             ((a -> r) (listof a) -> (union false r)))
                     (forall (a b r)
                             ((a b -> r) (listof a) (listof b) 
                              -> (union false r)))
                     (forall (a b c r)
                             ((a b c -> r) (listof a) (listof b) (listof c) 
                              -> (union false r)))
                     (forall (x r)
                             (((arglistof x) *-> r) (arglistof (listof x))
                              *-> (union false r)))))
     (andmap        (case->
                     (forall (a r)
                             ((a -> r) (listof a) -> (union true r)))
                     (forall (a b r)
                             ((a b -> r) (listof a) (listof b) 
                              -> (union true r)))
                     (forall (a b c r)
                             ((a b c -> r) (listof a) (listof b) (listof c) 
                              -> (union true r)))
                     (forall (x r)
                             (((arglistof x) *-> r) (arglistof (listof x))
                              *-> (union true r)))))

     (append!        (forall (p)
                       ((arglistof
                          (MU q (union (cons _ q) (cons _ p) (cons _ (! p)))))
                         *->
                         p)))
     (exit          (optional num -> empty))
     (format        (str _ *-> str))
     (fprintf       (oport str (arglistof _) *-> void))
     (getenv        (str -> (union str false)))
     (list*         (forall (l a)
                            ((arglistof (union l (listof a))) 
                             *-> (MU o (union l (cons a o))))))
     (printf        (str _ *-> void))
     (putenv        (str str -> bool))
     (random        (num -> num))
     (random-seed   (num -> void))
     (reverse!      
      (forall 
       (l p a)
       ((arglistof (MU l (union p (cons a l) (cons _ (! p))))) *-> p)))

     (pretty-print  (_ optional oport -> void)                        )
     (gensym        (optional (union sym str) -> sym))
     (sort          (forall (a) ((a a -> _) (listof a) -> (listof a))))

     (string->uninterned-symbol (str -> sym))

     (remove        (forall (a)
                            (a (listof a) optional (a a -> bool) 
                             -> (listof a))))
     (remq          (forall (a) (a (listof a) -> (listof a))))
     (remv          (forall (a) (a (listof a) -> (listof a))))

     (dynamic-wind  (forall (b) ((-> _) (-> b) (-> _) -> b)))
     (call/cc       (forall (a) (((a *-> empty) ->* (mvalues a)) ->* (mvalues a))))

     )))


(define (init-Chez-on-R4RS!)

  (language-add-boxes-etc!)

  (add-default-primitives!
   `(
     (void           (-> void)                                        )

     ;; Standard Chez Scheme primitives.
     (make-list     (forall (a) (num a -> (listof a))))
     (error         (_ str _ *-> empty))
     (expand-once   (sexp -> sexp)                                    )

     )))

;; ----------------------------------------------------------------------

(define this-directory (current-load-relative-directory))

(define (init-MzScheme-on-R4RS!)

  ;; Syntactic forms unimplemented
  ;;    define-struct
  ;;    objects
  ;;    let/cc, let/rc
  ;;    eval, comple
  ;;    begin0
  ;;    fluid-let
  ;;    letrec*
  ;;    make-global-value-list
  ;;    time, unless

  (language-add-boxes-etc!)

  ;; Could make weight a field of thread
  (add-constructor! 'thread)
  (add-constructor! 'hash-table #t #t)
  (add-constructor! 'weak-box #f)
  (add-constructor! 'regexp)
  (add-constructor! 'arity-at-least #f)
  (add-constructor! 'parameterization)
  (add-constructor! 'semaphore)
  (add-constructor! 'type-symbol)
  (add-constructor! 'namespace)
  (add-constructor! 'custodian)
  (add-constructor! 'will-executor)
  (add-constructor! 'tcp-listener)

  (add-default-primitives!
    `(
       ;; MzSchemes f*cked up void :-)
       (void           (_ *-> void))

       (eof                  eof)

       ;; Organization follows MzScheme Reference Manual
       ;; --- Programming Constructs
     
       ;; Void and Undefined
       (void?          (_ -> bool) (predicate void))

       ;; Number extensions
       (bitwise-ior         (num (arglistof num) *-> num))
       (bitwise-and         (num (arglistof num) *-> num))
       (bitwise-xor         (num (arglistof num) *-> num))
       (bitwise-not         (num (arglistof num) *-> num))
       (arithmetic-shift    (num num -> num))

       ;; --- Semaphores

       (make-semaphore      (num -> semaphore))
       (semaphore?          (semaphore -> bool)          (predicate semaphore))
       (semaphore-post      (semaphore -> void))
       (semaphore-wait      (semaphore -> void))
       (semaphore-try-wait  (semaphore -> bool))
       (semaphore-callback  (semaphore (-> _) -> bool))
       (input-port-post-semaphore  (iport semaphore -> void))

       ;; --- Ports
       ;; Current ports
       (current-input-port  (optional iport -> iport))
       (current-output-port (optional oport -> oport))
       (current-error-port  (optional oport -> oport))

       (thread-input-port   (-> iport))
       (thread-output-port  (-> oport))
       (thread-error-port   (-> iport))

       (open-input-file     (str optional sym -> iport))
       (open-output-file    (str optional sym optional sym -> oport))

       (call-with-input-file  
         (forall (a) (str (iport -> a) optional sym              -> a)))
       (call-with-output-file
         (forall (a) (str (oport -> a) optional sym optional sym -> a)))
       (with-input-from-file
         (forall (a) (str (-> a)       optional sym              -> a)))
       (with-output-to-file
         (forall (a) (str (-> a)       optional sym optional sym -> a)))

       ;; String ports
       (open-input-string   (str -> iport))
       (open-output-string  (-> oport))
       (get-output-string   (oport -> str))

       ;; File ports
       (flush-output        (optional oport -> void))
       (file-position       ((union iport oport) optional num -> num))

       ;; Custom ports
       (make-input-port     ((-> char) (-> bool) (-> void) -> iport))
       (make-output-port    ((str -> void) (-> void) -> oport))

       ;; ---- Filesystem Utilities
       ;; Files
       (file-exists?        (str -> bool))
       (delete-file         (str -> bool))
       (rename-file         (str str -> bool))
       (file-modify-seconds (str -> num))
       (file-or-directory-permissions (str -> (listof sym)))

       ;; Hash Tables - fields are key and value
       (make-hash-table     (optional sym -> (hash-table empty empty)))
       (make-hash-table-weak (optional sym -> (hash-table empty empty)))
       (hash-table?         (_ -> bool)      (predicate hash-table))
       (hash-table-put!     
         (forall (k v) ((hash-table (! k) (! v)) k v -> void)))
       (hash-table-get          
         (forall (k v r) ((hash-table k v) _ optional (-> r) -> (union v r))))
       (hash-table-remove!      
         (forall (k v) ((hash-table _ _) _ -> void)))
       (hash-table-map          
         (forall (k v w) ((hash-table k v) (k v -> w) -> (listof w))))
       (hash-table-for-each     
         (forall (k v) ((hash-table k v) (k v -> _) -> void)))

       ;; Weak boxes
       (make-weak-box       (forall (v) (v -> (weak-box (union v false)))))
       (weak-box?           (_ -> bool) (predicate weak-box))
       (weak-box-value      (forall (v) ((weak-box v) -> v)))

       ;; Regular expressions
       (regexp              (str -> regexp))
       (regexp?             (_ -> bool) (predicate regexp))
       (regexp-match        ((union str regexp) str 
                              -> (union false (listof str))))
       (regexp-match-positions 
         ((union str regexp) str -> (union false (listof (cons num num)))))
       (regexp-replace      ((union str regexp) str str -> str))
       (regexp-replace*     ((union str regexp) str str -> str))

       ;; Global type variables don't work, so analysis of
       ;; exception handling is unsound here

       ;; Exceptions
;;     (global:CEH           GLOBAL-TYPE-VARIABLE)
;;     (global:raised-values GLOBAL-TYPE-VARIABLE);
;;
;;     (raise               (global:raised-values -> empty))
;;     (current-exception-handler
;;                          (optional (intersect global:CEH
;;                                               (global:raised-values -> _))
;;                                    -> global:CEH))

       (raise               (_ -> empty))
       (current-exception-handler         (optional (empty -> _) -> empty))
 
       (make-exn:else       (_ *-> empty))

       ;; Flow control
       (call-with-escaping-continuation
                            (forall (a) (((a *-> empty) ->* (mvalues a)) ->* (mvalues a))))
        
       (call/ec             (forall (a) (((a *-> empty) ->* (mvalues a)) ->* (mvalues a))))

       (values              (forall (a) (a *->* (mvalues a))))
       (call-with-values    (forall (x r) ((list (nil *->* (mvalues x))
                                             (x *->* (mvalues r)))
                                            *->* (mvalues r))))

       ;; --------------------

       ;; Arity
       (arity               ((empty *-> _) -> 
                              (rec ([r (union num (arity-at-least num))])
                                (union r (listof r)))))
       (arity-at-least?     (_ -> bool) (predicate arity-at-least))
       (arity-at-least-value (forall (v) ((arity-at-least v) -> v)))

       ;; Global and Constant Names
       (defined?            (sym -> bool))

       ;; Evaluation Handler - no can type :-

       ;; Handlers
       (current-print       (optional (empty -> void) -> (_ -> void)))
       (current-prompt-read
         (optional (-> (union eof sexp)) -> (-> (union eof sexp))))
       (error              ((union sym str) (arglistof _) *-> empty))

       ;; ### problems with types for handlers

       (error-display-handler  (optional (empty -> void) -> (_ -> void)))
       (error-escape-handler   (optional (-> _) -> (-> empty)))
       (exit-handler           (optional (num -> _) -> (num -> empty)))

       ;; User Breaks
       (user-break-poll-handler (optional (-> bool) -> (-> bool)))
       (break-enabled          (optional _ -> bool))
       (dynamic-enable-break   (forall (v) ((-> v) -> v)))

       ;; Compilation
       (compile-file           ((union str iport) (union str oport) _ -> void))

       ;; Dynamic extensions
       (load-extension         (str -> void))

       ;; Operating System Processes
       (system                 (str -> bool))
       (sytem*                 (str (arglistof str) *-> bool))
       (process                (str -> 
                                 (cons iport 
                                   (cons oport
                                     (cons num (cons iport nil))))))
       (process*               (str (arglistof str) *-> 
                                 (cons iport 
                                   (cons oport
                                     (cons num (cons iport nil))))))

       ;; Misc
       (banner                 (-> str))
       (gensym                 (optional (union str sym) -> sym))
       (load/cd                (str -> void))
       (load-with-cd           (str -> void))
       (promise?               (_ -> bool) (predicate promise))
       (read-eval-print-loop   (-> void))
       (read-line              (optional iport -> (union str eof)))
       (system-type            (-> sym))
       (version                (-> str))

       ;; Signature stuff
       (#%unit-with-signature-unit empty)
       (#%make-unit-with-signature empty)
       (#%verify-linkage-signature-match empty)

       ;; --------------------------------------------------------------------
       ;; the following is structured after the MzScheme ref manual 5.9.97

       ;; ------ basic data extensions
       ;; ---- procedures
       ;; -- primitives
       (primitive?             (_ -> bool) (predicate* (#t lambda) _))
       (primitive-name         ((empty *->* _) -> sym))
       (primitive-result-arity? ((empty *->* _) -> num))
       (primitive-result-arity? ((empty *->* _) -> bool))

       ;; ------ structures 
       ;; ---- structure utilities
       (struct?                (_ -> bool) (predicate structure:))
       (struct-length          (structure: -> num))
       (struct-type?           (-> bool))
       (struct-constructor-procedure? (-> bool))
       (struct-predicate-procedure? (-> bool))
       (struct-selector-procedure? (-> bool))
       (struct-setter-procedure? (-> bool))

       ;; ------ classes and objects
       ;; ---- object utilities

       (object?                (_ -> bool))
       (class?                 (_ -> bool) (predicate internal-class))
       (is-a?                  (_ _ -> bool))
       (make-object
         (forall (args u o f v)
           ( (internal-class u o (args *->* _) (! u) (! o) o (! o) (! o) v)
             args 
             *->
             (union o v))))
       (ivar-in-class?        (_ _ -> bool))
       (uq-ivar               (forall (i) ((all-ivars i) _ -> i)))




       ;; ------ units
       (unit/sig->unit         (forall (a) (a -> a)))
       (unit?                  (_ -> bool))

       ;; ------ threads and namespaces
       ;; ---- threads
       (thread              ((-> _) -> thread))
       ;; -- thread utilities
       (current-thread         (-> thread))
       (thread?                (_ -> bool) (predicate thread))
       (sleep                  (num -> void))
       (thread-running?        (thread -> bool))
       (thread-wait            (thread -> void))
       (kill-thread            (thread -> void))
       (break-thread           (thread -> void))
       (thread-weight          (case->
                                 (thread -> num)
                                 (thread num -> void)))
       ;; ---- semaphores
       (make-semaphore         (optional num -> semaphore))
       (semaphore?             (_ -> bool) (predicate semaphore))
       (semaphore-post         (semaphore -> void))
       (semaphore-wait         (semaphore -> void))
       (semaphore-try-wait?    (semaphore -> bool))
       (semaphore-wait/enable-break (semaphore -> void))
       (semaphore-callback     (semaphore (-> _) -> void))
       (input-port-post-semaphore (iport semaphore -> void))

       ;; ---- parameterization
       ;; -- built-in parameters
       ;; loading
       (current-load-relative-directory (union str false))
       ;; -- exceptions
       (debug-info-handler     (-> (-> void)))
       ;; libraries
       (current-library-collections-paths
         (case->
           ((listof str) -> void)
           (-> (listof str))))
       (require-library-use-compiled (case-> (bool -> void) (-> bool)))
       ;; parsing
       (read-case-sensitive    (case-> (-> bool) (bool -> void)))
       (read-square-bracket-as-paren (case-> (-> bool) (bool -> void)))
       (read-curly-brace-as-paren (case-> (-> bool) (bool -> void)))
       (read-accept-box        (case-> (-> bool) (bool -> void)))
       (read-accept-type-symbol (case-> (-> bool) (bool -> void)))
       (read-accept-compiled   (case-> (-> bool) (bool -> void)))
       (read-accept-bar-quote  (case-> (-> bool) (bool -> void)))
       (read-accept-graph      (case-> (-> bool) (bool -> void)))
       ;; printing
       (print-graph            (case-> (-> bool) (bool -> void)))
       (print-struct           (case-> (-> bool) (bool -> void)))
       (print-box              (case-> (-> bool) (bool -> void)))
       
       ;; -- parameter utilities
       ;; We should arguably have a separate type for parameter procedures
       (make-parameter (forall (x i)
                         (x optional (i -> x) 
                           -> (optional i -> (union x void)))))
       (parameter?            (_ -> bool))
       (parameter-procedure=? (_ _ -> bool))

       ;; -- parameterization utilities
       (make-parameterization (parameterization -> parameterization))
       (current-parameterization
         (case->
           (parameterization -> void)
           (-> parameterization)))
       (parameterization?     (-> bool) (predicate parameterization))
       (in-parameterization   (forall (param)
                                (parameterization
                                  param
                                  optional _
                                  -> param)))
       (with-parameterization (parameterization (-> result) -> result))
       (with-new-parameterization ((-> result) -> result))
       (parameterization-branch-handler (-> parameterization))

       ;; ---- custodians
       (make-custodian         (custodian -> custodian))
       (custodian-shutdown-all (custodian -> void))
       (custodian?             (_ -> bool) (predicate custodian))
       (current-custodian      (case->
                                 (-> custodian)
                                 (custodian -> void)))

       ;; ---- namespaces
       (make-namespace           ((listof sym) *-> namespace))
       (namespace?               (_ -> bool) (predicate namespace))
       (current-namespace        (case->
                                   (-> namespace)
                                   (namespace -> void)))

       ;; ------ System utilities
       ;; ---- ports
       ;; ---- filesystem utilities
       ;; -- pathnames
       (build-path          (str (arglistof (union str sym)) *-> str))
       (absolute-path?      (str -> bool))
       (relative-path?      (str -> bool))
       (complete-path?      (str -> bool))
       (path->complete-path (str -> str))
       (resolve-path        (str -> str))
       (expand-path         (str -> str))
       (normal-case-path    (str -> str))

       (split-path          (str -> (union str sym false)
                              (union str sym)
                              bool))
       (find-executable-path (str str -> str))
       ;; -- directories
       (current-directory   (case->
                              (str -> void)
                              (-> str)))
       (current-drive       (-> (union bool str)))
       (directory-exists?   (str -> bool))
       (make-directory      (str -> bool))
       (delete-directory    (str -> bool))
       (directory-list      (optional str -> (listof str)))
       (filesystem-root-list (-> (listof str)))
     
       ;; ---- networking
       (tcp-listen          (num optional num -> tcp-listener))
       (tcp-connect         (str num ->* (mvalues (list iport oport))))
       (tcp-accept          (tcp-listener ->* (mvalues (list iport oport))))
       (tcp-accept-ready?   (tcp-listener -> bool))
       (tcp-close           (tcp-listener -> void))
       (tcp-listener?       (_ -> bool) (predicate tcp-listener))
       (tcp-port-send-waiting? (oport -> bool))

       ;; ---- time
       ;; -- real time
       (current-seconds     (-> num))
       ;(seconds->date       (num -> (structure:date num num num num 
       ;                               num num num num bool)))
       ;; -- machine time
       (current-milliseconds(-> num))
       (current-process-milliseconds(-> num))
       (current-gc-milliseconds(-> num))
       ;; -- timing execution
       (time-apply          (forall (a) ((-> a) -> (list a num num))))
       
       ;; ---- operating system processes
       (system              (str -> bool))
       (system*             (str (listof str) *-> bool))
       (execute             (str -> void))
       (execute*            (str (listof str) *-> void))
       (process             (str -> (list iport oport num iport (sym -> sym))))
       (process*            (str (listof str)
                              *-> (list iport oport num iport (sym -> sym))))
       
       
       ;; ------ memory management
       ;; ---- will executors
       (make-will-executor       (-> will-executor))
       (will-executor?           (_ -> bool) (predicate will-executor))
       (register-will            (forall (a)
                                   (a (a ->* _) optional will-executor 
                                     -> void)))
       (will-executor-try        (will-executor -> void))
       (current-will-executor 
         (case-> (will-executor -> void) (-> will-executor)))

       ;; ---- garbage collection
       (collect-garbage          (-> void))
       (dump-memory-stats        (-> void))

       ;; ------ macros
       ;; ---- expanding macros
       (syntax?                  (_ -> bool))
       (macro?                   (_ -> bool))
       (id-macro?                (_ -> bool))

       ;; ------ support facilities
       ;; ---- input parsing
       (type-symbol?             (_ -> bool) (predicate type-symbol))
       (string->type-symbol      (str -> type-symbol))




       ;; --------------------------------------------------------------------
       (print                 (_ optional oport -> void))
       (make-pipe             (->* (mvalues (list iport oport))))

       ;; --------------------------------------------------------------------
       ;; MzScheme stand-alone definitions
       (program               str)
       (argv                  (vec str))

       ))


  '(language-add-boot-file 
     (build-path 
       (or this-directory 
         (build-path
	   (collection-path "mrspidey") ; MATTHEW: got rid of plt-home
           "Sba"))
       "exn-hierarchy.ss"))
  )




;; ======================================================================

(define (init-MrEd-on-MzScheme!)
  (language-add-boot-file "~/Spidey/wx/all.sig")
  )

;; ======================================================================

(define (init-mzlib!)

  (constructor-alias! 'null 'nil)

  (add-default-primitives!
   '(
     (require-library (str -> void))
     (=?             ((arg num (arg num (arglistof num))) *-> bool)         )
     (<?             ((arg num (arg num (arglistof num))) *-> bool)         )
     (>?             ((arg num (arg num (arglistof num))) *-> bool)         )
     (<=?            ((arg num (arg num (arglistof num))) *-> bool)         )
     (>=?            ((arg num (arg num (arglistof num))) *-> bool)         )
     (1+             (num -> num))                                          
     (1-             (num -> num))

     (null           nil)
     (cons?          (_ -> bool) (predicate cons))
     (gentemp        (-> sym)                                          )
     (bound?         (sym -> bool))
     (flush-output-port (optional oport -> void))
     (real-time      (-> num))

     ;; --- file.ss
     (build-absolute-path (str (arglistof (union str sym)) -> str))
     (build-relative-path (str (arglistof (union str sym)) -> str))
     (explode-path        (str -> (listof str)))
     (filename-extension  (str -> str))
     (find-relative-path  (str str -> str))
     (normalize-path      (str optional str -> str))

     ;; --- function.ss
     (first         (forall (a) ((cons a _) -> a)))
     (second        (forall (a) ((cons _ (cons a _)) -> a)))
     (third         (forall (a) ((cons _ (cons _ (cons a _))) -> a)))
     (fourth        (forall (a) ((cons _ (cons _ (cons _ (cons a _)))) -> a)))
     (fifth         (forall (a) ((cons _ (cons _ (cons _ (cons _ (cons a _))))) -> a)))
     (sixth         (forall (a) ((cons _ (cons _ (cons _ (cons _ (cons _ (cons a _)))))) -> a)))
     (seventh       (forall (a) ((cons _ (cons _ (cons _ (cons _ (cons _ (cons _ (cons a _))))))) -> a)))
     (eighth         (forall (a) ((cons _ (cons _ (cons _ (cons _ (cons _ (cons _ (cons _ (cons a _)))))))) -> a)))

     (build-list    (forall (a) (num (num -> a) -> (listof a))))
     (build-string  (num (num -> char) -> str))
     (build-vector  (forall (a) (num (num -> a) -> (vec a))))
     (cons?         (_ -> bool) (predicate cons))
     (compose       (forall (x y z) ((x ->* y) (y *->* z) -> (x ->* z))))
       
     (dynamic-disable-break (forall (v) ((-> v) -> v)))
     (dynamic-wind/protect-break
                    (forall (v) ((-> _) (-> v) (-> _) -> v)))
     
     (foldl         (case->
                     (forall (a z) ((a z -> z) z (listof a) -> z))
                     (forall (a b z) 
                             ((a b z -> z) z (listof a) (listof b) -> z))
                     (forall 
                      (a b c z)
                      ((a b c z -> z) z (listof a) (listof b) (listof c) -> z))
                     (forall 
                      (x z)
                      (((arglistof x) *-> z) z (listof (arglistof x)) *-> z))))
     (foldr         (case->
                     (forall (a z) ((a z -> z) z (listof a) -> z))
                     (forall (a b z) 
                             ((a b z -> z) z (listof a) (listof b) -> z))
                     (forall 
                      (a b c z)
                      ((a b c z -> z) z (listof a) (listof b) (listof c) -> z))
                     (forall
                      (x z)
                      (((arglistof x) *-> z) z (listof (arglistof x)) *-> z))))
     (ignore-errors (forall (x) ((-> x) -> (union x void))))
     (last-pair     (forall (p l)
                      ( (MU l (union p (cons _ l))) -> p)))
     (loop-until    (forall (x)
                      (x (x -> _) (x -> x) (x -> _) -> void)))

     (identity      (forall (x) (x -> x)))
     (quicksort     (forall (a) ((listof a) (a a -> _) -> (listof a))))
     
     ;; --- pretty.ss
     (pretty-print  (_ optional oport 
                       optional num
                       optional bool
                       optional bool
                       optional num
                       -> void))
     (pretty-print-columns (optional num -> (union num void)))
     (pretty-print-depth   (optional num -> (union num void)))
     ;; NOT pretty-print-handler

     ;; --- strings.ss
     ;; NOT eval-string
     (expr->string    (_ -> str))
     (newline-string  (-> str))
     (read-string     (str -> sexp))
     (read-string-all (str -> (listof sexp)))
      ;;(regexp-match    ((union str regexp) str -> bool))
     (string-lowercase! (str -> str))
     (string-uppercase! (str -> str))

     (match:error   (_ optional _ -> empty))
;     (match:andmap  (case->
;                     (forall (a r)
;                             ((a -> r) (listof a) -> (union true r)))
;                     (forall (a b r)
;                             ((a b -> r) (listof a) (listof b) 
;                              -> (union true r)))
;                     (forall (a b c r)
;                             ((a b c -> r) (listof a) (listof b) (listof c) 
;                              -> (union true r)))
;                     (forall (x r)
;                             (((arglistof x) *-> r) (arglistof (listof x))
;                              *-> (union true r)))))
;     
     )))


(define (init-zmath!)
  ;; --- zmath.ss
  (add-default-primitives!
   `(
     (conjugate       (num -> num))
     (cosh            (num -> num))
     (make-rectangular (num num -> num)                                   )
     (sinh            (num -> num))
     (zabs            (num -> num))
     (zacos           (num -> num))
     (zasin           (num -> num))
     (zatan           (num -> num))
     (zcos            (num -> num))
     (zexp            (num -> num))
     (zlog            (num -> num))
     (zsin            (num -> num))
     (zsqrt           (num -> num))
     (ztan            (num -> num))
     (pi              3.14159)
     (e               2.71828)
     )))

;; ======================================================================

(define (init-DrScheme-on-MzScheme!)

  (add-constructor! '2vec #t)
  (add-constructor! 'viewport)
  (add-constructor! 'posn #t #t)
  (add-constructor! 'rgb #t #t #t)
  (add-constructor! 'mouse-click)

  (add-constructor! 'module #f)

  (add-default-primitives!
    `(
       (match:error    (_ -> empty))
      
       (make-rs:module  (forall (x) (x -> (module x))))

       ;; Can't do anything smarter, even though b must be a list
       ;; Need to implement *
                                        ;(cons           (forall (a b) (a (union b nil (cons _ _))-> (cons a b))))
                                        ;(set-cdr!       (forall (b) ((cons _ (! b)) b -> void)))
       (atom?          (_ -> bool)
         (predicate* (#f cons) (#t cons)))
       (build-list     (forall (a) (num (num -> a) -> (listof a))))
       (build-string   (num (num -> char) -> str))
    
       ;; Vectors
       (build-vector   (forall (a) (num (num -> a) -> (vec a))))
       (tabulate       (forall (a) (num (num -> a) -> (vec a))))
       (foreach!       (forall 
                         (v a b)
                         ((union v (vec a) (vec (! b))) (a num -> b) -> v)))
       (2vector        (forall (a) ((listof (listof a)) -> (2vec a))))
       (2make-vector   (num num -> (2vec void)))
       (2vector-init   (forall (a) (num num (num num -> a) -> (2vec a))))
       (2vector-ref    (forall (a) ((2vec a) num num -> a)))
       (2vector-set!   (forall (a) ((2vec (! a)) num num a -> void)))
       (2foreach!      (forall 
                         (v a b)
                         ((union v (2vec a) (2vec (! b))) (a num num -> b) -> v)))
       (2vector-print  (forall (a) ((2vec a) ((vec a) -> void) -> void)))

       ;; --------------------
       ;; SIXlib
       (open-viewport  (str num optional num optional num -> viewport))
       (open-pixmap    (str num optional num optional num -> viewport))
       (close-viewport (viewport -> void))

       (make-posn      (forall (x y)
                         ((union num x) (union num y) -> (posn x y))))
       (posn-x         (forall (x) ((posn x _) -> x)))
       (posn-y         (forall (y) ((posn _ y) -> y)))
       (posn?          ((union (posn _ _) _) -> bool) (predicate posn))

       (get-pixel      (viewport -> ((posn _ _) -> num)))

       (make-rgb       (forall (r g b)
                         ((union num r) (union num g) (union num b) 
                           -> (rgb r g b))))
       (rgb-red        (forall (r) ((rgb r _ _) -> r)))
       (rgb-green      (forall (g) ((rgb _ g _) -> g)))
       (rgb-blue       (forall (b) ((rgb _ _ b) -> b)))
       (rgb?           ((union (rgb _ _ _) _) -> bool)
         (predicate rgb))
       (change-color   (num (rgb _ _ _) -> void))
       (default-display-is-color? (-> bool))
    
       ;; --- Drawing ops
       (draw-viewport  (viewport -> (optional num -> void)))
       (draw-pixel     (viewport -> ((posn _ _) optional num -> void)))
       (draw-line      (viewport -> 
                         ((posn _ _) (posn _ _) optional num -> void)))
       (draw-string    (viewport -> ((posn _ _) str optional num -> void)))
       (draw-pixmap    (viewport -> (str (posn _ _) -> void)))
                              
       (clear-viewport (viewport -> (-> void)))
       (clear-pixel    (viewport -> ((posn _ _) -> void)))
       (clear-line     (viewport -> ((posn _ _) (posn _ _) -> void)))
       (clear-string   (viewport -> ((posn _ _) str -> void)))
       (clear-pixmap   (viewport -> (str (posn _ _) -> void)))
                              
       (flip-viewport (viewport -> (-> void)))
       (flip-pixel    (viewport -> ((posn _ _) -> void)))
       (flip-line     (viewport -> ((posn _ _) (posn _ _) -> void)))
       (flip-string   (viewport -> ((posn _ _) str -> void)))
       (flip-pixmap   (viewport -> (str (posn _ _) -> void)))
                              
       ;; --- Mouse ops
       (get-mouse-click      (viewport -> mouse-click))
       (ready-mouse-click    (viewport -> (union false mouse-click)))
       (ready-mouse-release  (viewport -> (union false mouse-click)))
       (query-mouse-posn     (viewport -> (union false (posn num num))))
       (mouse-click-posn     (mouse-click -> (posn num num)))
       (left-mouse-click?    (mouse-click -> bool))
       (middle-mouse-click?  (mouse-click -> bool))
       (right-mouse-click?   (mouse-click -> bool))
       (viewport-flush-input (viewport -> void))

       ;; --------------------
       ;; Module values

       ;; Miscellaneous
       (rs:major-version num)
       (rs:minor-version num)
       (rs:date        str)
       (rs:banner-lines (str -> void))
       )))

;; ----------------------------------------------------------------------

(define (init-smart-numops!)

  (st:constants #t)

  (add-constructor! 'apply+  #f)
  (add-constructor! 'apply-  #f)
  (add-constructor! 'apply*  #f)
  (add-constructor! 'apply/  #f)

  ;; The following are binary ops
  ;; they return the elements of the first set that satisfy the
  ;; appropriate relation wrt some element of the second set
  (add-constructor! '= #f #f)
  (add-constructor! 'not= #f #f)
  (add-constructor! '< #f #f)
  (add-constructor! '<= #f #f)
  (add-constructor! '> #f #f)
  (add-constructor! '>= #f #f)

  (record-super-constructor! 'num 'apply+)
  (record-super-constructor! 'num 'apply-)
  (record-super-constructor! 'num 'apply*)
  (record-super-constructor! 'num 'apply/)

  (record-super-constructor! 'num '=)
  (record-super-constructor! 'num 'not=)
  (record-super-constructor! 'num '<)
  (record-super-constructor! 'num '<=)
  (record-super-constructor! 'num '>)
  (record-super-constructor! 'num '>=)

  (install-output-type-expander!
   (match-lambda 
    [('apply+ ('list . t*)) `(+ ,@t*)]
    [('apply* ('list . t*)) `(* ,@t*)]
    [('apply- ('list . t*)) `(- ,@t*)]
    [('apply/ ('list . t*)) `(/ ,@t*)]
    [('+ t 1)               `(add1 ,t)]
    [('- t 1)               `(sub1 ,t)]
    [type type]))

  (let* ([bin-pred
          (lambda (op) (lambda (x y) 
                         '(pretty-debug
                           `(make-constructed-Tvar ,op
                                                   ,(Tvar-name x)
                                                   ,(Tvar-name y)))
                         (make-constructed-Tvar op x y)))]
         [bin-pred-r
          (lambda (op) (lambda (x y) ((bin-pred op) y x)))]
         [comparator-helper-fn
          (lambda (op reverse-op negation-op negation-reverse-op)
            ;; eg < > >= <=
            (lambda (before after Tvar bool)
              '(printf "before ~s after ~s  Tvar ~s~n" 
                       (map Tvar-name before)
                       (map Tvar-name after)
                       (Tvar-name Tvar))
              (if bool
                  (foldl (bin-pred-r op) 
                         (foldl (bin-pred-r reverse-op) Tvar before)
                         after)
                  (match (list before after)
                    [((arg) ()) ((bin-pred negation-reverse-op) Tvar arg)]
                    [(() (arg)) ((bin-pred negation-op) Tvar arg)]
                    [_ Tvar]))))])
 
    (add-default-primitives!
     `(
       ;; numbers
       (+              (forall (a) ((union a (arglistof num)) *-> (apply+ a))))
       (*              (forall (a) ((union a (arglistof num)) *-> (apply* a))))
       (-              (forall (a) ((union a (arg num (arglistof num))) 
                                    *-> (apply- a))))
       (/              (forall (a)
                               ((union a (arg num (arglistof num)))
                                *-> (apply/ a))))
       (add1           (forall (a) ((union a num) -> (apply+ (list a 1)))))
       (sub1           (forall (a) ((union a num) -> (apply- (list a 1)))))

       (=              ((arg num (arg num (arglistof num))) *-> bool)
                       (predicate-fn 
                        ,(comparator-helper-fn '= '= 'not= 'not=)))
       (<              ((arg num (arg num (arglistof num))) *-> bool)
                       (predicate-fn 
                        ,(comparator-helper-fn '< '> '>= '<=)))
       (>              ((arg num (arg num (arglistof num))) *-> bool)
                       (predicate-fn 
                        ,(comparator-helper-fn '> '< '<= '>=)))
       (<=             ((arg num (arg num (arglistof num))) *-> bool)
                       (predicate-fn 
                        ,(comparator-helper-fn '<= '>= '> '<)))
       (>=             ((arg num (arg num (arglistof num))) *-> bool)
                       (predicate-fn 
                        ,(comparator-helper-fn '>= '<= '< '>)))
       (zero?          (num -> bool)
                       (predicate-fn 
                        ,(lambda (before after Tvar bool)
                           (let ([Tvar-zero (mk-Tvar 'zero?)])
                             (new-AV! Tvar-zero (traverse-const-exact 0))
                             (if bool
                                 Tvar-zero
                                 (make-constructed-Tvar 'not= Tvar Tvar-zero))))))

       ))))

;; ----------------------------------------------------------------------

(define (init-vectors-w/-length)

  (add-constructor! 'vect #t #f)
  (add-default-primitives!
    `(
       (make-vector    (case->
                         (forall (n) ((union num n)  -> (vect void n)))
                         (forall (a n) ((union num n) a -> (vect a n)))
                         (forall (a n) ((union num n) optional a -> (vect (union a void) n)))))
       (vector         (forall (a) ((arglistof a) *-> (vect a num))))
       (vector-length  (forall (a n) ((vect a n) -> n)))
       (vector-ref     (forall (a) ((vect a _) num -> a)))
       (vector-set!    (forall (a) ((vect (! a) _) num a -> void)))
       (vector->list   (forall (a) ((vect a _) -> (listof a))))
       (list->vector   (forall (a) ((listof a) -> (vect a num))))
       (vector-fill!   (forall (a) ((vect (! a) _) a -> void)))
       (vector?        (_ -> bool)         (predicate vect))
       (build-vector   (forall (a) ((union num n) (num -> a) -> (vect a n))))
       (tabulate       (forall (a) ((union num n) (num -> a) -> (vect a n))))
       (foreach!       (forall 
                         (v a b)
                         ((union v (vect a _) (vect (! b) _)) (a num -> b)
                           -> v)))
       (2vector-print  (forall (a) ((2vec a) ((vect a _) -> void) -> void)))
        
       )))
