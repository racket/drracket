;; devel.ss
;; Development helper file
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
;; Timing

(defmacro trace-time args
  (match args
    [(fn)
     (let* ([c (box 0)]
            [f (gensym)])
       `(begin
          (define ,f ,fn)
          (define ,fn (lambda args
                        (record-time ,c
                                     (lambda () (apply ,f args)))))
          ,c))]))

(define counter-fns
  '(
     st:
     analyze-program
     zodiac:read*
     top-level-parse-defs
     top-level-parse-exp
     top-level-traverse-defs
     open-code-file
     system-expand-port
     expand-zexp->port

     minimize-constraints
     find-nonempty-tvars
     copy-live-constraints
     copy-live-constraints-noe
     copy-live-constraints-few-e
     minimize-constraints-dfa-min
     minimize-constraints-dfa-min-inv
     minimize-constraints-dfa-min-1
     minimize-constraints-dfa-min-2

     copy-constraints-equiv!
     find-nonempty-nts-rhs-nts
     calc-live-tvars-nts
     ;; copy-constraint-set
     Hopcroft-calc-equivalences

     read-za
     write-za
     calc-checks

     calc-productions!                 
     Tvar-in-type?

     resize-hash-table
     save-kernel-state
     restore-kernel-state!
     free-kernel-state!
     zodiac:zero!

     ;atenv:extend
     ;atenv:lookup
     ;atenv:change-binding
     ;atenv:capture-locs
     ;atenv:unflush
     ;atenv:flush!
     ;zodiac:free-vars

     ;chema->con
     initialize-analysis!
     get-default-bindings
     ))

(define counter-nonfns
  '( ;;live-data
     ))

(define counters
  (append
   (map
    (lambda (fn)
      (let ([c (cons 0 0)]
            [f (gensym)])
        (eval
         `(begin
            (define ,f (if (defined? ',fn) ,fn (lambda (x) x)))
            (define ,fn (lambda args
                          (record-time ',c
                                       (lambda () (apply ,f args)))))))
        (list fn c)))
    counter-fns)
   (map
    (lambda (name)
      (let ([c (cons 0 0)])
        (eval `(define ,(symbol-append name '-counter) (quote ,c)))
        (list name c)))
    counter-nonfns)))

(define (show-counters)
  (printf "COUNTERS:~n")
  (for-each
   (match-lambda
    [(f (n . t))
     (unless (zero? t)
       (printf "   ~a ~a ~a ms~n" (padr f 35) (padl n 5) (padl t 7)))])
   counters)
  (printf "   ~a ~a ~a ms~n" 
    (padr "ANALYSIS TIME" 35)
    (padl 1 5)
    (padl (apply -
            (map get-counter
              '(
                 analyze-program
                 zodiac:read*
                 top-level-parse-defs
                 top-level-parse-exp
                 open-code-file
                 expand-zexp->port
                 initialize-analysis!
                 get-default-bindings
                 )))
      7)))

(define (clear-counters!)
  (initialize-analysis!)
  (set! hash-table '())
  (set! global-defs-parsed '())
  (set! global-def-env '())
  ;;((collect-request-handler) (collect-maximum-generation))
  (for-each
   (match-lambda
    [(f c) (set-car! c 0) (set-cdr! c 0)])
   counters))

(define (get-counter c)
  (ormap
   (match-lambda
    [(f (n . t))
     (if (eq? f c) t #f)])
   counters))

;(collect-notify #t)

(define (with-counters f args)
  (clear-counters!)
  (apply f args)
  (show-counters)
  (show-stats))
 
;; ======================================================================
;; Setup live-data-counter

'(define base-data-live 0)

'(define my-collect
  (lambda args
    (apply #%collect args)
    ;;(#%collect 4)
    (set-car! live-data-counter (add1 (car live-data-counter)))
    (let ([live (truncate (/ (- (bytes-allocated) base-data-live) 1000))])
      (set-cdr! live-data-counter (max (cdr live-data-counter) live))
      (when (collect-notify)
        (printf "GC: HWM ~sK + ~sK~n" 
                (truncate (/ base-data-live 1000))
                live)
        (show-stat-small)))))
;;(collect-request-handler my-collect)

;; ======================================================================
;; Compare counters under different strategies

(define (diff-counters th1 th2)
  (th1)
  ;;((collect-request-handler) (collect-maximum-generation))
  (show-stat)
  (let ([t* (map cdadr counters)])
    (th2)
    ;;((collect-request-handler) (collect-maximum-generation))
    (show-stat)
    (printf "DIFFERENCE IN TIMES:~n")
    (for-each
     (match-lambda*
      [((f (n . t2)) t)
       (unless (and (zero? t) (zero? t2))
         (printf "   ~a ~a vs ~a, extra ~a ms~n" 
                 (padr f 35) (padl t 7) (padl t2 7) (padl (- t2 t) 7)))])
     counters t*)))

(define (diff-times files)
  (diff-counters
   (lambda ()
     (dynamic-let ([st:analysis 'sba]) (tsal files)))
   (lambda ()
     (dynamic-let ([st:analysis 'za-reanalyze]) (tsal files)))))

(define (diff-times2 files)
  (diff-counters
   (lambda ()
     (dynamic-let ([st:unit-simplify 'live]) (tsal files)))
   (lambda ()
     (dynamic-let ([st:unit-simplify 'dfa-min]) (tsal files)))))

;; ======================================================================
;; Check correctness of seperate analysis

(define ord-types '())
(define sep-types '())

(define (test-seperate-same files)
  (dynamic-let 
   ([st:type-compression '(basic-types . tidy)]
    [st:primitive-types 'inferred])
   (let ([def-types
           (lambda ()
             (printf "Calculating types ...")
             (begin0
              (filter
               (match-lambda
                [($ sym-def sym def) (symbol? sym)])
               global-out-env)
              (printf " done~n")))])
     (diff-counters
      (lambda ()
        (dynamic-let ([st:analysis 'sba]) 
                      (tsal files)
                      (set! ord-types (def-types))))
      (lambda ()
        (dynamic-let ([st:analysis 'za-reanalyze])
                      (tsal files)
                      (set! sep-types (def-types)))))
     (printf "Comparing types ... ~n")
     (unless (= (length ord-types) (length sep-types))
       (error 'test-seperate-same
              "Different lengths"))
     (for-each
      (match-lambda*
       [(($ sym-def sym1 def1)
         ($ sym-def sym2 def2))
        (unless (eq? sym1 sym2)
          (error 'test-seperate-same "Different symbols defined"))
        (unless (and (Tvar? def1) (ftype? def2))
          (error 'test-seperate-same "Definition not Tvar"))
        (unless (and (Tvar-containment? def1 def1)
                     (Tvar-containment? def2 def2))
          (error 'test-seperate-same "Tvar-containment? bad"))
        (unless (and (Tvar-containment? def1 def2)
                     (Tvar-containment? def2 def1))
          (dynamic-let ([st:type-compression '(first-order . none)])
                        (printf "Ordinary type~n")
                        (pretty-print (Tvar->SDL def1))
                        (printf "Seperate type~n")
                        (pretty-print (Tvar->SDL def2)))dev
          (error 'test-seperate-same "Definitions of ~s differ" sym1))])
      ord-types sep-types)
     (printf "Types are identical~n"))))
  
;; ======================================================================
;; Results File

(define results-file "/home/cormac/Spidey/results/results")

(define (clear-results)
  (system (format "\\rm ~s" results-file)))

(define (write-results title size)
  (let ([p (open-output-file results-file 'append)]
        [o `(RESULTS
             ,title
             (size    ,size)
             (num-ftype ,num-Atype)
             (num-AV  ,num-AV)
             (num-con ,num-con)
             (num-edge ,num-edge)
             (num-AV-a ,num-AV-in-Tvar)
             (entries ,entries-in-table)
             ,counters
             ,(mrspidey:control-fn))])
    (pretty-print o p)
    (close-output-port p)
    o))

(define (analyze-and-write-results files)
  (tsal files)
  (write-results files (apply + (map calc-AST-size global-defs-parsed))))

;; ======================================================================
;; Scaling of (st:analysis 'sba)

(define (test-scaling-file files)
  (printf "TEST-SCALING-FILE ~s~n" files)
  (dynamic-let 
   ([st:analysis 'sba])
   (analyze-and-write-results files)))

(define (test-scaling)
  (cd "/home/cormac/Spidey")
  (clear-results)
  (map test-scaling-file (map list benchmark-files))
  (test-scaling-file sba-kernel-spj)
  (test-scaling-file TC-spj)
  (test-scaling-file t11-spj)
  (test-scaling-file sba-zodiac-spj)
  (test-scaling-file sba-small-spj)
  ;;(test-scaling-file sba-spj)
  )

(define (test-scaling-sba)
  ;; Add on extra sba files one-by-one
  (recur loop ([f '()]
               [r (append
                   sba-small-spj
                   (filter
                    (lambda (x) (not (member x sba-small-spj)))
                    sba-spj))])
    (unless (null? r)
      (let ([f (append f (list (car r)))])
        (test-scaling-file f)
        (loop f (cdr r))))))

(define matlab-scaling-file "/home/cormac/Spidey/results/scaling.m")
(define get-results-file
  (case-lambda
   [() (get-results-file results-file)]
   [(file)
    (match-let*
        ([l '()]
         [_ (with-input-from-file file
              (lambda ()
                (recur loop ()
                  (let ([x (read)])
                    (unless (eof-object? x)
                      (match x
                        [('quote . _) (void)]
                        [_ (set! l (append l (list x)))])
                      (loop))))))]
         ;; Group l together by filename
         ;; grouped-l: (listof (listof info))
         [grouped-l (recur loop ([l l])
                      (match l
                        [() '()]
                        [(('RESULTS files . _) . _)
                         (let-values
                             ([(info* rest)
                               (filter-map-split
                                (match-lambda
                                 [(and x ('RESULTS files2 . _))
                                  (and (equal? files files2) x)])
                                l)])
                           (cons (reverse info*) (loop rest)))]))]
         [name->ndx
          (lambda (filename)
            (recur loop ([i 0])
              (cond
               [(= i (length wright-files)) 
                (printf "Warning: not found~n") 
                i]
               [(string=? filename (nth wright-files i)) i]
               [else (loop (add1 i))])))]
         [grouped-l (sort 
                     (match-lambda*
                      [((and a (('RESULTS f1 ('size s1) . _) . _))
                        (('RESULTS f2 ('size s2) . _) . _))
                       (< s1 s2)
                       (< (name->ndx f1) (name->ndx f2))])
                     grouped-l)]
         [_ (assert (= (length l) (apply + (map length grouped-l))))]
         [_ (pretty-print 
             (map
              (match-lambda
               [(('RESULTS files ('size s1) . _) . _)
                (list files s1)])
              grouped-l))]
         [cnth (lambda (n) (lambda (x) (cadr (nth x n))))]
         [get-counters (lambda (x) (nth x 9))]
         [get-control  (lambda (x) (nth x 10))]
         [get-time     
          (lambda (f)
            (lambda (x) (cdr (cadr (assq f (get-counters x))))))]
         [ctrl-eq?
          (lambda (para val)
            (lambda (x)
              (eq? (cadr (assq para (get-control x))) val)))]
         [select
          (lambda (pred)
            (map
             (lambda (group)
               (match (filter pred group)
                 [() #f]
                 [(x) x]
                 [m (pretty-print m)
                    (error 'get-results-file "Multiple matches")]))
             grouped-l))]
         [split
          (lambda (preds)
            (map select preds))])

      (list l cnth get-counters get-control get-time ctrl-eq?
            select split))]))


(define (scaling->matlab)
  (system (format "\\rm ~s" matlab-scaling-file))
  (match-let*
      ([(l cnth get-counters get-control get-time l-filter-ctrl split)
        (get-results-file)]
       [l-p (lambda (p) (select (l-filter-ctrl 'st:analysis p)))]
       [l-sba (l-p 'sba)]
       [l-precompress (l-p 'precompress)]
       [l-za  (l-p 'za-reanalyze)]
       [l-nc  (l-p 'no-combine)])

      ;;(pretty-print (map cadr l))
      (with-output-to-file matlab-scaling-file
        (lambda ()
          (for-each
           (match-lambda
            [(name fn)
             (printf "~s = [ " name)
             (for-each (lambda (x) (printf "~s " (if x (fn x) 0)))
                       l-sba)
             (add-zeros l-sba)
             (printf "; ")
             (for-each (lambda (x) (printf "~s " (fn x)))
                       l-precompress)             
             (add-zeros l-precompress)
             (printf "; ")
             (for-each (lambda (x) (printf "~s " (fn x)))
                       l-za)
             (add-zeros l-za)
             (printf "; ")
             (for-each (lambda (x) (printf "~s " (fn x)))
                       l-nc)
             (add-zeros l-nc)
             (printf "]~n")])
           `((ast_size  ,(cnth 2))
             (num_Tvar   ,(cnth 3))
             (num_AV    ,(cnth 4))
             (num_con   ,(cnth 5))
             (num_edges ,(cnth 6))
             (num_AV_a  ,(cnth 7))
             (entries   ,(cnth 8))
             (ttl_time      ,(get-time 'seperately-analyze-and-load-files))
             (local_time    ,(get-time 'sba-analyze-file))
             (parse_time    ,(get-time 'load-parse-expand))
             (traverse_time ,(get-time 'traverse-def))
             (combine_time  ,(get-time 'top-level-combine))
             (live_data     ,(get-time 'live-data))
             (min_time      ,(get-time 'minimize-constraints-live))
             (resize_time   ,(get-time 'resize-hash-table))
             ))))))

;; ======================================================================
;; Comparison of st:analysis 'za-reanalyze and #f

(define (compare-analyzes-files files)
  (let ([size
         (dynamic-let 
          ([st:analysis 'sba])
          (printf "~nCOMPARE-ANALYZES 'sba ~s~n" files)
          (tsal files)
          (let ([size (apply + (map calc-AST-size seperate-defs))])
            (write-results files size)
            size))])
    (for-each 
     (lambda (a)
       (printf "~nCOMPARE-ANALYZES '~s ~s~n" a files)
       (dynamic-let 
        ([st:analysis a])
        (if (memq a '(precompress za-reanalyze))
            (for-each (lambda (c)
                        (dynamic-let
                         ([st:unit-simplify c])
                         (tsal files)
                         (write-results files size)))
                      '(live-few-e
                        dfa-min))
            (begin
              (tsal files)
              (write-results files size)))))
     '(precompress za-reanalyze no-combine))))

(define (compare-analyzes)
  (cd "/home/cormac/Spidey")
  (clear-results)
  (compare-analyzes-files tp-spj)
  (compare-analyzes-files sba-kernel-spj)
  (compare-analyzes-files TC-spj)
  (compare-analyzes-files t11-spj)
  (compare-analyzes-files sba-zodiac-spj)
  (compare-analyzes-files sba-small-spj)
  ;;(test-scaling-file sba-spj)
  (compare-analyzes-sba)
  )

(define (compare-analyzes-sba . rest)
  ;; Add on extra sba files one-by-one
  (recur loop ([f '()]
               [r sba-spj]
               [n (match rest
                    [() 0]
                    [(n) n])])
    (unless (null? r)
      (let ([f (append f (list (car r)))])
        (when (<= n 0)
          (compare-analyzes-files f))
        (loop f (cdr r) (sub1 n))))))


(define matlab-seperate-file "/home/cormac/Spidey/results/seperate.m")

;; ======================================================================

(define (ts file)
  (seperately-analyze-file-thunk* (files->file-thunk* file) "test/out.za")
  (read-constraint-set "test/out.za"))

(define (tsal files)
  (clear-counters!)
  (seperately-analyze-and-load-files files)
  (show-counters)
  (void))

;; ======================================================================
;; Some benchmarks

(define tp-spj (list "mod/part1.ss" "mod/part2.ss"))
(define mod-spj (list "mod/mod1.ss" "mod/mod2.ss" "mod/mod3.ss"))
(define TC-spj (list "mod/TC/env.ss" 
                     "mod/TC/parse.ss"
                     "mod/TC/type.ss"
                     "mod/TC/eval.ss"
                     "mod/TC/go.ss"
                     ;;"mod/TC/test.ss"
                     ))
(define t11-spj (list "mod/11/FrontEnd.ss"
                      "mod/11/Registers.ss"
                      "mod/11/Memory.ss"
                      "mod/11/Machine.ss"
                      "mod/11/go.ss"
                      ))

(define (tp)   (tsal tp-spj))
(define (mod)  (tsal mod-spj))
(define (TC)   (tsal TC-spj))
(define (t11)  (tsal t11-spj))

;; ======================================================================
;; SBA benchmarks

(define sba-files
  (list "library"
        "env"
        "config"
        "driver"
        "sba"

        "zodiac"
        "compat"
        "loadexpand"
        "bind"
        "traverse"
        "global-env"
        "prototype"

        "hash"
        "kernel"

        "toplevelenv"
        "templates"
        "languages-abstract"

        "sdl"
        "results"

        "calc-checks"
        "hyper"
        "seperate"

        "type-con"
        "contained"
        "gram"
        "min"
        "min2"
        "dfa-min"
        ))

(define sba-spj 
  (map (lambda (s) (string-append "/home/cormac/Spidey/mod/sba/" s ".ss"))
       sba-files))

(define (sba-spj-until x)
  (recur loop ([l sba-spj])
    (cons (car l)
          (if (substring? x (car l))
              '()
              (loop (cdr l))))))

(define sba-small-spj (sba-spj-until "kernel"))
(define sba-medium-spj (sba-spj-until "calc-checks"))

(define sba-kernel-spj
  (map (lambda (s) (string-append "/home/cormac/Spidey/mod/sba/" s ".ss"))
       (list "hash" "kernel" "test-kernel")))

(define sba-zodiac-spj
  (map (lambda (s) (string-append "/home/cormac/Spidey/mod/sba/" s ".ss"))
       (list "bind" "compat" "env" "zodiac" "test-zodiac")))

;; ----------------------------------------------------------------------

(define (separate-expt)
  (st:polymorphism 'compress)
  (st:type-compression-poly 'live-few-e)
  (st:library-prims #f)
  (st:topo-sort #f)
  (st:use-module #t)

  (for-each
   (lambda (p)
     (printf "~n========================================~n")
     (printf "SIMPL STRATEGY: ~s~n" p)
     (st:unit-simplify p)
     (st:analysis 'za-reanalyze)
     (tsal sba-kernel-spj)
     (write-results sba-kernel-spj 0)
     (st:analysis 'za)
     (for-each
      (lambda (file)
        (printf "~n========================================~n")
        (printf "FILES: ~s~n" file)
        (system (format "touch ~a" file))
        (tsal sba-kernel-spj)
        (write-results file 0))
      sba-kernel-spj))
   '(live live-few-e dfa-min-AV)))

(define (tsba) (tsal sba-spj))
(define (tker) (tsal sba-kernel-spj))
(define (tzod) (tsal sba-zodiac-spj))
(define (tker2)
  (clear-counters!)
  (initialize-analysis!)
  (read-constraint-set-to-global-env "~/Spidey/mod/sba/hash.ss")
  (read-constraint-set-to-global-env "~/Spidey/mod/sba/test-kernel.ss")
  (hyper (sba-analyze-file (files->file-thunk* "~/Spidey/mod/sba/kernel.ss")))
  (report-unbound-vars)
  (show-counters))

;; ======================================================================

(define (test-seperate)
  (for-each
   (lambda (thunk)
     (for-each
      (lambda (c)
        (for-each
         (lambda (b)
           (st:unit-simplify c)
           (st:add-hyper-links b)
           (printf "==============================~n")
           (printf "Type-compression ~s   Hyper-links ~s~n" c b)
           (thunk)
           (show-stat))
         (list #f )))
      (list 'live 'dfa-min)))
   (list ;;(lambda () (tp))
         (lambda () (TC) (system "ls -l mod/TC*.za"))
         ;;(lambda () (tsba) (system "ls -l sba/*.za"))
         )))
   
;; ======================================================================

(define (st:tybe . args)
  (dynamic-let
   ([st:type-compression '(higher-order . live-few-e)])
   (pretty-print (apply st:type-fn args)))
  (dynamic-let
   ([st:type-compression '(higher-order . dfa-min)])
   (apply st:type-fn args)))

;; ======================================================================

(define (compare-poly-file file)
  (printf "~n========================================~nFile: ~s~n" files)
  (for-each-parameter 
    st:polymorphism
    (lambda (p)
      (printf "~n==============================~nPOLY STRATEGY: ~s~n" p)
      (if (eq? p 'compress)
        (for-each-parameter 
          st:type-compression-poly 
          (lambda (c)
            (when (memq c '(live live-few-e))
              (compare-poly-files-one files))))
        (compare-poly-file-one file)))))

(define (compare-poly-file-one file)
  (printf "~n========================================~n")
  (printf "FILE: ~s~n" file)
  (printf "POLY STRATEGY: ~s ~s~n" 
    (st:polymorphism) (st:type-compression-poly))
  (clear-counters!)
  (st:analyze files)
  (show-counters)
  (show-stat)
  (let ([size (calc-AST-size defs-ordered)])
    (write-results files size)))

(define (make-compare-poly-file files)
  `(dynamic-let 
    ([st:topo-sort #t]
     [st:library-prims #t])
    (begin 
      ,@(map
         (lambda (p)
           `(begin
              (st:polymorphism (quote ,p))
              ,(if (eq? p 'compress)
                   `(begin 
                      ,@(map
                         (lambda (c)
                           `(begin
                              (st:type-compression-poly (quote ,c))
                             (compare-poly-file-one ,files)))
                         '(live live-few-e dfa-min-AV)))
                   `(compare-poly-file-one ,files))))
         (map car (st:polymorphism '?))))))

(define (all-compare-poly) (for-each compare-poly-file benchmark-files))
(define (wright-compare-poly) (for-each compare-poly-file wright-files))

(define wright-ok
  `(begin ,@(map make-compare-poly-file wright-files)))

(define wright-ok
  '(begin (dynamic-let ([st:topo-sort #t]
                      [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/boyer.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/boyer.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/boyer.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/boyer.scm"))))
                (begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/boyer.scm"))
                (begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/boyer.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/graphs.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/graphs.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/graphs.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/graphs.scm"))))
                (begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/graphs.scm"))
                (begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/graphs.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/lattice.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/lattice.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/lattice.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/lattice.scm"))))
                (begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/lattice.scm"))
                (begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/lattice.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/matrix.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/matrix.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/matrix.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/matrix.scm"))))
                (begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/matrix.scm"))
                (begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/matrix.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/maze.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/maze.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/maze.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/maze.scm"))))
                (begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/maze.scm"))
                (begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/maze.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/nbody.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/nbody.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/nbody.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/nbody.scm"))))
                (begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/nbody.scm"))
                (begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/nbody.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/splay.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/splay.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/splay.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/splay.scm"))))
                (begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/splay.scm"))
                (begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/splay.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/browse.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/browse.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/browse.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/browse.scm"))))
                (begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/browse.scm"))
                (begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/browse.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/check.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/check.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/check.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/check.scm"))))
                (begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/check.scm"))
                (begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/check.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/nucleic.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/nucleic.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/nucleic.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/nucleic.scm"))))
                '(begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/nucleic.scm"))
                '(begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/nucleic.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin '(begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/nucleic-2.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/nucleic-2.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/nucleic-2.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/nucleic-2.scm"))))
                '(begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/nucleic-2.scm"))
                '(begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/nucleic-2.scm"))))
       (dynamic-let ([st:topo-sort #t] [st:library-prims #t])
         (begin (begin (st:polymorphism 'none)
                       (compare-poly-file-one "~/Spidey/wright/dynamic.scm"))
                (begin (st:polymorphism 'compress)
                       (begin (begin (st:type-compression-poly 'live)
                                     (compare-poly-file-one "~/Spidey/wright/dynamic.scm"))
                              (begin (st:type-compression-poly 'live-few-e)
                                     (compare-poly-file-one "~/Spidey/wright/dynamic.scm"))
                              (begin (st:type-compression-poly 'dfa-min-AV)
                                     (compare-poly-file-one "~/Spidey/wright/dynamic.scm"))))
                '(begin (st:polymorphism 'copy-con)
                       (compare-poly-file-one "~/Spidey/wright/dynamic.scm"))
                '(begin (st:polymorphism 'reanalyze)
                       (compare-poly-file-one "~/Spidey/wright/dynamic.scm"))))))

;; ======================================================================

(define (poly->matlab . files)
  (match-let*
      ([f "/home/cormac/papers/popl95/ Spidey/results/poly.m"]
       [(l cnth get-counters get-control get-time ctrl-eq? select split)
        (apply get-results-file files)]
       [l-p (lambda (p) (select (ctrl-eq? 'st:polymorphism p)))]
       [l-none (l-p 'none)]
       [l-copy (l-p 'copy-con)]
       [l-reanalyze (l-p 'reanalyze)]
       [l-compress-alg
        (lambda (alg)
          (select
           (lambda (x)
             (and 
              (eq? (cadr (assq 'st:polymorphism (get-control x))) 'compress)
              (eq? (cadr (assq 'st:type-compression-poly (get-control x)))
                   alg)))))]
       [l-compress-live (l-compress-alg 'live)]
       [l-compress-live-noe (l-compress-alg 'live-few-e)]
       [lists (list  l-reanalyze
                     l-copy 
                     l-compress-live
                     l-compress-live-noe
                     (l-compress-alg 'dfa-min-AV)
                     l-none)]
       [show-pairs `((ast_size  ,(cnth 2))
                     (num_Tvar   ,(cnth 3))
                     (num_AV    ,(cnth 4))
                     (num_con   ,(cnth 5))
                     (num_edges ,(cnth 6))
                     (num_AV_a  ,(cnth 7))
                     (entries   ,(cnth 8))
                     (ttl_time      ,(get-time 'st:analyze))
                     (local_time    ,(get-time 'sba-analyze-file))
                     (parse_time    ,(get-time 'load-parse-expand))
                     (traverse_time ,(get-time 'traverse-def))
                     (combine_time  ,(get-time 'top-level-combine))
                     (live_data     ,(get-time 'live-data))
                     (min_time      ,(get-time 'minimize-constraints))
                     (resize_time   ,(get-time 'resize-hash-table))
                     (topo          ,(get-time 'topological-sort))
                     (inst          ,(get-time 'instantiate-polymorphic-def))
                     )])

    (if (file-exists? f) (delete-file f))

    ;;(pretty-print (map cadr l))

    (for-each
         
     (match-lambda
      [(name fn)
       (printf "==============~n~s~n" name)
       (for i 0 (apply max (map length lists))
            (printf "~a: " 
                    (let* ([none (nth l-none i)]
                           [f (if none (cadr none) "-")])
                      (if (string? f)
                          (padr f 30)
                          f)))
            (for-each
             (lambda (l)
               (let ([x (nth l i)])
                 (printf "~a " (if x (padl (fn x) 7) "      -"))))
             lists)
            (newline))])
     show-pairs)

    (with-output-to-file f
      (lambda ()
        (for-each
         
         (match-lambda
          [(name fn)
           (printf "~s = [ " name)
           (for-each
            (lambda (l)
              (for-each (lambda (x) (printf "~s " (if x (fn x) 0))) l)
              (if (eq? l l-none)
                  (printf "]~n~n")
                  (printf "; ")))
            lists)])

         show-pairs)))))

(define (poly->fig . files)
  (match-let*
      ([f "/home/cormac/papers/popl97/analysis-times.tex"]
       [(l cnth get-counters get-control get-time ctrl-eq? select split)
        (apply get-results-file files)]
       [l-p (lambda (p) (select (ctrl-eq? 'st:polymorphism p)))]
       [l-none (l-p 'none)]
       [l-copy (l-p 'copy-con)]
       [l-reanalyze (l-p 'reanalyze)]
       [l-compress-alg
        (lambda (alg)
          (select
           (lambda (x)
             (and 
              (eq? (cadr (assq 'st:polymorphism (get-control x))) 'compress)
              (eq? (cadr (assq 'st:type-compression-poly (get-control x)))
                   alg)))))]
       [l-compress-live (l-compress-alg 'live)]
       [l-compress-live-noe (l-compress-alg 'live-few-e)]
       [l-compress-dfa (l-compress-alg 'dfa-min-AV)]
       [lists (list  )]
       [atime
        (lambda (x) 
          (if x 
              (- ((get-time 'st:analyze) x) 
                 ((get-time 'load-parse-expand) x))
              0))]
       [doit
        (lambda ()
          (for-each
           (lambda (name lines none copy reanalyze live live-noe dfa)
             (printf "   {\\tt ~a } & ~s & ~ss " 
                     name lines 
                     (/ (round (/ (atime none) 10.0)) 100))
             (for-each
              (lambda (x)
                (printf " & ~a " 
                        (if (zero? (atime x)) 
                            '*
                            (/ (round (/ (atime x) (atime none) 0.010))
                               100))))
              (list copy reanalyze live live-noe dfa))
             (printf " \\\\ ~n"))
           '(lattice browse splay check graph boyer  
                     matrix maze nbody nucleic)
           '(215 233 265 281 621 624 744 857 880 3335)
           l-none
           l-copy 
           l-reanalyze
           l-compress-live
           l-compress-live-noe
           l-compress-dfa))]
       )
    (pretty-print (map length (list
                                          l-none
           l-copy 
           l-reanalyze
           l-compress-live
           l-compress-live-noe
           l-compress-dfa)))

    (doit)
    (if (file-exists? f) (delete-file f))
    (with-output-to-file f doit)))
;; ======================================================================

(define (fixup x)
  (let ([y (recur loop ([x x]) 
             (if (null? x)
               '()
               (cons (caddr x) (loop (cddddr x)))))])
    (apply + y)))



