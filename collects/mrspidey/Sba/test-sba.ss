;; test-suite1.ss
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

;; Testing files:

(define (run-test-suites fn)
  (letrec ([doit
             (case-lambda
               [(s) (doit s 0 #f)]
               [(s a) (doit s a #f)]
               [(s a c)
                 (fn (string-append "~/Spidey/test-suite/" s))
                 (when a
                   (unless (= (type-assert-failed-total 0) a)
                     (error 'run-test-suites "Wrong # failed assertions!")))
                 (when c
                   (unless (= (total-checks 0) c)
                     (error 'run-test-suites "Wrong # checks!")))])])

    ;; --- Core Scheme
    (doit "testall.ss" 1)
    (doit "test-const.ss")
    (doit "test-exprs.ss")
    (doit "test-if-split.ss")
    (doit "test-flow.ss")
    (doit "test-box.ss")
    (doit "test-user-prims.ss")
    (doit "test-struct.ss")
    (doit "test-mvalues.ss")
    (doit "test-checks.ss" 0 6)
    (doit "test-no-checks.ss" 0 0)

    ;; --- Objects
    (doit "test-obj-syntax.ss")
    (doit "test-obj-simple.ss")
    (doit "test-obj-thread.ss")
    (doit "test-obj-derive.ss")
    (doit "mflatt.ss" 0 3)
    (doit "../test/trivia/trivia-unit.ss" 0 1)

    ;; --- Units
    (dynamic-let ([st:system-expand #t])
      (dynamic-let ([st:fo-units #t]) (doit "test-unit.ss" 0))
      (dynamic-let ([st:fo-units #f]) (doit "test-unit.ss" 0))
      ;;(doit  "test-fo-units.ss" 0)
      )
    (doit "../test/trivia/trivia-unit.ss" 0 1)

    "MrSpidey passes the test suite :-)"
    ))

;; ----------------------------------------------------------------------

(define mred-system 
  '(
     "autoload.ss"
     "autosave.ss"
     "canvas.ss"
     "connect.ss"
     "console.ss"
     "containr.ss"
     "contfram.ss"
     "contkids.ss"
     "contpanl.ss"
     "cparens.ss"
     "cppmode.ss"
     "edframe.ss"
     "edit.ss"
     "exit.ss"
     "exn.ss"
     "fileutil.ss"
     "finder.ss"
     "findstr.ss"
     "frame.ss"
     "group.ss"
     "guiutils.ss"
     "handler.ss"
     "html.ss"
     "hyper.ss"
     "hypersig.ss"
     "hyprdial.ss"
     "hypredit.ss"
     "hyprfram.ss"
     "icon.ss"
     "include.ss"
     "keys.ss"
     "link.ss"
     "macros.ss"
     "mcache.ss"
     "menu.ss"
     "mode.ss"
     "mrsystem.ss"
     "noconsle.ss"
     "panel.ss"
     "paren.ss"
     "prefs.ss"
     "project.ss"
     "sig.ss"
     "sparen.ss"
     "ssmode.ss"
     "url.ss"
     "version.ss"
     ))
;; ----------------------------------------------------------------------

(define benchmark-files
  (append
   (map (lambda (x) (string-append "~/Spidey/benchmarks/" x ".ss"))
        '(
          "one"
          "const"
          "sum"
          "qsort"
          ;; Programs from experiment
          "gauss"
          "du"
          "hanoi"
          "merge"
          "np"
          "tc"

          "taut"

          ;; Misc others
          "sba"
          "bignum"
          ;;"TC"
          "elev"
          "dfs-bfs"
          "checks"
          "gauss-imp"
          "gauss-fn"
          "Cannibals-sba"
          "mst"
          ;;"gab-boyer"
          ;;"nucleic"  -- internal defines, etc
          ;;"nucleic2" -- internal defines, etc
          ;;"slatex"   -- too large
          ))

   (map (lambda (x) (string-append "~/Spidey/wright/" x ".scm"))
        '(
          "boyer"
          "browse"
          "check"
          "nbody"
          "graphs1"
          "graphs2"
          "lattice-cormac"
          ;;"dynamic"
          ))))

(define test-files 
  (append
   (list "~/Spidey/test/sum.ss")
   (list "~/Spidey/test-suite/testall.ss")
   ;; Can't use testnumeric as general test 
   ;; type annotations require accurate numeric constants
   ;; (list "~/Spidey/test-suite/testnumeric.ss")
   (map (lambda (x) (string-append "~/Spidey/test/" x ".ss"))
        '(
          "defstruct"
          "defstruct2"))
   benchmark-files))

'(define test-files 
   '("~/Spidey/test/one.ss"
     "~/Spidey/test/sum.ss"
     "~/Spidey/sba/testall.ss"
     "~/Spidey/test/du.ss"))


;(define test-files '("test/one.ss" "test/test.ss" "test/readlot.ss"))

;; ======================================================================
;; Helper functions

(define (exec-mzscheme exps)
  (recur loop ( [s "mzscheme -x -g -a -f /home/cormac/scheme/mzschemerc.ss"]
                [exps exps])
    (if (null? exps)
      (begin
        (printf "Command: ") (display s) (newline)
        (system s))
      (loop 
        (string-append s (format " -e '~s'" (car exps)))
        (cdr exps)))))

(define (for-each-parameter para thunk)
  (for-each
   (lambda (p) (dynamic-let ([para p]) (thunk p)))
   (map car (para '?))))

(define (for-each-parameters para-list thunk)
  (if (null? para-list)
      (thunk)
      (for-each-parameter
       (car para-list)
       (lambda (x)
         (for-each-parameters (cdr para-list) thunk)))))

(define (for-each-quoted-parameters para-list thunk)
  (recur loop ([l para-list][d '()])
    (match l
      [()
       (printf "CONFIGURATION: ~s~n" d)
       (thunk)]
      [(p . rest)
       (for-each-parameter
        (eval p)
        (lambda (x) (loop (cdr l) (cons `(,p ,x) d))))])))

(define (smart-for-each-quoted-parameters para-list limit thunk)
  (for num-chgs 0 (add1 (min limit (length para-list)))
       ;; Try changing num-chgs parameters
       (recur loop ([l para-list][d '()][n num-chgs])
         (match l
           [()
            (when (zero? n)
              ;; run the test
              (printf "CONFIGURATION: num-chgs=~s ~s~n" num-chgs d)
              (thunk))]
           [(p . rest)
            (let* ([para (eval p)]
                   [options (map car (para '?))]
                   [default (car options)])
              ;; --- First don't change
              (dynamic-let 
               ([para default])
               (loop rest (cons `(,p ,default) d) n))
              ;; Now change
              (unless (zero? n)
                (for-each
                 (lambda (v) 
                   (dynamic-let
                    ([para v]) 
                    (loop rest (cons `(,p ,v) d) (sub1 n))))
                 (cdr options))))]))))

(define (for-every-nth f l n)
  (recur loop ([i (random (inexact->exact n))][l l])
    (unless (null? l)
      (if (zero? i)
        (begin
          (f (car l))
          (loop n (cdr l)))
        (loop (sub1 i) (cdr l))))))

; ======================================================================
; Check minimization algorithms preserve meaning

(define min-algs-to-check
  '( none
     nonempty
     nonempty-copy
     live
     live-few-e
     ;live-few-e-L
     ;live-few-e-U
     ;(live-few-e dfa-min)
     ;(live-few-e dfa-min-inv)
     ;(live-few-e dfa-min-inv dfa-min)
     ;(live-few-e dfa-min dfa-min-inv)

     ;(live-few-e dfa-min-1)
     ;(live-few-e dfa-min-1)
     ;(live-few-e dfa-min-2)
     ;(live-few-e dfa-min-2)
     ;(live-few-e dfa-min-1 live dfa-min-2)
     ;(live-few-e dfa-min-2 live dfa-min-1)

     (dfa-min-lub)
     (dfa-min-glb)
     (dfa-min-lub dfa-min-glb)
     (dfa-min-glb dfa-min-lub)

     ;(live-few-e min-table)
     ))

(define (check-min-ok-file file)
  (let* 
    ( [defs (analyze-program file)]
      [type-annotations (calc-type-annotations defs)]
      [tvar* (filter Tvar? (map type-annotation-FlowType type-annotations))]
      ;;[_ (pretty-print `(Tvar* ,(map Tvar-name tvar*)))]
      [min-algs min-algs-to-check]
      [p (make-parameter-list 
           (car min-algs) 
           (map (lambda (x) (list x "")) min-algs))]
      [min-con-1
        (lambda (which Tvar)
          (let*-vals
            ([(_ Tvar->nutvar) 
               (minimize-constraints which '() (list Tvar) '() '())])
            (Tvar->nutvar Tvar)))])

    (printf "Testing ~s, ~s Tvar~n" file (length tvar*))
    ;; Compression algorithms don't preserve filters,
    ;; so turn all filters into edges
    (for-each
      (lambda (tvar)
        (when (Tvar? tvar)
          (set-Tvar-constraints!
            tvar
            (filter
              (match-lambda
                [($ con) #t]
                [($ con-filter _ _ to)
                  (new-edge! tvar to)
                  #f])
              (Tvar-constraints tvar)))))
      list-ftype)

    (for-each-parameter 
      p
      (lambda (v)
        (printf "Minimization strategy ~s~n" v)
        (for-each
          (lambda (Tvar)
            (printf ".") (flush-output)
            (let* ( [live-copy (min-con-1 'none Tvar)]
                    [p-copy    (min-con-1 (p)   Tvar)])
              (Tvar-equiv?
                (lambda ()
                  (printf "Original is:~n")
                  (pretty-print (Tvar-name Tvar)))
                "Live copy" live-copy
                (format "~s p-copy" (p)) p-copy)))
          tvar*)
        (newline)))))

(define (Tvar-equiv? fail-thunk name-A A name-B B)
  (let* ( [c1 (Tvar-containment? A B)]
          [c2 (Tvar-containment? B A)])
    (unless (and c1 c2)
      (fail-thunk)
      (printf "~a is:~n" name-A) 
      (pretty-print (Tvar-name A))
      (printf "~a is:~n" name-B)
      (pretty-print (Tvar-name B))
      (if c1
        (printf "~s is larger!~n" name-B)
        (printf "~s is smaller!~n" name-B))
      (error 'Tvar-equiv? "Failure!"))))

(define (check-min-ok-all)
  (for-each check-min-ok-file benchmark-files))

;; ======================================================================

(define (check-type-file f)
  (printf "==================================================~n")
  (printf "File: ~s~n" f)
  (st: f)
  (pretty-print (st:type)))

; ======================================================================
  
(define (check-SDL-file f)
  (printf "==================================================~n")
  (printf "File: ~s~n" f)
  (st: f)
  (let* ([l list-ftype])
    (for-each-parameter
      st:type-compression
      (lambda (t)
          (printf "----------------------------------------~n")
          (printf "Type compression: ~s~n" t)
          (for-each
            (lambda (Tvar) 
              (when (Tvar? Tvar)
                (printf "~s~n" (Tvar-name Tvar))
                (flush-output)
                (let ([sdl (Tvar->SDL Tvar)])
                  (void))))
            l)))))

;; ----------------------------------------------------------------------

(define (check-SDL2-file f)
  (printf "==================================================~n")
  (printf "File: ~s~n" f)
  (st: f)
  (let* ([l list-ftype]
         [len (length l)])
    (smart-for-each-quoted-parameters
      '(st:type-compression
         st:primitive-types 
         st:naming-strategy )
      2
      (lambda () 
        (for-every-nth
          (lambda (Tvar) 
            (when (Tvar? Tvar)
              (printf "~s " (FlowType-num Tvar))
              (flush-output)
              (let ([sdl (Tvar->SDL Tvar)])
                (void))))
          l 
          ;; do (* 3 (sqrt len)) cases
          (truncate (/ len (* 3 (sqrt len)))))
        (newline)))))

;; ----------------------------------------------------------------------


;; ### DOESNT WORK CAUSE LISTIFY ETC
(define (check-SDL3-file f)
  (printf "==================================================~n")
  (printf "File: ~s~n" f)
  (st: f)
  (let* ([l list-ftype]
          [len (length l)])
    (dynamic-let ([st:primitive-types 'inferred])
      (smart-for-each-quoted-parameters
        '(st:type-compression
           st:naming-strategy )
        1
        (lambda () 
          (for-every-nth
            (lambda (Tvar) 
              (when (Tvar? Tvar)
                (printf "~s " (FlowType-num Tvar))
                (flush-output)
                (let*-vals
                  (;; minimize w/ live
                    [(_ Tvar->nutvar)
                      (minimize-constraints 'live '() (list Tvar))]
                    [tvar-live (Tvar->nutvar Tvar)]
                    [sdl (dynamic-let
                           ([st:listify-etc #f])
                           (Tvar->SDL tvar-live))]
                    [tvar2 (mk-Tvar 'check-SDL3)])
                  (pretty-print sdl)
                  (tschema->con
                    (expand-input-type sdl)
                    tvar2 'check-SDL3: '())
                  (Tvar-equiv? 
                    (lambda ()
                      (pretty-print 
                        `( st:type-compression ,(st:type-compression)
                           st:naming-strategy ,(st:naming-strategy)
                           sdl ,sdl)))
                    "Live" tvar-live
                    "Regenerated" tvar2))))
            l
            ;; do (* 3 (sqrt len)) cases
            (truncate (/ len (* 3 (sqrt len)))))
          (newline))))))

;; ======================================================================

(define (acid-test-analysis sub-test)
  (lambda (f)
    (smart-for-each-quoted-parameters
     '(st:constants
       st:if-split
       st:flow-sensitive
       st:numops
       st:special-fo-prims
       ;;st:system-expand
       st:see-void)
     3
     (lambda () (sub-test f)))))

(define (acid-acid-test-analysis f)
  ((acid-test-analysis acid-test-SDL) f))

;; ----------------------------------------------------------------------

(define (all-test-sba-terminates)
  (for-each acid-test-SDL test-files)
  (for-each acid-acid-test-analysis test-files))

;; ======================================================================

(define compare-min-file-results (void)) 
(define (compare-min-file file)
  (set! compare-min-file-results '())
  (for-each-parameter
    st:unit-simplify
    (lambda (p)
      (unless (eq? p 'none)
        (pretty-print `(st:unit-simplify ,p))
        (let*-vals
          ( [(_ t real-t) (time-apply (lambda () (st: file)))])
          (set! compare-min-file-results
            (cons (list p t) compare-min-file-results))
          (pretty-print 
            `(compare-min-file-results ,compare-min-file-results))))))
  compare-min-file-results)

;; ======================================================================
;; EXPERIMENT A
;;
;; Figure describing Behavior of Constraint Compression Algorithms

(define compress-files
  (map (lambda (x) (string-append "~/papers/compress/bench/compress/" x ".ss"))
    '(
       "t"
       "map"
       "reverse2"
       "substring"
       "qsortmod"
       "unify"
       "hopcroft"
       "check-mod"
       "escher"
       "scanner"
       )))

(define (fig6-file file)
  (dynamic-let ( [st:compare-min-algs #t]
                 [st:unit-read-za #f]
                 [st:unit-write-za #f]
                 [st:use-fo-ftype #f]
                 [st:polymorphism 'compress]
                 [st:if-split #f]
                 [st:flow-sensitive #f]
                 [st:unit-separate-S #f]
                 )
    (st:analyze file)))

(define (fig6)
  (for-each fig6-file compress-files))

;; ======================================================================
;; EXPERIMENT B
;;
;; Polymorphic analysis times

(define wright-files
  (map (lambda (x) (string-append "~/papers/compress/bench/poly/" x ".ss"))
    '(
       "t"
       "lattice"
       "browse"
       "splay"
       "check"
       "graphs"
       "boyer"
       "matrix"
       "maze"
       "nbody"
       "nucleic-3"
       ;; "dynamic"

       )))

;(define wright-poly-files 
;  (map (lambda (file) (regexp-replace ".ss$" file ".poly.ss"))
;    wright-files))

(define fig7-algs
  '( (traverse none)
     (none none)
     (compress (dfa-min-lub))
     (compress (dfa-min-glb))
     (compress live-few-e)
     (compress live)
     (compress nonempty)
     (compress none)
     ;;(copy-con none)
     (reanalyze none)))

(define (fig7-file-alg a b file)
  (dynamic-let ( [st:polymorphism (if (eq? a 'traverse) 'none a)]
                 [st:type-compression-poly b]
                 [st:use-fo-ftype #f]
                 [st:if-split #f]
                 [st:flow-sensitive #f]
                 [keep-S-closed (if (eq? a 'traverse) #f (keep-S-closed))])
    (printf "--------------------------------------------------------~n")
    (printf "FILE: ~s ANALYSIS ~s~n" file (list a b))
    (clear-counters!)
    (set! need-label-types #t)
    (st: file)
    (printf "FILE: ~s ANALYSIS ~s~n" file (list a b))
    (show-counters)
    (show-stat-small)
    (list a b (get-counter 'top-level-traverse-defs))))

(define (fig7-file file)
  (let* ( [file (topo-file file)]
          [r (map
               (match-lambda [(a b) (fig7-file-alg a b file)])
               fig7-algs)])
    (pretty-print `(RESULTS-FOR ,file ,r))
    (list file r)))

(define (fig7)
  (recur loop ([files wright-files][r* '()])
    (unless (null? files)
      (let* ( [r (fig7-file (car files))]
              [r* (append r* (list r))])
        (pretty-print `(RESULTS-TO-DATE ,r*))
        (loop (cdr files) r*)))))

(define (fig7s-file file)
  ;; Spawns nu mzscheme each time
  (let* ( [file (topo-file file)])
    (for-each
      (match-lambda 
        [(and analyze (a b))
          (printf "--------------------------------------------------------~n")
          (printf "FILE: ~s ANALYSIS ~s~n" file analyze)
          (exec-mzscheme
            `( (define __NO_DEBUGGING 1)
               (ST)
               (set! mrspidey:zprogress (lambda x 1))
               (fig7-file-alg (quote ,a) (quote ,b) ,file)
               (exit)))])
      fig7-algs)))

(define (fig7s)
  (for-each fig7s-file (cdr wright-files)))

;; ======================================================================
;; EXPERIMENT C
;; Test how analysis scales to large programs
;; Have: 
;;  analyze-scanner.ss
;;  analyze-zodiac.ss
;;  analyze-small.ss  - part of sba analysis
;;  analyze-no-min.ss - sba except minimization
;;  analyze.ss        - all of sba
;;
;;  analyze-CDL.ss    - usless, units never used
;;  analyze-min.ss    - useless, units never used

(define (test-scalable-alg alg file)
  (printf "--------------------------------------------------------~n")
  (printf "FILE: ~s SIMPLIFICATION ~s~n" file alg)
  (dynamic-let ( [st:unit-simplify (if (eq? alg 'traverse) 'none alg)]
                 [st:use-fo-ftype #f]
                 [st:if-split #f]
                 [st:flow-sensitive #f]
                 [keep-S-closed (if (eq? alg 'traverse) #f (keep-S-closed))]
                 [st:unit-read-za #f]
                 [st:unit-write-za #f]
                 [st:unit-separate-S #t])
    (clear-counters!)
    (st:analyze file)
    (printf "FILE: ~s SIMPLIFICATION ~s~n" file alg)
    (show-counters)
    (pretty-print (st:control))
    (show-stat-small)))

(define (bad-alg-for-file? alg file)
  (cond
    [(equal? file "analyze-small.ss")
      ;; none ran to 404M
      (memq alg '(none nonempty))]
    [(equal? file "analyze-no-min.ss")
      (memq alg '(none nonempty))]
    [(equal? file "analyze.ss")
      (memq alg '(none nonempty nonempty-copy))]
    [(equal? file "~/papers/compress/bench/separate/nucleic/nucleic.ss")
      (memq alg '())]
    [else #f]))

(define (test-scalable-file file)
  (for-each
    (lambda (t)
      (unless (bad-alg-for-file? t file)
        (exec-mzscheme 
          `( (define __NO_DEBUGGING 1)
             (ST)
             ;;(expander-eval (quote (defmacro cache-exp (exp za) exp)))
             ;;(expander-eval (quote (defmacro cache-inv (exp za) exp)))
             (set! mrspidey:zprogress (lambda x 1))
             (test-scalable-alg (quote ,t) ,file)
             (exit)))))
    '(traverse none nonempty live live-few-e dfa-min-lub dfa-min-glb)))

(define (test-scalable-all)
  (for-each test-scalable-file
    '( "analyze-scanner.ss"
       "analyze-zodiac.ss" 
       "analyze-small.ss"       
       "analyze-no-min.ss"
       "analyze.ss")))

;; ======================================================================
;; EXPERIMENT D
;; Test separate analysis

(define (test-separate-analyze alg read-za file)
  (printf "--------------------------------------------------------~n")
  (printf "FILE: ~s SIMPLIFICATION ~s ~s~n" file alg read-za)
  (let ([files '()])
    (dynamic-let 
      ( [st:unit-simplify (if (eq? alg 'traverse) 'none alg)]
        [st:use-fo-ftype #f]
        [st:if-split #f]
        [st:flow-sensitive #f]
        [keep-S-closed (if (eq? alg 'traverse) #f (keep-S-closed))]
        [st:unit-separate-S #t]
        [st:unit-read-za read-za]
        [st:unit-write-za #t]
        [st:unit-separate-S #t]
        [record-analyzed-file-hook
          (lambda (filename . _)
            (printf "Record-analyzed-file ~s~n" filename)
            (set! files (cons filename files)))])
      (clear-counters!)
      (st:analyze file)
      (printf "FILE: ~s SIMPLIFICATION ~s ~s~n" file alg read-za)
      (show-counters)
      (pretty-print (st:control))
      (show-stat-small)
      (printf "ZA FILES:~n")
      (system
        (foldr
          (lambda (file acc)
            (string-append acc " " (regexp-replace "-source$" file "")))
          "wc "
          files)))))

(define (test-separate-alg alg file module-file)
  (assert (file-exists? module-file))
  (exec-mzscheme 
    `( (define __NO_DEBUGGING 1)
       (ST)
       ;(set! mrspidey:zprogress (lambda x 1))
       (test-separate-analyze (quote ,alg) (= 0 1) ,file)
       (exit)))
  (system (format "touch ~a" module-file))
  (exec-mzscheme 
    `( (define __NO_DEBUGGING 1)
       (ST)
       ;(set! mrspidey:zprogress (lambda x 1))
       (test-separate-analyze (quote ,alg) (= 1 1) ,file)
       (exit))))

(define (test-separate-file file module-file)
  (for-each
    (lambda (t)
      (unless (bad-alg-for-file? t file)
        (test-separate-alg t file module-file)))
    '(none nonempty-copy live live-few-e dfa-min-lub)))

(define (test-separate-all)
  '(test-separate-file 
     "~/Spidey/Unit/mod/test-separate.ss"
     "/home/cormac/Spidey/Unit/mod/test-separate-1.ss")    
  (test-separate-file "analyze-scanner.ss" "zodiac/scanner-parameters.ss")
  (test-separate-file "analyze-zodiac.ss"  "zodiac/corelate.ss")
  (test-separate-file 
    "~/papers/compress/bench/separate/nucleic/nucleic.ss"
    "/home/cormac/papers/compress/bench/separate/nucleic/search.ss")
  (test-separate-file "analyze-small.ss"   "hash.ss")
  (test-separate-file "analyze-no-min.ss"  "hash.ss")
  (test-separate-file "analyze.ss"         "hash.ss"))

;; ======================================================================
       
