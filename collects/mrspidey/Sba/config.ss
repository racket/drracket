;; config.ss
;; Lots of parameters for analysis
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

(define st:restricted 
  (not (string? (current-load-relative-directory))))
(define st:name    "MrSpidey")
(define st:version (lambda () "49s1"))

;; ----------------------------------------------------------------------
;; Front end parameters

(define st:fake-reader      (make-parameter-boolean #f))
(define st:system-expand    (make-parameter-boolean #f))

;; ----------------------------------------------------------------------
;; Analysis parameters

(define st:constants        (make-parameter-boolean #f))
(define st:const-merge-size (make-parameter-integer 7))

(define st:if-split       (make-parameter-boolean #t))
; If #t, then knowledgeable about if's

(define st:flow-sensitive (make-parameter-boolean #t))
; If #t, then in (begin (car x) (cdr x)), cdr is never checked

(define st:fo-units         (make-parameter-boolean #t))
;; If #t, then first-order units treated specially

(define st:lazy-fo-units         (make-parameter-boolean #t))
;; If #t, then first-order units treated specially

(define st:cache-units      (make-parameter-boolean #t))
;; If #t, then first-order units treated specially

(define st:whole-program    (make-parameter-boolean #t))

(define st:special-fo-prims (make-parameter-boolean #t))
(define st:see-void         (make-parameter-boolean #t))
(define st:cons-mutable     (make-parameter-boolean #t))
(define st:use-fo-ftype     (make-parameter-boolean #t))

(define need-label-types #t)
(define need-explanation #t)

(define st:zero-old-constraint-sets (make-parameter-boolean #t))
(define st:zero-old-asts            (make-parameter-boolean #t))

;; ----------------------------------------------------------------------
;; Simplification parameters

(define constraint-simplification-list
  (if st:restricted
    '( 
       (none            "No simplification")
       (nonempty        "Remove empty constraints")
       (live            "Remove empty and unreachable constraints")
       (live-few-e      "As above, and also remove epsilon constraints")
       ((live-few-e dfa-min-glb dfa-min-lub) 
         "Live, few epsilon, DFA min")
       )
    '( 
       (none            "No simplification")
       (nonempty        "Remove empty constraints")
       (nonempty-copy   "Copy nonempty")
       (live            "Remove empty and unreachable constraints")
       (live-few-e-L    "Live, few lower epsilon")
       (live-few-e-U    "Live, few upper epsilon")
       (live-few-e      "Live, few epsilon")
       ((live-few-e dfa-min-lub)
         "Live, few epsilon, DFA min LUB")
       ((live-few-e dfa-min-glb)
         "Live, few epsilon, DFA min GLB")
       ((live-few-e  dfa-min-lub dfa-min-glb) 
         "Live, few epsilon, DFA min")
       )))

(define make-constraint-simplification-para
  (case-lambda
    [() (make-constraint-simplification-para 'live-few-e)]
    [(default)
      (make-parameter-list
        default
        constraint-simplification-list
        (lambda (_) (void))
        ;; also-ok?
        (lambda (x)
          (and (list? x)
            (andmap 
              (lambda (y) (member y (map car constraint-simplification-list)))
              x))))]))

(define st:minimize-respect-assignable-part-of-fields 
  (make-parameter-boolean #t))

;; ----------------------------------------------------------------------
;; Polymorphism parameters

(define st:constraint-simplification-poly (make-constraint-simplification-para))

(define st:polymorphism
  (make-parameter-list 'compress
                       `((none      "No polymorphism")
                         (compress  "Simplify constraints")
                         ;;(copy-con  "") 
                         (reanalyze "Reanalyze")
                         )))

;; ----------------------------------------------------------------------
;; Unit parameters 

(define st:unit-read-za            (make-parameter-boolean #t))
(define st:unit-write-za           (make-parameter-boolean #t))
(define st:unit-simplify           
  (make-constraint-simplification-para 'live-few-e))
(define st:unit-separate-S         (make-parameter-boolean #t))
(define st:save-za-in
  (make-parameter-list
    'source-directory
    `( (source-directory "Source file directory" "")
       (tmp-directory    
         ,(string-append 
            (if (defined? 'wx:find-path)
              (wx:find-path 'temp-dir) " directory"))
         ""))))

;; ----------------------------------------------------------------------
;; Type Viewing parameters 

;; --- copying

(define st:sdl-fo
  (make-parameter-list
    'basic-types
    '( (basic-types  "Basic Types"     "ie (1 -> 1)")
       (type-schemas   "Type Schemas"  "ie (X1 -> X1)"))))

(define st:sdl-fo-ivars (make-parameter-boolean #t))
(define st:sdl-fo-struct-fields (make-parameter-boolean #t))
(define st:sdl-fo-depth-limit? (make-parameter-boolean #t))
(define st:sdl-fo-depth-limit (make-parameter-integer 50))
(define st:sdl-fo-size-limit? (make-parameter-boolean #t))
(define st:sdl-fo-size-limit (make-parameter-integer 50))

;; --- simplification

(define st:sdl-constraint-simplification 
  (make-constraint-simplification-para 'live-few-e))

(define st:show-assignable-part-of-fields 
  (make-parameter-boolean #f))

;; --- to SDL

(define st:listify-etc (make-parameter-boolean #t))

(define st:sdl-constructor/selector
  (make-parameter-list 
    'constructor
    '((constructor "Show types as constructors" "")
      (selector    "Show types as selectors"    ""))))

(define st:naming-strategy 
  (make-parameter-list
   'multiple
   (if st:restricted
       '((recursive "Recursive" "Name types on cycles")
         (multiple  "Multiple"  "Name types referenced more than once")
         (nontrivial  "Non-Trivial"  "Name non-trivial types")
         (all       "All"       "Name all types"))
       '((recursive "Recursive" "Name types on cycles")
         (multiple  "Multiple"  "Name types referenced more than once")
         (nontrivial  "Non-Trivial"  "Name non-trivial types")
         (all       "All"       "Name all types")))))

(define st:primitive-types 
  (make-parameter-list
   'inferred
   '((prim      "(prim ...)"     "ie (prim car)")
     (given     "Given types"    "ie ((cons a b) -> a)")
     (inferred  "Inferred types" "ie ((cons 'a-symbol 4) -> 'a-symbol)"))))

;; --- simplifying SDL

(define st:expand-output-type (make-parameter-boolean #t))
(define st:sdl-tidy (make-parameter-boolean #t))

;; ---

(define st:pretty-type-width (make-parameter-integer 60))

;; ----------------------------------------------------------------------

(define st:check-kernel (make-parameter-boolean #f))
(define st:compare-min-algs (make-parameter-boolean #f))

;; ----------------------------------------------------------------------
;; Checking parameters

(define st:all-checks     (make-parameter-boolean #f))

;; ----------------------------------------------------------------------
;; control - interface to all parameters

(define mrspidey:control-fn
  (let ([paras 
          (begin-elaboration-time
            (cons 'list
              (map 
                (lambda (x) (list 'cons (list 'quote x) x))
                '(;; --- Analysis time
                   st:constants 
                   st:const-merge-size
                   st:fo-units
                   st:if-split 
                   st:flow-sensitive 
                   st:special-fo-prims
                   st:system-expand
                   st:see-void
                   st:unit-read-za
                   st:unit-write-za
                   st:save-za-in
                   ;; --- Polymorphism
                   st:polymorphism
                   st:constraint-simplification-poly
                   ;;st:library-prims
                   ;;st:topo-sort
                   ;; --- Seperate analysis
                   st:unit-simplify
                   ;; --- Type viewing
                   st:sdl-fo
                   st:sdl-fo-ivars 
                   st:sdl-fo-struct-fields
                   st:sdl-fo-depth-limit? 
                   st:sdl-fo-depth-limit
                   st:sdl-fo-size-limit?
                   st:sdl-fo-size-limit
                   st:sdl-constraint-simplification
                   st:show-assignable-part-of-fields
                   st:listify-etc
                   st:sdl-constructor/selector
                   st:naming-strategy
                   st:primitive-types
                   st:expand-output-type
                   st:sdl-tidy
                   st:pretty-type-width

                   ;; --- checking parameters
                   st:all-checks 
                   ))))])
    (match-lambda*
     [()
      ;; Return list of all settings
       (map
         (match-lambda [(sym . para) (list sym (para))])
         paras)
       ]
     [(para-name)
      ;; Return one setting
       (match (assq para-name paras)
         [(_ . para) (para)]
         [#f (error 'mrspidey:control "Unknown parameter ~s" para-name)])]
     [(para-name nu-val)
       ;; Return one setting
       (match (assq para-name paras)
         [(_ . para)
           (if (memq nu-val (map car (para '?)))
             (para nu-val)
             (error 'mrspidey:control "Value ~s invalid for parameter ~s" 
               nu-val para-name))]
         [#f
           (error 'mrspidey:control "Unknown parameter ~s" para-name)])]
     [_ (error 'mrspidey:control "Bad # arguments")])))
     
;======================================================================
 
(when st:restricted
  ;; . special configuration, if necy
  (void)
  )

 














