;; sba-sigs.ss
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

(define-signature mzlib:unprefixed-core^
  ( (open mzlib:pretty-print^)
    (open mzlib:file^)
    (open mzlib:function^)
    (open mzlib:compat^) 
    (open mzlib:string^)))

(define-signature mrspidey:library-paras^ 
  (make-parameter-boolean
   make-parameter-integer
   make-parameter-list))

(define-signature mrspidey:library-list^
  (mapLR
   mapRL
   foldl-with-n
   foldr2
   filter
   filter-map
   filter-map-split
   rac
   rdc
   map-with-n
   for-each-with-n
   map-ilist
   length-ilist
   improper?
   flatten-ilist
   map2
   for-each2
   andmap2
   andmap2len
   ormap2
   list-n-copies
   count
   get-prefix
   mklist
   nth
   list-pos
   list-pos-equal
   find
   index
))

(define-signature mrspidey:library-vec^
  ( vector-copy
    vector-map1
    vector-map2
    vector-map
    vector-for-each-with-n
    vector-for-each
    vector-andmap1
    vector-andmap2
    vector-andmap
    vector-ormap1
    vector-ormap2
    vector-ormap
    vector-zero!))

(define-signature mrspidey:library-set^
  (empty-set
   empty-set? 
   set
   list->set
   list->set-equal?
   element-of?
   cardinality
   set<=
   set-eq?
   union2
   union
   setdiff2
   setdiff
   intersect2
   intersect))

(define-signature mrspidey:library-misc^
  (symbol-append
    get-cpu-time
    get&return-cpu-time
    make-timer
    clear-timer!
    record-time
    strip-string
    padl
    padr
    chop-number
    substring?
    char-find
    file-newer
    eqc?
    ))

(define-signature
  mrspidey:env^
  ( empty-env lookup-or-fail lookup-or-#f lookup bound-in-env?
    extend-env extend-env* join-env bang-env!
    env:change-binding env:remove
    ))

(define-signature mrspidey:library^
  ((open mrspidey:library-paras^)
   (open mrspidey:library-list^)
   (open mrspidey:library-vec^)
   (open mrspidey:library-set^)
   (open mrspidey:library-misc^)
   (open mrspidey:env^)))

;; ----------------------------------------------------------------------

(define-signature mrspidey:interaction^
  (mrspidey:error 
   mrspidey:warning 
   mrspidey:internal-error
   mrspidey:add-summary
   mrspidey:add-summary-handler
   mrspidey:progress
   mrspidey:progress-handler
   record-analyzed-file
   record-analyzed-file-hook
    
))

(define-signature mrspidey:config^
  ( st:restricted
    st:name
    st:version

    st:fake-reader
    st:system-expand

    st:constants
    st:const-merge-size
    st:if-split
    st:flow-sensitive
    st:fo-units
    st:lazy-fo-units
    st:cache-units
    st:whole-program
    st:special-fo-prims
    st:see-void
    st:cons-mutable
    st:use-fo-ftype
    need-label-types
    need-explanation
    st:zero-old-constraint-sets
    st:zero-old-asts

    st:minimize-respect-assignable-part-of-fields

    st:constraint-simplification-poly
    st:polymorphism

    st:unit-read-za
    st:unit-write-za
    st:unit-simplify
    st:unit-separate-S
    st:save-za-in

    ;; --- Type Viewing Parameters
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

    st:check-kernel
    st:compare-min-algs

    ;; --- Checking Parameters
    st:all-checks
 
    ;; st:show-filters
    ;; st:add-hyper-links
    ;; st:dont-combine-definitions
    ;; st:structure-opaque
    ;; st:library-prims
    ;; st:topo-sort

    mrspidey:control-fn

    ))

(define-signature mrspidey:debugging^
  (debugging
    debugging-front
    debugging-traverse
    debugging-unit
    debugging-check
    debugging-atenv
    debugging-atype
    debugging-sdl
    debugging-sdl2
    debugging-gui

    debugging-min
    debugging-min2
    debugging-few
    debugging-gram
    debugging-dfa-min
    debugging-min-table
    debugging-object

    timing-min

    pretty-print-debug
    set-debug-flag
    ))

(define-signature mrspidey:CDL^
  ((open mrspidey:config^)
   (open mrspidey:debugging^)
   (open mrspidey:library^)))

;; ----------------------------------------------------------------------
;; Kernel analysis stuff

(define-signature mrspidey:hash^ 
  ( init-hash-table 
    hash-fn hash-fn* 
    add-entry hash-find hash-table-info
    capture-hash-table-state restore-hash-table-state!
    prompt-hash-table-state unprompt-hash-table-state!
    free-hash-table-state!
    ))

(define-signature
  mrspidey:kernel^
  (;; --- Structures
    (struct FlowType
      (num expr arrowto arrowfrom type-annotation proplist values-ftype))
    num-ftype list-ftype num-edge 
    add-FlowType-arrow! add-FlowType! 
    add-FlowType-prop! get-FlowType-prop FlowType-name

    (struct Tvar (objs orig-objs constraints edgeto edgefrom L U))

    (struct AV (num template misc fields+ fields- U PU))
    (struct template
      (type num+ num- ref assign super-templates misc-eq?))
    (struct con (num template field-no tvar misc sign))
    (struct con-filter (num filter tvar))
    (struct filter (sign templates))
    create-filter
    mt-vector

    ;; --- Values
    num-con    
    num-AV   
    num-AV-in-Tvar

    ;; --- Functions
    init-kernel!
    create-AV
    mk-Tvar
    create-con create-con-misc create-con-filter
    new-edge! new-bidir-edge! new-edge-para new-AV! new-create-AV! new-con!
    new-leq-top-s! new-geq-top-s!
    mk-Tvar-init-AV
    constraint-system-size
    get-Tvar-objs
    add-AV! add-con! add-edge!
    Tvar-name

    save-kernel-state restore-kernel-state!
    prompt-kernel-state unprompt-kernel-state!
    free-kernel-state!

    alloc-Tvar-field alloc-AV-field field->set
    check-kernel-ok
    really-check-kernel-ok
    check-unreachable
    ))

(define-signature
  mrspidey:gram^
  ( ;; --- grsyms
    (struct grsym (fn mono))
    (struct grsym-normal (template misc field-no))
    (struct grsym-misc ())
    grsym-eq?

    ;; --- rhs
    (struct rhs (grsym nt))
    add-rhs!

    calc-productions!
    ))

;; ----------------------------------------------------------------------
;; Front end stuff

(define-signature mrspidey:loadexpand^ 
  (open-code-file
   zodiac:read*
   mrspidey:zprogress
   strip-hash-percent
   system-expand-if-necy
    expand-zexp->port

   clear-file-time-cache!
   extend-file-time-cache!
   zodiac-time
   zodiac-time*

    init-expand!
    my-scheme-expand-program
))

;;(define-signature mrspidey:parser^ (top-level-parse-defs top-level-parse-exp))

;; ----------------------------------------------------------------------

(define-signature mrspidey:atenv^
  ( atenv:empty
    atenv:extend
    atenv:extend-mutated
    atenv:extend*
    atenv:extend-voids
    atenv:extend-undefineds
    atenv:lookup
    atenv:change-binding
    atenv:change-bindings
    atenv:capture-locs
    atenv:unflush
    atenv:flush!
    atenv->pretty
    atenv:domain
    atenv:ok?
    link-parsed-ftype!
    ))

(define-signature mrspidey:traverse^
  ( top-level-traverse-defs
    traverse-defs
    ;;traverse-def
    traverse-exp
    ))

(define-signature mrspidey:atype^
  ((struct fo-FlowType (def))
   FlowType->Atype
   create-fo-FlowType
   fo-Atype? poly-atype?

   (struct atconst (c))
   (struct schema (tvar tvar* edges))
   (struct atprim (sym type domain-filters predicate-fn attrs orig-type))
   (struct atthunk (thunk))
   (struct atstruct (struct:sym super-constructors 
                                     parent-gen-args
                                     parent-match-args
                                     parent-field-types
                                     parent-list-mutable))
   (struct atvalues (values))
   wrap-value extract-1st-value multiple-value-components

   (struct atunit (imports exports result expr))
   (struct atlunit (ui))

   FlowType->Tvar
   Atype->Tvar
   FlowType->pretty
   FlowType->SDL
    copy-ftype
   ))

(define-signature mrspidey:atlunit^
  (create-atlunit-unit
   create-atlunit-cmpd
   create-atlunit-reference

   apply-unit
   atlunit->atunit
   apply-atlunit

   ))


(define-signature mrspidey:type-env^
  (global-tref-env
   global-tdef-env 
   global-tbang-env

   add-global-tref!
   add-global-tdef!
   add-global-tbang!
   init-global-tenv!

   connect-bangs-defs
   report-duplicate-defs
   connect-refs-defs
   report-unbound-vars
   ))

;; ----------------------------------------------------------------------
;; ???

(define-signature mrspidey:templates^
  ( constructor->template
    constructor-env
    set-constructor-env!
    extend-constructor-env!
    add-constructor!
    constructor-alias!
    record-super-constructor!
    record-super-constructor-of-template!
    record-super-template!

    lam-misc-eq?
    template-lam
    template-lam++
    filter-not-lam
    template-cons  
    template-nil   
    template-num   
    template-sym   
    template-str   
    template-char  
    template-void 
    template-undefined
    template-true  
    template-false 
    template-promise 
    template-unit        
    template-structure
    template-mvalues
    template-internal-class
    template-all-ivars
    template-dots
    ;;template-ivar-box
    template-top-s
    template-ivarset

    init-default-constructor-env!

    saved-constructor-env
    init-constructor-env!

    get-unit-import-template
    get-unit-export-template
    get-ivar-template
   
    type-constructor?
    lookup-template
    lookup-template-or-error
    is-template?
    ))

(define-signature mrspidey:kernel-aux^
  ( make-constructed-AV-template
    make-constructed-AV
    make-constructed-Tvar

    make-AV-cons
    make-con-car
    make-con-cdr
    make-con-dom
    make-con-rng
                                        ;make-con-arg-car
                                        ;make-con-arg-cdr
    make-AV-vec
    make-AV-lam

    AV-nil   
    AV-numb  
    AV-sym   
    AV-str   
    AV-char  
    AV-true  
    AV-false 
    AV-void  
    AV-undefined
    AV-top-s

    mk-tvar-nil   
    mk-tvar-numb  
    mk-tvar-sym   
    mk-tvar-str   
    mk-tvar-char  
    mk-tvar-void  
    mk-tvar-undefined
    mk-tvar-true  
    mk-tvar-false 
    mk-tvar-empty

    init-common-AV!
    traverse-simple-const
    traverse-const-exact

    Tvar-transitive-edgeto
    copy-constraint-set
    AV->rep

    ;; --- NTs
    (struct NT (tvar type rhs*))
    mk-Tvar-NTs! mk-AV-NTs! 
    alloc-NT-field
    nt->sym
    select-L
    select-U
    chk-Tvar-U
    chk-Tvar-L
    chk-AV-U
    ))

(define-signature mrspidey:sdl^
  (Tvar->SDL))

(define-signature mrspidey:languages^
  ( st:language 
    initialize-language!
    st:numops
    get-default-bindings
    make-expander-namespace
))

; ----------------------------------------------------------------------

(define-signature mrspidey:typelang^
  ( absUnion absunion absIntersect absintersect
    init-output-type-expander! install-output-type-expander!
    init-input-type-expander! 
    install-input-type-expander!
    expand-input-type expand-input-type-err expand-output-type
    typevar?

    type->templates
    split-schema
    schema->con
    tschema->con
    tschema->con-for-nargs
    ;;dom+rng-for-nargs
    Tvar-in-type?
    primitive->atprim))

(define-signature mrspidey:contained^
  (Tvar-containment?))
; ----------------------------------------------------------------------

(define-signature mrspidey:min^
  ( minimize-constraints-&-compare
    minimize-constraints
    min-record-progress))

(define-signature mrspidey:min-live^ 
  ( copy-live-constraints
    copy-live-constraints-noe
    calc-live-tvars-nts
    copy-constraints-equiv!
    follow-antimono-fields))

(define-signature mrspidey:find-nonempty-tvars^ 
  ( copy-nonempty-tvars
    find-nonempty-tvars))

(define-signature mrspidey:min-live-few-e^ 
  (copy-live-constraints-few-e))

(define-signature mrspidey:hopcroft^ 
  (Hopcroft-calc-equivalences))

(define-signature mrspidey:min-dfa-fast^
  ( minimize-constraints-dfa-min-lub 
    minimize-constraints-dfa-min-glb))

; ----------------------------------------------------------------------

(define-signature mrspidey:calc-checks^
  ( calc-checks
    calc-type-annotations
    (struct annotation (loc))
    (struct check-annotation (text num rest))
    (struct uncheck-annotation (text))
    (struct type-annotation (end-first finish FlowType))
    ))

(define-signature mrspidey:za^
  (read-za write-za))

; ----------------------------------------------------------------------

(define-signature mrspidey:program^
  (analyze-program global-def-env initialize-analysis!))

(define-signature mrspidey:driver^
  (st:analyze st: st:type-fn st:help st:set-debug))

;; ----------------------------------------------------------------------

(define-signature
  mrspidey:zodiac-aux^
  ( compat compat*
    ast-size const-size
    unparse-dynamic-letd
    stripper

    parsed-ftype set-parsed-ftype!
    parsed-check set-parsed-check!
    parsed-atprim set-parsed-atprim!
    app-tvar-args set-app-tvar-args!
    binding-refs set-binding-refs! 
    binding-mutated set-binding-mutated!
    varref-binding
    my-create-binding

    lambda-flatten-arglist

    no-location
    location-inc
    determine-end-first-token

    parsed-value?
    free-refs
    free-vars
    initialize-mutated
    free-vars-defs
    zero!
    inline-begins
    ))

(define-signature mrspidey:zodiac^
  ((open zodiac:system^) (open mrspidey:zodiac-aux^)))

; ----------------------------------------------------------------------

(define-signature mrspidey:hyper^
  ( analysis-set-arrow-filter!
    analysis-filter-on?
    analysis-get-filters
    analysis-shortest-path
    analysis-parents
    analysis-children
    analysis-ancestors
    analysis-descendants
    analysis-callback
    analysis-get-param
    analysis-set-param!
    calc-annotations
    st:analyze-and-make-annotations

    ))

(define-signature mrspidey:sba^
  ( (open mrspidey:driver^)
    (open mrspidey:CDL^)
    (open mrspidey:atype^)
    (open mrspidey:hyper^)
    (open mrspidey:kernel^)
    (open mrspidey:calc-checks^)
    (open mrspidey:languages^)
    (unit zodiac : mrspidey:zodiac^)
))

; ----------------------------------------------------------------------
