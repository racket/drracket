;; sba-unit.ss
;; Defines all the units in SBA
;; ----------------------------------------------------------------------
;; elaboration-time context:
;; sparams.ss Spidey/Code/macros.ss 

(load-relative "sigs.ss")

;; ----------------------------------------------------------------------
;; Library

(define mrspidey:library@ 
  (reference (build-path "lib" "main.ss")))

;; ----------------------------------------------------------------------

(define mrspidey:config@
  (unit/sig mrspidey:config^
    (import mrspidey:library^ mzlib:unprefixed-core^ [wx : wx^])
    (include "config.ss")))

(define mrspidey:debugging@
  (unit/sig mrspidey:debugging^
    (import
      mrspidey:library^
      mzlib:unprefixed-core^)
    (include "debug.ss")))

(define mrspidey:CDL@
  (compound-unit/sig
    (import 
      (I : mrspidey:interaction^)
      (MZLIB : mzlib:unprefixed-core^)
      (WX : wx^))
    (link
      [L :
        mrspidey:library^
        (mrspidey:library@ I MZLIB)]
      [C :
        mrspidey:config^
        (mrspidey:config@ L MZLIB WX)]
      [D : 
        mrspidey:debugging^
        (mrspidey:debugging@ L MZLIB)])
    (export (open C) (open D) (open L))))

;; ---------------------------------------------------------------------
;; Front End Stuff

(define mrspidey:loadexpand@
  (unit/sig mrspidey:loadexpand^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:languages^
      (zodiac : mrspidey:zodiac^)
      mzlib:unprefixed-core^)
    (include "ldexpand.ss")))

;; ======================================================================
;; Kernel stuff

(define mrspidey:hash@
  (unit/sig mrspidey:hash^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mzlib:unprefixed-core^)
    (include "hash.ss")))

(define mrspidey:kernel@
  (unit/sig mrspidey:kernel^
    (import
      mrspidey:CDL^
      mrspidey:hash^
      mrspidey:kernel-aux^
      mrspidey:templates^)
    (include "kernel.ss")))

;; ----------------------------------------------------------------------
;; Minimization

(define mrspidey:min-live@
  (unit/sig
    mrspidey:min-live^
    (import
      mrspidey:CDL^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:min^
      mrspidey:templates^)
    (include (begin-elaboration-time (build-path "min" "min-live.ss")))))

(define mrspidey:find-nonempty-tvars@
  (unit/sig
    mrspidey:find-nonempty-tvars^
    (import
      mrspidey:CDL^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:min-live^
      mrspidey:templates^)
    (include (begin-elaboration-time (build-path "min" "nonempty.ss")))))

(define mrspidey:min-live-few-e@
  (unit/sig
    mrspidey:min-live-few-e^
    (import
      mrspidey:CDL^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:min^
      mrspidey:min-live^)
    (include (begin-elaboration-time (build-path "min" "livefewe.ss")))))

(define mrspidey:hopcroft@
  (unit/sig
    mrspidey:hopcroft^
    (import
      mrspidey:CDL^
      mzlib:unprefixed-core^
      mrspidey:min^)
    (include (begin-elaboration-time (build-path "min" "hopcroft.ss")))))

'(define-signature mrspidey:min-dfa^ (minimize-constraints-dfa-min))
'(mrspidey:load-unit "min/" min-dfa-old ( mrspidey:CDL^
                                          mrspidey:kernel^
                                          mrspidey:kernel-aux^
                                          mrspidey:min^
                                          mrspidey:find-nonempty-tvars^
                                          mrspidey:min-live^
                                          mrspidey:hopcroft^))

'(define-signature mrspidey:min-dfa-inv^ (minimize-constraints-dfa-min-inv))
'(mrspidey:load-unit "min/" min-dfa-inv ( mrspidey:CDL^
                                         mrspidey:kernel^
                                         mrspidey:kernel-aux^
                                         mrspidey:min^
                                         mrspidey:min-live^
                                         mrspidey:hopcroft^))

'(define-signature mrspidey:min-dfa-strange ( minimize-constraints-dfa-min-1 
                                             minimize-constraints-dfa-min-2))
'(mrspidey:load-unit "min/" min-dfa-strange ( mrspidey:CDL^
                                             mrspidey:kernel^
                                             mrspidey:kernel-aux^
                                             mrspidey:min-live^
                                             mrspidey:hopcroft^))

(define mrspidey:min-dfa-fast@
  (unit/sig
    mrspidey:min-dfa-fast^
    (import
      mrspidey:CDL^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:min^
      mrspidey:min-live^
      mrspidey:min-live-few-e^
      mrspidey:hopcroft^)
    (include (begin-elaboration-time (build-path "min" "min-dfa.ss")))))

(define mrspidey:min@
  (unit/sig
    mrspidey:min^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:find-nonempty-tvars^
      mrspidey:min-live^
      mrspidey:min-live-few-e^
                                        ;mrspidey:min-dfa^
                                        ;mrspidey:min-dfa-inv^
                                        ;mrspidey:min-dfa-strange^
      mrspidey:min-dfa-fast^
      )
    (include (begin-elaboration-time (build-path "min" "min.ss")))
    (define minimize-constraints-dfa-min 1)
    (define minimize-constraints-dfa-min-inv 1)
    (define minimize-constraints-dfa-min-1 1)
    (define minimize-constraints-dfa-min-2 1)
    (define min-table 1)
    ))

(define mrspidey:all-min@
  (compound-unit/sig
    (import
      (CDL : mrspidey:CDL^)
      (INTERACTION : mrspidey:interaction^)
      (KERNEL : mrspidey:kernel^)
      (KERNEL-AUX : mrspidey:kernel-aux^)
      (TEMPLATES : mrspidey:templates^)
      (MZLIB : mzlib:unprefixed-core^)
      )
    (link
      [MIN : mrspidey:min^ 
        (mrspidey:min@ CDL INTERACTION KERNEL 
          FIND LIVE FEW FAST)]
      [FIND : mrspidey:find-nonempty-tvars^
        (mrspidey:find-nonempty-tvars@ CDL KERNEL KERNEL-AUX LIVE TEMPLATES)]
      [LIVE : mrspidey:min-live^
        (mrspidey:min-live@ CDL KERNEL KERNEL-AUX MIN TEMPLATES)]
      [FEW : mrspidey:min-live-few-e^
        (mrspidey:min-live-few-e@ CDL KERNEL KERNEL-AUX MIN LIVE)]
      [HOPCROFT : mrspidey:hopcroft^
        (mrspidey:hopcroft@ CDL MZLIB MIN)]
      ;;[DFA : mrspidey:min-dfa^
      ;;  (mrspidey:min-dfa@ CDL KERNEL KERNEL-AUX LIVE HOPCROFT)]
      ;;[INV : mrspidey:min-dfa-inv^
      ;;  (mrspidey:min-dfa-inv@ CDL KERNEL KERNEL-AUX LIVE HOPCROFT)]
      ;;[STRANGE : mrspidey:min-dfa-strange^
      ;;  (mrspidey:min-dfa-strange^@ CDL KERNEL KERNEL-AUX LIVE HOPCROFT)]
      [FAST : mrspidey:min-dfa-fast^
        (mrspidey:min-dfa-fast@ CDL KERNEL KERNEL-AUX MIN LIVE FEW HOPCROFT)])
    (export (open MIN))))

;; ----------------------------------------------------------------------
;; top level stuff

(define mrspidey:templates@
  (unit/sig
    mrspidey:templates^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^)
    (include "template.ss")))

(define mrspidey:kernel-aux@
  (unit/sig
    mrspidey:kernel-aux^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:templates^
      (zodiac : mrspidey:zodiac^))
    (include "kern-aux.ss")))

(define mrspidey:typelang@
  (unit/sig
    mrspidey:typelang^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:templates^
      mrspidey:type-env^
      mrspidey:atype^
      mrspidey:contained^
      mzlib:unprefixed-core^
      (zodiac : mrspidey:zodiac^))
    (include "typelang.ss")))

(define mrspidey:contained@
  (unit/sig
    mrspidey:contained^
    (import
      mrspidey:CDL^
      mrspidey:kernel^
      mrspidey:sdl^)
    (include "contain.ss")))

(define mrspidey:type-env@
  (unit/sig
    mrspidey:type-env^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:templates^
      mrspidey:min^
      mrspidey:atype^
      (zodiac : mrspidey:zodiac^))
    (include "type-env.ss")))

(define mrspidey:sdl@
  (unit/sig
    mrspidey:sdl^
    (import
      mrspidey:CDL^
      mrspidey:kernel^
      mrspidey:min^
      mrspidey:typelang^
      mrspidey:kernel-aux^
      mrspidey:templates^
      mrspidey:atype^
      mzlib:unprefixed-core^)
    (include "sdl.ss")))

(define mrspidey:languages@
  (unit/sig
    mrspidey:languages^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:templates^
      mrspidey:kernel-aux^
      mrspidey:atype^
      mrspidey:type-env^
      mrspidey:typelang^
      mrspidey:atenv^
      mrspidey:loadexpand^
      mrspidey:traverse^
      mzlib:unprefixed-core^
      (zodiac : mrspidey:zodiac^))
    (include "language.ss")))

(define mrspidey:za@
  (unit/sig
    mrspidey:za^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:templates^
      mrspidey:type-env^
      mrspidey:typelang^
      mrspidey:atype^
      mzlib:unprefixed-core^)
    (include "za.ss")))

;; ----------------------------------------------------------------------
;; Traversal

(define mrspidey:atype@
  (unit/sig
    mrspidey:atype^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:min^
      mrspidey:typelang^
      mrspidey:templates^
      mrspidey:type-env^
      mrspidey:sdl^
      mrspidey:atlunit^
      mzlib:unprefixed-core^)
    (include "atype.ss")))

(define mrspidey:atenv@
  (unit/sig
    mrspidey:atenv^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:atype^
      (zodiac : mrspidey:zodiac^)
      mzlib:unprefixed-core^)
    (include "atenv.ss")))

(define mrspidey:atlunit@
  (unit/sig
    mrspidey:atlunit^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:min^
      mrspidey:loadexpand^
      mrspidey:type-env^
      mrspidey:templates^
      mrspidey:languages^
      mrspidey:atype^
      mrspidey:atenv^
      mrspidey:traverse^
      mrspidey:za^
      (zodiac : mrspidey:zodiac^)
      mzlib:unprefixed-core^
      [wx : wx^])
    (include "atlunit.ss")))

(define mrspidey:traverse@
  (unit/sig
    mrspidey:traverse^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:min^
      mrspidey:loadexpand^
      mrspidey:templates^
      mrspidey:kernel-aux^
      mrspidey:typelang^
      mrspidey:type-env^
      mrspidey:languages^
      mrspidey:atype^
      mrspidey:atlunit^
      mrspidey:atenv^
      (zodiac : mrspidey:zodiac^)
      mzlib:unprefixed-core^)
    (include "traverse.ss")))

;; ----------------------------------------------------------------------

(define mrspidey:program@
  (unit/sig
    mrspidey:program^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:type-env^
      mrspidey:loadexpand^
      mrspidey:traverse^
      mrspidey:templates^
      mrspidey:atenv^
      mrspidey:atype^
      mrspidey:languages^
      (zodiac : mrspidey:zodiac^)
      mzlib:unprefixed-core^)
    (include "program.ss")))

(define mrspidey:calc-checks@
  (unit/sig
    mrspidey:calc-checks^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:loadexpand^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:typelang^
      mrspidey:templates^
      mrspidey:atype^
      mrspidey:sdl^
      (zodiac : mrspidey:zodiac^)
      mzlib:unprefixed-core^)
    (include "checks.ss")))

(define mrspidey:driver@
  (unit/sig
    mrspidey:driver^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:kernel^
      mrspidey:templates^
      mrspidey:typelang^
      mrspidey:type-env^
      mrspidey:atype^
      mrspidey:atenv^
      mrspidey:program^
      mrspidey:calc-checks^
      mrspidey:languages^
      (zodiac : mrspidey:zodiac^))
    (include "driver.ss")))

(define mrspidey:hyper@
  (unit/sig
    mrspidey:hyper^
    (import
      mrspidey:CDL^
      mrspidey:interaction^
      mrspidey:loadexpand^
      mrspidey:program^
      mrspidey:calc-checks^
      mrspidey:templates^
      mrspidey:kernel^
      mrspidey:kernel-aux^
      mrspidey:atype^
      mzlib:unprefixed-core^)
    (include "hyper.ss")))

(load-relative "zod-extra.ss")

;; ----------------------------------------------------------------------
;; Put it all together

(load-relative "link.ss")

;; ----------------------------------------------------------------------







