#lang info

(define collection 'multi)

(define deps '(["base" #:version "8.2.0.8"]
               "compatibility-lib"
               ["draw-lib" #:version "1.7"]
               "drracket-core"
               "drracket-core-lib"
               ["drracket-plugin-lib" #:version "1.1"]
               "drracket-tool-lib"
               "drracket-tool-doc"
               ["gui-lib" #:version "1.74"]
               "html-lib"
               ["icons" #:version "1.2"]
               "images-lib"
               ["macro-debugger" #:version "1.1"]
               "macro-debugger-text-lib"
               "net-lib"
               "pict-lib"
               "planet-lib"
               "quickscript"
               ["racket-doc" #:version "1.1"]
               "scheme-lib"
               ["snip-lib" #:version "1.2"]
               ["string-constants-lib" #:version "1.51"]
               "tex-table"))

(define build-deps '("gui-doc"
                     "net-doc"
                     "racket-index"
                     "scribble-lib"))

;; implies drracket-tool-lib so that others dependencies don't break
;; (redex, in particular, used to depend on drracket but it really
;; needs only the parts in drracket-tool-lib)
(define implies '("drracket-plugin-lib" "drracket-tool-lib" "drracket-core" "drracket-core-lib"))

(define pkg-desc "The DrRacket programming environment")

(define pkg-authors '(robby))

(define version "1.16")

(define license
  '(Apache-2.0 OR MIT))
