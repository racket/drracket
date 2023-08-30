#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               "data-lib"
               "compiler-lib"
               ["base" #:version "8.2.0.8"]
               "planet-lib"
               "compatibility-lib"
               ["draw-lib" #:version "1.7"]
               ["errortrace-lib" #:version "1.5"]
               "macro-debugger-text-lib"
               "parser-tools-lib"
               "pconvert-lib"
               "pict-lib"
               "profile-lib"
               "sandbox-lib"
               ("scribble-lib" #:version "1.11")
               ("snip-lib" #:version "1.2")
               ["string-constants-lib" #:version "1.41"]
               "typed-racket-lib"
               "wxme-lib"
               ["gui-lib" #:version "1.68"]
               ("racket-index" #:version "1.2")
               ["racket-doc" #:version "1.1"]
               "html-lib"
               "images-lib"
               ["icons" #:version "1.2"]
               ["typed-racket-more" #:version "1.12"]
               "trace"
               ["macro-debugger" #:version "1.1"]
               "net-lib"
               "tex-table"
               ["htdp-lib" #:version "1.7"]
               ("drracket-plugin-lib" #:version "1.1")
               "gui-pkg-manager-lib"
               "drracket-tool-lib"
               "drracket-tool-doc"
               "pict-snip-lib"
               "option-contract-lib"
               ("syntax-color-lib" #:version "1.4")
               "quickscript"
               "simple-tree-text-markup-lib"))

(define build-deps '("mzscheme-doc"
                     "net-doc"
                     "planet-doc"
                     "compatibility-doc"
                     "string-constants-doc"
                     "draw-doc"
                     "errortrace-doc"
                     "gui-doc"
                     "pict-doc"
                     "profile-doc"
                     "r5rs-doc"
                     "at-exp-lib"
                     "rackunit-lib"
                     "scheme-doc"))

;; implies drracket-tool-lib so that others dependencies don't break
;; (redex, in particular, used to depend on drracket but it really
;; needs only the parts in drracket-tool-lib)
(define implies '("drracket-plugin-lib" "drracket-tool-lib"))

(define pkg-desc "The DrRacket programming environment")

(define pkg-authors '(robby))

(define version "1.15")

(define license
  '(Apache-2.0 OR MIT))
