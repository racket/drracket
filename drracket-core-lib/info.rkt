#lang info

(define collection 'multi)

(define pkg-desc "implementation (no documentation) part of \"drracket-core\"")

(define deps '(["base" #:version "8.2.0.8"]
               "data-lib"
               "compiler-lib"
               ["draw-lib" #:version "1.7"]
               ["drracket-plugin-lib" #:version "1.1"]
               "drracket-tool-lib"
               "drracket-tool-text-lib"
               ["errortrace-lib" #:version "1.5"]
               ["gui-lib" #:version "1.76"]
               "gui-pkg-manager-lib"
               ["icons" #:version "1.2"]
               "images-lib"
               "option-contract-lib"
               "pconvert-lib"
               "profile-lib"
               "parser-tools-lib"
               "pict-lib"
               "pict-snip-lib"
               ["snip-lib" #:version "1.2"]
               "trace-lib"
               "net-lib"
               "option-contract-lib"
               "racket-lib"
               ["racket-index" #:version "1.2"]
               "sandbox-lib"
               ["scribble-lib" #:version "1.11"]
               ["string-constants-lib" #:version "1.48"]
               ["syntax-color-lib" #:version "1.4"]
               "simple-tree-text-markup-lib"
               "typed-racket-lib"
               ["typed-racket-more" #:version "1.12"]
               "wxme-lib"))

(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-authors '(robby))

(define version "1.0")

(define license
  '(Apache-2.0 OR MIT))
