#lang info

(define collection 'multi)

(define deps '(["base" #:version "6.2.900.10"]
               "drracket-tool-text-lib"
               "scribble-lib"
               ["string-constants-lib" #:version "1.43"]
               "scribble-lib"
               "racket-index"
               "gui-lib"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define implies '("drracket-tool-text-lib"))

(define pkg-desc "GUI code implementing programmatic interfaces to some IDE tools that DrRacket supports")

(define pkg-authors '(robby))

(define version "1.1")

(define license
  '(Apache-2.0 OR MIT))
