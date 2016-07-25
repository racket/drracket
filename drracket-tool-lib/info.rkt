#lang info

(define collection 'multi)

(define deps '(["base" #:version "6.2.900.10"]
               "scribble-lib"
               ["string-constants-lib" #:version "1.12"]
               "scribble-lib"
               "racket-index"
               "gui-lib"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "Code implementing programmatic interfaces to some IDE tools that DrRacket supports")

(define pkg-authors '(robby))

(define version "1.1")
