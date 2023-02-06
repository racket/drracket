#lang info

(define collection 'multi)

(define deps '(["base" #:version "6.2.900.10"]
               "scribble-lib"
               ["string-constants-lib" #:version "1.12"]
               "racket-index"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "Non-GUI code implementing programmatic interfaces to some IDE tools that DrRacket supports")

(define pkg-authors '(robby))

(define version "1.3")

(define license
  '(Apache-2.0 OR MIT))
