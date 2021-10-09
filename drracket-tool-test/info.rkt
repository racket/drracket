#lang info

(define collection 'multi)

(define deps '("base"
               "rackunit-lib"
               "drracket-tool-lib"))
(define build-deps '())

(define pkg-desc "Tests for IDE tools that DrRacket supports")

(define pkg-authors '(robby))

(define version "1.0")

(define license
  '(Apache-2.0 OR MIT))
