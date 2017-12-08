#lang info

(define collection 'multi)

(define deps '("base" "scribble-lib" "drracket-tool-lib"))
(define build-deps '("racket-doc" "gui-doc" "gui-lib" "drracket"))

(define pkg-desc "Docs for the programmatic interface to some IDE tools that DrRacket supports")

(define pkg-authors '(robby))

(define version "1.0")
