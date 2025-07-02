#lang info

(define collection 'multi)

(define pkg-desc "The core of the DrRacket programming environment")

(define deps '("base"
               "drracket-core-lib"))

(define build-deps '("at-exp-lib"
                     "draw-doc"
                     "drracket-plugin-lib"
                     "drracket-tool-text-lib"
                     "errortrace-doc"
                     "errortrace-lib"
                     "gui-doc"
                     "gui-lib"
                     "net-doc"
                     "pconvert-doc"
                     "pconvert-lib"
                     "pict-doc"
                     "pict-lib"
                     "planet-doc"
                     "profile-doc"
                     "racket-doc"
                     "scheme-lib"
                     "scribble-lib"
                     "snip-lib"
                     "string-constants-lib"
                     "string-constants-doc"
                     "syntax-color-doc"
                     "syntax-color-lib"
                     "tex-table"))

(define implies '("drracket-core-lib"))

(define pkg-authors '(robby))

(define version "1.16")

(define license
  '(Apache-2.0 OR MIT))
