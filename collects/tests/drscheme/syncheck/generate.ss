#!/bin/sh

string=? ; exec /home/mflatt/plt/bin/mzscheme -qr $0

;; run this to output lots of scheme code to stdout
;; that code should all pass thru the syntax checker

(require-library "pretty.ss")

(let ([orig-eval (current-eval)]
      [orig-output (current-output-port)]
      [dir (current-directory)])
  (parameterize ([current-eval
		  (lambda (x)
		    (begin0 (orig-eval x)
			    (pretty-print x orig-output)))]
		 [current-output-port (make-output-port void void)]
		 [current-error-port (make-output-port void void)]
		 [error-display-handler void])
    (current-directory "/home/mflatt/proj/mred/mzscheme/tests")
    (load "testing.ss")

    (load "basic.ss")
    (load "read.ss")
    (load "syntax.ss")
    (load "file.ss")
    (load "path.ss")
    (load "number.ss")
    (load "object.ss")
    (load "struct.ss")
    (load "unit.ss")
    (load "thread.ss")
    (load "param.ss")
    (current-directory dir)))
