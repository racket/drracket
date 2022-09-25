#lang racket/base
(require racket/cmdline racket/runtime-path)

(define as-library? #f)
(define file
  (command-line
   #:once-any
   [("-l" "--lib") "treat <file> as a library name" (set! as-library? #t)]
   #:args
   (file) file))
(when as-library?
  (define name (read (open-input-string file)))
  (unless (module-path? name)
    (raise-user-error 'raco\ dependencies-graph "expected a valid argument to `require`, got ~a" file))
  (define mod-path-name
    (resolved-module-path-name
     ((current-module-name-resolver)
      (read (open-input-string file)) #f #f #f)))
  (define path
    (if (pair? mod-path-name) (car mod-path-name) mod-path-name))
  (unless (path? path)
    (raise-user-error 'raco\ dependencies-graph "could not find file for ~a" file))
  (set! file path))

(define-runtime-path module-browser.rkt "standalone-module-browser.rkt")
(define standalone-module-overview/file 
  (dynamic-require module-browser.rkt 'standalone-module-overview/file))

(standalone-module-overview/file file)

;; so 'raco test' doesn't try to run the module browser
(module test racket/base)
