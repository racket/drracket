#lang racket
(require racket/runtime-path
         compiler/find-exe
         pkg)

(define-runtime-path online-compilation-worker.rkt "private/online-compilation-run-tests.rkt")

(define test-pkgs '("test-pkg1" "test-pkg2" "test-pkg3"))

(define (main)
  (define scratch-directory (make-temporary-file "drracket-test_online-compilation.rkt~a"
                                                 'directory))
  (parameterize ([current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (define PLTUSERHOME (build-path scratch-directory "home"))
    (make-directory* PLTUSERHOME)
    (putenv "PLTUSERHOME" (~a PLTUSERHOME))
    
    (for ([test-pkg (in-list test-pkgs)])
      (setup-pkg scratch-directory test-pkg))
    
    (system* (find-exe)
             "-e"
             (~s '(require racket/base pkg))
             "-e"
             (~s `(pkg-install-command
                   #:link #t
                   #:scope 'user
                   ,@(map (λ (x) (path->string (build-path scratch-directory x)))
                          test-pkgs))))
    
    (system* (find-exe) "-t-"
             (~a online-compilation-worker.rkt)
             (~a scratch-directory))
    
    (delete-directory/files scratch-directory)
    (void)))

(define (setup-pkg scratch-directory pkg)
  (define pkg-dir (build-path scratch-directory pkg))
  (unless (directory-exists? pkg-dir) (make-directory pkg-dir))
  (call-with-output-file (build-path pkg-dir "info.rkt")
    (λ (port)
      (display "#lang info\n" port)
      (fprintf port "~s\n" '(define collection 'multi)))
    #:exists 'truncate))


(module+ test (main))
