#lang racket/base

(module main racket
  (require rackunit/log
           (prefix-in d: drracket/private/expanding-place)
           (prefix-in d: drracket/private/eval-helpers-and-pref-init)
           compiler/cm
           rackunit
           rackunit/text-ui)

  ;; these are expected to be set up pkgs
  (define test-pkgs '("test-pkg1" "test-pkg2" "test-pkg3"))
  
  (define argv (current-command-line-arguments))
  (unless (= 1 (vector-length argv))
    (raise-user-error 'online-compilation-run-tests.rkt
                      "expected the name of the scratch directory on the command line"))
  (define scratch-directory (vector-ref argv 0))
  
  (define (run-the-tests)
    (let ()
      (setup-files
       '("test-pkg1/test-pkg1/x.rkt"
         "#lang racket/base"
         (define x 1)
         (provide x))
       '("test-pkg1/test-pkg1/y.rkt"
         "#lang racket/base"
         (require "x.rkt")))
      
      (check-equal?
       (run-the-compilation-and-get-cms-compiled-file-announcements
        "#lang racket/base\n(require \"y.rkt\")\n"
        (build-path scratch-directory "test-pkg1" "test-pkg1" "z.rkt")
        '()
        'debug)
       (set "test-pkg1/test-pkg1/compiled/drracket/x_rkt.zo"
            "test-pkg1/test-pkg1/compiled/drracket/y_rkt.zo"))
      
      (check-equal?
       (run-the-compilation-and-get-cms-compiled-file-announcements
        "#lang racket/base\n(require \"y.rkt\")\n"
        (build-path scratch-directory "test-pkg1" "test-pkg1" "z.rkt")
        '()
        'debug)
       (set)))
    
    (let ()
      (setup-files
       '("test-pkg1/test-coll/x.rkt"
         "#lang racket/base"
         (define x 1)
         (provide x))
       '("test-pkg2/test-coll/y.rkt"
         "#lang racket/base"
         (require test-coll/x)))
      
      (check-equal?
       (run-the-compilation-and-get-cms-compiled-file-announcements
        (format "#lang racket/base\n~s\n" '(require test-coll/y))
        (build-path scratch-directory "test-pkg3" "test-coll" "z.rkt")
        '()
        'debug)
       (set "test-pkg1/test-coll/compiled/drracket/x_rkt.zo"
            "test-pkg2/test-coll/compiled/drracket/y_rkt.zo"))
      
      (check-equal?
       (run-the-compilation-and-get-cms-compiled-file-announcements
        (format "#lang racket/base\n~s\n" '(require test-coll/y))
        (build-path scratch-directory "test-pkg3" "test-pkg" "z.rkt")
        '()
        'debug)
       (set)))
    
    (let ()
      (setup-files
       '("test-pkg1/test-coll/x.rkt"
         "#lang racket/base"
         (define x 1)
         (provide x))
       '("test-pkg2/test-coll/y.rkt"
         "#lang racket/base"
         (require test-coll/x)))
      
      (check-equal?
       (run-the-compilation-and-get-cms-compiled-file-announcements
        (format "#lang racket/base\n~s\n" '(require test-coll/y))
        (build-path scratch-directory "test-pkg3" "test-pkg" "z.rkt")
        (list (build-path scratch-directory "test-pkg2" "test-coll" "y.rkt"))
        'debug)
       (set "test-pkg1/test-coll/compiled/drracket/x_rkt.zo"
            "test-pkg2/test-coll/compiled/drracket/errortrace/y_rkt.zo"))
      
      (check-equal?
       (run-the-compilation-and-get-cms-compiled-file-announcements
        (format "#lang racket/base\n~s\n" '(require test-coll/y))
        (build-path scratch-directory "test-pkg3" "test-pkg" "z.rkt")
        (list (build-path scratch-directory "test-pkg2" "test-coll" "y.rkt"))
        'debug)
       (set)))
    
    (let ()
      (setup-files
       '("test-pkg1/test-coll/x.rkt"
         "#lang racket/base"
         (define x 1)
         (provide x))
       '("test-pkg1/test-coll/y.rkt"
         "#lang racket/base"
         (define y 1)
         (provide y))
       '("test-pkg1/test-coll/z.rkt"
         "#lang racket/base"
         (define z 1)
         (provide z))
       '("test-pkg1/test-coll/a.rkt"
         "#lang racket/base"
         (require "x.rkt" "y.rkt" "z.rkt")
         (+ x y z)))
      
      (check-equal?
       (run-the-compilation-and-get-cms-compiled-file-announcements
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list (build-path scratch-directory "test-pkg1" "test-coll" "a.rkt"))
        'debug)
       (set "test-pkg1/test-coll/compiled/drracket/errortrace/a_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/y_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/z_rkt.zo")))
    
    (let ()
      (setup-files
       '("test-pkg1/test-coll/x.rkt"
         "#lang racket/base"
         (define x 1)
         (provide x))
       '("test-pkg1/test-coll/y.rkt"
         "#lang racket/base"
         (define y 1)
         (provide y))
       '("test-pkg1/test-coll/z.rkt"
         "#lang racket/base"
         (define z 1)
         (provide z))
       '("test-pkg1/test-coll/a.rkt"
         "#lang racket/base"
         (require "b.rkt"))
       '("test-pkg1/test-coll/b.rkt"
         "#lang racket/base"
         (require "c.rkt"))
       '("test-pkg1/test-coll/c.rkt"
         "#lang racket/base"
         (require test-coll/x test-coll/y test-coll/z)
         (+ x y z)))
      
      (check-equal?
       (run-the-compilation-and-get-cms-compiled-file-announcements
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list (build-path scratch-directory "test-pkg1" "test-coll" "a.rkt"))
        'debug)
       (set "test-pkg1/test-coll/compiled/drracket/errortrace/a_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/b_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/c_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/y_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/z_rkt.zo")))
    
    (let ()
      (setup-files
       '("test-pkg1/test-coll/x.rkt"
         "#lang racket/base"
         (define x 1)
         (provide x))
       '("test-pkg1/test-coll/y.rkt"
         "#lang racket/base"
         (define y 1)
         (provide y))
       '("test-pkg1/test-coll/z.rkt"
         "#lang racket/base"
         (define z 1)
         (provide z))
       '("test-pkg1/test-coll/a.rkt"
         "#lang racket/base"
         (require (for-syntax "b.rkt")))
       '("test-pkg1/test-coll/b.rkt"
         "#lang racket/base"
         (require (for-syntax "c.rkt")))
       '("test-pkg1/test-coll/c.rkt"
         "#lang racket/base"
         (require test-coll/x test-coll/y test-coll/z)
         (+ x y z)))
      
      (check-equal?
       (run-the-compilation-and-get-cms-compiled-file-announcements
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list (build-path scratch-directory "test-pkg1" "test-coll" "a.rkt"))
        'debug)
       (set "test-pkg1/test-coll/compiled/drracket/errortrace/a_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/b_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/c_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/y_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/z_rkt.zo")))
    
    (let ()
      (setup-files
       '("test-pkg1/test-coll/x.rkt"
         "#lang racket/base"
         (define x 1)
         (provide x))
       '("test-pkg1/test-coll/y.rkt"
         "#lang racket/base"
         (define y 1)
         (provide y))
       '("test-pkg1/test-coll/z.rkt"
         "#lang racket/base"
         (define z 1)
         (provide z))
       '("test-pkg1/test-coll/a.rkt"
         "#lang racket/base"
         (require (for-syntax "b.rkt")))
       '("test-pkg1/test-coll/b.rkt"
         "#lang racket/base"
         (require (for-syntax "c.rkt")))
       '("test-pkg1/test-coll/c.rkt"
         "#lang racket/base"
         (require test-coll/x test-coll/y test-coll/z)
         (+ x y z)))
      
      (check-equal?
       (run-the-compilation-and-get-compiled-files
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list (build-path scratch-directory "test-pkg1" "test-coll" "a.rkt"))
        'debug)
       (set "test-pkg1/test-coll/compiled/drracket/errortrace/a_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/b_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/c_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/y_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/z_rkt.zo")))
    
    (let ()
      (setup-files
       '("test-pkg1/test-coll/x.rkt"
         "#lang racket/base"
         (define x 1)
         (provide x))
       '("test-pkg1/test-coll/y.rkt"
         "#lang racket/base"
         (define y 1)
         (provide y))
       '("test-pkg2/test-coll/z.rkt"
         "#lang racket/base"
         (define z 1)
         (provide z))
       '("test-pkg3/test-coll/a.rkt"
         "#lang racket/base"
         (require test-coll/b))
       '("test-pkg3/test-coll/b.rkt"
         "#lang racket/base"
         (require test-coll/c))
       '("test-pkg3/test-coll/c.rkt"
         "#lang racket/base"
         (require test-coll/x test-coll/y test-coll/z)
         (+ x y z)))
      
      (check-equal?
       (run-the-compilation-and-get-compiled-files
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list (build-path scratch-directory "test-pkg1" "test-coll" "x.rkt")
              (build-path scratch-directory "test-pkg3" "test-coll" "a.rkt"))
        'debug)
       (set "test-pkg3/test-coll/compiled/drracket/errortrace/a_rkt.zo"
            "test-pkg3/test-coll/compiled/drracket/errortrace/b_rkt.zo"
            "test-pkg3/test-coll/compiled/drracket/errortrace/c_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/y_rkt.zo"
            "test-pkg2/test-coll/compiled/drracket/z_rkt.zo"))
      
      (check-equal?
       (run-the-compilation-and-get-compiled-files
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list (build-path scratch-directory "test-pkg3" "test-coll" "a.rkt"))
        'debug)
       (set "test-pkg3/test-coll/compiled/drracket/errortrace/a_rkt.zo"
            "test-pkg3/test-coll/compiled/drracket/errortrace/b_rkt.zo"
            "test-pkg3/test-coll/compiled/drracket/errortrace/c_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/y_rkt.zo"
            "test-pkg2/test-coll/compiled/drracket/z_rkt.zo"))
      
      (check-equal?
       (run-the-compilation-and-get-compiled-files
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list (build-path scratch-directory "test-pkg1" "test-coll" "x.rkt")
              (build-path scratch-directory "test-pkg3" "test-coll" "a.rkt"))
        'debug)
       (set "test-pkg3/test-coll/compiled/drracket/errortrace/a_rkt.zo"
            "test-pkg3/test-coll/compiled/drracket/errortrace/b_rkt.zo"
            "test-pkg3/test-coll/compiled/drracket/errortrace/c_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/y_rkt.zo"
            "test-pkg2/test-coll/compiled/drracket/z_rkt.zo"
            
            ;; these aren't necessary, but also aren't problematic (I think)
            "test-pkg1/test-coll/compiled/drracket/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/y_rkt.zo"))
      
      (check-equal?
       (run-the-compilation-and-get-compiled-files
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list (build-path scratch-directory "test-pkg1" "test-coll" "x.rkt"))
        'debug)
       (set "test-pkg3/test-coll/compiled/drracket/a_rkt.zo"
            "test-pkg3/test-coll/compiled/drracket/b_rkt.zo"
            "test-pkg3/test-coll/compiled/drracket/c_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/errortrace/y_rkt.zo"
            "test-pkg2/test-coll/compiled/drracket/z_rkt.zo"
            
            ;; these two are not necessary, but also not problematic (I think)
            "test-pkg1/test-coll/compiled/drracket/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/y_rkt.zo")))
    
    (let ()
      (setup-files
       '("test-pkg1/test-coll/x.rkt"
         "#lang racket/base"
         (define x 1)
         (provide x))
       '("test-pkg1/test-coll/y.rkt"
         "#lang racket/base"
         (define y 1)
         (provide y))
       '("test-pkg2/test-coll/z.rkt"
         "#lang racket/base"
         (define z 1)
         (provide z))
       `("test-pkg3/test-coll/a.rkt"
         "#lang racket/base"
         (require (for-syntax racket/base))
         (begin-for-syntax
           (require compiler/cm-accomplice)
           (register-external-module
            (string->path ,(path->string (build-path scratch-directory "test-pkg3/test-coll/b.rkt"))))))
       '("test-pkg3/test-coll/b.rkt"
         "#lang racket/base"
         (require test-coll/x test-coll/y test-coll/z)
         (+ x y z)))
      
      ;; the first run here seems to just register the external depdency
      ;; and not yet compile any files. I'm not sure if that's right,
      ;; so the expected result here could be different and that would
      ;; be okay. (Not for the next one, tho; that test is supposed to be
      ;; testing that all of the dependencies are gotten)
      (check-equal?
       (run-the-compilation-and-get-compiled-files
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list)
        'debug)
       (set "test-pkg3/test-coll/compiled/drracket/a_rkt.zo"))
      
      (check-equal?
       (run-the-compilation-and-get-compiled-files
        (format "#lang racket/base\n~s\n" '(require test-coll/a))
        (build-path scratch-directory "test-pkg1" "test-pkg" "q.rkt")
        (list)
        'debug)
       (set "test-pkg3/test-coll/compiled/drracket/a_rkt.zo"
            "test-pkg3/test-coll/compiled/drracket/b_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/x_rkt.zo"
            "test-pkg1/test-coll/compiled/drracket/y_rkt.zo"
            "test-pkg2/test-coll/compiled/drracket/z_rkt.zo"))))
  
  (define/contract (run-the-compilation-and-get-cms-compiled-file-announcements program-as-string
                                                                                path
                                                                                currently-open-files
                                                                                annotations)
    (-> string?
        path?
        (listof path?)
        (or/c 'debug 'none) ;; technically more are allowed, but they aren't used now
        (set/c string?))
    (define done (make-channel))
    (thread
     (λ ()
       (define lr (make-log-receiver (current-logger) 'debug 'compiler/cm))
       (let loop ([msgs '()])
         (sync
          (handle-evt lr (λ (x) (loop (cons x msgs))))
          (handle-evt done (λ (c) (channel-put c msgs)))))))
    
    
    (run-expanding-place program-as-string
                         path
                         currently-open-files
                         annotations)
    
    (define msgs (let ([c (make-channel)])
                   (channel-put done c)
                   (channel-get c)))
    (define reg #rx"wrote zo file: (.*)$")
    (for/set ([msg (in-list msgs)]
              #:when (regexp-match? #rx"wrote zo file:" (vector-ref msg 1)))
      (path->string
       (find-relative-path
        scratch-directory
        (string->path (list-ref (regexp-match reg (vector-ref msg 1)) 1))))))
  
  (define/contract (run-the-compilation-and-get-compiled-files program-as-string
                                                               path
                                                               currently-open-files
                                                               annotations)
    (-> string?
        path?
        (listof path?)
        (or/c 'debug 'none) ;; technically more are allowed, but they aren't used now
        (set/c string?))
    
    (run-expanding-place program-as-string
                         path
                         currently-open-files
                         annotations)
    
    (for/set ([file (in-directory scratch-directory)]
              #:when (regexp-match? #rx"[.]zo$" (path->string file)))
      (path->string
       (find-relative-path
        scratch-directory
        file))))
  
  (define/contract (run-expanding-place program-as-string
                                        path
                                        currently-open-files
                                        annotations)
    (-> string?
        path?
        (listof path?)
        (or/c 'debug 'none) ;; technically more are allowed, but they aren't used now
        void?)
    (define-values (us them) (place-channel))
    (thread (λ () (d:start them)))
    
    ;; send a compile lock that protects nothing,
    ;; but that's okay since no one else is writing
    ;; to these files
    (place-channel-put us (make-compile-lock))
    
    ;; no online expansion handlers
    (place-channel-put us '())
    
    (define-values (response-pc-this-side response-pc) (place-channel))
    (define-values (pc-status-expanding-place-this-side pc-status-expanding-place) (place-channel))
    (thread (λ () (let loop () (sync pc-status-expanding-place-this-side) (loop))))
    
    (define settings
      (d:prefab-module-settings #()
                                '(default)
                                #t   ;; compilation-on? 
                                #f   ;; compile-context-preservation-enabled
                                annotations
                                #t)) ;; compile-enforce-module-constants
    
    ;; send the program
    (place-channel-put
     us
     (vector program-as-string
             path
             response-pc
             settings
             pc-status-expanding-place
             currently-open-files))
    (define ans (place-channel-get response-pc-this-side))
    (unless (and (vector? ans)
                 (> (vector-length ans) 0)
                 (equal? (vector-ref ans 0) 'handler-results))
      (error 'run-expanding-place "unexpected result ~s" ans)))
  
  
  (define (setup-files . files)
    
    ;; remove files from previous test
    (for ([pkg (in-list test-pkgs)])
      (for ([dname (in-list (directory-list (build-path scratch-directory pkg)))])
        (define d (build-path scratch-directory pkg dname))
        (when (directory-exists? d) (delete-directory/files d))))
    
    ;; create files according to argument specification
    (for ([file (in-list files)])
      (define filename (build-path scratch-directory (list-ref file 0)))
      (make-directory*
       (let-values ([(base name dir?) (split-path filename)]) base))
      (define lang-line (list-ref file 1))
      (define exps (cddr file))
      (call-with-output-file filename
        (λ (port)
          (display lang-line port)
          (newline port)
          (for ([exp (in-list exps)])
            (pretty-write exp port))))))
  

  (run-the-tests)
  (void (test-log #:display? #t)))

;; raco test does nothign for drdr, since ../online-compilation.rkt
;; will actually set up the environment and run this file's main.
(module+ test)
