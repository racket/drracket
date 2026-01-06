#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/file
         drracket/private/local-member-names
         racket/class
         racket/path
         racket/pretty
         framework
         (submod drracket/private/module-language oc-status-structs))

(define (wait-for-compilation-to-finish drr-frame #:expected-error-regexp [expected-error-regexp #f])
  (define clean
    (poll-until
     (λ ()
       (queue-callback/res
        (λ ()
          (define status (send (send drr-frame get-current-tab) get-oc-status))
          (and (clean? status) status))))))
  (when (clean-error-type clean)
    (cond
      [expected-error-regexp
       (unless (equal? exn (clean-error-type clean))
         (define msg (clean-error-messages+locs clean))
         (struct exn-info (str full-str src-vecs exn-stack missing-mods) #:prefab)
         (unless (and (pair? msg)
                      (regexp-match? expected-error-regexp (exn-info-str (car msg))))
           (eprintf "online check syntax finished with an error but it doesn't match ~s\n" expected-error-regexp)
           (pretty-write clean (current-error-port))))]
      [else
       (eprintf "online check syntax finished with an error\n")
       (pretty-write clean (current-error-port))])))


(fire-up-drracket-and-run-tests
 (λ ()
   (define tmp-dir (make-temporary-directory "online-compilation-zo-creation~a"))

   ;; 1. setup the files used in the test

   (define x.rkt (build-path tmp-dir "x.rkt"))
   (define y.rkt (build-path tmp-dir "y.rkt"))
   (call-with-output-file x.rkt
     (λ (port)
       (fprintf port "#lang racket/base\n")
       (fprintf port "~s\n" `(require "y.rkt")))
     #:exists 'truncate)
   (call-with-output-file y.rkt
     (λ (port)
       (fprintf port "#lang racket/base\n(+ 1 (+ 2 (+ 3 (car 'i-am-not-a-pair))))\n"))
     #:exists 'truncate)

   ;; 2. load up x.rkt and wait for online expansion to finish

   (define drr-frame (wait-for-drracket-frame))
   (queue-callback/res
    (λ ()
      (send (send drr-frame get-definitions-text) load-file x.rkt)))
   (wait-for-compilation-to-finish drr-frame)

   ;; 3. check to make sure the files are compiled

   (define compiled-dir (let ([l (use-compiled-file-paths)])
                          (if (pair? l)
                              (car l)
                              "compiled")))

   (define compiled-dir-files
     (cond
       [(directory-exists? (build-path tmp-dir compiled-dir))
        (for/list ([file (in-directory (build-path tmp-dir compiled-dir))])
          (find-relative-path tmp-dir file))]
       [else
        '()]))
   (define expected-file (build-path compiled-dir "drracket/errortrace/y_rkt.zo"))
   (unless (member expected-file compiled-dir-files)
     (eprintf "expected to find ~s in compiled dir but it contained ~s\n"
              expected-file compiled-dir-files))

   ;; 4. run the program to see it complete successfully

   (do-execute drr-frame)
   (define the-output (fetch-output drr-frame))

   (unless (regexp-match?
            (string-append
             "^[^\n]*" ;; this makes sure the real error message is on the first line
             (regexp-quote "car: contract violation\n  expected: pair?\n  given: 'i-am-not-a-pair"))
            the-output)
     (eprintf "wrong error message in interactions window:\n--------------------\n\n~a\n\n--------------------\n"
              the-output))
   
   ;; 5. delete y.rkt (so the require in x.rkt is an error now), edit x.rkt, and wait for compilation to finish
   
   (delete-file y.rkt)
   (queue-callback/res
    (λ ()
      (define defs (send drr-frame get-definitions-text))
      (send defs insert " " (send defs last-position) (send defs last-position))))
   
   (wait-for-compilation-to-finish drr-frame #:expected-error-regexp #rx"^x.rkt[^ ]* cannot open module file module path")


   ;; 6. use a 3d value and make sure online compilation still works in the default mode

   (call-with-output-file x.rkt
     (λ (port)
       (fprintf port "#lang racket\n")
       (fprintf port "~s\n" `(define-syntax (m stx)
                               (define (f x) x)
                               #`(#,f 1)))
       (fprintf port "~s\n" `m))
     #:exists 'truncate)
   (queue-callback/res
    (λ ()
      (send (send drr-frame get-definitions-text) load-file x.rkt)))

   (wait-for-compilation-to-finish drr-frame)

   ;; 7. use a 3d value and make sure online compilation still works in debug mode

   (set-module-language! #f)
   (test:set-radio-box-item! #rx"No debugging or profiling")
   (let ([f (test:get-active-top-level-window)])
     (test:button-push "OK")
     (wait-for-new-frame f))

   (queue-callback/res
    (λ ()
      (send (send drr-frame get-definitions-text) load-file x.rkt)))

   (wait-for-compilation-to-finish drr-frame)

   (delete-directory/files tmp-dir)))
