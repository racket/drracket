#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/file
         drracket/private/local-member-names
         racket/class
         racket/path
         (submod drracket/private/module-language oc-status-structs))
(fire-up-drracket-and-run-tests 
 (λ ()
   (define tmp-dir (make-temporary-file "online-compilation-zo-creation~a" 'directory))
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
   (define drs-frame (wait-for-drracket-frame))
   (queue-callback/res
    (λ ()
      (send (send drs-frame get-definitions-text) load-file x.rkt)))
   (poll-until
    (λ ()
      (queue-callback/res
       (λ ()
         (clean? (send (send drs-frame get-current-tab) get-oc-status))))))

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

   (do-execute drs-frame)
   (define the-output (fetch-output drs-frame))

   (unless (regexp-match?
            (string-append
             "^[^\n]*" ;; this makes sure the real error message is on the first line
             (regexp-quote "car: contract violation\n  expected: pair?\n  given: 'i-am-not-a-pair"))
            the-output)
     (eprintf "wrong error message in interactions window:\n--------------------\n\n~a\n\n--------------------\n"
              the-output))
   
   (delete-directory/files tmp-dir)))
