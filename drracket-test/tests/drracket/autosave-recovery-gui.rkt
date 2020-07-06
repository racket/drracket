#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/class
         racket/file
         racket/gui/base
         framework)

(let ([raw-autosave-contents
       (if (file-exists? autosave:toc-path)
           (call-with-input-file autosave:toc-path read)
           '())])
  (define autosave-contents
    (filter (λ (x) (file-exists? (bytes->path (list-ref x 1))))
            raw-autosave-contents))
  (unless (null? autosave-contents)
    (error 'autosave-recovery-gui.rkt
           "there was a non-empty autosave toc when starting the test, so the test won't work right; contents:\n  ~s"
           autosave-contents)))

(define (run-autosave-test drs defs)
  (queue-callback/res (λ ()
                        ;; insert something so the save file is out of date
                        (send defs insert "\n1")

                        ;; set the autosave delay to 1 second
                        (preferences:set 'framework:autosave-delay 1)))
  ;; wait for twice that long
  (sleep 2)
  (queue-callback
   (λ ()
     ((dynamic-require 'framework 'autosave:restore-autosave-files/gui))))

  (define new-frame (wait-for-new-frame drs))
  (define new-label (send new-frame get-label))
  (queue-callback/res (λ () (send defs undo)))
  (unless (equal? "Recover Autosaved Files" new-label)
    (error 'autosave-recovery-gui.rkt
           "didn't get autosave file frame, title was ~s" new-label))

  (test:button-push "Done"))

(define (run-autoload-test drs defs)
  (define autoload-tmp-file (make-temporary-file "drracket-autoload-test~a.rkt"))
  (call-with-output-file autoload-tmp-file
    (λ (port)
      (displayln "#lang racket" port))
    #:exists 'truncate)
  (queue-callback/res
   (λ ()
     (send defs load-file autoload-tmp-file)))

  (call-with-output-file autoload-tmp-file
    (λ (port)
      (displayln "1" port))
    #:exists 'append)

  (define first-warning-dialog (wait-for-new-frame drs))
  (unless (equal? (send first-warning-dialog get-label) "Warning")
    (error 'autoload-test "warning about file change dialog didn't appear"))
  (define do-not-ask-again "Do not ask again (always use current choice)")
  (test:set-check-box! do-not-ask-again #t)
  (test:button-push "Revert")
  (wait-for-new-frame first-warning-dialog) ;; wait for the dialog to go away after clicking revert

  (define second-defs-content
    (queue-callback/res
     (λ ()
       (send defs get-text))))
  (unless (equal? second-defs-content "#lang racket\n1\n")
    (error 'autoload-test "The new content of the buffer didn't appear the first time, got ~s"
           second-defs-content))

  (call-with-output-file autoload-tmp-file
    (λ (port)
      (displayln "2" port))
    #:exists 'append)

  ;; wait for the revert to kick in, changing the definitions to not
  ;; match the second-defs-content
  (poll-until
   (λ ()
     (queue-callback/res
      (λ ()
        (not (equal? (send defs get-text) second-defs-content))))))

  (define third-defs-content
    (queue-callback/res
     (λ ()
       (send defs get-text))))
  (unless (equal? third-defs-content "#lang racket\n1\n2\n")
    (error 'autoload-test "The new content of the buffer didn't appear the second time, got ~s"
           third-defs-content))

  ;; dirty the buffer
  (queue-callback/res
   (λ ()
     (send defs insert "'dirty-buffer\n"
           (send defs last-position)
           (send defs last-position))))

  ;; dirty the file on disk
  (call-with-output-file autoload-tmp-file
    (λ (port)
      (displayln "3" port))
    #:exists 'append)


  ;; this should notify us about both dialogs
  (define second-warning-dialog (wait-for-new-frame drs))
  (unless (equal? (send second-warning-dialog get-label) "Warning")
    (error 'autoload-test "warning about file change dialog didn't appear"))

  (define has-do-not-ask-again?
    (queue-callback/res
     (λ ()
       (define answer? #f)
       (let loop ([win second-warning-dialog])
         (when (is-a? win window<%>)
           (set! answer? (or answer?
                             (equal? do-not-ask-again (send win get-label)))))
         (when (is-a? win area-container<%>)
           (for ([child (in-list (send win get-children))])
             (loop child))))
       answer?)))
  (when has-do-not-ask-again?
    (error 'autoload-test
           "expected the dialog box when both are dirty to refrain from offering to not ask again"))
  (test:button-push "Revert")
  (wait-for-new-frame second-warning-dialog))

(fire-up-drracket-and-run-tests
 (λ ()
   (define drs (wait-for-drracket-frame))
   (define defs (send drs get-definitions-text))
   (run-autosave-test drs defs)
   (run-autoload-test drs defs)
   ))
