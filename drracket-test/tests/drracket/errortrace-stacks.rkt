#lang at-exp racket/base
(require "private/drracket-test-util.rkt"
         framework/test
         racket/class
         racket/file
         racket/path
         racket/gui/base
         rackunit/log
         rackunit)

(define (setup-racket/base-raw) (setup/rb "No debugging or profiling"))
(define (setup-racket/base-debug) (setup/rb "Debugging"))
(define (setup-racket/base-profile) (setup/rb "Debugging and profiling"))
(define (setup-racket/base-coverage) (setup/rb "Syntactic test suite coverage"))

(define (setup/rb which-rb)
  (set-module-language! #f)
  (test:set-radio-box-item! which-rb)
  (let ([f (test:get-active-top-level-window)])
    (test:button-push "OK")
    (wait-for-new-frame f)))

(define-syntax-rule
  (get id ...)
  (begin (define id (dynamic-require 'drracket/tool-lib 'id)) ...))

;; this test runs a program with an error and checks to make
;; sure that the stack dialog pops up and it either does or
;; does not have an errortrace-based stack, as appropriate
(define (check-stack-appearance errortrace-stack? setup-language)
  (define drracket-frame (wait-for-drracket-frame))

  (define ints-text (queue-callback/res (λ () (send drracket-frame get-interactions-text))))

  (setup-language)
  (clear-definitions drracket-frame)
  (insert-in-definitions
   drracket-frame
   @string-append{
    #lang racket/base
    (define (f x)
      (+ 1 (/ x)))

    (define (g x)
      (+ 1 (+ 1 (+ 1 (f x)))))

    (g 0)})

  (do-execute drracket-frame)

  (define ints-content (queue-callback/res (λ () (send ints-text get-text))))
  (unless (regexp-match? #rx"division by zero" ints-content)
    (error 'errortrace-stacks.rkt
           "expected a division by zero error in the interactions window, got:\n~a"
           ints-content))

  ;; try to find the stacktrace button in the interactions window
  (define cb
    (queue-callback/res
     (λ ()
       (let loop ([snip (send ints-text find-first-snip)])
         (cond
           [snip
            (define cb
              (with-handlers ([exn:fail? (λ (x) #f)])
                ;; string snips will fail this
                (send snip get-callback)))
            (cond
              [cb (λ () (cb snip))]
              [else (loop (send snip next))])]
           [else #f])))))

  (unless cb
    (error 'errortrace-stacks.rkt
           (string-append
            "could not find the second clickable snip"
            "in the interactions text, got: ~a")
           ints-content))

  (queue-callback (λ () (cb)))

  (define stacks (wait-for-new-frame drracket-frame))

  (define tab-panel-labels (get-tab-panel-labels stacks))

  (define test-passed?
    (cond
      [errortrace-stack?
       (equal? tab-panel-labels '("Errortrace" "Builtin"))]
      [else
       (equal? tab-panel-labels #f)]))
  (check-true test-passed?)

  (unless test-passed?
    (eprintf "errortrace-stack? ~s and tab-panel-labels ~s don't match up for ~s"
             errortrace-stack? tab-panel-labels setup-language))

  ;; close the stacks window and log the test's result
  (queue-callback/res (λ ()
                        (test-log! test-passed?)
                        (send stacks close)))

  ;; wait for it to close
  (wait-for-new-frame stacks))

(define (test-open-and-highlight-in-file)
  (get drracket:debug:open-and-highlight-in-file)

  (define drracket-frame1 (wait-for-drracket-frame))

  (define tmp1-rkt-file (normalize-path (make-temporary-file "drracket-tests-open-and-highlight1.rkt~a.rkt")))
  (call-with-output-file tmp1-rkt-file
    (λ (port)
      (display "#lang racket/base\n(+ 1 #f)" port))
    #:exists 'truncate)

  (define tmp2-rkt-file (normalize-path (make-temporary-file "drracket-tests-open-and-highlight2.rkt~a.rkt")))
  (call-with-output-file tmp2-rkt-file
    (λ (port)
      (display "#lang racket/base\n(+ 2 #f)" port))
    #:exists 'truncate)

  (queue-callback/res
   (λ ()
     (send (send drracket-frame1 get-definitions-text) load-file tmp1-rkt-file)))

  (test:menu-select "File" "New")

  (define drracket-frame2 (wait-for-new-frame drracket-frame1))

  (queue-callback/res
   (λ ()
     (send (send drracket-frame2 get-definitions-text) load-file tmp2-rkt-file)))

  (queue-callback/res
   (λ ()
     (drracket:debug:open-and-highlight-in-file (srcloc tmp1-rkt-file 1 1 18 1))))

  (define newly-focused-window (wait-for-new-frame drracket-frame2))
  (check-eq? newly-focused-window drracket-frame1)

  (check-equal? (queue-callback/res
                 (λ ()
                   (send (send drracket-frame1 get-definitions-text) get-start-position)))
                17)

  (delete-file tmp1-rkt-file)
  (delete-file tmp2-rkt-file)

  (void))

(define (test-show-backtrace-window/edition-pairs/two)
  (get drracket:debug:show-backtrace-window/edition-pairs/two)

  (define drracket-frame (wait-for-drracket-frame))
  (define tmp1-rkt-file (make-temporary-file "drracket-tests-errortrace-stacks1.rkt~a.rkt"))
  (call-with-output-file tmp1-rkt-file
    (λ (port)
      (display "#lang racket/base\n" port)
      (display "\"line 1\"\n" port)
      (display "\"line 2\"\n" port))
    #:exists 'truncate)
  (queue-callback/res
   (λ ()
     (send (send drracket-frame get-definitions-text) load-file tmp1-rkt-file)))

  (queue-callback/res
   (λ ()
     (drracket:debug:show-backtrace-window/edition-pairs/two
      "error message"
      (list (srcloc tmp1-rkt-file 2 1 19 9)
            (srcloc tmp1-rkt-file 3 1 28 9))
      (list #f #f)
      '() '()
      (send drracket-frame get-definitions-text)
      (send drracket-frame get-interactions-text))))

  (define stacks1 (wait-for-new-frame drracket-frame))
  (check-false (get-tab-panel-labels stacks1))

  (check-equal? (queue-callback/res
                 (λ () (count-embedded-editors-in-container stacks1)))
                2)

  (queue-callback/res
   (λ ()
     (drracket:debug:show-backtrace-window/edition-pairs/two
      "error message"
      (list (srcloc tmp1-rkt-file 2 1 19 9)
            (srcloc tmp1-rkt-file 3 1 28 9))
      (list #f #f)
      (list (srcloc tmp1-rkt-file 2 1 19 9)
            (srcloc tmp1-rkt-file 3 1 28 9))
      (list #f #f)
      (send drracket-frame get-definitions-text)
      (send drracket-frame get-interactions-text))))

  (define stacks2 (wait-for-new-frame drracket-frame))
  (check-not-false (get-tab-panel-labels stacks2))
  (check-equal? (queue-callback/res
                 (λ () (count-embedded-editors-in-container stacks2)))
                2)
  (queue-callback (λ () (send stacks2 close)))

  (delete-file tmp1-rkt-file)
  (void))

(define (test-show-backtrace-window)
  (get drracket:debug:show-backtrace-window)

  (define drracket-frame (wait-for-drracket-frame))
  (define tmp1-rkt-file (make-temporary-file "drracket-tests-errortrace-stacks1.rkt~a.rkt"))
  (call-with-output-file tmp1-rkt-file
    (λ (port)
      (display
       @string-append{
 #lang racket/base
 (provide main)

 (define (f x)
   (+ 1 (/ x)))

 (define (g x)
   (+ 1 (+ 1 (+ 1 (f x)))))
 (set! f f) (set! g g)
 (define (main) (g 0))
 }
       port))
    #:exists 'truncate)

  (queue-callback/res
   (λ ()
     (send (send drracket-frame get-definitions-text) load-file tmp1-rkt-file)))

  ;; here we run the program on our own (not via drr)
  ;; but the exn record should still be good to get
  ;; drracket to show it (without errortrace)
  (define an-exn
    (let ([main (dynamic-require tmp1-rkt-file 'main)])
      (with-handlers ([exn? values])
        (main))))

  ;; test for passing a list of srclocs to
  ;; show-backtrace-window
  (let ()
    (queue-callback/res
     (λ ()
       (drracket:debug:show-backtrace-window
        (exn-message an-exn)
        (for/list ([frame (in-list (continuation-mark-set->context (exn-continuation-marks an-exn)))]
                   #:when (srcloc? (cdr frame)))
          (cdr frame))
        (send drracket-frame get-interactions-text)
        (send drracket-frame get-definitions-text))))

    (define stacks1 (wait-for-new-frame drracket-frame))
    (check-false (get-tab-panel-labels stacks1))

    (let ([frames-in-error
           (queue-callback/res
            (λ () (count-embedded-editors-in-container stacks1)))])
      (check-true (frames-in-error . > . 0)))


    (queue-callback (λ () (send stacks1 close))))

  ;; test for passing an exn to
  ;; show-backtrace-window
  (let ()
    (queue-callback/res
     (λ ()
       (drracket:debug:show-backtrace-window
        (exn-message an-exn)
        an-exn
        (send drracket-frame get-interactions-text)
        (send drracket-frame get-definitions-text))))

    (define stacks2 (wait-for-new-frame drracket-frame))
    (check-false (get-tab-panel-labels stacks2))

    (let ([frames-in-error
           (queue-callback/res
            (λ () (count-embedded-editors-in-container stacks2)))])
      (check-true (frames-in-error . > . 0)))


    (queue-callback (λ () (send stacks2 close))))

  (delete-file tmp1-rkt-file)
  (void))

;; make sure that we see the context of the file when there is an unsaved file
(define (test-show-backtrace-window/unsaved-file)
  (get drracket:debug:show-backtrace-window)

  (define drracket-frame (wait-for-drracket-frame))
  (queue-callback
   (λ ()
     (send (send drracket-frame get-definitions-text) insert
           @string-append{
 #lang racket/base
 (define (f x)
   (+ 1 (/ x)))

 (define (g x)
   (+ 1 (+ 1 (+ 1 (f x)))))
 (set! f f) (set! g g)
 (g 0)
 })))

  (do-execute drracket-frame)

  (define found-and-clicked-snip?
    (queue-callback/res
     (λ ()
       (define ints (send drracket-frame get-interactions-text))
       (let loop ([snip (send ints find-first-snip)])
         (cond
           [(not snip) #f]
           [(object-method-arity-includes? snip 'get-callback 0)
            ((send snip get-callback) snip)
            #t]
           [else (loop (send snip next))])))))

  (check-true found-and-clicked-snip?)

  (when found-and-clicked-snip?
    (define stacks2 (wait-for-new-frame drracket-frame))

    (let ([frames-in-error
           (queue-callback/res
            (λ () (count-embedded-editors-in-container stacks2)))])
      (check-true (frames-in-error . > . 0)))

    (queue-callback (λ () (send stacks2 close))))
  (void))


;; get-tab-panel-labels : -> (or/c (listof string?) #f)
;; #f => no tab panel in the frame
(define (get-tab-panel-labels stacks)
  (queue-callback/res
   (λ ()
     (let loop ([window stacks])
       (cond
         [(is-a? window tab-panel%)
          (for/list ([i (in-range (send window get-number))])
            (send window get-item-label i))]
         [(is-a? window area-container<%>)
          (for/or ([child (in-list (send window get-children))])
            (loop child))]
         [else #f])))))

;; -> natural
;; determines the number of embedded editors in any visible editors it finds
;; should be the same as the number of contexts that are displayed
;; (if there are no duplicates)
(define (count-embedded-editors-in-container stacks)
  (let loop ([container stacks])
    (cond
      [(is-a? container area-container<%>)
       (for/sum ([containee (in-list (send container get-children))])
         (loop containee))]
      [(is-a? container editor-canvas%)
       (count-embedded-editors-in-editor (send container get-editor))]
      [else 0])))

(define (count-embedded-editors-in-editor ed)
  (let loop ([snip (send ed find-first-snip)]
             [n 0])
    (cond
      [snip
       (loop (send snip next)
             (if (is-a? snip editor-snip%)
                 (+ n 1)
                 n))]
      [else n])))


(fire-up-drracket-and-run-tests
 (λ ()
   (test-show-backtrace-window/unsaved-file)
   (test-show-backtrace-window/edition-pairs/two)
   (test-show-backtrace-window)
   (test-open-and-highlight-in-file)
   (check-stack-appearance #f setup-racket/base-raw)
   (check-stack-appearance #t setup-racket/base-debug)
   (check-stack-appearance #t setup-racket/base-profile)
   (check-stack-appearance #t setup-racket/base-coverage)
   ))
