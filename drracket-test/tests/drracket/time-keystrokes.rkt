#lang racket

(require drracket/tool
         racket/gui/base
         framework)

(provide tool@)

(define-local-member-name test-running?)

(define short-str "(abc)")
(define chars-to-test (build-string
                       400
                       (λ (i) (string-ref short-str (modulo i (string-length short-str))))))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define (tool-mixin super%)
      (class super%
        (define r? #f)
        (define/public (test-running?) r?)
        (inherit get-button-panel)
        (super-new)
        (let ([button (new button%
                           (label "Time Keystrokes")
                           (parent (get-button-panel))
                           (callback
                            (lambda (button evt)
                              (time-keystrokes this))))])
          (send (get-button-panel) change-children
                (lambda (l)
                  (cons button (remq button l)))))

        (define/private (time-keystrokes frame)
          (set! r? #t)
          (let loop ([n 10])
            (when (zero? n)
              (error 'time-keystrokes "could not find drracket frame"))
            (define front-frame (test:get-active-top-level-window))
            (unless (eq? front-frame frame)
              (sleep 1/10)
              (loop (- n 1))))
          (let ([win (send frame get-definitions-canvas)])
            (send win focus)
            (time (send-key-events win chars-to-test)))
          (set! r? #f))))

    (define (send-key-events window chars)
      (for-each (λ (char)
                  (send-key-event window (new key-event% (key-code char))))
                (string->list chars)))

    ;; copied from framework/test.rkt
    (define (send-key-event window event)
      (let loop ([l (ancestor-list window #t)])
        (cond [(null? l)
               (cond
                 [(method-in-interface? 'on-char (object-interface window))
                  (send window on-char event)]
                 [(is-a? window text-field%)
                  (send (send window get-editor) on-char event)]
                 [else
                  (error
                   'send-key-event
                   "focused window is not a text-field% and does not have on-char: ~s" window)])]
              [(and (is-a? (car l) window<%>)
                    (send (car l) on-subwindow-char window event))
               #f]
              [else (loop (cdr l))])))

    ;; copied from framework/test.rkt
    (define (ancestor-list window stop-at-top-level-window?)
      (let loop ([w window] [l null])
        (if (or (not w)
                (and stop-at-top-level-window?
                     (is-a? w top-level-window<%>)))
            l
            (loop (send w get-parent) (cons w l)))))

    (when (getenv "PLTDRKEYS")
      (printf "PLTDRKEYS: installing unit frame mixin\n")
      (drracket:get/extend:extend-unit-frame tool-mixin))))

(module+ test
  (require tests/drracket/private/drracket-test-util
           framework/test)

  (parameterize ([current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (putenv "PLTDRKEYS" "1")
    (fire-up-drracket-and-run-tests
     (λ ()
       (define drr-frame (wait-for-drracket-frame))
       (test:button-push "Time Keystrokes")
       (poll-until (λ () (not (send drr-frame test-running?))))))))
