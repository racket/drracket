
(define (send-string str)
  (for-each fw:test:keystroke (string->list str)))

(define top-window 
  (let loop ([window (get-top-level-focus-window)])
    (if (is-a? window frame%)
        window
        (begin
          (printf "Got this value: ~s~n" window)
          (printf "waiting...~n")
          (sleep 3)
          (loop (get-top-level-focus-window))))))

(printf "got a frame.~n")

(send (ivar top-window definitions-canvas) focus)

(send-string "(+ 1 2)")

(fw:test:button-push (ivar top-window stepper-button))

