#lang info

(define tools '(("time-keystrokes.rkt")))
(define tool-names '("Time Keystrokes"))
(define compile-omit-paths '("snip" "image-and-comment-box.rkt"))

(define test-timeouts '(("easter-egg.rkt" 300)
                        ("teaching-lang-sharing-modules.rkt" 600)
                        ("no-write-and-frame-leak.rkt" 300)))

(define test-responsibles '(("test-engine-test.rkt" sperber)
                            ("teachpack.rkt" (robby matthias))
                            ("teaching-lang-save-file.rkt" (robby matthias))
                            ("teaching-lang-coverage.rkt" (robby matthias))
                            ("language-test.rkt" (robby matthias))
                            ("hangman.rkt" (robby matthias))))
