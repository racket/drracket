;;; repl-test.ss

(require-library "function.ss")
(require-library "file.ss")

(load "drscheme-test-util.ss")

(define (open-language-dialog)
  (let ([f (mred:get-top-level-focus-window)])
    (fw:test:menu-select "Language" "Configure Language...")
    (wait-for-new-frame f)
    (with-handlers ([exn:user? (lambda (x) (void))])
      (fw:test:button-push "Show Details"))))

(define (set-language vocabulary close-dialog?)
  (open-language-dialog)
  (fw:test:set-choice! "Language" vocabulary)
  (when close-dialog?
    (let ([f (mred:get-top-level-focus-window)])
      (fw:test:button-push "OK")
      (wait-for-new-frame f))))

(define (test-setting setting-name value expression result)
  (fw:test:set-check-box! setting-name value)
  (let ([f (mred:get-top-level-focus-window)])
    (fw:test:button-push "OK")
    (wait-for-new-frame f))
  (let* ([drs (mred:get-top-level-focus-window)]
	 [interactions (ivar drs interactions-edit)])
    (clear-definitions drs)
    (type-in-definitions drs expression)
    (push-button-and-wait (ivar drs execute-button))
    (let* ([ans-start (send interactions paragraph-start-position 2)]
	   [ans-end (send interactions paragraph-end-position (- (send interactions last-paragraph) 1))]
	   [got (send interactions get-text ans-start ans-end)])
      (unless (string=? result got)
	(printf "FAILED: ~a test~n expected: ~a~n     got: ~a~n" expression result got)))))

(define (mzscheme)
  (set-language "MzScheme" #f)
  (test-setting "Eq? only compares symbols" #f "(eq? 1 1)" "#t")
  (open-language-dialog)
  (test-setting "Eq? only compares symbols" #t "(eq? 1 1)" "eq?: expected symbols as arguments, received 1 and 1")
  (open-language-dialog)
  (test-setting "Unmatched cond/case is an error" #t "(cond [#f #f])" "cond or case: no matching clause")
  (open-language-dialog)
  (test-setting "Unmatched cond/case is an error" #f "(cond [#f #f])" "")
  (open-language-dialog)
  (test-setting "Allow set! on undefined identifiers" #t "(begin (set! x 123) x)" "123")
  (open-language-dialog)
  (test-setting "Allow set! on undefined identifiers" #f "(begin (set! x 123) x)" "set!: cannot set undefined identifier: x"))

(define (zodiac-generic language)
  (set-language language #f)
  (test-setting "Eq? only compares symbols" #f "(eq? 1 1)" "#t")
  (open-language-dialog)
  (test-setting "Eq? only compares symbols" #t "(eq? 1 1)" "eq?: expected symbols as arguments, received 1 and 1")
  (open-language-dialog)
  (test-setting "Unmatched cond/case is an error" #t "(cond [#f #f])" "no matching cond clause")
  (open-language-dialog)
  (test-setting "Unmatched cond/case is an error" #f "(cond [#f #f])" ""))

(define (zodiac-beginner)
  (zodiac-generic "Beginner"))

(define (zodiac-intermediate)
  (zodiac-generic "Intermediate"))
  
(define (zodiac-advanced)
  (zodiac-generic "Advanced")
  (open-language-dialog)
  (test-setting "Allow set! on undefined identifiers" #t "(set! x 123) x" "123")
  (open-language-dialog)
  (test-setting "Allow set! on undefined identifiers" #f "(set! x 123) x" "set!: cannot set undefined identifier: x"))

(zodiac-beginner)
(zodiac-intermediate)
(zodiac-advanced)
(mzscheme)


