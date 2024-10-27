#lang at-exp racket/base
(require "private/module-lang-test-utils.rkt"
         "private/drracket-test-util.rkt"
         framework
         drracket/private/stack-checkpoint
         racket/list
         racket/class)
(provide run-test)

;; set up for tests that need external files
(write-test-modules
 (module module-lang-test-tmp1 mzscheme
   (provide (all-from-except mzscheme +)
            x)
   (define x 1))
 (module module-lang-test-tmp2 mzscheme
   (provide e)
   (define e #'1))
 (module module-lang-test-tmp3 mzscheme
   (define-syntax (bug-datum stx)
     (syntax-case stx ()
       [(dat . thing)
        (number? (syntax-e (syntax thing)))
        (syntax/loc stx (#%datum . thing))]))
   (provide #%module-begin [rename bug-datum #%datum]))
 (module module-lang-test-tmp4 racket/base
   (/ 888 2)
   (provide (except-out (all-from-out racket/base) #%top-interaction)))
 (module module-lang-test-syn-error racket/base
   (lambda)))

(test @t{}
      #f
      @rx{Module Language: There must be a valid module
          Try starting your program with
          Interactions disabled}
      #t)
(test @t{1}
      #f
      @rx{Module Language: only a module expression is allowed
          Interactions disabled}
      #t)
(test @t{(module m racket) 1}
      #f
      @rx{Module Language: there can only be one expression in the definitions
          Interactions disabled}
      #t)
(test @t{.}
      #f
      @rx{read-syntax: illegal
          Interactions disabled}
      #t)
(test @t{#lang mzscheme
         (define x 1)}
      @t{x}
      "1")
(test @t{#lang m-z-scheme
         (define x 1)}
      #f
      @rx{collection not found
          Interactions disabled}
      #t)
(test @t{#lang racket
         3}
      #f
      "3")
(test @t{(module m racket (provide x) (define x 1))}
      @t{x}
      "1")
(test @t{(module m racket (define x 1))}
      @t{x}
      "1")
(test @t{(module m racket (define x 1) (define y 1) (provide y))}
      @t{x}
      "1")
(test @t{(module m racket (define x 1) (define y 2) (provide y))}
      @t{y}
      "2")
(test @t{(module m mzscheme (require mzlib/list))}
      @t{foldl}
      #rx"foldl")
(test @t{(module m mzscheme (require (rename mzlib/list local-foldl foldl)))}
      @t{local-foldl}
      #rx"foldl>")
(test @t{(module m mzscheme (require (all-except mzlib/list foldl)))}
      @t{first}
      #rx"first>")
(test @t{(module m mzscheme (require (all-except mzlib/list foldl)))}
      @t{foldl}
      #rx"[.] [.] foldl:.*cannot reference an identifier before its definition")
(test @t{(module m mzscheme (require (prefix mz: mzscheme)))}
      @t{mz:+}
      #rx"procedure:[+]")
(test @t{(module n mzscheme (provide (all-from-except mzscheme +)))}
      @t{+}
      #rx"procedure:[+]")
(test @t{(module m mzscheme
           (require (prefix x: mzlib/list) mzlib/list))}
      @t{foldl}
      #rx"foldl>")
(test @t{(module m mzscheme
           (require (prefix x: mzlib/list) mzlib/list))}
      @t{x:foldl}
      #rx"foldl>")
(test @t{(module m (file @in-here{module-lang-test-tmp1.rkt}) x)}
      @t{x}
      "1")
;; + shouldn't be bound in the REPL because it isn't bound in the module.
(test @t{(module m (file @in-here{module-lang-test-tmp1.rkt}) x)}
      @t{+}
      #rx"[.] [.] [+]:.*cannot reference an identifier before its definition")
(test @t{(module m mzscheme (provide lambda))}
      @t{(lambda (x) x)}
      #rx"<procedure")
(test @t{(module m mzscheme (define-syntax (m x) (syntax 1)) (provide m))}
      @t{(m)}
      "1")
(test @t{(module m mzscheme (define-syntax s (syntax 1)) (provide s))}
      @t{s}
      #rx"[.] s: illegal use of syntax")
(test @t{(module m mzscheme (define-syntax (x stx) #'(define a 10)) x x)}
      @t{a}
      #rx"[.] [.] a:.*cannot reference an identifier before its definition")
(test @t{(module m mzscheme
           (define-syntax (x stx) #'(define-syntax (a stx) #'10))
           x
           x)}
      @t{a}
      #rx"[.] [.] a:.*cannot reference an identifier before its definition")
(test @t{(module m mzscheme
           (define-syntax (x stx) #'(define a 10))
           x
           x
           (define a 77))}
      @t{a}
      "77")
(test @t{(module m mzscheme
           (define-syntax (x stx) #'(define-syntax (a stx) #'10))
           x
           x
           (define a 78))}
      @t{a}
      "78")
(test @t{(module m mzscheme
           (require-for-syntax (file @in-here{module-lang-test-tmp2.rkt}))
           (provide s)
           (define-syntax (s stx) e))}
      @t{(require 'm) s}
      @rx{[?]:.*literal data is not allowed.*no #%datum syntax transformer is bound in: 1$})
(test @t{(module tmp mzscheme
           (provide (rename app #%app)
                    (rename -current-namespace current-namespace)
                    (rename -module->namespace module->namespace))
           (define x 2)
           (define -current-namespace error)
           (define -module->namespace error)
           (define-syntax app (syntax-rules () ((app . x) '(app . x)))))}
      @t{x}
      "2")
(test @t{#lang racket
         (eval 'cons)}
      #f
      @rx{cons: unbound identifier.*no #%top syntax transformer is bound})
(test @t{(module m (file @in-here{module-lang-test-tmp1.rkt}) 1 2 3)}
      @t{1} ;; just make sure no errors.
      "1")

(test @t{#lang racket}
      @t{(begin-for-syntax (+ 1 2))}
      @t{})

(test @t{#lang racket}
      @t{(begin (struct s (x)) (struct t s (y)) (s-x (t 1 2)))}
      "1")

(test @t{#lang racket/base
         (read-accept-reader)
         (read-accept-compiled)}
      #f
      @t{#f
         #f})

;; check that we have a working repl in the right language after
;; syntax errors, unless it's a bad language
(test @t{#lang racket
         (define x 1)
         (define y (/ 0))}
      @t{(+ 122 x)}
      @rx{. /: division by zero
          123}
      #t)
(test @t{#lang racket
         (define x 1)
         (define y (/ 0))}
      @t{(if x 123)}
      @rx{/: division by zero.*if: missing an "else"}
      #t)
(test @t{#lang mzscheme
         (define x 1)
         (define y (/ 0))}
      @t{(if x 123)}
      @rx{. /: division by zero
          123}
      #t)
(test @t{(module xx scheme/list
           (define x 1)
           (define y (/ 0)))}
      #f
      @rx{no #%module-begin binding in the module's language
          Interactions disabled:
          does not support a REPL \(no #%top-interaction\)}
      #t)
(test @t{(module xx (file @in-here{module-lang-test-tmp4.rkt})
           (define x 1)
           (* x 123))}
      #f
      @rx{444
          123
          Interactions disabled:
          does not support a REPL \(no #%top-interaction\)
          }
      #t)
(test @t{(module xx (file @in-here{this-file-does-not-exist})
           (define x 1)
           (* x 123))}
      #f
      @rx{cannot open module file
          Module Language: invalid language specification
          Interactions disabled}
      #t)
(test @t{#lang info}
      #f
      ;; test the complete buffer, to make sure that there is no error
      ;; would be better if this said "lang" and not "setup/infotab" but
      ;; leave it like this for now.
      "\nInteractions disabled: setup/infotab does not support a REPL (no #%top-interaction)"
      #t)

;; test racket/load behavior
(test @t{#lang racket/load
         (module m mzscheme (provide x) (define x 2))
         (require 'm)
         (printf "~s\n" x)
         (flush-output)}
      #f
      "2")
(test @t{#lang racket/load
         (module m mzscheme (provide x) (define x 2))
         (module n racket/base (require 'm) (provide y) (define y (* x x)))
         (require 'n)
         (printf "~s\n" y)
         (flush-output)}
      #f
      "4")

(test @t{#lang racket
         (define-syntax (f stx)
           (syntax-case stx ()
             [(f)
              (raise (make-exn:fail:syntax "both" (current-continuation-marks) (list #'f stx)))]))}
      @t{(f)}
      (string-append "> (f)\n"
                     ". both in:\n"
                     "  f\n"
                     "  (f)")
      #t)

(test @t{#lang racket/base}
      @t{(begin (values) 1)}
      "1")

(test @t{#lang racket/base}
      @t{    (eval '(values 1 2))}
      @t{1@"\n"2})
(test @t{#lang racket/base}
      @t{    (eval '(list 1 2))}
      @t{'(1 2)})
(test @t{#lang racket/base}
      @t{    (eval '(lambda ()))}
      @t{lambda: bad syntax in: (lambda ())})
(test @t{#lang racket/base}
      @t{(expt 3 (void))}
      @rx{expt: contract violation.*given: #<void>})
(test @t{#lang racket/base}
      @t{1 2 ( 3 4}
      #rx"1\n2\n[.][^\n]*read-syntax: expected a `[)]` to close `[(]`")
(test @t{#lang racket/base}
      "1 2 . 3 4"
      #rx"1\n2\n[.][^\n]*read-syntax: illegal use of `[.]`")
(test @t{#lang racket/base}
      "1 2 (lambda ()) 3 4"
      "1\n2\n. lambda: bad syntax in: (lambda ())")
(test @t{#lang racket/base}
      "1 2 x 3 4"
      #rx"1\n2\n[.] [.] x:.*cannot reference an identifier before its definition")
(test @t{#lang racket/base}
      "1 2 (raise 1) 3 4"
      "1\n2\nuncaught exception: 1")
(test @t{#lang racket/base}
      "1 2 (raise #f) 3 4"
      "1\n2\nuncaught exception: #f")
(test @t{#lang racket/base}
      "(current-namespace (make-empty-namespace)) if"
      #rx". #%top-interaction: unbound identifier.*no #%app syntax transformer is bound")
(test @t{#lang racket/base}
      (string-append
       "(let ([old (error-escape-handler)])\n"
       "(+ (let/ec k\n(dynamic-wind\n"
       "(lambda () (error-escape-handler (lambda () (k 5))))\n"
       "(lambda () (expt 3 #f))\n"
       "(lambda () (error-escape-handler old))))\n"
       "10))")
      #rx"[.] [.] expt: contract violation.*given: #f\n15")
(test @t{#lang racket/base}
      "(write (list (syntax x)))"
      "(.)")
(test @t{#lang racket/base}
      "(parameterize ([current-output-port (open-output-string)]) (write #'1))"
      "")
(test @t{#lang racket/base}
      "(write-special 1)"
      "1#t")
(test @t{#lang racket/gui}
      (format "~s ~s ~s"
              '(define s (make-semaphore 0))
              '(queue-callback
                (lambda ()
                  (dynamic-wind
                   void
                   (lambda () (expt 3 #f))
                   (lambda () (semaphore-post s)))))
              '(begin (yield s) (void)))
      #rx"[.] [.] expt: contract violation.*given: #f")
(test @t{#lang racket/base}
      (format "~s ~s" 
              '(define x 1) 
              '((λ (x y) y) (set! x (call/cc (lambda (x) x))) (x 3)))
      #rx". . application:.*given: 3")
(test @t{#lang racket/base}
      (format "~s ~s ~s ~s"
              '(begin (define k (call/cc (λ (x) x)))
                      (define x 'wrong))
              '(set! x 'right)
              '(k 1)
              'x)
      "'right")
(test @t{#lang racket/base}
      (format "~s"
              '(call-with-continuation-prompt
                (lambda ()
                  (eval '(begin (abort-current-continuation
                                 (default-continuation-prompt-tag)
                                 1 2 3)
                                10)))
                (default-continuation-prompt-tag)
                list))
      "'(1 2 3)")
(test @t{#lang racket/gui}
      "(vector (new snip%))"
      "(vector .)")
(test @t{#lang racket/base}
      "(begin (thread (lambda () x)) (sleep 1/10))"
      #rx"[.] [.] x:.*cannot reference an identifier before its definition")
(test @t{#lang racket/base}
      "(require texpict/utils)(disk 3)"
      ".")
(test @t{#lang racket/base}
      (string-append
       "(require mzlib/pretty)"
       "(pretty-print-print-hook (lambda x (expt 3 #f)))"
       "(list 1 2 3)")
      "'(1 2 3)")

;; test protection against user-code changing the namespace
(test @t{#lang racket/base
         (current-namespace (make-base-namespace))}
      "(+ 1 2)"
      "3")
(test @t{#lang racket/base
         (current-namespace (make-base-empty-namespace))}
      "(+ 1 2)"
      "3")
(test @t{#lang racket/base}
     @t{(parameterize ([current-directory "/does/not/exists/well/it/better/not/anwyays"])
         (load @in-here{module-lang-test-syn-error.rkt}))}
     ;; test to make sure that we don't get "exception raised by error display handler"
     #rx"module-lang-test-syn-error.rkt:[0-9]+:[0-9]+: lambda: bad syntax in: \\(lambda\\)")

(test @t{#lang racket
(module+ main (printf "main\n"))
(module+ test (printf "test\n"))
(module+ other (printf "other\n"))}
      #f
      #rx"test\nmain")


(test @t{#lang racket}
      (format "~s" '(+ 1 (+ 1 (abort-current-continuation
                               (default-continuation-prompt-tag)
                               (lambda () 
                                 (abort-current-continuation
                                  (default-continuation-prompt-tag)
                                  (λ () 0)))))))
      "0")
   
(test @t{#lang racket}
      (format "~s ~s ~s" 
              '1
              '(+ 1 (+ 1 (abort-current-continuation
                          (default-continuation-prompt-tag)
                          (lambda () 
                            (abort-current-continuation
                             (default-continuation-prompt-tag)
                             (λ () 0))))))
              '2)
      "1\n0")

(test @t{#lang racket}
      (format "~s" 
              '(begin
                 1
                 (+ 1 (+ 1 (abort-current-continuation
                            (default-continuation-prompt-tag)
                            (lambda () 
                              (abort-current-continuation
                               (default-continuation-prompt-tag)
                               (λ () 0))))))
                 2))
      "0")

(test '("#lang racket\n" xml-box)
      #f
      @t{'(a () "x")})


(test @t{#lang racket/base
         (define os (open-output-string))
         (display "before\n")
         (parameterize ([current-output-port os])
           (displayln "not shown")
           ((current-print) 'aaa))
         (display "after\n")}
        #f
        #rx"before\nafter")

(test @t{#lang racket/base
         ((current-print) 1)
         ((current-print) 2)
         ((current-print) 3)}
      #f
      "1\n2\n3")
(test @t{#lang racket/base
         (define (f n) ((global-port-print-handler) n (current-output-port)))
         (f 1) (f 2) (f 3)}
      #f
      "123")
(test @t{#lang htdp/bsl
         1 2 3}
      #f
      "1\n2\n3")
(test @t{#lang racket
         (require pict)
         (define-values (in out) (make-pipe-with-specials))
         (print (colorize (filled-rectangle 4 4) "red") out)}
      #f
      "")
(test @t{#lang racket
         (require pict)
         (scale (blank 1 1) -1 1)}
      #f
      ".")
(test @t{#lang racket
         (void "🏴‍☠️")
         free-🏴‍☠️-var}
      #f
      #rx"unbound identifier in: free-🏴‍☠️-var"
      #:extra-assert (λ (defs ints)
                       (equal?
                        (for/list ([range (send defs get-highlighted-ranges)])
                          (list (text:range-start range) (text:range-end range)))
                        '((27 40)))))
(test @t{
#lang racket
(struct exn:fail:user-srcloc exn:fail:user (srclocs)
  #:property prop:exn:srclocs
  (λ (s) (exn:fail:user-srcloc-srclocs s)))

(define-syntax (m stx)
  (syntax-case stx ()
    [(_ expr)
     #`(raise (exn:fail:user-srcloc "srcloc" (current-continuation-marks)
                                    (list
                                     (srcloc '#,(syntax-source #'expr)
                                             '#,(syntax-line #'expr)
                                             '#,(syntax-column #'expr)
                                             '#,(syntax-position #'expr)
                                             '#,(syntax-span #'expr)))))]))

(m abc)
}
      #f
      #rx"^[.] srcloc"
      #:extra-assert
      (λ (defs ints)
        (equal?
         (for/list ([range (send defs get-highlighted-ranges)])
           (list (text:range-start range) (text:range-end range)))
         '((680 683)))))

(test @t{
#lang racket
(define sp (open-output-string))
(let/ec k
  (parameterize ([error-escape-handler k])
    (parameterize ([current-error-port sp])
      (raise-argument-error 'f "string?" 1 0 1 2 3))))
(display (get-output-string sp))
}
      #f
      @t{
f: contract violation
  expected: string?
  given: 1
  argument position: 2nd
  other arguments...:
   0
   2
   3
}
      )

(test @t{
 #lang htdp/isl+

 (check-expect (+ 123 45 6) even?)

}
      #f
      #rx"check-expect.*function"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:3:0"
                              (srcloc->string loc)))
             ;; ^ check-expect is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-expect.*[?][)]"
                                      (test-definitions test)))
             ;; ^ check-expect is highlighted
             )))

(test @t{
 #lang htdp/isl+

 (check-expect (sqrt 2) (sqrt 2))

}
      #f
      #rx"check-expect.*inexact"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:3:0"
                              (srcloc->string loc)))
             ;; ^ check-expect is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-expect.*sqrt 2[)][)]"
                                      (test-definitions test)))
             ;; ^ check-expect is highlighted
             )))

(test @t{
 #lang htdp/isl+
 (define p (make-posn 7 3))
 (check-expect posn-x 7)

}
      #f
      #rx"Ran 1 test.\n0 tests passed."
      #|
      check-expect encountered the following error instead of the expected value, 7. 
         ::  at line 3, column 0 first argument of equality cannot be a function, given posn-x
      at line 3, column 0
      |#
      #:extra-assert
      (λ (defs ints #:test test)
        (define re
          (pregexp
           (string-append
            "check-expect[ a-z]+error.*[^\n]+\n"
            ".*::.*at line 3, column 0 first argument.*function.*given posn-x[^\n]*\n"
            "at line 3, column 0")))
        ;; Includes the flattened test result snips.
        (define full-ints-text
          (send ints get-text (send ints paragraph-start-position 2) 'eof #t))
        (define passed?
          (regexp-match? re full-ints-text))
        (unless passed?
          (eprintf "FAILED line ~a: ~a\n  extra assertion expected: ~s\n\n  got: ~a\n"
                   (test-line test)
                   (test-definitions test)
                   re
                   full-ints-text))
        passed?))

(test @t{
 #lang htdp/isl+


 (check-random (+ (random 5) (sqrt 2))
               (+ (random 5) (sqrt 2)))

}
      #f
      #rx"check-random.*inexact"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:4:0"
                              (srcloc->string loc)))
             ;; ^ check-random is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-random.*sqrt 2[)][)][)]"
                                      (test-definitions test)))
             ;; ^ check-random is highlighted
             )))

(test @t{
 #lang htdp/isl+

  (check-within (sqrt 2) 3/2 "0.1")

}
      #f
      #rx"check-within.*\"0[.]1\".*not inexact"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:3:1"
                              (srcloc->string loc)))
             ;; ^ check-within is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-within.*0[.]1\"[)]"
                                      (test-definitions test)))
             ;; ^ check-within is highlighted
             )))

(test @t{
 #lang htdp/isl+
 (define (my-add1 n) (+ n 1))
 my-add1
 (check-expect my-add1 2)
}
      #f
      #rx"^my-add1\nRan 1 test[.]\n0 tests passed[.]"
      #:extra-assert
      (λ (defs ints)
        (regexp-match? #px"::\\s+at line 4, column 0[^\n]+function[^\n]+given my-add1"
                       ;; Includes the flattened test result snips.
                       (send ints get-text (send ints paragraph-start-position 2) 'eof #t))))

(test @t{#lang htdp/isl
         (check-expect (* 2 3) 6)
         (check-expect (+ 2 3) 5)}
      #f
      #rx"^Both tests passed!$")

(test @t{#lang htdp/isl}
      ;; REPL
      @t{(check-expect (* 2 3) 6)
         (check-expect (+ 2 3) 5)}
      #rx"^The test passed!\nThe test passed!$")

(test @t{#lang htdp/isl
         (check-expect (* 2 3) 6)
         (check-expect (* 2 3) 5)}
      #f
      #rx"^Ran 2 tests[.]\n1 of the 2 tests failed[.].*Check failures:")

(test @t{#lang htdp/isl}
      ;; REPL
      @t{(check-expect (* 2 3) 6)
         (check-expect (* 2 3) 5)}
      #rx"^The test passed!\nRan 1 test[.]\n0 tests passed[.].*Check failures:")

(test @t{#lang htdp/isl}
      ;; REPL
      @t{(check-expect (* 2 3) 5)
         (check-expect (* 2 3) 6)}
      #rx"^Ran 1 test[.]\n0 tests passed[.].*Check failures:.*\nThe test passed!$")

(test @t{#lang htdp/isl
         (check-expect (* 2 3) 6)
         (check-expect (* 2 3) 5)
         (check-expect (+ 2 3) 5)}
      ;; REPL
      @t{(check-expect (+ 4 5) 9)
         (check-expect (+ 6 7) 42)
         (check-expect (* 8 9) 72)
         (check-expect (error 'oops) 111)}
      #px"^Ran 3 tests[.]\\s+1 of the 3 tests failed[.]"
      #t
      #:extra-assert
      (λ (defs ints #:test test)
        (define re
          (pregexp
           @t{^Ran 3 tests[.]
              1 of the 3 tests failed[.]

              Check failures:\s*
                +Actual value 6 differs from 5, the expected value[.]\s*
              at line 3, column 0
              > @(regexp-quote (test-interactions test))
              The test passed!
              Ran 1 test[.]
              0 tests passed[.]

              Check failures:\s*
                +Actual value 13 differs from 42, the expected value[.]\s*
              at line 10, column 0
              The test passed!
              Ran 1 test[.]
              0 tests passed[.]

              Check failures:\s*
                +check-expect encountered the following error instead of the expected value, 111[.]\s*
                +:: +at line 12, column 14 oops:\s*
              at line 12, column 0
              > }))
        ;; Includes the flattened test result snips.
        (define full-ints-text
          (send ints get-text (send ints paragraph-start-position 2) 'eof #t))
        (define passed?
          (regexp-match? re full-ints-text))
        (unless passed?
          (eprintf "FAILED line ~a: ~a\n  extra assertion expected: ~s\n\n  got: ~a\n"
                   (test-line test)
                   (test-definitions test)
                   re
                   full-ints-text))
        passed?))

(fire-up-drracket-and-run-tests run-test)

;; Test mode:
(module test racket/base
  (require racket/port syntax/location)
  (define-values (inp outp) (make-pipe))
  (define tee-error-port (open-output-bytes 'tee-stderr))
  (define stderr (current-error-port))
  (void
   (thread
    (λ () (copy-port inp tee-error-port stderr))))
  (exit-handler
   (let ([old-exit-hdlr (exit-handler)])
     (λ (code)
       (define stderr-content-length
         (bytes-length (get-output-bytes tee-error-port #t)))
       (cond
         [(and (zero? code) (> stderr-content-length 0))
          (write-string "non-empty stderr\n" stderr)
          (old-exit-hdlr 1)]
         [else
          (old-exit-hdlr code)]))))
  (putenv "PLTDRTEST" "yes")
  (eval-jit-enabled #f)
  (parameterize ([current-error-port outp])
    (dynamic-require (quote-module-path "..") #f))
  (module config info
    (define timeout 800)))
