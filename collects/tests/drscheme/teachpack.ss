(define drs-frame (wait-for-drscheme-frame))

(define (test-good-teachpack tp-exps dr-exp expected)
  (clear-definitions drs-frame)
  (type-in-definitions drs-frame dr-exp)
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  
  (let ([tp-names
         (let ([teachpack-path (normal-case-path
                                (normalize-path
                                 (collection-path "tests" "drscheme")))])
           (let loop ([tp-exps (if (pair? tp-exps)
                                   tp-exps
                                   (list tp-exps))]
                      [n 0])
             (cond
               [(null? tp-exps) null]
               [else
                (let ([tp-name (build-path teachpack-path (format "teachpack-tmp~a.ss" n))])
                  (call-with-output-file tp-name
                    (lambda (port) (display (car tp-exps) port))
                    'truncate)
                  (use-get/put-dialog
                   (lambda ()
                     (fw:test:menu-select "Language" "Add Teachpack..."))
                   tp-name)
                  (cons tp-name (loop (cdr tp-exps) (- n 1))))])))])
    (do-execute drs-frame)
    (let ([got (fetch-output drs-frame)]
          [full-expectation 
           (string-append
            (apply string-append (map (lambda (x) (format "Teachpack: ~a.~n" x)) tp-names))
            expected)])
      (unless (equal? got 
                      full-expectation)
        (printf "FAILED:       tp: ~s~n             exp: ~s~n        expected: ~s~n             got: ~s~n"
                tp-exps
                dr-exp full-expectation got)))))

(define (test-bad-teachpack tp-exps dr-exp expected-error)
  (clear-definitions drs-frame)
  (clear-definitions drs-frame)
  (type-in-definitions drs-frame dr-exp)
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  
  (let ([tp-names
         (let ([teachpack-path (normal-case-path
                                (normalize-path
                                 (collection-path "tests" "drscheme")))])
           (let loop ([tp-exps (if (pair? tp-exps)
                                   tp-exps
                                   (list tp-exps))]
                      [n 0])
             (cond
               [(null? tp-exps) null]
               [else
                (let ([tp-name (build-path teachpack-path (format "teachpack-tmp~a.ss" n))])
                  (call-with-output-file tp-name
                    (lambda (port) (display (car tp-exps) port))
                    'truncate)
                  (use-get/put-dialog
                   (lambda ()
                     (fw:test:menu-select "Language" "Add Teachpack..."))
                   tp-name)
                  (cons tp-name (loop (cdr tp-exps) (- n 1))))])))])
    (void)))
  
(define (generic-tests)
  (test-good-teachpack
   "(unit/sig (not-a-primitive) (import plt:userspace^) (define not-a-primitive 1))"
   "not-a-primitive"
   "1")
  
  (test-good-teachpack
   (list "(unit/sig (not-a-primitive1) (import plt:userspace^) (define not-a-primitive1 1))"
         "(unit/sig (not-a-primitive2) (import plt:userspace^) (define not-a-primitive2 1))")
   "(+ not-a-primitive1 not-a-primitive2)"
   "2")
  
  (test-good-teachpack
   "(unit/sig (first) (import [p : plt:userspace^]) (define first \"not-firsts-original-defn\"))"
   "first"
   "\"not-firsts-original-defn\""))

(set-language-level! "Graphical (MrEd)")
(generic-tests)

(set-language-level! "Beginning Student")
(generic-tests)
  
(test-good-teachpack
 "(unit/sig (car) (import [p : plt:userspace^]) (define car (list \"not-cars-original-defn\")))"
 "(first car)"
 "\"not-cars-original-defn\"")
