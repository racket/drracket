(define drs-frame (wait-for-drscheme-frame))
(define interactions-text (ivar drs-frame interactions-text))

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

(define (test-bad/load-teachpack tp-exp expected-error)
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  (let ([tp-name (normal-case-path
		  (normalize-path
		   (build-path
		    (collection-path "tests" "drscheme")
		    "teachpack-tmp.ss")))])
    (call-with-output-file tp-name
      (lambda (port) (display tp-exp port))
      'truncate)
    (use-get/put-dialog
     (lambda ()
       (fw:test:menu-select "Language" "Add Teachpack..."))
     tp-name)
    (let ([dialog
	   (with-handlers ([(lambda (x) #t)
			    (lambda (x) #f)])
	     (wait-for-new-frame drs-frame))])
      (cond
       [dialog
	(let ([got (send dialog get-message)])
	  (unless (string=? got expected-error)
	    (printf "FAILED:       tp: ~s~n        expected: ~s~n             got: ~s~n"
		    tp-exp expected-error got))
	  (fw:test:button-push "Ok")
          (wait-for-new-frame dialog))]
       [else
	(printf "FAILED: no error message appeared~n              tp: ~s~n        expected: ~s~n"
		tp-exp expected-error)]))))

(define (test-bad/execute-teachpack tp-exp expected)
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  (let ([tp-name (normal-case-path
		  (normalize-path
		   (build-path
		    (collection-path "tests" "drscheme")
		    "teachpack-tmp.ss")))])
    (call-with-output-file tp-name
      (lambda (port) (display tp-exp port))
      'truncate)
    (use-get/put-dialog
     (lambda ()
       (fw:test:menu-select "Language" "Add Teachpack..."))
     tp-name)
    (do-execute drs-frame #f)
    (let ([dialog
	   (with-handlers ([(lambda (x) #t)
			    (lambda (x) #f)])
	     (let ([wait-for-error-pred
		    (lambda ()
		      (let ([active
			     (or
			      (get-top-level-focus-window)
			      (and (ivar interactions-text user-eventspace)
				   (parameterize ([current-eventspace
						   (ivar interactions-text user-eventspace)])
				     (get-top-level-focus-window))))])
			(if (and active (not (eq? active drs-frame)))
			    active
			    #f)))])
	       (poll-until wait-for-error-pred)))])
      (cond
       [dialog
	(let ([got (send dialog get-message)]
              [expected-error
               (string-append (format "Invalid Teachpack: ~a~n" tp-name)
                              expected)])
	  (unless (string=? got expected-error)
	    (printf "FAILED:       tp: ~s~n        expected: ~s~n             got: ~s~n"
		    tp-exp expected-error got))
	  (fw:test:button-push "Ok")
          (wait-for-new-frame dialog))]
       [else
	(printf "FAILED: no error message appeared~n              tp: ~s~n        expected: ~s~n"
		tp-exp error)]))))

(define (generic-tests)
  (test-good-teachpack
   "(unit/sig () (import plt:userspace^))"
   "1"
   "1")
  
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
   "\"not-firsts-original-defn\"")
  
  (test-good-teachpack
   "(cons (unit/sig () (import plt:userspace^)) (unit/sig () (import plt:userspace^)))"
   "1"
   "1")

  (test-good-teachpack
   (format
    "(cons (unit/sig () (import plt:userspace^)) ~
   ~n      (unit/sig (m) 
   ~n        (import plt:userspace^)
   ~n        (define m (lambda (x) `(+ ,x 1)))))")
   "m"
   "keyword: invalid use of keyword m")

  (test-good-teachpack
   (format
    "(cons (unit/sig () (import plt:userspace^)) ~
    ~n      (unit/sig (m) 
    ~n        (import plt:userspace^)
    ~n        (define m (lambda (x) `(+ ,x 1)))))")
   "(m 1)"
   "2")

  (test-good-teachpack
   (format
    "(cons (unit/sig () (import plt:userspace^))~
   ~n      (unit/sig (m) ~
   ~n        (import plt:userspace^)~
   ~n        (define m (lambda (x) `(+ ,x 1)))))")
   "(define (f x) (m x)) (m 1)"
   "2")

  (test-good-teachpack
   (format
    "(cons (unit/sig (frc) (import plt:userspace^) (define (frc x) (x)))~
 ~n      (unit/sig (dly) 
 ~n       (import plt:userspace^)
 ~n       (define dly (lambda (x) `(lambda () ,x)))))")
   "(frc (dly 11))"
   "11")

  (test-good-teachpack
   (format
    "(cons (unit/sig (mo) (import plt:userspace^) (define (mo x) (ivar (make-object x) x)))~
 ~n      (unit/sig (m) 
 ~n       (import plt:userspace^)
 ~n       (define m (lambda (x) `(class object% () (public [x ,x]) (sequence (super-init)))))))")
   "(mo (m 11))"
   "11")

  (test-good-teachpack
   (get-string-from-file
    (build-path (collection-path "tests" "drscheme")
		"force-delay-teachpack.scm"))
   "(force (delay 1))"
   "1"))

(define (good-tests)
  (set-language-level! "Graphical (MrEd)")
  (generic-tests)

  (set-language-level! "Beginning Student")
  (generic-tests)

  (test-good-teachpack
   "(unit/sig (car) (import [p : plt:userspace^]) (define car (list \"not-cars-original-defn\")))"
   "(first car)"
   "\"not-cars-original-defn\"")

  ;; re-defined an old primitive
  (test-good-teachpack
   "(unit/sig (display) (import plt:userspace^) (define (display x) x))"
   "(display 1)"
   "1"))
  
(define (bad-tests)
  (set-language-level! "Beginning Student")

  (test-bad/load-teachpack
   "undefined-id"
   "reference to undefined identifier: undefined-id")

  (test-bad/load-teachpack
   "1"
   "loading Teachpack file does not result in a either a unit/sig or a pair of unit/sigs, got: 1")

  (test-bad/execute-teachpack
   "(unit/sig () (import))"
   "global-define-values/invoke-unit/sig: invoke unit imports 0 units, but 1 units were provided")

  (test-bad/execute-teachpack
   "(unit/sig () (import plt:userspace^) (car 1))"
   "car: expects argument of type <pair>; given 1"))

(define (get-string-from-file fn)
  (call-with-input-file fn
    (lambda (port)
      (apply string-append
	     (let loop ()
	       (let ([l (read-line port)])
		 (if (eof-object? l)
		     null
		     (list* l " " (loop)))))))
    'text))

(define (test-built-in-teachpacks)
  (clear-definitions drs-frame)
  (type-in-definitions drs-frame "1")
  (let* ([test-teachpack
          (lambda (dir)
            (lambda (teachpack)
              (when (or (equal? "ss" (filename-extension teachpack))
                        (equal? "scm" (filename-extension teachpack)))
		(printf "  testing ~a~n" teachpack)
		(let ([filename (normal-case-path (build-path dir teachpack))])
		  (fw:test:menu-select "Language" "Clear All Teachpacks")
		  (use-get/put-dialog
		   (lambda ()
		     (fw:test:menu-select "Language" "Add Teachpack..."))
		   filename)
		  (do-execute drs-frame)
		  
		  (let ([got (fetch-output drs-frame)]
			[expected (format "Teachpack: ~a.~n1" filename)])
		    (unless (equal? got expected)
		      (printf "FAILED built in teachpack test: ~a~n" filename)
		      (printf "       got: ~s~n  expected: ~s~n" got expected)))))))]
         [test-teachpacks
          (lambda (dir)
            (for-each (test-teachpack dir) (directory-list dir)))]
         [teachpack-dir (normalize-path (build-path (collection-path "mzlib") 'up 'up "teachpack"))])
    (set-language-level! "Beginning Student")
    (test-teachpacks teachpack-dir)
    (test-teachpacks (build-path teachpack-dir "htdp"))))
      
(bad-tests)
(good-tests)
(test-built-in-teachpacks)