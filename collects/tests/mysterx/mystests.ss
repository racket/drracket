;;; mystests.ss -- test suite for MysterX

(require-library "mysterx.ss" "mysterx")

(define wb (make-object mx-browser% "MysTest" 230 250))
(define doc (send wb current-document))

(define ctrl (send doc insert-object "TestControl Class" 95 95 'percent))

(define (inv f . args) (apply com-invoke ctrl f args))

(define errors? #f)

(define tests
	`(("AddTest" (39 ,(box 420)) ,(+ 39 420))
	  ("AddTest" (420 ,(box 39)) ,(+ 420 39))
	  ("FloatTest" (4.7 5.2) ,(- 5.2 4.7))
	  ("FloatTest" (88.7 33.2) ,(- 33.2 88.7))
	  ("FloatTest" (-88.7 33.2) ,(- 33.2 -88.7))
	  ("StringTest" ("abc" "def") ,"abcdef")
	  ("StringTest" ("Supercali" "fragilistic") ,"Supercalifragilistic")
          ("ShortTest"  (42 17) ,(* 42 17))
          ("ShortTest"  (77 -22) ,(* 77 -22))))

(for-each 
 (lambda (t)
   (let ([got (apply inv (car t) (cadr t))]
	 [expected (caddr t)])
     (unless (equal? got expected)
	     (set! errors? #t)
	     (printf "Expected: ~a~nGot     : ~a~n"
		     expected got))))
 tests)

(define caption "SomeCaption")

(com-set-property! ctrl "Caption" caption)

(unless (string=? caption (com-get-property ctrl "Caption"))
	(set! errors? #t))

(when errors?
      (printf "There were errors!~n"))

(define (make-mousefun s)
  (let ([t (string-append s ": button = ~a shift = ~a x = ~a y = ~a~n")])
    (lambda (button shift x y) 
      (printf t button shift x y))))

(define (mouse-pair s)
  (list s (make-mousefun s)))
  
(unless errors?
	(for-each 
	 (lambda (sf) 
	   (com-register-event-handler ctrl (car sf) (cadr sf)))
	 `(("Click"
	    ,(lambda () (printf "Click~n")))
	   ,(mouse-pair "MouseMove")
	   ,(mouse-pair "MouseDown")
	   ,(mouse-pair "MouseUp")))

	(printf "Try clicking and moving the mouse over the object~n")
	(printf "You should see Click, MouseMove, MouseDown, and MouseUp events~n"))

	   
         
	
	


