;;; pr-48.ss

;;; tests font style changes to text after syntax check 

;;; Author: Paul Steckler

(require-library "function.ss")

(load-relative "drscheme-test-util.ss")

; a font description is a list

(define make-font-desc
  (lambda (slant weight uline)
    (list slant weight uline)))

(define slant car)
(define weight cadr)
(define uline caddr)

; the descriptions for the 5 different syntax items should be 
; distinct from one another

(define normal-font-desc
  (make-font-desc wx:const-normal wx:const-normal #f))

(define syn-font-desc
  (make-font-desc wx:const-normal wx:const-bold #t))

(define prim-font-desc
  (make-font-desc wx:const-slant wx:const-normal #f))

(define const-font-desc 
  (make-font-desc wx:const-normal wx:const-bold #f))

(define bound-var-font-desc
  (make-font-desc wx:const-slant wx:const-bold #t))

(define free-var-font-desc
  (make-font-desc wx:const-normal wx:const-normal #t))

; a problem is a syntax string and a list of font descriptions
; for each character in the string

(define problem
  (lambda (str descs)
    (list str descs)))
  
(let* ([drs-frame (wait-for-drscheme-frame)] 
       [definitions-edit (ivar drs-frame definitions-edit)]
       [get-font-cbs
	(lambda (lst)
	  (let ([get-cb-with-label
		 (lambda (label)
		   (car (memf (lambda (elt)
				(and (is-a? elt mred:check-box%)
				     (string=? (send elt get-label) label)))
			      lst)))])
	    (map get-cb-with-label '("Slant" "Bold" "Underline"))))]
       [set-check-boxes!
	(lambda (cbs desc)
	  (mred:test:set-check-box! (slant cbs)
			  (if (eq? (slant desc) wx:const-normal)
			      #f
			      #t))
	  (mred:test:set-check-box! (weight cbs)
			  (if (eq? (weight desc) wx:const-normal)
			      #f
			      #t))
	  (mred:test:set-check-box! (uline cbs) (uline desc)))]
       [set-syn-check-preferences!
	(lambda ()
	  (mred:test:menu-select "Edit" "Preferences...")
	  (let* ([frame 
		   (letrec ([loop 
			     (lambda () 
			       (let ([active (mred:test:get-active-frame)])
				 (if (or (eq? active #f)
					 (eq? active drs-frame))
				     (begin
				       (sleep 1/2)
				       (loop))
				     active)))])
		     (loop))]
		 [panel (send frame get-top-panel)]
		 [children (ivar panel children)]
		 [choice-box (car children)]
		 [choice-box-event 
		  (let ([event-obj 
			 (make-object wx:command-event% 
				      wx:const-event-type-choice-command)])
		  (send event-obj set-event-object choice-box)
		  event-obj)])
	    (send choice-box-event set-command-int 
		  (send choice-box find-string "Check Syntax"))
	    (send choice-box command choice-box-event)

	    (let* ([upper-panel (cadr children)]
		   [check-syntax-panel (send upper-panel active-child)]
		   [check-box-panels (ivar check-syntax-panel children)]

		   [syntax-panel (car check-box-panels)]
		   [syntax-check-boxes (get-font-cbs (ivar syntax-panel children))]

		   [primitive-panel (cadr check-box-panels)]
		   [primitive-check-boxes (get-font-cbs (ivar primitive-panel children))]

		   [constant-panel (caddr check-box-panels)]
		   [constant-check-boxes (get-font-cbs (ivar constant-panel children))]

		   [bound-var-panel (cadddr check-box-panels)]
		   [bound-var-check-boxes (get-font-cbs (ivar bound-var-panel children))]

		   [free-var-panel (car (cddddr check-box-panels))]
		   [free-var-check-boxes (get-font-cbs (ivar free-var-panel children))])

	      (for-each 

	       (lambda (p)
		 (set-check-boxes! (car p) (cadr p)))

	       (list 
		(list syntax-check-boxes syn-font-desc)
		(list primitive-check-boxes prim-font-desc)
		(list constant-check-boxes const-font-desc)
		(list bound-var-check-boxes bound-var-font-desc)
		(list free-var-check-boxes free-var-font-desc)))

	      (mred:test:button-push "OK"))))]
       [print-desc 
	(lambda (d)
	  (let ([slant 
		 (let ([slant-res (slant d)])
		   (cond
		    [(eq? slant-res wx:const-normal)
		     'normal-slant]
		    [(eq? slant-res wx:const-slant)
		     'slant]
		    [(eq? slant-res wx:const-italic)
		     'italic]
		    [else
		     'unknown]))]
		[weight 
		 (let ([weight-res (weight d)])
		   (cond
		    [(eq? weight-res wx:const-normal) 
		     'normal-weight]
		    [(eq? weight-res wx:const-light) 
		     'light]
		    [(eq? weight-res wx:const-bold) 
		     'bold]
		    [else 
		     'unknown]))]
		[uline (case (uline d)
			 [(#t) 'underline]
			 [(#f) 'no-underline]
			 [else (number->string (uline d))])])
	    (printf "~a/~a/~a~n" slant weight uline)))]
       [check-check-syntax-fonts
	(lambda (problem)
	  (letrec* 
	   ([str (car problem)]
	    [font-descs (cadr problem)]
	    [loop
	     (lambda (n descs)
	       (if (null? descs)
		   '()
		   (let* ([the-snip (send definitions-edit 
					  find-snip n wx:const-snip-after)]
			  [the-style (send the-snip get-style)]
			  [the-font (send the-style get-font)]
			  [exp-desc (car descs)]
			  [actual-desc 
			   (list (send the-font get-style)
				 (send the-font get-weight)  
				 (send the-font get-underlined))])
		     (if (equal? exp-desc actual-desc)
			 (loop (add1 n) (cdr descs))
			 (begin
			   (printf "*** Failed on input ~a ***~n" str)
			   (printf "At position ~a:~nExpected style: " n)
			   (print-desc exp-desc)
			   (printf "Actual style:   ")  
			   (print-desc actual-desc))))))])
	   (clear-definitions drs-frame)
	   (type-in-definitions drs-frame str)
	   (mred:test:button-push (ivar drs-frame check-syntax-button))
	   (loop 0 font-descs)))])

  ; set syntax-check font preferences in dialog

  (set-syn-check-preferences!)

  ; now run problems

  (wait-for-drscheme-frame)

  ; a problem is a pair: 
  ; the first element is a piece of syntax to check
  ; the second element is a list of font descriptions, 
  ;    one for each character in the syntax

  (for-each check-check-syntax-fonts 

	    (list
	    
	     (problem

	      "(or 1 2 3)"

	      (list 
	       normal-font-desc ; (
	       syn-font-desc    ; o
	       syn-font-desc    ; r
	       normal-font-desc ; _
	       const-font-desc  ; 1
	       normal-font-desc ; _
	       const-font-desc  ; 2
	       normal-font-desc ; _
	       const-font-desc  ; 3
	       normal-font-desc ; )
	      ))

	     (problem

	     "(and 1 2 3)"

	     (list 
	      normal-font-desc ; (
	      syn-font-desc    ; a
	      syn-font-desc    ; n
	      syn-font-desc    ; d
	      normal-font-desc ; _
	      const-font-desc  ; 1
	      normal-font-desc ; _
	      const-font-desc  ; 2
	      normal-font-desc ; _
	      const-font-desc  ; 3
	      normal-font-desc ; )
	      ))

	     (problem

	     "'(a b c)"

	     (list 
	      const-font-desc ; '
	      const-font-desc ; (
	      const-font-desc ; a
	      const-font-desc ; _
	      const-font-desc ; b
	      const-font-desc ; _
	      const-font-desc ; c
	      const-font-desc ; )

	      ))

	     (problem

	     "(quote x)"

	     (list 
	      const-font-desc ; (
	      const-font-desc ; q
	      const-font-desc ; u
	      const-font-desc ; o
	      const-font-desc ; t
	      const-font-desc ; e
	      const-font-desc ; _
	      const-font-desc ; x
	      const-font-desc ; )
	      ))

	     (problem

	     "(quasiquote x)"

	     (list 
	      const-font-desc ; (
	      const-font-desc ; q
	      const-font-desc ; u
	      const-font-desc ; a
	      const-font-desc ; s
	      const-font-desc ; i
	      const-font-desc ; q
	      const-font-desc ; u
	      const-font-desc ; o
	      const-font-desc ; t
	      const-font-desc ; e
	      const-font-desc ; _
	      const-font-desc ; x
	      const-font-desc ; )
	      ))

	     (problem

	      "#&a"

	      (list

	       const-font-desc ; #
	       const-font-desc ; &
	       const-font-desc ; a
	       ))

	     (problem

	      "#&\"hi\""

	      (list

	       const-font-desc ; #
	       const-font-desc ; &
	       const-font-desc ; "
	       const-font-desc ; h
	       const-font-desc ; i
	       const-font-desc ; "
	       ))

	     (problem

	      "#&2"

	      (list

	       const-font-desc ; #
	       const-font-desc ; &
	       const-font-desc ; 2
	       ))

	     (problem

	      "(define x 3)"

	      (list

	       normal-font-desc    ; (
	       syn-font-desc       ; d
	       syn-font-desc       ; e
	       syn-font-desc       ; f
	       syn-font-desc       ; i
	       syn-font-desc       ; n
	       syn-font-desc       ; e
	       normal-font-desc    ; _ 
	       bound-var-font-desc ; x
	       normal-font-desc    ; _
	       const-font-desc     ; 3
	       normal-font-desc    ; )
	       ))

	     (problem

	      "(local ([define x 3]) x)"

	      (list

	       normal-font-desc    ; (
	       syn-font-desc       ; l
	       syn-font-desc       ; o
	       syn-font-desc       ; c
	       syn-font-desc       ; a
	       syn-font-desc       ; l
	       normal-font-desc    ;  
	       normal-font-desc    ; (
	       normal-font-desc    ; [
	       syn-font-desc       ; d
	       syn-font-desc       ; e
	       syn-font-desc       ; f
	       syn-font-desc       ; i
	       syn-font-desc       ; n
	       syn-font-desc       ; e
	       normal-font-desc    ;  
	       bound-var-font-desc ; x
	       normal-font-desc    ;  
	       const-font-desc     ; 3
	       normal-font-desc    ; ]
	       normal-font-desc    ; )
	       const-font-desc     ;  
	       bound-var-font-desc ; x
	       normal-font-desc    ; )
	       ))

	     )))
