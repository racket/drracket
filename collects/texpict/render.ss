
(define (parse-string s f)
  (cond
   [(regexp-match "^{\\\\bf (.*)}$" s)
    => (lambda (m)
	 (parse-string (cadr m)
		       (send the-font-list find-or-create-font
			     (send f get-point-size)
			     (send f get-family)
			     (send f get-style)
			     'bold)))]
   [(regexp-match "^{\\\\it (.*)}$" s)
    => (lambda (m)
	 (parse-string (cadr m)
		       (send the-font-list find-or-create-font
			     (send f get-point-size)
			     (send f get-family)
			     'italic
			     (send f get-weight))))]
   [else (values s f)]))

(define (set-dc-for-text-size dc)
  (output-measure-commands #f)
  (draw-bezier-lines #t)
  (current-tex-sizer
   (lambda (s)
     (let-values ([(s f) (parse-string s (send dc get-font))])
       (let-values ([(w h d a) (send dc get-text-extent s f)])
	 (list w (- h d) d))))))

(define (draw-pict dc p dx dy)
  
  (define (render dc w h l dx dy)
    (define b&w? #f)
    (define straight? #f)
    (define draw-line (ivar dc draw-line))
    (define draw-spline (ivar dc draw-spline))
    (define get-pen (ivar dc get-pen))
    (define get-brush (ivar dc get-brush))
    (define set-pen (ivar dc set-pen))
    (define set-brush (ivar dc set-brush))
    (define find-or-create-pen (ivar the-pen-list find-or-create-pen))
    (define find-or-create-brush (ivar the-brush-list find-or-create-brush))
    (set-brush (find-or-create-brush "black" 'solid))
    (let loop ([dx dx][dy dy][l l][color "black"])
      (unless (null? l)
	(let ([x (car l)])
	  (if (string? x)
	      (let-values ([(tw th td ta) (send dc get-text-extent x)]
			   [(c) (send dc get-text-foreground)]
			   [(f) (send dc get-font)])
		(let-values ([(x f2) (parse-string x f)])
		  (send dc set-font f2)
		  (send dc set-text-foreground (make-object color% color))
		  (send dc draw-text x dx (- h dy (- th td)))
		  (send dc set-text-foreground c)
		  (send dc set-font f)))
	      (case (car x)
		[(offset) (loop (+ dx (cadr x))
				(+ dy (caddr x))
				(cadddr x)
				color)]
		[(line vector)
		 (let ([xs (cadr x)]
		       [ys (caddr x)]
		       [len (cadddr x)])
		   (draw-line 
		    dx (- h dy)
		    (+ dx (* xs len)) (- h (+ dy (* ys len)))))]
		[(circle circle*)
		 (let ([size (cadr x)])
		   (send dc draw-ellipse 
			 dx (- h dy size)
			 size size))]
		[(oval)
		 (let ([b (get-brush)])
		   (set-brush (find-or-create-brush "BLACK" 'transparent))
		   (send dc draw-rounded-rectangle
			 (- dx (/ (cadr x) 2))
			 (- h dy (/ (caddr x) 2))
			 (cadr x) (caddr x)
			 -0.2)
		   (set-brush b))]
		[(bezier)
		 (if straight?
		     (draw-line (+ dx (list-ref x 1))
				(- h (+ dy (list-ref x 2)))
				(+ dx (list-ref x 5))
				(- h (+ dy (list-ref x 6))))
		     (draw-spline (+ dx (list-ref x 1))
				  (- h (+ dy (list-ref x 2)))
				  (+ dx (list-ref x 3))
				  (- h (+ dy (list-ref x 4)))
				  (+ dx (list-ref x 5))
				  (- h (+ dy (list-ref x 6)))))]
		[(with-color)
		 (if b&w?
		     (loop dx dy (caddr x) color)
		     (let ([p (get-pen)]
			   [b (get-brush)])
		       (set-pen (find-or-create-pen (cadr x) (send p get-width) 'solid))
		       (set-brush (find-or-create-brush (cadr x) 'solid))
		       (loop dx dy (caddr x) (cadr x))
		       (set-pen p)
		       (set-brush b)))]
		[(with-thickness)
		 (let ([p (get-pen)])
		   (set-pen (find-or-create-pen (send p get-color) 
						(if (eq? (cadr x) 'thicklines)
						    1
						    0)
						'solid))
		   (loop dx dy (caddr x) color)
		   (set-pen p))]
		[(prog)
		 ((cadr x) dc dx (- h dy))]
		[else (error 'rander "unknown command: ~a~n" x)])))
	(loop dx dy (cdr l) color))))

  (render dc (pict-width p) (pict-height p)
	  (pict->commands p) 
	  dx dy))
