
; For information about texpict, see texpicts.ss

(unit/sig 
 texpict^
 (import)

(define default-seg 5)
(define recordseplinespace 4)

(define using-pict2e-package
  (make-parameter #f
		  (lambda (x)
		    (and x #t))))

(define use-old-connect
  (make-parameter #f
		  (lambda (x)
		    (and x #t))))

(define output-measure-commands
  (make-parameter #t
		  (lambda (x)
		    (and x #t))))

(define draw-bezier-lines
  (make-parameter #f
		  (lambda (x)
		    (if (procedure? x)
			(begin
			  (unless (procedure-arity-includes? x 1)
			      (raise-type-error 'draw-bezier-lines
						"boolean or procedure of one argument"
						x))
			  x)
			(and x #t)))))

(define serialize-tex-picts
  (make-parameter #f
		  (lambda (x)
		    (and x #t))))

(define tex-series-prefix
  (make-parameter #f
		  (lambda (s)
		    (when s
		      (unless (string? s)
			(raise-type-error 'tex-series-prefix "string or #f" s)))
		    s)))

(define current-tex-sizer
  (make-parameter (lambda (t) #f)))

(define-struct pict (draw ; drawing instructions
		     width ; total width
		     height ; total height >= ascent + desecnt
		     ascent ; portion of height above top baseline
		     descent ; portion of height below bottom baseline
		     children)) ; list of child records
(define-struct child (pict dx dy))

(define blank 
  (case-lambda
   [() (blank 0 0 0)]
   [(s) (blank s s)]
   [(w h) (blank w h 0)]
   [(w a d) (make-pict `(picture ,w ,(+ a d)) w (+ a d) a d null)]))

(define (extend-pict box dx dy dw da dd draw)
  (let ([w (pict-width box)]
	[h (pict-height box)]
	[d (pict-descent box)]
	[a (pict-ascent box)])
    (make-pict (if draw draw (pict-draw box))
	       (+ w dw) (+ h da dd) 
	       (+ a da) (+ d dd) 
	       (list (make-child box dx dy)))))

(define (single-pict-offset pict subbox)
  (let floop ([box pict]
	      [found values]
	      [not-found (lambda () (error 'find-XX
					   "sub-pict: ~a not found in: ~a" 
					   subbox pict))])
    (if (eq? box subbox)
	(found 0 0)
	(let loop ([c (pict-children box)])
	  (if (null? c) 
	      (not-found)
	      (floop (child-pict (car c))
		     (lambda (dx dy)
		       (found (+ dx (child-dx (car c)))
			      (+ dy (child-dy (car c)))))
		     (lambda ()
		       (loop (cdr c)))))))))

(define (find-lb pict subbox-path)
  (if (pict? subbox-path)
      (single-pict-offset pict subbox-path)
      (let loop ([p pict][l subbox-path][dx 0][dy 0])
	(if (null? l)
	    (values dx dy)
	    (let-values ([(x y) (find-lb p (car l))])
	       (loop (car l) (cdr l) (+ x dx) (+ y dy)))))))

(define-values (find-lt
		find-lc
		find-ltl
		find-lbl
		find-ct
		find-cc
		find-cb
		find-ctl
		find-cbl
		find-rt
		find-rc
		find-rb
		find-rtl
		find-rbl)
  (let ([lb (lambda (x w d a) x)]
	[c (lambda (x w d a) (+ x (quotient w 2)))]
	[rt (lambda (x w d a) (+ x w))]
	[tline (lambda (x w d a) (+ x (- w a)))]
	[bline (lambda (x w d a) (+ x d))]
	[find (lambda (get-x get-y)
		(lambda (pict pict-path)
		  (let-values ([(dx dy) (find-lb pict pict-path)])
		     (let ([p (let loop ([path pict-path])
				(cond
				 [(pict? path) path]
				 [(null? (cdr path)) (loop (car path))]
				 [else (loop (cdr path))]))])
		       (values (get-x dx (pict-width p) 0 0)
			       (get-y dy (pict-height p) (pict-descent p) (pict-ascent p)))))))])
    (values (find lb rt)
	    (find lb c)
	    (find lb tline)
	    (find lb bline)
	    (find c rt)
	    (find c c)
	    (find c lb)
	    (find c tline)
	    (find c bline)
	    (find rt rt)
	    (find rt c)
	    (find rt lb)
	    (find rt tline)
	    (find rt bline))))

(define (launder box)
  (let ([b (extend-pict box 0 0 0 0 0 #f)])
    (set-pict-children! b null)
    b))

(define label-sizes null)
(define (extract-num s) ; strip off trainling `pt'
  (let ([str (symbol->string s)])
    (inexact->exact
     (ceiling
      (string->number (substring str 0 (- (string-length str) 2)))))))

(define (read-in-sizes file)
  (parameterize ([read-case-sensitive #t])
    (when (file-exists? file)
	  (set! label-sizes
		(append (with-input-from-file file
			  (lambda ()
			    (let loop ()
			      (let ([e (read)])
				(if (eof-object? e)
				    null
				    (let ([w (read)]
					  [h (read)]
					  [d (read)])
				      (cons (list e
						  (extract-num w) 
						  (extract-num h)
						  (extract-num d))
					    (loop))))))))
			label-sizes)))))

;; Marshall a tex string into a simple symbol
(define digits (make-vector 64))
(let loop ([i 0])
  (unless (= i 10)
	  (vector-set! digits i (integer->char (+ i (char->integer #\0))))
	  (loop (add1 i))))
(let loop ([i 0])
  (unless (= i 26)
	  (vector-set! digits (+ i 10) (integer->char (+ i (char->integer #\a))))
	  (vector-set! digits (+ i 36) (integer->char (+ i (char->integer #\A))))
	  (loop (add1 i))))
(vector-set! digits 62 #\-)
(vector-set! digits 63 #\+)
(define (number->base-64-string prefix n)
  (let loop ([n n][s null])
    (if (zero? n)
	(list->string (cons prefix s))
	(loop (arithmetic-shift n -6)
	      (cons (vector-ref digits (bitwise-and 63 n)) s)))))
(define serial-number 0)
(define (serialize s)
  (cond
   [(serialize-tex-picts)
    (set! serial-number (add1 serial-number))
    (format "~a.~a" serial-number s)]
   [(tex-series-prefix)
    (format "~a.~a" (tex-series-prefix) s)]
   [else s]))
(define (make-label s)
  (string->symbol
   (serialize
    (number->base-64-string
     #\T
     (let loop ([l (string->list s)][n 0])
       (if (null? l)
	   n
	   (loop (cdr l) (+ (arithmetic-shift n 7) (char->integer (car l))))))))))

(define tex
  (case-lambda
   [(t) (tex t 10 10)]
   [(t guess-width guess-height)
    (let* ([label (make-label t)]
	   [info (or (assq label label-sizes)
		     (let ([v ((current-tex-sizer) t)])
		       (and v
			    (cons label v))))]
	   [w (if info (cadr info) guess-width)]
	   [h (if info (caddr info) guess-height)]
	   [d (if info (cadddr info) guess-height)])
      (make-pict `(picture ,w ,(+ d h)
			   (put 0 ,d
				,(if (output-measure-commands)
				     (format "\\mztpMeasure{~a}{~a}"
					     t label)
				     t)))
		 w
		 (+ d h)
		 h d
		 null))]))

(define (text-line/phantom text phantom . args)
  (apply tex (format "\\makebox[0pt]{\\vphantom{~a}}~a" phantom text) args))

(define (text-line text . args)
  (apply text-line/phantom text "Xy" args))

(define (tex-no-descent . args)
  (clip-descent (apply tex args)))

(define tex-paragraph
  (case-lambda
   [(w str) (tex-paragraph w str 'top)]
   [(w str align)
    (tex (format "\\parbox[~a]{~apt}{~a}"
		 (case align
		   [(top) 't]
		   [(bottom) 'b]
		   [else (error 'tex-paragraph "bad alignment: ~a" align)])
		 w
		 str))]))

(define (clip-descent b)
  (let* ([w (pict-width b)]
	 [h (pict-height b)]
	 [d (pict-descent b)])
    (extend-pict
     b 0 (- d) 
     0 0 (- d)
     `(picture ,w ,(- h d)
	       (put 0 ,(- d) ,(pict-draw b))))))

(define (thickness mode b)
  (let* ([w (pict-width b)]
	 [h (pict-height b)])
    (extend-pict
     b 0 0 0 0 0
     `(picture ,w ,h
	       (thickness ,mode ,(pict-draw b))))))

(define (thick b) (thickness 'thicklines b))
(define (thin b) (thickness 'thinlines b))

(define delimit-str
  "\\hbox{$\\~a{\\hbox{$\\left~a\\rule{0pt}{~apt}\\right.$}}$}")

(define (mk-delimit left? middle? right? delim h)
  (let ([str (format delimit-str
		     (cond
		      [left? "mathopen"]
		      [right? "mathclose"]
		      [middle? "mathrel"])
		     delim
		     h)])
    (tex str 10 h)))

(define (left-delimit delim h)
  (mk-delimit #t #f #f delim h))
(define (middle-delimit delim h)
  (mk-delimit #f #t #f delim h))
(define (right-delimit delim h)
  (mk-delimit #f #f #t delim h))

(define (left-brace h)
  (left-delimit "\\{" h))
(define (right-brace h)
  (right-delimit "\\}" h))

(define (make-h-brace kind w)
  (tex (format "$\\~a{\\hbox{\\begin{picture}(~a,0)(0,0)\\end{picture}}}$"
	       kind w)))

(define (top-brace w)
  (make-h-brace "overbrace" w))
(define (bottom-brace w)
  (make-h-brace "underbrace" w))

(define inset
  (case-lambda
   [(box a) (inset box a a a a)]
   [(box h v) (inset box h v h v)]
   [(box l t r b)
    (let ([w (+ l r (pict-width box))]
	  [h (+ t b (pict-height box))])
      (extend-pict
       box l b
       (+ l r) t b
       `(picture
	 ,w ,h
	 (put ,l ,b ,(pict-draw box)))))]))

(define dash-frame 
  (case-lambda
   [(box) (dash-frame box default-seg)]
   [(box seg)
    (let ([w (pict-width box)]
	  [h (pict-height box)])
      (extend-pict
       box 0 0 0 0 0
       `(picture
	 ,w ,h
	 (put 0 0 ,(pict-draw box))
	 (put 0 0 ,(pict-draw (dash-hline w 0 seg)))
	 (put 0 ,h ,(pict-draw (dash-hline w 0 seg)))
	 (put 0 0 ,(pict-draw (dash-vline 0 h seg)))
	 (put ,w 0 ,(pict-draw (dash-vline 0 h seg))))))]))

(define (frame box)
  (dash-frame box (max (pict-width box) (pict-height box))))

(define (dash-line width height rotate seg)
  (let ([vpos (quotient height 2)])
    (make-pict
     `(picture
       ,@(rotate width height)
       ,@(if (>= seg width)
	     `((put ,@(rotate 0 vpos) (line ,@(rotate 1 0) ,width)))
	     (let* ([remain (remainder width (* 2 seg))]
		    [count (quotient width (* 2 seg))]
		    [lremain (quotient remain 2)]
		    [rremain (- remain lremain)])
	   `((put ,@(rotate 0 vpos) (line ,@(rotate 1 0) ,lremain))
	     ,@(let loop ([count count][pos lremain])
		 (if (zero? count)
		     null
		     (cons `(put ,@(rotate (+ pos seg) vpos) 
				 (line ,@(rotate 1 0) ,seg))
			   (loop (sub1 count) (+ pos seg seg)))))
	     (put ,@(rotate (- width rremain) vpos) 
		  (line ,@(rotate 1 0) ,rremain))))))
     (car (rotate width height))
     (cadr (rotate width height))
     (cadr (rotate 0 height)) 0
     null)))

(define (rlist b a) (list a b))

(define (hline width height)
  (dash-line width height list width))

(define (vline width height)
  (dash-line height width rlist height))

(define dash-hline
  (case-lambda 
   [(width height) (dash-hline width height default-seg)]
   [(width height seg) (dash-line width height list seg)]))

(define dash-vline
  (case-lambda 
   [(width height) (dash-vline width height default-seg)]
   [(width height seg) (dash-line height width rlist seg)]))

(define (oval box)
  (let ([w (pict-width box)]
	[h (pict-height box)])
    (extend-pict
     box 0 0 0 0 0
     `(picture
       ,w ,h
       (put 0 0 ,(pict-draw box))
       (put ,(quotient w 2) ,(quotient h 2) (oval "" ,w ,h))))))

(define (oval/radius box r)
  (let* ([w (pict-width box)]
	 [h (pict-height box)]
	 [rr (* 2 r)]
	 [lw (- w rr)]
	 [lh (- h rr)])
    (extend-pict
     box 0 0 0 0 0
     `(picture
       ,w ,h
       (put 0 0 ,(pict-draw box))
       (put ,r ,r (oval "[bl]" ,rr ,rr))
       (put ,r 0 (line 1 0 ,lw))
       (put ,(- w r) ,r (oval "[br]" ,rr ,rr))
       (put ,w ,r (line 0 1 ,lh))
       (put ,r ,(- h r) (oval "[tl]" ,rr ,rr))
       (put ,r ,h (line 1 0 ,lw))
       (put ,(- w r) ,(- h r) (oval "[tr]" ,rr ,rr))
       (put ,0 ,r (line 0 1 ,lh))))))

(define (big-circle d)
  (let ([r (quotient d 2)])
    (picture
     d d
     `((curve 0 ,r ,r 0 0 0)
       (curve ,r 0 ,d ,r ,d 0)
       (curve ,d ,r ,r ,d ,d ,d)
       (curve ,r ,d 0 ,r 0 ,d)))))

(define (ghost box)
  (let ([w (pict-width box)]
	[h (pict-height box)])
    (extend-pict
     box 0 0 0 0 0
     `(picture
       ,w ,h))))

(define-values (vl-append 
		vc-append 
		vr-append 
		ht-append
		hc-append
		hb-append
		htl-append
		hbl-append)
  (let ([make-append-boxes 
	 (lambda (wcomb hcomb fxoffset fyoffset rxoffset ryoffset 
			combine-ascent combine-descent)
	   (lambda (sep . args)
	     (unless (number? sep)
		     (raise-type-error 'XXX-append "number" sep))
	     (let append-boxes ([args args])
	       (cond
		[(null? args) (blank)]
		[(null? (cdr args)) (car args)]
		[else
		 (let* ([first (car args)]
			[rest (append-boxes (cdr args))]
			[w (wcomb (pict-width first) (pict-width rest) sep)]
			[h (hcomb (pict-height first) (pict-height rest) sep)]
			[fw (pict-width first)]
			[fh (pict-height first)]
			[rw (pict-width rest)]
			[rh (pict-height rest)]
			[fd1 (pict-ascent first)]
			[fd2 (pict-descent first)]
			[rd1 (pict-ascent rest)]
			[rd2 (pict-descent rest)]
			[dx1 (fxoffset fw fh rw rh sep fd1 fd2 rd1 rd2)]
			[dy1 (fyoffset fw fh rw rh sep fd1 fd2 rd1 rd2)]
			[dx2 (rxoffset fw fh rw rh sep fd1 fd2 rd1 rd2)]
			[dy2 (ryoffset fw fh rw rh sep fd1 fd2 rd1 rd2)])
		   (make-pict
		    `(picture 
		      ,w ,h
		      (put ,dx1
			   ,dy1
			   ,(pict-draw first))
		      (put ,dx2
			   ,dy2
			   ,(pict-draw rest)))
		    w h
		    (combine-ascent fd1 rd1 fd2 rd2 fh rh h)
		    (combine-descent fd2 rd2 fd1 rd1 fh rh h)
		    (list (make-child first dx1 dy1)
			  (make-child rest dx2 dy2))))]))))]
	[2max (lambda (a b c) (max a b))]
	[zero (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2) 0)]
	[fv (lambda (a b . args) a)]
	[sv (lambda (a b . args) b)]
	[min2 (lambda (a b . args) (min a b))]
	[max2 (lambda (a b . args) (max a b))]
	[min-ad (lambda (a b oa ob ah bh h)
		  (if (and (= ah (+ a oa))
			   (= bh (+ b ob)))
		      (- h (max oa ob))
		      (min a b)))])
    (values
     (make-append-boxes 2max + 
			zero (lambda (fw fh rw rh sep . a) (+ sep rh))
			zero zero 
			fv sv)
     (make-append-boxes 2max + 
			(lambda (fw fh rw rh sep . a) (quotient (- (max fw rw) fw) 2))
			(lambda (fw fh rw rh sep . a) (+ sep rh))
			(lambda (fw fh rw rh sep . a) (quotient (- (max fw rw) rw) 2))
			zero 
			fv sv)
     (make-append-boxes 2max + 
			(lambda (fw fh rw rh sep . a) (- (max fw rw) fw))
			(lambda (fw fh rw rh sep . a) (+ sep rh))
			(lambda (fw fh rw rh sep . a) (- (max fw rw) rw))
			zero 
			fv sv)
     (make-append-boxes + 2max
			zero
			(lambda (fw fh rw rh sep . a) (- (max fh rh) fh))
			(lambda (fw fh rw rh sep . a) (+ sep fw))
			(lambda (fw fh rw rh sep . a) (- (max fh rh) rh))
			max2 min2)
     (make-append-boxes + 2max
			zero
			(lambda (fw fh rw rh sep . a) (quotient (- (max fh rh) fh) 2))
			(lambda (fw fh rw rh sep . a) (+ sep fw))
			(lambda (fw fh rw rh sep . a) (quotient (- (max fh rh) rh) 2))
			min2 max2)
     (make-append-boxes + 2max 
			zero zero
			(lambda (fw fh rw rh sep . a) (+ sep fw)) zero
			min2 max2)
     (make-append-boxes + 2max
			zero
			(lambda (fw fh rw rh sep fd1 fd2 rd1 rd2) 
			  (- (max fh rh) fh (- (max fd1 rd1) fd1)))
			(lambda (fw fh rw rh sep . a) (+ sep fw))
			(lambda (fw fh rw rh sep fd1 fd2 rd1 rd2) 
			  (- (max fh rh) rh (- (max fd1 rd1) rd1)))
			max2 min-ad)
     (make-append-boxes + 2max
			zero
			(lambda (fw fh rw rh sep fd1 fd2 rd1 rd2) 
			  (- (max fd2 rd2) fd2))
			(lambda (fw fh rw rh sep . a) (+ sep fw))
			(lambda (fw fh rw rh sep fd1 fd2 rd1 rd2) 
			  (- (max fd2 rd2) rd2))
			min-ad max2))))

(define-values (lt-superimpose
		lb-superimpose
		lc-superimpose
		ltl-superimpose
		lbl-superimpose
		rt-superimpose
		rb-superimpose
		rc-superimpose
		rtl-superimpose
		rbl-superimpose
		ct-superimpose
		cb-superimpose
		cc-superimpose
		ctl-superimpose
		cbl-superimpose)
  (let ([make-superimpose 
	 (lambda (get-h get-v get-th)
	   (lambda boxes
	     (let ([max-w (apply max (map pict-width boxes))]
		   [max-h (apply max (map pict-height boxes))]
		   [max-a (apply max (map pict-ascent boxes))]
		   [max-a-complement (apply max (map (lambda (b) (- (pict-height b) (pict-ascent b)))
						     boxes))]
		   [max-d (apply max (map pict-descent boxes))]
		   [max-d-complement (apply max (map (lambda (b) (- (pict-height b) (pict-descent b)))
						     boxes))])
	       (picture max-w (get-th max-h max-a max-d max-a-complement max-d-complement)
			(map (lambda (box)
			       `(place ,(get-h max-w (pict-width box))
				       ,(get-v max-h (pict-height box)
					       max-d (pict-descent box)
					       max-a-complement)
				       ,box))
			     boxes)))))]
	[norm (lambda (h a d ac dc) h)]
	[tbase (lambda (h a d ac dc) (+ a ac))] 
	[bbase (lambda (h a d ac dc) (+ d dc))] 
	[lb (lambda (m v . rest) 0)]
	[rt (lambda (m v . rest) (- m v))]
	[tline (lambda (m v md d mac) mac)]
	[bline (lambda (m v md d mac) (- md d))]
	[c (lambda (m v . rest) (quotient (- m v) 2))])
    (values
     (make-superimpose lb rt norm)
     (make-superimpose lb lb norm)
     (make-superimpose lb c norm)
     (make-superimpose lb tline tbase)
     (make-superimpose lb bline bbase)
     (make-superimpose rt rt norm)
     (make-superimpose rt lb norm)
     (make-superimpose rt c norm)
     (make-superimpose rt tline tbase)
     (make-superimpose rt bline bbase)
     (make-superimpose c rt norm)
     (make-superimpose c lb norm)
     (make-superimpose c c norm)
     (make-superimpose c tline tbase)
     (make-superimpose c bline bbase))))

(define table
  (case-lambda
   [(ncol cells col-aligns row-aligns col-seps row-seps)
    (unless (positive? ncol)
	    (raise-type-error 'table "positive column count" ncol))
    (let ([count (length cells)])
      (unless (zero? (remainder count ncol))
	      (error 'table "cell count isn't divisble by the provided column count"))
      (let* ([w ncol]
	     [h (/ count w)]
	     [cells (let rloop ([r h][cells cells][r-acc null])
		      (if (zero? r)
			  (list->vector (reverse r-acc))
			  (let loop ([c w][cells cells][one-acc null])
			    (if (zero? c)
				(rloop (sub1 r) cells (cons (list->vector (reverse one-acc)) r-acc))
				(loop (sub1 c) (cdr cells) (cons (car cells) one-acc))))))]
	     [imp-list->vector (lambda (l n)
				 (let ([v (make-vector n)])
				   (let loop ([l l][p 0])
				     (unless (= n p)
				       (vector-set! v
						    p
						    (if (pair? l)
							(car l)
							l))
				       (loop (if (pair? l) (cdr l) l) (add1 p))))
				   v))]
	     [ralign (imp-list->vector row-aligns h)]
	     [calign (imp-list->vector col-aligns w)]
	     [rsep (imp-list->vector row-seps h)]
	     [csep (imp-list->vector col-seps w)]
	     [get-cell (lambda (c r) (vector-ref (vector-ref cells r) c))]
	     [nmap (lambda (f w)
		     (let loop ([n w][acc null])
		       (if (zero? n)
			   acc
			   (loop (sub1 n) (cons (f (sub1 n)) acc)))))]
	     [rowmap (lambda (f) (nmap f h))]
	     [colmap (lambda (f) (nmap f w))]
	     [superimposed-rows (list->vector
				 (rowmap (lambda (r)
					   (apply
					    (vector-ref ralign r)
					    (colmap (lambda (c) (get-cell c r)))))))]
	     [superimposed-cols (list->vector
				 (colmap (lambda (c)
					   (apply
					    (vector-ref calign c)
					    (rowmap (lambda (r) (get-cell c r)))))))])
	; No space after the last row/col
	(vector-set! rsep (sub1 h) 0)
	(vector-set! csep (sub1 w) 0)

	(apply
	 vl-append
	 0
	 (rowmap
	  (lambda (r)
	    (vl-append
	     0
	     (apply
	      ht-append
	      0
	      (colmap (lambda (c)
			(ht-append
			 0
			 (let* ([cell (get-cell c r)]
				[sc (vector-ref superimposed-cols c)]
				[sr (vector-ref superimposed-rows r)]
				[w (pict-width sc)]
				[h (pict-height sr)])
			   (let-values ([(x __) (find-lb sc cell)]
					[(_  y) (find-lb sr cell)])
			     (picture
			      w h
			      `((place ,x ,y ,cell)))))
			 (blank (vector-ref csep c) 0)))))
	     (blank 0 (vector-ref rsep r))))))))]))

(define (record title . fields)
  (let* ([totalwidth (apply max (pict-width title) (map pict-width fields))]
	 [linespace (if (null? fields) 0 recordseplinespace)]
	 [totalheight (+ (pict-height title) (apply + (map pict-height fields))
			 linespace)]
	 [title-y (- totalheight (pict-height title))]
	 [field-ys (let loop ([pos (- totalheight (pict-height title) linespace)]
			      [fields fields])
		     (if (null? fields)
			 null
			 (let* ([p (- pos (pict-height (car fields)))])
			   (cons p
				 (loop p (cdr fields))))))])
    (make-pict
     `(picture
       ,totalwidth ,totalheight
       (put 0 0 (line 1 0 ,totalwidth))
       (put 0 ,totalheight (line 1 0 ,totalwidth))
       (put 0 0 (line 0 1 ,totalheight))
       (put ,totalwidth 0 (line 0 1 ,totalheight))
       (put 0 ,title-y ,(pict-draw title))
       ,@(if (null? fields)
	     '()
	     `((put 0 ,(- totalheight (pict-height title) (quotient linespace 2))
		    (line 1 0 ,totalwidth))))
       ,@(map (lambda (f p) `(put 0 ,p ,(pict-draw f)))
	      fields field-ys))
     totalwidth totalheight
     totalheight 0
     (cons
      (make-child title 0 title-y)
      (map (lambda (child child-y) (make-child child 0 child-y)) fields field-ys)))))

(define (find-slope dh dv max-slope-num h-within v-within) ; max-slope-num is 4 or 6
  ; Result is (slope new-dh), where slope can be 'vertical, in which case
  ;                           new-dh is really dv
  (letrec ([best-of-two
	    (lambda (a b)
	      (let*-values ([(ls lh) (a)]
			    [(rs rh) (b)])
			   (if (and ls (or (not rs) (< (abs (- lh dh)) (abs (- rh dh)))))
			       (values ls lh)
			       (values rs rh))))]
	   [search-h
	    (lambda (dh dv depth direction)
	      (if (zero? depth)
		  (values #f #f)
		  (if (zero? dh)
		      (values 'vertical dv)
		      (let ([slope (/ dv dh)])
			(if (and (<= (abs (numerator slope)) max-slope-num)
				 (<= (abs (denominator slope)) max-slope-num))
			    (values slope dh)
			    (search-h (+ dh direction) dv (sub1 depth) direction))))))]
	   [sign (lambda (x) (if (positive? x) 1 -1))]
	   [flip
	    (lambda (s l)
	      (if s
		  (cond
		   [(eq? s 'vertical) (values (sign l) 0 (abs l))]
		   [(zero? s) (values 'vertical l)]
		   [else (values (/ 1 s) (round (* s l)))])
		  (values #f #f)))]
	   [search-v
	    (lambda (dh dv depth direction)
	      (call-with-values (lambda () (search-h dv dh depth direction))
				flip))]
	   [change-h
	    (lambda (dh dv h-within)
		(best-of-two (lambda () (search-h dh dv h-within -1))
			     (lambda () (search-h dh dv h-within 1))))]
	   [change-v
	    (lambda (dh dv v-within)
	      (call-with-values (lambda () (change-h dv dh v-within))
				flip))])
    (cond
     [(zero? v-within) (change-h dh dv h-within)]
     [(zero? h-within) (change-v dh dv v-within)]
     [else (let-values ([(s l) (search-h dh dv 1 0)])
	     (if s
		 (values s l)
		 (best-of-two 	 
		  (lambda ()
		    (best-of-two (lambda () (find-slope dh (add1 dv) max-slope-num h-within (sub1 v-within)))
				 (lambda () (find-slope dh (sub1 dv) max-slope-num h-within (sub1 v-within)))))
		  (lambda ()
		    (best-of-two (lambda () (find-slope (add1 dh) dv max-slope-num (sub1 h-within) v-within))
				 (lambda () (find-slope (sub1 dh) dv max-slope-num (sub1 h-within) v-within)))))))])))
	 
(define (parse-slope sl dh)
  (if (eq? sl 'vertical)
      (if (negative? dh)
	  (values 0 -1 (abs dh))
	  (values 0 1 dh))
      (let ([d (denominator sl)]
	    [n (numerator sl)])
	(if (negative? dh)
	    (values (- d) (- n) (abs dh))
	    (values d n dh)))))

(define connect 
  (case-lambda
   [(x1 y1 x2 y2) (connect x1 y1 x2 y2 #f)]
   [(x1 y1 x2 y2 arrow?)
    (if (not (or (use-old-connect) (draw-bezier-lines)))
	(~connect 'r +inf.0 x1 y1 x2 y2 arrow?)
	(let loop ([dd (if (draw-bezier-lines) 0 1)])
	  (if (> dd (if (draw-bezier-lines) 0 4))
	      ; give up
	      (if (draw-bezier-lines)
		  (let* ([get-len (lambda () (sqrt (+ (* (- x1 x2) (- x1 x2))
						      (* (- y1 y2)  (- y1 y2)))))]
			 [c (if (procedure? (draw-bezier-lines))
				((draw-bezier-lines) (get-len))
				#f)])
		    `((qbezier ,c ,x1 ,y1 ,(quotient (+ x1 x2) 2) ,(quotient (+ y1 y2) 2) ,x2 ,y2)))
		  (let ([xd (- x2 x1)])
		    `((put ,x1 ,y1 (line ,(if (negative? xd) -1 1) 0 ,(abs xd))))))
	      (let-values ([(s l) (find-slope (- x2 x1) (- y2 y1) 
					      (if (using-pict2e-package)
						  +inf.0
						  (if arrow? 4 6))
					      dd dd)])
		(if s
		    (let-values ([(lh lv ll) (parse-slope s l)])
		      `((put ,x1 ,y1 (,(if arrow? 'vector 'line) ,lh ,lv ,ll))))
		    (loop (add1 dd)))))))]))

(define ~connect 
  (case-lambda
   [(exact close-enough x1 y1 x2 y2) (~connect exact close-enough x1 y1 x2 y2 #f)]
   [(exact close-enough x1 y1 x2 y2 arrow?)
    (if (= x2 x1)
	; "infinite" slope
	(let ([dy (- y2 y1)])
	  `((put ,x1 ,y1 (,(if arrow? 'vector 'line) 0 ,(if (negative? dy) -1 1) ,(abs dy)))))
	(let ([real-slope (/ (- y2 y1) (- x2 x1))]
	      [split (lambda (xm ym)
		       (append
			(~connect exact close-enough xm ym x1 y1 #f)
			(~connect exact close-enough xm ym x2 y2 arrow?)))])
	  (if (or (>= real-slope (if arrow? 7/8 11/12))
		  (<= real-slope (if arrow? -7/8 -11/12)))
	      ; rounds to "infinite" slope
	      (if (> (abs (- x2 x1)) close-enough)
		  (split x1 (truncate (quotient (+ y1 y2) 2)))
		  (let ([dy (- y2 y1)])
		    `((put ,x1 ,y1 (,(if arrow? 'vector 'line) 
				    0 
				    ,(if (negative? dy) -1 1) ,(abs dy))))))
	      (let* ([slope (let loop ([slope real-slope][tolerances
							  (if arrow?
							      '(1/100 1/12 1/4)
							      '(1/100 1/50 1/25 1/10 1/6))])
			      (if (<= (denominator slope) (if arrow? 4 6))
				  slope
				  (loop (rationalize real-slope (car tolerances))
					(cdr tolerances))))]
		     [exact-x? (or (eq? exact 'x) (zero? slope))]
		     [r (sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))]
		     [dx (cond
			  [exact-x? (- x2 x1)]
			  [(eq? exact 'r) (truncate (* r (let ([d (denominator slope)]
							       [n (numerator slope)])
							   (/ d (sqrt (+ (* d d) (* n n)))))))]
			  [else (truncate (* (/ slope) (- y2 y1)))])]
		     [dy (truncate (* slope dx))])
		(if (or (and exact-x?
			     (> (abs (- dy (- y2 y1))) close-enough))
			(and (not exact-x?) (eq? exact 'y)
			     (> (abs (- dx (- x2 x1))) close-enough))
			(and (not exact-x?) (eq? exact 'y)
			     (> (abs (- (sqrt (+ (* dx dx) (* dy dy))) r)) close-enough)))
		    (if (or exact-x? (eq? exact 'r))
			(let ([xm (truncate (quotient (+ x1 x2) 2))]) 
			  (split xm (+ y1 (truncate (* slope (- xm x1))))))
			(let ([ym (truncate (quotient (+ y1 y2) 2))]) 
			  (split (+ x1 (truncate (* (/ slope) (- ym y1)))) ym)))
		    (let ([same-sign (lambda (v s)
				       (if (negative? s)
					   (- (abs v))
					   (abs v)))])
		      `((put ,x1 ,y1 (,(if arrow? 'vector 'line) 
				      ,(same-sign (denominator slope) (- x2 x1))
				      ,(same-sign (numerator slope) (- y2 y1))
				      ,(abs dx))))))))))]))

(define (picture w h commands)
  (let loop ([commands commands][translated null][children null])
    (if (null? commands)
	(make-pict
	 `(picture ,w ,h
		   ,@(reverse translated))
	 w h
	 h 0
	 children)
	(let ([c (car commands)]
	      [rest (cdr commands)])
	  (unless (and (pair? c) (symbol? (car c)))
		  (error 'picture "bad command: ~a" c))
	  (case (car c)
	    [(connect) (loop rest
			     (append (apply connect (cdr c))
				     translated)
			     children)]
	    [(dconnect) (loop rest
			      (let ([x (cadr c)]
				    [y (caddr c)]
				    [dx (cadddr c)]
				    [dy (list-ref c 4)])
				(append (connect x y (+ x dx) (+ y dy)
						 (if (null? (list-tail c 5))
						     #t
						     (list-ref c 5)))
					translated))
			      children)]
	    [(connect~y) (loop rest
			       (append (apply ~connect 'x (cdr c))
				       translated)
			       children)]
	    [(connect~x) (loop rest
			       (append (apply ~connect 'y (cdr c))
				       translated)
			       children)]
	    [(connect~xy) (loop rest
				(append (apply ~connect 'r (cdr c))
					translated)
				children)]
	    [(curve) (loop rest
			   (let ([x1 (cadr c)]
				 [y1 (caddr c)]
				 [x2 (cadddr c)]
				 [y2 (list-ref c 4)]
				 [xm (list-ref c 5)]
				 [ym (list-ref c 6)]
				 [d (if (null? (list-tail c 7))
					1.0
					(list-ref c 7))])
			     (let ([p (if (and d (>= d 0))
					  (inexact->exact (floor (* d (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))))
					  #f)])
			       (if (and (= x1 x2) (= y1 y2))
				   translated
				   (cons `(qbezier ,p ,x1 ,y1 ,xm ,ym ,x2 ,y2)
					 translated))))
			   children)]
	    [(place) (let ([x (cadr c)]
			   [y (caddr c)]
			   [p (cadddr c)])
		       (loop rest
			     (cons
			      `(put ,x ,y ,(pict-draw p))
			      translated)
			     (cons
			      (make-child p x y)
			      children)))]
	    [else (loop rest (cons c translated) children)])))))

(define (cons-picture p commands)
  (picture
   (pict-width p) (pict-height p)
   (cons
    `(place 0 0 ,p)
    commands)))

(define black-and-white
  (make-parameter #f
		  (lambda (x)
		    (and x #t))))

(define (colorize p color)
  (if (black-and-white)
      p
      (extend-pict 
       p 0 0 0 0 0
       `(color ,color ,(pict-draw p)))))

(define (optimize s)
  (let o-loop ([s s][dx 0][dy 0])
    (if (string? s)
	s
	(let ([tag (car s)])
	  (case tag
	    [(picture)
	     (list* 'picture (cadr s) (caddr s)
		    (map optimize (cdddr s)))]
	    [(color)
	     (let ([next (caddr s)])
	       (if (and (pair? next) (eq? (car next) 'color))
		   (optimize next)
		   (list* 'color (cadr s) 
			  (list 'put dx dy (optimize next)))))]
	    [(thickness)
	     (let ([t (cadr s)]
		   [p (caddr s)])
	       (list 'put dx dy 
		     (list 'thickness t 
			   (optimize p))))]
	    [(put)
	     (let ([x (cadr s)]
		   [y (caddr s)]
		   [next (cadddr s)])
	       (if (and (pair? next) (eq? (car next) 'picture))
		   ; optmize put-picture to just contents ...
		   (cons 'begin (map (lambda (s) (o-loop s (+ x dx) (+ y dy))) (cdddr next)))
		   ; normal
		   (list 'put (+ x dx) (+ y dy) (optimize next))))]
	    [(qbezier)
	     (let ([x1 (list-ref s 2)]
		   [y1 (list-ref s 3)]
		   [xm (list-ref s 4)]
		   [ym (list-ref s 5)]
		   [x2 (list-ref s 6)]
		   [y2 (list-ref s 7)]
		   [p (list-ref s 1)])
	       (list 'qbezier p
		     (+ x1 dx) (+ y1 dy)
		     (+ xm dx) (+ ym dy)
		     (+ x2 dx) (+ y2 dy)))]
	    [(frame)
	     (list 'frame (optimize (cadr s)))]
	    [(colorbox)
	     (list 'colorbox (cadr s) (optimize (caddr s)))]
	    [(line vector circle circle* make-box oval) s]
	    [else (error 'optimize "bad tag: ~s" tag)])))))

(define (fixup-top s)
  (cond
   [(and (pair? s) (eq? (car s) 'color))
    ;; Drop initial put
    (list* 'color (cadr s) (caddr (cdddr s)))]
   [(and (pair? s) (eq? (car s) 'put))
    ;; Wrap initial put (from thickness) in a pair of braces
    `(local ,(cadddr s))]
   [else
    ;; Do nothing
    s]))

(define (pict->string s)
  (let output ([s (fixup-top (optimize (pict-draw s)))])
    (if (string? s)
	s
	(let ([tag (car s)])
	  (case tag
	    [(local)
	     (format "{~a}~n" (output (cadr s)))]
	    [(begin)
	     (apply string-append (map output (cdr s)))]
	    [(picture)
	     (format "\\begin{picture}(~a,~a)~n~a\\end{picture}~n"
		     (cadr s) (caddr s)
		     (apply string-append (map output (cdddr s))))]
	    [(color)
	     (format "\\special{color push ~a}~n~a\\special{color pop}~n"
		     (cadr s) (output (cddr s)))]
	    [(thickness)
	     (format "\\~a~a" (cadr s) (output (caddr s)))]
	    [(put)
	     (format "\\put(~a,~a){~a}~n" (cadr s) (caddr s) (output (cadddr s)))]
	    [(qbezier)
	     (apply format "\\qbezier~a(~a,~a)(~a,~a)(~a,~a)~n"
		    (if (cadr s)
			(format "[~a]" (cadr s))
			"")
		    (cddr s))]
	    [(line vector)
	     (format "\\~a(~a,~a){~a}" tag (cadr s) (caddr s) (cadddr s))]
	    [(circle)
	     (format "\\circle{~a}" (cadr s))]
	    [(circle*)
	     (format "\\circle*{~a}" (cadr s))]
	    [(frame)
	     (format "\\frame{~a}" (output (cadr s)))]
	    [(colorbox)
	     (format "\\colorbox{~a}{~a}" (cadr s) (output (caddr s)))]
	    [(oval)
	     (format "\\oval(~a,~a)~a" (caddr s) (cadddr s) (cadr s))]
	    [(make-box)
	     (format "\\makebox(~a, ~a)[~a]{~a}"
		     (cadr s) (caddr s) (cadddr s) (car (cddddr s)))]
	    [else (error 'pict->string "bad tag: ~s" tag)])))))

(define (pict->commands s)
  (let output ([s (fixup-top (optimize (pict-draw s)))])
    (if (string? s)
	(list s)
	(let ([tag (car s)])
	  (case tag
	    [(local)
	     (output (cadr s))]
	    [(begin)
	     (apply append (map output (cdr s)))]
	    [(picture)
	     (apply append (map output (cdddr s)))]
	    [(color)
	     `((with-color ,(cadr s) ,(output (cddr s))))]
	    [(thickness)
	     `((with-thickness ,(cadr s) ,(output (caddr s))))]
	    [(put)
	     `((offset ,(cadr s) ,(caddr s) ,(output (cadddr s))))]
	    [(qbezier)
	     `((bezier ,@(cddr s)))]
	    [(line vector)
	     `((,tag ,(cadr s) ,(caddr s) ,(cadddr s)))]
	    [(circle circle*)
	     `((,tag ,(cadr s)))]
	    [(frame)
	     `((frame ,(output (cadr s))))]
	    [(colorbox)
	     `((colorbox ,(cadr s) ,(output (caddr s))))]
	    [(oval)
	     `((oval ,(caddr s) ,(cadddr s) ,(cadr s)))]
	    [(make-box)
	     `((make-box ,(cadr s) ,(caddr s) ,(cadddr s) ,(car (cddddr s))))]
	    [else (error 'pict->commands "bad tag: ~s" tag)])))))

)
