;peephole.scm
;SLaTeX Version 2.3
;Peephole adjuster used by the SLaTeX typesetter
;(c) Dorai Sitaram, Rice U., 1991, 1994

(eval-within slatex 

  (define slatex::get-line
    (let ((curr-notab &void-notab))
      (lambda (line)
        ;read the current tex line into "line";
        ;returns false on eof
        (let ((graphic-char-seen? #f))
          (let loop ((i 0))
            (let ((c (read-char *in*)))
              (cond (graphic-char-seen? (void))
                    ((or (eof-object? c)
                         (char=? c *return*)
                         (char=? c #\newline)
                         (char=? c #\space) (char=? c *tab*))
                     (void))
                    (else (set! graphic-char-seen? #t)))
              (cond
                ((eof-object? c)
                 (cond ((eq? curr-notab &mid-string)
                        (if (> i 0)
                            (setf (of line =notab / (- i 1)) &end-string)))
                       ((eq? curr-notab &mid-comment)
                        (set! curr-notab &void-notab))
                       ((eq? curr-notab &mid-math)
                        (error "get-line: Found eof inside math.")))
                 (setf (of line =char / i) #\newline)
                 (setf (of line =space / i) &void-space)
                 (setf (of line =tab / i) &void-tab)
                 (setf (of line =notab / i) &void-notab)
                 (setf (of line =rtedge) i)
                 (if (eq? (of line =notab / 0) &mid-string)
                     (setf (of line =notab / 0) &begin-string))
                 (if (= i 0) #f #t))
                ((or (char=? c *return*) (char=? c #\newline))
                 (if (and (memv slatex::*op-sys* '(dos windows os2 os2fat))
                          (char=? c *return*))
                     (if (char=? (peek-char *in*) #\newline)
                         (read-char *in*)))
                 (cond ((eq? curr-notab &mid-string)
                        (if (> i 0)
                            (setf (of line =notab / (- i 1)) &end-string)))
                       ((eq? curr-notab &mid-comment)
                        (set! curr-notab &void-notab))
                       ((eq? curr-notab &mid-math)
                        (error "get-line: Sorry, you can't split ~
                          math formulas across lines in Scheme code.")))
                 (setf (of line =char / i) #\newline)
                 (setf (of line =space / i) &void-space)
                 (setf (of line =tab / i)
                   (cond ((eof-object? (peek-char *in*)) &plain-crg-ret)
                         (*intext?* &plain-crg-ret)
                         (else &tabbed-crg-ret)))
                 (setf (of line =notab / i) &void-notab)
                 (setf (of line =rtedge) i)
                 (if (eq? (of line =notab / 0) &mid-string)
                     (setf (of line =notab / 0) &begin-string))
                 #t)
                ((eq? curr-notab &mid-comment)
                 (setf (of line =char / i) c)
                 (setf (of line =space / i)
                   (cond ((char=? c #\space) &plain-space)
                         ((char=? c *tab*) &plain-space)
                         (else &void-space)))
                 (setf (of line =tab / i) &void-tab)
                 (setf (of line =notab / i) &mid-comment)
                 (loop (+ i 1)))
                ((char=? c #\\)
                 (setf (of line =char / i) c)
                 (setf (of line =space / i) &void-space)
                 (setf (of line =tab / i) &void-tab)
                 (setf (of line =notab / i) curr-notab)
                 (let ((i+1 (+ i 1)) (c+1 (read-char *in*)))
                   (if (char=? c+1 *tab*) (set! c+1 #\space))
                   (setf (of line =char / i+1) c+1)
                   (setf (of line =space / i+1)
                     (if (char=? c+1 #\space) &plain-space
                         &void-space))
                   (setf (of line =tab / i+1) &void-tab)
                   (setf (of line =notab / i+1) curr-notab)
                   (loop (+ i+1 1))))
                ((eq? curr-notab &mid-math)
                 (if (char=? c *tab*) (set! c #\space))
                 (setf (of line =space / i)
                   (if (char=? c #\space) &plain-space
                       &void-space))
                 (setf (of line =tab / i) &void-tab)
                 (cond ((memv c *math-triggerers*)
                        (setf (of line =char / i) #\$)
                        (setf (of line =notab / i) &end-math)
                        (setf curr-notab &void-notab))
                       (else (setf (of line =char / i) c)
                         (setf (of line =notab / i) &mid-math)))
                 (loop (+ i 1)))
                ((eq? curr-notab &mid-string)
                 (if (char=? c *tab*) (set! c #\space))
                 ;or should tab and space be treated differently?
                 (setf (of line =char / i) c)
                 (setf (of line =space / i)
                   (if (char=? c #\space) &inner-space &void-space))
                 (setf (of line =tab / i) &void-tab)
                 (setf (of line =notab / i)
                   (cond ((char=? c #\")
                          (set! curr-notab &void-notab)
                          &end-string)
                         (else &mid-string)))
                 (loop (+ i 1)))
                ;henceforth curr-notab is &void-notab
                ((char=? c #\space)
                 (setf (of line =char / i) c)
                 (setf (of line =space / i)
                   (cond (*intext?* &plain-space)
                         (graphic-char-seen? &inner-space)
                         (else &init-space)))
                 (setf (of line =tab / i) &void-tab)
                 (setf (of line =notab / i) &void-notab)
                 (loop (+ i 1)))
                ((char=? c *tab*)
                 (let loop1 ((i i) (j 0))
                   (if (< j 8)
                       (begin
                         (setf (of line =char / i) #\space)
                         (setf (of line =space / i)
                           (cond (*intext?* &plain-space)
                                 (graphic-char-seen? &inner-space)
                                 (else &init-space)))
                         (setf (of line =tab / i) &void-tab)
                         (setf (of line =notab / i) &void-notab)
                         (loop1 (+ i 1) (+ j 1)))))
                 (loop (+ i 8)))
                ((char=? c #\")
                 (setf (of line =char / i) c)
                 (setf (of line =space / i) &void-space)
                 (setf (of line =tab / i) &void-tab)
                 (setf (of line =notab / i) &begin-string)
                 (set! curr-notab &mid-string)
                 (loop (+ i 1)))
                ((char=? c #\;)
                 (setf (of line =char / i) c)
                 (setf (of line =space / i) &void-space)
                 (setf (of line =tab / i) &void-tab)
                 (setf (of line =notab / i) &begin-comment)
                 (set! curr-notab &mid-comment)
                 (loop (+ i 1)))
                ((memv c *math-triggerers*)
                 (setf (of line =char / i) #\$)
                 (setf (of line =space / i) &void-space)
                 (setf (of line =tab / i) &void-tab)
                 (setf (of line =notab / i) &begin-math)
                 (set! curr-notab &mid-math)
                 (loop (+ i 1)))
                (else (setf (of line =char / i) c)
                  (setf (of line =space / i) &void-space)
                  (setf (of line =tab / i) &void-tab)
                  (setf (of line =notab / i) &void-notab)
                  (loop (+ i 1))))))))))

  (define slatex::peephole-adjust
    (lambda (curr prev)
      ;adjust the tabbing information on the current line curr and
      ;its previous line prev relative to each other
      (if (or (slatex::blank-line? curr)
              (slatex::flush-comment-line? curr))
	  (if (not *latex-paragraph-mode?*)
	      (begin
                (set! *latex-paragraph-mode?* #t)
		(if (not *intext?*)
		    (begin
                      (slatex::remove-some-tabs prev 0)
		      (let ((prev-rtedge (of prev =rtedge)))
			(if (eq? (of prev =tab / prev-rtedge) &tabbed-crg-ret)
			    (setf (of prev =tab / (of prev =rtedge))
				  &plain-crg-ret)))))))
	  (begin
            (if *latex-paragraph-mode?*
                (set! *latex-paragraph-mode?* #f)
                (if (not *intext?*)
                    (let ((remove-tabs-from #f))
                      (let loop ((i 0))
                        (cond
                         ((char=? (of curr =char / i) #\newline)
                          (set! remove-tabs-from i))
                         ((char=? (of prev =char / i) #\newline)
                          (set! remove-tabs-from #f))
                         ((eq? (of curr =space / i) &init-space)
                          ;eating initial space of curr
                          (if (eq? (of prev =notab / i) &void-notab)
                              (begin
				(cond
				 ((or (char=? (of prev =char / i) #\()
                                      (eq? (of prev =space / i) &paren-space))
				  (setf (of curr =space / i) &paren-space))
				 ((or (char=? (of prev =char / i) #\[)
                                      (eq? (of prev =space / i) &bracket-space))
				  (setf (of curr =space / i) &bracket-space))
				 ((or (memv (of prev =char / i) '(#\' #\` #\,))
                                      (eq? (of prev =space / i) &quote-space))
				  (setf (of curr =space / i) &quote-space)))
                                (if (memq (of prev =tab / i)
					  (list &set-tab &move-tab))
                                    (setf (of curr =tab / i) &move-tab))))
                          (loop (+ i 1)))
                         ;finished tackling &init-spaces of curr
                         ((= i 0) ;curr starts left-flush
                          (set! remove-tabs-from 0))
                         ;at this stage, curr[notab,i]
                         ;is either #f or a &begin-comment/string
                         ((not (eq? (of prev =tab / i) &void-tab))
                          ;curr starts with nice alignment with prev
                          (set! remove-tabs-from (+ i 1))
                          (if (memq (of prev =tab / i)
				    (list &set-tab &move-tab))
                              (setf (of curr =tab / i) &move-tab)))
                         ((memq (of prev =space / i)
				(list &init-space &init-plain-space
				      &paren-space &bracket-space
				      &quote-space))
                          ;curr starts while prev is still empty
                          (set! remove-tabs-from (+ i 1)))
                         ((and (char=? (of prev =char / (- i 1)) #\space)
			       (eq? (of prev =notab / (- i 1)) &void-notab))
                          ;curr can induce new alignment straightaway
                          (set! remove-tabs-from (+ i 1))
                          (setf (of prev =tab / i) &set-tab)
                          (setf (of curr =tab / i) &move-tab))
                         (else ;curr stakes its &move-tab (modulo parens/bkts)
                          ;and induces prev to have corresp &set-tab
                          (set! remove-tabs-from (+ i 1))
                          (let loop1 ((j (- i 1)))
                            (cond ((<= j 0) 'exit-loop1)
                                  ((not (eq? (of curr =tab / j) &void-tab))
                                   'exit-loop1)
                                  ((memq (of curr =space / j)
					 (list &paren-space &bracket-space
					       &quote-space))
                                   (loop1 (- j 1)))
                                  ((or (not (eq? (of prev =notab / j)
                                                 &void-notab))
				       (char=? (of prev =char / j) #\space))
                                   (let ((k (+ j 1)))
                                     (if (not (memq (of prev =notab / k)
						    (list &mid-comment
							  &mid-math &end-math
							  &mid-string
							  &end-string)))
                                         (begin
					   (if (eq? (of prev =tab / k)
						    &void-tab)
					       (setf (of prev =tab / k)
						     &set-tab))
                                           (setf (of curr =tab / k)
						 &move-tab)))))
                                  (else 'anything-else?)
                                  )))))
                      (remove-some-tabs prev remove-tabs-from))))
	    (if (not *intext?*) (slatex::add-some-tabs curr))
	    (slatex::clean-init-spaces curr)
	    (slatex::clean-inner-spaces curr)))))

  (define slatex::add-some-tabs
    (lambda (line)
      ;add some tabs in the body of line "line" so the next line
      ;can exploit them
      (let loop ((i 1) (succ-parens? #f))
	(let ((c (of line =char / i)))
	  (cond ((char=? c #\newline) 'exit-loop)
		((not (eq? (of line =notab / i) &void-notab))
		 (loop (+ i 1) #f))
		((char=? c #\[)
		 (if (eq? (of line =tab / i) &void-tab)
		     (setf (of line =tab / i) &set-tab))
		 (loop (+ i 1) #f))
		((char=? c #\()
		 (if (eq? (of line =tab / i) &void-tab)
		     (if (not succ-parens?)
			 (setf (of line =tab / i) &set-tab)))
		 (loop (+ i 1) #t))
		(else (loop (+ i 1) #f)))))))

  (define slatex::remove-some-tabs
    (lambda (line i)
      ;remove useless tabs on line "line" after index i
      (if i
	  (let loop ((i i))
	    (cond ((char=? (of line =char / i) #\newline) 'exit)
		  ((eq? (of line =tab / i) &set-tab)
		   (setf (of line =tab / i) &void-tab)
		   (loop (+ i 1)))
		  (else (loop (+ i 1))))))))

  (define slatex::clean-init-spaces
    (lambda (line)
      ;remove init-spaces on line "line" because
      ;tabs make them defunct
      (let loop ((i (of line =rtedge)))
	(cond ((< i 0) 'exit-loop)
	      ((eq? (of line =tab / i) &move-tab)
	       (let loop1 ((i (- i 1)))
		 (cond ((< i 0) 'exit-loop1)
		       ((memq (of line =space / i)
			      (list &init-space &paren-space &bracket-space
				    &quote-space))
			(setf (of line =space / i) &init-plain-space)
			(loop1 (- i 1)))
		       (else (loop1 (- i 1))))))
	      (else (loop (- i 1)))))))

  (define slatex::clean-inner-spaces
    (lambda (line)
      ;remove single inner spaces in line "line" since
      ;paragraph mode takes care of them
      (let loop ((i 0) (succ-inner-spaces? #f))
	(cond ((char=? (of line =char / i) #\newline) 'exit-loop)
	      ((eq? (of line =space / i) &inner-space)
	       (if (not succ-inner-spaces?)
		   (setf (of line =space / i) &plain-space))
	       (loop (+ i 1) #t))
	      (else (loop (+ i 1) #f))))))

  (define slatex::blank-line?
    (lambda (line)
      ;check if line "line" is blank
      (let loop ((i 0))
	(let ((c (of line =char / i)))
	  (cond ((char=? c #\space)
		 (if (eq? (of line =notab / i) &void-notab)
		     (loop (+ i 1)) #f))
		((char=? c #\newline)
		 (let loop1 ((j (- i 1)))
		   (if (not (<= j 0))
		       (begin
                         (setf (of line =space / i) &void-space)
			 (loop1 (- j 1)))))
		 #t)
		(else #f))))))

  (define slatex::flush-comment-line?
    (lambda (line)
      ;check if line "line" is one with ; in the leftmost column
      (and (char=? (of line =char / 0) #\;)
	   (eq? (of line =notab / 0) &begin-comment)
	   (not (char=? (of line =char / 1) #\;)))))

  (define slatex::do-all-lines
    (lambda ()
      ;process all lines, adjusting each adjacent pair
      (let loop ((line1 *line1*) (line2 *line2*))
	(let* ((line2-paragraph? *latex-paragraph-mode?*)
	       (more? (get-line line1)))
	  ;
	  (peephole-adjust line1 line2)
	  ;
	  (funcall (if line2-paragraph?
		       (function slatex::display-tex-line)
		       (function slatex::display-scm-line)) line2)
	  ;
	  (if (not (eq? line2-paragraph? *latex-paragraph-mode?*))
	      (funcall (if *latex-paragraph-mode?*
			   (function display-end-sequence)
			   (function display-begin-sequence)) *out*))
	  ;
	  (if more? (loop line2 line1))))))

  ;scheme2tex is the "interface" procedure supplied by this file --
  ;it takes Scheme code from inport and produces LaTeX source for same
  ;in outport

  (define slatex::scheme2tex
    (lambda (inport outport)
      ;create a typeset version of scheme code from inport
      ;in outport;
      ;local setting of keywords, etc.?
      (set! *in* inport)
      (set! *out* outport)
      (set! *latex-paragraph-mode?* #t)
      (set! *in-qtd-tkn* #f)
      (set! *in-bktd-qtd-exp* 0)
      (set! *in-mac-tkn* #f)
      (set! *in-bktd-mac-exp* 0)
      (set! *case-stack* '())
      (set! *bq-stack* '())
      (let ((flush-line ;needed anywhere else?
	     (lambda (line)
	       (setf (of line =rtedge) 0)
	       (setf (of line =char / 0) #\newline)
	       (setf (of line =space / 0) &void-space)
	       (setf (of line =tab / 0) &void-tab)
	       (setf (of line =notab / 0) &void-notab))))
	(funcall flush-line *line1*)
	(funcall flush-line *line2*))
      (do-all-lines)))
  )
