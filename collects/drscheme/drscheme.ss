(module drscheme mzscheme

  ;; except for the mred and class libraries
  ;; (which should already be in this namespace anyway)
  ;; this module only has dynamic-requires to
  ;; get the timing right of the debug display, etc.
  
  (require (lib "mred.ss" "mred")
	   (lib "class.ss"))


  (when (getenv "MREDDEBUG")
    (let ([main-eventspace-thread (current-thread)])
      (parameterize ([current-eventspace (make-eventspace)])
	(let-values ([(sw sh) (get-display-size)])
	  (let* ([stack-frame (make-object frame% "Debugging Window" #f 500)]
		 [button-panel (make-object horizontal-panel% stack-frame)]
		 [messages-panel (make-object vertical-panel% stack-frame)])
	    
	    
	    (let* ([mem (make-object message% "000.000.000" button-panel)]
		   [c (make-object canvas% button-panel)]
		   [quit-button (make-object button% "Quit" button-panel (lambda (x y) (exit)))]
		   [break-button (make-object button% "Break" button-panel 
					      (lambda (x y) (break-thread main-eventspace-thread)))]
		   [onb (make-object bitmap% (build-path (collection-path "icons")
							 "recycle.gif"))]
		   [offb
		    (let ([bdc (make-object bitmap-dc%)]
			  [bitmap (make-object bitmap%
				    (send onb get-width)
				    (send onb get-height))])
		      (send bdc set-bitmap bitmap)
		      (send bdc clear)
		      (send bdc set-bitmap #f)
		      bitmap)])
	      
	      (thread
	       (lambda ()
		 (let loop ()
		   (sleep 1)
		   (let* ([mem-usage (current-memory-use)]
			  [spacer 1000]
			  [pad (lambda (x)
				 (cond
				   [(x . < . 10) (format "00~a" x)]
				   [(x . < . 100) (format "0~a" x)]
				   [else (number->string x)]))]
			  [f1 (pad (modulo mem-usage spacer))]
			  [f2 (pad (modulo (quotient mem-usage spacer) spacer))]
			  [f3 (pad (quotient (quotient mem-usage spacer) spacer))])
		     (send mem set-label (string-append f3 "." f2 "." f1)))
		   (loop))))
	      
	      (register-collecting-blit c 0 0 (send onb get-width) (send onb get-height) onb offb)
	      (send c min-width (send onb get-width))
	      (send c min-height (send onb get-height))
	      (send c stretchable-width #f)
	      (send c stretchable-height #f)
	      (send button-panel set-alignment 'center 'center)
	      (send quit-button stretchable-height #t)
	      (send break-button stretchable-height #t))
	    
	    (let* ([new-message
		    (lambda ()
		      (let ([m (make-object message% "" messages-panel)])
			(send m stretchable-width #t)
			m))]
		   [messages (list (new-message)
				   (new-message)
				   (new-message)
				   (new-message)
				   (new-message)
				   (new-message)
				   (new-message)
				   (new-message))]
		   [stack null]
		   [ol (current-load)]
		   [update-messages
		    (lambda ()
		      (let ([new-messages-needed (- (length stack) (length messages))])
			(when (new-messages-needed . > . 0)
			  (let loop ([n new-messages-needed])
			    (unless (zero? n)
			      (let ([m (new-message)])
				(set! messages (append messages (list m)))
				(loop (- n 1)))))))
		      (let loop ([stack (reverse stack)]
				 [messages messages])
			(cond
			  [(null? messages) (void)]
			  [(null? stack) 
			   (for-each (lambda (m)
				       (unless (equal? "" (send m get-label))
					 (send m set-label "")))
				     messages)]
			  [else
			   (let ([msg (car messages)]
				 [fn (car stack)])
			     (unless (string=? (send msg get-label) fn)
			       (send msg set-label fn)))
			   (loop (cdr stack) (cdr messages))])))])
	      (send stack-frame reflow-container)
	      (send stack-frame move 
		    (- sw (send stack-frame get-width))
		    0)
	      (send stack-frame show #t)
	      (current-load
	       (lambda (fn module)
		 (set! stack (cons fn stack))
		 (update-messages)
		 (begin0 (ol fn module)
			 (set! stack (cdr stack))
			 (update-messages)))))))))

    (dynamic-require '(lib "errortrace.ss" "errortrace") #f)
    (error-print-width 200))

  ((dynamic-require '(lib "splash.ss" "framework") 'start-splash)
   (build-path (collection-path "icons") 
               (if ((get-display-depth) . <= . 8)
                   "plt-flat.gif"
                   "plt.jpg"))
   "DrScheme"
   99)

  (dynamic-require '(lib "start.ss" "drscheme" "private") #f))
