;; need to use the namespace to transmit the
;; splash definitions so the timing of the loading
;; is right.

(when (getenv "MREDDEBUG")
  (parameterize ([current-eventspace (make-eventspace)])
    (let-values ([(sw sh) (get-display-size)])
      (let* ([stack-frame (make-object frame% "Debugging Window" #f 500)]
	     [button-panel (make-object horizontal-panel% stack-frame)]
	     [messages-panel (make-object vertical-panel% stack-frame)])


	(let* ([c (make-object canvas% button-panel)]
	       [quit-button (make-object button% "Quit" button-panel (lambda (x y) (exit)))]
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
	  (register-collecting-blit c 0 0 (send onb get-width) (send onb get-height) onb offb)
	  (send c min-width (send onb get-width))
	  (send c min-height (send onb get-height))
	  (send c stretchable-width #f)
	  (send c stretchable-height #f)
	  (send button-panel set-alignment 'center 'center)
	  (send quit-button stretchable-height #t)
	  (send quit-button stretchable-width #f))

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
	   (lambda (fn)
	     (set! stack (cons fn stack))
	     (update-messages)
	     (begin0 (ol fn)
		     (set! stack (cdr stack))
		     (update-messages))))))))

  (dynamic-require '(lib "errortrace.ss" "errortrace") #f)
  (error-print-width 200))

(require (lib "splash.ss" "framework"))

(define-values (get-dropped-files shutdown-splash close-splash)
  (splash
   (build-path (collection-path "icons") "plt.gif")
   "DrScheme"
   81))

(module drscheme mzscheme
  (require "private/link.ss")
  ((namespace-variable-binding 'shutdown-splash))
  (invoke-unit/sig drscheme@)
  ((namespace-variable-binding 'close-splash)))



