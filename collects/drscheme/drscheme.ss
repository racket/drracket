;; need to use the namespace to transmit the
;; splash definitions so the timing of the loading
;; is right.

(when (getenv "MREDDEBUG")
  (parameterize ([current-eventspace (make-eventspace)])
    (let* ([f (make-object frame% "Quit")]
	   [c (make-object canvas% f)]
           [h (make-object horizontal-panel% f)]
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
      (send (make-object button% "Quit" h (lambda (x y) (exit))) stretchable-width #t)
      (make-object grow-box-spacer-pane% h)
      (send f reflow-container)
      (let-values ([(w h) (get-display-size)])
        (send f move 
              (- w (send f get-width))
              (- h (send f get-height))))
      (send f show #t)))
  (dynamic-require '(lib "errortrace.ss" "errortrace") #f)
  (error-print-width 200)
  (current-load (let ([ol (current-load)]) (lambda (x) (printf "~a~n" x) (ol x)))))

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



