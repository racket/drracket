;;; pr-80.ss

;;; Create a frame with buggy callback in the definitions window.
;;; After invoking the callback, make sure the source text is properly highlighted.

(load-relative "drscheme-test-util.ss")

(letrec* ([_ (wait-for-drscheme-frame)]
	  [drscheme-frame (mred:test:get-active-frame)]
	  [eq-frame? (lambda () (eq? (mred:test:get-active-frame) drscheme-frame))]
	  [interactions-edit (ivar drscheme-frame interactions-edit)]
	  [interactions-canvas (ivar drscheme-frame interactions-canvas)]
	  [definitions-edit (ivar drscheme-frame definitions-edit)]
	  [definitions-canvas (ivar drscheme-frame definitions-canvas)]
	  [execute-button (ivar drscheme-frame execute-button)]
	  [code "(let* ([frame (make-object mred:frame% null \"MyFrame\" 100 100 200 300)]
                        [panel (make-object mred:vertical-panel% frame)]
                        [button (make-object mred:button% panel
			    (lambda (self event) 
                                (send frame show #f) (car 4))
			    \"Push me\")])
	           (send frame show #t))"])

	 (type-in-definitions drscheme-frame code)
	 (push-button-and-wait execute-button)

	 (printf "Code in callback should be highlighted~n"))


