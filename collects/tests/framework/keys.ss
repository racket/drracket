(begin-elaboration-time (current-load-relative-directory 
			 "Cupertino:robby:plt:collects:tests:framework")
			(printf "3 curr-dir ~a curr-load-dir ~a~n"
				(current-directory)
				(current-load-relative-directory)))

(include "key-specs.ss")

(define (test-key key-spec)
  (let* ([keys ((case (system-type)
		  [(macos) key-spec-macos]
		  [(unix) key-spec-unix]
		  [(windows) key-spec-windows])
		key-spec)]
	 [before (key-spec-before key-spec)]
	 [after (key-spec-after key-spec)]
	 [process-key
	  (lambda (key)
	    (let ([text-expect (buff-spec-string after)]
		  [start-expect (buff-spec-start after)]
		  [end-expect (buff-spec-end after)])
	      (test key
		    (lambda (x) (equal? x (vector text-expect start-expect end-expect)))
		    `(let* ([text (send (get-top-level-focus-window) get-editor)])
		       (send text erase)
		       (send text insert ,(buff-spec-string before))
		       (send text set-position ,(buff-spec-start before) ,(buff-spec-end before))
		       (test:keystroke ',(car key) ',(cdr key))
		       (vector (send text get-text)
			       (send text get-start-position)
			       (send text get-end-position))))))])
    (for-each process-key keys)))

(send-sexp-to-mred `(send (make-object frame:basic% "dummy to trick frame group") show #t))
(wait-for-frame "dummy to trick frame group")

(define (test-specs frame-name frame-class specs)
  (send-sexp-to-mred `(send (make-object ,frame-class ,frame-name) show #t))
  (wait-for-frame frame-name)
  (for-each test-key specs)
  (send-sexp-to-mred `(test:close-frame (get-top-level-focus-window))))

(test-specs "global keybingings test" 'frame:text% global-specs)
(test-specs "scheme mode keybindings test" 
	    '(class frame:editor% (name)
	       (override
		[get-editor%
		 (lambda ()
		   (scheme:text-mixin text:basic%))])
	       (sequence (super-init name)))
	    scheme-specs)
