
(require-library "macro.ss")

(let ([c%
       (class-asi canvas%
	 (override
	   [on-event
	    (lambda (ev)
	      (printf "MOUSE ~a  meta: ~a  control: ~a  alt: ~a  shift: ~a buttons: ~a ~a ~a~n" 
		      (send ev get-event-type)
		      (send ev get-meta-down)
		      (send ev get-control-down)
		      (send ev get-alt-down)
		      (send ev get-shift-down)
		      (send ev get-left-down)
		      (send ev get-middle-down)
		      (send ev get-right-down)))]
	   [on-char
	    (lambda (ev)
	      (printf "KEY code: ~a  meta: ~a  control: ~a  alt: ~a  shift: ~a~n" 
		      (let ([v (send ev get-key-code)])
			(if (symbol? v)
			    v
			    (format "~a = ASCII ~a" v (char->integer v))))
		      (send ev get-meta-down)
		      (send ev get-control-down)
		      (send ev get-alt-down)
		      (send ev get-shift-down)))]))])
  (define f (make-object (class frame% ()
                           (inherit accept-drop-files)
                           (override
                             [on-drop-file (lambda (file)
                                             (printf "Dropped: ~a~n" file))])
                           (sequence
                             (super-init "tests" #f 100 100)
                             (accept-drop-files #t)))))
  (define c (make-object c% f))
  (send c focus)
  (send f show #t))

