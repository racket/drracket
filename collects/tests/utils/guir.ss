(unit/sig test-utils:gui^
  (import mred^)
  
  ;;; find-labelled-window : (union ((union #f string) -> window<%>)
  ;;;                               ((union #f string) (union #f class) -> window<%>)
  ;;;                               ((union #f string) (union class #f) area-container<%> -> area-container<%>))
  ;;;;  may call error, if no control with the label is found
  (define find-labelled-window
    (case-lambda
     [(label) (find-labelled-window label #f)]
     [(label class) (find-labelled-window label class (get-top-level-focus-window))]
     [(label class window)
      (unless (or (not label)
                  (string? label))
	(error 'find-labelled-window "first argument must be a string or #f, got ~e; other args: ~e ~e"
	       label class window))
      (unless (or (class? class)
		  (not class))
	(error 'find-labelled-window "second argument must be a class or #f, got ~e; other args: ~e ~e"
	       class label window))
      (unless (is-a? window area-container<%>)
	(error 'find-labelled-window "third argument must be a area-container<%>, got ~e; other args: ~e ~e"
	       window label class))
      (let ([ans
	     (let loop ([window window])
	       (cond
                 [(and (or (not class)
                           (is-a? window class))
                       (let ([win-label (and (is-a? window window<%>)
                                             (send window get-label))])
                         (equal? label win-label)))
                  window]
                 [(is-a? window area-container<%>) (ormap loop (send window get-children))]
                 [else #f]))])
	(or ans
	    (error 'find-labelled-window "no window labelled ~e in ~e~a"
		   label
		   window 
		   (if class
		       (format " matching class ~e" class)
		       ""))))])))

