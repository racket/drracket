(unit/sig drscheme:interface^
  (import [aries : plt:aries^]
          [zodiac : zodiac:system^])
  
  (define zodiac-phase #f)
  (define (set-zodiac-phase sym)
    (unless (or (not sym)
		(memq sym '(reader expander)))
      (error 'set-zodiac-phase "unknown phase: ~a~n" sym))
    (set! zodiac-phase sym))

  (define-struct (exn:zodiac-syntax struct:exn:syntax) (link-tag))
  (define-struct (exn:zodiac-read struct:exn:read) (link-tag))

  ;; init-substring? : string string -> boolean
  ;; calculates if sub is an initial substring of str
  (define (init-substring? sub str)
    (and (>= (string-length str)
	     (string-length sub))
	 (string=? (substring str 0 (string-length sub))
		   sub)))

  ;; dispatch-report : string zodiac:zodiac -> ALPHA 
  ;; escapes
  (define (dispatch-report string object link-tag)
    (raise
     (with-continuation-mark 
      aries:w-c-m-key
      (aries:make-zodiac-mark object)
	(case zodiac-phase
	  [(expander)
	   (make-exn:zodiac-syntax string
				   (current-continuation-marks)
				   #f
				   link-tag)]
	  [(reader)
	   (make-exn:zodiac-read
	    string (current-continuation-marks) #f link-tag)]
	  [else (make-exn:user string (current-continuation-marks))]))))
  
  ;; report-error : symbol -> (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define report-error
    (lambda (type)
      (lambda (link-text link-tag z s . args)
	(let ([string (apply format
			     (if (eq? type 'internal)
				 (string-append "Internal error: "
						link-text
						": "
						s)
				 (string-append link-text ": " s))
			     args)])
	  (cond
	    [(zodiac:zodiac? z)
	     (dispatch-report string z link-tag)]
	    [(zodiac:eof? z)
	     (dispatch-report
	      string
	      (zodiac:make-zodiac 'origin
				  (zodiac:eof-location z)
				  (zodiac:eof-location z))
	      link-tag)]
	    [(zodiac:period? z)
	     (dispatch-report
	      string
	      (zodiac:make-zodiac 'origin
				  (zodiac:period-location z)
				  (zodiac:period-location z))
	      link-tag)]
	    [else ((error-display-handler)
		   (format "internal-error.report-error: ~a: ~a" z string))])))))
  
  ;; static-error : (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define static-error (report-error 'static))

  ;; internal-error : (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define internal-error (report-error 'internal)))
