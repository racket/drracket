(unit/sig drscheme:interface^
  (import [aries : plt:aries^]
          [zodiac : zodiac:system^])
  
  (define zodiac-phase #f)
  (define (set-zodiac-phase sym)
    (unless (or (not sym)
		(memq sym '(reader expander)))
      (error 'set-zodiac-phase "unknown phase: ~a~n" sym))
    (set! zodiac-phase sym))

  ;; dispatch-report : string zodiac:zodiac -> ALPHA 
  ;; escapes
  (define dispatch-report
    (lambda (string object)
      (raise
       (with-continuation-mark 
	aries:w-c-m-key 
        object
	(case zodiac-phase
	  [(expander) (make-exn:syntax string (current-continuation-marks) #f)]
	  [(reader) (make-exn:read string (current-continuation-marks) #f)]
	  [else (make-exn:user string (current-continuation-marks))])))))
  
  ;; report-error : symbol -> (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define report-error
    (lambda (type)
      (lambda (z s . args)
	(let ([string (apply format (if (eq? type 'internal)
					(string-append "report error.Internal error: "
						       s)
					s)
			     args)])
	  (cond
	    [(zodiac:zodiac? z) (dispatch-report string z)]
	    [(zodiac:eof? z) (dispatch-report string (zodiac:make-zodiac 'origin
									 (zodiac:eof-location z)
									 (zodiac:eof-location z)))]
	    [(zodiac:period? z) (dispatch-report string (zodiac:make-zodiac 'origin
									    (zodiac:period-location z)
									    (zodiac:period-location z)))]
	    [else ((error-display-handler) (format "internal-error.report-error: ~a: ~a" z string))])))))
  
  ;; static-error : (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define static-error (report-error 'static))

  ;; dynamic-error : (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define dynamic-error (report-error 'dynamic))

  ;; internal-error : (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define internal-error (report-error 'internal)))
