(local [(define pref-file (build-path (find-system-path 'pref-dir)
				      (case (system-type)
					[(macos) "MrEd Preferences"]
					[(windows) "mred.pre"]
					[(unix) ".mred.prefs"]
					[else (error 'prefs.ss "unknown os: ~a~n" (system-type))])))
	(define old-prefs (if (file-exists? pref-file)
			      (call-with-input-file pref-file read)
			      null))
	(define (check-eq? s) (lambda (t) (eq? s t)))
	(define pref-sym 'framework:test-suite)]

  (call-with-output-file pref-file
    (lambda (port) (write (filter (lambda (x) (not (eq? (car x) pref-sym)))
				  old-prefs)
			  port))
    'truncate)
  (shutdown-mred)

  (test
   'preference-unbound
   (check-eq? 'passed)
   `(with-handlers ([exn:unknown-preference?
		     (lambda (x)
		       'passed)])
      (preferences:get ',pref-sym)))
  (test 'preference-set-default/get
	(check-eq? 'passed)
	`(begin (preferences:set-default ',pref-sym 'passed symbol?)
		(preferences:get ',pref-sym)))
  (test 'preference-set/get
	(check-eq? 'new-pref)
	`(begin (preferences:set ',pref-sym 'new-pref)
		(preferences:get ',pref-sym)))
  (with-handlers ([eof-result? (lambda (x) (void))])
    (send-sexp-to-mred '(begin (preferences:set 'framework:verify-exit #f) (exit:exit))))

  (test 'preference-get-after-restart
	(check-eq? 'new-pref)
	`(begin (preferences:set-default ',pref-sym 'passed symbol?)
		(preferences:get ',pref-sym))))


(test 'dialog-appears
      (lambda (x) (eq? 'passed x))
      (lambda ()
	(send-sexp-to-mred '(preferences:show-dialog))
	(wait-for-frame "Preferences")
	(send-sexp-to-mred '(begin (preferences:hide-dialog)
				   (let ([f (get-top-level-focus-window)])
				     (if f
					 (if (string=? "Preferences" (send f get-label))
					     'failed
					     'passed)
					 'passed))))))
