(unit/sig quasiquote:quotester^
  (import
    quasiquote:graphical-interface^
    (url : mzlib:url^))

  (define-struct entity (name))
  (define-struct (stock struct:entity) ())
  (define-struct (fund struct:entity) ())

  (define get-chart
    (lambda (entity)
      (define base-directory-for-stocks "/sm/pg/")
      ;; Rule: append <capital initial of entity>/<entity>.gif
      (define base-directory-for-funds "/sm/trmfg/")
      ;; Rule: append <capital initial of entity>/<entity>.gif
      (define handle-processing
	(lambda (base-dir)
	  (let ((s (entity-name entity)))
	    (display-image-stream
	      (url:get-pure-port
		(url:make-url "http" "www.stockmaster.com" #f
		  (string-append base-dir "/"
		    (string (string-ref s 0))
		    "/" s ".gif")
		  #f #f #f))
	      s))))
      (cond
	((stock? entity)
	  (handle-processing base-directory-for-stocks))
	((fund? entity)
	  (handle-processing base-directory-for-funds))
	(else
	  (error 'get-chart
	    "~s is not a stock or fund" entity)))))

  ;; http://www.stocksmart.com/ows-bin/owa/sq.returnPrice?symbol=<SYMBOL>
  ;; (regexp "<TD ALIGN=\"RIGHT\">\\$(.+)</TD>")
  ;; no longer works -- advantage is it provided ratios instead of decimals

  ;; http://quote.yahoo.com/q?s=<SYMBOL>&d=v1
  ;; provides some quotes as ratios -- hence the second regexp

  (define extract-quote-amount
    (let ((quote-pattern (regexp "<td nowrap><b>(.+)</b></td>"))
	   (ratio-pattern (regexp "<sup>([0-9]+)</sup>/<sub>([0-9]+)</sub>")))
      (lambda (port symbol)
	(let loop ()
	  (let ((line (read-line port)))
	    (if (eof-object? line)
	      (error 'get-quote
		"No quote found for ~s" (entity-name symbol))
	      (let ((matched (regexp-match quote-pattern line)))
		(if matched
		  (let ((value
			  (let (($string (cadr matched)))
			    (let ((p (open-input-string $string)))
			      (let loop ((sum 0))
				(let ((r (read p)))
				  (if (eof-object? r)
				    sum
				    (loop (+ (if (number? r)
					       r
					       (let ((ratio-matched
						       (regexp-match
							 ratio-pattern
							 (symbol->string r))))
						 (if ratio-matched
						   (/ (string->number
							(cadr ratio-matched))
						     (string->number
						       (caddr ratio-matched)))
						   (error 'get-quote
						     "Unrecognized quote ~s"
						     r))))
					    sum)))))))))
		    ;; out of courtesy to the server, we'll read it all
		    (let finish-loop ()
		      (let ((line (read-line port)))
			(unless (eof-object? line)
			  (finish-loop))))
		    value)
		  (loop)))))))))

  (define get-quote
    (lambda (symbol)
      (extract-quote-amount
	(url:get-pure-port
	  (url:make-url "http" "quote.yahoo.com" #f
	    "/q"			;; leading slash essential
	    #f
	    (string-append "s=" (entity-name symbol) "&d=v1")
	    #f))
	symbol)))

  (define stock make-stock)
  (define fund make-fund)

  )
