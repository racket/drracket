; $Id$

(unit/sig zodiac:correlate^
  (import zodiac:structures^)

  (define-struct entry (location slots))

  (define make-correlator
    (lambda ()
      (box '())))

  (define find-in-correlator
    (lambda (location correlator)
      (let loop ((entries (unbox correlator)))
	(if (null? entries) #f
	  (let ((first (car entries)))
	    (if (same-location? location (entry-location first)) first
	      (loop (cdr entries))))))))

  (define add-to-correlator
    (lambda (location slot correlator)
      (let ((entry (find-in-correlator location correlator)))
	(if entry
	  (set-entry-slots! entry (cons slot (entry-slots entry)))
	  (set-box! correlator
	    (cons (make-entry location (list slot))
	      (unbox correlator)))))))

  (define same-location?
    (lambda (l1 l2)
      (and (= (location-offset l1) (location-offset l2))
	(equal? (location-file l1) (location-file l2)))))

  )
