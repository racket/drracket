(test
 'exit-off
 (lambda (x) (not (equal? x "test")))
 (lambda ()
   (send-sexp-to-mred
    '(begin (send (make-object frame:basic% "test") show #t)
	    (preferences:set 'framework:verify-exit #f)
	    (preferences:set 'framework:exit-when-no-frames #f)))
   (wait-for-frame "test")
   (send-sexp-to-mred
    `(begin (send (get-top-level-focus-window) close)
	    (let ([f (get-top-level-focus-window)])
	      (if f
		  (send f get-label)
		  #f))))))
(test
 'exit-on
 (lambda (x) (not (equal? x "test")))
 (lambda ()
   (send-sexp-to-mred
    '(begin (send (make-object frame:basic% "test") show #t)
	    (preferences:set 'framework:verify-exit #t)
	    (preferences:set 'framework:exit-when-no-frames #t)))
   (wait-for-frame "test")
   (send-sexp-to-mred
    `(queue-callback (lambda () (send (get-top-level-focus-window) close))))
   (wait-for-frame "Warning")
   (send-sexp-to-mred
    `(test:button-push "Cancel"))
   (wait-for-frame "test")
   (queue-sexp-to-mred
    `(begin (preferences:set 'framework:exit-when-no-frames #f)
	    (send (get-top-level-focus-window) close)
	    (let ([f (get-top-level-focus-window)])
	      (if f
		  (send f get-label)
		  #f))))))

(test
 'one-frame-registered
 (lambda (x) (equal? x (list "test")))
 (lambda ()
   (send-sexp-to-mred
    `(send (make-object frame:basic% "test") show #t))
   (wait-for-frame "test")
   (send-sexp-to-mred
    `(begin0
      (map (lambda (x) (send x get-label)) (send (group:get-the-frame-group) get-frames))
      (send (get-top-level-focus-window) close)))))

(test
 'two-frames-registered
 (lambda (x) (equal? x (list "test2" "test1")))
 (lambda ()
   (send-sexp-to-mred
    '(send (make-object frame:basic% "test1") show #t))
   (wait-for-frame "test1")
   (send-sexp-to-mred
    '(send (make-object frame:basic% "test2") show #t))
   (wait-for-frame "test2")
   (send-sexp-to-mred
    `(begin0
      (let ([frames (send (group:get-the-frame-group) get-frames)])
	(for-each (lambda (x) (send x close)) frames)
	(map (lambda (x) (send x get-label)) frames))))))

(test
 'one-frame-unregistered
 (lambda (x) (equal? x (list "test1")))
 (lambda ()
   (send-sexp-to-mred
    '(send (make-object frame:basic% "test1") show #t))
   (wait-for-frame "test1")
   (send-sexp-to-mred
    '(send (make-object frame:basic% "test2") show #t))
   (wait-for-frame "test2")
   (queue-sexp-to-mred
    `(send (get-top-level-focus-window) close))
   (send-sexp-to-mred
    `(let ([frames (send (group:get-the-frame-group) get-frames)])
       (for-each (lambda (x) (send x close)) frames)
       (map (lambda (x) (send x get-label)) frames)))))

(test
 'windows-menu
 (lambda (x)
   (equal? x (list "test")))
 (lambda ()
   (send-sexp-to-mred
    '(let ([frame (make-object frame:basic% "test")])
       (send frame show #t)))
   (wait-for-frame "test")
   (send-sexp-to-mred
    '(begin0
      (map
       (lambda (x) (send x get-label))
       (send (car (send (send (get-top-level-focus-window) get-menu-bar) get-items)) get-items))
      (send (get-top-level-focus-window) close)))))

(test
 'windows-menu-sorted1
 (lambda (x)
   (equal? x (list "aaa" "bbb")))
 (lambda ()
   (send-sexp-to-mred
    '(let ([frame (make-object frame:basic% "aaa")])
       (send frame show #t)))
   (wait-for-frame "aaa")
   (send-sexp-to-mred
    '(let ([frame (make-object frame:basic% "bbb")])
       (send frame show #t)))
   (wait-for-frame "bbb")
   (send-sexp-to-mred
    `(let ([frames (send (group:get-the-frame-group) get-frames)])
       (begin0
	(map
	 (lambda (x) (send x get-label))
	 (send (car (send (send (car frames) get-menu-bar) get-items)) get-items))
	(for-each (lambda (x) (send x close)) frames))))))

(test
 'windows-menu-sorted2
 (lambda (x)
   (equal? x (list "aaa" "bbb")))
 (lambda ()
   (send-sexp-to-mred
    '(let ([frame (make-object frame:basic% "bbb")])
       (send frame show #t)))
   (wait-for-frame "bbb")
   (send-sexp-to-mred
    '(let ([frame (make-object frame:basic% "aaa")])
       (send frame show #t)))
   (wait-for-frame "aaa")
   (send-sexp-to-mred
    `(let ([frames (send (group:get-the-frame-group) get-frames)])
       (begin0
	(map
	 (lambda (x) (send x get-label))
	 (send (car (send (send (car frames) get-menu-bar) get-items)) get-items))
	(for-each (lambda (x) (send x close)) frames))))))


