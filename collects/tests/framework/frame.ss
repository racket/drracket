(define (test-creation name class-expression)
  (test
   name
   (lambda (x) x)
   (lambda ()
     (send-sexp-to-mred
      `(begin (preferences:set 'framework:exit-when-no-frames #f)
	      (send (make-object ,class-expression "test") show #t)))
     (wait-for-frame "test")
     (queue-sexp-to-mred
      '(send (get-top-level-focus-window) close))
     #t)))

(test-creation
 'basic%-creation
 'frame:basic%)
(test-creation
 'basic-mixin-creation
 '(frame:basic-mixin frame%))

(test-creation
 'info-mixin-creation
 '(frame:info-mixin frame:basic%))
(test-creation
 'info%-creation
 'frame:info%)

(test-creation
 'text-info-mixin-creation
 '(frame:text-info-mixin frame:info%))
(test-creation
 'text-info%-creation
 'frame:text-info%)

(test-creation
 'pasteboard-info-mixin-creation
 '(frame:pasteboard-info-mixin frame:info%))
(test-creation
 'pasteboard-info%-creation
 'frame:pasteboard-info%)

(test-creation
 'standard-menus%-creation
 'frame:standard-menus%)
(test-creation
 'standard-menus-mixin
 '(frame:standard-menus-mixin frame:basic%))

(test-creation
 'text%-creation
 'frame:text%)
(test-creation
 'text-mixin-creation
 '(frame:text-mixin frame:editor%))
(test-creation
 'text-mixin-creation
 '(frame:text-mixin frame:editor%))

(test-creation
 'searchable%-creation
 'frame:searchable%)
(test-creation
 'searchable-mixin
 '(frame:searchable-mixin frame:text%))

(test-creation
 'text-info-file%-creation
 'frame:text-info-file%)
(test-creation
 'text-info-file-mixin-creation
 '(frame:file-mixin frame:text%))

(test-creation
 'pasteboard-mixin-creation
 '(frame:pasteboard-mixin frame:editor%))
(test-creation
 'pasteboard-mixin-creation
 '(frame:pasteboard-mixin (frame:editor-mixin frame:standard-menus%)))
(test-creation
 'pasteboard%-creation
 'frame:pasteboard%)

(test-creation
 'pasteboard-info-file-mixin-creation
 '(frame:file-mixin frame:pasteboard%))
(test-creation
 'pasteboard-info-file%-creation
 'frame:pasteboard-info-file%)

(define (test-open name class-expression)
  (let* ([test-file-contents "test"]
	 [tmp-file-name "framework-tmp"]
	 [tmp-file (build-path (collection-path "tests" "framework")
			       tmp-file-name)])
    (test
     name
     (lambda (x)
       (delete-file tmp-file)
       (equal? x test-file-contents))
     (lambda ()
       (send-sexp-to-mred
	`(begin
	   (preferences:set 'framework:exit-when-no-frames #f)
	   (preferences:set 'framework:file-dialogs 'common)
	   (send (make-object ,class-expression "test open") show #t)))
       (wait-for-frame "test open")
       (send-sexp-to-mred
	`(test:menu-select "File" "Open..."))
       (wait-for-frame "Get file")
       (call-with-output-file tmp-file
	 (lambda (port)
	   (display test-file-contents port))
	 'truncate)
       (send-sexp-to-mred
	`(begin (send (find-labelled-window "Full pathname") focus)
		,(case (system-type)
		   [(macos) `(test:keystroke #\a '(meta))]
		   [(unix) `(test:keystroke #\a '(meta))]
		   [(windows) `(test:keystroke #\a '(control))]
		   [else (error "unknown system type: ~a" (system-type))])
		(for-each test:keystroke
			  (string->list ,tmp-file))
		(test:keystroke #\return)))
       (wait-for-frame tmp-file-name)
       (begin0
	(send-sexp-to-mred
	 `(let* ([w (get-top-level-focus-window)]
		 [t (send (send w get-editor) get-text)])
	    (test:close-top-level-window w)
	    t))
	(wait-for-frame "test open")
	(queue-sexp-to-mred
	 `(send (get-top-level-focus-window) close)))))))

(test-open "frame:editor open" 'frame:text%)
(test-open "frame:searchable open" 'frame:searchable%)
(test-open "frame:text-info open" 'frame:text-info-file%)
