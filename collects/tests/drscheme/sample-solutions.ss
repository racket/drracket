;; memory debugging
(global-defined-value 'top-level-frames null)

(define sample-solutions-dir
  (build-path (collection-path "mzlib")
	      'up
	      'up
	      'up
	      "robby"
	      "collects"
	      "solutions"))
(unless (directory-exists? sample-solutions-dir)
  (error 'sample-solutions.ss "expected directory ~s to exist" sample-solutions-dir))

(set! sample-solutions-dir (normalize-path sample-solutions-dir))

;; add the full pathname to the toc entries.
(define toc
  (map (lambda (x) (cons (build-path sample-solutions-dir (car x)) (cdr x)))
       (call-with-input-file (build-path sample-solutions-dir "toc.ss") read)))

;; close out the first frame to avoid complications
(let ([orig-drs (wait-for-drscheme-frame)])
  (fw:test:menu-select "File" "New")
  (wait-for-new-frame orig-drs)
  (send orig-drs close))

(define frame-to-close (wait-for-drscheme-frame))

(define (test-single-file toc-entry)
  (let ([filename (car toc-entry)]
	[language (cadr toc-entry)]
	[errors-ok? (caddr toc-entry)]
	[teachpacks (cadddr toc-entry)]
	[old-pref (fw:preferences:get 'framework:file-dialogs)])
    (fw:preferences:set 'framework:file-dialogs 'common)


    (let* ([drs-frame (wait-for-drscheme-frame)]
	   [wait-for-execute
	    (lambda ()
	      (wait-for-button (ivar drs-frame execute-button)))])
      (fw:test:menu-select "File" "Open...")
      (wait-for-new-frame drs-frame)
      (let ([pathname (find-labelled-window "Full pathname")]
	    [dialog (get-top-level-focus-window)])
	(send pathname focus)
	(fw:test:keystroke #\a (case (system-type)
				 [(windows) (list 'control)]
				 [(macos) (list 'meta)]
				 [(unix) (list 'meta)]))
	(let loop ([i 0])
	  (when (< i (string-length filename))
	    (fw:test:keystroke (string-ref filename i))
	    (loop (+ i 1))))
	(fw:test:keystroke #\return)
	(wait-for-new-frame dialog))
      (wait-for-new-frame drs-frame))

    (let* ([drs-frame (wait-for-drscheme-frame)]
	   [wait-for-execute
	    (lambda ()
	      (wait-for-button (ivar drs-frame execute-button)))])

      (when frame-to-close (send frame-to-close close))
      (set! frame-to-close drs-frame)

      ;; memory debugging
      (global-defined-value 'top-level-frames
			    (cons
			     (make-weak-box drs-frame)
			     (global-defined-value 'top-level-frames)))
      (collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)
      (send drs-frame update-memory-text) 
      ;(dump-memory-stats)

      (set-language-level! language)
      (fw:test:menu-select "Language" "Clear All Teachpacks")
      (for-each (lambda (teachpack)
		  (let ([filename (normalize-path (apply
						   build-path
						   (collection-path "mzlib")
						   'up
						   'up
						   "teachpack"
						   teachpack))])
		  (fw:test:menu-select "Language" "Add Teachpack...")
		  (let ([dialog (wait-for-new-frame drs-frame)])
		    (send (find-labelled-window "Full pathname") focus)
		    (fw:test:keystroke #\a (case (system-type)
					     [(windows) (list 'control)]
					     [(macos) (list 'meta)]
					     [(unix) (list 'meta)]))
		    (let loop ([i 0])
		      (when (< i (string-length filename))
			(fw:test:keystroke (string-ref filename i))
			(loop (+ i 1))))
		    (fw:test:keystroke #\return)
		    (wait-for-new-frame dialog))))
		teachpacks)

      (do-execute drs-frame)
      (wait-for-execute)
      
      (when (and (not errors-ok?)
		 (has-error? drs-frame))
	(error 'sample-solutions.ss "should be no errors for ~s" filename))
	  

      (let ([lines
	     (let ([port (open-input-string (fetch-output drs-frame))])
	       (let loop ()
		 (let ([line (read-line port)])
		   (if (eof-object? line)
		       null
		       (cons line (loop))))))])
	(unless (< (length lines) 3)
	  (let loop ([before (car lines)]
		     [during (cadr lines)]
		     [after (caddr lines)]
		     [rest (cdddr lines)])
	    (when (string=? during "=")
	      (unless (string=? before after)
		(printf "FAILED ~s; expected ~s and ~s to be the same~n"
			filename before after)))
	    (unless (null? rest)
	      (loop during after (car rest) (cdr rest))))))

	(fw:preferences:set 'framework:file-dialogs old-pref))))

(for-each test-single-file toc)