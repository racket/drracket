(define sample-solutions-dir
  (let ([try1
	 (collection-path "solutions")]
	[try2
	 (build-path (collection-path "mzlib")
		     'up
		     'up
		     'up
		     "robby"
		     "collects"
		     "solutions")])
    (cond
     [(directory-exists? try1) try1]
     [else try2])))

(unless (directory-exists? sample-solutions-dir)
  (error 'sample-solutions.ss "expected directory ~s to exist" sample-solutions-dir))

(set! sample-solutions-dir (normalize-path sample-solutions-dir))

(define toc
  (call-with-input-file (build-path sample-solutions-dir "toc.ss") read))
(define default-toc-entry '("Beginning Student" #f ()))

(define labels
  (let ([all-info (call-with-input-file (build-path (collection-path "solutions") 'up 'up "proj" "book" "solutions") read)]
        [all-labels (map car all-info)]
        [ex-labels (filter (lambda (x) (and (string=? (substring x 0 3) "ex:")
                                            (> (string-length x) 3)))
                           all-labels)])
    (map (lambda (x) (string-append (symbol->string x) ".scm"))
         ex-labels)))        

(define sample-solutions
  (filter (lambda (x) (and 
                       (> (string-length x) 3)
                       (string=? "scm" (substring x (- (string-length x) 3) (string-length x)))
                       (member x labels)))
          (directory-list sample-solutions-dir)))

;; just for now to test the first two
(set! sample-solutions (list (car sample-solutions) (cadr sample-solutions)))

(define (test-single-file filename)
  (let* ([toc-entry (let ([lookup (assoc (string->symbol filename) toc)])
                      (if lookup
                          (cdr lookup)
                          default-toc-entry))]
         [language (car toc-entry)]
         [errors-ok? (cadr toc-entry)]
         [teachpacks (caddr toc-entry)])

    (let* ([drs-frame (wait-for-drscheme-frame)]
           [definitions-text (ivar drs-frame definitions-text)])
      (send definitions-text load-file (build-path sample-solutions-dir filename))
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
      
      (when (and (not errors-ok?)
		 (has-error? drs-frame))
	(error 'sample-solutions.ss "should be no errors for ~s" filename))
      (let ([output (fetch-output drs-frame)])
        (void)))))

(for-each test-single-file sample-solutions)