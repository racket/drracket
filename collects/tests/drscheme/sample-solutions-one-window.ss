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

(define toc (call-with-input-file (build-path sample-solutions-dir "toc.ss") read))
(define default-toc-entry '(#f ()))

(define labels
  (let* ([all-info (call-with-input-file (build-path (collection-path "solutions") 
                                                     'up 'up "proj" "book" "solutions"
                                                     "labels.scm") read)]
         [all-labels (map car all-info)]
         [ex-labels (filter (lambda (x) (and (string=? (substring x 0 3) "ex:")
                                             (> (string-length x) 3)))
                            all-labels)])
    (map (lambda (x) (string-append (substring x 3 (string-length x)) ".scm"))
         ex-labels)))

(define sample-solutions
  (filter (lambda (x) (and 
                       (> (string-length x) 3)
                       (string=? "scm" (substring x (- (string-length x) 3) (string-length x)))
                       (member x labels)))
          (directory-list sample-solutions-dir)))

(define (test-single-file filename)
  (let* ([toc-entry (let ([lookup (assoc (string->symbol filename) toc)])
                      (if lookup
                          (cdr lookup)
                          default-toc-entry))]
         [language "Beginning Student"] ;; should be calculated from the section
         [errors-ok? (car toc-entry)]
         [teachpacks (cadr toc-entry)])
    
    (let* ([drs-frame (wait-for-drscheme-frame)]
           [definitions-text (ivar drs-frame definitions-text)]
           [interactions-text (ivar drs-frame interactions-text)])
      
      ;; update the program (cheat to hack around gc bug -- should really use file|open)
      (send definitions-text load-file (build-path sample-solutions-dir filename))
      
      ;; only bother changing the language dialog when necessary.
      (unless (string=?
               (send interactions-text get-text
                     (send interactions-text paragraph-start-position 1)
                     (send interactions-text paragraph-end-position 1))
               (format "Language: ~a." language))
        (set-language-level! language))
      
      ;; only bother changing the teachpacks when necessary.
      (let* ([get-full-path
              (lambda (teachpack)
                (normal-case-path
                 (normalize-path
                  (build-path (collection-path "mzlib") 'up 'up
                              "teachpack" "htdp" teachpack))))]
             [teachpack-should-be
              (apply string-append (map (lambda (tp) (format "Teachpack: ~a.~n" (get-full-path tp))) teachpacks))]
             [teachpack-is
              (send interactions-text get-text
                    (send interactions-text paragraph-start-position 2)
                    (send interactions-text paragraph-start-position (+ 2 (length teachpacks))))]
             [teachpacks-already-set? (string=? teachpack-should-be teachpack-is)])
        (unless teachpacks-already-set?
          (fw:test:menu-select "Language" "Clear All Teachpacks")
          (for-each (lambda (teachpack)
                      (use-get/put-dialog
                       (lambda ()
                         (fw:test:menu-select "Language" "Add Teachpack..."))
                       (get-full-path teachpack)))
                    teachpacks)))
      
      (do-execute drs-frame)

      ;; kill the program, but only when it opens windows
      (unless (eq? drs-frame (get-top-level-focus-window))
        (custodian-shutdown-all (ivar interactions-text user-custodian))
        (let ([wait-for-kill-window
               (lambda ()
                 (let ([f (get-top-level-focus-window)])
                   (and f (equal? (send f get-label)
                                  "Evaluation Terminated"))))])
          (poll-until wait-for-kill-window)
          (fw:test:button-push "Ok")
          (wait-for-drscheme-frame #f)))
      
      ;; still check equal pairs when there is a sanctioned error.
      (cond
        [(and (not errors-ok?)
              (has-error? drs-frame))
	 =>
	 (lambda (err-msg)
	   (printf "ERROR: ~a: found error, but should be no errors:~n  ~a~n"
		   filename
		   err-msg))]
        [else
         (let* ([output (fetch-output drs-frame)]
                [port (open-input-string output)])
           (let loop ([last #f]
                      [equal-count 0])
             (let ([sexp (with-handlers ([(lambda (exn) #t)
                                          (lambda (exn) exn)])
                           (read port))])
               (unless (eof-object? sexp)
                 (cond
                   [(and (not last) (equal? sexp "="))
                    (printf "ERROR: ~a: found = as first sexp~n" filename)]
                   [(and last (equal? '= sexp))
                    (let ([after (with-handlers ([(lambda (exn) #t)
                                                  (lambda (exn) exn)])
                                   (read port))])
                      (unless (equal? after last)
                        (printf "ERROR: ~a: ~a mismatched.~n     got ~s~nexpected ~s~n"
                                filename equal-count 
                                (if (exn? last) (exn-message last) last)
                                (if (exn? after) (exn-message after) after)))
                      (loop after (+ equal-count 1)))]
                   [else (loop sexp equal-count)])))))]))))

(for-each test-single-file sample-solutions)