
(module stepper-test mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           "drscheme-test-util.ss"
           (lib "gui.ss" "tests" "utils")
           (lib "framework.ss" "framework"))
  
  (provide run-test)
  
  ;; type contents = (listof (union snip string contents))
  ;; type error = (make-error string)
  ;; type step = (make-step contents contents (union error contents))
  (define-struct step (definitions before after) (make-inspector))
  (define-struct err (message) (make-inspector))

  ;; type program-spec = (union string (make-file string))
  (define-struct file (name))
  
  (define current-program-id (make-parameter 'current-program-id-unset))
  (define failure-escape (make-parameter 'failure-escape-unset))
  
  (define sample-solutions-directory "/home/robby/unison/collects/solutions")
  
  (define (run-test)
    (set-language-level! (list "How to Design Programs" "Beginning Student"))
    ;(run-file-test (build-path sample-solutions-directory "add.scm"))
    (run-string-test "(define (f x) (* x 2))\n(+ 1 (f (+ 1 1)))")
    (run-string-test "(sqrt 2)")
    (run-string-test "(car)")
    '(for-each
      (lambda (file) (run-file-test (build-path sample-solutions-directory file)))
      (directory-list sample-solutions-directory))
    
    '(set-language-level! (list "How to Design Programs" "Beginning Student with List Abbreviations"))
    '(for-each
      (lambda (file) (run-file-test (build-path sample-solutions-directory file)))
      (directory-list sample-solutions-directory)))

  ;; run-file-test : string -> void
  (define (run-file-test filename)
    (let/ec k
      (parameterize ([current-program-id filename]
                     [failure-escape k])
        (check-steps
         (make-file filename)
         (step-and-extract-program (make-file filename))))))
  
  ;; run-string-test : string -> void
  ;; runs the test in the string.
  (define (run-string-test prog)
    (let/ec k
      (parameterize ([current-program-id prog]
                     [failure-escape k])
        (check-steps
         prog
         (step-and-extract-program prog)))))
  
  ;; simple-failure : (union #f step) string any ... -> beta
  ;; indicates that this one test failed, but the stepper may still
  ;; be steppable, so just jumps out of this one test.
  (define (simple-failure step message . args)
    (printf "FAILED TEST ~s:\n" (current-program-id))
    (when step (printf "~s\n" step))
    (printf "~a\n\n" (apply format message args))
    ((failure-escape)))
  
  ;; check-steps : program-spec (listof step) -> void
  ;; executes each of the steps in DrScheme and raises
  ;; an exception if something doesn't match up.
  (define (check-steps program steps)
    (let* ([drs-frame (wait-for-drscheme-frame)]
           [defs-text (send drs-frame get-definitions-text)])
      (let loop ([last-results #f]
                 [steps steps])
        (cond
          [(null? steps)
           (clear-definitions drs-frame)
           (type-in-definitions drs-frame ";; full prog\n")
           (set-definitions-to-program drs-frame program)
           (do-execute drs-frame)
           (let ([prog-results (fetch-output drs-frame)])
             
             ;; can only check subset here, since we may have stopped stepping early
             (check-subset-results #f last-results prog-results))]
          [else
           (let ([step (car steps)])
             (clear-definitions drs-frame)
             (dynamic-wind
              (lambda ()
                (send defs-text begin-edit-sequence))
              (lambda ()
                (type-in-definitions drs-frame ";; before\n")
                (insert-into-definitions drs-frame (step-definitions step))
                (type-in-definitions drs-frame "\n")
                (insert-into-definitions drs-frame (step-before step)))
              (lambda ()
                (send defs-text end-edit-sequence)))
             (do-execute drs-frame)
             (let ([before-results (fetch-output drs-frame)])
               (when last-results
                 (check-subset-results step last-results before-results))
               (clear-definitions drs-frame)
               (dynamic-wind
                (lambda ()
                  (send defs-text begin-edit-sequence))
                (lambda ()
                  (type-in-definitions drs-frame ";; after\n")
                  (insert-into-definitions drs-frame (step-definitions step))
                  (type-in-definitions drs-frame "\n")
                  (unless (err? (step-after step))
                    (insert-into-definitions drs-frame (step-after step))))
                (lambda ()
                  (send defs-text end-edit-sequence)))
               (do-execute drs-frame)
               (cond
                 [(err? (step-after step))
                  (let* ([pre-output (fetch-output drs-frame)]
                         [add-newline? (and ((string-length pre-output) . >= . 1)
                                            (not
                                             (char=?
                                              (string-ref pre-output 
                                                          (- (string-length pre-output) 1))
                                              #\newline)))]
                         [after-results 
                          (if add-newline?
                              (string-append pre-output
                                             "\n"
                                             (err-message (step-after step)))
                              (string-append pre-output
                                             (err-message (step-after step))))])
                    (check-same-results step before-results after-results)
                    (unless (null? (cdr steps))
                      (simple-failure #f "expected no more steps after an error, found ~s" (cdr steps)))
                    (loop after-results null))]
                 [else
                  (let ([after-results (fetch-output drs-frame)])
                    (check-same-results step before-results after-results)
                    (loop after-results (cdr steps)))])))]))))

  ;; insert-info-definitions : frame contents -> void
  ;; technically, this function should probably type the
  ;; contents into the definitions window, but that is
  ;; considerably slower than just inserting it directly....
  (define (insert-into-definitions drs-frame orig-contents)
    (let ([defns (send drs-frame get-definitions-text)])
      (let loop ([contents orig-contents])
        (for-each
         (lambda (content)
           (cond
             [(or (string? content) 
                  (is-a? content snip%))
              (send defns insert content
                    (send defns last-position)
                    (send defns last-position))]
             [(eq? content 'unknown)
              (error 'insert-into-definitions "found unknown snip in ~e" orig-contents)]
             [(list? content) 
              ;; wrong thing. this flattens embedded editors
              (loop content)]))
         contents))))
  
  ;; check-subset-results : step string string -> void
  ;; raises an error if s1 is not the beginning of s2.
  (define (check-subset-results step s1 s2)
    (unless (and (<= (string-length s1)
                     (string-length s2))
                 (string=? (substring s2 0 (string-length s1))
                           s1))
      (simple-failure step "expected ~s to be the beginning of ~s" s1 s2)))
  
  ;; check-same-results : step string string -> void
  ;; raises an error if s1 is not s2.
  (define (check-same-results step s1 s2)
    (unless (string=? s1 s2)
      (simple-failure step "expected ~s to be the same as ~s" s1 s2)))
  
  ;; step-and-extract-program : program-spec -> (listof step)
  (define (step-and-extract-program program)
    (let ([drs-frame (wait-for-drscheme-frame)])
      (clear-definitions drs-frame)
      (set-definitions-to-program drs-frame program)
      (let* ([stepper-frame (start-stepper drs-frame)]
             [steps (get-all-steps stepper-frame)])
        (test:menu-select "File" "Close")
        (let ([drs-frame1 (wait-for-new-frame stepper-frame)])
          (unless (eq? drs-frame1 drs-frame)
            (error 'step-and-extract "didn't get back to drscheme frame, got: ~e" drs-frame)))
        steps)))

  ;; set-definitions-to-program : program-spec -> void
  (define (set-definitions-to-program drs-frame program)
    (cond
      [(string? program)
       (type-in-definitions drs-frame program)]
      [else
       (let ([definitions-text (send drs-frame get-definitions-text)])
         (send definitions-text load-file (file-name program))
         (send definitions-text set-filename #f))]))

  ;; start-stepper : frame -> frame
  (define (start-stepper drs-frame)
    (test:button-push (send drs-frame get-stepper-button))
    (let ([stepper-frame (wait-for-new-frame drs-frame)])
      stepper-frame))
  
  ;; get-all-steps : frame -> (listof step)
  (define (get-all-steps stepper-frame)
    (let* ([stepper-canvas (find-labelled-window #f editor-canvas% stepper-frame)]
           [stepper-editor (poll-until (lambda () (send stepper-canvas get-editor)))])
      (cons (get-step stepper-frame stepper-editor)
            (get-more-steps stepper-frame))))

  ;; get-more-steps : stepper-frame -> (listof step)
  ;; repeatedly push the next button to get out all of the steps
  (define (get-more-steps stepper-frame)
    (let ([next-button (find-labelled-window "Next >>" button% stepper-frame)]
          [stepper-canvas (find-labelled-window #f editor-canvas% stepper-frame)])
      
      ;; just make sure we are in a ready state.
      (poll-until (lambda () (send next-button is-enabled?)) 1 void)
      
      (let loop ([n 200])
        (cond
          [(zero? n) null] ;; at most 200 steps
          [(send next-button is-enabled?)
           (test:button-push next-button)
           
           ;; wait for the next button to re-enable to
           ;; indicate a new step is rendered
           (poll-until
            (lambda () (send next-button is-enabled?))
            2
            void) ;; no error signalled if button doesn't show up.
           
           (let ([step (get-step stepper-frame (send stepper-canvas get-editor))])
             (if step
                 (cons step (loop (- n 1)))
                 (loop (- n 1))))]
          [else null]))))
  
  ;; get-step : frame editor -> (union step #f)
  ;; extracts a step from the stepper window. Only returns #f for
  ;; the "I'm done stepping" message that sometimes appears at the end.
  (define (get-step stepper-frame stepper-editor)
    (let ([canvas (find-labelled-window #f canvas% stepper-frame (lambda () #f))])
      (when canvas
        (error 'get-steps "stepper warning present!")))
      (let* ([extraction (extract-from-editor stepper-editor)]
             [step (separate-steps extraction)])
        (unless step
          (unless (equal? '("evaluation of program is complete.") extraction)
            (error 'get-step "couldn't parse stepper window: ~s\n" extraction)))
        step))
      
  ;; snips = (union (cons error snips) (cons snip snips) (cons snips snips) null)
  ;; extract-from-editor : editor -> snips
  (define (extract-from-editor editor)
    (let loop ([snip (send editor find-first-snip)])
      (cond
        [(not snip) null]
        [(is-a? snip editor-snip%)
         (let ([editor (send snip get-editor)])
           (cons (cond
                   [(not editor) '()]
                   [(contains-error-message? editor)
                    (let ([ans (extract-from-editor editor)])
                      (unless (and (list? ans)
                                   (andmap string? ans))
                        (error 'extract-from-editor "couldn't parse error message: ~s" ans))
                      (make-err (apply string-append ans)))]
                   [else (extract-from-editor editor)])
                 (loop (send snip next))))]
        [(is-a? snip string-snip%)
         (cons (send snip get-text 0 (send snip get-count) #t)
               (loop (send snip next)))]
        [(is-a? snip image-snip%)
         (cons snip (loop (send snip next)))]
        [(and (is-a? snip snip%)
              (method-in-interface? 'get-fraction-view (object-interface snip)))
         (cons snip (loop (send snip next)))]
        [else (cons 'unknown
                    (loop (send snip next)))])))
  
  ;; contains-error-message? : editor -> boolean
  ;; returns #t if the editors contents look like an error message
  (define (contains-error-message? editor)
    (let ([snip (send editor find-first-snip)])
      (and snip
           (error-style? (send snip get-style)))))  
  
  ;; error-style? : style -> boolean
  (define (error-style? style)
    (let ([color (send style get-foreground)])
      (and (color-equal? (send the-color-database find-color "red") color)
           (memq (send style get-style) '(slant italic)))))
  
  ;; color-equal? : color color -> boolean
  (define (color-equal? c1 c2)
    (and (= (send c1 red) (send c2 red))
         (= (send c1 green) (send c2 green))
         (= (send c1 blue) (send c2 blue))))

           
  ;; separate-steps : snips -> (union step #f)
  (define (separate-steps snips)
    (let/ec k
      (let ([program null]
            [before null]
            [after null]
            [snips snips])
        
        (define (hunt-for-list id)
          (let loop ()
            (cond
              [(null? snips) (k #f)]
              [(eq? 'unknown (car snips)) (k #f)]
              [(list? (car snips))
               (begin0 (car snips)
                       (set! snips (cdr snips)))]
              [else (set! snips (cdr snips))
                    (loop)])))
        (define (hunt-for-list/error id)
          (let loop ()
            (cond
              [(null? snips) (k #f)]
              [(eq? 'unknown (car snips)) (k #f)]
              [(or (err? (car snips)) (list? (car snips)))
               (begin0 (car snips)
                       (set! snips (cdr snips)))]
              [else (set! snips (cdr snips))
                    (loop)])))
        (define (hunt-for-unknown id)
          (let loop ()
            (cond
              [(null? snips) (k #f)]
              [(eq? 'unknown (car snips)) (set! snips (cdr snips))]
              [else (set! snips (cdr snips))
                    (loop)])))
        
        (set! program (hunt-for-list 'program))
        (hunt-for-unknown 'before)
        (set! before (hunt-for-list 'before))
        (hunt-for-unknown 'after)
        (set! after (hunt-for-list/error 'after))
        
        (make-step program
                   before
                   after)))))
