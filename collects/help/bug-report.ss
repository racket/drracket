(require-library "framework.ss" "framework")
(require-library "head.ss" "net")
(require-library "smtp.ss" "net")

(preferences:set-default 'drscheme:email "" string?)
(preferences:set-default 'drscheme:full-name "" string?)

(define bug-frame%
  (class frame% (title)
    (override
      [on-close
       (lambda ()
         (cancel))])))

(define bug-frame (make-object frame% "Bug Report Form"))
(define top-panel (make-object vertical-panel% bug-frame))

(define lps null)

;; build/label : ((union string (list-of string)) (area-container<%> -> item<%>) boolean -> item<%>)
;; constructs and arranges the gui objects for the bug report form
;; effect: updates lps with the new label panel, for future alignment
(define build/label
  (case-lambda
   [(text make-item top?)
    (build/label text make-item top? #f)]
   [(text make-item top? stretch?)
    (let*-values ([(hp) (make-object horizontal-panel% top-panel)]
                  [(lp) (make-object vertical-panel% hp)]
                  [(ip) (make-object vertical-panel% hp)]
                  [(label/s) (if (string? text)
                                 (make-object message% text lp)
                                 (map (lambda (s)
                                        (make-object message% s lp))
                                      text))]
                  [(item) (make-item ip)])
      (set! lps (cons lp lps))
      (unless stretch? 
        (send hp stretchable-height #f)
        (send lp stretchable-height #f)
        (send ip stretchable-height #f))
      (send lp stretchable-width #f)
      (send lp set-alignment 'right (if top? 'top 'center))
      (send ip set-alignment 'left 'top)
      item)]))

(define (align-labels)
  (let ([width (apply max (map (lambda (x) (send (car (send x get-children)) min-width))
                               lps))])
    (for-each (lambda (x) (send x min-width width)) lps))) 

(define name
  (build/label 
   "Name"
   (lambda (panel)
     (make-object text-field% #f panel
       (lambda (text event)
         (preferences:set 'drscheme:full-name (send text get-value)))
       (preferences:get 'drscheme:full-name)))
   #f))

(build/label
 "Email"
 (lambda (panel)
   (make-object text-field% #f panel
     (lambda (text event)
       (preferences:set 'drscheme:email (send text get-value)))
     (preferences:get 'drscheme:email)))
 #f)

(define summary
  (build/label
   "Summary" 
   (lambda (panel)
     (make-object text-field% #f panel void))
   #f))


(define severity
  (build/label 
   "Severity" 
   (lambda (panel)
     (make-object choice% 
       #f
       (list "critical" "serious" "non-critical")
       panel
       void))
   #f))

(define class
  (build/label
   "Class" 
   (lambda (panel)
     (make-object choice%
       #f
       (list "software bug" "documentation bug" "change request" "support")
       panel
       void))
   #f))

(define priority
  (build/label
   "Priority" 
   (lambda (panel)
     (make-object choice%
       #f
       (list "high" "medium" "low")
       panel
       void))
   #f))
(send priority set-selection 1)

(define (make-big-text label)
  (let ([canvas 
         (build/label 
          label 
          (lambda (panel)
            (let* ([text (make-object text%)]
                   [canvas (make-object editor-canvas% panel text)])
              
              canvas))
          #t
          #t)])
    (send canvas min-width 400)
    (send canvas min-height 100)
    (send canvas get-editor)))

(define description (make-big-text "Description"))
(define reproduce (make-big-text '("Steps to" "Reproduce")))

(define version
  (build/label
   "Version"
   (lambda (panel)
     (make-object text-field% #f panel void ""))
   #f))
(send version set-value   
      (format "~a"
              (version:version)))
(define environment
  (build/label
   "Environment"
   (lambda (panel)
     (make-object text-field% #f panel void ""))
   #f))
(send environment set-value   
      (format "~a (~a)"
              (system-type)
              (system-library-subpath)))
(define tools
  (build/label
   "Tools"
   (lambda (panel)
     (make-object text-field% #f panel void ""))
   #f))
(send tools set-value 
      (format "~s" (directory-list (collection-path "drscheme" "tools"))))
(define collections
  (build/label 
   "Collections"
   (lambda (panel)            
     (make-object text-field% #f panel void
       ""))
   #f))
(send collections set-value       
      (format "~s"
               (map (lambda (x) (list x (directory-list x)))
                    (current-library-collection-paths))))

(align-labels)

(define button-panel (make-object horizontal-panel% bug-frame))
(send button-panel set-alignment 'right 'center)
(send button-panel stretchable-height #f)
(define cancel-button (make-object button% "Cancel" button-panel (lambda x (cancel))))
(define ok-button (make-object button% "Submit" button-panel (lambda x (ok)) '(border)))

(send (if (string=? "" (preferences:get 'drscheme:full-name))
          name
          summary)
      focus)

(define (smtp-send-bug-report)
  (smtp-send-message
   (insert-field
    "X-Mailer"
    (format "Help Desk ~a (bug report form)" (version:version))
    (insert-field     
     "Subject" 
     (send summary get-value)
     (insert-field
      "To"
      "plt-gnats@cs.rice.edu"
      (insert-field
       "From"
       (preferences:get 'drscheme:email)
       empty-header))))
   (append
    (list
     ">Category:       all"
     (format ">Synopsis:       ~a" (send summary get-value))
     ">Confidential:   no"
     (format ">Severity:       ~a" (send severity get-string-selection))
     (format ">Priority:       ~a" (send priority get-string-selection))
     (format ">Class:          ~a" (send class get-string-selection))
     ">Submitter-Id:   unknown"
     (format ">Originator:     ~a" (preferences:get 'drscheme:full-name))
     ">Organization:"
     "titan"
     (format ">Release:        ~a" (send version get-value))
     ">Environment:"
     (format "~a" (send environment get-value))
     (format "Tools: ~a" (send tools get-value))
     "Collections:"
     (format "~a" (send collections get-value))
     ">Fix: ")
    (cons
     ">Description:"
     (get-strings description))
    (cons
     ">How-To-Repeat:"
     (get-strings reproduce)))))

(define (send-bug-report)
  (letrec ([f (make-object dialog% "Sending Bug Report")]
           [sema (make-semaphore 0)]
           [msg (make-object message% "Sending Bug Report" f)]
           [button (make-object button% "Cancel" f (lambda (x y)
                                                     (break-thread smtp-thread)
                                                     (send f show #f)))]
           [smtp-thread
            (thread
             (lambda ()
               (semaphore-wait sema)
               (send button enable #t)
               (parameterize ([smtp-sending-end-of-message
                               (lambda ()
                                 (send button enable #f))])
                 (semaphore-wait (make-semaphore 0))
                 '(smtp-send-bug-report)
                 (send f show #f))))])
    (send button enable #f)
    (queue-callback (lambda () (semaphore-post sema)))
    (send f show #t)))

(define (get-strings t)
  (let loop ([n 0])
    (cond
      [(= n (send t last-paragraph)) null]
      [else (cons (send t get-text
                        (send t paragraph-start-position n)
                        (send t paragraph-end-position n))
                  (loop (+ n 1)))])))

(define (ok)
  (send-bug-report)
  (cleanup-frame))

(define (cancel)
  (cleanup-frame))

(define (cleanup-frame)
  (send bug-frame show #t))

(send bug-frame show #t)
