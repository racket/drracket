(define saved-name "")
(define saved-email "")

(define bug-frame%
  (class frame% (title)
    (override
      [on-close
       (lambda ()
         (cancel))])))

(define bug-frame (make-object frame% "Bug Report Form"))
(define top-panel (make-object horizontal-panel% bug-frame))
(define label-panel (make-object vertical-panel% top-panel))
(define item-panel (make-object vertical-panel% top-panel))
(send label-panel set-alignment 'right 'center)
(send item-panel set-alignment 'left 'center)

(define (add-label text item)
  (let*-values ([(panel) (make-object vertical-pane% label-panel)]
                [(label) (make-object message% text panel)]
                [(_1 l-min) (send item get-graphical-min-size)]
                [(_2 r-min) (send label get-graphical-min-size)])
    (send panel set-alignment 'center 'top)
    (let ([height (max l-min r-min)])
      (send panel min-height height)
      (send item min-height height))))

(define name (make-object text-field% #f item-panel void saved-name))
(add-label "Name" name)

(define email (make-object text-field% #f item-panel void saved-email))
(add-label "Email" email)

(define summary (make-object text-field% #f item-panel void))
(add-label "Summary" summary)

(define severity (make-object choice% 
                   #f
                   (list "critical" "serious" "non-critical")
                   item-panel
                   void))
(add-label "Severity" severity)

(define class-choice 
  (make-object choice%
    #f
    (list "software bug" "documentation bug" "change request" "support")
    item-panel
    void))
(add-label "Class" class-choice)

(define priority (make-object choice%
                   #f
                   (list "high" "medium" "low")
                   item-panel
                   void))
(add-label "Priority" priority)
(send priority set-selection 1)

(define (make-big-text label)
  (let* ([text (make-object text%)]
         [canvas (make-object editor-canvas% item-panel text)])
    (send canvas min-width 600)
    (send canvas min-height 250)
    (add-label label canvas)
    text))

(define description (make-big-text "Description"))
(define reproduce (make-big-text "Reproduce"))

(define button-panel (make-object panel% bug-frame))
(send button-panel set-alignment 'right 'center)
(send button-panel stretchable-height #f)
(define cancel-button (make-object button% "Cancel" button-panel (lambda x (cancel))))
(define ok-button (make-object button% "OK" button-panel (lambda x (ok))))


(define (ok)
  (cleanup-frame))

(define (cancel)
  (cleanup-frame))

(define (cleanup-frame)
  (send bug-frame show #t))

(send bug-frame show #t)
