(define saved-name "")
(define saved-email "")

(define (same-height l r)
  (let-values ([(_1 l-min) (send l get-graphical-min-size)]
               [(_2 r-min) (send r get-graphical-min-size)])
    ;(printf "l-min ~a r-min ~a~n" l-min r-min)
    (let ([height (max l-min r-min)])
      (send l min-height height)
      (send r min-height height))))

(define bug-frame (make-object frame% "Bug Report Form"))
(define top-panel (make-object horizontal-panel% bug-frame))
(define label-panel (make-object vertical-panel% top-panel))
(define item-panel (make-object vertical-panel% top-panel))
(send label-panel set-alignment 'right 'center)
(send item-panel set-alignment 'left 'center)

(define name (make-object text-field% #f item-panel void saved-name))
(define name-label (make-object message% "Name" label-panel))
(same-height name name-label)

(define email (make-object text-field% #f item-panel void saved-email))
(define email-label (make-object message% "Email" label-panel))
(same-height email email-label)

(define summary (make-object text-field% #f item-panel void))
(define summary-label (make-object message% "Summary" label-panel))
(same-height summary summary-label)

(define severity (make-object choice% 
                   #f
                   (list "critical" "serious" "non-critical")
                   item-panel
                   void))
(define severity-label (make-object message% "Severity" label-panel))
(send severity set-selection 1)
(same-height severity severity-label)

(define class-choice 
  (make-object choice%
    #f
    (list "software bug" "documentation bug" "change request" "support")
    item-panel
    void))
(define class-label (make-object message% "Class" label-panel))
(same-height class-choice class-label)

(define priority (make-object choice%
                   #f
                   (list "high" "medium" "low")
                   bug-frame
                   void))
(define priority-label (make-object message% "Priority" label-panel))
(send priority set-selection 1)
(same-height priority priority-label)

(send bug-frame show #t)