(unit/sig stepper:gui^
  (import mzlib:pretty-print^
   stepper:stepper^)

  (define step
  (let ([first-time #t])
    (lambda ()
      (if first-time
          (begin
            (set! first-time #f)
            (stepper:stepper-start
             (program->string struct-program)))
          (stepper:stepper-step)))))
  
  ;; view : (-> A) -> (void)
  (define (view thunk)
    (letrec ([frame (make-object frame% "Stepper")]
             [output-delta (make-object style-delta% 'change-family 'modern)]
             [result-delta (make-object style-delta% 'change-family 'modern)]
             [button-panel (make-object horizontal-panel% frame)]
             [home-button (make-object button% "Home" button-panel
                                       (lambda (_1 _2) (home)))]
             [previous-button (make-object button% "<< Previous" button-panel
                                           (lambda (_1 _2) (previous)))]
             [next-button (make-object button% "Next >>" button-panel (lambda
                                                                          (_1 _2) (next)))]
             [canvas (make-object editor-canvas% frame)]
             
             [history null]
             [view 0]
             
             [fetch-next-step
              (lambda ()
                (let ([result (open-output-string)]
                      [output (open-output-string)])
                  (parameterize ([current-output-port output])
                    ; changed by JBC for list of results
                    (for-each
                     (lambda (expr)
                       (pretty-print expr result))
                     (thunk)))
                  (let ([outer-edit (make-object text%)])
                    
                    (send outer-edit insert (get-output-string result))
                    (send outer-edit insert #\newline)
                    (let ([between (send outer-edit last-position)])
                      (send outer-edit insert (get-output-string output))
                      (send outer-edit change-style result-delta 0 between)
                      (send outer-edit change-style output-delta between
                            (send outer-edit last-position))
                      (set! history (append history (list outer-edit)))))))]
             
             [update-view
              (lambda (new-view)
                (set! view new-view)
                (send canvas set-editor (list-ref history view))
                (send previous-button enable (not (zero? view)))
                (send home-button enable (not (zero? view))))]
             [home
              (lambda ()
                (update-view 0))]
             [next
              (lambda ()
                (when (= view (- (length history) 1))
                  (fetch-next-step))
                (update-view (+ view 1)))]
             [previous
              (lambda ()
                (update-view (- view 1)))])
      (send result-delta set-delta-foreground "BLUE")
      (send output-delta set-delta-foreground "PURPLE")
      (fetch-next-step)
      (update-view 0)
      (send button-panel stretchable-width #f)
      (send canvas min-width 500)
      (send canvas min-height 500)
      (send previous-button enable #f)
      (send home-button enable #f)
      (send frame show #t)))
  
  ;; example
  ;(view (let ([n 'core])
  ;        (lambda ()
  ;          (write n)
  ;          (begin0 n
  ;                  (set! n `(,n ,n))))))
  
  )