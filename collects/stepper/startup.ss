(unit/sig (invoke-stepper)
  (import mzlib:core^
          [mred : mred^]
          [fw : framework^]
          [drscheme : drscheme:export^]
          (stepper-go))
  
  (define (invoke-stepper frame)
      (let ([existing-stepper (send frame stepper-frame)])
        (if existing-stepper
            (send existing-stepper show #t)
              (fw:gui-utils:show-busy-cursor
               (lambda ()
                 (stepper-go frame))))))
  
  (define stepper-bitmap
    (drscheme:unit:make-bitmap
     "Step"
     (build-path (collection-path "icons") "foot.bmp")))
  
  (drscheme:get/extend:extend-unit-frame
   (lambda (super%)
     (class super% args
       (inherit button-panel)
       (sequence (apply super-init args))
       (rename [super-disable-evaluation disable-evaluation]
               [super-enable-evaluation enable-evaluation])
       (override
         [enable-evaluation
          (lambda ()
            (send stepper-button enable #t)
            (super-enable-evaluation))]
         [disable-evaluation
          (lambda ()
            (send stepper-button enable #f)
            (super-disable-evaluation))])
       (public
         [stepper-frame
          (let ([frame #f])
            (case-lambda
             (() frame)
             ((new-val) (set! frame new-val))))]
         
         [stepper-button (make-object mred:button%
                                      (stepper-bitmap this)
                                      button-panel
                                      (lambda (button evt) 
                                        (invoke-stepper this)))])
       (sequence
         (send button-panel change-children
               (lambda (l)
                 (cons stepper-button (function:remq stepper-button l)))))))))
