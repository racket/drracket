(unit/sig (break)
  (import [mred : mred^]
          [marks : stepper:marks^]
          [annotate : stepper:annotate^])
  
  (define drscheme-eventspace (mred:current-eventspace))
  (define break-semaphore (make-semaphore))
  (define break-resume-value #f)
  
  (define (break)
    (let ([break-info (continuation-mark-set->list (current-continuation-marks) 
                                                   annotate:debug-key)])
      (parameterize
          ([mred:current-eventspace drscheme-eventspace])
        (mred:queue-callback 
         (lambda ()
           (current-namespace (make-namespace))
           (global-defined-value 'break-info break-info)
           (global-defined-value 'break-resume (lambda (val) 
                                                 (set! break-resume-value val)
                                                 (semaphore-post break-semaphore)))
           (global-defined-value 'expose-mark marks:expose-mark)
           (global-defined-value 'display-mark marks:display-mark)
           (mred:graphical-read-eval-print-loop)))
        (semaphore-wait break-semaphore)
        break-resume-value))))