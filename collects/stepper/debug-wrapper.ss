(unit/sig plt:aries-no-break^
  (import [zodiac : zodiac:system^]
          [utils : stepper:cogen-utils^]
          [marks : stepper:marks^]
          [annotate : stepper:annotate^])
  
  (define w-c-m-key annotate:debug-key)
  
  (define current-environments #f)
  
  (define (annotate sexp zodiac-read)
    (let-values 
        ([(annotateds new-envs)
          (annotate:annotate (and zodiac-read (list zodiac-read)) 
                             (list sexp) 
                             current-environments 
                             #f
                             #t)])
      (set! current-environments new-envs)
      (car annotateds)))
  
  (define (extract-zodiac-location mark-set)
    (let ([mark-list (continuation-mark-set->list mark-set annotate:debug-key)])
      (if (null? mark-list)
          #f
          (marks:mark-source (car mark-list)))))
  
  (define (make-zodiac-mark location)
    (marks:make-cheap-mark location))
    
  (define signal-not-boolean utils:signal-not-boolean)
  (define signal-undefined utils:signal-undefined)
  
  ; initialization --- should be called once per execute
  ; (except that (2000-02-20) it doesn't matter anyway because
  ; these environments are totally irrelevant to non-stepper
  ; use of the annotater.
  (set! current-environments annotate:initial-env-package))

  