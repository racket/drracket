
(module get-extend mzscheme
  (require (lib "unitsig.ss")
           "drsig.ss"
           "mred-wrap.ss"
           (prefix mzlib:function (lib "etc.ss")))
  
  (provide get-extend@)
  
  (define get-extend@
    (unit/sig drscheme:get/extend^
      
      (define make-extender
        (lambda (base%)
          (let ([extensions (lambda (x) x)]
                [built-yet? #f]
                [built #f]
                [verify
                 (lambda (f)
                   (lambda (%)
                     (let ([new% (f %)])
                       (if (and (class? new%)
                                (subclass? new% %))
                           new%
                           (error 'extend-% "expected output of extension to create a subclass of its input, got: ~a"
                                  new%)))))])
            (values
             (rec add-extender
               (case-lambda
                [(extension) (add-extender extension #t)]
                [(extension before?)
                 (when built-yet?
                   (error 'extender "cannot build a new extension of ~a after initialization"
                          base%))
                 (set! extensions 
                       (if before?
                           (mzlib:function:compose (verify extension) extensions)
                           (mzlib:function:compose extensions (verify extension))))]))
             (lambda ()
               (unless built-yet?
                 (set! built-yet? #t)
                 (set! built (extensions base%)))
               built)))))
      
      (define-values (extend-interactions-canvas get-interactions-canvas%)
        (make-extender drscheme:unit:interactions-canvas%))
      
      (define-values (extend-definitions-canvas get-definitions-canvas%)
        (make-extender drscheme:unit:definitions-canvas%))  
      
      (define-values (extend-unit-frame get-unit-frame%)
        (make-extender drscheme:unit:frame%))
      
      (define-values (extend-interactions-text get-interactions-text%)
        (make-extender drscheme:rep:text%))
      
      (define-values (extend-definitions-text get-definitions-text%)
        (make-extender drscheme:unit:definitions-text%)))))