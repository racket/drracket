
(module get-extend mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
           "drsig.ss"
	   (lib "mred.ss" "mred")
           (lib "etc.ss"))
  
  (provide get-extend@)
  
  (define get-extend@
    (unit/sig drscheme:get/extend^

      (import [drscheme:unit : drscheme:unit^]
	      [drscheme:frame : drscheme:frame^]
	      [drscheme:rep : drscheme:rep^]
              [drscheme:debug : drscheme:debug^])
      
      (define make-extender
        (lambda (get-base% name)
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
                          name))
                 (set! extensions 
                       (if before?
                           (compose (verify extension) extensions)
                           (compose extensions (verify extension))))]))
             (lambda ()
               (unless built-yet?
                 (set! built-yet? #t)
                 (set! built (extensions (get-base%))))
               built)))))
      
      (define (get-base-interactions-canvas%)
        drscheme:unit:interactions-canvas%)
      
      (define-values (extend-interactions-canvas get-interactions-canvas)
        (make-extender get-base-interactions-canvas% 'interactions-canvas%))

      (define (get-base-definitions-canvas%)
        drscheme:unit:definitions-canvas%)
      
      (define-values (extend-definitions-canvas get-definitions-canvas)
        (make-extender get-base-definitions-canvas% 'definitions-canvas%))  
      
      (define (get-base-unit-frame%)
        (drscheme:debug:test-coverage-unit-frame-mixin
         (drscheme:debug:profile-unit-frame-mixin
          drscheme:unit:frame%)))

      (define-values (extend-unit-frame get-unit-frame)
        (make-extender get-base-unit-frame% 'drscheme:unit:frame))
      
      (define (get-base-interactions-text%)
        (drscheme:debug:test-coverage-interactions-text-mixin
         (drscheme:debug:profile-interactions-text-mixin
          drscheme:rep:text%)))

      (define-values (extend-interactions-text get-interactions-text)
        (make-extender get-base-interactions-text% 'interactions-text%))

      (define (get-base-definitions-text%)
        (drscheme:debug:test-coverage-definitions-text-mixin
         (drscheme:debug:profile-definitions-text-mixin
          (drscheme:unit:get-definitions-text%))))

      (define-values (extend-definitions-text get-definitions-text)
        (make-extender get-base-definitions-text% 'definitions-text%)))))
