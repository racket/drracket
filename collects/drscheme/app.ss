(define drscheme:application@
  (unit/sig mred:application^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [drscheme:project : drscheme:project^])
    
    (mred:debug:printf 'invoke "drscheme:application@")


    (mred:debug:printf 'super-init "before console")
    (define console (if mred:debug:on?
			(make-object mred:console-frame%)
			(make-object drscheme:project:scheme-project-frame%)))
    (mred:debug:printf 'super-init "after console")
    (define eval-string (if mred:debug:on?
			    (lambda args (void))
			    (ivar (ivar console console-edit) do-eval)))

    (mred:add-preference-callback 'drscheme:project-visible?
				  (lambda (p v) 
				    (send console show v) 
				    #t))

    (mzlib:pretty-print@:pretty-print-size-hook 
     (lambda (x _) (and (is-a? x wx:snip%) 1)))
    (mzlib:pretty-print@:pretty-print-print-hook
     (lambda (x _) 
       (let ([edit (ivar console console-edit)])
	 (send edit insert (send x copy) (send edit last-position)))))))