(unit/sig drscheme:tool^
  (import mred^
	  mzlib:core^
	  mzlib:print-convert^ 
	  zodiac:system^
	  plt:parameters^
	  [drscheme:frame : drscheme:frame^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:compound-unit : drscheme:compound-unit^])
  
  (mred:debug:printf 'invoke "drscheme:tool@")

  (define-struct tool (name file callback))
  
  (define tools
    (map (lambda (x)
	   (let* ([name (car x)]
		  [filename (cadr x)]
		  [callback
		   (letrec
		       ([f
			 (lambda (frame)
			   (let ([new-callback (load/invoke-tool filename)])
			     (set! f (if (procedure? new-callback)
					 new-callback
					 (lambda (frame) (wx:bell))))
			     (f frame)))])
		     (lambda (frame) (f frame)))])
	     (make-tool name filename callback)))
	 (global-defined-value 'drscheme:toplevel-tools)))
  
  (define load/invoke-tool
    (lambda (filename)
      (file@:load-recent (build-path plt-home-directory filename))
      (invoke-unit/sig (global-defined-value 'tool@) 
		       mred^
		       mzlib:core^
		       mzlib:print-convert^
		       drscheme:export^
		       zodiac:system^
		       plt:parameters^))))
