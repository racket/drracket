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
  
  (define tool-path (build-path plt-home-directory "drscheme" "tools"))

  (define tools null)

  (define build-tool
    (lambda (directory)
      (when (directory-exists? (build-path tool-path directory))
	(let/ec k
	  (let ([unit-file (build-path tool-path directory "unit.ss")]
		[sig-file (build-path tool-path directory "sig.ss")])
	    (when (file-exists? sig-file)
	      (load/use-compiled sig-file))
	    (unless (file-exists? unit-file)
	      (printf "WARNING: expected to find file: ~a~n" unit-file)
	      (k (void)))
	    (let ([unit (load/use-compiled unit-file)])
	      (unless (unit/sig? unit)
		(printf "WARNING: expected to get a unit from: ~a, got: ~a"
			unit-file unit)
		(k (void)))
	      (invoke-unit/sig unit 
			       mred^
			       mzlib:core^
			       mzlib:print-convert^
			       drscheme:export^
			       zodiac:system^
			       plt:parameters^)))))))

  (for-each build-tool (directory-list tool-path))


  '(for-each 
   (lambda (x)
     (letrec* ([id #f])
       (set! id (send tools-menu append-item
		      (drscheme:tool:tool-name x)
		      (let ([c (drscheme:tool:tool-callback x)])
			(lambda ()
			  (c this)))))))
   drscheme:tool:tools)

  (define-struct tool (name file callback))
  
  '(define tools
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
  
  '(define load/invoke-tool
    (lambda (filename)
      (file@:load-recent (build-path plt-home-directory filename))
      (invoke-unit/sig (global-defined-value 'tool@) 
		       mred^
		       mzlib:core^
		       mzlib:print-convert^
		       drscheme:export^
		       zodiac:system^
		       plt:parameters^))))
