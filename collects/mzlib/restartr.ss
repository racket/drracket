
(unit/sig mzlib:restart^
  (import mzlib:command-line^)

  (define (restart-mzscheme init-argv adjust-flag-table argv init-namespace)
    (let* ([args #f]
	   [mute-banner? #f]
	   [no-rep? #f]
	   [no-coll-paths? #f]
	   [no-init-file? #f]
	   [case-sensitive? #f]
	   [esc-cont-only? #f]
	   [allow-set!-undefined? #t]
	   [no-auto-else? #f]
	   [no-enforce-keywords? #f]
	   [hp-only? #f]
	   [print-error
	    (lambda (e)
	      (if (exn? e)
		  (fprintf (current-error-port) "~a~n" (exn-message e))
		  (fprintf (current-error-port) "Exception in init file: ~e~n" e)))]
	   [table
	    `([multi
	       [("-e")
		,(lambda (f expr) expr)
		("Evaluates <expr>" "expr")]
	       [("-f")
		,(lambda (f file) (format "(load ~s)" file))
		("Loads <file>" "file")]
	       [("-f")
		,(lambda (f file) (format "(load/cd ~s)" file))
		("Load/cds <file>" "file")]
	       [("-F")
		,(lambda (f . files) (map (lambda (file)
					    (format "(load ~s)" file))
					  files))
		("Loads all <file>s" "file")]
	       [("-D")
		,(lambda (f . files) (map (lambda (file)
					    (format "(load/cd ~s)" file))
					  files))
		("Load/cds all <file>s" "file")]
	       [("-l")
		,(lambda (f file) (format "(require-library ~s)" file))
		("Requires library <file>" "file")]
	       [("-L")
		,(lambda (f file collection) (format "(require-library ~s ~s)" file collection))
		("Requires library <file> in <collection>" "file" "collection")]
	       [("-r" "--script")
		,(lambda (f file . rest) 
		   (format "(load ~s)" file)
		   (set! mute-banner? #t)
		   (set! no-rep? #t)
		   (set! args rest))
		("Same as -fmv-" "file" "arg")]
	       [("-i" "--script-cd")
		,(lambda (f file . rest) 
		   (format "(load/cd ~s)" file)
		   (set! mute-banner? #t)
		   (set! no-rep? #t)
		   (set! args rest))
		("Same as -dmv-" "file" "arg")]
	       [("-w" "--awk")
		,(lambda (f) "(require-library \"awk.ss\")")
		("Same as -l awk.ss")]
	       [("-x" "--no-init-path")
		,(lambda (f) (set! no-coll-paths? #t))
		("Don't set current-library-collection-paths")]
	       [("-q" "--no-init-file")
		,(lambda (f) (set! no-init-file? #t))
		("Don't load \"~/.mzschemerc\" or \"mzscheme.rc\"")]
	       [("-g" "--case-sens")
		,(lambda (f) (set! case-sensitive? #t))
		("Identifiers and symbols are initially case-sensitive")]
	       [("-c" "--esc-cont")
		,(lambda (f) (set! esc-cont-only? #t))
		("Call/cc is replaced with call/ec")]
	       [("-s" "--set-undef")
		,(lambda (f) (set! allow-set!-undefined? #t))
		("Set! works on undefined identifiers")]
	       [("-a" "--no-auto-else")
		,(lambda (f) (set! no-auto-else? #t))
		("Fall-through cond or case is an error")]
	       [("-n" "--no-key")
		,(lambda (f) (set! no-enforce-keywords? #t))
		("Keywords are not enforced")]
	       [("-y" "--hash-percent-syntax")
		,(lambda (f) (set! hp-only? #t))
		("Only #% syntactic forms are present")]
	       [("-m" "--mute-banner")
		,(lambda (f) (set! mute-banner? #t))
		("Suppresses the startup banner text")]
	       [("-v" "--version")
		,(lambda (f) (set! no-rep? #t))
		("Suppresses the read-eval-print loop")]
	       [("--restore")
		,(lambda (f) (error 'mzscheme "The --restore flag is not supported in this mode"))
		("Not supported")]])])
      (parse-command-line
       "mzscheme"
       argv
       table
       void
       '("ignored"))
      (set! args #f)
      (parse-command-line
       "mzscheme"
       argv
       (adjust-flag-table table)
       (lambda (exprs . rest)
	 (unless (null? rest)
	   (set! args rest))
	 ;(when args (set! rest args))
	 (let ([n (make-namespace
		   (if no-enforce-keywords? 'no-keywords 'keywords)
		   (if esc-cont-only? 'call/cc=call/ec 'call/cc!=call/ec)
		   (if hp-only? 'hash-percent-syntax 'all-syntax))])
	   (thread-wait
	    (thread
	     (lambda ()
	       (current-namespace n)
	       (let ([program (with-handlers ([void (lambda (x) "MzScheme")])
				(global-defined-value 'program))])
		 (read-case-sensitive case-sensitive?)
		 (compile-allow-set!-undefined allow-set!-undefined?)
		 (compile-allow-cond-fallthrough (not no-auto-else?))
		 
		 (unless mute-banner? (display (banner)))
		 
		 (eval `(#%define-values (argv) (#%quote ,(if args (list->vector args) (vector)))))
		 (eval `(#%define-values (program) (#%quote ,program)))
		 
		 (current-library-collection-paths
		  (if no-coll-paths?
		      #f
		      (path-list-string->path-list 
		       (or (getenv "PLTCOLLECTS") "")
		       (or
			(ormap
			 (lambda (f) (let ([p (f)]) (and p (directory-exists? p) (list p))))
			 (list
			  (lambda () (let ((v (getenv "PLTHOME")))
				       (and v (build-path v "collects"))))
			  (lambda () (find-executable-path program "collects"))
			  (lambda ()
			    (case (system-type)
			      [(unix beos) "/usr/local/lib/plt/collects"]
			      [(windows) "c:\\plt\\collects"]
			      [else #f]))))
			null)))))
	       
	       (init-namespace)
	       
	       (unless no-init-file?
		 (let ([f (case (system-type)
			    [(unix beos) "~/.mzschemerc"]
			    [else "mzscheme.rc"])])
		   (when (file-exists? f)
		     (with-handlers ([void print-error])
		       (load f)))))
	       
	       (let ([result
		      (let/ec escape
			(for-each
			 (lambda (e)
			   (with-handlers ([void (lambda (e) (print-error e) (escape #f))])
			     (eval (read (open-input-string e)))))
			 exprs)
			#t)])
		 (let/ec k
		   (exit-handler
		    (lambda (status)
		      (when result
			(set! result (= status 0)))
		      (k #f)))
		   (unless no-rep? (read-eval-print-loop)))
		 result))))))
	 `("arg")))))
