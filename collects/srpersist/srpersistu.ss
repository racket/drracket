(require-relative-library "sigs.ss")

(define srpersist@

  (if (defined? 'odbc-version)

      (let ([the-unit 
	     (load-relative-extension 
	      (string-append
	       (build-path 
		"lib"
		(system-library-subpath)
		(case (system-type)
		  [(unix) "srpmain.so"]
		  [(windows) "srpmain.dll"]
		  [(macos) "srpmain.so"]
		  [else (error "Unknown platform")]))))])
    
	(cond
	 
	 [(>= odbc-version 3.5)
	  (compound-unit/sig 
	   (import)
	   (link [srpersist : srpersist:odbc-3.5^
			    ((unit->unit/sig 
			      the-unit
			      ()
			      srpersist:odbc-3.5^))])
	   (export
	    (open srpersist)))]

	 [(>= odbc-version 3.0)
	  (compound-unit/sig 
	   (import)
	   (link [srpersist : srpersist:odbc-3.0^
			    ((unit->unit/sig 
			      the-unit
			      ()
			      srpersist:odbc-3.0^))])
	   (export
	    (open srpersist)))]

	 [(>= odbc-version 2.0)
	  (compound-unit/sig 
	   (import)
	   (link [srpersist : srpersist:odbc-2.0^
			    ((unit->unit/sig 
			      the-unit
			      ()
			      srpersist:odbc-2.0^))])
	   (export
	    (open srpersist)))]

	 [(>= odbc-version 1.0)
	  (compound-unit/sig 
	   (import)
	   (link [srpersist : srpersist:odbc-1.0^
			    ((unit->unit/sig 
			      the-unit
			      ()
			      srpersist:odbc-1.0^))])
	   (export
	    (open srpersist)))]))

      ; no ODBC version defined

      (error "odbc-version not defined")))
