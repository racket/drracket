(compound-unit/sig
   (import)
   (link [core : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
	 [compat : mzlib:compat^ ((require-library-unit/sig "compatr.ss") (core function))]
	 [convert : mzlib:print-convert^
		   ((require-library-unit/sig "pconverr.ss") 
		    (core string) 
		    (core function))]
	 [date : mzlib:date^ ((require-library-unit/sig "dater.ss")
			       (core function))]
	 [inflate : mzlib:inflate^ ((require-library-unit/sig "inflater.ss"))]
	 [command-line : mzlib:command-line^ ((require-library-unit/sig "cmdliner.ss"))]
	 [restart : mzlib:restart^ ((require-library-unit/sig "restartr.ss")
				     command-line)]
	 [transcript : mzlib:transcript^ ((require-library-unit/sig "transcrr.ss"))])
   (export (open core)
	   (open compat)
	   (unit convert)
	   (unit date)
	   (unit inflate)
	   (unit command-line)
	   (unit restart)
	   (unit transcript)))

	 