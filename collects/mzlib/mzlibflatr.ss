(compound-unit/sig
   (import)
   (link [core : mzlib:core-flat^ ((require-library-unit/sig "coreflatr.ss"))]
	 [compat : mzlib:compat^ ((require-library-unit/sig "compatr.ss") (core : mzlib:function^))]
	 [convert : mzlib:print-convert^
		   ((require-library-unit/sig "pconverr.ss") 
		    (core : mzlib:string^) 
		    (core : mzlib:function^))]
	 [date : mzlib:date^ ((require-library-unit/sig "dater.ss")
			       (core : mzlib:function^))]
	 [inflate : mzlib:inflate^ ((require-library-unit/sig "inflater.ss"))]
	 [command-line : mzlib:command-line^ ((require-library-unit/sig "cmdliner.ss"))]
	 [restart : mzlib:restart^ ((require-library-unit/sig "restartr.ss")
				     command-line)]
	 [transcript : mzlib:transcript^ ((require-library-unit/sig "transcrr.ss"))])
   (export (open core)
	   (open compat)
	   (open convert)
	   (open date)
	   (open inflate)
	   (open command-line)
	   (open restart)
	   (open transcript)))

	 