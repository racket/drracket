; $Id: link.ss,v 1.17 2000/01/02 23:28:25 robby Exp $

(compound-unit/sig
  (import
    (INTERFACE : zodiac:interface^)
    (PRETTY : mzlib:pretty-print^)
    (MZLIB-FILE : mzlib:file^))
  (link
    [NEW-INTERFACE : zodiac:interface^
      ((unit/sig zodiac:interface^
	 (import (real : zodiac:interface^))
	 (define static-error
	   (case-lambda
	     [(link-text link-tag source-term fmt-spec . args)
	       (apply real:static-error
		 source-term
		 (string-append link-text ": " fmt-spec)
		 args)]
	     [(where fmt-spec . args)
	       (real:internal-error where
		 "static-error interface has changed: called with ~s, ~s"
		 fmt-spec args)]))
	 (define internal-error real:internal-error))
	INTERFACE)]
    [REAL-LINKER : zodiac:system^
      ((require-relative-library-unit/sig "link2.ss")
	NEW-INTERFACE
	PRETTY
	MZLIB-FILE)])
  (export (open REAL-LINKER)))
