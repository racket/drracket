
MZSCHEME=../../mzscheme/${MZBUILDDIR}mzscheme -e "(use-compiled-file-kinds 'none)"

all: ../wxs/wrap.inc ../wxs/cwrap.inc

again:
	rm wrap.ss
	$(MAKE) all

../wxs/wrap.inc: wrap.ss
	perl ../../mzscheme/src/sstoinc < wrap.ss > ../wxs/wrap.inc

../wxs/cwrap.inc: ../wxs/wrap.inc ../../mzscheme/src/schvers.h
	$(MZSCHEME) -gqrna ../../mzscheme/src/sstoinc.ss < wrap.ss > ../wxs/cwrap.inc

wrap.ss: import.ss export.ss propgate.ss mred.ss mkwrap.ss
	$(MZSCHEME) -qmvqr mkwrap.ss
