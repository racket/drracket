#
# Makefile for mzscheme basic sources
#

EXPORT= CC='$(CC)' CXX='$(CXX)' AS='$(AS)' RANLIB='$(RANLIB)' PERL='$(PERL)' DEBUGFLAGS='$(DEBUGFLAGS)' OPTIONS='$(OPTIONS)' DYNOPTIONS='$(DYNOPTIONS)' WARN='$(WARN)' OPT='$(OPT)' OBJDIR='$(OBJDIR)' GCDIR='$(GCDIR)' LIBS='$(LIBS)' GCLIBMAKEEXTRA='$(GCLIBMAKEEXTRA)'

XMAKE = $(MAKE) $(EXPORT)

CFLAGS_NODBG = $(OPT) $(WARN) $(OPTIONS) $(THREADFLAGS) -I../include
CFLAGS = $(DEBUGFLAGS) $(CFLAGS_NODBG)

OBJS =  $(OBJDIR)/salloc.o \
	$(OBJDIR)/bignum.o \
	$(OBJDIR)/bool.o \
	$(OBJDIR)/char.o \
	$(OBJDIR)/complex.o \
	$(OBJDIR)/dynext.o \
	$(OBJDIR)/env.o \
	$(OBJDIR)/error.o \
	$(OBJDIR)/eval.o \
	$(OBJDIR)/file.o \
	$(OBJDIR)/fun.o \
	$(OBJDIR)/hash.o \
	$(OBJDIR)/image.o \
	$(OBJDIR)/list.o \
	$(OBJDIR)/number.o \
        $(OBJDIR)/object.o \
	$(OBJDIR)/port.o \
	$(OBJDIR)/print.o \
	$(OBJDIR)/process.o \
	$(OBJDIR)/promise.o \
        $(OBJDIR)/rational.o \
	$(OBJDIR)/read.o \
	$(OBJDIR)/regexp.o \
	$(OBJDIR)/sema.o \
	$(OBJDIR)/setjmpup.o \
	$(OBJDIR)/string.o \
	$(OBJDIR)/struct.o \
	$(OBJDIR)/symbol.o \
	$(OBJDIR)/syntax.o \
	$(OBJDIR)/type.o \
        $(OBJDIR)/unit.o \
	$(OBJDIR)/vector.o

SRCS =  salloc.c \
	bignum.c \
	bool.c \
	char.c \
	complex.c \
	dynext.c \
	env.c \
	error.c \
	eval.c \
	file.c \
	fun.c \
	hash.c \
	image.c \
	list.c \
	number.c \
	object.c \
	port.c \
	print.c \
	process.c \
	promise.c \
	rational.c \
	read.c \
	regexp.c \
	sema.c \
	setjmpup.c \
	string.c \
	struct.c \
	symbol.c \
	syntax.c \
	type.c \
        unit.c \
	vector.c

wrong:
	# Make from the directory above this one

all: objects $(OBJDIR)
	$(XMAKE) mzheaders
	$(XMAKE) mzobjects

$(OBJDIR):
	mkdir $(OBJDIR)
objects:
	mkdir objects

mzheaders: schemex.h schemexm.h schemex.inc schexn.h $(OBJDIR)/schsys.h \
           ../include/mzscheme.exp ../include/mzwin.def ../include/gmzwin.def

mzobjects: $(OBJS)

macro.inc : macro.ss sstoinc
	if [ "$(PERL)" = '' ] ; then touch macro.inc ; else $(PERL) ./sstoinc < macro.ss > macro.inc ; fi

unitsig.inc : unitsig.ss sstoinc
	if [ "$(PERL)" = '' ] ; then touch unitsig.inc ; else $(PERL) ./sstoinc < unitsig.ss > unitsig.inc ; fi

schemex.h : schemef.h makex
	if [ "$(PERL)" = '' ] ; then touch schemex.h ; else $(PERL) ./makex < schemef.h > schemex.h ; fi

schemexm.h : schemef.h makex
	if [ "$(PERL)" = '' ] ; then touch schemexm.h ; else $(PERL) -s -- ./makex -list < schemef.h > schemexm.h ; fi

schemex.inc : schemef.h makex
	if [ "$(PERL)" = '' ] ; then touch schemex.inc ; else $(PERL) -s -- ./makex -assign < schemef.h > schemex.inc ; fi

../include/mzscheme.exp : schemef.h makex
	if [ "$(PERL)" = '' ] ; then touch ../include/mzscheme.exp ; else $(PERL) -s -- ./makex -exports < schemef.h > ../include/mzscheme.exp ; fi

../include/mzwin.def : schemef.h makex
	if [ "$(PERL)" = '' ] ; then touch ../include/mzwin.def ; else $(PERL) -s -- ./makex -winex < schemef.h > ../include/mzwin.def ; fi

../include/gmzwin.def : schemef.h makex
	if [ "$(PERL)" = '' ] ; then touch ../include/gmzwin.def ; else $(PERL) -s -- ./makex -gwinex < schemef.h > ../include/gmzwin.def ; fi

$(OBJDIR)/schsys.h : sysname
	$(MAKE) objects
	$(MAKE) $(OBJDIR)
	echo -n "#define SCHEME_PLATFORM_LIBRARY_SUBPATH " > $(OBJDIR)/schsys.h
	./sysname >> $(OBJDIR)/schsys.h

depend:
	makedepend -- $(CFLAGS) -- $(SRCS)

clean:
	/bin/rm -f objects/*/* *~ Makefile.bak $(OBJDIR)/schsys.h

$(OBJDIR)/salloc.o: salloc.c
	$(CC) $(CFLAGS) -c salloc.c -o $(OBJDIR)/salloc.o
$(OBJDIR)/bignum.o: bignum.c bgnfloat.inc
	$(CC) $(CFLAGS) -c bignum.c -o $(OBJDIR)/bignum.o
$(OBJDIR)/bool.o: bool.c
	$(CC) $(CFLAGS) -c bool.c -o $(OBJDIR)/bool.o
$(OBJDIR)/char.o: char.c
	$(CC) $(CFLAGS) -c char.c -o $(OBJDIR)/char.o
$(OBJDIR)/complex.o: complex.c
	$(CC) $(CFLAGS) -c complex.c -o $(OBJDIR)/complex.o
$(OBJDIR)/dynext.o: dynext.c
	$(CC) $(CFLAGS) -c dynext.c -o $(OBJDIR)/dynext.o
$(OBJDIR)/env.o: env.c
	$(CC) $(CFLAGS) -c env.c -o $(OBJDIR)/env.o
$(OBJDIR)/error.o: error.c
	$(CC) $(CFLAGS) -c error.c -o $(OBJDIR)/error.o
$(OBJDIR)/eval.o: eval.c schapp.inc
	$(CC) $(CFLAGS) -c eval.c -o $(OBJDIR)/eval.o
$(OBJDIR)/file.o: file.c
	$(CC) $(CFLAGS) -c file.c -o $(OBJDIR)/file.o
$(OBJDIR)/fun.o: fun.c
	$(CC) $(CFLAGS) -c fun.c -o $(OBJDIR)/fun.o
$(OBJDIR)/hash.o: hash.c
	$(CC) $(CFLAGS) -c hash.c -o $(OBJDIR)/hash.o
$(OBJDIR)/image.o: image.c
	$(CC) $(CFLAGS) -c image.c -o $(OBJDIR)/image.o
$(OBJDIR)/list.o: list.c
	$(CC) $(CFLAGS) -c list.c -o $(OBJDIR)/list.o
$(OBJDIR)/number.o: number.c
	$(CC) $(CFLAGS) -c number.c -o $(OBJDIR)/number.o
$(OBJDIR)/object.o: object.c
	$(CC) $(CFLAGS) -c object.c -o $(OBJDIR)/object.o
$(OBJDIR)/port.o: port.c
	$(CC) $(CFLAGS) -c port.c -o $(OBJDIR)/port.o
$(OBJDIR)/print.o: print.c
	$(CC) $(CFLAGS) -c print.c -o $(OBJDIR)/print.o
$(OBJDIR)/process.o: process.c
	$(CC) $(CFLAGS) -c process.c -o $(OBJDIR)/process.o
$(OBJDIR)/promise.o: promise.c
	$(CC) $(CFLAGS) -c promise.c -o $(OBJDIR)/promise.o
$(OBJDIR)/rational.o: rational.c ratfloat.inc
	$(CC) $(CFLAGS) -c rational.c -o $(OBJDIR)/rational.o
$(OBJDIR)/read.o: read.c
	$(CC) $(CFLAGS) -c read.c -o $(OBJDIR)/read.o
$(OBJDIR)/regexp.o: regexp.c
	$(CC) $(CFLAGS) -c regexp.c -o $(OBJDIR)/regexp.o
$(OBJDIR)/sema.o: sema.c
	$(CC) $(CFLAGS) -c sema.c -o $(OBJDIR)/sema.o
$(OBJDIR)/setjmpup.o: setjmpup.c
	$(CC) $(CFLAGS) -c setjmpup.c -o $(OBJDIR)/setjmpup.o
$(OBJDIR)/string.o: string.c
	$(CC) $(CFLAGS) -I$(OBJDIR) -c string.c -o $(OBJDIR)/string.o
$(OBJDIR)/struct.o: struct.c
	$(CC) $(CFLAGS) -c struct.c -o $(OBJDIR)/struct.o
$(OBJDIR)/symbol.o: symbol.c
	$(CC) $(CFLAGS) -c symbol.c -o $(OBJDIR)/symbol.o
$(OBJDIR)/syntax.o: syntax.c
	$(CC) $(CFLAGS) -c syntax.c -o $(OBJDIR)/syntax.o
$(OBJDIR)/type.o: type.c
	$(CC) $(CFLAGS) -c type.c -o $(OBJDIR)/type.o
$(OBJDIR)/unit.o: unit.c
	$(CC) $(CFLAGS) -c unit.c -o $(OBJDIR)/unit.o
$(OBJDIR)/vector.o: vector.c
	$(CC) $(CFLAGS) -c vector.c -o $(OBJDIR)/vector.o

SCONFIG =   ../sconfig.h ../uconfig.h

# Note: 'make depend' is no longer required. As distributed, the below
#       dependencies have been trimmed to include only dependencies
#       on MzScheme files

# DO NOT DELETE THIS LINE -- make depend depends on it.

$(OBJDIR)/salloc.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
            ../gc/gc.h
$(OBJDIR)/bignum.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
          ../src/stypes.h    
$(OBJDIR)/bool.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
          ../src/stypes.h mzeqchk.inc
$(OBJDIR)/char.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h  
$(OBJDIR)/complex.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h  \
         ../src/stypes.h    
$(OBJDIR)/dynext.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h  \
         ../src/stypes.h ../src/schemex.h \
         schvers.h ../gc/gc.h schemex.h
$(OBJDIR)/env.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h  \
        ../src/stypes.h  schminc.h macro.inc cmacro.inc
$(OBJDIR)/error.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h  
$(OBJDIR)/eval.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h   \
          schmach.h mzstkchk.h schrunst.h
$(OBJDIR)/file.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h  
$(OBJDIR)/fun.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h   
$(OBJDIR)/hash.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h  
$(OBJDIR)/image.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h  schvers.h
$(OBJDIR)/list.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h  
$(OBJDIR)/number.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h nummacs.h random.inc
$(OBJDIR)/object.o: ../include/scheme.h  ../src/stypes.h \
          schpriv.h schexn.h $(SCONFIG) \
        ../gc/gc.h  schrunst.h
$(OBJDIR)/unit.o: ../include/scheme.h  ../src/stypes.h  \
         schpriv.h schexn.h $(SCONFIG) schrunst.h schminc.h \
         unitsig.inc cunitsig.inc
$(OBJDIR)/port.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h  schfd.h
$(OBJDIR)/print.o: ../include/scheme.h ../src/stypes.h ../src/schcpt.h \
          schpriv.h schexn.h $(SCONFIG)
$(OBJDIR)/process.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h  schfd.h
$(OBJDIR)/promise.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h  
$(OBJDIR)/rational.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h    
$(OBJDIR)/read.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h ../src/schcpt.h \
        ../src/stypes.h  
$(OBJDIR)/regexp.o:  schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h  
$(OBJDIR)/setjmpup.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h   schmach.h
$(OBJDIR)/string.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h    schvers.h
$(OBJDIR)/struct.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h  
$(OBJDIR)/symbol.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h   
$(OBJDIR)/syntax.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h  
$(OBJDIR)/sema.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h  
$(OBJDIR)/type.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
        ../src/stypes.h  
$(OBJDIR)/vector.o: schpriv.h schexn.h $(SCONFIG) ../include/scheme.h \
         ../src/stypes.h  
