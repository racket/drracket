#
# Makefile for mzscheme
#
# 'make' or 'make bin' - makes mzscheme
# 'make mzschemelib' - make libmzscheme.a and libgc.a
# 'make mzschemelib++' - make libmzscheme.a and libgc.a with C++ GC tools
# 'make libguile' - make libguile.a
# 'make dynlib' - make mzdyn.o, for use in extending MzScheme
# 'make g-c' - makes only libgc.a

# SEE ALSO sconfig.h for more configuration

# This must be an ANSI C compiler.
CC=gcc
CXX=gcc
AS=gcc -c -x assembler-with-cpp

# Debugging flags
DEBUGFLAGS = -g

# Other options; mainly used to import other options
#  Add -DUSE_SENORA_GC for SenoraGC
#  Add -DSOLARIS_THREADS to THREADFLAGS for Solaris threads
#  Add -DIRIX_SPROCS to THREADFLAGS for Irix sprocs
OPTIONS = 
NONLINKOPTIONS = 
THREADFLAGS = 

# Options required for dynamically-linkable compilation
DYNOPTIONS =

# Optimization
OPT = -O2

# Warnings
WARN = -Wall 

# The math library is needed for the numeric functions
#  in number.c.
# You may need -ldl for dlopen, etc.  (Added automatically for AIX.)
# Add lib$(MZPLATFORMSUFFIX)$(MZTHREADSUFFIX)/sproc.o for Irix
#  sprocs
LIBS=-lm # -ldl

# If your system needs ranlib, put it here.  Otherwise,
# use a colon.
RANLIB=ranlib

# GC Directory
# sgc for SenoraGC
GCDIR = gc

# Objects directory
OBJDIR = objects/sys$(MZPLATFORMSUFFIX)$(MZTHREADSUFFIX)

# Library directory
LIBDIR = lib/sys$(MZPLATFORMSUFFIX)$(MZTHREADSUFFIX)

# Binary
MZSCHEME = $(MZBUILDDIR)mzscheme

# Extra makes needed for gc lib
# Change to sproc-extra-lib for Irix sprocs
GCLIBMAKEEXTRA = no-extra-lib

################################################################################

# Destination for support files installation (Probably PLT only)
INSTALLDEST = /home/scheme/plt

# Export string for making src & GC
EXPORT= CC='$(CC)' CXX='$(CXX)' AS='$(AS)' RANLIB='$(RANLIB)' DEBUGFLAGS='$(DEBUGFLAGS)' OPTIONS='$(OPTIONS) $(THREADFLAGS)' DYNOPTIONS='$(DYNOPTIONS)' WARN='$(WARN)' OPT='$(OPT)' OBJDIR='$(OBJDIR)' GCDIR='$(GCDIR)' LIBS='$(LIBS)' GCLIBMAKEEXTRA='$(GCLIBMAKEEXTRA)'

MAIN_CFLAGS_NODBG = $(WARN) $(OPT) $(FLAGS) $(OPTIONS) $(THREADFLAGS) -Iinclude
MAIN_CFLAGS = $(DEBUGFLAGS) $(NONLINKOPTIONS) $(MAIN_CFLAGS_NODBG)

XMAKE = $(MAKE) $(EXPORT)

bin: 
	$(XMAKE) g-c
	./ifaix $(XMAKE) rs6kbin
	./ifnotaix $(XMAKE) xbin

rs6kbin:
	./ifaix $(XMAKE) aixdl
	$(XMAKE) LIBS='$(LIBS) -ldl -lld -Wl,-bE:include/mzscheme.exp' xbin

xbin:
	$(XMAKE) mzlibrary
	$(XMAKE) $(MZSCHEME) 

mzschemelib: 
	./ifaix $(XMAKE) aixdl
	$(XMAKE) g-c 
	$(XMAKE) mzlibrary 

mzschemelib++:
	./ifaix $(XMAKE) aixdl
	$(XMAKE) g-c 
	cd $(GCDIR); $(XMAKE) $(OBJDIR)/gc_cpp.o 
	$(XMAKE) $(GCDIR)/$(OBJDIR)/gc.a
	$(XMAKE) $(LIBDIR)/libgc.a
	$(XMAKE) mzlibrary

libguile:
	$(XMAKE) mzschemelib
	cd guile; $(XMAKE)

$(LIBDIR): 
	mkdir $(LIBDIR)
lib:
	mkdir lib

$(OBJDIR): 
	mkdir $(OBJDIR)
objects:
	mkdir objects

dynlib:
	cd dynsrc; $(XMAKE) LIBDIR='$(LIBDIR)' -e dynlib 

dynexample:
	$(XMAKE) dynlib
	cd dynsrc; $(XMAKE) LIBDIR='$(LIBDIR)' -e dynexample 

withwin:
	$(XMAKE) dynlib
	cd dynsrc; $(XMAKE) -e withwin 

oe:
	$(XMAKE) dynlib
	cd dynsrc; $(XMAKE) -e oe

aixdl:
	cd aixdlfcn; $(MAKE) CC=cc
	$(XMAKE) $(LIBDIR)/libdl.a

g-c: $(GCDIR)/objects $(GCDIR)/$(OBJDIR)
	cd $(GCDIR); $(XMAKE) -e $(OBJDIR)/gc.a 
	$(XMAKE) $(LIBDIR)/libgc.a
	$(XMAKE) $(GCLIBMAKEEXTRA)

no-extra-lib: # Do nothing

sproc-extra-lib:
	cd $(GCDIR); $(XMAKE) -e $(OBJDIR)/sproc.o
	$(XMAKE) $(LIBDIR)/sproc.o

$(GCDIR)/$(OBJDIR): 
	mkdir $(GCDIR)/$(OBJDIR)
gc/objects:
	mkdir gc/objects
sgc/objects:
	mkdir sgc/objects

$(GCDIR)/$(OBJDIR)/gc.a: $(GCDIR)/$(OBJDIR)/*.o
	$(AR) ruv $(GCDIR)/$(OBJDIR)/gc.a $(GCDIR)/$(OBJDIR)/*.o  

mzlibrary: lib $(LIBDIR) 
	cd src; $(XMAKE) -e all 
	$(XMAKE) $(LIBDIR)/libmzscheme.a

$(LIBDIR)/libmzscheme.a: src/$(OBJDIR)/*.o
	$(AR) ruv $(LIBDIR)/libmzscheme.a src/$(OBJDIR)/*.o
	$(RANLIB) $(LIBDIR)/libmzscheme.a

$(LIBDIR)/libgc.a: lib $(LIBDIR) $(GCDIR)/$(OBJDIR)/gc.a
	cp $(GCDIR)/$(OBJDIR)/gc.a $(LIBDIR)/libgc.a
	$(RANLIB) $(LIBDIR)/libgc.a

# Irix sproc, only
$(LIBDIR)/sproc.o: lib $(LIBDIR) $(GCDIR)/$(OBJDIR)/sproc.o
	cp $(GCDIR)/$(OBJDIR)/sproc.o $(LIBDIR)/sproc.o

$(LIBDIR)/libdl.a: aixdlfcn/libdl.a
	cp aixdlfcn/libdl.a $(LIBDIR)/libdl.a
$(GCDIR)/$(OBJDIR)/gc_cpp.o:
	cd $(GCDIR); $(XMAKE) -e c++ 

$(MZSCHEME): objects $(OBJDIR) $(LIBDIR)/libmzscheme.a $(LIBDIR)/libgc.a $(OBJDIR)/main.o $(SPECIALIZINGOBJECTS)
	$(CC) $(MAIN_CFLAGS_NODBG) -o $(MZSCHEME) $(OBJDIR)/main.o $(SPECIALIZINGOBJECTS) -L$(LIBDIR)/ -lmzscheme -lgc $(LIBS)

$(OBJDIR)/main.o: main.c include/scheme.h sconfig.h
	$(CC) $(MAIN_CFLAGS) -c main.c -o $(OBJDIR)/main.o

testext.o: testext.c
	$(CC) $(MAIN_CFLAGS) -I/home/scheme/plt/compiler/ -c testext.c -o testext.o

CMACRODEST = cmacro.inc

cmacro:
	$(XMAKE) src/$(CMACRODEST)
	$(XMAKE) src/cunitsig.inc

headers:
	$(XMAKE) ../collects/mzscheme/include/scheme.h

../collects/mzscheme/include/scheme.h: sconfig.h uconfig.h include/escheme.h src/schemef.h \
                                       src/stypes.h src/schemex.h src/schemexm.h src/schexn.h \
                                       include/ext.exp include/mzscheme.exp include/scheme.h
	rm -rf ../collects/mzscheme/include
	mkdir ../collects/mzscheme/include
	cp sconfig.h uconfig.h include/escheme.h src/schemef.h \
          src/stypes.h src/schemex.h src/schemexm.h src/schexn.h \
          include/ext.exp include/mzscheme.exp ../collects/mzscheme/include 
	perl -ne 's/\/\*III\*\//\#define INCLUDE_WITHOUT_PATHS/; print' \
          < include/scheme.h > ../collects/mzscheme/include/scheme.h

src/$(CMACRODEST): src/macro.ss src/schvers.h
	cd src; ../$(MZSCHEME) -gqrna sstoinc.ss $(CMACROEXTRA) < macro.ss > $(CMACRODEST)

src/cunitsig.inc: src/unitsig.ss src/schvers.h
	cd src; ../$(MZSCHEME) -gqrna sstoinc.ss < unitsig.ss > cunitsig.inc

cmacro.zo:
	$(XMAKE) cmacro CMACROEXTRA='zo' CMACRODEST="../cmacro.zo"

zo:
	rm -f ../collects/mzlib/compiled/*.zo
	rm -f ../collects/mzlib/compiled/*/*.so
	$(MZSCHEME) -qf mkmzlib.ss -e '(make-zo)' -e '(exit)'

depend:
	cd src; $(XMAKE) -e depend 
	cd dynsrc; $(XMAKE) -e depend 

tmpclean:
	/bin/rm -rf tmp[123456789] tests/tmp[123456789] tests/sub[123]

tests-clean:
	$(XMAKE) tmpclean
	/bin/rm -f tests/*~

clean:
	$(XMAKE) tmpclean
	$(XMAKE) tests-clean
	/bin/rm -f objects/*/*.o *~ Makefile.bak utils/*~ include/*~ ../collects/mzlib/*~ core
	/bin/rm -f aixdlfcn/dlfcn.o aixdlfcn/libdl.a 
	cd gc; $(XMAKE) clean
	cd sgc; $(XMAKE) clean
	cd src; $(XMAKE) clean
	cd dynsrc; $(XMAKE) clean
	cd aixdlfcn; $(XMAKE) clean
	cd guile; $(XMAKE) clean

# exile & repatriate for my development process

HIDEDIR = /home/mflatt/mredhide

exile: lib objects gc/objects sgc/objects
	mkdir $(HIDEDIR)/mzscheme_lib
	mv lib $(HIDEDIR)/mzscheme_lib
	mkdir $(HIDEDIR)/mzscheme_main_objects
	mv objects $(HIDEDIR)/mzscheme_main_objects
	mv .bin $(HIDEDIR)/mzscheme_main_objects
	mkdir $(HIDEDIR)/mzscheme_objects
	mv src/objects $(HIDEDIR)/mzscheme_objects
	mkdir $(HIDEDIR)/gc_objects
	mv gc/objects $(HIDEDIR)/gc_objects	
	mkdir $(HIDEDIR)/sgc_objects
	mv sgc/objects $(HIDEDIR)/sgc_objects	
	mkdir $(HIDEDIR)/guile_objects
	mv guile/objects $(HIDEDIR)/guile_objects	
	mkdir $(HIDEDIR)/dynsrc_objects
	mv dynsrc/objects $(HIDEDIR)/dynsrc_objects	
	mkdir $(HIDEDIR)/mzscheme_collects
	mv ../collects/mzscheme/lib $(HIDEDIR)/mzscheme_collects
	$(XMAKE) clean
	/bin/rm -f aixdlfcn/dlfcn.o aixdlfcn/libdl.a 
	/bin/rm -f $(LIBDIR)/* mzscheme withwin.so dynexmpl.so oe.so
	/bin/rm ../collects/mzlib/RCS
	/bin/rm tests/RCS

repatriate:
	mv $(HIDEDIR)/mzscheme_lib/lib .
	rmdir $(HIDEDIR)/mzscheme_lib
	mv $(HIDEDIR)/mzscheme_main_objects/objects .
	mv $(HIDEDIR)/mzscheme_main_objects/.bin .
	rmdir $(HIDEDIR)/mzscheme_main_objects
	mv $(HIDEDIR)/mzscheme_objects/objects src
	rmdir $(HIDEDIR)/mzscheme_objects
	mv $(HIDEDIR)/gc_objects/objects gc
	rmdir $(HIDEDIR)/gc_objects
	mv $(HIDEDIR)/sgc_objects/objects sgc
	rmdir $(HIDEDIR)/sgc_objects
	mv $(HIDEDIR)/guile_objects/objects guile
	rmdir $(HIDEDIR)/guile_objects
	mv $(HIDEDIR)/dynsrc_objects/objects dynsrc
	rmdir $(HIDEDIR)/dynsrc_objects
	mv $(HIDEDIR)/mzscheme_collects/lib ../collects/mzscheme/lib
	rmdir $(HIDEDIR)/mzscheme_collects
	/bin/ln -s /home/scheme/plt/collects/mzlib/RCS ../collects/mzlib/RCS
	/bin/ln -s /home/scheme/plt/tests/mzscheme/RCS tests/RCS

# Doesn't install binaries, only support files:
install:
	rm -rf ../collects/mzlib/*~
	@echo --- README, etc. ---
	cp README SCRIPTS HISTORY COPYING.LIB mzscheme.init openbugs $(INSTALLDEST)/notes/mzscheme
	@echo --- tests ---
	cd tests; echo 'for file in *.ss ; do if [ "`rlog -R -Lmflatt $$file`" != "" ] ; then ci -l -mrelease $$file ; fi ; done' | sh -t 
	cd tests; ci -l -mrelease README
	cd $(INSTALLDEST)/tests/mzscheme; co RCS/*
	$(XMAKE) install-mzlib	
	$(XMAKE) install-scripts
	$(XMAKE) install-dynext
	$(XMAKE) install-headers

install-mzlib:
	@echo --- mzlib ---
	cd ../collects/mzlib; echo 'for file in *.ss ; do if [ "`rlog -R -Lmflatt $$file`" != "" ] ; then ci -l -mrelease $$file ; fi ; done' | sh -t
	cd $(INSTALLDEST)/collects/mzlib; /home/scheme/Executables/update
	rm -f $(INSTALLDEST)/collects/mzlib/compiled/*.zo
	cp ../collects/mzlib/compiled/*.zo $(INSTALLDEST)/collects/mzlib/compiled 
	touch $(INSTALLDEST)/collects/mzlib/compiled/*.zo

install-scripts:
	@echo --- script ---
	cp mzscheme.sh $(INSTALLDEST)/bin/mzscheme
	cp dynsrc/dynlink $(INSTALLDEST)/bin/dynlink

install-dynext:
	@echo --- script ---
	rm -rf $(INSTALLDEST)/collects/mzscheme/dynext/*
	cp ../collects/mzscheme/dynext/* $(INSTALLDEST)/collects/mzscheme/dynext

install-headers:
	$(XMAKE) headers
	@echo --- headers ---
	rm -rf $(INSTALLDEST)/collects/mzscheme/include/*
	cp ../collects/mzscheme/include/* $(INSTALLDEST)/collects/mzscheme/include
