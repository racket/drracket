#
# Makefile for mzscheme
#
# 'make' or 'make bin' - makes mzscheme and mzdyn.o
# 'make mzschemelib' - make libmzscheme.a and libgc.a
# 'make libguile' - make libguile.a
# 'make dynlib' - make mzdyn.o, for use in extending MzScheme
# 'make g-c' - makes only libgc.a

# SEE ALSO sconfig.h for more configuration

# This must be an ANSI C compiler.
CC = gcc
CXX = gcc
CPP = gcc -E
AS = gcc -c -x assembler-with-cpp

# If you don't have perl, use:
# PERL = 
PERL = perl

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
LIBS = -lm # -ldl

# If your system needs ranlib, put it here.  Otherwise, use
# RANLIB = :
RANLIB = ranlib

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

# system name for installation
INSTALLSYS = `../../bin/archsys`

# dependency for performing an install
INSTALLTARGET = normal-install

# output file for the OSKit-based MzScheme
MZOSKDEST = mzscheme.multiboot

################################################################################

# Export string for making src & GC
EXPORT= CC='$(CC)' CXX='$(CXX)' CPP='$(CPP)' AS='$(AS)' RANLIB='$(RANLIB)' PERL='$(PERL)' DEBUGFLAGS='$(DEBUGFLAGS)' OPTIONS='$(OPTIONS) $(THREADFLAGS)' THREADFLAGS='' DYNOPTIONS='$(DYNOPTIONS)' WARN='$(WARN)' OPT='$(OPT)' OBJDIR='$(OBJDIR)' GCDIR='$(GCDIR)' LIBS='$(LIBS)' GCLIBMAKEEXTRA='$(GCLIBMAKEEXTRA)'

MAIN_CFLAGS_NODBG = $(WARN) $(OPT) $(FLAGS) $(OPTIONS) $(THREADFLAGS) -Iinclude
MAIN_CFLAGS = $(DEBUGFLAGS) $(NONLINKOPTIONS) $(MAIN_CFLAGS_NODBG)

XMAKE = $(MAKE) $(EXPORT)

bin: 
	$(XMAKE) g-c
	$(XMAKE) dynlib
	./ifaix $(XMAKE) rs6kbin
	./ifnotaix $(XMAKE) xbin

rs6kbin:
	./ifaix $(XMAKE) aixdl
	$(XMAKE) LIBS='$(LIBS) -ldl -lld -Wl,-bE:include/mzscheme.exp' xbin

xbin:
	$(XMAKE) mzlibrary
	$(XMAKE) $(MZSCHEME) 

oskit: 
	$(XMAKE) g-c
	$(XMAKE) dynlib
	$(XMAKE) mzlibrary
	$(XMAKE) $(MZOSKDEST)

mzschemelib: 
	./ifaix $(XMAKE) aixdl
	$(XMAKE) g-c 
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
	cd src; $(XMAKE) -e mzheaders
	cd dynsrc; $(XMAKE) LIBDIR='$(LIBDIR)' -e dynlib 

dynexample:
	$(XMAKE) dynlib
	cd dynsrc; $(XMAKE) LIBDIR='$(LIBDIR)' -e dynexample 

oe:
	$(XMAKE) dynlib
	cd dynsrc; $(XMAKE) -e oe

aixdl:
	cd aixdlfcn; $(MAKE) CC='$(CC)'
	$(XMAKE) $(LIBDIR)/libdl.a

cygwin-dll-setup:
	cd src; $(XMAKE) -e mzheaders
	$(XMAKE) gmzwin.exp

gmzwin.exp: include/gmzwin.def
	dlltool --def include/gmzwin.def --output-exp gmzwin.exp --output-lib gmzwin.a --dllname mzscheme.exe

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
	$(AR) $(ARFLAGS) $(GCDIR)/$(OBJDIR)/gc.a $(GCDIR)/$(OBJDIR)/*.o  

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

# OSKit-based MzScheme
$(MZOSKDEST) : objects $(OBJDIR) $(LIBDIR)/libmzscheme.a $(LIBDIR)/libgc.a $(OBJDIR)/main.o
	$(LD) -Ttext 100000 $(LDFLAGS) $(OSKIT_LDFLAGS) $(OSKHOME)/lib/oskit/multiboot.o \
		-o $(MZOSKDEST) $(OBJDIR)/main.o -L$(LIBDIR)/ -lmzscheme -lgc \
                -L$(OSKHOME)/lib -loskit_kern -loskit_clientos $(EXTRA_OSK_LIBS) \
                -loskit_freebsd_c -loskit_c -loskit_lmm \
                -loskit_freebsd_m -loskit_memfs -loskit_fsnamespace  \
                -loskit_dev -loskit_com \
                -loskit_kern -loskit_freebsd_c -loskit_c \
		$(OSKHOME)/lib/oskit/crtn.o

$(OBJDIR)/main.o: main.c include/scheme.h sconfig.h cmdline.inc
	$(CC) $(MAIN_CFLAGS) -c main.c -o $(OBJDIR)/main.o

$(OBJDIR)/main_ee.o: $(OBJDIR)/main.o
	$(CC) $(MAIN_CFLAGS) -DSTANDALONE_WITH_EMBEDDED_EXTENSION -c main.c -o $(OBJDIR)/main_ee.o

ee-main:
	$(MAKE) $(OBJDIR)/main_ee.o

ee-app: $(MZSCHEME) $(OBJDIR)/main_ee.o
	if [ "$(EEAPP)" = '' ] ; then echo "ERROR: You must specify EEAPP" ; else $(CC) $(MAIN_CFLAGS_NODBG) -o $(EEAPP) $(OBJDIR)/main_ee.o $(EEOBJECTS) $(SPECIALIZINGOBJECTS) -L$(LIBDIR)/ -lmzscheme -lgc $(LIBS) ; fi

testext.o: testext.c
	$(CC) $(MAIN_CFLAGS) -I/home/scheme/plt/compiler/ -c testext.c -o testext.o

mzstart.exe: dynsrc/start.c
	$(CC) -o mzstart.exe dynsrc/start.c

starter: mzstart.exe

exn:
	$(XMAKE) src/schexn.h

CMACRODEST = cmacro.inc

cmacro:
	$(XMAKE) src/$(CMACRODEST)
	$(XMAKE) src/cunitsig.inc

headers:
	$(XMAKE) ../../collects/mzscheme/include/scheme.h
	$(XMAKE) ../../collects/mzscheme/lib/mzdyn.c

# Needed for extension compilation when MzScheme headers change
../../collects/mzscheme/include/scheme.h: sconfig.h uconfig.h include/escheme.h src/schemef.h \
                                       src/stypes.h src/schemex.h src/schemexm.h src/schexn.h \
                                       include/ext.exp include/mzscheme.exp include/scheme.h \
                                       src/schvers.h
	rm -rf ../../collects/mzscheme/include/*.* # avoid CVS
	cp sconfig.h uconfig.h include/escheme.h src/schemef.h src/schvers.h \
          src/stypes.h src/schemex.h src/schemexm.h src/schexn.h \
          include/ext.exp include/mzscheme.exp ../../collects/mzscheme/include 
	perl -ne 's/\/\*III\*\//\#define INCLUDE_WITHOUT_PATHS/; print' \
          < include/scheme.h > ../../collects/mzscheme/include/scheme.h

# mzdyn.c, used for MacOS "library"
../../collects/mzscheme/lib/mzdyn.c: dynsrc/mzdyn.c
	rm -rf ../../collects/mzscheme/lib/mzdyn.c
	cp dynsrc/mzdyn.c ../../collects/mzscheme/lib/mzdyn.c

src/schexn.h: src/exnsrc.ss src/makeexn
	cd src; ../$(MZSCHEME) -gqr makeexn < exnsrc.ss > schexn.h

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

mzscheme2k:
	$(XMAKE) bin
	cd gc2; $(XMAKE) all
	cd gc2; $(XMAKE) $(MZBUILDDIR)mzscheme2k

depend:
	cd src; $(XMAKE) -e depend 
	cd dynsrc; $(XMAKE) -e depend 

clean:
	/bin/rm -rf tmp[123456789] tests/tmp[123456789] tests/sub[123]
	/bin/rm -f tests/*~
	/bin/rm -f objects/*/*.o *~ Makefile.bak utils/*~ include/*~ ../collects/mzlib/*~ core
	/bin/rm -f aixdlfcn/dlfcn.o aixdlfcn/libdl.a 
	/bin/rm -f $(LIBDIR)/mzdyn.o $(LIBDIR)/libgc.a $(LIBDIR)/libmzscheme.a $(LIBDIR)/libdl.a
	cd gc; $(XMAKE) clean
	cd sgc; $(XMAKE) clean
	cd src; $(XMAKE) clean
	cd dynsrc; $(XMAKE) clean
	cd aixdlfcn; $(XMAKE) clean
	cd guile; $(XMAKE) clean

install:
	$(XMAKE) $(INSTALLTARGET)

normal-install:
	if [ ! -d ../../.bin ] ; then mkdir ../../.bin ; fi
	if [ ! -d ../../.bin/$(INSTALLSYS) ] ; then mkdir ../../.bin/$(INSTALLSYS) ; fi
	rm -f ../../.bin/$(INSTALLSYS)/mzscheme
	cp $(MZSCHEME) ../../.bin/$(INSTALLSYS)/mzscheme
	if [ ! -d ../../collects/mzscheme/lib/$(INSTALLSYS) ] ; then mkdir ../../collects/mzscheme/lib/$(INSTALLSYS) ; fi
	cp $(LIBDIR)/mzdyn.o ../../collects/mzscheme/lib/$(INSTALLSYS)/mzdyn.o
	cp $(LIBDIR)/libgc.a ../../collects/mzscheme/lib/$(INSTALLSYS)/libgc.a
	cp $(LIBDIR)/libmzscheme.a ../../collects/mzscheme/lib/$(INSTALLSYS)/libmzscheme.a

cygwin-install:
	rm -f ../../mzscheme.exe
	cp $(MZSCHEME).exe ../../mzscheme.exe
	cd dynsrc; ./gmkmzdyn.bat
	cp mzstart.exe ../../collects/launcher/mzstart.exe
