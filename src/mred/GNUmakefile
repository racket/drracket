
WXDIR=../wxxt

PLTSRCDIR = ..
include $(WXDIR)/src/xtmake.env

# system name for installation
INSTALLSYS = `../../bin/archsys`

OBJDIR=objects/sys$(MZPLATFORMSUFFIX)$(GUISUFFIX)
LIBDIR=lib/sys$(MZPLATFORMSUFFIX)
MZLIBDIR=$(LIBDIR)$(MZTHREADSUFFIX)

MZSCHEMELDFLAGS = -L../mzscheme/$(LIBDIR)/
MZSCHEMELDLIBS = -lmzscheme -lgc

WXSCHEMELDFLAGS = -Lwxs/$(OBJDIR)
WXSCHEMELDLIBS = -lwxscheme

WXMELDFLAGS = -Lwxme/$(OBJDIR)
WXMELDLIBS = -lwxme

MREDLDFLAGS = $(LDFLAGS) $(MZSCHEMELDFLAGS) $(WXSCHEMELDFLAGS) $(WXMELDFLAGS)
MREDLDLIBS = $(MZSCHEMELDLIBS) $(WXSCHEMELDLIBS) $(WXMELDLIBS) -lwx_xt -lXpm -limage_xt $(LDLIBS) $(LDNONSTATICLIBS)

MREDOBJECTS = $(OBJDIR)/mred.o $(OBJDIR)/mredx.o $(OBJDIR)/wxGC.o

MRED = $(MRBUILDDIR)mred
MZSCHEME = ../mzscheme/$(MZLIBDIR)/libmzscheme.a ../mzscheme/$(MZLIBDIR)/libgc.a
WXSCHEME = wxs/$(OBJDIR)/libwxscheme.a
WXME = wxme/$(OBJDIR)/libwxme.a

INSTALLDEST=/home/scheme/plt

MRSTATIC = 
MRSTATIC_STUB =

MRCHECK =

WXINCDEP = $(WXDIR)/src/Windows/Window.h

# For SGIs and gcc 2.7.2, I needed a hack
# (not needed with CC)
SGILINKHACK =
# SGILINKHACK = $(OBJDIR)/sgilinkhack.o

EXPORT = CC='$(CC)' CXX='$(CXX)' AS='$(AS)' RANLIB='$(RANLIB)' DEBUGFLAGS='$(DEBUGFLAGS)' OPTIONS='$(OPTIONS)' THREADFLAGS='$(THREADFLAGS)' WARN='$(WARN)' OPT='$(OPT)' $(MZEXTRAEXPORT)

# Used for distribution
HIDEDIR = /home/mflatt/mredhide
FTPDIR = /home/scheme/FTP/plt/staging

all:  objects $(OBJDIR)
	cd ../mzscheme; $(MAKE) $(EXPORT) mzschemelib
	$(MAKE) wx
	if [ -f wrap/wrap.ss ] ; then $(MAKE) mred-wrap ; fi
	cd wxs; $(MAKE)
	cd wxme; $(MAKE)
	$(MAKE) $(MRED) MRCHECK='$(MRCHECK)'

mred-wrap:
	cd wrap; $(MAKE)

bin:
	$(MAKE) $(MRED)

checked:
	$(MAKE) all MRCHECK='$(OBJDIR)/checkm.o'

$(OBJDIR):
	mkdir $(OBJDIR)
objects:
	mkdir objects

$(MRED) : $(WXME) $(WXSCHEME) $(MZSCHEME) $(MREDOBJECTS) \
          $(WXDIR)/$(LIBDIR)/libwx$(GUISUFFIX).a \
          $(WXDIR)/utils/image/$(LIBDIR)/libimage_xt.a \
          $(MRSTATIC_STUB) $(MRCHECK)
	$(LINKER) $(MREDLDFLAGS) $(MRSTATIC) -o $(MRED) $(LDSTARTERS) $(MREDOBJECTS) $(MRCHECK) $(MREDLDLIBS) $(MRSTATIC_STUB) $(SGILINKHACK) $(LDENDERS)

ee-app: $(MRED) $(OBJDIR)/mred_ee.o
	if [ "$(EEAPP)" = '' ] ; then echo "ERROR: You must specify EEAPP" ; else $(LINKER) $(MREDLDFLAGS) $(MRSTATIC) -o $(EEAPP) $(LDSTARTERS) $(OBJDIR)/mred_ee.o $(OBJDIR)/mredx.o $(EEOBJECTS) $(MRCHECK) $(MREDLDLIBS) $(MRSTATIC_STUB) $(SGILINKHACK) $(LDENDERS) ; fi

sunos4-static: $(OBJDIR)/dl_stub.o
	$(MAKE) $(MRED) MRSTATIC=-static  MRSTATIC_STUB='$(OBJDIR)/dl_stub.o' LDNONSTATICLIBS=''

solaris-static:
	$(MAKE) $(MRED) MRSTATIC="-Wl,-Bstatic" LDNONSTATICLIBS='-Wl,-Bdynamic -ldl -Wl,-Bstatic'

linux-static: $(OBJDIR)/dl_stub.o
	$(MAKE) $(MRED) MRSTATIC=-static MRSTATIC_STUB='$(OBJDIR)/dl_stub.o' LDNONSTATICLIBS='-Wl,--verbose'

$(OBJDIR)/checkm.o: misc/checkm.c
	$(CC) $(CFLAGS) $(LOCALFLAGS) -c misc/checkm.c -o $(OBJDIR)/checkm.o

wx:
	cd $(WXDIR)/src/x; $(MAKE)

$(OBJDIR)/mred.o : mred.cxx wxme/wx_media.h wxme/wx_medad.h wxs/wxscheme.h ../mzscheme/cmdline.inc mred.h \
                   ../mzscheme/include/scheme.h ../mzscheme/src/schvers.h $(WXINCDEP) $(WXDIR)/src/Windows/Frame.h \
                   $(WXDIR)/src/DataStructures/Object.h ../wxcommon/wxGC.h ../wxcommon/wx_list.h
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c mred.cxx -o $(OBJDIR)/mred.o

$(OBJDIR)/mred_ee.o : $(OBJDIR)/mred.o
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -DSTANDALONE_WITH_EMBEDDED_EXTENSION -c mred.cxx -o $(OBJDIR)/mred_ee.o

ee-main:
	$(OBJDIR)/mred_ee.o

$(OBJDIR)/mredx.o : mredx.cxx ../mzscheme/include/scheme.h mred.h $(WXDIR)/src/Windows/Frame.h \
                    $(WXDIR)/src/DataStructures/Object.h ../wxcommon/wxGC.h ../wxcommon/wx_list.h
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c mredx.cxx -o $(OBJDIR)/mredx.o

$(OBJDIR)/wxGC.o : ../wxcommon/wxGC.cxx ../wxcommon/wxGC.h
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c ../wxcommon/wxGC.cxx -o $(OBJDIR)/wxGC.o

$(OBJDIR)/dl_stub.o: misc/dl_stub.c
	$(CC) $(CPPFLAGS) $(LOCALFLAGS) -c misc/dl_stub.c -o $(OBJDIR)/dl_stub.o

$(OBJDIR)/sgilinkhack.o:
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c misc/sgilinkhack.cxx -o $(OBJDIR)/sgilinkhack.o

$(WXDIR)/$(LIBDIR)/libwx$(GUISUFFIX).a:
	echo Updating $(WXDIR)/$(LIBDIR)/libwx$(GUISUFFIX).a
	$(MAKE) wx

depend:
	cd wxs; $(MAKE) depend
	cd ../mzscheme; $(MAKE) depend

TILDES=*~ ../collects/mrdemo/*~ demo/turtles/*~ demo/morph/*~ system/*~ collects/mred/*~ tests/*~ misc/*~ windows/*~ ../*~

clean-mred:
	cd $(WXDIR)/src/x; $(MAKE) clean
	rm -rf objects/*/*.o $(TILDES) core
	cd wxme; $(MAKE) clean
	cd wxs; $(MAKE) clean

clean:
	$(MAKE) clean-mred
	cd ../mzscheme; $(MAKE) clean

install:
	if [ ! -d ../../.bin ] ; then mkdir ../../.bin ; fi
	if [ ! -d ../../.bin/$(INSTALLSYS) ] ; then mkdir ../../.bin/$(INSTALLSYS) ; fi
	rm -f ../../.bin/$(INSTALLSYS)/mred
	cp $(MRED) ../../.bin/$(INSTALLSYS)/mred


################################
#          Precise GC          #
################################

XFORM = ../mzscheme/$(MZBUILDDIR)mzscheme -rq ../mzscheme/gc2/xform.ss "gcc -E -I../mzscheme/gc2 $(OPTIONS) $(GUI) $(INC) -I$(WXDIR)/src/XWidgets -I$(WXDIR)/src $(XINCLUDE)"

gc2/xxx: 
	gcc -E -H $(OPTIONS) $(GUI) -I../mzscheme/gc2 $(INC) -I$(WXDIR)/src/XWidgets -I$(WXDIR)/src $(XINCLUDE) -o gc2/xxx wxme/wx_media.cxx

gc2/Window.cc: $(WXDIR)/src/Windows/Window.cc
	$(XFORM) $< $@

gc2/WindowDC.cc: $(WXDIR)/src/DeviceContexts/WindowDC.cc
	$(XFORM) $< $@

gc2/wx_media.cc: wxme/wx_media.cxx
	$(XFORM) $< $@

gc2/wx_mpriv.cc: wxme/wx_mpriv.cxx
	$(XFORM) $< $@

gc2/wx_snip.cc: wxme/wx_snip.cxx
	$(XFORM) $< $@

gc2/wx_msnip.cc: wxme/wx_msnip.cxx
	$(XFORM) $< $@

gc2/wx_mbuf.cc: wxme/wx_mbuf.cxx
	$(XFORM) $< $@

gc2/wx_mpbrd.cc: wxme/wx_mpbrd.cxx
	$(XFORM) $< $@

gc2/wx_keym.cc: wxme/wx_keym.cxx
	$(XFORM) $< $@

gc2/wx_medio.cc: wxme/wx_medio.cxx
	$(XFORM) $< $@

gc2/wx_style.cc: wxme/wx_style.cxx
	$(XFORM) $< $@

gc2/mred.cc: mred.cxx
	$(XFORM) $< $@

gc2/mredx.cc: mredx.cxx
	$(XFORM) $< $@


gc2/$(OBJDIR)/%.o: gc2/%.cc
	$(CXX) -DGC2_JUST_MACROS -include ../mzscheme/gc2/gc2.h -c $(CXXFLAGS) -o $@ $<

XSRCS = gc2/Window.cc \
  gc2/WindowDC.cc \
  gc2/wx_media.cc \
  gc2/wx_mpriv.cc \
  gc2/wx_snip.cc \
  gc2/wx_msnip.cc \
  gc2/wx_mbuf.cc \
  gc2/wx_mpbrd.cc \
  gc2/wx_keym.cc \
  gc2/wx_medio.cc \
  gc2/wx_style.cc \
  gc2/mred.cc \
  gc2/mredx.cc

XOBJS = ${XSRCS:.cc/=$(OBJDIR)/.o}

xform: $(XSRCS)
